use core::panic;
use std::str::FromStr;

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{Data, DataEnum, DataStruct, DeriveInput, Expr, Ident, Lit, LitInt, Type, Attribute};

fn serialize_struct(ident: Ident, s: DataStruct) -> TokenStream2 {
    let attrs: Vec<Ident> = s
        .fields
        .iter()
        .map(|field| field.ident.clone().unwrap())
        .collect();
    let types: Vec<Type> = s.fields.iter().map(|field| field.ty.clone()).collect();

    let mut cursors_a: Vec<TokenStream2> = types
        .iter()
        .scan(Vec::<TokenStream2>::new(), |acc, t| {
            let t = t.clone();
            acc.push(quote! { <#t as _TinySerSized>::SIZE });
            Some(quote! { {#( #acc )+*} })
        })
        .collect();
    let cursors_b = cursors_a.clone();
    cursors_a.insert(0, quote! { 0 });

    quote! {
        impl _TinySerSized for #ident {
            const SIZE: usize = {#( <#types as _TinySerSized>::SIZE )+*};
        }

        impl Serialize<{<#ident as _TinySerSized>::SIZE}> for #ident {
            fn serialize(self) -> [u8; {<#ident as _TinySerSized>::SIZE}] {
                let mut result = [0u8; {<#ident as _TinySerSized>::SIZE}];

                #(
                    let data = self.#attrs.serialize();
                    result[#cursors_a..#cursors_b].copy_from_slice(&data);
                )*

                result
            }
        }
    }
}

fn deserialize_struct(ident: Ident, s: DataStruct) -> TokenStream2 {
    let attrs: Vec<Ident> = s
        .fields
        .iter()
        .map(|field| field.ident.clone().unwrap())
        .collect();
    let types: Vec<Type> = s.fields.iter().map(|field| field.ty.clone()).collect();

    let mut cursors_a: Vec<TokenStream2> = types
        .iter()
        .scan(Vec::<TokenStream2>::new(), |acc, t| {
            let t = t.clone();
            acc.push(quote! { <#t as _TinyDeSized>::SIZE });
            Some(quote! { {#( #acc )+*} })
        })
        .collect();
    let cursors_b = cursors_a.clone();
    cursors_a.insert(0, quote! { 0 });

    quote! {
        impl _TinyDeSized for #ident {
            const SIZE: usize = {#( <#types as _TinyDeSized>::SIZE )+*};
        }

        impl Deserialize<{<#ident as _TinyDeSized>::SIZE}> for #ident {
            fn deserialize(data: [u8; {<#ident as _TinyDeSized>::SIZE}]) -> Option<Self> {
                Some(
                    Self {
                        #(
                            #attrs: #types::deserialize(data[#cursors_a..#cursors_b].try_into().unwrap())?
                        ),*
                    }
                )
            }
        }
    }
}

fn serialize_enum(ident: Ident, ty: Type) -> TokenStream2 {
    quote! {
        impl _TinySerSized for #ident {
            const SIZE: usize = {<#ty as _TinySerSized>::SIZE};
        }

        impl Serialize<{<#ident as _TinySerSized>::SIZE}> for #ident {
            fn serialize(self) -> [u8; {<#ident as _TinySerSized>::SIZE}] {
                (self as #ty).serialize()
            }
        }
    }
}

fn deserialize_enum(ident: Ident, ty: Type, e: DataEnum) -> TokenStream2 {
    let idents: Vec<Ident> = e
        .variants
        .iter()
        .map(|variant| variant.ident.clone())
        .collect();
    let discriminants: Vec<TokenStream2> = e
        .variants
        .iter()
        .scan(0isize, |acc, variant| {
            Some(if let Some((_, value)) = variant.discriminant.clone() {
                match value {
                    Expr::Lit(lit) => match lit.lit {
                        Lit::Int(int) => {
                            *acc = int.base10_parse().unwrap();
                            *acc
                        }
                        _ => {
                            panic!("Only integer discriminants are supported for now.")
                        }
                    },
                    _ => {
                        panic!("Discriminant must be a literal.")
                    }
                }
            } else {
                *acc += 1;
                acc.clone()
            })
        })
        .map(|value| {
            let lit: LitInt = syn::parse2(quote! { #value }).unwrap();
            TokenStream2::from_str(lit.base10_digits()).unwrap()
        })
        .collect();

    quote! {
        impl _TinyDeSized for #ident {
            const SIZE: usize = {<#ty as _TinyDeSized>::SIZE};
        }

        impl Deserialize<{<#ty as _TinyDeSized>::SIZE}> for #ident {
            fn deserialize(data: [u8; {<#ty as _TinyDeSized>::SIZE}]) -> Option<Self> {
                let repr = #ty::deserialize(data)?;

                match repr {
                    #(
                        #discriminants => Some(Self::#idents),
                    )*
                    _ => None
                }
            }
        }
    }
}

fn get_repr(attrs: Vec<Attribute>) -> Type {
    attrs
        .iter()
        .find(|&attr| attr.path().is_ident("repr"))
        .expect("Enum must have #[repr(...)] attribute.")
        .parse_args()
        .expect("#[repr(...) can only have one type.")
}

fn impl_serialize(body: DeriveInput) -> TokenStream2 {
    match body.data {
        Data::Struct(s) => serialize_struct(body.ident, s),
        Data::Enum(_) => {
            let ty = get_repr(body.attrs);

            serialize_enum(body.ident, ty)
        }
        Data::Union(_) => panic!("#[derive(Serialize)] does not support union types."),
    }
}

fn impl_deserialize(body: DeriveInput) -> TokenStream2 {
    match body.data {
        Data::Struct(s) => deserialize_struct(body.ident, s),
        Data::Enum(e) => {
            let ty = get_repr(body.attrs);

            deserialize_enum(body.ident, ty, e)
        }
        Data::Union(_) => panic!("#[derive(Serialize)] does not support union types."),
    }
}

#[proc_macro_derive(Serialize)]
pub fn serialize(input: TokenStream) -> TokenStream {
    impl_serialize(syn::parse2(input.into()).unwrap()).into()
}

#[proc_macro_derive(Deserialize)]
pub fn deserialize(input: TokenStream) -> TokenStream {
    impl_deserialize(syn::parse2(input.into()).unwrap()).into()
}

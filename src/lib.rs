//! # Builder
//!
//! A simple builder derive macro, with the intention of accomplishing all
//! needs from a builder.
//!
//! ```rust
//! # use builder::Builder;
//! # const _: &str = stringify!(
//! #[derive(Builder)]
//! # );
//! # #[derive(Builder, PartialEq, Debug)]
//! pub struct Foo {
//!     #[builder(default = "42")]
//!     field_a: u32,
//!     field_b: bool,
//!     #[builder(into)]
//!     field_c: String,
//!     #[builder(repeat, repeat_n = 1..4)]
//!     field_d: Vec<f32>,
//! }
//!
//! let foo: Foo = Foo::builder()
//!     .field_b(true)
//!     .field_c("hello world")
//!     .field_d(3.14)
//!     .field_d(6.28)
//!     .field_d(2.72)
//!     .build()
//!     .unwrap();
//!
//! assert_eq!(
//!     foo,
//!     Foo {
//!         field_a: 42,
//!         field_b: true,
//!         field_c: String::from("hello world"),
//!         field_d: vec![3.14, 6.28, 2.72],
//!     },
//! );
//! ```

use convert_case::{Case, Casing};
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{ToTokens, format_ident, quote};
use syn::{
    DeriveInput, Expr, ExprRange, Field, Ident, LitStr, Token, Type, Visibility,
    parse::ParseStream, parse_macro_input, spanned::Spanned,
};

fn get_single_generic<'a>(ty: &'a Type, name: Option<&str>) -> Option<&'a Type> {
    match ty {
        Type::Path(path)
            if path
                .path
                .segments
                .last()
                .is_some_and(|s| name.is_none_or(|name| s.ident == name))
                && path.path.segments.len() == 1 =>
        {
            let option = path
                .path
                .segments
                .last()
                .expect("checked in guard condition");

            let arg = match option.arguments {
                syn::PathArguments::AngleBracketed(ref args) if args.args.len() == 1 => {
                    let Some(syn::GenericArgument::Type(arg)) = args.args.first() else {
                        return None;
                    };
                    arg
                }
                _ => return None,
            };
            Some(arg)
        }
        Type::Array(arr) if name.is_none() => Some(&arr.elem),
        Type::Slice(slice) if name.is_none() => Some(&slice.elem),
        Type::Reference(r) => get_single_generic(&r.elem, name),
        _ => None,
    }
}

struct BuilderField {
    ident: Ident,
    #[allow(unused)]
    vis: Visibility,
    ty: Type,
    attr: BuilderAttr,
    missing_err: Option<Ident>,
    wrapped_option: bool,
}

struct Repeat {
    ident: Ident,
    inner_ty: Type,
    len: Option<(ExprRange, Ident)>,
}

#[derive(Default)]
struct BuilderAttr {
    /// Some(Some(expr)) -> default is expr
    /// Some(None) -> default is Default::default()
    /// None -> no default
    default: Option<Option<Expr>>,
    into: bool,
    repeat: Option<Repeat>,
}

macro_rules! bail {
    ($span: expr => $message: literal $(, $args: expr)*$(,)?) => {
        return Err(syn::Error::new(
            $span,
            format!($message, $($args),*),
        ))
    }
}

impl BuilderAttr {
    fn parse(input: syn::parse::ParseStream, field: &Field) -> syn::Result<Self> {
        let mut out = BuilderAttr::default();
        let field_ident = field.ident.as_ref().unwrap();

        while input.peek(syn::Ident) {
            let ident: Ident = input.parse()?;
            if ident == "default" {
                if out.default.is_some() {
                    bail!(ident.span() => "`default` may only be used once.");
                }

                if out.repeat.is_some() {
                    bail!(ident.span() => "`default` cannot be added with `repeat`");
                }

                let value: Option<Expr> = if input.peek(Token![=]) {
                    let _: Token![=] = input.parse()?;
                    let s: LitStr = input.parse()?;
                    Some(s.parse()?)
                } else {
                    None
                };

                out.default = Some(value)
            } else if ident == "into" {
                if out.into {
                    bail!(ident.span() => "`into` may only be used once.");
                }

                out.into = true
            } else if ident == "repeat" {
                if out.repeat.is_some() {
                    bail!(ident.span() => "`repeat` may only be used once.");
                }

                if out.default.is_some() {
                    bail!(ident.span() => "`repeat` cannot be added with `default`");
                }

                let value: Ident = if input.peek(Token![=]) {
                    let _: Token![=] = input.parse()?;
                    let s: LitStr = input.parse()?;
                    s.parse()?
                } else {
                    field_ident.clone()
                };

                let Some(inner) = get_single_generic(&field.ty, None) else {
                    bail!(field.ty.span() => "Cannot repeat on value with no generics");
                };

                out.repeat = Some(Repeat {
                    ident: value,
                    inner_ty: inner.clone(),
                    len: None,
                });
            } else if ident == "repeat_n" {
                let Some(rep) = &mut out.repeat else {
                    bail!(ident.span() => "`repeat_n` may only be used with `repeat`");
                };

                if rep.len.is_some() {
                    bail!(ident.span() => "`repeat_n` may only be used once.");
                }

                let _: Token![=] = input.parse()?;
                let mut ident =
                    format_ident!("Range{}", field_ident.to_string().to_case(Case::Pascal));
                ident.set_span(ident.span());
                rep.len = Some((input.parse()?, ident));
            }

            if input.peek(Token![,]) {
                let _: Token![,] = input.parse()?;
            } else {
                break;
            }
        }

        Ok(out)
    }
}

impl TryFrom<&Field> for BuilderField {
    type Error = syn::Error;

    fn try_from(value: &Field) -> Result<Self, Self::Error> {
        let ident = value.ident.as_ref().expect("We only support named fields");
        let attr: BuilderAttr = if let Some(builder_attr) =
            value.attrs.iter().find(|a| a.path().is_ident("builder"))
        {
            builder_attr.parse_args_with(|input: ParseStream| BuilderAttr::parse(input, value))?
        } else {
            BuilderAttr::default()
        };

        let (ty, wrapped_option) = if let Some(ty) = get_single_generic(&value.ty, Some("Option")) {
            (ty, true)
        } else {
            (&value.ty, false)
        };

        Ok(BuilderField {
            ident: ident.clone(),
            vis: value.vis.clone(),
            ty: ty.clone(),
            missing_err: if attr.default.is_none() && attr.repeat.is_none() {
                let mut ident = format_ident!("Missing{}", ident.to_string().to_case(Case::Pascal));
                ident.set_span(value.ident.as_ref().unwrap().span());
                Some(ident)
            } else {
                None
            },
            attr,
            wrapped_option,
        })
    }
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn builder(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let ident = &input.ident;
    // TODO: custom visiblity of builder
    let vis = &input.vis;

    let data_struct = match input.data {
        syn::Data::Struct(ref data_struct) => data_struct,
        syn::Data::Enum(data_enum) => {
            return syn::Error::new(data_enum.enum_token.span(), "Enums are not supported.")
                .to_compile_error()
                .into();
        }
        syn::Data::Union(data_union) => {
            return syn::Error::new(data_union.union_token.span(), "Unions are not supported.")
                .to_compile_error()
                .into();
        }
    };

    let builder = format_ident!("{}Builder", ident);
    let build_err = format_ident!("{}BuildError", ident);
    let fields_named: Vec<_> = match data_struct.fields {
        syn::Fields::Named(ref fields_named) => match fields_named
            .named
            .iter()
            .map(BuilderField::try_from)
            .collect::<Result<_, _>>()
        {
            Ok(v) => v,
            Err(e) => return e.to_compile_error().into(),
        },
        syn::Fields::Unnamed(_) => {
            return syn::Error::new(ident.span(), "Unnamed fields are not supported.")
                .to_compile_error()
                .into();
        }
        syn::Fields::Unit => {
            return syn::Error::new(ident.span(), "Unit structs are not supported.")
                .to_compile_error()
                .into();
        }
    };

    let fields: TokenStream2 = fields_named
        .iter()
        .map(|f| {
            let ident = &f.ident;
            if let Some(Repeat { inner_ty, .. }) = &f.attr.repeat {
                quote! {
                    #ident: ::std::vec::Vec<#inner_ty>,
                }
            } else {
                let ty = &f.ty;
                quote! {
                    #ident: ::core::option::Option<#ty>,
                }
            }
        })
        .collect();

    let functions: TokenStream2 = fields_named
        .iter()
        .map(|f| {
            let (ty, ident) = if let Some(Repeat {
                ident, inner_ty, ..
            }) = &f.attr.repeat
            {
                (inner_ty, ident)
            } else {
                (&f.ty, &f.ident)
            };

            let (source, value) = if f.attr.into {
                (
                    quote! { impl ::core::convert::Into<#ty> },
                    quote! { ::core::convert::Into::into(#ident) },
                )
            } else {
                (ty.to_token_stream(), ident.to_token_stream())
            };

            if f.attr.repeat.is_some() {
                let vec = &f.ident;
                quote! {
                    #vis fn #ident(self, #ident: #source) -> Self {
                        let mut this = self;
                        this.#vec.push(#value);
                        this
                    }
                }
            } else {
                quote! {
                    #vis fn #ident(self, #ident: #source) -> Self {
                        let mut this = self;
                        this.#ident = Some(#value);
                        this
                    }
                }
            }
        })
        .collect();

    let build_err_variants = fields_named.iter().flat_map(|f| {
        let mut variants = Vec::new();
        if let Some(err) = &f.missing_err {
            variants.push(err.to_token_stream());
        }
        if let Some(Repeat {
            len: Some((_, err)),
            ..
        }) = &f.attr.repeat
        {
            variants.push(quote! {
                #err(usize)
            });
        }
        variants.into_iter()
    });

    let field_names: Vec<_> = fields_named.iter().map(|f| &f.ident).collect();

    let build_fields = fields_named.iter().map(|field| {
        let name = &field.ident;

        if let Some(Repeat { len, .. }) = &field.attr.repeat {
            if let Some((range, err)) = len {
                quote! {
                    #name: match self.#name.len() {
                        #range => self.#name.into_iter().collect(),
                        len => return Err(#build_err::#err(len)),
                    }
                }
            } else {
                quote! {
                    #name: self.#name.into_iter().collect()
                }
            }
        } else if field.wrapped_option {
            quote! {
                #name: self.#name
            }
        } else if let Some(default) = &field.attr.default {
            if let Some(default) = default {
                if field.attr.into {
                    quote! {
                        #name: self.#name.unwrap_or_else(|| #default.into())
                    }
                } else {
                    quote! {
                        #name: self.#name.unwrap_or_else(|| #default)
                    }
                }
            } else {
                quote! {
                    #name: self.#name.unwrap_or_else(|| ::core::default::Default::default())
                }
            }
        } else {
            let err = field
                .missing_err
                .as_ref()
                .expect("missing_err is set when default is none");
            quote! {
                #name: self.#name.ok_or(#build_err::#err)?
            }
        }
    });

    quote! {
        #[derive(::std::fmt::Debug)]
        #vis enum #build_err {
            #(#build_err_variants),*
        }

        #vis struct #builder {
            #fields
        }

        impl #builder {
            #functions

            #vis fn build(self) -> ::core::result::Result<#ident, #build_err> {
                Ok(#ident {
                    #(#build_fields),*
                })
            }
        }

        impl ::core::default::Default for #builder {
            fn default() -> Self {
                Self {
                    #(#field_names: ::core::default::Default::default()),*
                }
            }
        }

        impl #ident {
            #vis fn builder() -> #builder {
                ::core::default::Default::default()
            }
        }
    }
    .into()
}

use convert_case::{Case, Casing};
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{ToTokens, format_ident, quote};
use syn::{
    DeriveInput, Expr, Field, Ident, LitStr, Token, Type, Visibility, parse::Parse,
    spanned::Spanned,
};

struct BuilderField {
    ident: Ident,
    #[allow(unused)]
    vis: Visibility,
    ty: Type,
    attr: BuilderAttr,
    missing_err: Option<Ident>,
    wrapped_option: bool,
}

#[derive(Default)]
struct BuilderAttr {
    default: Option<Expr>,
    into: bool,
}

impl Parse for BuilderAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut out = BuilderAttr::default();

        while input.peek(syn::Ident) {
            let ident: Ident = input.parse()?;
            if ident == "default" {
                if out.default.is_some() {
                    return Err(syn::Error::new(
                        ident.span(),
                        "`default` may only be used once.",
                    ));
                }

                let value: Expr = if input.peek(Token![=]) {
                    let _: Token![=] = input.parse()?;
                    let s: LitStr = input.parse()?;
                    s.parse()?
                } else {
                    syn::parse_quote! { ::core::default::Default::default() }
                };

                out.default = Some(value)
            } else if ident == "into" {
                if out.into {
                    return Err(syn::Error::new(
                        ident.span(),
                        "`default` may only be used once.",
                    ));
                }

                out.into = true
            }
        }

        Ok(out)
    }
}

impl TryFrom<&Field> for BuilderField {
    type Error = syn::Error;

    fn try_from(value: &Field) -> Result<Self, Self::Error> {
        let mut attr: BuilderAttr =
            if let Some(builder_attr) = value.attrs.iter().find(|a| a.path().is_ident("builder")) {
                builder_attr.parse_args()?
            } else {
                BuilderAttr::default()
            };

        let (ty, wrapped_option) = match &value.ty {
            Type::Path(path)
                if path
                    .path
                    .segments
                    .last()
                    .is_some_and(|s| s.ident == "Option")
                    && path.path.segments.len() == 1 =>
            'x: {
                let option = path
                    .path
                    .segments
                    .last()
                    .expect("checked in guard condition");

                let arg = match &option.arguments {
                    syn::PathArguments::AngleBracketed(args) if args.args.len() == 1 => {
                        let Some(syn::GenericArgument::Type(arg)) = args.args.first() else {
                            break 'x (value.ty.clone(), false);
                        };
                        arg.clone()
                    }
                    _ => break 'x (value.ty.clone(), false),
                };
                attr.default = Some(syn::parse_quote! { ::core::option::Option::None });
                (arg, true)
            }
            _ => (value.ty.clone(), false),
        };

        Ok(BuilderField {
            ident: value.ident.clone().unwrap(),
            vis: value.vis.clone(),
            ty,
            missing_err: if attr.default.is_none() {
                let mut ident = format_ident!(
                    "Missing{}",
                    value
                        .ident
                        .as_ref()
                        .unwrap()
                        .to_string()
                        .to_case(Case::Pascal)
                );
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
    let input = syn::parse_macro_input!(input as DeriveInput);
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
            let ty = &f.ty;
            quote! {
                #ident: ::core::option::Option<#ty>,
            }
        })
        .collect();

    let functions: TokenStream2 = fields_named
        .iter()
        .map(|f| {
            let ident = &f.ident;
            let ty = &f.ty;
            let (source, value) = if f.attr.into {
                (
                    quote! { impl ::core::convert::Into<#ty> },
                    quote! { ::core::convert::Into::into(#ident) },
                )
            } else {
                (ty.to_token_stream(), ident.to_token_stream())
            };

            quote! {
                #vis fn #ident(self, #ident: #source) -> Self {
                    let mut this = self;
                    this.#ident = Some(#value);
                    this
                }
            }
        })
        .collect();

    let build_err_variants = fields_named.iter().filter_map(|f| f.missing_err.as_ref());

    let field_names: Vec<_> = fields_named.iter().map(|f| &f.ident).collect();

    let build_fields = fields_named.iter().map(|field| {
        let name = &field.ident;
        if field.wrapped_option {
            quote! {
                #name: self.#name
            }
        } else if let Some(default) = &field.attr.default {
            quote! {
                #name: self.#name.unwrap_or_else(|| #default)
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
                    #(#field_names: None),*
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

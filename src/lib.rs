//! A derive macro for automatically generating the builder pattern
//!
//! ```rust
//! use bauer::Builder;
//!
//! # const _: &str = stringify!(
//! #[derive(Builder)]
//! # );
//! # #[derive(Builder, PartialEq, Debug)]
//! pub struct Foo {
//!     bar: u32,
//! }
//!
//! let foo: Foo = Foo::builder()
//!     .bar(42)
//!     .build()
//!     .unwrap();
//!
//! assert_eq!(foo, Foo { bar: 42, });
//! ```

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{ToTokens, format_ident, quote, quote_spanned};
use std::fmt::Write;
use syn::{DeriveInput, Ident, Type, parse::ParseStream, parse_macro_input, spanned::Spanned};

use crate::{
    builder::{BuilderAttr, Kind},
    field::{BuilderField, Repeat},
};

mod builder;
mod field;

pub(crate) fn get_single_generic<'a>(ty: &'a Type, name: Option<&str>) -> Option<&'a Type> {
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

/// The main macro.
///
/// ## Usage
///
/// ```
/// use bauer::Builder;
///
/// #[derive(Builder)]
/// pub struct Foo {
///     #[builder(default = "42")]
///     pub field_a: u32,
///     pub field_b: bool,
///     #[builder(into)]
///     pub field_c: String,
///     #[builder(repeat, repeat_n = 1..=3)]
///     pub field_d: Vec<f64>,
/// }
/// ```
///
/// ## Builder Attributes
///
/// ### **`kind`**
///
/// Possible values: `"owned"`, `"borrowed"`  
/// Default: `"owned"`
///
/// Whether the builder should be passed around as an owned value or a mutable reference.
///
/// ```
/// # use bauer::Builder;
/// #[derive(Builder)]
/// #[builder(kind = "borrowed")]
/// pub struct Foo {
///     a: u32,
/// }
/// ```
///
/// ### **`prefix`**/**`suffix`**
///
/// Default: `prefix = "", suffix = ""`
///
/// Set the prefix or suffix for the generated builder functions
///
/// ```
/// # use bauer::Builder;
/// #[derive(Builder)]
/// #[builder(prefix = "set_")]
/// pub struct Foo {
///     a: u32,
/// }
///
/// let f = Foo::builder()
///     .set_a(42)
///     .build()
///     .unwrap();
/// ```
///
/// ### **`visibility`**
///
/// Default: visibility of the struct
///
/// Set the visibilty for the created builder
///
/// ```
/// # use bauer::Builder;
/// #[derive(Builder)]
/// #[builder(visibility = pub(crate))]
/// pub struct Foo {
///     a: u32,
/// }
/// ```
///
/// ## Fields Attributes
///
/// ### **`default`**
///
/// Argument: Optional String
///
/// If provided, the field does not need to be specified, and will default to the value provided.
/// If not value is provided to the `default` attribute, then [`Default::default`] will be used.
///
/// ```
/// # use bauer::Builder;
/// # const _: &str = stringify!(
/// #[derive(Builder)]
/// # );
/// # #[derive(Builder, PartialEq, Debug)]
/// pub struct Foo {
///     #[builder(default)]
///     a: u32, // defaults to 0
///     #[builder(default = "std::f32::consts::PI")]
///     b: f32, // defaults to PI
/// }
///
/// let foo = Foo::builder()
///     .build()
///     .unwrap();
/// assert_eq!(foo, Foo { a: 0, b: std::f32::consts::PI });
///
/// let foo = Foo::builder()
///     .a(42)
///     .build()
///     .unwrap();
/// assert_eq!(foo, Foo { a: 42, b: std::f32::consts::PI });
/// ```
///
/// ### **`into`**
///
/// Make the method accept anything can be turned into the field.
///
/// ```
/// # use bauer::Builder;
/// # const _: &str = stringify!(
/// #[derive(Builder)]
/// # );
/// # #[derive(Builder, PartialEq, Debug)]
/// pub struct Foo {
///     #[builder(into)]
///     a: String,
/// }
///
/// let foo = Foo::builder()
///     .a("hello")
///     .build()
///     .unwrap();
/// assert_eq!(foo, Foo { a: String::from("hello") });
/// ```
///
/// ### **`repeat`**
///
/// Make the method accept only a single item and build a list from it
///
/// ```
/// # use bauer::Builder;
/// # const _: &str = stringify!(
/// #[derive(Builder)]
/// # );
/// # #[derive(Builder, PartialEq, Debug)]
/// pub struct Foo {
///     #[builder(repeat)]
///     items: Vec<u32>,
/// }
///
/// let foo = Foo::builder()
///     .items(0)
///     .items(1)
///     .items(2)
///     .build()
///     .unwrap();
/// assert_eq!(foo, Foo { items: vec![0, 1, 2] });
/// ```
///
/// ### **`repeat_n`**
///
/// Attribute `repeat` must also be specified.
///
/// Ensure that the length of items supplied via repeat is within a certain range.  If this range
/// is not met, an error will be returned.
///
/// ```
/// # use bauer::Builder;
/// # const _: &str = stringify!(
/// #[derive(Builder)]
/// # );
/// # #[derive(Builder, PartialEq, Debug)]
/// pub struct Foo {
///     #[builder(repeat, repeat_n = 2..=3)]
///     items: Vec<u32>,
/// }
///
/// let foo = Foo::builder()
///     .items(0)
///     .items(1)
///     .items(2)
///     .build()
///     .unwrap();
/// assert_eq!(foo, Foo { items: vec![0, 1, 2] });
///
/// let foo = Foo::builder()
///     .items(0)
///     .build()
///     .unwrap_err();
/// assert_eq!(foo, FooBuildError::RangeItems(1));
/// ```
///
/// ### **`rename`**
///
/// Make the function that is generated use a different name from field itself.
///
/// ```
/// # use bauer::Builder;
/// # const _: &str = stringify!(
/// #[derive(Builder)]
/// # );
/// # #[derive(Builder, PartialEq, Debug)]
/// pub struct Foo {
///     #[builder(repeat, rename = "item")]
///     items: Vec<u32>,
/// }
///
/// let foo = Foo::builder()
///     .item(0)
///     .item(1)
///     .build()
///     .unwrap();
/// assert_eq!(foo, Foo { items: vec![0, 1] });
/// ```
///
/// ### **`skip_prefix`**/**`skip_suffix`**
///
/// If a prefix or a suffix is specified in the builder attributes, skip applying those to the name
/// of this function.  This is epecially useful with `rename`.
///
/// ```
/// # use bauer::Builder;
/// # const _: &str = stringify!(
/// #[derive(Builder)]
/// # );
/// # #[derive(Builder, PartialEq, Debug)]
/// #[builder(prefix = "set_")]
/// pub struct Foo {
///     #[builder(repeat, rename = "item", skip_prefix)]
///     items: Vec<u32>,
/// }
///
/// let foo = Foo::builder()
///     .item(0)
///     .item(1)
///     .build()
///     .unwrap();
/// assert_eq!(foo, Foo { items: vec![0, 1] });
/// ```
#[proc_macro_derive(Builder, attributes(builder))]
pub fn builder(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let ident = &input.ident;
    let vis = &input.vis;

    let attr = input.attrs.iter().find(|a| a.path().is_ident("builder"));
    let attr: BuilderAttr = if let Some(attr) = attr {
        match attr.parse_args_with(|ps: ParseStream| BuilderAttr::parse(ps, vis.clone())) {
            Ok(a) => a,
            Err(e) => return e.to_compile_error().into(),
        }
    } else {
        BuilderAttr::new(vis.clone())
    };

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

    let (prefix, ret) = match attr.kind {
        Kind::Owned => (quote! { mut }, quote! { Self }),
        Kind::Borrowed => (quote! { &mut }, quote! { &mut Self }),
    };
    let builder_vis = attr.vis;

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
            let field_name = &f.ident;
            let ident = f.attr.rename.as_ref().unwrap_or(&f.ident);
            let ty = f.attr.repeat.as_ref().map(|r| &r.inner_ty).unwrap_or(&f.ty);

            let mut fn_ident = String::with_capacity(attr.prefix.len() + attr.suffix.len());
            if !f.attr.skip_prefix {
                fn_ident.push_str(&attr.prefix);
            }
            write!(fn_ident, "{}", ident).expect("Inserting into string will never fail");
            if !f.attr.skip_suffix {
                fn_ident.push_str(&attr.suffix);
            }
            let fn_ident = Ident::new(&fn_ident, ident.span());

            let (source, value) = if f.attr.into {
                (
                    quote! { impl ::core::convert::Into<#ty> },
                    quote! { ::core::convert::Into::into(#field_name) },
                )
            } else {
                (ty.to_token_stream(), field_name.to_token_stream())
            };

            if f.attr.repeat.is_some() {
                let vec = &f.ident;
                quote! {
                    fn #fn_ident(#prefix self, #field_name: #source) -> #ret {
                        self.#vec.push(#value);
                        self
                    }
                }
            } else {
                quote! {
                    fn #fn_ident(#prefix self, #field_name: #source) -> #ret {
                        self.#ident = Some(#value);
                        self
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
                        #range => self.#name.drain(..).collect(),
                        len => return Err(#build_err::#err(len)),
                    }
                }
            } else {
                quote! {
                    #name: self.#name.drain(..).collect()
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
                        #name: self.#name.take().unwrap_or_else(|| #default.into())
                    }
                } else {
                    quote! {
                        #name: self.#name.take().unwrap_or_else(|| #default)
                    }
                }
            } else {
                quote_spanned! {
                    field.ty.span() =>
                    #name: self.#name.take().unwrap_or_else(|| ::core::default::Default::default())
                }
            }
        } else {
            let err = field
                .missing_err
                .as_ref()
                .expect("missing_err is set when default is none");
            quote! {
                #name: self.#name.take().ok_or(#build_err::#err)?
            }
        }
    });

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    quote! {
        #[derive(::std::fmt::Debug, ::std::cmp::PartialEq, ::std::cmp::Eq)]
        #builder_vis enum #build_err {
            #(#build_err_variants),*
        }

        #builder_vis struct #builder #ty_generics {
            #fields
        }

        impl #impl_generics #builder #ty_generics #where_clause {
            #functions

            #builder_vis fn build(#prefix self) -> ::core::result::Result<#ident #ty_generics, #build_err> {
                Ok(#ident {
                    #(#build_fields),*
                })
            }
        }

        impl #impl_generics ::core::default::Default for #builder #ty_generics #where_clause {
            fn default() -> Self {
                Self {
                    #(#field_names: ::core::default::Default::default()),*
                }
            }
        }

        impl #impl_generics #ident #ty_generics #where_clause {
            #builder_vis fn builder() -> #builder #ty_generics {
                ::core::default::Default::default()
            }
        }
    }
    .into()
}

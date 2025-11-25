use std::str::FromStr;

use strum::{IntoStaticStr, VariantArray};
use syn::{
    Ident, LitStr, Token, Visibility,
    parse::{Parse, ParseStream},
};

macro_rules! bail {
    ($span: expr => $message: literal $(, $args: expr)*$(,)?) => {
        return Err(syn::Error::new(
            $span,
            format!($message, $($args),*),
        ))
    }
}

#[derive(Debug, Default)]
pub enum Kind {
    #[default]
    Owned,
    Borrowed,
    // TypeState,
}

impl FromStr for Kind {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "owned" => Ok(Self::Owned),
            "borrowed" => Ok(Self::Borrowed),
            // "type-state" => Ok(Self::TypeState),
            _ => Err(format!(
                "Unknown kind \"{}\".  Valid kinds are: \"owned\", \"borrowed\"",
                s
            )),
        }
    }
}

impl Parse for Kind {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let s: LitStr = input.parse()?;
        match Kind::from_str(&s.value()) {
            Ok(v) => Ok(v),
            Err(e) => Err(syn::Error::new(s.span(), e)),
        }
    }
}

#[derive(Clone, Copy, VariantArray, IntoStaticStr)]
#[strum(serialize_all = "snake_case")]
enum Attribute {
    Kind,
    Prefix,
    Suffix,
    Visibility,
}

impl Attribute {
    fn as_str(self) -> &'static str {
        self.into()
    }
}

impl AsRef<str> for Attribute {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl Attribute {
    fn parse(ident: &Ident) -> syn::Result<Self> {
        Self::VARIANTS
            .iter()
            .copied()
            .find(|e| ident == e)
            .ok_or_else(|| {
                syn::Error::new(
                    ident.span(),
                    format!(
                        "Unknown attribute '{}'.  Valid attribute are: '{}'",
                        ident,
                        Self::VARIANTS
                            .iter()
                            .map(|s| s.as_str())
                            .collect::<Vec<_>>()
                            .join(", ")
                    ),
                )
            })
    }
}

#[derive(Debug)]
pub struct BuilderAttr {
    pub kind: Kind,
    pub prefix: String,
    pub suffix: String,
    pub vis: Visibility,
}

impl BuilderAttr {
    pub fn new(vis: Visibility) -> Self {
        Self {
            kind: Default::default(),
            prefix: Default::default(),
            suffix: Default::default(),
            vis,
        }
    }

    pub fn parse(input: ParseStream, vis: Visibility) -> syn::Result<Self> {
        let mut out = Self::new(vis);

        let mut kind_set = false;
        let mut prefix_set = false;
        let mut suffix_set = false;
        let mut vis_set = false;

        while input.peek(syn::Ident) {
            let ident = input.parse()?;
            match Attribute::parse(&ident)? {
                Attribute::Kind => {
                    if kind_set {
                        bail!(ident.span() => "`kind` may only be used once.");
                    }

                    let _: Token![=] = input.parse()?;
                    out.kind = input.parse()?;
                    kind_set = true;
                }
                Attribute::Prefix => {
                    if prefix_set {
                        bail!(ident.span() => "`prefix` may only be used once.");
                    }

                    let _: Token![=] = input.parse()?;
                    out.prefix = input.parse::<LitStr>()?.value();
                    prefix_set = true;
                }
                Attribute::Suffix => {
                    if suffix_set {
                        bail!(ident.span() => "`suffix` may only be used once.");
                    }

                    let _: Token![=] = input.parse()?;
                    out.suffix = input.parse::<LitStr>()?.value();
                    suffix_set = true;
                }
                Attribute::Visibility => {
                    if vis_set {
                        bail!(ident.span() => "`visibility` may only be used once.");
                    }

                    let _: Token![=] = input.parse()?;
                    out.vis = input.parse()?;
                    vis_set = true;
                }
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

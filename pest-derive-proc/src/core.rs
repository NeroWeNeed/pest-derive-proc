use std::{borrow::Cow, env, path::Path};

use pest_generator::parse_derive::ParsedDerive;
use proc_macro2::Span;
use syn::{
    Attribute, DeriveInput, Expr, ExprLit, Generics, Ident, Lit, LitStr, Meta, Token,
    parenthesized, parse::Parse, token::Paren,
};

use super::constructor::RuleDefinition;
pub(super) mod kw {
    syn::custom_keyword!(path);
    syn::custom_keyword!(non_exhaustive);
}

pub trait GrammarNodeEmitter<'a> {
    fn emit<F>(self, handler: &mut F) -> syn::Result<()>
    where
        F: FnMut(GrammarNode<'a>) -> syn::Result<()>;
}

impl<'a> GrammarNodeEmitter<'a> for GrammarNode<'a> {
    fn emit<F>(self, handler: &mut F) -> syn::Result<()>
    where
        F: FnMut(GrammarNode<'a>) -> syn::Result<()>,
    {
        match self {
            GrammarNode::Path(source) => source.emit(handler),
            GrammarNode::Inline(source) => source.emit(handler),
            GrammarNode::Definition(source) => source.emit(handler),
        }
    }
}

impl<'a> syn::parse::Parse for GrammarAttribute<'a> {
    fn parse(input: syn::parse::ParseStream<'_>) -> syn::Result<Self> {
        if input.peek(kw::path) && input.peek2(Paren) {
            input.parse::<kw::path>()?;
            let inner;
            parenthesized!(inner in input);
            Ok(GrammarAttribute::Node(GrammarNode::Path(
                inner.parse::<GrammarNodeData<Cow<'_, Path>>>()?,
            )))
        } else if input.peek(kw::non_exhaustive) {
            input.parse::<kw::non_exhaustive>()?;
            Ok(GrammarAttribute::NonExhaustive)
        } else if input.peek(LitStr) {
            Ok(GrammarAttribute::Node(GrammarNode::Inline(
                input.parse::<GrammarNodeData<Cow<'_, str>>>()?,
            )))
        } else if input.peek(Ident) && input.peek2(Token![=]) {
            Ok(GrammarAttribute::Node(GrammarNode::Definition(
                input.parse::<GrammarNodeData<RuleDefinition<'a>>>()?,
            )))
        } else {
            Err(syn::Error::new(
                Span::call_site(),
                "invalid argument for grammar(...)",
            ))
        }
    }
}
pub struct GrammarDefinition<'a> {
    pub ident: Ident,
    pub generics: Generics,
    pub non_exhaustive: bool,
    pub nodes: Vec<GrammarNode<'a>>,
}
impl<'a> Parse for GrammarDefinition<'a> {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let item = input.parse::<DeriveInput>()?;
        let attrs = item.attrs;
        let mut grammar_sources = Vec::with_capacity(attrs.len());
        let mut non_exhaustive = false;
        parse_attributes(attrs, &mut |attr| match attr {
            GrammarAttribute::Node(value) => {
                grammar_sources.push(value);
            }
            GrammarAttribute::NonExhaustive => {
                non_exhaustive = true;
            }
        });
        Ok(GrammarDefinition {
            ident: item.ident,
            generics: item.generics,
            non_exhaustive,
            nodes: Vec::new(),
        })
    }
}
fn parse_attributes<'a, F>(attrs: Vec<Attribute>, handler: &mut F) -> syn::Result<()>
where
    F: FnMut(GrammarAttribute<'a>) -> (),
{
    for attr in attrs {
        match &attr.meta {
            Meta::List(meta) => {
                if meta.path.is_ident("grammar") {
                    handler(meta.parse_args::<GrammarAttribute>()?);
                }
            }
            Meta::NameValue(meta) => match &meta.value {
                Expr::Lit(ExprLit {
                    lit: Lit::Str(string),
                    ..
                }) => {
                    if meta.path.is_ident("grammar") {
                        let path = &string.value();
                        let root = env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".into());
                        // Check whether we can find a file at the path relative to the CARGO_MANIFEST_DIR
                        // first.
                        //
                        // If we cannot find the expected file over there, fallback to the
                        // `CARGO_MANIFEST_DIR/src`, which is the old default and kept for convenience
                        // reasons.
                        // TODO: This could be refactored once `std::path::absolute()` get's stabilized.
                        // https://doc.rust-lang.org/std/path/fn.absolute.html
                        let path = if Path::new(&root).join(path).exists() {
                            Path::new(&root).join(path)
                        } else {
                            Path::new(&root).join("src/").join(path)
                        };

                        let file_name = match path.file_name() {
                            Some(file_name) => file_name,
                            None => panic!("grammar attribute should point to a file"),
                        };
                        handler(GrammarAttribute::Node(GrammarNode::Path(
                            GrammarNodeData::new(None, path.into()),
                        )));
                    } else {
                        handler(GrammarAttribute::Node(GrammarNode::Inline(
                            GrammarNodeData::new(None, string.value().into()),
                        )))
                    }
                }
                _ => {
                    return Err(syn::Error::new(
                        Span::call_site(),
                        "grammar attribute must be a string",
                    ));
                }
            },
            _ => {}
        }
    }
    Ok(())
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GrammarNodeData<T> {
    rule: Option<String>,
    data: T,
}
#[derive(Clone, PartialEq)]
pub(crate) enum GrammarNode<'a> {
    Path(GrammarNodeData<Cow<'a, Path>>),
    Inline(GrammarNodeData<Cow<'a, str>>),
    Definition(GrammarNodeData<RuleDefinition<'a>>),
}
impl<'a> GrammarNode<'a> {
    pub fn rule(&self) -> Option<&String> {
        match self {
            GrammarNode::Path(value) => value.rule(),
            GrammarNode::Inline(value) => value.rule(),
            GrammarNode::Definition(value) => value.rule(),
        }
    }
}

impl<'a> From<GrammarDefinition<'a>> for ParsedDerive {
    fn from(value: GrammarDefinition<'a>) -> Self {
        ParsedDerive {
            name: value.ident,
            generics: value.generics,
            non_exhaustive: value.non_exhaustive,
        }
    }
}
#[derive(Clone)]
pub(crate) enum GrammarAttribute<'a> {
    Node(GrammarNode<'a>),
    NonExhaustive,
}

impl GrammarNodeData<()> {
    pub fn new<T>(rule: Option<String>, data: T) -> GrammarNodeData<T> {
        GrammarNodeData { rule, data }
    }
}
impl<T> GrammarNodeData<T> {
    pub fn data(&self) -> &T {
        &self.data
    }
    pub fn rule(&self) -> Option<&String> {
        self.rule.as_ref()
    }
}

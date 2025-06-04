use std::borrow::Cow;

use syn::LitStr;

use crate::{GrammarNode, GrammarNodeData};

use super::GrammarNodeEmitter;

impl<'a> GrammarNodeEmitter<'a> for GrammarNodeData<Cow<'a, str>> {
    fn emit<F>(self, handler: &mut F) -> syn::Result<()>
    where
        F: FnMut(GrammarNode<'a>) -> syn::Result<()>,
    {
        handler(GrammarNode::Inline(GrammarNodeData::new(
            self.rule().cloned(),
            self.data().clone(),
        )))
    }
}

impl<'a> syn::parse::Parse for GrammarNodeData<Cow<'a, str>> {
    fn parse(input: syn::parse::ParseStream<'_>) -> syn::Result<Self> {
        let inner = input.parse::<LitStr>()?.value();
        Ok(GrammarNodeData::new(None, inner.into()))
    }
}

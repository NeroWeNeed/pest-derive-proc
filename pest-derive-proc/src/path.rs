use std::{
    borrow::Cow,
    env,
    path::{Path, PathBuf},
};

use proc_macro2::Span;
use syn::LitStr;

use crate::{GrammarNode, GrammarNodeData, core::GrammarNodeEmitter};

impl<'a> GrammarNodeEmitter<'a> for GrammarNodeData<Cow<'a, Path>> {
    fn emit<F>(self, handler: &mut F) -> syn::Result<()>
    where
        F: FnMut(GrammarNode<'a>) -> syn::Result<()>,
    {
        fn emit_path<'b, F>(path: PathBuf, handler: &mut F) -> syn::Result<()>
        where
            F: FnMut(GrammarNode<'b>) -> syn::Result<()>,
        {
            let root_rule = path
                .file_stem()
                .and_then(|value| value.to_os_string().into_string().ok());
            handler(GrammarNode::Path(GrammarNodeData::new(
                root_rule,
                path.into(),
            )))?;
            Ok(())
        }
        let path = self.data();
        if path.is_file() {
            emit_path(path.to_path_buf(), handler)?;
        } else if path.is_dir() {
            let root = env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".into());
            for path in path.read_dir().unwrap() {
                let path = path.unwrap().path();
                if path.is_file() {
                    let path = if Path::new(&root).join(&path).exists() {
                        Path::new(&root).join(path)
                    } else {
                        Path::new(&root).join("src/").join(&path)
                    };
                    emit_path(path, handler)?;
                }
            }
        }
        Ok(())
    }
}

impl<'a> syn::parse::Parse for GrammarNodeData<Cow<'a, Path>> {
    fn parse(input: syn::parse::ParseStream<'_>) -> syn::Result<Self> {
        let path = input.parse::<LitStr>()?.value();
        let root = env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".into());
        let path = std::path::absolute(Path::new(&root).join(path))
            .map_err(|err| syn::Error::new(Span::call_site(), err))?;
        Ok(GrammarNodeData::new(None, path.into()))
    }
}

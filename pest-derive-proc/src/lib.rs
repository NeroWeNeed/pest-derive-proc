mod core;
mod inline;
mod path;
use core::*;
mod constructor;

use std::collections::VecDeque;

use pest_generator::docs;
use pest_generator::generator::generate;
use pest_meta::parser::{self, Rule, rename_meta_rule};
use pest_meta::{optimizer, unwrap_or_report, validator};
use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{Ident, parse_macro_input};

#[proc_macro_derive(ParserProc, attributes(grammar))]
pub fn derive_parser(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let definition = parse_macro_input!(input as GrammarDefinition<'_>);
    let mut data = String::new();
    let mut paths = vec![];
    let mut nodes = VecDeque::new();
    let mut rules = vec![];
    nodes.extend(definition.nodes.clone());
    while let Some(node) = nodes.pop_front() {
        match node {
            GrammarNode::Path(node) => {
                let path = node.data();
                let content = match std::fs::read_to_string(path) {
                    Ok(data) => data,
                    Err(error) => panic!("error opening {:?}: {}", path.file_name(), error),
                };
                data.push_str(&content);
                paths.push(path.to_path_buf());
            }
            GrammarNode::Inline(node) => {
                data.push_str(node.data());
            }
            GrammarNode::Definition(node) => {
                let output = if let Some(rule) = node.rule() {
                    if !rule.starts_with("_") {
                        let rule = rule.clone();
                        let mut rule_group = Vec::new();
                        match node.build(&mut |node| {
                            if let Some(rule) = node.rule() {
                                rule_group.push(rule.clone());
                            }
                            nodes.push_front(node);
                            Ok(())
                        }) {
                            Ok(output) => {
                                rules.push((rule, rule_group));
                                Ok(output)
                            }
                            err => err,
                        }
                    } else {
                        node.build(&mut |node| {
                            nodes.push_front(node);
                            Ok(())
                        })
                    }
                } else {
                    node.build(&mut |node| {
                        nodes.push_front(node);
                        Ok(())
                    })
                };
                match output {
                    Ok(node) => {
                        nodes.push_front(node);
                    }
                    Err(err) => {
                        return err.to_compile_error().into();
                    }
                }
            }
        };
    }
    let pairs = match parser::parse(Rule::grammar_rules, &data) {
        Ok(pairs) => pairs,
        Err(error) => panic!("error parsing \n{}", error.renamed_rules(rename_meta_rule)),
    };

    let defaults = unwrap_or_report(validator::validate_pairs(pairs.clone()));
    let doc_comment = docs::consume(pairs.clone());
    let ast = unwrap_or_report(parser::consume_rules(pairs));
    let optimized = optimizer::optimize(ast);
    let parsed_derive = definition.into();
    let mut output = generate(
        parsed_derive,
        paths,
        optimized,
        defaults,
        &doc_comment,
        true,
    );
    output.extend(generate_rule_macros(&rules));
    output.into()
}
fn generate_rule_macros(routes: &[(String, Vec<String>)]) -> TokenStream {
    if !routes.is_empty() {
        let route_macro_ident = Ident::new("rules", Span::call_site());
        let (root_rules, root_rules_items): (Vec<_>, Vec<_>) = routes
            .into_iter()
            .map(|(root, items)| {
                (
                    Ident::new(root, Span::call_site()),
                    items
                        .into_iter()
                        .map(|item| Ident::new(item, Span::call_site()))
                        .collect::<Vec<_>>(),
                )
            })
            .unzip();
        quote! {
            macro_rules! #route_macro_ident {
                #((#root_rules,$rule:ident,$($tokens:tt)*) => {
                    match $rule {
                        #(Rule::#root_rules_items => Some(super::#root_rules_items$($tokens)*),)*
                        _  => None
                    }
                });*
            }
        }
    } else {
        quote! {}
    }
}

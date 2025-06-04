use core::fmt;
use std::{borrow::Cow, fmt::Write, path::Path, usize};

use proc_macro2::Span;
use syn::{
    Ident, LitChar, LitInt, LitStr, Token, braced, parenthesized,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    token::{Brace, Paren},
};

use crate::{GrammarNode, GrammarNodeData, core::GrammarNodeEmitter};

trait RuleStep<'a> {
    fn step<F>(&self, handler: &mut F, output: &mut dyn Write) -> fmt::Result
    where
        F: RuleVisitor<'a>;
}
trait RuleVisitor<'a> {
    fn visit(&mut self, evt: GrammarNodeTerminal<'a>);
}

#[derive(Clone)]
enum Node<'a> {
    Sequence(Punctuated<Node<'a>, Token![~]>),
    OrderedChoice(Punctuated<Node<'a>, Token![|]>),
    Repetition(Box<Node<'a>>, usize, usize),
    Predicate(Box<Node<'a>>, PredicateType),
    String(LitStr),
    CharacterRange(LitChar, LitChar),
    RuleRef(String),
    Collection(Vec<Node<'a>>),
    Terminal(GrammarNodeTerminal<'a>),
    Empty,
}
#[derive(Debug, Clone)]
enum GrammarNodeTerminal<'a> {
    Path(GrammarNodeData<Cow<'a, Path>>),
    Inline(GrammarNodeData<Cow<'a, str>>),
}
impl<'a> TryFrom<GrammarNode<'a>> for GrammarNodeTerminal<'a> {
    type Error = syn::Error;

    fn try_from(value: GrammarNode<'a>) -> Result<Self, Self::Error> {
        match value {
            GrammarNode::Path(value) => Ok(GrammarNodeTerminal::Path(value)),
            GrammarNode::Inline(value) => Ok(GrammarNodeTerminal::Inline(value)),
            GrammarNode::Definition(_) => Err(syn::Error::new(
                Span::call_site(),
                "embedding definitions into emissions is prohibited",
            )),
        }
    }
}
impl<'a> From<GrammarNodeTerminal<'a>> for GrammarNode<'a> {
    fn from(value: GrammarNodeTerminal<'a>) -> Self {
        match value {
            GrammarNodeTerminal::Path(value) => GrammarNode::Path(value),
            GrammarNodeTerminal::Inline(value) => GrammarNode::Inline(value),
        }
    }
}

fn handle_rule_fn<'b, 'c>(ident: Ident, input: ParseStream<'b>) -> syn::Result<Node<'c>> {
    match ident.to_string().as_str() {
        "path" => {
            let value = GrammarNodeData::<Cow<'_, Path>>::parse(input)?;
            if input.peek2(LitChar) {
                input.parse::<Token![,]>()?;
                let delimiter = input.parse::<LitChar>()?.value();
                let mut output = Vec::new();

                if delimiter == '|' {
                    let mut punctuated: Punctuated<Node, Token![|]> = Punctuated::new();
                    value.emit(&mut |node| {
                        if let Some(root_rule) = node.clone().rule() {
                            output.push(Node::Terminal(node.try_into()?));
                            punctuated.push_value(Node::RuleRef(root_rule.clone()));
                        } else {
                            output.push(Node::Terminal(node.try_into()?));
                        }
                        Ok(())
                    })?;
                    output.push(Node::OrderedChoice(punctuated));
                    Ok(Node::Collection(output))
                } else if delimiter == '~' {
                    let mut punctuated: Punctuated<Node, Token![~]> = Punctuated::new();
                    value.emit(&mut |node| {
                        if let Some(root_rule) = node.clone().rule() {
                            output.push(Node::Terminal(node.try_into()?));
                            punctuated.push_value(Node::RuleRef(root_rule.clone()));
                        } else {
                            output.push(Node::Terminal(node.try_into()?));
                        }
                        Ok(())
                    })?;
                    output.push(Node::Sequence(punctuated));
                    Ok(Node::Collection(output))
                } else {
                    return Err(syn::Error::new(
                        Span::call_site(),
                        "invalid delimiter for path",
                    ));
                }
            } else {
                let mut output = None;
                value.emit(&mut |node| {
                    if output.is_none() {
                        output = Some(node);
                        Ok(())
                    } else {
                        Err(syn::Error::new(
                            Span::call_site(),
                            "path() requires delimiter when a directory is specified",
                        ))
                    }
                })?;
                if let Some(node) = output {
                    if let Some(root_rule) = node.clone().rule() {
                        Ok(Node::Collection(vec![
                            Node::Terminal(node.try_into()?),
                            Node::RuleRef(root_rule.clone()),
                        ]))
                    } else {
                        Ok(Node::Terminal(node.try_into()?))
                    }
                } else {
                    Ok(Node::Empty)
                }
            }
        }
        _ => {
            return Err(syn::Error::new(
                Span::call_site(),
                "path() requires delimiter when a directory is specified",
            ));
        }
    }
}

impl<'a> RuleStep<'a> for Node<'a> {
    fn step<F>(&self, handler: &mut F, output: &mut dyn Write) -> fmt::Result
    where
        F: RuleVisitor<'a>,
    {
        match self {
            Node::Sequence(punctuated) => {
                if punctuated.len() > 1 {
                    output.write_char('(')?;

                    let mut iter = punctuated.iter().peekable();
                    while let Some(node) = iter.next() {
                        node.step(handler, output)?;
                        if iter.peek().is_some() {
                            output.write_char('~')?;
                        }
                    }
                    output.write_char(')')?;
                } else {
                    let mut iter = punctuated.iter().peekable();
                    while let Some(node) = iter.next() {
                        node.step(handler, output)?;
                        if iter.peek().is_some() {
                            output.write_char('~')?;
                        }
                    }
                }
            }
            Node::OrderedChoice(punctuated) => {
                if punctuated.len() > 1 {
                    output.write_char('(')?;

                    let mut iter = punctuated.iter().peekable();
                    while let Some(node) = iter.next() {
                        node.step(handler, output)?;
                        if iter.peek().is_some() {
                            output.write_char('|')?;
                        }
                    }
                    output.write_char(')')?;
                } else {
                    let mut iter = punctuated.iter().peekable();
                    while let Some(node) = iter.next() {
                        node.step(handler, output)?;
                        if iter.peek().is_some() {
                            output.write_char('|')?;
                        }
                    }
                }
            }
            Node::Repetition(node, start, end) => {
                node.step(handler, output)?;
                if start == end {
                    output.write_fmt(format_args!("{{{}}}", start))?;
                } else {
                    match (*start, *end) {
                        (0, 1) => {
                            output.write_char('?')?;
                        }
                        (0, usize::MAX) => {
                            output.write_char('*')?;
                        }
                        (1, usize::MAX) => {
                            output.write_char('+')?;
                        }
                        (start, end) => {
                            output.write_fmt(format_args!("{{{},{}}}", start, end))?;
                        }
                    }
                }
            }
            Node::Predicate(node, predicate_type) => {
                match predicate_type {
                    PredicateType::Positive => output.write_char('&'),
                    PredicateType::Negative => output.write_char('!'),
                    PredicateType::Casing => output.write_char('^'),
                }?;
                node.step(handler, output)?;
            }
            Node::String(lit_str) => {
                output.write_fmt(format_args!("\"{}\"", lit_str.value()))?;
            }
            Node::CharacterRange(start, end) => {
                output.write_fmt(format_args!("'{}'..'{}'", start.value(), end.value()))?;
            }
            Node::RuleRef(ident) => {
                output.write_fmt(format_args!("{}", ident))?;
            }
            Node::Empty => {}
            Node::Collection(nodes) => {
                for node in nodes {
                    node.step(handler, output)?;
                }
            }
            Node::Terminal(grammar_node) => handler.visit(grammar_node.clone()),
        }
        Ok(())
    }
}
impl<'a> Parse for Node<'a> {
    fn parse(input: &syn::parse::ParseBuffer<'_>) -> syn::Result<Self> {
        fn parse_data<'b, 'c>(input: ParseStream<'b>) -> syn::Result<Node<'c>> {
            if input.peek(Token![&]) {
                input.parse::<Token![&]>()?;
                Ok(Node::Predicate(
                    Box::new(input.parse::<Node>()?),
                    PredicateType::Positive,
                ))
            } else if input.peek(Token![!]) {
                input.parse::<Token![!]>()?;
                Ok(Node::Predicate(
                    Box::new(input.parse::<Node>()?),
                    PredicateType::Negative,
                ))
            } else if input.peek(LitStr) {
                Ok(Node::String(input.parse::<LitStr>()?))
            } else if input.peek(Token![^]) && input.peek2(LitStr) {
                input.parse::<Token![^]>()?;
                Ok(Node::Predicate(
                    Box::new(Node::String(input.parse::<LitStr>()?)),
                    PredicateType::Casing,
                ))
            } else if input.peek(Ident) {
                let ident = input.parse::<Ident>()?;

                if input.peek(Paren) {
                    let inner;
                    parenthesized!(inner in input);
                    handle_rule_fn(ident, &inner)
                } else {
                    Ok(Node::RuleRef(ident.to_string()))
                }
            } else if input.peek(LitChar) {
                let start = input.parse::<LitChar>()?;
                input.parse::<Token![..]>()?;
                let end = input.parse::<LitChar>()?;
                Ok(Node::CharacterRange(start, end))
            } else if input.peek(Paren) {
                let inner;
                parenthesized!(inner in input);
                if inner.is_empty() {
                    return Ok(Node::Empty);
                }
                let first_item = inner.parse::<Node>()?;
                if inner.is_empty() {
                    return Ok(first_item);
                }
                if inner.peek(Token![|]) {
                    inner.parse::<Token![|]>()?;
                    let next = inner.parse_terminated(Node::parse, Token![|])?;
                    let mut result = Punctuated::new();
                    result.push_value(first_item);
                    result.extend(next);
                    return Ok(Node::OrderedChoice(result));
                } else if inner.peek(Token![~]) {
                    inner.parse::<Token![~]>()?;
                    let next = inner.parse_terminated(Node::parse, Token![~])?;
                    let mut result = Punctuated::new();
                    result.push_value(first_item);
                    result.extend(next);
                    return Ok(Node::Sequence(result));
                } else {
                    return Err(syn::Error::new(
                        Span::call_site(),
                        "invalid group delimiter",
                    ));
                }
            } else {
                Err(syn::Error::new(
                    Span::call_site(),
                    "invalid token for grammar",
                ))
            }
        }
        fn parse_footer<'b, 'c>(node: Node<'c>, input: ParseStream<'b>) -> syn::Result<Node<'c>> {
            if input.peek(Token![*]) {
                input.parse::<Token![*]>()?;
                Ok(Node::Repetition(Box::new(node), 0, usize::MAX))
            } else if input.peek(Token![*]) {
                input.parse::<Token![+]>()?;
                Ok(Node::Repetition(Box::new(node), 1, usize::MAX))
            } else if input.peek(Token![?]) {
                input.parse::<Token![*]>()?;
                Ok(Node::Repetition(Box::new(node), 0, 1))
            } else if input.peek(Brace) {
                let inner;
                braced!(inner in input);
                let start = inner.parse::<LitInt>()?.base10_parse()?;
                if inner.peek(Token![,]) {
                    inner.parse::<Token![,]>()?;
                    let end = inner.parse::<LitInt>()?.base10_parse()?;
                    Ok(Node::Repetition(Box::new(node), start, end))
                } else if inner.is_empty() {
                    Ok(Node::Repetition(Box::new(node), start, start))
                } else {
                    Err(syn::Error::new(
                        Span::call_site(),
                        "invalid token for grammar",
                    ))
                }
            } else {
                Ok(node)
            }
        }
        parse_footer(parse_data(input)?, input)
    }
}

#[derive(Debug, Clone, Copy)]
enum PredicateType {
    Positive,
    Negative,
    Casing,
}
#[derive(Default, Debug, Clone, Copy)]
enum RuleType {
    #[default]
    Regular,
    Atomic,
    CompoundAtomic,
    Silent,
    NonAtomic,
}
impl Parse for RuleType {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        if input.peek(Token![@]) {
            input.parse::<Token![@]>()?;
            Ok(Self::Atomic)
        } else if input.peek(Token![$]) {
            input.parse::<Token![$]>()?;
            Ok(Self::CompoundAtomic)
        } else if input.peek(Token![_]) {
            input.parse::<Token![_]>()?;
            Ok(Self::Silent)
        } else if input.peek(Token![!]) {
            input.parse::<Token![!]>()?;
            Ok(Self::NonAtomic)
        } else {
            Ok(Self::Regular)
        }
    }
}
#[derive(Clone)]
pub struct RuleDefinition<'a> {
    ty: RuleType,
    content: Node<'a>,
}
impl<'a> RuleStep<'a> for RuleDefinition<'a> {
    fn step<F>(&self, handler: &mut F, output: &mut dyn Write) -> fmt::Result
    where
        F: RuleVisitor<'a>,
    {
        match self.ty {
            RuleType::Regular => {}
            RuleType::Atomic => output.write_char('@')?,
            RuleType::CompoundAtomic => output.write_char('$')?,
            RuleType::Silent => output.write_char('_')?,
            RuleType::NonAtomic => output.write_char('!')?,
        };
        output.write_char('{')?;
        self.content.step(handler, output)?;
        output.write_char('}')?;
        Ok(())
    }
}

impl<'a> Parse for RuleDefinition<'a> {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let rule_type = input.parse::<RuleType>()?;
        let inner;
        braced!(inner in input);
        if inner.is_empty() {
            return Ok(Self {
                ty: rule_type,
                content: Node::Empty,
            });
        }
        let first_item = inner.parse::<Node>()?;
        if inner.is_empty() {
            return Ok(Self {
                ty: rule_type,
                content: first_item,
            });
        }
        if inner.peek(Token![|]) {
            inner.parse::<Token![|]>()?;
            let next = inner.parse_terminated(Node::parse, Token![|])?;
            let mut result = Punctuated::new();
            result.push_value(first_item);
            result.extend(next);
            return Ok(Self {
                ty: rule_type,
                content: Node::OrderedChoice(result),
            });
        } else if inner.peek(Token![~]) {
            inner.parse::<Token![~]>()?;
            let next = inner.parse_terminated(Node::parse, Token![~])?;
            let mut result = Punctuated::new();
            result.push_value(first_item);
            result.extend(next);
            return Ok(Self {
                ty: rule_type,
                content: Node::Sequence(result),
            });
        } else {
            return Err(syn::Error::new(
                Span::call_site(),
                "invalid group delimiter",
            ));
        }
    }
}

impl<'a> PartialEq for GrammarNodeData<RuleDefinition<'a>> {
    fn eq(&self, other: &Self) -> bool {
        self.rule().eq(&other.rule())
    }
}
impl<'a> RuleStep<'a> for GrammarNodeData<RuleDefinition<'a>> {
    fn step<F>(&self, handler: &mut F, output: &mut dyn Write) -> fmt::Result
    where
        F: RuleVisitor<'a>,
    {
        if let Some(rule) = self.rule() {
            output.write_str(rule.strip_prefix("_").unwrap_or(rule))?;
            output.write_char('=')?;
            self.data().step(handler, output)
        } else {
            Ok(())
        }
    }
}
impl<'a> Parse for GrammarNodeData<RuleDefinition<'a>> {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let rule = input.parse::<Ident>()?.to_string();
        input.parse::<Token![=]>()?;
        Ok(GrammarNodeData::new(
            Some(rule.to_string()),
            input.parse::<RuleDefinition>()?,
        ))
    }
}
#[derive(Default)]
struct RuleStepper<'a> {
    pub sources: Vec<GrammarNode<'a>>,
}
impl<'a> RuleVisitor<'a> for RuleStepper<'a> {
    fn visit(&mut self, value: GrammarNodeTerminal<'a>) {
        self.sources.push(value.into());
    }
}

impl<'a> GrammarNodeEmitter<'a> for GrammarNodeData<RuleDefinition<'a>> {
    fn emit<F>(self, handler: &mut F) -> syn::Result<()>
    where
        F: FnMut(GrammarNode<'a>) -> syn::Result<()>,
    {
        let result = self.build(&mut *handler)?;
        handler(result)
    }
}

impl<'a> GrammarNodeData<RuleDefinition<'a>> {
    pub fn build<F>(self, handler: &mut F) -> syn::Result<GrammarNode<'a>>
    where
        F: FnMut(GrammarNode<'a>) -> syn::Result<()>,
    {
        let mut output = String::new();
        let mut stepper = RuleStepper::default();

        self.step(&mut stepper, &mut output)
            .map_err(|err| syn::Error::new(Span::call_site(), err))?;
        let RuleStepper { sources } = stepper;
        for source in sources.into_iter() {
            handler(source)?;
        }
        Ok(GrammarNode::Inline(GrammarNodeData::new(
            self.rule().cloned(),
            output.into(),
        )))
    }
}

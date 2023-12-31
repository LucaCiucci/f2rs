use f2rs_parser_combinator::prelude::*;

use f2rs_parser_combinator::provided::text::rusty::identifier;
use f2rs_parser_combinator::{tokenization::{TextSource, Parser}, provided::text::rusty::Identifier};

use super::elements::{spaced, keyword, eol_or_comment, LineComment};
use super::element;
use super::{elements::{Type, type_}, Element};

#[derive(Debug, Clone)]
pub struct Function<Span> {
    pub name: Identifier<Span>,
    pub opening_comment: Option<LineComment<Span>>,
    pub closing_comment: Option<LineComment<Span>>,
    pub args: Vec<Identifier<Span>>,
    pub return_type: Option<Type<Span>>,
    pub return_variable: Option<Identifier<Span>>,
    pub body: Vec<Element<Span>>,
}

pub fn function_end<S: TextSource>() -> impl Parser<S, Token = Option<LineComment<S::Span>>> {
    (
        spaced(keyword("end")),
        (spaced(keyword("function")), spaced(identifier()).optional()).optional(),
        eol_or_comment(),
    )
        .map(|(_, _, c)| c)
}

pub fn function<S: TextSource>() -> impl Parser<S, Token = Function<S::Span>> {
    (
        spaced(type_()).optional(),
        spaced(keyword("function")),
        spaced(identifier()),
        spaced('('),
        separated(spaced(identifier()), spaced(','), 0..),
        spaced(')'),
        (
            spaced(keyword("result")),
            spaced('('),
            spaced(identifier()),
            spaced(')'),
        ).map(|(_, _, id, _)| id).optional(),
        eol_or_comment(),
        many_until(
            element(),
            function_end(),
            0..,
        )
    ).map(|(ty, _, name, _, args, _, r, oc, (body, cc))| {
        let cc = if let Some(cc) = cc {
            cc
        } else {
            None
        };

        Function {
            name,
            opening_comment: oc,
            closing_comment: cc,
            args,
            return_type: ty,
            return_variable: r,
            body,
        }
    })
}
use f2rs_parser_combinator::prelude::*;

pub fn keyword<S: TextSource>(keyword: &'static str) -> impl Parser<S, Token = S::Span> {
    StringMatch::exact(keyword, false).map(|m| m.span)
}

// TODO tests

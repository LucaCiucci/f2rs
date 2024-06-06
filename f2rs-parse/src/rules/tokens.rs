use std::{iter::once, ops::RangeBounds};

use enum_as_inner::EnumAsInner;
use enum_iterator::Sequence;
use f2rs_parser_combinator::prelude::*;
use f2rs_parse_derive::syntax_rule;

use crate::{Cfg, Standard::*};

#[derive(Debug, Clone)]
pub struct Keyword<Span>(pub Name<Span>);

#[syntax_rule(
    F18V007r1 rule "keyword" #516 : "is name",
)]
pub fn keyword<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = Keyword<S::Span>> + 'a {
    name(cfg, false).map(Keyword)
}

#[syntax_rule(
    F18V007r1,
)]
pub fn kw<'a, S: TextSource + 'a>(keyword: &'static str, cfg: &'a Cfg) -> impl Parser<S, Token = Keyword<S::Span>> + 'a {
    StringMatch::exact(keyword, false).map(|m| Keyword(Name(m)))
}



// TODO ???
pub fn continuation<'a, S: TextSource + 'a>() -> impl Parser<S, Token = ()> + 'a {
    (
        '&', blanks(0..), nl.map(|_| ()).or(line_comment().map(|_| ())), // TODO keep comments
        blanks(0..), Char::exact('&').optional(),
        blanks(0..) // TODO remove
    ).map(|_| ())
}



// TODO ???
pub fn eol_or_comment<S: TextSource>() -> impl Parser<S, Token = Option<LineComment<S::Span>>> {
    alt! {
        eol().map(|_| None),
        (space(0), line_comment()).map(|(_, c)| Some(c)),
    }
}

// TODO ???
pub fn statement_termination<'a, S: TextSource + 'a>() -> impl Parser<S, Token = Option<LineComment<S::Span>>> + 'a {
    alt!(
        eol_or_comment(),
        (
            space(0),
            Char::exact(';'),
            space(0),
            eol_or_comment(),
        ).map(|(_, _, _, c)| c),
    )
}

// TODO test
#[syntax_rule(
    F18V007r1 rule "lbracket" #771 : "is [",
)]
pub fn lbracket<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ()> + 'a {
    '['.map(|_| ())
}

// TODO test
#[syntax_rule(
    F18V007r1 rule "rbracket" #772 : "is ]",
)]
pub fn rbracket<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ()> + 'a {
    ']'.map(|_| ())
}

#[derive(Debug, Clone)]
pub struct EmptyLines {
    pub count: usize,
}

pub fn empty_lines<'a, S: TextSource + 'a>() -> impl Parser<S, Token = EmptyLines> + 'a {
    fold_many(
        (many(' ', 0..), nl),
        || 0,
        |count, _| (count + 1, true),
        1..,
    )
    .map(|count| EmptyLines { count })
}

#[cfg(test)]
mod test {
    use crate::rules::test_configs;

    use super::*;

    // TODO ???
    #[test]
    fn test_comment_start() {
        comment_start().parse("!").expect("failed to parse");
    }

    // TODO ???
    #[test]
    fn test_line_comment() {
        let r = line_comment().parse("! hello world\n").unwrap().0;
        assert_eq!(r.text, " hello world");
    }
}

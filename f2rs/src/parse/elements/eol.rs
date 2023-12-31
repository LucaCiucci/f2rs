use f2rs_parser_combinator::prelude::*;

use super::*;

/// End of line
pub fn eol<S: TextSource>() -> impl Parser<S, Token = ()> {
    alt! {
        StringMatch::exact("\r\n", true),
        StringMatch::exact("\n", true),
    }
    .map(|_| ())
    .or(eof())
}

pub fn eol_or_comment<S: TextSource>() -> impl Parser<S, Token = Option<LineComment<S::Span>>> {
    alt! {
        eol().map(|_| None),
        spaced(line_comment()).map(|c| Some(c)),
    }
}

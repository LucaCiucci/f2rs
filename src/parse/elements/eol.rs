use riddle::prelude::*;

use super::*;

/// End of line
pub fn eol<S: TextSource>() -> impl Parser<S, Token = ()> {
    ('\n', Char::exact('\r').optional()).map(|_| ()).or(eof())
}

pub fn eol_or_comment<S: TextSource>() -> impl Parser<S, Token = Option<LineComment<S::Span>>> {
    alt! {
        eol().map(|_| None),
        line_comment().map(|c| Some(c)),
    }
}
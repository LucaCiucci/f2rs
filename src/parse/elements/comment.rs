use riddle::{tokenization::{TextSource, Parser}, provided::{common::many_until, text::Char}};

use super::*;

#[derive(Debug, Clone)]
pub struct LineComment<Span> {
    pub text: String,
    pub span: Span,
}

pub fn line_comment<S: TextSource>() -> impl Parser<S, Token = LineComment<S::Span>> {
    spaced('!'.then(|bang| many_until(Char::<S::Span>::any(), '\n', 0..).map(move |(chars, _newline)| {
        let bang = bang.clone();
        let span = if let Some(last) = chars.last() {
            S::joint_span(bang.span, last.span.clone())
        } else {
            bang.span
        };

        LineComment {
            text: chars.into_iter().map(|c| c.value).collect(),
            span,
        }
    })))
}
use riddle::{
    provided::{
        common::many_until,
        text::{Char, ExactMatch},
    },
    tokenization::{Parser, TextSource},
};

use super::*;

#[derive(Debug, Clone)]
pub struct LineComment<Span> {
    pub text: String,
    pub span: Span,
}

pub fn comment_start<S: TextSource>() -> impl Parser<S, Token = ExactMatch<S::Span>> {
    // we accept both ! and c as comment starters
    //ExactMatch::exact("!", false).or(ExactMatch::exact("c ", true))
    ExactMatch::exact("!", false)
}

pub fn line_comment<S: TextSource>() -> impl Parser<S, Token = LineComment<S::Span>> {
    spaced(comment_start().then(|bang| {
        many_until(Char::<S::Span>::any(), '\n', 0..).map(move |(chars, _newline)| {
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
        })
    }))
}

#[cfg(test)]
mod tests {
    use riddle::prelude::*;

    use super::*;

    #[test]
    fn test_comment_start() {
        comment_start().parse("!").0.expect("failed to parse");
    }

    #[test]
    fn test_line_comment() {
        let r = line_comment().parse("! hello world\n").0.unwrap();
        assert_eq!(r.text, " hello world");
    }
}
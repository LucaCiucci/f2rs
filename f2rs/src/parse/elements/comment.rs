use f2rs_parser_combinator::{
    provided::{
        common::{many_until, many, fold_many},
        text::{Char, StringMatch},
    },
    tokenization::{Parser, TextSource},
};

#[derive(Debug, Clone)]
pub struct LineComment<Span> {
    pub text: String,
    pub span: Span,
}

pub fn comment_start<S: TextSource>() -> impl Parser<S, Token = S::Span> {
    // we accept both ! and c as comment starters
    //ExactMatch::exact("!", false).or(ExactMatch::exact("c ", true))
    fold_many(
        StringMatch::exact("!", false),
        || S::null_span(),
        |s, m| (S::merge_span(s, m.span), true),
        1..,
    )
}

pub fn line_comment<S: TextSource>() -> impl Parser<S, Token = LineComment<S::Span>> {
    comment_start().then(|bang_span: S::Span| {
        many_until(Char::<S::Span>::any(), '\n', 0..).map(move |(chars, _newline)| {
            let bang_span = bang_span.clone();
            let span = if let Some(last) = chars.last() {
                S::merge_span(bang_span, last.span.clone())
            } else {
                bang_span
            };

            LineComment {
                text: chars.into_iter().map(|c| c.value).collect(),
                span,
            }
        })
    })
}

#[cfg(test)]
mod tests {
    use f2rs_parser_combinator::prelude::*;

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

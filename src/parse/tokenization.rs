use riddle::{tokenization::{TextSource, TokenParser}, provided::{common::{many_until, fold_many, many}, text::{Char, spaced}}};



pub mod literals {
    use riddle::alt;
    use riddle::provided::text::{rusty, Literal};
    use riddle::provided::text::string::string_literal_impl;

    use riddle::tokenization::{TextSource, TokenParser};
    use rusty::string::StringLiteral;

    pub fn string_literal<S: TextSource>() -> impl TokenParser<S, Token = StringLiteral<S::Span>> {
        string_literal_impl(|_| 0..)
    }

    // TODO true, false, null, etc.
    pub fn fortran_literal<S: TextSource>() -> impl TokenParser<S, Token = Literal<S::Span>> {
        alt! {
            string_literal().map(Literal::String),
            rusty::numeric_literal().map(Literal::Number),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LineComment<Span> {
    pub text: String,
    pub span: Span,
}

pub fn line_comment<S: TextSource>() -> impl TokenParser<S, Token = LineComment<S::Span>> {
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

#[derive(Debug, Clone)]
pub struct EmptyLines {
    pub count: usize,
}

pub fn empty_lines<S: TextSource>() -> impl TokenParser<S, Token = EmptyLines> {
    fold_many((many(' ', 0..), '\n'), || 0, |count, _| (count + 1, true), 1..).map(|count| EmptyLines { count })
}
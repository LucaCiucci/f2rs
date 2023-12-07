use riddle::provided::text::rusty::StringLiteral;

use crate::parse::eol_or_comment;

use super::*;

#[derive(Debug, Clone)]
pub enum PrintFormat<Span> {
    Star,
    FormatString(StringLiteral<Span>),
}

pub fn print_format<S: TextSource>() -> impl Parser<S, Token = PrintFormat<S::Span>> {
    alt! {
        spaced('*').map(|_| PrintFormat::Star),
        spaced(string_literal()).map(PrintFormat::FormatString),
    }
}

#[derive(Debug, Clone)]
pub struct PrintStatement<Span> {
    pub format: PrintFormat<Span>,
    pub items: Vec<Expression<Span>>,
}

pub fn print_statement<S: TextSource>() -> impl Parser<S, Token = PrintStatement<S::Span>> {
    (
        spaced(keyword("print")),
        print_format(),
        spaced(','),
        separated(expression(), spaced(','), 0..),
        eol_or_comment(),
    ).map(|(_, format, _, items, _)| PrintStatement { format, items })
}
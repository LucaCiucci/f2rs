

pub mod number;
pub mod string;

pub use number::*;
pub use string::*;

use crate::{alt, tokenization::{TextSource, ParserCore, Parser}};

#[derive(Debug, Clone)]
pub enum Literal<Span> {
    Number(NumericLiteral<Span>),
    String(StringLiteral<Span>),
}

pub fn literal<S: TextSource>() -> impl Parser<S, Token = Literal<S::Span>> {
    alt! {
        numeric_literal().map(Literal::Number),
        string_literal().map(Literal::String),
    }
}
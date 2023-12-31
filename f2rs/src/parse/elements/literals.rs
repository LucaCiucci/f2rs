use enum_as_inner::EnumAsInner;
use f2rs_parser_combinator::{
    prelude::*,
    provided::text::rusty::{numeric_literal, string_literal_impl, NumericLiteral, StringLiteral},
};

#[derive(Debug, Clone, EnumAsInner)]
pub enum Literal<Span> {
    String(StringLiteral<Span>),
    Number(NumericLiteral<Span>),
    True,
    False,
}

pub fn string_literal<S: TextSource>() -> impl Parser<S, Token = StringLiteral<S::Span>> {
    string_literal_impl(|_| 0..)
}

pub fn fortran_literal<S: TextSource>() -> impl Parser<S, Token = Literal<S::Span>> {
    alt! {
        StringMatch::exact(".true.", false).map(|_| Literal::True),
        StringMatch::exact(".false.", false).map(|_| Literal::False),
        string_literal().map(Literal::String),
        numeric_literal().map(Literal::Number),
    }
}

// TODO tests

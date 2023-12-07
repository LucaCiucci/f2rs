use riddle::{prelude::*, provided::text::rusty::{string_literal_impl, StringLiteral, NumericLiteral, numeric_literal}};
use enum_as_inner::EnumAsInner;

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
        ExactMatch::exact(".true.", false).map(|_| Literal::True),
        ExactMatch::exact(".false.", false).map(|_| Literal::False),
        string_literal().map(Literal::String),
        numeric_literal().map(Literal::Number),
    }
}
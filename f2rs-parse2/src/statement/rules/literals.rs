

use super::*;



#[derive(Debug, Clone, EnumAsInner)]
pub enum ComplexPart<Span> {
    Literal(NonComplexLiteralConstant<Span>),
    Name(Name<Span>),
}

#[syntax_rule(
    F18V007r1 rule "real-part" #719 :
    "is signed-int-literal-constant"
    "or signed-real-literal-constant"
    "or named-constant",
    F18V007r1 rule "imag-part" #720 :
    "is signed-int-literal-constant"
    "or signed-real-literal-constant"
    "or named-constant",
)]
pub fn complex_part<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ComplexPart<MultilineSpan>> + 'a {
    // TODO this is more general, the actual rule checks shall be done some another phase
    alt! {
        non_complex_literal_constant().map(ComplexPart::Literal),
        name().map(ComplexPart::Name),
    }
}

#[derive(Debug, Clone)]
pub struct ComplexLiteralConstant<Span> {
    pub real_part: ComplexPart<Span>,
    pub imaginary_part: ComplexPart<Span>,
}

#[syntax_rule(
    F18V007r1 rule "complex-literal-constant" #718 : "is ( real-part , imag-part )",
)]
pub fn complex_literal_constant<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ComplexLiteralConstant<MultilineSpan>> + 'a {
    (
        delim('('),
        complex_part(cfg),
        comma(),
        complex_part(cfg),
        delim(')'),
    )
        .map(|(_, real_part, _, imaginary_part, _)| ComplexLiteralConstant {
            real_part,
            imaginary_part,
        })
}
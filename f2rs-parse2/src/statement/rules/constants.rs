use crate::tokens::rules::{BozLiteralConstant, CharLiteralConstant, IntLiteralConstant, LogicalLiteralConstant, RealLiteralConstant};

use super::*;

#[derive(Debug, Clone, EnumAsInner)]
pub enum Constant<Span> {
    Literal(LiteralConstant<Span>),
    Named(NamedConstant<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "constant" #604 :
    "is literal-constant"
    "or named-constant",
)]
pub fn constant<S: Lexed>(source: S) -> PResult<Constant<MultilineSpan>, S> {
    alt!(
        for S =>
        literal_constant.map(Constant::Literal),
        named_constant.map(Constant::Named),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct NamedConstant<Span> {
    pub name: Name<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "named-constant" #606 : "is name",
)]
pub fn named_constant<S: Lexed>(source: S) -> PResult<NamedConstant<MultilineSpan>, S> {
    name().map(|name| NamedConstant { name }).parse(source)
}

#[derive(Debug, Clone)]
pub struct IntConstant<Span>(pub Constant<Span>);

#[doc = s_rule!(
    F18V007r1 rule "int-constant" #607 : "is constant",
)]
pub fn int_constant<S: Lexed>(source: S) -> PResult<IntConstant<MultilineSpan>, S> {
    constant.map(IntConstant).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum LiteralConstant<Span> {
    Int(IntLiteralConstant<Span>),
    Real(RealLiteralConstant<Span>),
    Complex(ComplexLiteralConstant<Span>),
    Logical(LogicalLiteralConstant<Span>),
    Char(CharLiteralConstant<Span>),
    Boz(BozLiteralConstant<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "literal-constant" #605 :
    "is int-literal-constant"
    "or real-literal-constant"
    "or complex-literal-constant"
    "or logical-literal-constant"
    "or char-literal-constant"
    "or boz-literal-constant",
)]
pub fn literal_constant<S: Lexed>(source: S) -> PResult<LiteralConstant<MultilineSpan>, S> {
    // NOTE: The order of the alternatives is important, is different from the standard
    alt!(
        for S =>
        non_complex_literal_constant().map(|c| match c {
            NonComplexLiteralConstant::Int(i) => LiteralConstant::Int(i),
            NonComplexLiteralConstant::Real(r) => LiteralConstant::Real(r),
            NonComplexLiteralConstant::Logical(l) => LiteralConstant::Logical(l),
            NonComplexLiteralConstant::Char(c) => LiteralConstant::Char(c),
            NonComplexLiteralConstant::Boz(b) => LiteralConstant::Boz(b),
        }),
        complex_literal_constant.map(LiteralConstant::Complex),
    ).parse(source)
}

#[cfg(test)]
mod test {
    // use super::*;

    // TODO #[test]
    //fn test_named_constant() {
    //    for cfg in test_configs() {
    //        let parser = named_constant(&cfg);
    //        assert!(parser.parses("foo"));
    //    }
    //}
//
    // TODO #[test]
    //fn test_literal_constant() {
    //    for cfg in test_configs() {
    //        let parser = literal_constant(&cfg);
    //        assert!(parser.parse("1").unwrap().0.is_int());
    //        assert!(parser.parse("1.0").unwrap().0.is_real());
    //        assert!(parser.parse("(1.0, 1.0)").unwrap().0.is_complex());
    //        assert!(parser.parse(".true.").unwrap().0.is_logical());
    //        assert!(parser.parse("'a'").unwrap().0.is_char());
    //        assert!(parser.parse("b'101010'").unwrap().0.is_boz());
    //        assert!(parser.parse("o'01234567'").unwrap().0.is_boz());
    //        assert!(parser.parse("z'0123456789abcdef'").unwrap().0.is_boz());
    //    }
    //}
}
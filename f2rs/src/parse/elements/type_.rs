use super::*;
use riddle::{prelude::*, provided::text::rusty::Identifier};

use enum_as_inner::EnumAsInner;

// https://docs.oracle.com/cd/E19957-01/805-4939/index.html
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, EnumAsInner)]
pub enum BasicType {
    Integer,
    Integer2,
    Integer4,
    Integer8,
    Integer16,
    Real,
    Real4,
    Real8,
    Real16,
    Complex,
    Complex8,
    Complex16,
    Complex32,
    DoubleComplex,
    Character,
    CharacterN(usize),
    Logical,
    Logical1,
    Logical2,
    Logical4,
    Logical8,
    DoublePrecision,
}

pub fn basic_type<S: TextSource>() -> impl Parser<S, Token = BasicType> {
    alt! {
        (keyword("integer"), spaced('*'), keyword("2")).map(|_| BasicType::Integer2),
        (keyword("integer"), spaced('*'), keyword("4")).map(|_| BasicType::Integer4),
        (keyword("integer"), spaced('*'), keyword("8")).map(|_| BasicType::Integer8),
        (keyword("integer"), spaced('*'), keyword("16")).map(|_| BasicType::Integer16),
        (keyword("integer"), spaced('('), keyword("2"), spaced(')')).map(|_| BasicType::Integer2),
        (keyword("integer"), spaced('('), keyword("4"), spaced(')')).map(|_| BasicType::Integer4),
        (keyword("integer"), spaced('('), keyword("8"), spaced(')')).map(|_| BasicType::Integer8),
        (keyword("integer"), spaced('('), keyword("16"), spaced(')')).map(|_| BasicType::Integer16),
        keyword("integer").map(|_| BasicType::Integer),
        (keyword("real"), spaced('*'), keyword("4")).map(|_| BasicType::Real4),
        (keyword("real"), spaced('*'), keyword("8")).map(|_| BasicType::Real8),
        (keyword("real"), spaced('*'), keyword("16")).map(|_| BasicType::Real16),
        (keyword("real"), spaced('('), keyword("4"), spaced(')')).map(|_| BasicType::Real4),
        (keyword("real"), spaced('('), keyword("8"), spaced(')')).map(|_| BasicType::Real8),
        (keyword("real"), spaced('('), keyword("16"), spaced(')')).map(|_| BasicType::Real16),
        keyword("real").map(|_| BasicType::Real),
        (keyword("complex"), spaced('*'), keyword("8")).map(|_| BasicType::Complex8),
        (keyword("complex"), spaced('*'), keyword("16")).map(|_| BasicType::Complex16),
        (keyword("complex"), spaced('*'), keyword("32")).map(|_| BasicType::Complex32),
        (keyword("complex"), spaced('('), keyword("8"), spaced(')')).map(|_| BasicType::Complex8),
        (keyword("complex"), spaced('('), keyword("16"), spaced(')')).map(|_| BasicType::Complex16),
        (keyword("complex"), spaced('('), keyword("32"), spaced(')')).map(|_| BasicType::Complex32),
        keyword("complex").map(|_| BasicType::Complex),
        (keyword("double"), space(), keyword("complex")).map(|_| BasicType::DoubleComplex),
        (keyword("logical"), spaced('*'), keyword("1")).map(|_| BasicType::Logical1),
        (keyword("logical"), spaced('*'), keyword("2")).map(|_| BasicType::Logical2),
        (keyword("logical"), spaced('*'), keyword("4")).map(|_| BasicType::Logical4),
        (keyword("logical"), spaced('*'), keyword("8")).map(|_| BasicType::Logical8),
        (keyword("logical"), spaced('('), keyword("1"), spaced(')')).map(|_| BasicType::Logical1),
        (keyword("logical"), spaced('('), keyword("2"), spaced(')')).map(|_| BasicType::Logical2),
        (keyword("logical"), spaced('('), keyword("4"), spaced(')')).map(|_| BasicType::Logical4),
        (keyword("logical"), spaced('('), keyword("8"), spaced(')')).map(|_| BasicType::Logical8),
        keyword("logical").map(|_| BasicType::Logical),
        (keyword("character"), spaced('*'), integer_literal()).map(|(_, _, n)| BasicType::CharacterN(n as usize)),
        (keyword("character"), spaced('('), integer_literal(), spaced(')')).map(|(_, _, n, _)| BasicType::CharacterN(n as usize)),
        keyword("character").map(|_| BasicType::Character),
        (keyword("double"), space(), keyword("precision")).map(|_| BasicType::DoublePrecision),
    }
}

#[derive(Debug, Clone)]
pub enum Type<Span> {
    /// ex: `integer`
    Basic(BasicType),

    /// Ex: `integer(c_int)`
    BasicAlias(BasicType, Identifier<Span>),

    Type(Box<Type<Span>>),

    Array {
        ty: Box<Type<Span>>,
        ranges: Vec<Expression<Span>>,
    },
}

// TODO parse and use
//#[derive(Debug, Clone)]
//pub struct Modifiers {
//    pub intent: Option<Intent>,
//    // TODO ...
//}
//
//#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
//pub enum Intent {
//    In,
//    Out,
//    InOut,
//}

pub fn type_<S: TextSource>() -> impl Parser<S, Token = Type<S::Span>> {
    alt!(
        (basic_type(), spaced('('), identifier(), spaced(')'))
            .map(|(ty, _, alias, _)| Type::BasicAlias(ty, alias)),
        basic_type().map(Type::Basic),
        (keyword("type"), spaced('('), type_(), spaced(')'))
            .map(|(_, _, ty, _)| Type::Type(Box::new(ty))),
    )
}

// TODO tests

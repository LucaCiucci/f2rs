use super::*;
use f2rs_parser_combinator::{prelude::*, provided::text::rusty::Identifier};

use enum_as_inner::EnumAsInner;

// https://docs.oracle.com/cd/E19957-01/805-4939/index.html
#[derive(Debug, Clone, EnumAsInner)]
pub enum BasicType<Span> {
    Integer,
    Real,
    Complex,
    DoubleComplex,
    Character,
    CharacterN(StarOrExpr<Span>),
    Logical,
    DoublePrecision,
}

pub fn basic_type<S: TextSource>() -> impl Parser<S, Token = BasicType<S::Span>> {
    alt! {
        keyword("integer").map(|_| BasicType::Integer),
        keyword("real").map(|_| BasicType::Real),
        keyword("complex").map(|_| BasicType::Complex),
        (keyword("double"), space(), keyword("complex")).map(|_| BasicType::DoubleComplex),
        keyword("logical").map(|_| BasicType::Logical),
        (keyword("character"), spaced('*'), star_or_expr()).map(|(_, _, n)| BasicType::CharacterN(n)),
        (keyword("character"), spaced('('), star_or_expr(), spaced(')')).map(|(_, _, n, _)| BasicType::CharacterN(n)),
        (keyword("character"), spaced('('), spaced(keyword("len")), spaced('='), star_or_expr(), spaced(')')).map(|(_, _, _, _, n, _)| BasicType::CharacterN(n)),
        keyword("character").map(|_| BasicType::Character),
        (keyword("double"), space(), keyword("precision")).map(|_| BasicType::DoublePrecision),
    }
}

#[derive(Debug, Clone, enum_as_inner::EnumAsInner)]
pub enum Type<Span> {
    /// ex: `integer`
    Basic(BasicType<Span>),

    /// Ex: `integer(c_int)`
    BasicWithKind(BasicType<Span>, Expression<Span>),

    Type(Identifier<Span>),

    Array {
        ty: Box<Type<Span>>,
        ranges: Vec<Expression<Span>>,
    },
}

// TODO parse and use
#[derive(Debug, Clone)]
pub struct TypeWithModifiers<Span> {
    pub ty: Type<Span>,
    pub modifiers: Modifiers,
}

// TODO parse and use
#[derive(Debug, Clone)]
pub struct Modifiers {
    pub intent: Option<Intent>,
    pub parameter: bool,
    pub optional: bool,
    pub pointer: bool,
    pub save: bool,
    pub external: bool,
    // TODO ...
}

#[derive(Debug, Clone)]
pub enum Modifier<Span> {
    Intent(Intent),
    Parameter,
    Optional,
    Pointer,
    Save,
    Dimension(Vec<Expression<Span>>), // TODO StarOrExpr
    External,
    // TODO ...
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Intent {
    In,
    Out,
    InOut,
}

pub fn type_<S: TextSource>() -> impl Parser<S, Token = Type<S::Span>> {
    alt!(
        (basic_type(), spaced('('), spaced(keyword("kind")), spaced('='), expression_non_range(), spaced(')')).map(|(ty, _, _, _, kind, _)| Type::BasicWithKind(ty, kind)),
        (basic_type(), spaced('('), expression_non_range(), spaced(')')).map(|(ty, _, kind, _)| Type::BasicWithKind(ty, kind)),
        (basic_type(), spaced('*'), spaced(keyword("kind")), spaced('='), expression_non_range()).map(|(ty, _, _, _, kind)| Type::BasicWithKind(ty, kind)),
        (basic_type(), spaced('*'), expression_non_range()).map(|(ty, _, kind)| Type::BasicWithKind(ty, kind)),
        basic_type().map(Type::Basic),
        (keyword("type"), spaced('('), identifier(), spaced(')')).map(|(_, _, ty, _)| Type::Type(ty)),
    )
}

pub fn modifier<S: TextSource>() -> impl Parser<S, Token = Modifier<S::Span>> {
    alt!(
        spaced(keyword("intent")).then(|_| {
            alt!(
                (spaced('('), spaced(keyword("in")), spaced(')')).map(|_| Intent::In),
                (spaced('('), spaced(keyword("out")), spaced(')')).map(|_| Intent::Out),
                (spaced('('), spaced(keyword("inout")), spaced(')')).map(|_| Intent::InOut),
            )
        }).map(Modifier::Intent),
        spaced(keyword("parameter")).map(|_| Modifier::Parameter),
        spaced(keyword("optional")).map(|_| Modifier::Optional),
        spaced(keyword("pointer")).map(|_| Modifier::Pointer),
        spaced(keyword("save")).map(|_| Modifier::Save),
        spaced(keyword("dimension")).then(|_| {
            (
                spaced('('),
                separated(spaced(expression()), spaced(','), 0..),
                spaced(')'),
            )
                .map(|(_, exprs, _)| exprs)
        }).map(Modifier::Dimension),
        spaced(keyword("external")).map(|_| Modifier::External),
    )
}

pub fn type_with_modifiers<S: TextSource>() -> impl Parser<S, Token = TypeWithModifiers<S::Span>> {
    type_().then(|ty| {
        fold_many(
            (spaced(','), modifier()).map(|(_, m)| m),
            move || TypeWithModifiers {
                ty: ty.clone(),
                modifiers: Modifiers {
                    intent: None,
                    parameter: false,
                    optional: false,
                    pointer: false,
                    save: false,
                    external: false,
                }
            },
            |mut ty, modifier| {
                match modifier {
                    Modifier::Intent(intent) => {
                        ty.modifiers.intent = Some(intent);
                    }
                    Modifier::Parameter => {
                        ty.modifiers.parameter = true;
                    }
                    Modifier::Optional => {
                        ty.modifiers.optional = true;
                    }
                    Modifier::Pointer => {
                        ty.modifiers.pointer = true;
                    }
                    Modifier::Save => {
                        ty.modifiers.save = true;
                    }
                    Modifier::Dimension(ranges) => {
                        ty.ty = Type::Array {
                            ty: Box::new(ty.ty.clone()),
                            ranges,
                        };
                    }
                    Modifier::External => {
                        ty.modifiers.external = true;
                    }
                }
                (ty, true)
            },
            0..,
        )
    })
}

// TODO tests

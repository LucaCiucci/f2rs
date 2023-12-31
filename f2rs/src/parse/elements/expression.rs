use enum_as_inner::EnumAsInner;
use f2rs_parser_combinator::{prelude::*, provided::text::rusty::Identifier};

use super::*;

mod index_range;
pub use index_range::*;
mod operation;
pub use operation::*;
mod monome;
pub use monome::*;
mod if_arithmetic;
pub use if_arithmetic::*;

#[derive(Debug, Clone, EnumAsInner)]
pub enum Expression<Span> {
    Literal(Literal<Span>),
    Identifier(Identifier<Span>),
    Parenthesis(Box<Expression<Span>>),
    CallOrIndexing(Box<CallOrIndexing<Span>>),
    Operation(Box<Operation<Span>>),
    UnaryLeftOperation(Box<UnaryLeftOperation<Span>>),
    IndexRange(Box<IndexRange<Span>>),
    IfArithmetic(Box<IfArithmetic<Span>>),
    Array(Vec<Expression<Span>>),
}

pub fn array<S: TextSource>() -> impl Parser<S, Token = Expression<S::Span>> {
    alt! {
        (
            spaced('['),
            separated(spaced(expression()), spaced(','), 0..),
            spaced(']'),
        )
            .map(|(_, exprs, _)| Expression::Array(exprs)),
        (
            spaced("(/"),
            separated(spaced(expression()), spaced(','), 0..),
            spaced("/)"),
        )
            .map(|(_, exprs, _)| Expression::Array(exprs)),
    }
}

pub fn expression_non_range<S: TextSource>() -> impl Parser<S, Token = Expression<S::Span>> {
    alt! {
        operation(),
        expression_monome(),
    }
}

pub fn expression<S: TextSource>() -> impl Parser<S, Token = Expression<S::Span>> {
    alt! {
        index_range().map(|r| Expression::IndexRange(Box::new(r))),
        expression_non_range(),
    }
}

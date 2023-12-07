
use riddle::{prelude::*, provided::text::rusty::Identifier};

use super::*;

mod index_range; pub use index_range::*;
mod operation; pub use operation::*;
mod monome; pub use monome::*;

#[derive(Debug, Clone)]
pub enum Expression<Span> {
    Literal(Literal<Span>),
    Identifier(Identifier<Span>),
    Parenthesis(Box<Expression<Span>>),
    CallOrIndexing {
        function: Box<Expression<Span>>,
        arguments: Vec<Expression<Span>>,
    },
    Operation {
        left: Box<Expression<Span>>,
        operator: ExactMatch<Span>,
        right: Box<Expression<Span>>,
    },
    UnaryLeftOperation {
        operator: ExactMatch<Span>,
        right: Box<Expression<Span>>,
    },
    IndexRange(Box<IndexRange<Span>>),
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
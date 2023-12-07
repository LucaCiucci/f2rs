
use super::*;

#[derive(Debug, Clone)]
pub struct IndexRange<Span> {
    pub start: Option<Expression<Span>>,
    pub end: Option<Expression<Span>>,
}

pub fn index_range<S: TextSource>() -> impl Parser<S, Token = IndexRange<S::Span>> {
    (
        expression_non_range().optional(),
        spaced(':'),
        expression_non_range().optional(),
    ).map(|(start, _, end)| IndexRange {
        start,
        end,
    })
}
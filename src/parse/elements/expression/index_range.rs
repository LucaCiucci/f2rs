
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

#[cfg(test)]
mod tests {
    use riddle::prelude::*;

    use super::*;

    #[test]
    fn test_index_range() {
        let r = index_range().parse("a:b").0.unwrap();
        assert_eq!(r.start.unwrap().as_identifier().unwrap().value, "a");
        assert_eq!(r.end.unwrap().as_identifier().unwrap().value, "b");

        let r = index_range().parse("a:").0.unwrap();
        assert_eq!(r.start.unwrap().as_identifier().unwrap().value, "a");
        assert!(r.end.is_none());

        let r = index_range().parse(":b").0.unwrap();
        assert!(r.start.is_none());
        assert_eq!(r.end.unwrap().as_identifier().unwrap().value, "b");

        let r = index_range().parse(":").0.unwrap();
        assert!(r.start.is_none());
        assert!(r.end.is_none());
    }
}
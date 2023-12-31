use std::ops::RangeBounds;

use super::*;

// TODO test
pub fn list<'a, S: TextSource + 'a, P: Parser<S, Token = T> + 'a, T: 'a>(
    element: P,
    range: impl RangeBounds<usize> + Clone + 'a,
) -> impl Parser<S, Token = Vec<P::Token>> + 'a {
    separated(
        element,
        (space(0), ',', space(0)),
        range
    )
}
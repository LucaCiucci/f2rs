use f2rs_parser_combinator::{provided::common::separated, tokenization::TextSource};

use crate::parse::elements::spaced;

use super::*;

#[derive(Debug, Clone)]
pub struct IfArithmetic<Span> {
    pub selector: Expression<Span>,
    pub cases: Vec<Expression<Span>>,
}

pub fn if_arithmetic<S: TextSource>() -> impl Parser<S, Token = IfArithmetic<S::Span>> {
    (
        spaced(keyword("if")),
        spaced(expression()),
        separated(expression(), spaced(','), 0..),
    )
        .map(|(_, selector, cases)| IfArithmetic { selector, cases })
}

#[cfg(test)]
mod tests {
    use f2rs_parser_combinator::prelude::*;

    use super::*;

    #[test]
    fn test_if_arithmetic() {
        let r = if_arithmetic().parse("if (n) a, b, c").0.unwrap();
        assert_eq!(
            r.selector
                .as_parenthesis()
                .unwrap()
                .as_identifier()
                .unwrap()
                .value,
            "n"
        );
        assert_eq!(r.cases.len(), 3);
        assert_eq!(r.cases[0].as_identifier().unwrap().value, "a");
        assert_eq!(r.cases[1].as_identifier().unwrap().value, "b");
        assert_eq!(r.cases[2].as_identifier().unwrap().value, "c");
    }
}

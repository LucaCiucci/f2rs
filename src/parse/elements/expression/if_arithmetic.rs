use riddle::{tokenization::TextSource, provided::common::separated};

use crate::parse::elements::spaced;

use super::*;



#[derive(Debug, Clone)]
pub struct IfArithmetic<Span> {
    pub selector: Expression<Span>,
    pub cases: Vec<Expression<Span>>
}

pub fn if_arithmetic<S: TextSource>() -> impl Parser<S, Token = IfArithmetic<S::Span>> {
    (
        spaced(keyword("if")),
        spaced(expression()),
        separated(expression(), spaced(','), 0..),
    ).map(|(_, selector, cases)| IfArithmetic { selector, cases })
}
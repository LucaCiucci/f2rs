use riddle::provided::text::rusty::Identifier;

use crate::parse::eol_or_comment;

use super::*;

#[derive(Debug, Clone)]
pub enum StarOrExpr<Span> {
    Star,
    Expression(Expression<Span>),
}

pub fn star_or_expr<S: TextSource>() -> impl Parser<S, Token = StarOrExpr<S::Span>> {
    alt! {
        spaced('*').map(|_| StarOrExpr::Star),
        spaced(expression()).map(StarOrExpr::Expression),
    }
}

// fortran
const SPECIAL_FUNCTIONS: [&str; 3] = [
    "print",
    "write",
    "read",
];

pub fn special_function_name<S: TextSource>() -> impl Parser<S, Token = Identifier<S::Span>> {
    spaced(identifier()).condition(|id, _| SPECIAL_FUNCTIONS.contains(&id.value.as_str()))
}

#[derive(Debug, Clone)]
pub struct SpecialFunction<Span> {
    pub name: Identifier<Span>,
    pub special_args: Option<Vec<StarOrExpr<Span>>>,
    pub items: Vec<StarOrExpr<Span>>,
}

pub fn special_function<S: TextSource>() -> impl Parser<S, Token = SpecialFunction<S::Span>> {
    (
        spaced(special_function_name()),
        (
            spaced('('),
            separated(spaced(star_or_expr()), spaced(','), 0..),
            spaced(')'),
        ).map(|(_, s, _)| s).optional(),
        spaced(',').optional(),
        separated(star_or_expr(), spaced(','), 0..),
        eol_or_comment(),
    ).map(|(
        name,
        special_args,
        _,
        items,
        _,
    )| SpecialFunction { name, special_args, items })
}
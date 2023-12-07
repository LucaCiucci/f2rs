use riddle::prelude::*;

use super::*;

pub fn expression_monome<S: TextSource>() -> impl Parser<S, Token = Expression<S::Span>> {
    alt! {
        (spaced(operator()), expression_monome()).map(|(op, expr)| Expression::UnaryLeftOperation { operator: op, right: Box::new(expr) }),
        fortran_literal().map(Expression::Literal),
        identifier().map(Expression::Identifier),
        (
            spaced('('),
            spaced(expression()),
            spaced(')'),
        ).map(|(_, expr, _)| Expression::Parenthesis(Box::new(expr))),
    }
    .then(|expr| {
        many(
            call_or_indexing(),
            0..,
        ).map(move |calls| {
            let mut expr = expr.clone();
            for call in calls {
                expr = Expression::CallOrIndexing {
                    function: Box::new(expr),
                    arguments: call,
                };
            }
            expr
        })
    })
}

fn call_or_indexing<S: TextSource>() -> impl Parser<S, Token = Vec<Expression<S::Span>>> {
    (
        spaced('('),
        separated(
            spaced(expression()),
            ',',
            0..,
        ),
        spaced(')'),
    ).map(|(_, args, _)| args)
}
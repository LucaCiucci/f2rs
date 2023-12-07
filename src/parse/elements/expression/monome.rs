use riddle::prelude::*;

use super::*;

#[derive(Debug, Clone)]
pub struct UnaryLeftOperation<Span> {
    pub operator: ExactMatch<Span>,
    pub right: Expression<Span>,
}

pub fn expression_monome<S: TextSource>() -> impl Parser<S, Token = Expression<S::Span>> {
    alt! {
        if_arithmetic().map(|if_arithmetic| Expression::IfArithmetic(Box::new(if_arithmetic))),
        (spaced(operator()), expression_monome()).map(|(op, expr)| Expression::UnaryLeftOperation(Box::new(UnaryLeftOperation {
            operator: op,
            right: expr,
        }))),
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn if_arithmetic() {
        expression_monome().parse("if (n) a, b, c").0.unwrap();
    }

    #[test]
    fn unary_operator() {
        let r = expression_monome().parse("-a").0.unwrap();
        assert_eq!(r.as_unary_left_operation().unwrap().operator.value, "-");
        assert_eq!(r.as_unary_left_operation().unwrap().right.as_identifier().unwrap().value, "a");
    }

    #[test]
    fn literal() {
        let r = expression_monome().parse("1").0.unwrap();
        assert_eq!(
            r.as_literal().unwrap()
                .as_number().unwrap().value,
                "1"
        );

        let r = expression_monome().parse("1.0").0.unwrap();
        assert_eq!(
            r.as_literal().unwrap()
                .as_number().unwrap().value,
                "1.0"
        );

        let r = expression_monome().parse("1.0e-10").0.unwrap();
        assert_eq!(
            r.as_literal().unwrap()
                .as_number().unwrap().value,
                "1.0e-10"
        );

        let r = expression_monome().parse("\"hello world\"").0.unwrap();
        assert_eq!(
            r.as_literal().unwrap()
                .as_string().unwrap().value(),
                "hello world"
        );
    }

    #[test]
    fn parenthesis() {
        let r = expression_monome().parse("(a)").0.unwrap();
        assert_eq!(r.as_parenthesis().unwrap().as_identifier().unwrap().value, "a");
    }

    #[test]
    fn identifier_expression() {
        let r = expression_monome().parse("a").0.unwrap();
        assert_eq!(r.as_identifier().unwrap().value, "a");
    }
}
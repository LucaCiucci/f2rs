use f2rs_parser_combinator::prelude::*;

use super::*;

#[derive(Debug, Clone)]
pub struct UnaryLeftOperation<Span> {
    pub operator: StringMatch<Span>,
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
        array(),
    }
    .then(|expr| {
        many(
            call_or_indexing(),
            0..,
        ).map(move |calls| {
            let mut expr = expr.clone();
            for call in calls {
                expr = Expression::CallOrIndexing(Box::new(CallOrIndexing {
                    function: expr,
                    arguments: call,
                }));
            }
            expr
        })
    })
}

#[derive(Debug, Clone)]
pub struct CallOrIndexing<Span> {
    pub function: Expression<Span>,
    pub arguments: Vec<Expression<Span>>,
}

fn call_or_indexing<S: TextSource>() -> impl Parser<S, Token = Vec<Expression<S::Span>>> {
    (
        spaced('('),
        separated(spaced(expression()), ',', 0..),
        spaced(')'),
    )
        .map(|(_, args, _)| args)
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
        assert_eq!(
            r.as_unary_left_operation()
                .unwrap()
                .right
                .as_identifier()
                .unwrap()
                .value,
            "a"
        );
    }

    #[test]
    fn literal() {
        let r = expression_monome().parse("1").0.unwrap();
        assert_eq!(r.as_literal().unwrap().as_number().unwrap().value, "1");

        let r = expression_monome().parse("1.0").0.unwrap();
        assert_eq!(r.as_literal().unwrap().as_number().unwrap().value, "1.0");

        let r = expression_monome().parse("1.0e-10").0.unwrap();
        assert_eq!(
            r.as_literal().unwrap().as_number().unwrap().value,
            "1.0e-10"
        );

        let r = expression_monome().parse("\"hello world\"").0.unwrap();
        assert_eq!(
            r.as_literal().unwrap().as_string().unwrap().value(),
            "hello world"
        );
    }

    #[test]
    fn identifier_expression() {
        let r = expression_monome().parse("a").0.unwrap();
        assert_eq!(r.as_identifier().unwrap().value, "a");
    }

    #[test]
    fn parenthesis() {
        let r = expression_monome().parse("(a)").0.unwrap();
        assert_eq!(
            r.as_parenthesis().unwrap().as_identifier().unwrap().value,
            "a"
        );
    }

    #[test]
    fn call_or_indexing() {
        let r = expression_monome().parse("a(b)").0.unwrap();
        assert_eq!(
            r.as_call_or_indexing()
                .unwrap()
                .function
                .as_identifier()
                .unwrap()
                .value,
            "a"
        );
        assert_eq!(r.as_call_or_indexing().unwrap().arguments.len(), 1);
        assert_eq!(
            r.as_call_or_indexing().unwrap().arguments[0]
                .as_identifier()
                .unwrap()
                .value,
            "b"
        );

        let r = expression_monome().parse("a(b:c, :d)").0.unwrap();
        assert_eq!(
            r.as_call_or_indexing()
                .unwrap()
                .function
                .as_identifier()
                .unwrap()
                .value,
            "a"
        );
        assert_eq!(r.as_call_or_indexing().unwrap().arguments.len(), 2);
        assert_eq!(
            r.as_call_or_indexing().unwrap().arguments[0]
                .as_index_range()
                .unwrap()
                .start
                .as_ref()
                .unwrap()
                .as_identifier()
                .unwrap()
                .value,
            "b"
        );
        assert_eq!(
            r.as_call_or_indexing().unwrap().arguments[0]
                .as_index_range()
                .unwrap()
                .end
                .as_ref()
                .unwrap()
                .as_identifier()
                .unwrap()
                .value,
            "c"
        );
        assert!(r.as_call_or_indexing().unwrap().arguments[1]
            .as_index_range()
            .unwrap()
            .start
            .is_none());
        assert_eq!(
            r.as_call_or_indexing().unwrap().arguments[1]
                .as_index_range()
                .unwrap()
                .end
                .as_ref()
                .unwrap()
                .as_identifier()
                .unwrap()
                .value,
            "d"
        );
    }
}

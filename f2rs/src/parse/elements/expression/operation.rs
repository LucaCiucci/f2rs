
use riddle::provided::common::chained;

use super::*;

#[derive(Debug, Clone)]
pub struct Operation<Span> {
    pub left: Expression<Span>,
    pub operator: ExactMatch<Span>,
    pub right: Expression<Span>,
}

pub fn operation<S: TextSource>() -> impl Parser<S, Token = Expression<S::Span>> {
    chained(spaced(expression_monome()), spaced(operator()))
        .map(|
            (
                mut first,
                mut tail,
            ): (
                Expression<S::Span>,
                Vec<(ExactMatch<S::Span>, Expression<S::Span>)>
            )
        | {
            let operator_priority = |op: &ExactMatch<S::Span>| -> usize {
                OPERATORS
                    .iter()
                    .cloned()
                    .find(|(o, _)| *o == op.value)
                    .map(|(_, p)| p)
                    .unwrap()
            };

            fn pick_ith<'a, Span>(first: &'a mut Expression<Span>, tail: &'a mut Vec<(ExactMatch<Span>, Expression<Span>)>, i: usize) -> (&'a mut Expression<Span>, ExactMatch<Span>, Expression<Span>) {
                let (
                    op,
                    expr
                ) = tail.remove(i);
                let l_expr = if i == 0 {
                    first
                } else {
                    let (_, op) = tail.get_mut(i - 1).unwrap();
                    op
                };
                (l_expr, op, expr)
            }

            while !tail.is_empty() {
                let mut max_priority = 0;
                let mut max_priority_i = 0;
                for (i, (op, _)) in tail.iter().enumerate() {
                    let priority = operator_priority(op);
                    if priority > max_priority {
                        max_priority = priority;
                        max_priority_i = i;
                    }
                }

                let (
                    l_expr,
                    op,
                    r_expr
                ) = pick_ith(&mut first, &mut tail, max_priority_i);

                // https://stackoverflow.com/questions/67461269/replace-a-value-behind-a-mutable-reference-by-moving-and-mapping-the-original
                // XXX unsound, don't use
                //pub fn map_in_place<T>(place: &mut T, f: impl FnOnce(T) -> T) {
                //    let place = place as *mut T;
                //    unsafe {
                //        let val = std::ptr::read(place);
                //        let new_val = f(val);
                //        std::ptr::write(place, new_val);
                //    }
                //}

                *l_expr = Expression::Operation(Box::new(Operation {
                    operator: op,
                    left: l_expr.clone(),
                    right: r_expr,
                }));

                //map_in_place(l_expr, |l_expr| Expression::Operation {
                //    operator: op,
                //    left: Box::new(l_expr),
                //    right: Box::new(r_expr),
                //});
            }

            first
        })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_operation() {
        let r = operation().parse("a + b * c - d / e ** f").0.unwrap();

        assert_eq!(
            r.as_operation().unwrap().operator.value,
            "-"
        );
        assert_eq!(
            r.as_operation().unwrap().left.as_operation().unwrap().operator.value,
            "+"
        );
        assert_eq!(
            r.as_operation().unwrap().left.as_operation().unwrap().left.as_identifier().unwrap().value,
            "a"
        );
        assert_eq!(
            r.as_operation().unwrap().left.as_operation().unwrap().right.as_operation().unwrap().operator.value,
            "*"
        );
        assert_eq!(
            r.as_operation().unwrap().left.as_operation().unwrap().right.as_operation().unwrap().left.as_identifier().unwrap().value,
            "b"
        );
        assert_eq!(
            r.as_operation().unwrap().left.as_operation().unwrap().right.as_operation().unwrap().right.as_identifier().unwrap().value,
            "c"
        );
        assert_eq!(
            r.as_operation().unwrap().right.as_operation().unwrap().operator.value,
            "/"
        );
        assert_eq!(
            r.as_operation().unwrap().right.as_operation().unwrap().left.as_identifier().unwrap().value,
            "d"
        );
        assert_eq!(
            r.as_operation().unwrap().right.as_operation().unwrap().right.as_operation().unwrap().operator.value,
            "**"
        );
        assert_eq!(
            r.as_operation().unwrap().right.as_operation().unwrap().right.as_operation().unwrap().left.as_identifier().unwrap().value,
            "e"
        );
        assert_eq!(
            r.as_operation().unwrap().right.as_operation().unwrap().right.as_operation().unwrap().right.as_identifier().unwrap().value,
            "f"
        );
    }
}
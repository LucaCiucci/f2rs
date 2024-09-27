use f2rs_parser_combinator::provided::text::rusty::Identifier;

use crate::parse::element;

use super::*;

#[derive(Debug, Clone)]
pub struct DoLoop<Span> {
    pub variable: String,
    pub start: Expression<Span>,
    pub end: Expression<Span>,
    pub step: Option<Expression<Span>>,
    pub body: Vec<Element<Span>>,
    pub opening_comment: Option<LineComment<Span>>,
    pub closing_comment: Option<LineComment<Span>>,
}

pub fn enddo<S: TextSource>() -> impl Parser<S, Token = Option<LineComment<S::Span>>> {
    (
        alt! {
            spaced(keyword("enddo")).map(|_| ()),
            (spaced(keyword("end")), spaced(keyword("do").optional())).map(|_| ()),
        },
        eol_or_comment(),
    ).map(|(_, c)| c)
}

pub fn label_match<S: TextSource>(label: i128) -> impl Parser<S, Token = ()> {
    spaced_or_nl(integer_literal())
    //spaced_or_nl(integer_literal())
        .condition(move |i, _| *i == label)
        .map(|_| ())
}

fn labeled_enddo<S: TextSource>(label: Option<i128>) -> impl Parser<S, Token = Option<LineComment<S::Span>>> {
    (
        label_match(label.unwrap_or(0)).condition(move |_, _| label.is_some()), // TODO remove unwrap
        enddo(),
    ).map(|(_, c)| c)
}

fn labeled_continue<S: TextSource>(label: Option<i128>) -> impl Parser<S, Token = Option<LineComment<S::Span>>> {
    (
        label_match(label.unwrap_or(0)).condition(move |_, _| label.is_some()), // TODO remove unwrap
        (spaced(keyword("continue")), eol_or_comment()).map(|(_, c)| c),
    ).map(|(_, c)| c)
    .do_not_consume() // For shared do termination label, removed in fortran 2018
}

// Fortran 2018 deleted feature: DO termination statement which is not END DO or CONTINUE with label 100 at (1)
fn labeled_expression<S: TextSource>(label: Option<i128>) -> impl Parser<S, Token = (Statement<S::Span>, Option<LineComment<S::Span>>)> {
    (
        label_match(label.unwrap_or(0)).condition(move |_, _| label.is_some()), // TODO remove unwrap
        spaced(statement()),
    ).map(|(_, e)| (e, None))
}

fn labeled_enddo_or_continue<S: TextSource>(label: Option<i128>) -> impl Parser<S, Token = (Option<Statement<S::Span>>, Option<LineComment<S::Span>>)> {
    alt! {
        labeled_enddo(label).map(|c| (None, c)),
        labeled_continue(label).map(|c| (None, c)),
        labeled_expression(label).map(|(e, c)| (Some(e), c)),
    }
}

pub fn do_loop_end<S: TextSource>(label: Option<i128>) -> impl Parser<S, Token = (Option<Statement<S::Span>>, Option<LineComment<S::Span>>)> {
    alt! {
        enddo().condition(move |_, _| label.is_none()).map(|c| (None, c)),
        labeled_enddo_or_continue(label).condition(move |_, _| label.is_some()),
    }
}

struct DoLoopStart<Span> {
    label: Option<i128>,
    variable: Identifier<Span>,
    from: Expression<Span>,
    to: Expression<Span>,
    step: Option<Expression<Span>>,
    opening_comment: Option<LineComment<Span>>,
}

fn do_loop_start<S: TextSource>() -> impl Parser<S, Token = DoLoopStart<S::Span>> {
    (
        spaced(keyword("do")),
        spaced(integer_literal()).optional(),
        spaced(identifier()),
        spaced('='),
        spaced(expression()),
        spaced(','),
        spaced(expression()),
        (spaced(','), spaced(expression()))
            .optional()
            .map(|opt| opt.map(|(_, step)| step)),
        eol_or_comment(),
    ).map(|(_, label, var, _, from, _, to, step, oc)| DoLoopStart {
        label,
        variable: var,
        from,
        to,
        step,
        opening_comment: oc,
    })
}

pub fn do_loop<S: TextSource>() -> impl Parser<S, Token = Statement<S::Span>> {
    do_loop_start().then(|start| many_until(
        element(),
        do_loop_end(start.label),
        0..,
    ).map(move |(mut body, last_expression_and_cc)| {
        let (last_expression, cc) = if let Some((e, cc)) = last_expression_and_cc {
            (e, cc)
        } else {
            (None, None)
        };

        if let Some(e) = last_expression {
            body.push(Element::Statement(e));
        }

        DoLoop {
            variable: start.variable.value.clone(),
            start: start.from.clone(),
            end: start.to.clone(),
            step: start.step.clone(),
            body,
            opening_comment: start.opening_comment.clone(),
            closing_comment: cc,
        }
    }))
    .map(|do_loop| Statement::DoLoop(do_loop))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_enddo() {
        let r = enddo().parse(" enddo something");
        assert!(r.0.is_some());
        assert_eq!(r.1, "something");

        let r = enddo().parse(" end do \nsomething");
        assert!(r.0.is_some());
        assert_eq!(r.1, "\nsomething");
    }

    //#[test]
    //fn test_enddo_or_continue() {
    //    //let r = enddo_or_continue().parse(" enddo \nsomething");
    //    //assert_eq!(r.0.unwrap(), ());
    //    //assert_eq!(r.1, "\nsomething");
//
    //    let r = enddo_or_continue().parse(" continue \nsomething");
    //    assert!(r.0.is_some());
    //    assert_eq!(r.1, "\nsomething");
    //}

    #[test]
    fn test_label_match() {
        let r = label_match(1).parse(" 1 \nsomething");
        assert_eq!(r.0.unwrap(), ());
        assert_eq!(r.1, "\nsomething");

        let r = label_match(1).parse(" 2 \nsomething");
        assert!(r.0.is_none());
        assert_eq!(r.1, " 2 \nsomething");
    }

    #[test]
    fn test_labeled_enddo() {
        let r = labeled_enddo(Some(1)).parse(" 1 continue \nsomething");
        assert!(r.0.is_some());
        assert_eq!(r.1, "\nsomething");

        let r = labeled_enddo(Some(1)).parse(" 2 continue \nsomething");
        assert!(r.0.is_none());
        assert_eq!(r.1, " 2 continue \nsomething");

        let r = labeled_enddo(None).parse(" continue \nsomething");
        assert!(r.0.is_none());
        assert_eq!(r.1, " continue \nsomething");
    }

    #[test]
    fn test_do_loop_end() {
        let r = do_loop_end(None).parse(" enddo \n something");
        assert!(r.0.is_some());
        assert_eq!(r.1, " something");

        let r = do_loop_end(Some(1)).parse(" continue \n something");
        assert!(r.0.is_none());
        assert_eq!(r.1, " continue \n something");

        let r = do_loop_end(Some(1)).parse(" 1 continue \n something");
        assert!(r.0.is_some());
        assert_eq!(r.1, " something");

        let r = do_loop_end(Some(1)).parse(" 2 continue \n something");
        assert!(r.0.is_none());
        assert_eq!(r.1, " 2 continue \n something");
    }

    #[test]
    fn test_do_loop() {
        let r = do_loop()
            .parse(include_str!("do_loop/test_1.f90"))
            .0
            .unwrap();

        assert_eq!(r.as_do_loop().unwrap().variable, "i");
        assert_eq!(
            r.as_do_loop()
                .unwrap()
                .start
                .as_literal()
                .unwrap()
                .as_number()
                .unwrap()
                .value,
            "1"
        );
        assert_eq!(
            r.as_do_loop()
                .unwrap()
                .end
                .as_literal()
                .unwrap()
                .as_number()
                .unwrap()
                .value,
            "10"
        );
        assert_eq!(
            r.as_do_loop()
                .unwrap()
                .step
                .as_ref()
                .unwrap()
                .as_literal()
                .unwrap()
                .as_number()
                .unwrap()
                .value,
            "2"
        );
        assert_eq!(r.as_do_loop().unwrap().body.len(), 1);

        let r = do_loop()
            .parse(include_str!("do_loop/test_2.f90"))
            .0
            .unwrap();

        assert_eq!(r.as_do_loop().unwrap().variable, "i");
        assert_eq!(
            r.as_do_loop()
                .unwrap()
                .start
                .as_literal()
                .unwrap()
                .as_number()
                .unwrap()
                .value,
            "1"
        );
        assert_eq!(
            r.as_do_loop()
                .unwrap()
                .end
                .as_literal()
                .unwrap()
                .as_number()
                .unwrap()
                .value,
            "10"
        );
        assert!(
            r.as_do_loop()
                .unwrap()
                .step
                .is_none()
        );
        assert_eq!(r.as_do_loop().unwrap().body.len(), 1);
    }
}

use crate::parse::item;

use super::*;

#[derive(Debug, Clone)]
pub struct DoLoop<Span> {
    pub variable: String,
    pub start: Expression<Span>,
    pub end: Expression<Span>,
    pub step: Option<Expression<Span>>,
    pub body: Vec<Item<Span>>,
    pub opening_comment: Option<LineComment<Span>>,
    pub closing_comment: Option<LineComment<Span>>,
}

pub fn do_loop<S: TextSource>() -> impl Parser<S, Token = Statement<S::Span>> {
    (
        (
            spaced(keyword("do")),
            spaced(identifier()),
            spaced('='),
            spaced(expression()),
            spaced(','),
            spaced(expression()),
            (spaced(','), spaced(expression()))
                .optional()
                .map(|opt| opt.map(|(_, step)| step)),
            eol_or_comment(),
        )
            .map(|(_, var, _, from, _, to, step, oc)| (var, from, to, step, oc)),
        many_until(
            item(),
            (
                spaced(keyword("end")),
                spaced(keyword("do").optional()),
                eol_or_comment(),
            )
                .map(|(_, _, cc)| cc),
            0..,
        )
        .map(|(body, cc)| (body, cc)),
    )
        .map(|((var, from, to, step, oc), (body, cc))| {
            Statement::DoLoop(DoLoop {
                variable: var.value,
                start: from,
                end: to,
                step,
                body,
                opening_comment: oc,
                closing_comment: if let Some(cc) = cc { cc } else { None }
            })
        })
}

#[cfg(test)]
mod tests {
    use super::*;

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
    }
}

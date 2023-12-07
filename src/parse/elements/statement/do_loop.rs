use crate::parse::item;

use super::*;

pub fn do_loop<S: TextSource>() -> impl Parser<S, Token = Statement<S::Span>> {
    (
        (
            spaced(keyword("do")),
            spaced(identifier()),
            spaced('='),
            spaced(expression()),
            spaced(','),
            spaced(expression()),
            (
                spaced(','),
                spaced(expression()),
            ).optional().map(|opt| opt.map(|(_, step)| step)),
            eol_or_comment(),
        ).map(|(_, var, _, from, _, to, step, _)| (var, from, to, step)),
        many_until(
            item(),
            (spaced(keyword("end")), spaced(keyword("do").optional()), eol_or_comment()).map(|_| ()),
            0..,
        ).map(|(body, _)| body),
    ).map(|((var, from, to, step), body)| Statement::DoLoop {
        variable: var.value,
        start: from,
        end: to,
        step,
        body,
    })
}
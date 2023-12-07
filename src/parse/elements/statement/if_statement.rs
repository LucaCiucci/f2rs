use crate::parse::item;

use super::*;

pub fn if_<S: TextSource>() -> impl Parser<S, Token = Statement<S::Span>> {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    enum BodyTermination {
        EndIf,
        ElseIf,
    }

    fn if_termination<S: TextSource>() -> impl Parser<S, Token = BodyTermination> {
        (
            spaced(keyword("end")),
            spaced(keyword("if")).optional(),
            eol_or_comment(),
        ).map(|_| BodyTermination::EndIf)
    }

    fn ok_body_termination<S: TextSource>() -> impl Parser<S, Token = BodyTermination> {
        if_termination()
            .or((
                spaced(keyword("else")),
                spaced(keyword("if").optional()),
                eol_or_comment(),
            ).map(|_| BodyTermination::ElseIf))
    }

    (
        (
            spaced(keyword("if")),
            spaced(expression()),
            spaced(keyword("then")),
            eol_or_comment(),
        ).map(|(_, cond, _, _)| cond),
        many_until(
            item(),
            ok_body_termination(),
            0..,
        ).then(|(body, termination)| {
            //(spaced(keyword("else")), eol_or_comment),
            many_until(
                item(),
                if_termination(),
                0..,
            )
                .map(|(else_body, _)| else_body)
                .where_(termination == Some(BodyTermination::ElseIf))
                .map(move |else_body| (body.clone(), else_body))
        })
    ).map(|(condition, (body, else_body))| Statement::If {
        condition,
        body,
        else_body,
    })
}
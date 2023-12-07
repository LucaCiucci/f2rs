use crate::parse::item;

use super::*;

#[derive(Debug, Clone)]
pub enum IfStatement<Span> {
    Statement {
        condition: Expression<Span>,
        body: Vec<Item<Span>>,
        else_body: Option<Vec<Item<Span>>>,
    },
    Logical {
        condition: Expression<Span>,
        statement: Statement<Span>,
    }
}

pub fn if_statement<S: TextSource>() -> impl Parser<S, Token = IfStatement<S::Span>> {
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
    ).map(|(condition, (body, else_body))| IfStatement::Statement {
        condition,
        body,
        else_body,
    })
}

// TODO to test!!!
pub fn if_logical<S: TextSource>() -> impl Parser<S, Token = IfStatement<S::Span>> {
    (
        spaced(keyword("if")),
        spaced(expression()),
        spaced(statement()),
    ).map(|(_, condition, statement)| IfStatement::Logical { condition, statement })
}

pub fn if_<S: TextSource>() -> impl Parser<S, Token = IfStatement<S::Span>> {
    if_statement().or(if_logical())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_if_statement() {
        let _r = if_statement().parse(include_str!("if_statement/test_1.f90")).0.unwrap();
    }

    #[test]
    fn test_if_logical() {
        let _r = if_logical().parse(include_str!("if_statement/test_2.f90")).0.unwrap();
    }
}
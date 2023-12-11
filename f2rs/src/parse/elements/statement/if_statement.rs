use crate::parse::item;

use super::*;

#[derive(Debug, Clone)]
pub enum IfStatement<Span> {
    Statement {
        condition: Expression<Span>,
        condition_comment: Option<LineComment<Span>>,
        body: Vec<Item<Span>>,
        body_closing_comment: Option<LineComment<Span>>,
        else_body: Option<Vec<Item<Span>>>,
        else_closing_comment: Option<LineComment<Span>>,
    },
    Logical {
        condition: Expression<Span>,
        statement: Statement<Span>,
    },
}

pub fn if_statement<S: TextSource>() -> impl Parser<S, Token = IfStatement<S::Span>> {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    enum BodyTermination {
        EndIf,
        ElseIf,
    }

    fn if_termination<S: TextSource>() -> impl Parser<S, Token = (BodyTermination, Option<LineComment<S::Span>>)> {
        (
            spaced(keyword("end")),
            spaced(keyword("if")).optional(),
            eol_or_comment(),
        )
            .map(|(_, _, c)| (BodyTermination::EndIf, c))
    }

    fn ok_body_termination<S: TextSource>() -> impl Parser<S, Token = (BodyTermination, Option<LineComment<S::Span>>)> {
        if_termination().or((
            spaced(keyword("else")),
            spaced(keyword("if").optional()),
            eol_or_comment(),
        )
            .map(|(_, _, c)| (BodyTermination::ElseIf, c)))
    }

    (
        (
            spaced(keyword("if")),
            spaced(expression()),
            spaced(keyword("then")),
            eol_or_comment(),
        )
            .map(|(_, cond, _, _)| cond),
        many_until(item(), ok_body_termination(), 0..).then(|(body, termination)| {
            //(spaced(keyword("else")), eol_or_comment),
            many_until(item(), if_termination(), 0..)
                .where_(termination.is_some() && termination.unwrap().0 == BodyTermination::ElseIf)
                .map(move |else_body| (body.clone(), else_body.map(|(b, _)| b)))
        }),
    )
        .map(|(condition, (body, else_body))| IfStatement::Statement {
            condition,
            condition_comment: None, // TODO
            body,
            body_closing_comment: None, // TODO
            else_body,
            else_closing_comment: None, // TODO
        })
}

// TODO to test!!!
pub fn if_logical<S: TextSource>() -> impl Parser<S, Token = IfStatement<S::Span>> {
    (
        spaced(keyword("if")),
        spaced(expression()),
        spaced(statement()),
    )
        .map(|(_, condition, statement)| IfStatement::Logical {
            condition,
            statement,
        })
}

pub fn if_<S: TextSource>() -> impl Parser<S, Token = IfStatement<S::Span>> {
    if_statement().or(if_logical())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_if_statement() {
        let _r = if_statement()
            .parse(include_str!("if_statement/test_1.f90"))
            .0
            .unwrap();
    }

    #[test]
    fn test_if_logical() {
        let _r = if_logical()
            .parse(include_str!("if_statement/test_2.f90"))
            .0
            .unwrap();
    }
}

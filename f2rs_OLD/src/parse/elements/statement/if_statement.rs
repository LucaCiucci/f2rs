use crate::parse::element;
use enum_as_inner::EnumAsInner;

use super::*;

#[derive(Debug, Clone, EnumAsInner)]
pub enum IfBodyTermination<Span> {
    Else,
    ElseIf(Expression<Span>),
    End,
}

#[derive(Debug, Clone)]
pub struct IfBlock<Span> {
    pub items: Vec<Element<Span>>,
    pub termination: Option<(IfBodyTermination<Span>, Option<LineComment<Span>>)>,
}

#[derive(Debug, Clone)]
pub enum IfStatement<Span> {
    Statement {
        condition: Expression<Span>,
        condition_comment: Option<LineComment<Span>>,
        blocks: Vec<IfBlock<Span>>,
    },
    Logical {
        condition: Expression<Span>,
        statement: Statement<Span>,
    },
}

pub fn if_statement_opening<S: TextSource>() -> impl Parser<S, Token = (Expression<S::Span>, Option<LineComment<S::Span>>)> {
    (
        spaced(keyword("if")),
        spaced(expression()),
        spaced(keyword("then")),
        eol_or_comment(),
    )
        .map(|(_, cond, _, c)| (cond, c))
}

pub fn endif<S: TextSource>() -> impl Parser<S, Token = Option<LineComment<S::Span>>> {
    alt!{
        (
            spaced(keyword("end")),
            spaced(keyword("if")).optional(),
            eol_or_comment(),
        ).map(|(_, _, c)| c),
        (spaced(keyword("endif")), eol_or_comment()).map(|(_, c)| c),
    }
}

pub fn else_if<S: TextSource>() -> impl Parser<S, Token = (Expression<S::Span>, Option<LineComment<S::Span>>)> {
    (
        alt!(
            (
                spaced(keyword("else")),
                spaced(keyword("if").optional()),
            ).map(|_| ()),
            spaced(keyword("elseif")).map(|_| ()),
        ),
        spaced(expression()),
        spaced(keyword("then")),
        eol_or_comment(),
    ).map(|(_, expr, _, c)| (expr, c))
}

pub fn else_<S: TextSource>() -> impl Parser<S, Token = Option<LineComment<S::Span>>> {
    (
        spaced(keyword("else")),
        eol_or_comment(),
    ).map(|(_, c)| c)
}

pub fn termination<S: TextSource>() -> impl Parser<S, Token = (IfBodyTermination<S::Span>, Option<LineComment<S::Span>>)> {
    alt!{
        else_if().map(|(e, c)| (IfBodyTermination::ElseIf(e), c)),
        else_().map(|c| (IfBodyTermination::Else, c)),
        endif().map(|c| (IfBodyTermination::End, c)),
    }
}

pub fn if_block<S: TextSource>() -> impl Parser<S, Token = IfBlock<S::Span>> {
    many_until(
        element(),
        termination(),
        0..,
    ).map(|(items, termination)| IfBlock {
        items,
        termination,
    })
}

pub fn if_statement<S: TextSource>() -> impl Parser<S, Token = IfStatement<S::Span>> {

    (
        if_statement_opening(),
        fold_many(
            if_block(),
            || Vec::<IfBlock<S::Span>>::new(),
            |mut blocks, block| {
                let is_last = block.termination.is_none() || block.termination.as_ref().unwrap().0.is_end();
                blocks.push(block);
                (blocks, !is_last)
            },
            1..,
        )
    )
        .map(|((condition, condition_comment), blocks)| IfStatement::Statement {
            condition,
            condition_comment,
            blocks,
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

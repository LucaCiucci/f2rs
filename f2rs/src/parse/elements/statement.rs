use riddle::prelude::*;

use crate::parse::Item;

use enum_as_inner::EnumAsInner;

use super::*;

mod use_statement;
pub use use_statement::*;
mod special_function;
pub use special_function::*;
mod variables_declaration;
pub use variables_declaration::*;
mod do_loop;
pub use do_loop::*;
mod if_statement;
pub use if_statement::*;
mod call_statement;
pub use call_statement::*;
mod implicit;
pub use implicit::*;
mod format_statement;
pub use format_statement::*;

#[derive(Debug, Clone, EnumAsInner)]
pub enum Statement<Span> {
    Implicit(Implicit<Span>),
    UseStatement(UseStatement<Span>),
    VariablesDeclaration(VariablesDeclaration<Span>),
    Expression(Expression<Span>, Option<LineComment<Span>>),
    DoLoop(DoLoop<Span>),
    If(Box<IfStatement<Span>>),
    CallStatement(Expression<Span>),
    PrintStatement(SpecialStatementFunction<Span>),
    FormatStatement(FormatStatement<Span>),
    Label(usize),
    GoTo(usize, Option<LineComment<Span>>), // TODO GO TO n
}

pub fn statement<S: TextSource>() -> impl Parser<S, Token = Statement<S::Span>> {
    alt! {
        format_statement().map(Statement::FormatStatement),
        spaced(integer_literal()).map(|i| Statement::Label(i as usize)),
        (spaced(keyword("go")), spaced(keyword("to")), spaced(integer_literal()), eol_or_comment()).map(|(_, _, i, c)| Statement::GoTo(i as usize, c)),
        implicit().map(Statement::Implicit),
        use_statement().map(Statement::UseStatement),
        variables_declaration().map(Statement::VariablesDeclaration),
        do_loop(),
        if_().map(|i| Statement::If(Box::new(i))),
        call_statement(),
        special_statement_function().map(Statement::PrintStatement),
        (expression(), eol_or_comment()).map(|(e, c)| Statement::Expression(e, c)),
    }
}

// TODO tests

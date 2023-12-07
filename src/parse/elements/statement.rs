use riddle::prelude::*;

use crate::parse::Item;

use super::*;

mod use_statement; pub use use_statement::*;
mod special_function; pub use special_function::*;
mod variables_declaration; pub use variables_declaration::*;
mod do_loop; pub use do_loop::*;
mod if_statement; pub use if_statement::*;
mod call_statement; pub use call_statement::*;
mod implicit; pub use implicit::*;

#[derive(Debug, Clone)]
pub enum Statement<Span> {
    Implicit(Implicit<Span>),
    UseStatement(UseStatement),
    VariablesDeclaration(VariablesDeclaration<Span>),
    Expression(Expression<Span>),
    DoLoop {
        variable: String,
        start: Expression<Span>,
        end: Expression<Span>,
        step: Option<Expression<Span>>,
        body: Vec<Item<Span>>,
    },
    If(Box<IfStatement<Span>>),
    CallStatement(Expression<Span>),
    PrintStatement(SpecialFunction<Span>),
}

pub fn statement<S: TextSource>() -> impl Parser<S, Token = Statement<S::Span>> {
    alt! {
        implicit().map(Statement::Implicit),
        use_statement().map(Statement::UseStatement),
        variables_declaration().map(Statement::VariablesDeclaration),
        do_loop(),
        if_().map(|i| Statement::If(Box::new(i))),
        call_statement(),
        special_function().map(Statement::PrintStatement),
        (expression(), eol_or_comment()).map(|(e, _)| e).map(Statement::Expression),
    }
}
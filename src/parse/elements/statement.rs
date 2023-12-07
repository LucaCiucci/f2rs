use riddle::prelude::*;

use crate::parse::Item;

use super::*;

mod use_statement; pub use use_statement::*;
mod print_statement; pub use print_statement::*;
mod variables_declaration; pub use variables_declaration::*;
mod do_loop; pub use do_loop::*;
mod if_statement; pub use if_statement::*;
mod call_statement; pub use call_statement::*;

#[derive(Debug, Clone)]
pub enum Statement<Span> {
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
    If {
        condition: Expression<Span>,
        body: Vec<Item<Span>>,
        else_body: Option<Vec<Item<Span>>>,
    },
    CallStatement(Expression<Span>),
    PrintStatement(PrintStatement<Span>),
}

pub fn statement<S: TextSource>() -> impl Parser<S, Token = Statement<S::Span>> {
    alt! {
        use_statement().map(Statement::UseStatement),
        variables_declaration().map(Statement::VariablesDeclaration),
        do_loop(),
        if_(),
        call_statement(),
        print_statement().map(Statement::PrintStatement),
        (expression(), eol_or_comment()).map(|(e, _)| e).map(Statement::Expression),
    }
}
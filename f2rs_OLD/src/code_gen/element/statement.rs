use std::{io::{BufWriter, Write}, ops::Deref};

use crate::{parse::elements::{Statement, IfStatement, IfBodyTermination, Implicit, DoLoop, SpecialStatementFunction}, code_gen::{Span, ctx::Ctx, optional_comment, element::statement::expression::star_or_expr_2_rs, correct_identifier}};

use self::{expression::expression_2_rs, use_stmnt::use_statement_2_rs, variables_declaration::variable_declaration_2_rs, do_loop::do_loop_2_rs};

use super::element_2_rs;

pub mod expression; use expression::*;
pub mod use_stmnt; use use_stmnt::*;
pub mod variables_declaration; use variables_declaration::*;
pub mod do_loop; use do_loop::*;
pub mod if_statement; use if_statement::*;
pub mod special_function; use special_function::*;

pub fn statement_2_rs(
    statement: &Statement<Span>,
    ctx: &Ctx,
    exclude_variables: &[String],
) -> String {
    match statement {
        Statement::Expression(expression, comment) => {
            format!("{};{}\n", expression_2_rs(expression, true), optional_comment(comment))
        }
        Statement::CallStatement(expression) => {
            format!("{};\n", expression_2_rs(expression, false))
        }
        Statement::DoLoop(do_loop) => {
            do_loop_2_rs(do_loop, ctx)
        }
        Statement::If(if_statement) => if_2_rs(if_statement, ctx),
        Statement::Implicit(implicit) => match implicit {
            Implicit::ImplicitNone(comment) => {
                format!("// implicit none {}\n", optional_comment(comment))
            }
            Implicit::_Phantom(_) => { unreachable!() }
        },
        Statement::SpecialFunction(special_function) => special_function_2_rs(special_function, ctx),
        Statement::UseStatement(use_statement) => {
            format!("{}\n", use_statement_2_rs(use_statement))
        }
        Statement::VariablesDeclaration(variables_declaration) => variable_declaration_2_rs(variables_declaration, ctx, exclude_variables),
        Statement::FormatStatement(_format_statement) => {
            format!("// TODO format statement\n")
        }
        Statement::Label(i) => {
            format!("fortran_label!({});\n", i)
        }
        Statement::GoTo(i, comment) => {
            format!("fortran_goto!({});{}\n", i, optional_comment(comment))
        }
        Statement::Continue => {
            format!("fortran!(CONTINUE);\n")
        }
    }
}
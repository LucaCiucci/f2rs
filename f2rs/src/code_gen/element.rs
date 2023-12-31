use std::io::{BufWriter, Write};

use crate::{parse::{Element, function::Function, StructType}, code_gen::element::statement::statement_2_rs};

use self::program::program_2_rs;

use super::{Span, type_2_rs, correct_identifier, ctx::Ctx};

pub mod program; use program::*;
pub mod statement; use statement::*;
pub mod module; use module::*;
pub mod subroutine; use subroutine::*;
pub mod function; use function::*;
pub mod struct_type; use struct_type::*;

pub fn element_2_rs(
    element: &Element<Span>,
    ctx: &Ctx,
    exclude_variables: &[String],
) -> String {
    match element {
        Element::EmptyLines(empty_lines) => {
            format!("{}\n", &"\n".repeat(empty_lines.count))
        }
        Element::LineComment(line_comment) => {
            format!("//{}\n", line_comment.text)
        }
        Element::Program(program) => {
            program_2_rs(program, ctx)
        }
        Element::Statement(statement) => {
            format!("{}", statement_2_rs(statement, ctx, exclude_variables))
        }
        Element::Module(module) => module_2_rs(module, ctx),
        Element::Contains => format!("// contains\n"),
        Element::Subroutine(subroutine) => subroutine_2_rs(subroutine, ctx),
        Element::Function(function) => function_2_rs(function, ctx),
        Element::StructType(s) => struct_type_2_rs(s, ctx),
        Element::Public(items) => {
            let items = items.iter().map(|i| correct_identifier(&i.value)).collect::<Vec<_>>();
            format!("pub use contains::{{{}}};\n", items.join(", "))
        }
        Element::Private => {
            format!("// private\n")
        }
        Element::UnclassifiedLine(_span, string) => {
            // TODO use span
            format!("unclassified_line!(r###\"{}\"###);\n", string.trim())
        }
    }
}

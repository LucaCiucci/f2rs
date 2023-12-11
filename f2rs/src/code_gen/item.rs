use std::io::{BufWriter, Write};

use crate::{code_gen::statement_2_rs, parse::Item};

use super::Span;

pub fn item_2_rs(item: &Item<Span>) -> String {
    match item {
        Item::EmptyLines(empty_lines) => {
            format!("{}\n", &"\n".repeat(empty_lines.count))
        }
        Item::LineComment(line_comment) => {
            format!("//{}\n", line_comment.text)
        }
        Item::Program(program) => {
            let mut out = BufWriter::new(Vec::new());
            //out.push_str(&format!("mod {} {{\n", program.name));
            //writeln!(&mut out, "pub mod {} {{", program.name).unwrap();
            writeln!(&mut out, "use super::*;").unwrap();
            writeln!(&mut out, "use crate::*;").unwrap();
            writeln!(&mut out, "// program \"{}\"", program.name).unwrap();
            writeln!(&mut out, "#[rewrite_fortran_goto]").unwrap();
            writeln!(&mut out, "pub unsafe fn main() {{").unwrap();
            let mut next = program.items.len();
            for (i, item) in program.items.iter().enumerate() {
                fn next_is_statement(items: &[Item<Span>], i: usize) -> bool {
                    if i >= items.len() {
                        return false;
                    }
                    let item = &items[i];
                    if item.is_line_comment() || item.is_empty_lines() {
                        // TODO remove recursion
                        return next_is_statement(items, i + 1);
                    } else {
                        if item.is_statement() {
                            let s = item.as_statement().unwrap();
                            !(s.is_implicit() || s.is_use_statement() || s.is_variables_declaration())
                        } else {
                            false
                        }
                    }
                }
                if next_is_statement(&program.items, i) {
                    next = i;
                    writeln!(&mut out, "fortran_body!();\n").unwrap();
                    break;
                }
                write!(&mut out, "{}", item_2_rs(item)).unwrap();
            }
            for item in &program.items[next..] {
                write!(&mut out, "{}", item_2_rs(item)).unwrap();
            }
            writeln!(&mut out, "}}").unwrap();
            //writeln!(&mut out, "}}").unwrap();
            String::from_utf8(out.into_inner().unwrap()).unwrap()
        }
        Item::Statement(statement) => {
            format!("{}", statement_2_rs(statement))
        }
        Item::UnclassifiedLine(_span, string) => {
            // TODO use span
            format!("// UNCLASSIFIED LINE: {}\n", string)
        }
    }
}

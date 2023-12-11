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
            writeln!(&mut out, "pub unsafe fn main() {{").unwrap();
            for item in &program.items {
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

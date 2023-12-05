use std::str::FromStr;

use proc_macro2::{TokenStream, TokenTree};

use crate::{Parsed, parse_implicit_none, statement::Statement};

pub struct Program {
    name: String,
    has_implicit_none: bool,
    items: Vec<Statement>,
}

impl Program {
    pub fn parse(lines: &[&str], mut line: usize) -> Option<Parsed<Self>> {
        let name = parse_program_declaration_line(lines[line])?;
        let mut has_implicit_none = false;

        let mut items = vec![];

        line += 1;
        loop {
            if line >= lines.len() {
                break;
            }
            if parse_program_end(lines[line]) {
                line += 1;
                break;
            }
            if parse_implicit_none(lines[line]) {
                has_implicit_none = true;
                line += 1;
                continue;
            }
            let parsed_item = Statement::parse(lines, line);
            let item = parsed_item.parsed;
            items.push(item);
            line = parsed_item.next;
        }

        Some(Parsed {
            parsed: Program {
                name,
                has_implicit_none,
                items,
            },
            next: line,
        })
    }

    pub fn write(&self, file: &mut dyn std::io::Write) -> std::io::Result<()> {
        writeln!(file, "fn main() {{ // PROGRAM {}", self.name)?;

        if !self.has_implicit_none {
            writeln!(file, "// MISSING implicit none")?;
        }

        for item in &self.items {
            item.write(file)?;
        }

        writeln!(file, "}}")
    }
}

fn parse_program_declaration_line(line: &str) -> Option<String> {
    let ts = TokenStream::from_str(line)
        .ok()?
        .into_iter()
        .collect::<Vec<_>>();

    if ts.len() != 2 {
        return None;
    }

    // find the "program" keyword
    if let TokenTree::Ident(ident) = &ts[0] {
        if ident.to_string() != "program" {
            return None;
        }
    } else {
        return None;
    }

    if let TokenTree::Ident(ident) = &ts[1] {
        return Some(ident.to_string());
    }

    None
}

pub fn parse_program_end(line: &str) -> bool {
    let ts = match TokenStream::from_str(line) {
        Ok(ts) => ts,
        Err(_) => return false,
    };

    let mut ts = ts.into_iter();

    let ok = if let Some(TokenTree::Ident(ident)) = ts.next() {
        ident.to_string().to_lowercase() == "end"
    } else {
        false
    };

    let ok = ok && if let Some(TokenTree::Ident(ident)) = ts.next() {
        ident.to_string().to_lowercase() == "program"
    } else {
        false
    };

    ok
}

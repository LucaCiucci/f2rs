use std::{io::Write, primitive, rc::{Rc, Weak}, cell::RefCell};

use proc_macro2::{TokenTree, Delimiter, Punct, Spacing};

use crate::{Parsed, parse_comment, parse_token_vec};

use self::variables::{VariablesDeclaration, parse_variables_declaration};

pub mod variables;

pub enum Statement {
    Empty,
    Comment(String),
    UnparsedLine(String),
    Use(String),
    VariableDeclaration(Rc<RefCell<VariablesDeclaration>>),
    // TODO ...
}

impl Statement {
    pub fn parse(lines: &[&str], line: usize) -> Parsed<Self> {
        if lines[line].is_empty() {
            return Parsed::new(Statement::Empty, line + 1);
        }
        if let Some(comment) = parse_comment(lines[line]) {
            return Parsed::new(Statement::Comment(comment), line + 1);
        }
        if let Some(module) = parse_use_module(lines[line]) {
            return Parsed::new(Statement::Use(module), line + 1);
        }
        if let Some(var) = parse_variables_declaration(lines[line]) {
            return Parsed::new(Statement::VariableDeclaration(var), line + 1);
        }
        Parsed::new(Statement::UnparsedLine(lines[line].to_string()), line + 1)
    }
    pub fn write(&self, w: &mut dyn Write) -> std::io::Result<()> {
        match self {
            Statement::Empty => {
                writeln!(w)?;
            },
            Statement::Comment(comment) => {
                writeln!(w, "// {}", comment)?;
            },
            Statement::UnparsedLine(line) => {
                writeln!(w, "{} // UNPARSED", line)?;
            },
            Statement::Use(module) => {
                writeln!(w, "use super::{}::*;", module)?;
            },
            Statement::VariableDeclaration(decl) => {
                // TODO
                for var in &decl.borrow().variables {
                    writeln!(w, "let mut {}: {};", var.name, var.ty.rust_variable_type())?;
                }
            }
        }
        Ok(())
    }
}

fn parse_use_module(line: &str) -> Option<String> {
    let tokens = parse_token_vec(line)?;
    if tokens.len() != 2 {
        return None;
    }

    if let TokenTree::Ident(ident) = &tokens[0] {
        if ident.to_string() != "use" {
            return None;
        }
    } else {
        return None;
    }

    if let TokenTree::Ident(ident) = &tokens[1] {
        return Some(ident.to_string());
    } else {
        return None;
    }
}
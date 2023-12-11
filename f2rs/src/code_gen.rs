use std::{
    io::{BufWriter, Write},
    ops::Deref,
};

mod item;
use item::*;

use crate::parse::{
    elements::{
        CallOrIndexing, Expression, IfStatement, Implicit, Literal, StarOrExpr, Statement,
        UseStatement, VariablesDeclaration, Type, BasicType, LineComment,
    },
    File,
};

type Span = ();

pub fn file_2_rs(file: &File<Span>) -> String {
    let mut out = BufWriter::new(Vec::new());

    writeln!(&mut out, "use f2rs_adapter::prelude::*;").unwrap();
    writeln!(&mut out, "").unwrap();

    for item in &file.items {
        write!(&mut out, "{}", item_2_rs(item)).unwrap();
    }

    String::from_utf8(out.into_inner().unwrap()).unwrap()
}

fn optional_comment(
    comment: &Option<LineComment<Span>>,
) -> String {
    comment.as_ref().map(|c| format!(" //{}", c.text)).unwrap_or("".into())
}

fn statement_2_rs(statement: &Statement<Span>) -> String {
    let mut out = BufWriter::new(Vec::new());

    match statement {
        Statement::Expression(expression, comment) => {
            writeln!(&mut out, "{};{}", expression_2_rs(expression, true), optional_comment(comment)).unwrap();
        }
        Statement::CallStatement(expression) => {
            writeln!(&mut out, "{};", expression_2_rs(expression, false)).unwrap();
        }
        Statement::DoLoop(do_loop) => {
            if let Some(step) = &do_loop.step {
                writeln!(
                    &mut out,
                    "for {} in ({}..={}).step_by({}) {{{}",
                    do_loop.variable,
                    expression_2_rs(&do_loop.start, false),
                    expression_2_rs(&do_loop.end, false),
                    expression_2_rs(&step, false),
                    optional_comment(&do_loop.opening_comment),
                )
                .unwrap();
            } else {
                writeln!(
                    &mut out,
                    "for {} in {}..={} {{{}",
                    do_loop.variable,
                    expression_2_rs(&do_loop.start, false),
                    expression_2_rs(&do_loop.end, false),
                    optional_comment(&do_loop.opening_comment),
                )
                .unwrap();
            }
            for item in &do_loop.body {
                write!(&mut out, "{}", item_2_rs(item)).unwrap();
            }
            writeln!(&mut out, "}}").unwrap();
        }
        Statement::If(if_statement) => match if_statement.deref() {
            IfStatement::Statement {
                condition,
                condition_comment,
                body,
                body_closing_comment,
                else_body,
                else_closing_comment,
            } => {
                writeln!(&mut out, "if {} {{{}", expression_2_rs(condition, false), optional_comment(condition_comment)).unwrap();
                for item in body {
                    write!(&mut out, "{}", item_2_rs(item)).unwrap();
                }
                writeln!(&mut out, "}}").unwrap();
                if let Some(else_body) = else_body {
                    writeln!(&mut out, "else {{").unwrap();
                    for item in else_body {
                        write!(&mut out, "{}", item_2_rs(item)).unwrap();
                    }
                    writeln!(&mut out, "}}").unwrap();
                }
            }
            IfStatement::Logical {
                condition,
                statement,
            } => {
                writeln!(&mut out, "if {} {{", expression_2_rs(condition, false)).unwrap();
                write!(&mut out, "{}", statement_2_rs(statement)).unwrap();
                writeln!(&mut out, "}}").unwrap();
            }
        },
        Statement::Implicit(implicit) => match implicit {
            Implicit::ImplicitNone(comment) => {
                writeln!(&mut out, "// implicit none {}", optional_comment(comment)).unwrap();
            }
            Implicit::_Phantom(_) => {}
        },
        Statement::PrintStatement(special_function) => {
            let special = if let Some(specials) = &special_function.special_args {
                let s = specials
                    .iter()
                    .map(|se| star_or_expr_2_rs(se))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!(" ({})", s)
            } else {
                String::new()
            };
            let items = special_function
                .items
                .iter()
                .map(|se| star_or_expr_2_rs(se))
                .collect::<Vec<_>>()
                .join(", ");
            writeln!(
                &mut out,
                "fortran!({}{} {});",
                special_function.name.value, special, items
            )
            .unwrap();
        }
        Statement::UseStatement(use_statement) => {
            writeln!(&mut out, "{}", use_statement_2_rs(use_statement)).unwrap();
        }
        Statement::VariablesDeclaration(variables_declaration) => {
            writeln!(&mut out, "{}", variable_declaration_2_rs(variables_declaration)).unwrap();
        }
        Statement::FormatStatement(format_statement) => {
            writeln!(&mut out, "// TODO format statement").unwrap();
        }
        Statement::Label(i) => {
            writeln!(&mut out, "fortran_label!({});", i).unwrap();
        }
        Statement::GoTo(i, comment) => {
            writeln!(&mut out, "fortran_goto!({});{}", i, optional_comment(comment)).unwrap();
        }
    }

    String::from_utf8(out.into_inner().unwrap()).unwrap()
}

fn variable_declaration_2_rs(variable_declaration: &VariablesDeclaration<Span>) -> String {
    let mut out = BufWriter::new(Vec::new());

    if variable_declaration.comment.is_some() {
        writeln!(&mut out, "{}", optional_comment(&variable_declaration.comment)).unwrap();
    }
    for (ty, name) in &variable_declaration.vars {
        writeln!(&mut out, "let mut {}: {} = {};", name, type_2_rs(ty).0, type_2_rs(ty).1).unwrap();
    }

    String::from_utf8(out.into_inner().unwrap()).unwrap()
}

fn type_2_rs(ty: &Type<Span>) -> (String, String) {
    match ty {
        Type::Basic(ty) => basic_type_2_rs(ty),
        Type::Array { ty, ranges } => ("TODO".into(), "TODO".into()),
        Type::BasicAlias(_, _) => ("TODO".into(), "TODO".into()),
        Type::Type(_) => ("TODO".into(), "TODO".into()),
    }
}

fn basic_type_2_rs(ty: &BasicType) -> (String, String) {
    match ty {
        BasicType::Integer => ("integer".into(), "0".into()),
        BasicType::Integer2 => ("integer2".into(), "0".into()),
        BasicType::Integer4 => ("integer4".into(), "0".into()),
        BasicType::Integer8 => ("integer8".into(), "0".into()),
        BasicType::Integer16 => ("integer16".into(), "0".into()),
        BasicType::Real => ("real".into(), "0.0".into()),
        BasicType::Real4 => ("real4".into(), "0.0".into()),
        BasicType::Real8 => ("real8".into(), "0.0".into()),
        BasicType::Real16 => ("real16".into(), "0.0".into()),
        BasicType::Complex => ("complex".into(), "0.0".into()),
        BasicType::Complex8 => ("complex8".into(), "0.0".into()),
        BasicType::Complex16 => ("complex16".into(), "0.0".into()),
        BasicType::Complex32 => ("complex32".into(), "0.0".into()),
        BasicType::DoubleComplex => ("double_complex".into(), "0.0".into()),
        BasicType::Logical => ("logical".into(), "false".into()),
        BasicType::Logical1 => ("logical1".into(), "false".into()),
        BasicType::Logical2 => ("logical2".into(), "false".into()),
        BasicType::Logical4 => ("logical4".into(), "false".into()),
        BasicType::Logical8 => ("logical8".into(), "false".into()),
        BasicType::Character => ("character".into(), "'\0'".into()),
        BasicType::CharacterN(_) => ("TODO".into(), "TODO".into()),
        BasicType::DoublePrecision => ("double_precision".into(), "0.0".into()),
    }
}

fn use_statement_2_rs(use_statement: &UseStatement<Span>) -> String {
    if use_statement.only.is_empty() {
        format!("use {}::*;{}", use_statement.module_name, optional_comment(&use_statement.comment))
    } else {
        let only = use_statement
            .only
            .iter()
            .map(|only| only.clone())
            .collect::<Vec<_>>()
            .join(", ");
        format!("use {}::{{{}}};{}", use_statement.module_name, only, optional_comment(&use_statement.comment))
    }
}

fn star_or_expr_2_rs(star_or_expr: &StarOrExpr<Span>) -> String {
    match star_or_expr {
        StarOrExpr::Star => String::from("*"),
        StarOrExpr::Expression(expression) => expression_2_rs(expression, false),
    }
}

fn expression_2_rs(expression: &Expression<Span>, as_statement: bool) -> String {
    match expression {
        Expression::CallOrIndexing(call_or_indexing) => call_or_indexing_2_rs(call_or_indexing),
        Expression::Identifier(identifier) => identifier.value.clone(),
        Expression::IfArithmetic(if_arithmetic) => {
            let mut out = BufWriter::new(Vec::new());

            let elements = if_arithmetic
                .cases
                .iter()
                .map(|expr| expression_2_rs(expr, false))
                .collect::<Vec<_>>()
                .join(", ");

            format!(
                "select!({} => {})",
                expression_2_rs(&if_arithmetic.selector, false),
                elements
            )
        }
        Expression::IndexRange(index_range) => match (&index_range.start, &index_range.end) {
            (Some(start), Some(end)) => format!(
                "{}..{}",
                expression_2_rs(start, false),
                expression_2_rs(end, false)
            ),
            (Some(start), None) => format!("{}..", expression_2_rs(start, false)),
            (None, Some(end)) => format!("..{}", expression_2_rs(end, false)),
            (None, None) => format!(".."),
        },
        Expression::Literal(literal) => literal_2_rs(literal),
        Expression::Operation(operation) => {
            // TODO other special operations
            match operation.operator.value {
                "**" => format!(
                    "pow!({}, {})",
                    expression_2_rs(&operation.left, false),
                    expression_2_rs(&operation.right, false)
                ),
                "=" => {
                    format!("assign!({}, {})", expression_2_rs(&operation.left, false), expression_2_rs(&operation.right, false))
                }
                "+" => {
                    format!("add!({}, {})", expression_2_rs(&operation.left, false), expression_2_rs(&operation.right, false))
                }
                "-" => {
                    format!("sub!({}, {})", expression_2_rs(&operation.left, false), expression_2_rs(&operation.right, false))
                }
                "*" => {
                    format!("mul!({}, {})", expression_2_rs(&operation.left, false), expression_2_rs(&operation.right, false))
                }
                "/" => {
                    format!("div!({}, {})", expression_2_rs(&operation.left, false), expression_2_rs(&operation.right, false))
                }
                // TODO ...
                op => {
                    if as_statement {
                        format!(
                            "{} {} {}",
                            expression_2_rs(&operation.left, false),
                            op,
                            expression_2_rs(&operation.right, false)
                        )
                    } else {
                        format!(
                            "({} {} {})",
                            expression_2_rs(&operation.left, false),
                            op,
                            expression_2_rs(&operation.right, false)
                        )
                    }
                },
            }
        }
        Expression::Parenthesis(parenthesis) => {
            let mut out = BufWriter::new(Vec::new());

            write!(&mut out, "({})", expression_2_rs(parenthesis, false)).unwrap();

            String::from_utf8(out.into_inner().unwrap()).unwrap()
        }
        Expression::UnaryLeftOperation(unary_left_operation) => {
            let mut out = BufWriter::new(Vec::new());

            write!(
                &mut out,
                "{}{}",
                unary_left_operation.operator.value,
                expression_2_rs(&unary_left_operation.right, false)
            )
            .unwrap();

            String::from_utf8(out.into_inner().unwrap()).unwrap()
        }
    }
}

fn call_or_indexing_2_rs(call_or_indexing: &CallOrIndexing<Span>) -> String {
    let mut out = BufWriter::new(Vec::new());

    if let Some(f) = call_or_indexing.function.as_identifier() {
        match f.value.to_lowercase().as_str() {
            "float" => {
                return format!(
                    "fortran!({}({}))",
                    f.value,
                    expression_2_rs(&call_or_indexing.arguments[0], false)
                );
            }
            _ => {}
        }
    }

    write!(
        &mut out,
        "call!({}(",
        expression_2_rs(&call_or_indexing.function, false)
    )
    .unwrap();
    for (i, arg) in call_or_indexing.arguments.iter().enumerate() {
        if i > 0 {
            write!(&mut out, ", ").unwrap();
        }
        write!(&mut out, "{}", expression_2_rs(arg, false)).unwrap();
    }
    write!(&mut out, "))").unwrap();

    String::from_utf8(out.into_inner().unwrap()).unwrap()
}

fn literal_2_rs(literal: &Literal<Span>) -> String {
    match literal {
        Literal::True => String::from("true"),
        Literal::False => String::from("false"),
        Literal::String(string) => format!("\"{}\"", string.value()),
        Literal::Number(number) => number.value.clone(),
    }
}

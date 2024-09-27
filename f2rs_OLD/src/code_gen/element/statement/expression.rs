use std::io::{BufWriter, Write};

use crate::{parse::elements::{Expression, CallOrIndexing, StarOrExpr}, code_gen::{Span, correct_identifier, literal_2_rs}};

pub fn expression_2_rs(expression: &Expression<Span>, as_statement: bool) -> String {
    match expression {
        Expression::CallOrIndexing(call_or_indexing) => call_or_indexing_2_rs(call_or_indexing),
        Expression::Identifier(identifier) => correct_identifier(&identifier.value),
        Expression::IfArithmetic(if_arithmetic) => {
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
                "%" | ".mod." => format!(
                    "mod_!({}, {})",
                    expression_2_rs(&operation.left, false),
                    expression_2_rs(&operation.right, false)
                ),
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
                "//" => {
                    format!("concat!({}, {})", expression_2_rs(&operation.left, false), expression_2_rs(&operation.right, false))
                }
                "<" | ".lt." => {
                    format!("lt!({}, {})", expression_2_rs(&operation.left, false), expression_2_rs(&operation.right, false))
                }
                ">" | ".gt." => {
                    format!("gt!({}, {})", expression_2_rs(&operation.left, false), expression_2_rs(&operation.right, false))
                }
                "<=" | ".le." => {
                    format!("le!({}, {})", expression_2_rs(&operation.left, false), expression_2_rs(&operation.right, false))
                }
                ">=" | ".ge." => {
                    format!("ge!({}, {})", expression_2_rs(&operation.left, false), expression_2_rs(&operation.right, false))
                }
                "==" | ".eq." => {
                    format!("eq!({}, {})", expression_2_rs(&operation.left, false), expression_2_rs(&operation.right, false))
                }
                "/=" | ".ne." => {
                    format!("ne!({}, {})", expression_2_rs(&operation.left, false), expression_2_rs(&operation.right, false))
                }
                ".and." => {
                    format!("and!({}, {})", expression_2_rs(&operation.left, false), expression_2_rs(&operation.right, false))
                }
                ".or." => {
                    format!("or!({}, {})", expression_2_rs(&operation.left, false), expression_2_rs(&operation.right, false))
                }
                ".not." => {
                    format!("not!({}, {})", expression_2_rs(&operation.left, false), expression_2_rs(&operation.right, false))
                }
                ".eqv." => {
                    format!("eqv!({}, {})", expression_2_rs(&operation.left, false), expression_2_rs(&operation.right, false))
                }
                ".neqv." => {
                    format!("neqv!({}, {})", expression_2_rs(&operation.left, false), expression_2_rs(&operation.right, false))
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

            let op = unary_left_operation.operator.value;

            let op = match op {
                "+" => "plus".into(),
                "-" => "minus".into(),
                ".not." => "not".into(),
                _ => format!("Unknown unary left operation: {}", op),
            };

            write!(
                &mut out,
                "{}!({})",
                op,
                expression_2_rs(&unary_left_operation.right, false)
            )
            .unwrap();

            String::from_utf8(out.into_inner().unwrap()).unwrap()
        }
        Expression::Array(elements) => {
            let mut out = BufWriter::new(Vec::new());

            write!(&mut out, "[").unwrap();
            for (i, element) in elements.iter().enumerate() {
                if i > 0 {
                    write!(&mut out, ", ").unwrap();
                }
                write!(&mut out, "{}", expression_2_rs(element, false)).unwrap();
            }
            write!(&mut out, "]").unwrap();

            String::from_utf8(out.into_inner().unwrap()).unwrap()
        }
    }
}

fn call_or_indexing_2_rs(call_or_indexing: &CallOrIndexing<Span>) -> String {
    let mut out = BufWriter::new(Vec::new());

    if let Some(f) = call_or_indexing.function.as_identifier() {
        match f.value.to_lowercase().as_str() {
            "float" | "kind" => {
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

pub fn star_or_expr_2_rs(star_or_expr: &StarOrExpr<Span>) -> String {
    match star_or_expr {
        StarOrExpr::Star => String::from("*"),
        StarOrExpr::Expression(expression) => expression_2_rs(expression, false),
    }
}
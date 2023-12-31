use std::{io::{BufWriter, Write}, fmt::format};

use crate::{parse::{Program, Element, elements::TypeWithModifiers}, code_gen::{Span, ctx::Ctx, element::element_2_rs, type_2_rs}};

// TODO move:
// writeln!(&mut out, "use super::*;").unwrap();
// writeln!(&mut out, "use crate::*;").unwrap();

pub fn program_2_rs(
    program: &Program<Span>,
    ctx: &Ctx,
) -> String {
    let mut out = BufWriter::new(Vec::new());

    let name = format!("{}_main", program.name);

    writeln!(&mut out, "// program \"{}\"", program.name).unwrap();
    write_function(
        &mut out,
        &name,
        &[],
        None,
        &program.body,
        ctx,
    );

    String::from_utf8(out.into_inner().unwrap()).unwrap()
}

pub fn write_function(
    out: &mut BufWriter<Vec<u8>>,
    name: &str,
    args: &[(String, Option<TypeWithModifiers<Span>>)],
    return_type: Option<&TypeWithModifiers<Span>>,
    body: &[Element<Span>],
    ctx: &Ctx,
) {
    // TODO handle optionals
    let function_args = args.iter().map(|(name, ty)| {
        if let Some(ty) = ty {
            format!("{}: &mut {}", name, type_2_rs(&ty.ty, ctx).0)
        } else {
            format!("{}: unresolved!()", name)
        }
    }).collect::<Vec<_>>().join(", ");

    writeln!(out, "#[fortran_function]").unwrap();
    if let Some(return_type) = return_type {
        writeln!(out, "pub unsafe fn {}({function_args}) -> {} {{", name, type_2_rs(&return_type.ty, ctx).0).unwrap();
    } else {
        writeln!(out, "pub unsafe fn {}({function_args}) {{", name).unwrap();
    }

    let excluded_variables = args.iter().map(|(name, _)| name.clone()).collect::<Vec<_>>();

    function_body_2_rs(
        out,
        body,
        ctx,
        &excluded_variables,
    );

    writeln!(out, "}}").unwrap();
}

pub fn function_body_2_rs(
    out: &mut BufWriter<Vec<u8>>,
    body: &[Element<Span>],
    ctx: &Ctx,
    exclude_variables: &[String],
) {
    let ctx = &ctx.in_body(body);

    let mut next = body.len();
    for (i, element) in body.iter().enumerate() {
        fn next_is_statement(elements: &[Element<Span>], i: usize) -> bool {
            if i >= elements.len() {
                return false;
            }
            let element = &elements[i];
            if element.is_line_comment() || element.is_empty_lines() {
                // TODO remove recursion
                return next_is_statement(elements, i + 1);
            } else {
                if element.is_statement() {
                    let s = element.as_statement().unwrap();
                    !(s.is_implicit() || s.is_use_statement() || s.is_variables_declaration())
                } else {
                    false
                }
            }
        }
        if next_is_statement(&body, i) {
            next = i;
            writeln!(out, "fortran_body!();\n").unwrap();
            break;
        }
        write!(out, "{}", element_2_rs(element, ctx, exclude_variables)).unwrap();
    }
    for item in &body[next..] {
        write!(out, "{}", element_2_rs(item, ctx, exclude_variables)).unwrap();
    }
}
use std::{
    io::{BufWriter, Write},
    ops::Deref,
};

pub mod element;
use enum_as_inner::EnumAsInner;
pub use element::*;
pub mod ctx;

use crate::parse::{
    elements::{
        CallOrIndexing, Expression, IfStatement, Implicit, Literal, StarOrExpr, Statement,
        UseStatement, VariablesDeclaration, Type, BasicType, LineComment, TypeWithModifiers, Modifiers, IfBlock, IfBodyTermination,
    },
    File,
};

use self::{ctx::Ctx, statement::expression::expression_2_rs};

type Span = ();

pub fn correct_identifier(s: &str) -> String {
    match s {
        "ref" => "ref_".into(),
        "type" => "type_".into(),
        "struct" => "struct_".into(),
        "enum" => "enum_".into(),
        "union" => "union_".into(),
        "match" => "match_".into(),
        "loop" => "loop_".into(),
        "while" => "while_".into(),
        "for" => "for_".into(),
        "if" => "if_".into(),
        "else" => "else_".into(),
        "continue" => "continue_".into(),
        "break" => "break_".into(),
        "return" => "return_".into(),
        "impl" => "impl_".into(),
        "trait" => "trait_".into(),
        "fn" => "fn_".into(),
        "let" => "let_".into(),
        "mut" => "mut_".into(),
        "const" => "const_".into(),
        "static" => "static_".into(),
        "use" => "use_".into(),
        "as" => "as_".into(),
        "mod" => "mod_".into(),
        "pub" => "pub_".into(),
        "crate" => "crate_".into(),
        "super" => "super_".into(),
        "self" => "self_".into(),
        _ => if s.ends_with("_") { s.to_string() + "_" } else { s.into() },
    }
}

pub fn file_2_rs(file: &File<Span>, original_path: Option<&str>) -> String {
    let ctx = Ctx::root();

    let mut out = BufWriter::new(Vec::new());

    writeln!(
        &mut out,
        "// Converted with f2rs v{}.{}.{}",
        env!("CARGO_PKG_VERSION_MAJOR"),
        env!("CARGO_PKG_VERSION_MINOR"),
        env!("CARGO_PKG_VERSION_PATCH"),
    ).unwrap();
    if let Some(original_path) = original_path {
        writeln!(&mut out, "// From: {}", original_path).unwrap();
    }
    writeln!(&mut out, "use f2rs_adapter::prelude::*;").unwrap();
    writeln!(
        &mut out,
        "f2rs_version!({}_{}_{}, requires adapter >= {}_{}_{});",
        env!("CARGO_PKG_VERSION_MAJOR"),
        env!("CARGO_PKG_VERSION_MINOR"),
        env!("CARGO_PKG_VERSION_PATCH"),
        env!("CARGO_PKG_VERSION_MAJOR"),
        env!("CARGO_PKG_VERSION_MINOR"),
        env!("CARGO_PKG_VERSION_PATCH"),
    ).unwrap();
    writeln!(&mut out, "").unwrap();

    for item in &file.items {
        write!(&mut out, "{}", element_2_rs(item, &ctx, &[])).unwrap();
    }

    String::from_utf8(out.into_inner().unwrap()).unwrap()
}

fn optional_comment(
    comment: &Option<LineComment<Span>>,
) -> String {
    comment.as_ref().map(|c| format!(" //{}", c.text)).unwrap_or("".into())
}

fn type_2_rs(ty: &Type<Span>, ctx: &Ctx) -> (String, String) {
    match ty {
        Type::Basic(ty) => basic_type_2_rs(ty),
        Type::Array { .. } => ("TODO".into(), "TODO".into()),
        Type::BasicWithKind(basic, kind) => (basic_type_2_rs(basic).0 + &format!("::<{}>", expression_2_rs(kind, false)), basic_type_2_rs(basic).1),
        Type::Type(ty) => (correct_identifier(&ty.value), "Default::default()".into()),
    }
}

fn basic_type_2_rs(ty: &BasicType<Span>) -> (String, String) {
    match ty {
        BasicType::Integer => ("integer".into(), "0".into()),
        BasicType::Real => ("real".into(), "0.0".into()),
        BasicType::Complex => ("complex".into(), "Default::default()".into()),
        BasicType::DoubleComplex => ("double_complex".into(), "Default::default()".into()),
        BasicType::Logical => ("logical".into(), "false".into()),
        BasicType::Character => ("character".into(), "'\\0'".into()),
        BasicType::CharacterN(_) => ("TODO".into(), "TODO".into()),
        BasicType::DoublePrecision => ("double_precision".into(), "0.0".into()),
    }
}

fn literal_2_rs(literal: &Literal<Span>) -> String {
    match literal {
        Literal::True => String::from("true"),
        Literal::False => String::from("false"),
        Literal::String(string) => format!(
            "\"{}\"",
            string.value()
                .replace("\\", "\\\\")
                .replace("\"", "\\\"")
                .replace("\n", "\\n")
                .replace("\r", "\\r")
                .replace("\t", "\\t")
                .replace("\0", "\\0"),
                // TODO ...
        ),
        Literal::Number(number) => number.value.clone(),
    }
}

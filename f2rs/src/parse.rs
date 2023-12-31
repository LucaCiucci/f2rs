use f2rs_parser_combinator::{
    provided::{
        common::{many, many_until},
        text::{Char, rusty::Identifier},
    },
    tokenization::{Parser, TextSource},
};

use crate::parse::elements::*;
use f2rs_parser_combinator::prelude::*;
use enum_as_inner::EnumAsInner;

use self::elements::{empty_lines, line_comment, EmptyLines, LineComment};

use f2rs_parser_combinator::alt;

pub mod elements;
pub mod function; use function::*;

#[derive(Debug, Clone, EnumAsInner)]
pub enum Element<Span> { // or construct?
    EmptyLines(EmptyLines),
    LineComment(LineComment<Span>),
    Program(Program<Span>),
    Statement(Statement<Span>),
    Module(Module<Span>),
    Contains,
    Subroutine(Subroutine<Span>),
    Function(Function<Span>),
    StructType(StructType<Span>),
    Public(Vec<Identifier<Span>>),
    Private,
    UnclassifiedLine(Span, String),
}

pub fn element<S: TextSource>() -> impl Parser<S, Token = Element<S::Span>> {
    alt! {
        empty_lines().map(Element::EmptyLines),
        spaced(line_comment()).map(Element::LineComment),
        program_definition().map(Element::Program),
        contains(),
        subroutine_definition().map(Element::Subroutine),
        function().map(Element::Function),
        struct_type().map(Element::StructType),
        module_definition().map(Element::Module),
        public(),
        spaced(keyword("private")).map(|_| Element::Private),
        statement().map(Element::Statement),
        unclassified_line().map(|(sp, s)| Element::UnclassifiedLine(sp, s)),
    }
}

#[derive(Debug, Clone)]
pub struct StructType<Span> {
    // TODO comments
    pub name: String,
    pub body: Vec<Element<Span>>,
}

pub fn struct_type<S: TextSource>() -> impl Parser<S, Token = StructType<S::Span>> {
    (
        spaced(keyword("type")),
        spaced(identifier()),
        eol_or_comment(),
    )
        .then(|(_, name, _)| {
            many_until(
                element(),
                (
                    alt! {
                        spaced(keyword("endtype")),
                        spaced(keyword("end")).then(|_| spaced(keyword("type"))),
                    },
                    spaced(identifier()).optional(),
                    eol_or_comment(),
                ),
                0..
            ).map(move |(body, _)| StructType { name: name.value.clone(), body })
        })
}

pub fn elements<S: TextSource>() -> impl Parser<S, Token = File<S::Span>> {
    many(element(), 0..).map(|element| File { items: element })
}

#[derive(Debug, Clone)]
pub struct Subroutine<Span> {
    pub name: String,
    pub args: Vec<Identifier<Span>>,
    pub body: Vec<Element<Span>>,
}

pub fn subroutine_declaration<S: TextSource>() -> impl Parser<S, Token = (String, Vec<Identifier<S::Span>>)> {
    (
        spaced(keyword("recursive")).optional(),
        spaced(keyword("subroutine")),
        spaced(identifier()),
        spaced(keyword("(")),
        separated(spaced(identifier()), spaced(keyword(",")), 0..),
        spaced(keyword(")")),
        eol_or_comment(),
    ).map(|(_, _, name, _, args, _, _)| (name.value, args))
}

pub fn subroutine_end<S: TextSource>() -> impl Parser<S, Token = ()> {
    (
        spaced(keyword("end")),
        (spaced(keyword("subroutine")), spaced(identifier()).optional()).optional(),
        eol_or_comment(),
    )
        .map(|_| ())
}

pub fn subroutine_definition<S: TextSource>() -> impl Parser<S, Token = Subroutine<S::Span>> {
    subroutine_declaration()
        .then(|(name, args)| {
            many_until(element(), subroutine_end(), 0..).map(move |(body, _)| Subroutine { name: name.clone(), args: args.clone(), body })
        })
}

pub fn contains<S: TextSource>() -> impl Parser<S, Token = Element<S::Span>> {
    (spaced(keyword("contains")), eol_or_comment()).map(|_| Element::Contains)
}

pub fn public<S: TextSource>() -> impl Parser<S, Token = Element<S::Span>> {
    (
        spaced(keyword("public")),
        spaced(keyword("::")).optional(),
        separated(spaced(identifier()), spaced(keyword(",")), 1..),
        eol_or_comment(),
    )
        .map(|(_, _, ids, _)| Element::Public(ids))
}

#[derive(Debug, Clone)]
pub struct Module<Span> {
    pub name: String,
    pub items: Vec<Element<Span>>,
}

pub fn module_declaration<S: TextSource>() -> impl Parser<S, Token = String> {
    (
        spaced(keyword("module")),
        spaced(identifier()),
        eol_or_comment(),
    )
        .map(|(_, name, _)| name.value)
}

pub fn module_end<S: TextSource>() -> impl Parser<S, Token = ()> {
    (
        spaced(keyword("end")),
        (spaced(keyword("module")), spaced(identifier()).optional()).optional(),
        eol_or_comment(),
    )
        .map(|_| ())
}

pub fn module_definition<S: TextSource>() -> impl Parser<S, Token = Module<S::Span>> {
    module_declaration()
        .then(|name| {
            many_until(element(), module_end(), 0..).map(move |(items, _)| (name.clone(), items))
        })
        .map(|(name, items)| Module { name, items })
}

#[derive(Debug, Clone)]
pub struct File<Span> {
    pub items: Vec<Element<Span>>,
}

#[derive(Debug, Clone)]
pub struct Program<Span> {
    pub name: String,
    pub body: Vec<Element<Span>>,
}

pub fn program_declaration<S: TextSource>() -> impl Parser<S, Token = String> {
    (
        spaced(keyword("program")),
        spaced(identifier()),
        eol_or_comment(),
    )
        .map(|(_, name, _)| name.value)
}

pub fn program_end<S: TextSource>() -> impl Parser<S, Token = ()> {
    (
        spaced(keyword("end")),
        (spaced(keyword("program")), spaced(identifier()).optional()).optional(),
        eol_or_comment(),
    )
        .map(|_| ())
}

pub fn program_definition<S: TextSource>() -> impl Parser<S, Token = Program<S::Span>> {
    program_declaration()
        .then(|name| {
            many_until(element(), program_end(), 0..).map(move |(items, _)| (name.clone(), items))
        })
        .map(|(name, items)| Program { name, body: items })
}

pub fn unclassified_line<S: TextSource>() -> impl Parser<S, Token = (S::Span, String)> {
    many_until(Char::<S::Span>::any(), eol_or_comment(), 1..).map(|(chars, _newline)| {
        if chars.is_empty() {
            (S::null_span(), String::new())
        } else {
            let mut span = chars.first().unwrap().span.clone();
            for c in chars.iter().skip(1) {
                span = S::merge_span(span, c.span.clone());
            }
            (span, chars.iter().map(|c| c.value).collect())
        }
    })
}

// TODO tests

use riddle::{tokenization::{TextSource, Parser}, provided::{common::{many, many_until}, text::Char}};

use crate::parse::elements::*;
use riddle::prelude::*;

use self::elements::{LineComment, empty_lines, EmptyLines, line_comment};

use riddle::alt;

pub mod elements;

#[derive(Debug, Clone)]
pub enum Item<Span> {
    EmptyLines(EmptyLines),
    LineComment(LineComment<Span>),
    Program(Program<Span>),
    Statement(Statement<Span>),
    UnclassifiedLine(Span),
}

pub fn item<S: TextSource>() -> impl Parser<S, Token = Item<S::Span>> {
    alt! {
        empty_lines().map(Item::EmptyLines),
        line_comment().map(Item::LineComment),
        program_definition().map(Item::Program),
        statement().map(Item::Statement),
        unclassified_line().map(Item::UnclassifiedLine),
    }
}

pub fn items<S: TextSource>() -> impl Parser<S, Token = File<S::Span>> {
    many(
        item(),
        0..,
    ).map(|items| File { items })
}

#[derive(Debug, Clone)]
pub struct File<Span> {
    pub items: Vec<Item<Span>>,
}

#[derive(Debug, Clone)]
pub struct Program<Span> {
    pub name: String,
    pub items: Vec<Item<Span>>,
}

pub fn program_declaration<S: TextSource>() -> impl Parser<S, Token = String> {
    (
        spaced(keyword("program")),
        spaced(identifier()),
        eol_or_comment(),
    ).map(|(_, name, _)| name.value)
}

pub fn program_end<S: TextSource>() -> impl Parser<S, Token = ()> {
    (
        spaced(keyword("end")),
        (
            spaced(keyword("program")),
            spaced(identifier()).optional(),
        ).optional(),
        eol_or_comment(),
    ).map(|_| ())
}

pub fn program_definition<S: TextSource>() -> impl Parser<S, Token = Program<S::Span>> {
    program_declaration()
        .then(|name|
            many_until(
                item(),
                program_end(),
                0..,
            ).map(move |(items, _)| (name.clone(), items))
        )
        .map(|(name, items)| Program { name, items })
}

pub fn unclassified_line<S: TextSource>() -> impl Parser<S, Token = S::Span> {
    many_until(
        Char::<S::Span>::any(),
        eol_or_comment(),
        0..,
    ).map_if(|(chars, _newline)| {
        let mut span = chars.first().map(|c| c.span.clone());
        for c in chars.iter().skip(1) {
            span = span.map(|s| S::joint_span(s, c.span.clone()));
        }
        span
    })
}

// TODO tests
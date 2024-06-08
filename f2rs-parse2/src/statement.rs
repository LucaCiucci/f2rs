use std::ops::Range;

use enum_as_inner::EnumAsInner;
use rules::{import_stmt, ImportStmt};

use crate::tokens::rules::LexicalToken;

use f2rs_parser_combinator::prelude::*;

pub mod rules;

#[derive(Debug, Clone, Copy)]
pub struct MultilineSpan {
    start_line: usize,
    end_line: usize,
    start_column: usize,
    end_column: usize,
}

impl MultilineSpan {
    pub fn from_line_span(line: usize, span: Range<usize>) -> Self {
        MultilineSpan {
            start_line: line,
            end_line: line,
            start_column: span.start,
            end_column: span.end,
        }
    }
}

impl SourceSpan for MultilineSpan {
    fn new_null() -> Self {
        MultilineSpan {
            start_line: 0,
            end_line: 0,
            start_column: 0,
            end_column: 0,
        }
    }

    fn is_null(&self) -> bool {
        (self.start_line == self.end_line) && (self.start_column == self.end_column)
    }

    fn merge(a: Self, b: Self) -> Self {
        if a.is_null() {
            b
        } else if b.is_null() {
            a
        } else {
            let start_line = if a.start_line < b.start_line { a.start_line } else { b.start_line };
            let end_line = if a.end_line > b.end_line { a.end_line } else { b.end_line };
            let start_column = if a.start_column < b.start_column { a.start_column } else { b.start_column };
            let end_column = if a.end_column > b.end_column { a.end_column } else { b.end_column };
            MultilineSpan {
                start_line,
                end_line,
                start_column,
                end_column,
            }
        }
    }
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum StatementValue<Span> {
    Import(ImportStmt<Span>),
}

pub fn statement<'a, S: Source<Element = LexicalToken<MultilineSpan>>>(source: S) -> PResult<StatementValue<MultilineSpan>, S> {
    alt!(
        for S =>
        import_stmt.map(StatementValue::Import),
    ).parse(source)
}

impl StatementValue<MultilineSpan> {
    pub fn parse<S: Source<Element = LexicalToken<MultilineSpan>>>(source: S) -> Option<Self> {
        statement.parse(source).map(|r| r.0)
    }
}
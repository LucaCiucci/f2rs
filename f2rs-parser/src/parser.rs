use std::{collections::HashMap};

pub mod statement;

use enum_as_inner::EnumAsInner;

use crate::tokenizer::{rules::LineComment, TokenizedFreeLine};

pub struct Stmt<Span> {
    variant: StmtVariant<Span>,
    comments: HashMap<usize, LineComment<Span>>,
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum StmtVariant<Span> {
    Unclassified(TokenizedFreeLine<Span>),
    Empty,
}

impl<Span: Clone> Stmt<Span> {
    pub fn parse(lines: &[TokenizedFreeLine<Span>]) -> Self {
        assert!(lines.len() >= 1, "Cannot parse zero lines into a stmt!");

        let mut comments = lines
            .iter()
            .enumerate()
            .filter_map(|(i, line)| {
                if let Some(comment) = &line.comment {
                    Some((i, comment.clone()))
                } else {
                    None
                }
            })
            .collect::<HashMap<usize, LineComment<Span>>>();

        todo!()
    }
}
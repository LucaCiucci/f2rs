use crate::parse::{elements::spaced, eol_or_comment};

use super::*;

#[derive(Debug, Clone)]
pub struct UseStatement {
    pub module_name: String,
    pub only: Vec<String>,
}

pub fn use_statement<S: TextSource>() -> impl Parser<S, Token = UseStatement> {
    (
        spaced(keyword("use")),
        spaced(identifier()),
        (
            spaced(','),
            spaced(keyword("only")),
            spaced(':'),
            separated(spaced(identifier()), ',', 0..), // TODO better, until eol
        ).optional(),
        eol_or_comment(),
    ).map(|(_, module, only, _)| {
        let only = only.map(|(_, _, _, ids)| ids).unwrap_or(vec![]);
        let only = only.into_iter().map(|id| id.value).collect();
        UseStatement {
            module_name: module.value,
            only,
        }
    })
}
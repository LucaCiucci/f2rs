use crate::parse::{elements::spaced, eol_or_comment};

use super::*;

#[derive(Debug, Clone)]
pub struct UseStatement<Span> {
    pub module_name: String,
    pub only: Vec<String>,
    pub comment: Option<LineComment<Span>>,
}

pub fn use_statement<S: TextSource>() -> impl Parser<S, Token = UseStatement<S::Span>> {
    (
        spaced(keyword("use")),
        spaced(identifier()),
        (
            spaced(','),
            spaced(keyword("only")),
            spaced(':'),
            separated(spaced(identifier()), ',', 0..), // TODO better, until eol
        )
            .optional(),
        eol_or_comment(),
    )
        .map(|(_, module, only, comment)| {
            let only = only.map(|(_, _, _, ids)| ids).unwrap_or(vec![]);
            let only = only.into_iter().map(|id| id.value).collect();
            UseStatement {
                module_name: module.value,
                only,
                comment,
            }
        })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_use_statement() {
        let r = use_statement().parse("use foo").0.unwrap();

        assert_eq!(r.module_name, "foo");
        assert_eq!(r.only, Vec::<String>::new());

        let r = use_statement().parse("use foo, only: bar, baz").0.unwrap();

        assert_eq!(r.module_name, "foo");
        assert_eq!(r.only, vec!["bar", "baz"]);
    }
}

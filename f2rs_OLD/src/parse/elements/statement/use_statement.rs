use crate::parse::{elements::spaced, eol_or_comment};

use super::*;

#[derive(Debug, Clone)]
pub struct UseStatement<Span> {
    pub module_name: String,
    pub only: Vec<(String, Option<String>)>,
    pub comment: Option<LineComment<Span>>,
}

pub fn only_element<S: TextSource>() -> impl Parser<S, Token = (String, Option<String>)> {
    (
        (
            spaced(identifier()),
            spaced("=>")
        ).map(|(id, _,)| id).optional(),
        spaced(identifier()),
    ).map(|(alias, id)| (id.value, alias.map(|alias| alias.value)))
}

pub fn use_statement<S: TextSource>() -> impl Parser<S, Token = UseStatement<S::Span>> {
    (
        spaced(keyword("use")),
        spaced(identifier()),
        (
            spaced(','),
            spaced(keyword("only")),
            spaced(':'),
            separated(only_element(), ',', 0..), // TODO better, until eol
        )
            .optional(),
        eol_or_comment(),
    )
        .map(|(_, module, only, comment)| {
            let only = only.map(|(_, _, _, ids)| ids).unwrap_or(vec![]);
            let only = only.into_iter().collect();
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
    fn test_only_element() {
        let r = only_element().parse("a").0.unwrap();
        assert_eq!(r, ("a".to_string(), None));

        let r = only_element().parse("a => b").0.unwrap();
        assert_eq!(r, ("b".to_string(), Some("a".to_string())));
    }

    #[test]
    fn test_use_statement() {
        let r = use_statement().parse("use foo").0.unwrap();

        assert_eq!(r.module_name, "foo");
        assert!(r.only.is_empty());

        let r = use_statement().parse("use foo, only: bar, b => baz").0.unwrap();

        assert_eq!(r.module_name, "foo");
        assert_eq!(r.only, vec![("bar".to_string(), None), ("baz".to_string(), Some("b".to_string()))]);
    }
}

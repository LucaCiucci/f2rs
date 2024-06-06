

use super::*;

#[derive(Debug, Clone, EnumAsInner)]
pub enum ImportStmtVariant<Span> {
    List(Option<Vec<Name<Span>>>),
    Only {
        only: Keyword<Span>,
        import_name_list: Vec<Name<Span>>,
    },
    None {
        none: Keyword<Span>,
    },
    All {
        all: Keyword<Span>,
    },
}

impl<Span> MapSpan<Span> for ImportStmtVariant<Span> {
    type Spanned<T> = ImportStmtVariant<T>;
    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        match self {
            ImportStmtVariant::List(import_name_list) => ImportStmtVariant::List(import_name_list.map(|list| list.map_span(f))),
            ImportStmtVariant::Only { only, import_name_list } => ImportStmtVariant::Only {
                only: only.map_span(f),
                import_name_list: import_name_list.map_span(f),
            },
            ImportStmtVariant::None { none } => ImportStmtVariant::None { none: none.map_span(f) },
            ImportStmtVariant::All { all } => ImportStmtVariant::All { all: all.map_span(f) },
        }
    }
}

#[derive(Debug, Clone)]
pub struct ImportStmt<Span> {
    pub kw: Keyword<Span>,
    pub data: ImportStmtVariant<Span>,
}

impl<Span> MapSpan<Span> for ImportStmt<Span> {
    type Spanned<T> = ImportStmt<T>;
    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        ImportStmt {
            kw: self.kw.map_span(f),
            data: self.data.map_span(f),
        }
    }
}

#[syntax_rule(
    F18V007r1 rule "import-stmt" #867 :
    "is IMPORT [[ :: ] import-name-list ]"
    "or IMPORT, ONLY : import-name-list"
    "or IMPORT, NONE"
    "or IMPORT, ALL",
)]
pub fn import_stmt<'a, S: Source<Element = LexicalToken<MultilineSpan>> + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ImportStmt<MultilineSpan>> + 'a {
    alt!(
        (
            kw!(import),
            (comma(), kw!(only), colon()).map(|(_, only, _)| only),
            list(name(), 0..),
        ).map(|(kw, only, import_name_list)| ImportStmt { kw, data: ImportStmtVariant::Only { only, import_name_list } }),
        (
            kw!(import),
            comma(),
            kw!(none),
        ).map(|(kw, _, none)| ImportStmt { kw, data: ImportStmtVariant::None { none } }),
        (
            kw!(import),
            comma(),
            kw!(all),
        ).map(|(kw, _, all)| ImportStmt { kw, data: ImportStmtVariant::All { all } }),
        (
            kw!(import),
            (
                double_colon().optional(),
                list(name(), 1..),
            ).map(|(_, import_name_list)| import_name_list).optional(),
        ).map(|(kw, import_name_list)| ImportStmt { kw, data: ImportStmtVariant::List(import_name_list) }),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keyword() {

        let tks = tokenize("end do");
        assert_eq!(kw!(end do).parses(&tks[..]), true);
        assert_eq!(kw!(end do).parse(&tks[..]).unwrap().0.matches.len(), 2);

        let tks = tokenize("enddo");
        assert_eq!(kw!(end do).parses(&tks[..]), true);
        assert_eq!(kw!(end do).parse(&tks[..]).unwrap().0.matches.len(), 1);

        let tks = tokenize("enddo");
        assert_eq!(kw!(enddo).parses(&tks[..]), true);

        let tks = tokenize("end do");
        assert_eq!(kw!(enddo).parses(&tks[..]), false);
    }

    #[test]
    fn test_import_stmt() {
        let tks = tokenize("IMPORT");
        assert!(import_stmt(&Cfg::f2018()).parse(&tks[..]).unwrap().0.data.as_list().unwrap().is_none());

        let tks = tokenize("IMPORT :: a, b, c");
        assert_eq!(import_stmt(&Cfg::f2018()).parse(&tks[..]).unwrap().0.data.as_list().unwrap().as_ref().unwrap().len(), 3);

        let tks = tokenize("IMPORT, ONLY : a, b, c");
        assert_eq!(
            import_stmt(&Cfg::f2018()).parse(&tks[..]).unwrap().0.data.as_only().unwrap().1.len(),
            3
        );

        let tks = tokenize("IMPORT, NONE");
        assert!(import_stmt(&Cfg::f2018()).parse(&tks[..]).unwrap().0.data.as_none().is_some());

        let tks = tokenize("IMPORT, ALL");
        assert!(import_stmt(&Cfg::f2018()).parse(&tks[..]).unwrap().0.data.as_all().is_some());
    }
}
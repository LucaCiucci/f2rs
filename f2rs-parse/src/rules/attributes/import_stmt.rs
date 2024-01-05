use super::*;

#[derive(Debug, Clone, EnumAsInner)]
pub enum ImportStmt<Span> {
    List {
        import_name_list: Option<Vec<Name<Span>>>,
        comment: Option<LineComment<Span>>,
    },
    Only {
        import_name_list: Vec<Name<Span>>,
        comment: Option<LineComment<Span>>,
    },
    None {
        comment: Option<LineComment<Span>>,
    },
    All {
        comment: Option<LineComment<Span>>,
    },
}

#[syntax_rule(
    F18V007r1 rule "import-stmt" #867 :
    "is IMPORT [[ :: ] import-name-list ]"
    "or IMPORT, ONLY : import-name-list"
    "or IMPORT, NONE"
    "or IMPORT, ALL",
)]
pub fn import_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ImportStmt<S::Span>> + 'a {
    alt!(
        (
            space(0),
            kw("import", cfg), space(0),
            (
                (space(0), "::", space(0)).optional(),
                list(name(cfg, false), 0..),
            ).map(|(_, import_name_list)| import_name_list).optional(),
            statement_termination(),
        ).map(|(_, _, _, import_name_list, comment)| ImportStmt::List {
            import_name_list,
            comment,
        }),
        (
            space(0),
            kw("import", cfg), space(0),
            (space(0), ',', space(0), kw("only", cfg), space(0), ':', space(0)),
            list(name(cfg, false), 0..),
            statement_termination(),
        ).map(|(_, _, _, _, import_name_list, comment)| ImportStmt::Only {
            import_name_list,
            comment,
        }),
        (
            space(0),
            kw("import", cfg),
            (space(0), ',', space(0)),
            kw("none", cfg),
            statement_termination(),
        ).map(|(_, _, _, _, comment)| ImportStmt::None {
            comment,
        }),
        (
            space(0),
            kw("import", cfg),
            (space(0), ',', space(0)),
            kw("all", cfg),
            statement_termination(),
        ).map(|(_, _, _, _, comment)| ImportStmt::All {
            comment,
        }),
    )
}
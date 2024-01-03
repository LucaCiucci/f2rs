use super::*;

#[derive(Debug, Clone)]
pub struct EnumDef<Span> {
    pub enum_def_stmt: EnumDefStmt<Span>,
    pub enumerator_def_stmt_list: Vec<MaybeStatement<EnumeratorDefStmt<Span>, Span>>,
    pub end_enum_stmt: Option<EndEnumStmt<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "enum-def" #759 :
    "is enum-def-stmt"
    "    enumerator-def-stmt"
    "    [ enumerator-def-stmt ] ..."
    "    end-enum-stmt",
)]
pub fn enum_def<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = EnumDef<S::Span>> + 'a {
    (
        enum_def_stmt(cfg),
        many_until(
            maybe_statement(enumerator_def_stmt(cfg)),
            end_enum_stmt(cfg),
            0..,
        ),
    ).map(|(enum_def_stmt, (enumerator_def_stmt_list, end_enum_stmt))| {
        EnumDef {
            enum_def_stmt,
            enumerator_def_stmt_list,
            end_enum_stmt,
        }
    })
}

#[derive(Debug, Clone)]
pub struct EnumDefStmt<Span> {
    pub comment: Option<LineComment<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "enum-def-stmt" #760 : "is ENUM, BIND(C)",
)]
pub fn enum_def_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = EnumDefStmt<S::Span>> + 'a {
    (
        space(0),
        kw("enum", cfg),
        (// NOTE: this is not optional in the standard, but we allow it to be
            space(0), Char::exact(',').optional(), space(0),
            kw("bind", cfg), space(0), '(', space(0), kw("c", cfg), space(0), ')',
        ).optional(),
        statement_termination(),
    ).map(|(_, _, _, comment)| EnumDefStmt {
        comment,
    })
}

#[derive(Debug, Clone)]
pub struct EnumeratorDefStmt<Span> {
    pub list: Vec<Enumerator<Span>>,
    pub comment: Option<LineComment<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "enumerator-def-stmt" #761 : "ENUMERATOR [ :: ] enumerator-list",
)]
pub fn enumerator_def_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = EnumeratorDefStmt<S::Span>> + 'a {
    (
        space(0),
        kw("enumerator", cfg),
        alt!(
            (space(0), "::", space(0)).map(|_| ()),
            space(1).map(|_| ()),
        ),
        list(enumerator(cfg), 0..),
        statement_termination(),
    ).map(|(_, _, _, list, comment)| EnumeratorDefStmt {
        list,
        comment,
    })
}

#[derive(Debug, Clone)]
pub struct Enumerator<Span> {
    pub name: NamedConstant<Span>,
    pub value: Option<IntConstantExpr<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "enumerator" #762 : "is named-constant [ = scalar-int-constant-expr ]",
)]
pub fn enumerator<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = Enumerator<S::Span>> + 'a {
    (
        named_constant(cfg),
        (
            space(0),
            kw("=", cfg),
            space(0),
            int_constant_expr(cfg),
        ).map(|(_, _, _, value)| value).optional(),
    ).map(|(name, value)| Enumerator {
        name,
        value,
    })
}

#[derive(Debug, Clone)]
pub struct EndEnumStmt<Span> {
    pub comment: Option<LineComment<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "end-enum-stmt" #763 : "is END ENUM",
)]
pub fn end_enum_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = EndEnumStmt<S::Span>> + 'a {
    (
        space(0),
        (kw("end", cfg), space(0), kw("enum", cfg)),
        statement_termination(),
    ).map(|(_, _, comment)| EndEnumStmt {
        comment,
    })
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_enum_def() {
        for cfg in test_configs() {
            let p = enum_def(&cfg);
            let src = include_str!("enumeration_tests/example_1.f90");
            let p = p.parse(src).unwrap().0;
            assert_eq!(p.enumerator_def_stmt_list.len(), 4);
            assert_eq!(p.enumerator_def_stmt_list[0].is_comment(), true);
            assert_eq!(p.enumerator_def_stmt_list[1].is_empty_lines(), true);
            assert_eq!(p.enumerator_def_stmt_list[2].as_statement().unwrap().list.len(), 2);
            assert_eq!(p.enumerator_def_stmt_list[3].as_statement().unwrap().list.len(), 1);
        }
    }

    #[test]
    fn test_enum_def_stmt() {
        for cfg in test_configs() {
            let p = enum_def_stmt(&cfg);
            assert_eq!(p.parses("enum"), true);
            assert_eq!(p.parses("ENUM"), true);
            assert_eq!(p.parses("ENUM bind(c)"), true);
            assert_eq!(p.parses("ENUM, bind(c)"), true);
            assert_eq!(p.parses("ENUM bind(c) ! ciao"), true);
            assert_eq!(p.parse(" ENUM bind(C) ! ciao").unwrap().0.comment.unwrap().text, " ciao");
        }
    }

    #[test]
    fn test_enumerator_def_stmt() {
        for cfg in test_configs() {
            let p = enumerator_def_stmt(&cfg);
            assert_eq!(p.parses("enumerator :: a"), true);
            assert_eq!(p.parses("enumerator a"), true);
            assert_eq!(p.parses("enumerator a, b"), true);
            assert_eq!(p.parses("enumerator a, b = 1, ! ciao"), true);
            assert_eq!(p.parses("enumerator a, b = 1, c ! ciao"), true);
            assert_eq!(p.parse(" enumerator :: a, b = 1, c ! ciao").unwrap().0.list.len(), 3);
        }
    }

    #[test]
    fn test_enumerator() {
        for cfg in test_configs() {
            let p = enumerator(&cfg);
            assert_eq!(p.parses("foo"), true);
            assert_eq!(p.parses("foo = 1"), true);
            assert_eq!(p.parse("foo = 1").unwrap().0.name.name.0.value(), "foo");
            assert_eq!(p.parse("foo = 1").unwrap().0.value.is_some(), true);
        }
    }

    #[test]
    fn test_end_enum_stmt() {
        for cfg in test_configs() {
            let p = end_enum_stmt(&cfg);
            assert_eq!(p.parses("end enum"), true);
            assert_eq!(p.parses("end ENUM"), true);
            assert_eq!(p.parses("endenum"), true);
            assert_eq!(p.parse(" end ENUM ! ciao").unwrap().0.comment.unwrap().text, " ciao");
        }
    }
}
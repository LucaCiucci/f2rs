use std::marker::PhantomData;

use super::*;

//#[derive(Debug, Clone)]
//pub struct EnumDef<Span> {
//    pub enum_def_stmt: EnumDefStmt<Span>,
//    pub enumerator_def_stmt_list: Vec<MaybeStatement<EnumeratorDefStmt<Span>, Span>>,
//    pub end_enum_stmt: Option<EndEnumStmt<Span>>,
//}
//
//#[doc = s_rule!(
//    F18V007r1 rule "enum-def" #759 :
//    "is enum-def-stmt"
//    "    enumerator-def-stmt"
//    "    [ enumerator-def-stmt ] ..."
//    "    end-enum-stmt",
//)]
//pub fn enum_def<S: Lexed>(source: S) -> PResult<EnumDef<MultilineSpan>, S> {
//    (
//        enum_def_stmt,
//        many_until(
//            maybe_statement(enumerator_def_stmt),
//            end_enum_stmt,
//            0..,
//        ),
//    ).map(|(enum_def_stmt, (enumerator_def_stmt_list, end_enum_stmt))| {
//        EnumDef {
//            enum_def_stmt,
//            enumerator_def_stmt_list,
//            end_enum_stmt,
//        }
//    })
//}

#[derive(Debug, Clone)]
pub struct EnumDefStmt<Span> {
    _phantom: PhantomData<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "enum-def-stmt" #760 : "is ENUM, BIND(C)",
)]
pub fn enum_def_stmt<S: Lexed>(source: S) -> PResult<EnumDefStmt<MultilineSpan>, S> {
    (
        kw!(enum),
        (// NOTE: this is not optional in the standard, but we allow it to be
            comma().optional(),
            kw!(bind), delim('('), kw!(C), delim(')'),
        ).optional(),
    ).map(|(_, _)| EnumDefStmt {
        _phantom: PhantomData,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct EnumeratorDefStmt<Span> {
    pub list: Vec<Enumerator<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "enumerator-def-stmt" #761 : "ENUMERATOR [ :: ] enumerator-list",
)]
pub fn enumerator_def_stmt<S: Lexed>(source: S) -> PResult<EnumeratorDefStmt<MultilineSpan>, S> {
    (
        kw!(enumerator),
        double_colon().optional(),
        list(enumerator, 0..),
    ).map(|(_, _, list)| EnumeratorDefStmt {
        list,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct Enumerator<Span> {
    pub name: NamedConstant<Span>,
    pub value: Option<IntConstantExpr<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "enumerator" #762 : "is named-constant [ = scalar-int-constant-expr ]",
)]
pub fn enumerator<S: Lexed>(source: S) -> PResult<Enumerator<MultilineSpan>, S> {
    (
        named_constant,
        (
            equals(),
            int_constant_expr,
        ).map(|(_, value)| value).optional(),
    ).map(|(name, value)| Enumerator {
        name,
        value,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct EndEnumStmt<Span> {
    _phantom: PhantomData<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "end-enum-stmt" #763 : "is END ENUM",
)]
pub fn end_enum_stmt<S: Lexed>(source: S) -> PResult<EndEnumStmt<MultilineSpan>, S> {
    (kw!(end enum)).map(|_| EndEnumStmt {
        _phantom: PhantomData,
    }).parse(source)
}

#[cfg(test)]
mod test {
    use super::*;

    // TODO #[test]
    //fn test_enum_def() {
    //    for cfg in test_configs() {
    //        let p = enum_def(&cfg);
    //        let src = include_str!("enumeration_tests/example_1.f90");
    //        let p = p.parse(src).unwrap().0;
    //        assert_eq!(p.enumerator_def_stmt_list.len(), 4);
    //        assert_eq!(p.enumerator_def_stmt_list[0].is_comment(), true);
    //        assert_eq!(p.enumerator_def_stmt_list[1].is_empty_lines(), true);
    //        assert_eq!(p.enumerator_def_stmt_list[2].as_statement().unwrap().list.len(), 2);
    //        assert_eq!(p.enumerator_def_stmt_list[3].as_statement().unwrap().list.len(), 1);
    //    }
    //}

    use crate::rule_test;

    rule_test! {
        enum_def_stmt(F18V007r1 760) {
            examples(|s| enum_def_stmt(s), [
                "ENUM, BIND(C)",
                "ENUM, BIND(C)",
            ]);
        }
    }

    rule_test! {
        enumerator_def_stmt(F18V007r1 761) {
            examples(|s| enumerator_def_stmt(s), [
                "ENUMERATOR :: a",
                "ENUMERATOR a",
                "ENUMERATOR a, b",
                "ENUMERATOR a, b = 1",
                "ENUMERATOR a, b = 1,",
                "ENUMERATOR a, b = 1, c",
            ]);

            // from the standard
            examples(|s| enumerator_def_stmt(s), [
                "ENUMERATOR :: RED = 4, BLUE = 9",
                "ENUMERATOR YELLOW",
            ]);
        }
    }

    rule_test! {
        enumerator(F18V007r1 762) {
            examples(|s| enumerator(s), [
                "foo",
                "foo = 1",
                "foo = exp(1 **2 + 2)",
            ]);

            // from the standard
            examples(|s| enumerator(s), [
                "RED = 4",
                "BLUE = 9",
                "YELLOW",
            ]);
        }
    }

    rule_test! {
        end_enum_stmt(F18V007r1 763) {
            examples(|s| end_enum_stmt(s), [
                "END ENUM",
                "ENDENUM",
            ]);
        }
    }

//
    // TODO #[test]
    //fn test_enum_def_stmt() {
    //    for cfg in test_configs() {
    //        let p = enum_def_stmt(&cfg);
    //        assert_eq!(p.parses("enum"), true);
    //        assert_eq!(p.parses("ENUM"), true);
    //        assert_eq!(p.parses("ENUM bind(c)"), true);
    //        assert_eq!(p.parses("ENUM, bind(c)"), true);
    //        assert_eq!(p.parses("ENUM bind(c) ! ciao"), true);
    //        assert_eq!(p.parse(" ENUM bind(C) ! ciao").unwrap().0.comment.unwrap().text, " ciao");
    //    }
    //}
//
    // TODO #[test]
    //fn test_enumerator_def_stmt() {
    //    for cfg in test_configs() {
    //        let p = enumerator_def_stmt(&cfg);
    //        assert_eq!(p.parses("enumerator :: a"), true);
    //        assert_eq!(p.parses("enumerator a"), true);
    //        assert_eq!(p.parses("enumerator a, b"), true);
    //        assert_eq!(p.parses("enumerator a, b = 1, ! ciao"), true);
    //        assert_eq!(p.parses("enumerator a, b = 1, c ! ciao"), true);
    //        assert_eq!(p.parse(" enumerator :: a, b = 1, c ! ciao").unwrap().0.list.len(), 3);
    //    }
    //}
//
    // TODO #[test]
    //fn test_enumerator() {
    //    for cfg in test_configs() {
    //        let p = enumerator(&cfg);
    //        assert_eq!(p.parses("foo"), true);
    //        assert_eq!(p.parses("foo = 1"), true);
    //        assert_eq!(p.parse("foo = 1").unwrap().0.name.name.0.value(), "foo");
    //        assert_eq!(p.parse("foo = 1").unwrap().0.value.is_some(), true);
    //    }
    //}
//
    // TODO #[test]
    //fn test_end_enum_stmt() {
    //    for cfg in test_configs() {
    //        let p = end_enum_stmt(&cfg);
    //        assert_eq!(p.parses("end enum"), true);
    //        assert_eq!(p.parses("end ENUM"), true);
    //        assert_eq!(p.parses("endenum"), true);
    //        assert_eq!(p.parse(" end ENUM ! ciao").unwrap().0.comment.unwrap().text, " ciao");
    //    }
    //}
}
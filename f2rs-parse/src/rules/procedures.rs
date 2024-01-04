use super::*;

#[derive(Debug, Clone)]
pub enum GenericSpec<Span> {
    GenericName(Name<Span>),
    Operator(DefinedOperator<Span>),
    Assignment,
    DefinedIoGenericSpec(DefinedIoGenericSpec),
}

#[syntax_rule(
    F18V007r1 rule "generic-spec" #1508 :
    "is generic-name"
    "or OPERATOR ( defined-operator )"
    "or ASSIGNMENT ( = )"
    "or defined-io-generic-spec",
)]
pub fn generic_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = GenericSpec<S::Span>> + 'a {
    alt!(
        name(cfg, false).map(GenericSpec::GenericName),
        (
            kw("operator", cfg), space(0), '(', space(0),
            defined_operator(cfg),
            (space(0), ')'),
        ).map(|(_, _, _, _, defined_operator, _)| GenericSpec::Operator(defined_operator)),
        (kw("assignment", cfg), space(0), '(', space(0), '=', space(0), ')').map(|_| GenericSpec::Assignment),
        defined_io_generic_spec(cfg).map(GenericSpec::DefinedIoGenericSpec),
    )
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum DefinedIoGenericSpec {
    ReadFormatted,
    ReadUnformatted,
    WriteFormatted,
    WriteUnformatted,
}

#[syntax_rule(
    F18V007r1 rule "defined-io-generic-spec" #1509 :
    "is READ (FORMATTED)"
    "or READ (UNFORMATTED)"
    "or WRITE (FORMATTED)"
    "or WRITE (UNFORMATTED)",
)]
pub fn defined_io_generic_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DefinedIoGenericSpec> + 'a {
    alt!(
        (kw("read", cfg), space(0), '(', space(0), kw("formatted", cfg), space(0), ')').map(|_| DefinedIoGenericSpec::ReadFormatted),
        (kw("read", cfg), space(0), '(', space(0), kw("unformatted", cfg), space(0), ')').map(|_| DefinedIoGenericSpec::ReadUnformatted),
        (kw("write", cfg), space(0), '(', space(0), kw("formatted", cfg), space(0), ')').map(|_| DefinedIoGenericSpec::WriteFormatted),
        (kw("write", cfg), space(0), '(', space(0), kw("unformatted", cfg), space(0), ')').map(|_| DefinedIoGenericSpec::WriteUnformatted),
    )
}

#[derive(Debug, Clone)]
pub struct FunctionReference<Span> {
    pub procedure_designator: ProcedureDesignator<Span>,
    pub actual_arg_spec_list: Option<Vec<ActualArgSpec<Span>>>,
}

#[syntax_rule(
    F18V007r1 rule "function-reference" #1520 : "is procedure-designator ( [ actual-arg-spec-list ] )",
)]
pub fn function_reference<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = FunctionReference<S::Span>> + 'a {
    (
        procedure_designator(cfg),
        (
            (space(0), '(', space(0)),
            list(actual_arg_spec(cfg), 0..),
            (space(0), ')', space(0)),
        ).map(|(_, list, _)| list).optional(),
    ).map(|(procedure_designator, actual_arg_spec_list)| FunctionReference {
        procedure_designator,
        actual_arg_spec_list,
    })
}

#[derive(Debug, Clone)]
pub struct CallStmt<Span> {
    pub procedure_designator: ProcedureDesignator<Span>,
    pub actual_arg_spec_list: Option<Vec<ActualArgSpec<Span>>>,
    pub comment: Option<LineComment<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "call-stmt" #1521 : "is CALL procedure-designator [ ( [ actual-arg-spec-list ] ) ]",
)]
pub fn call_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = CallStmt<S::Span>> + 'a {
    (
        space(0),
        kw("call", cfg),
        space(0),
        procedure_designator(cfg),
        (
            (space(0), '(', space(0)),
            list(actual_arg_spec(cfg), 0..),
            (space(0), ')', space(0)),
        ).map(|(_, list, _)| list).optional(),
    ).map(|(_, _, _, procedure_designator, actual_arg_spec_list)| CallStmt {
        procedure_designator,
        actual_arg_spec_list,
        comment: None,
    })
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ProcedureDesignator<Span> {
    Name(Name<Span>),
    ProcComponentRef(Box<ProcComponentRef<Span>>),
    DataRef(DataRef<Span>, Name<Span>),
}

#[syntax_rule(
    F18V007r1 rule "procedure-designator" #1522 :
    "is procedure-name"
    "or proc-component-ref"
    "or data-ref % binding-name",
)]
pub fn procedure_designator<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ProcedureDesignator<S::Span>> + 'a {
    alt!(
        name(cfg, false).map(ProcedureDesignator::Name),
        proc_component_ref(cfg).map(|p| ProcedureDesignator::ProcComponentRef(Box::new(p))),
        (
            data_ref(cfg),
            (space(0), '%', space(0)),
            name(cfg, false),
        ).map(|(data_ref, _, binding_name)| ProcedureDesignator::DataRef(data_ref, binding_name)),
    )
}

#[derive(Debug, Clone)]
pub struct ActualArgSpec<Span> {
    pub keyword: Option<Keyword<Span>>,
    pub actual_arg: ActualArg<Span>,
}

#[syntax_rule(
    F18V007r1 rule "actual-arg-spec" #1523 : "is [ keyword = ] actual-arg",
)]
pub fn actual_arg_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ActualArgSpec<S::Span>> + 'a {
    // TODO test
    (
        (
            keyword(cfg),
            (space(0), '=', space(0)),
        ).map(|(keyword, _)| keyword).optional(),
        actual_arg(cfg),
    ).map(|(keyword, actual_arg)| ActualArgSpec {
        keyword,
        actual_arg,
    })
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ActualArg<Span> {
    Expr(Expr<Span>),
    //Variable(Box<Variable<Span>>),
    Variable(Variable<Span>),
    ProcedureName(Name<Span>),
    ProcComponentRef(ProcComponentRef<Span>),
    AltReturnSpec(AltReturnSpec<Span>),
}

#[syntax_rule(
    F18V007r1 rule "actual-arg" #1524 :
    "is expr"
    "or variable"
    "or procedure-name"
    "or proc-component-ref"
    "or alt-return-spec",
)]
pub fn actual_arg<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ActualArg<S::Span>> + 'a {
    alt!(
        expr(cfg).map(ActualArg::Expr),
        variable(cfg, false).map(ActualArg::Variable),
        name(cfg, false).map(ActualArg::ProcedureName),
        proc_component_ref(cfg).map(ActualArg::ProcComponentRef),
        alt_return_spec(cfg).map(ActualArg::AltReturnSpec),
    )
}

#[derive(Debug, Clone)]
pub struct AltReturnSpec<Span> {
    pub label: Label<Span>,
}

#[syntax_rule(
    F18V007r1 rule "alt-return-spec" #1525 : "is * label",
)]
pub fn alt_return_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = AltReturnSpec<S::Span>> + 'a {
    (
        SpecialCharacter::Asterisk,
        space(0),
        label(cfg),
    ).map(|(_, _, label)| AltReturnSpec {
        label,
    })
}

#[derive(Debug, Clone)]
pub struct ProcLanguageBindingSpec<Span>(pub LanguageBindingSpec<Span>);

#[syntax_rule(
    F18V007r1 rule "proc-language-binding-spec" #1528 : "is language-binding-spec",
)]
pub fn proc_language_binding_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ProcLanguageBindingSpec<S::Span>> + 'a {
    language_binding_spec(cfg).map(ProcLanguageBindingSpec)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum Suffix<Span> {
    Form1(ProcLanguageBindingSpec<Span>, Option<Name<Span>>),
    Form2(Name<Span>, Option<ProcLanguageBindingSpec<Span>>),
}

#[derive(Debug, Clone)]
pub struct DummyArgName<Span>(pub Name<Span>);

#[syntax_rule(
    F18V007r1 rule "dummy-arg-name" #1531 : "is name",
)]
pub fn dummy_arg_name<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DummyArgName<S::Span>> + 'a {
    name(cfg, false).map(DummyArgName)
}

#[syntax_rule(
    F18V007r1 rule "suffix" #1532 :
    "is proc-language-binding-spec [ RESULT ( result-name ) ]"
    "or RESULT ( result-name ) [ proc-language-binding-spec ]",
)]
pub fn suffix<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = Suffix<S::Span>> + 'a {
    alt!(
        (
            proc_language_binding_spec(cfg),
            (space(0), kw("result", cfg), space(0), '(', space(0), name(cfg, false), space(0), ')'),
        ).map(|(proc_language_binding_spec, (_, _, _, _, _, result_name, _, _))| Suffix::Form1(proc_language_binding_spec, Some(result_name))),
        (
            kw("result", cfg), space(0), '(', space(0), name(cfg, false), space(0), ')',
            proc_language_binding_spec(cfg).optional(),
        ).map(|(_, _, _, _, result_name, _, _, proc_language_binding_spec)| Suffix::Form2(result_name, proc_language_binding_spec)),
    )
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum DummyArg<Span> {
    Name(DummyArgName<Span>),
    Star,
}

#[syntax_rule(
    F18V007r1 rule "dummy-arg" #1536 :
    "is dummy-arg-name"
    "or *",
)]
pub fn dummy_arg<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DummyArg<S::Span>> + 'a {
    alt!(
        dummy_arg_name(cfg).map(DummyArg::Name),
        (SpecialCharacter::Asterisk).map(|_| DummyArg::Star),
    )
}

#[derive(Debug, Clone)]
pub struct EntryStmt<Span> {
    pub entry_name: Name<Span>,
    pub dummy_arg_list: Vec<DummyArg<Span>>,
    pub suffix: Option<Suffix<Span>>,
    pub comment: Option<LineComment<Span>>,
}

/// ENTRY entry-name
#[syntax_rule(
    F18V007r1 rule "entry-stmt" #1541 :
    "is ENTRY entry-name [ ( [ dummy-arg-list ] ) [ suffix ] ]",
)]
pub fn entry_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = EntryStmt<S::Span>> + 'a {
    (
        space(0),
        kw("entry", cfg),
        space(0),
        name(cfg, false),
        (
            (space(0), '(', space(0)),
            list(dummy_arg(cfg), 0..),
            (space(0), ')', space(0)),
            suffix(cfg).optional(),
        ).map(|(_, list, _, suffix)| (list, suffix)).optional(),
        statement_termination(),
    ).map(|(_, _, _, entry_name, dummy_arg_list_suffix, comment)| {
        let (dummy_arg_list, suffix) = match dummy_arg_list_suffix {
            Some((dummy_arg_list, suffix)) => (dummy_arg_list, suffix),
            None => (vec![], None),
        };
        EntryStmt {
            entry_name,
            dummy_arg_list,
            suffix,
            comment,
        }
    })
}

#[derive(Debug, Clone)]
pub struct ReturnStmt<Span> {
    // TODO maybe a label?
    pub expr: Option<IntExpr<Span>>,
    pub comment: Option<LineComment<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "return-stmt" #1542 : "is RETURN [ scalar-int-expr ]",
)]
pub fn return_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ReturnStmt<S::Span>> + 'a {
    (
        space(0),
        kw("return", cfg),
        space(0),
        int_expr(cfg).optional(),
        statement_termination(),
    ).map(|(_, _, _, expr, comment)| ReturnStmt {
        expr,
        comment,
    })
}

#[derive(Debug, Clone)]
pub struct ContainsStmt<Span>{
    pub match_: StringMatch<Span>,
    pub comment: Option<LineComment<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "contains-stmt" #1543 : "is CONTAINS",
)]
pub fn contains_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ContainsStmt<S::Span>> + 'a {
    // TODO test
    (
        space(0),
        StringMatch::exact("contains", false),
        statement_termination(),
    ).map(|(_, match_, comment)| ContainsStmt {
        match_,
        comment,
    })
}

#[derive(Debug, Clone)]
pub struct StmtFunctionStmt<Span> {
    pub function_name: Name<Span>,
    pub dummy_arg_ame_list: Vec<DummyArgName<Span>>,
    pub expr: Expr<Span>,
    pub comment: Option<LineComment<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "stmt-function-stmt" #1544 : "is function-name ( [ dummy-arg-name-list ] ) = scalar-expr",
)]
pub fn stmt_function_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = StmtFunctionStmt<S::Span>> + 'a {
    (
        space(0),
        name(cfg, false),
        (space(0), '(', space(0)),
        list(dummy_arg_name(cfg), 0..),
        (space(0), ')', space(0)),
        (space(0), '=', space(0)),
        expr(cfg),
        statement_termination(),
    ).map(|(_, function_name, _, dummy_arg_ame_list, _, _, expr, comment)| StmtFunctionStmt {
        function_name,
        dummy_arg_ame_list,
        expr,
        comment,
    })
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_contains_stmt() {
        for cfg in test_configs() {
            let p = contains_stmt(&cfg);
            assert_eq!(p.parses("contains"), true);
            assert_eq!(p.parses("CONTAINS"), true);
            assert_eq!(p.parses("CONTAINS ciao"), false);
            assert_eq!(p.parses("CONTAINS\n"), true);
            assert_eq!(p.parses("CONTAINS ! ciao"), true);
        }
    }
}
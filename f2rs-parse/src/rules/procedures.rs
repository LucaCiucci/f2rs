use std::marker::PhantomData;

use super::*;

#[derive(Debug, Clone)]
pub enum GenericSpec<Span> {
    GenericName(GenericName<Span>),
    Operator(DefinedOperator<Span>),
    Assignment,
    DefinedIoGenericSpec(DefinedIoGenericSpec),
}

#[syntax_rule(
    F18V007r1 rule "generic-spec" #1508,
)]
pub fn generic_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = GenericSpec<S::Span>> + 'a {
    alt!(
        generic_name(cfg).map(GenericSpec::GenericName),
        (
            kw("operator", cfg), space(0), '(', space(0),
            defined_operator(cfg),
            (space(0), ')'),
        ).map(|(_, _, _, _, defined_operator, _)| GenericSpec::Operator(defined_operator)),
        (kw("assignment", cfg), space(0), '(', space(0), '=', space(0), ')').map(|_| GenericSpec::Assignment),
        defined_io_generic_spec(cfg).map(GenericSpec::DefinedIoGenericSpec),
    )
}

#[derive(Debug, Clone)]
pub struct GenericName<Span>(pub Name<Span>);

#[syntax_rule(
    F18V007r1 rule "generic-name",
)]
pub fn generic_name<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = GenericName<S::Span>> + 'a {
    name(cfg, false).map(GenericName)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum DefinedIoGenericSpec {
    ReadFormatted,
    ReadUnformatted,
    WriteFormatted,
    WriteUnformatted,
}

#[syntax_rule(
    F18V007r1 rule "defined-io-generic-spec" #1509,
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
    F18V007r1 rule "function-reference" #1520,
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

#[derive(Debug, Clone, EnumAsInner)]
pub enum ProcedureDesignator<Span> {
    Name(ProcedureName<Span>),
    ProcComponentRef(Box<ProcComponentRef<Span>>),
    DataRef(DataRef<Span>, BindingName<Span>),
}

#[syntax_rule(
    F18V007r1 rule "procedure-designator" #1522,
)]
pub fn procedure_designator<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ProcedureDesignator<S::Span>> + 'a {
    alt!(
        procedure_name(cfg).map(ProcedureDesignator::Name),
        proc_component_ref(cfg).map(|p| ProcedureDesignator::ProcComponentRef(Box::new(p))),
        (
            data_ref(cfg),
            (space(0), '%', space(0)),
            binding_name(cfg),
        ).map(|(data_ref, _, binding_name)| ProcedureDesignator::DataRef(data_ref, binding_name)),
    )
}

#[derive(Debug, Clone)]
pub struct ActualArgSpec<Span> {
    pub keyword: Option<Keyword<Span>>,
    pub actual_arg: ActualArg<Span>,
}

#[syntax_rule(
    F18V007r1 rule "actual-arg-spec" #1523,
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
    ProcedureName(ProcedureName<Span>),
    ProcComponentRef(ProcComponentRef<Span>),
    AltReturnSpec(AltReturnSpec<Span>),
}

#[syntax_rule(
    F18V007r1 rule "actual-arg" #1524,
)]
pub fn actual_arg<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ActualArg<S::Span>> + 'a {
    alt!(
        expr(cfg).map(ActualArg::Expr),
        variable(cfg, false).map(ActualArg::Variable),
        procedure_name(cfg).map(ActualArg::ProcedureName),
        proc_component_ref(cfg).map(ActualArg::ProcComponentRef),
        alt_return_spec(cfg).map(ActualArg::AltReturnSpec),
    )
}

#[derive(Debug, Clone)]
pub struct AltReturnSpec<Span> {
    pub label: Label<Span>,
}

#[syntax_rule(
    F18V007r1 rule "alt-return-spec" #1525,
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
pub struct BindingName<Span>(pub Name<Span>);

#[syntax_rule(
    F18V007r1 rule "binding-name",
)]
pub fn binding_name<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = BindingName<S::Span>> + 'a {
    name(cfg, false).map(BindingName)
}

#[derive(Debug, Clone)]
pub struct ContainsStmt<Span>{
    pub match_: StringMatch<Span>,
    pub comment: Option<LineComment<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "contains-stmt" #1543,
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
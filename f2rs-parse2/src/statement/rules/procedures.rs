use super::*;

#[derive(Debug, Clone, EnumAsInner)]
pub enum InterfaceStmt<Span> {
    Interface {
        generic_spec: Option<GenericSpec<Span>>,
    },
    AbstractInterface {
        _0: (),
    },
}

#[syntax_rule(
    F18V007r1 rule "interface-stmt" #1503 :
    "is INTERFACE [ generic-spec ]"
    "or ABSTRACT INTERFACE",
)]
pub fn interface_stmt_2<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = InterfaceStmt<MultilineSpan>> + 'a {
    alt!(
        (
            kw!(INTERFACE),
            generic_spec(cfg).optional(),
        ).map(|(_, generic_spec)| InterfaceStmt::Interface {
            generic_spec,
        }),
        (
            kw!(ABSTRACT),
            kw!(INTERFACE),
        ).map(|_| InterfaceStmt::AbstractInterface {
            _0: (),
        }),
    )
}

#[derive(Debug, Clone)]
pub struct EndInterfaceStmt<Span> {
    pub generic_spec: Option<GenericSpec<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "end-interface-stmt" #1504 : "is END INTERFACE [ generic-spec ]",
)]
pub fn end_interface_stmt_2<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = EndInterfaceStmt<MultilineSpan>> + 'a {
    (
        kw!(END),
        kw!(INTERFACE),
        generic_spec(cfg).optional(),
    ).map(|(_, _, generic_spec)| EndInterfaceStmt {
        generic_spec,
    })
}

#[derive(Debug, Clone)]
pub struct ProcedureStmt<Span> {
    pub module: bool,
    pub specific_procedure_list: Vec<SpecificProcedure<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "procedure-stmt" #1506 :
    "is [ MODULE ] PROCEDURE [ :: ] specific-procedure-list",
)]
pub fn procedure_stmt_2<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ProcedureStmt<MultilineSpan>> + 'a {
    (
        kw!(MODULE).optional(),
        kw!(PROCEDURE),
        double_colon().optional(),
        list(specific_procedure(cfg), 1..),
    ).map(|(module, _, _, specific_procedure_list)| ProcedureStmt {
        module: module.is_some(),
        specific_procedure_list,
    })
}

#[derive(Debug, Clone)]
pub struct SpecificProcedure<Span>(pub Name<Span>);

#[syntax_rule(
    F18V007r1 rule "specific-procedure" #1507 : "is procedure-name",
)]
pub fn specific_procedure<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = SpecificProcedure<MultilineSpan>> + 'a {
    name().map(SpecificProcedure)
}

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
pub fn generic_spec<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = GenericSpec<MultilineSpan>> + 'a {
    alt!(
        name().map(GenericSpec::GenericName),
        (
            kw!(OPERATOR), delim('('),
            defined_operator(),
            delim(')'),
        ).map(|(_, _, defined_operator, _)| GenericSpec::Operator(defined_operator)),
        (kw!(assignment), delim('('), equals(), delim(')')).map(|_| GenericSpec::Assignment),
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
pub fn defined_io_generic_spec<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DefinedIoGenericSpec> + 'a {
    alt!(
        (kw!(read), delim('('), kw!(formatted), delim(')')).map(|_| DefinedIoGenericSpec::ReadFormatted),
        (kw!(read), delim('('), kw!(unformatted), delim(')')).map(|_| DefinedIoGenericSpec::ReadUnformatted),
        (kw!(write), delim('('), kw!(formatted), delim(')')).map(|_| DefinedIoGenericSpec::WriteFormatted),
        (kw!(write), delim('('), kw!(unformatted), delim(')')).map(|_| DefinedIoGenericSpec::WriteUnformatted),
    )
}

#[derive(Debug, Clone)]
pub struct GenericStmt<Span> {
    pub access_spec: Option<AccessSpec>,
    pub generic_spec: GenericSpec<Span>,
    pub specific_procedure_list: Vec<Name<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "generic-stmt" #1510 :
    "is GENERIC [ , access-spec ] :: generic-spec => specific-procedure-list",
)]
pub fn generic_stmt<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = GenericStmt<MultilineSpan>> + 'a {
    (
        kw!(GENERIC),
        (
            comma(),
            access_spec(cfg),
        ).map(|(_, access_spec)| access_spec).optional(),
        double_colon(),
        generic_spec(cfg),
        arrow(),
        list(name(), 1..),
    ).map(|(_, access_spec, _, generic_spec, _, specific_procedure_list)| GenericStmt {
        access_spec,
        generic_spec,
        specific_procedure_list,
    })
}

#[derive(Debug, Clone)]
pub struct ExternalStmt<Span> {
    pub external_name_list: Vec<Name<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "external-stmt" #1511 : "is EXTERNAL [ :: ] external-name-list",
)]
pub fn external_stmt_2<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ExternalStmt<MultilineSpan>> + 'a {
    (
        kw!(external),
        double_colon().optional(),
        list(name(), 1..),
    ).map(|(_, _, external_name_list)| ExternalStmt {
        external_name_list,
    })
}

#[derive(Debug, Clone)]
pub struct ProcedureDeclarationStmt<Span> {
    pub proc_interface: Option<ProcInterface<Span>>,
    pub proc_attr_spec_list: Option<Vec<ProcAttrSpec<Span>>>,
    pub proc_decl_list: Vec<ProcDecl<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "procedure-declaration-stmt" #1512 :
    "is PROCEDURE ( [ proc-interface ] ) [ [ , proc-attr-spec ] ... :: ] proc-decl-list",
)]
pub fn procedure_declaration_stmt<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ProcedureDeclarationStmt<MultilineSpan>> + 'a {
    (
        (kw!(procedure), delim('(')),
        proc_interface(cfg).optional(),
        delim(')'),
        (
            comma(),
            many(
                (comma(), proc_attr_spec(cfg)).map(|(_, proc_attr_spec)| proc_attr_spec),
                0..,
            ),
            double_colon(),
        ).map(|(_, proc_attr_spec_list, _)| proc_attr_spec_list).optional(),
        list(proc_decl(cfg), 1..),
    ).map(|(_, proc_interface, _, proc_attr_spec_list, proc_decl_list)| ProcedureDeclarationStmt {
        proc_interface,
        proc_attr_spec_list: proc_attr_spec_list,
        proc_decl_list,
    })
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ProcInterface<Span> {
    InterfaceName(InterfaceName<Span>),
    DeclarationTypeSpec(DeclarationTypeSpec<Span>),
}

#[syntax_rule(
    F18V007r1 rule "proc-interface" #1513 :
    "is interface-name"
    "or declaration-type-spec",
)]
pub fn proc_interface<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ProcInterface<MultilineSpan>> + 'a {
    alt!(
        interface_name(cfg).map(ProcInterface::InterfaceName),
        declaration_type_spec(cfg).map(ProcInterface::DeclarationTypeSpec),
    )
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ProcAttrSpec<Span> {
    AccessSpec(AccessSpec),
    ProcLanguageBindingSpec(ProcLanguageBindingSpec<Span>),
    Intent(IntentSpec<Span>),
    Optional,
    Pointer,
    Protected,
    Save,
}

#[syntax_rule(
    F18V007r1 rule "proc-attr-spec" #1514 :
    "is access-spec"
    "or proc-language-binding-spec"
    "or INTENT ( intent-spec )"
    "or OPTIONAL"
    "or POINTER"
    "or PROTECTED"
    "or SAVE",
)]
pub fn proc_attr_spec<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ProcAttrSpec<MultilineSpan>> + 'a {
    alt!(
        access_spec(cfg).map(ProcAttrSpec::AccessSpec),
        proc_language_binding_spec(cfg).map(ProcAttrSpec::ProcLanguageBindingSpec),
        (kw!(intent), delim('('), intent_spec(cfg), delim(')')).map(|(_, _, intent_spec, _)| ProcAttrSpec::Intent(intent_spec)),
        (kw!(optional)).map(|_| ProcAttrSpec::Optional),
        (kw!(pointer)).map(|_| ProcAttrSpec::Pointer),
        (kw!(protected)).map(|_| ProcAttrSpec::Protected),
        (kw!(save)).map(|_| ProcAttrSpec::Save),
    )
}

#[derive(Debug, Clone)]
pub struct ProcDecl<Span> {
    pub procedure_entity_name: Name<Span>,
    pub proc_pointer_init: Option<ProcPointerInit<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "proc-decl" #1515 : "is procedure-entity-name [ => proc-pointer-init ]",
)]
pub fn proc_decl<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ProcDecl<MultilineSpan>> + 'a {
    (
        name(),
        (
            arrow(),
            proc_pointer_init(cfg).map(Some),
        ).map(|(_, proc_pointer_init)| proc_pointer_init).optional(),
    ).map(|(procedure_entity_name, proc_pointer_init)| ProcDecl {
        procedure_entity_name,
        proc_pointer_init: proc_pointer_init.flatten(),
    })
}

#[derive(Debug, Clone)]
pub struct InterfaceName<Span>(pub Name<Span>);

#[syntax_rule(
    F18V007r1 rule "interface-name" #1516 : "is name",
)]
pub fn interface_name<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = InterfaceName<MultilineSpan>> + 'a {
    name().map(InterfaceName)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ProcPointerInit<Span> {
    NullInit,
    InitialProcTarget(InitialProcTarget<Span>),
}

#[syntax_rule(
    F18V007r1 rule "proc-pointer-init" #1517 :
    "is null-init"
    "or initial-proc-target",
)]
pub fn proc_pointer_init<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ProcPointerInit<MultilineSpan>> + 'a {
    alt!(
        (kw!(null)).map(|_| ProcPointerInit::NullInit),
        initial_proc_target(cfg).map(|t| ProcPointerInit::InitialProcTarget(t)),
    )
}

#[derive(Debug, Clone)]
pub struct InitialProcTarget<Span>(pub Name<Span>);

#[syntax_rule(
    F18V007r1 rule "initial-proc-target" #1518 : "is procedure-name",
)]
pub fn initial_proc_target<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = InitialProcTarget<MultilineSpan>> + 'a {
    name().map(InitialProcTarget)
}

#[derive(Debug, Clone)]
pub struct IntrinsicStmt<Span> {
    pub intrinsic_procedure_name_list: Vec<Name<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "intrinsic-stmt" #1519 : "is INTRINSIC [ :: ] intrinsic-procedure-name-list",
)]
pub fn intrinsic_stmt_2<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = IntrinsicStmt<MultilineSpan>> + 'a {
    (
        kw!(intrinsic),
        double_colon().optional(),
        list(name(), 1..),
    ).map(|(_, _, intrinsic_procedure_name_list)| IntrinsicStmt {
        intrinsic_procedure_name_list,
    })
}

#[derive(Debug, Clone)]
pub struct FunctionReference<Span> {
    pub procedure_designator: ProcedureDesignator<Span>,
    pub actual_arg_spec_list: Option<Vec<ActualArgSpec<Span>>>,
}

#[syntax_rule(
    F18V007r1 rule "function-reference" #1520 : "is procedure-designator ( [ actual-arg-spec-list ] )",
)]
pub fn function_reference<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = FunctionReference<MultilineSpan>> + 'a {
    (
        procedure_designator(cfg),
        (
            (delim('(')),
            list(actual_arg_spec(cfg), 0..),
            delim(')'),
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
}

#[syntax_rule(
    F18V007r1 rule "call-stmt" #1521 : "is CALL procedure-designator [ ( [ actual-arg-spec-list ] ) ]",
)]
pub fn call_stmt<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = CallStmt<MultilineSpan>> + 'a {
    (
        kw!(call),
        procedure_designator(cfg),
        (
            delim('('),
            list(actual_arg_spec(cfg), 0..),
            delim(')'),
        ).map(|(_, list, _)| list).optional(),
    ).map(|(_, procedure_designator, actual_arg_spec_list)| CallStmt {
        procedure_designator,
        actual_arg_spec_list,
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
pub fn procedure_designator<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ProcedureDesignator<MultilineSpan>> + 'a {
    alt!(
        name().map(ProcedureDesignator::Name),
        proc_component_ref(cfg).map(|p| ProcedureDesignator::ProcComponentRef(Box::new(p))),
        (
            data_ref(cfg),
            percent(),
            name(),
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
pub fn actual_arg_spec<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ActualArgSpec<MultilineSpan>> + 'a {
    // TODO test
    (
        (
            name_as_keyword(),
            equals(),
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
pub fn actual_arg<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ActualArg<MultilineSpan>> + 'a {
    alt!(
        expr(cfg).map(ActualArg::Expr),
        variable(cfg, false).map(ActualArg::Variable),
        name().map(ActualArg::ProcedureName),
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
pub fn alt_return_spec<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = AltReturnSpec<MultilineSpan>> + 'a {
    (
        asterisk(),
        label(),
    ).map(|(_, label)| AltReturnSpec {
        label,
    })
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum PrefixSpec<Span> {
    DeclarationTypeSpec(DeclarationTypeSpec<Span>),
    Elemental,
    Impure,
    Module,
    NonRecursive,
    Pure,
    Recursive,
}

#[derive(Debug, Clone)]
pub struct Prefix<Span> {
    pub prefix_specs: Vec<PrefixSpec<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "prefix" #1526 : "is prefix-spec [ prefix-spec ] ...",
)]
pub fn prefix<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = Prefix<MultilineSpan>> + 'a {
    many(prefix_spec(cfg), 1..).map(|prefix_specs| Prefix {
        prefix_specs,
    })
}

#[syntax_rule(
    F18V007r1 rule "prefix-spec" #1527 :
    "is declaration-type-spec"
    "or ELEMENTAL"
    "or IMPURE"
    "or MODULE"
    "or NON_RECURSIVE"
    "or PURE"
    "or RECURSIVE",
)]
pub fn prefix_spec<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = PrefixSpec<MultilineSpan>> + 'a {
    alt!(
        declaration_type_spec(cfg).map(PrefixSpec::DeclarationTypeSpec),
        (kw!(elemental)).map(|_| PrefixSpec::Elemental),
        (kw!(impure)).map(|_| PrefixSpec::Impure),
        (kw!(module)).map(|_| PrefixSpec::Module),
        (kw!(non_recursive)).map(|_| PrefixSpec::NonRecursive),
        (kw!(pure)).map(|_| PrefixSpec::Pure),
        (kw!(recursive)).map(|_| PrefixSpec::Recursive),
    )
}

#[derive(Debug, Clone)]
pub struct ProcLanguageBindingSpec<Span>(pub LanguageBindingSpec<Span>);

#[syntax_rule(
    F18V007r1 rule "proc-language-binding-spec" #1528 : "is language-binding-spec",
)]
pub fn proc_language_binding_spec<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ProcLanguageBindingSpec<MultilineSpan>> + 'a {
    language_binding_spec(cfg).map(ProcLanguageBindingSpec)
}

#[derive(Debug, Clone)]
pub struct FunctionStmt<Span> {
    pub prefix: Option<Prefix<Span>>,
    pub function_name: Name<Span>,
    pub dummy_arg_name_list: Vec<DummyArgName<Span>>,
    pub suffix: Option<Suffix<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "function-stmt" #1530 :
    "is [ prefix ] FUNCTION function-name ( [ dummy-arg-name-list ] ) [ suffix ]",
)]
pub fn function_stmt_2<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = FunctionStmt<MultilineSpan>> + 'a {
    (
        prefix(cfg).optional(),
        kw!(function),
        name(),
        delim('('),
        list(dummy_arg_name(cfg), 0..),
        delim(')'),
        suffix(cfg).optional(),
    ).map(|(prefix, _, function_name, _, dummy_arg_name_list, _, suffix)| FunctionStmt {
        prefix,
        function_name,
        dummy_arg_name_list,
        suffix,
    })
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
pub fn dummy_arg_name<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DummyArgName<MultilineSpan>> + 'a {
    name().map(DummyArgName)
}

#[syntax_rule(
    F18V007r1 rule "suffix" #1532 :
    "is proc-language-binding-spec [ RESULT ( result-name ) ]"
    "or RESULT ( result-name ) [ proc-language-binding-spec ]",
)]
pub fn suffix<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = Suffix<MultilineSpan>> + 'a {
    alt!(
        (
            proc_language_binding_spec(cfg),
            (kw!(result), delim('('), name(), delim(')')),
        ).map(|(proc_language_binding_spec, (_, _, result_name, _))| Suffix::Form1(proc_language_binding_spec, Some(result_name))),
        (
            kw!(result), delim('('), name(), delim(')'),
            proc_language_binding_spec(cfg).optional(),
        ).map(|(_, _, result_name, _, proc_language_binding_spec)| Suffix::Form2(result_name, proc_language_binding_spec)),
    )
}

#[derive(Debug, Clone)]
pub struct EndFunctionStmt<Span> {
    pub function_name: Option<Name<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "end-function-stmt" #1533 : "is END [ FUNCTION [ function-name ] ]",
)]
pub fn end_function_stmt<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = EndFunctionStmt<MultilineSpan>> + 'a {
    (
        kw!(end),
        (
            kw!(function),
            name().optional(),
        ).map(|(_, name)| name).optional(),
    ).map(|(_, function_name)| EndFunctionStmt {
        function_name: function_name.flatten(),
    })
}

#[derive(Debug, Clone)]
pub struct SubroutineStmt<Span> {
    pub prefix: Option<Prefix<Span>>,
    pub subroutine_name: Name<Span>,
    pub dummy_arg_list: Vec<DummyArg<Span>>,
    pub proc_language_binding_spec: Option<ProcLanguageBindingSpec<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "subroutine-stmt" #1535 :
    "is [ prefix ] SUBROUTINE subroutine-name [ ( [ dummy-arg-list ] ) [ proc-language-binding-spec ] ]",
)]
pub fn subroutine_stmt<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = SubroutineStmt<MultilineSpan>> + 'a {
    (
        prefix(cfg).optional(),
        kw!(subroutine),
        name(),
        (
            delim('('),
            list(dummy_arg(cfg), 0..),
            delim(')'),
            proc_language_binding_spec(cfg).optional(),
        ).map(|(_, list, _, proc_language_binding_spec)| (list, proc_language_binding_spec)).optional()
    ).map(|(prefix, _, subroutine_name, dummy_arg_list_proc_language_binding_spec)| {
        let (dummy_arg_list, proc_language_binding_spec) = match dummy_arg_list_proc_language_binding_spec {
            Some((dummy_arg_list, proc_language_binding_spec)) => (dummy_arg_list, proc_language_binding_spec),
            None => (vec![], None),
        };
        SubroutineStmt {
            prefix,
            subroutine_name,
            dummy_arg_list,
            proc_language_binding_spec,
        }
    })
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
pub fn dummy_arg<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DummyArg<MultilineSpan>> + 'a {
    alt!(
        dummy_arg_name(cfg).map(DummyArg::Name),
        asterisk().map(|_| DummyArg::Star),
    )
}

#[derive(Debug, Clone)]
pub struct EndSubroutineStmt<Span> {
    pub subroutine_name: Option<Name<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "end-subroutine-stmt" #1537 : "is END [ SUBROUTINE [ subroutine-name ] ]",
)]
pub fn end_subroutine_stmt<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = EndSubroutineStmt<MultilineSpan>> + 'a {
    (
        kw!(END),
        (
            kw!(SUBROUTINE),
            name().optional(),
        ).map(|(_, name)| name).optional(),
    ).map(|(_, subroutine_name)| EndSubroutineStmt {
        subroutine_name: subroutine_name.flatten(),
    })
}

#[derive(Debug, Clone)]
pub struct MpSubprogramStmt<Span> {
    pub procedure_name: Name<Span>,
}

#[syntax_rule(
    F18V007r1 rule "mp-subprogram-stmt" #1539 : "is MODULE PROCEDURE procedure-name",
)]
pub fn mp_subprogram_stmt_2<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = MpSubprogramStmt<MultilineSpan>> + 'a {
    (
        kw!(MODULE),
        kw!(PROCEDURE),
        name(),
    ).map(|(_, _, procedure_name)| MpSubprogramStmt {
        procedure_name,
    })
}

#[derive(Debug, Clone)]
pub struct EndMpSubprogramStmt<Span> {
    pub procedure_name: Option<Name<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "end-mp-subprogram-stmt" #1540 : "is END [PROCEDURE [procedure-name]]",
)]
pub fn end_mp_subprogram_stmt<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = EndMpSubprogramStmt<MultilineSpan>> + 'a {
    (
        kw!(END),
        (
            kw!(PROCEDURE),
            name().optional(),
        ).map(|(_, name)| name).optional(),
    ).map(|(_, procedure_name)| EndMpSubprogramStmt {
        procedure_name: procedure_name.flatten(),
    })
}

#[derive(Debug, Clone)]
pub struct EntryStmt<Span> {
    pub entry_name: Name<Span>,
    pub dummy_arg_list: Vec<DummyArg<Span>>,
    pub suffix: Option<Suffix<Span>>,
}

/// ENTRY entry-name
#[syntax_rule(
    F18V007r1 rule "entry-stmt" #1541 :
    "is ENTRY entry-name [ ( [ dummy-arg-list ] ) [ suffix ] ]",
)]
pub fn entry_stmt_2<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = EntryStmt<MultilineSpan>> + 'a {
    (
        kw!(ENTRY),
        name(),
        (
            delim('('),
            list(dummy_arg(cfg), 0..),
            delim(')'),
            suffix(cfg).optional(),
        ).map(|(_, list, _, suffix)| (list, suffix)).optional(),
    ).map(|(_, entry_name, dummy_arg_list_suffix)| {
        let (dummy_arg_list, suffix) = match dummy_arg_list_suffix {
            Some((dummy_arg_list, suffix)) => (dummy_arg_list, suffix),
            None => (vec![], None),
        };
        EntryStmt {
            entry_name,
            dummy_arg_list,
            suffix,
        }
    })
}

#[derive(Debug, Clone)]
pub struct ReturnStmt<Span> {
    // TODO maybe a label?
    pub expr: Option<IntExpr<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "return-stmt" #1542 : "is RETURN [ scalar-int-expr ]",
)]
pub fn return_stmt<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ReturnStmt<MultilineSpan>> + 'a {
    (
        kw!(RETURN),
        int_expr(cfg).optional(),
    ).map(|(_, expr)| ReturnStmt {
        expr,
    })
}

#[derive(Debug, Clone)]
pub struct ContainsStmt<Span>{
    pub match_: Keyword<Span>,
}

#[syntax_rule(
    F18V007r1 rule "contains-stmt" #1543 : "is CONTAINS",
)]
pub fn contains_stmt<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ContainsStmt<MultilineSpan>> + 'a {
    // TODO test
    kw!(CONTAINS).map(|match_| ContainsStmt {
        match_,
    })
}

#[derive(Debug, Clone)]
pub struct StmtFunctionStmt<Span> {
    pub function_name: Name<Span>,
    pub dummy_arg_ame_list: Vec<DummyArgName<Span>>,
    pub expr: Expr<Span>,
}

#[syntax_rule(
    F18V007r1 rule "stmt-function-stmt" #1544 : "is function-name ( [ dummy-arg-name-list ] ) = scalar-expr",
)]
pub fn stmt_function_stmt<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = StmtFunctionStmt<MultilineSpan>> + 'a {
    (
        name(),
        delim('('),
        list(dummy_arg_name(cfg), 0..),
        delim(')'),
        equals(),
        expr(cfg),
    ).map(|(function_name, _, dummy_arg_ame_list, _, _, expr)| StmtFunctionStmt {
        function_name,
        dummy_arg_ame_list,
        expr,
    })
}

#[cfg(test)]
mod test {
    //use crate::test_configs;
//
    //use super::*;

    //#[test]
    //fn test_contains_stmt() {
    //    for cfg in test_configs() {
    //        let p = contains_stmt(&cfg);
    //        assert_eq!(p.parses("contains"), true);
    //        assert_eq!(p.parses("CONTAINS"), true);
    //        assert_eq!(p.parses("CONTAINS ciao"), false);
    //        assert_eq!(p.parses("CONTAINS\n"), true);
    //        assert_eq!(p.parses("CONTAINS ! ciao"), true);
    //    }
    //}
}
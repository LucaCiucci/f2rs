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

#[doc = s_rule!(
    F18V007r1 rule "interface-stmt" #1503 :
    "is INTERFACE [ generic-spec ]"
    "or ABSTRACT INTERFACE",
)]
pub fn interface_stmt_2<S: Lexed>(source: S) -> PResult<InterfaceStmt<MultilineSpan>, S> {
    alt!(
        for S =>
        (
            kw!(INTERFACE),
            generic_spec.optional(),
        ).map(|(_, generic_spec)| InterfaceStmt::Interface {
            generic_spec,
        }),
        (
            kw!(ABSTRACT),
            kw!(INTERFACE),
        ).map(|_| InterfaceStmt::AbstractInterface {
            _0: (),
        }),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct EndInterfaceStmt<Span> {
    pub generic_spec: Option<GenericSpec<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "end-interface-stmt" #1504 : "is END INTERFACE [ generic-spec ]",
)]
pub fn end_interface_stmt_2<S: Lexed>(source: S) -> PResult<EndInterfaceStmt<MultilineSpan>, S> {
    (
        kw!(END),
        kw!(INTERFACE),
        generic_spec.optional(),
    ).map(|(_, _, generic_spec)| EndInterfaceStmt {
        generic_spec,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct ProcedureStmt<Span> {
    pub module: bool,
    pub specific_procedure_list: Vec<SpecificProcedure<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "procedure-stmt" #1506 :
    "is [ MODULE ] PROCEDURE [ :: ] specific-procedure-list",
)]
pub fn procedure_stmt_2<S: Lexed>(source: S) -> PResult<ProcedureStmt<MultilineSpan>, S> {
    (
        kw!(MODULE).optional(),
        kw!(PROCEDURE),
        double_colon().optional(),
        list(specific_procedure, 1..),
    ).map(|(module, _, _, specific_procedure_list)| ProcedureStmt {
        module: module.is_some(),
        specific_procedure_list,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct SpecificProcedure<Span>(pub Name<Span>);

#[doc = s_rule!(
    F18V007r1 rule "specific-procedure" #1507 : "is procedure-name",
)]
pub fn specific_procedure<S: Lexed>(source: S) -> PResult<SpecificProcedure<MultilineSpan>, S> {
    name().map(SpecificProcedure).parse(source)
}

#[derive(Debug, Clone)]
pub enum GenericSpec<Span> {
    GenericName(Name<Span>),
    Operator(DefinedOperator<Span>),
    Assignment,
    DefinedIoGenericSpec(DefinedIoGenericSpec),
}

#[doc = s_rule!(
    F18V007r1 rule "generic-spec" #1508 :
    "is generic-name"
    "or OPERATOR ( defined-operator )"
    "or ASSIGNMENT ( = )"
    "or defined-io-generic-spec",
)]
pub fn generic_spec<S: Lexed>(source: S) -> PResult<GenericSpec<MultilineSpan>, S> {
    alt!(
        for S =>
        (
            kw!(OPERATOR), delim('('),
            defined_operator(),
            delim(')'),
        ).map(|(_, _, defined_operator, _)| GenericSpec::Operator(defined_operator)),
        (kw!(assignment), delim('('), equals(), delim(')')).map(|_| GenericSpec::Assignment),
        defined_io_generic_spec.map(GenericSpec::DefinedIoGenericSpec),
        name().map(GenericSpec::GenericName),
    ).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum DefinedIoGenericSpec {
    ReadFormatted,
    ReadUnformatted,
    WriteFormatted,
    WriteUnformatted,
}

#[doc = s_rule!(
    F18V007r1 rule "defined-io-generic-spec" #1509 :
    "is READ (FORMATTED)"
    "or READ (UNFORMATTED)"
    "or WRITE (FORMATTED)"
    "or WRITE (UNFORMATTED)",
)]
pub fn defined_io_generic_spec<S: Lexed>(source: S) -> PResult<DefinedIoGenericSpec, S> {
    alt!(
        for S =>
        (kw!(read), delim('('), kw!(formatted), delim(')')).map(|_| DefinedIoGenericSpec::ReadFormatted),
        (kw!(read), delim('('), kw!(unformatted), delim(')')).map(|_| DefinedIoGenericSpec::ReadUnformatted),
        (kw!(write), delim('('), kw!(formatted), delim(')')).map(|_| DefinedIoGenericSpec::WriteFormatted),
        (kw!(write), delim('('), kw!(unformatted), delim(')')).map(|_| DefinedIoGenericSpec::WriteUnformatted),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct GenericStmt<Span> {
    pub access_spec: Option<AccessSpec>,
    pub generic_spec: GenericSpec<Span>,
    pub specific_procedure_list: Vec<Name<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "generic-stmt" #1510 :
    "is GENERIC [ , access-spec ] :: generic-spec => specific-procedure-list",
)]
pub fn generic_stmt<S: Lexed>(source: S) -> PResult<GenericStmt<MultilineSpan>, S> {
    (
        kw!(GENERIC),
        (
            comma(),
            access_spec,
        ).map(|(_, access_spec)| access_spec).optional(),
        double_colon(),
        generic_spec,
        arrow(),
        list(name(), 1..),
    ).map(|(_, access_spec, _, generic_spec, _, specific_procedure_list)| GenericStmt {
        access_spec,
        generic_spec,
        specific_procedure_list,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct ExternalStmt<Span> {
    pub external_name_list: Vec<Name<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "external-stmt" #1511 : "is EXTERNAL [ :: ] external-name-list",
)]
pub fn external_stmt_2<S: Lexed>(source: S) -> PResult<ExternalStmt<MultilineSpan>, S> {
    (
        kw!(external),
        double_colon().optional(),
        list(name(), 1..),
    ).map(|(_, _, external_name_list)| ExternalStmt {
        external_name_list,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct ProcedureDeclarationStmt<Span> {
    pub proc_interface: Option<ProcInterface<Span>>,
    pub proc_attr_spec_list: Option<Vec<ProcAttrSpec<Span>>>,
    pub proc_decl_list: Vec<ProcDecl<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "procedure-declaration-stmt" #1512 :
    "is PROCEDURE ( [ proc-interface ] ) [ [ , proc-attr-spec ] ... :: ] proc-decl-list",
)]
pub fn procedure_declaration_stmt<S: Lexed>(source: S) -> PResult<ProcedureDeclarationStmt<MultilineSpan>, S> {
    (
        (kw!(procedure), delim('(')),
        proc_interface.optional(),
        delim(')'),
        (
            comma(),
            many(
                (comma(), proc_attr_spec).map(|(_, proc_attr_spec)| proc_attr_spec),
                0..,
            ),
            double_colon(),
        ).map(|(_, proc_attr_spec_list, _)| proc_attr_spec_list).optional(),
        list(proc_decl, 1..),
    ).map(|(_, proc_interface, _, proc_attr_spec_list, proc_decl_list)| ProcedureDeclarationStmt {
        proc_interface,
        proc_attr_spec_list: proc_attr_spec_list,
        proc_decl_list,
    }).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ProcInterface<Span> {
    InterfaceName(InterfaceName<Span>),
    DeclarationTypeSpec(DeclarationTypeSpec<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "proc-interface" #1513 :
    "is interface-name"
    "or declaration-type-spec",
)]
pub fn proc_interface<S: Lexed>(source: S) -> PResult<ProcInterface<MultilineSpan>, S> {
    alt!(
        for S =>
        interface_name.map(ProcInterface::InterfaceName),
        declaration_type_spec.map(ProcInterface::DeclarationTypeSpec),
    ).parse(source)
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

#[doc = s_rule!(
    F18V007r1 rule "proc-attr-spec" #1514 :
    "is access-spec"
    "or proc-language-binding-spec"
    "or INTENT ( intent-spec )"
    "or OPTIONAL"
    "or POINTER"
    "or PROTECTED"
    "or SAVE",
)]
pub fn proc_attr_spec<S: Lexed>(source: S) -> PResult<ProcAttrSpec<MultilineSpan>, S> {
    alt!(
        for S =>
        access_spec.map(ProcAttrSpec::AccessSpec),
        proc_language_binding_spec.map(ProcAttrSpec::ProcLanguageBindingSpec),
        (kw!(intent), delim('('), intent_spec, delim(')')).map(|(_, _, intent_spec, _)| ProcAttrSpec::Intent(intent_spec)),
        (kw!(optional)).map(|_| ProcAttrSpec::Optional),
        (kw!(pointer)).map(|_| ProcAttrSpec::Pointer),
        (kw!(protected)).map(|_| ProcAttrSpec::Protected),
        (kw!(save)).map(|_| ProcAttrSpec::Save),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct ProcDecl<Span> {
    pub procedure_entity_name: Name<Span>,
    pub proc_pointer_init: Option<ProcPointerInit<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "proc-decl" #1515 : "is procedure-entity-name [ => proc-pointer-init ]",
)]
pub fn proc_decl<S: Lexed>(source: S) -> PResult<ProcDecl<MultilineSpan>, S> {
    (
        name(),
        (
            arrow(),
            proc_pointer_init.map(Some),
        ).map(|(_, proc_pointer_init)| proc_pointer_init).optional(),
    ).map(|(procedure_entity_name, proc_pointer_init)| ProcDecl {
        procedure_entity_name,
        proc_pointer_init: proc_pointer_init.flatten(),
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct InterfaceName<Span>(pub Name<Span>);

#[doc = s_rule!(
    F18V007r1 rule "interface-name" #1516 : "is name",
)]
pub fn interface_name<S: Lexed>(source: S) -> PResult<InterfaceName<MultilineSpan>, S> {
    name().map(InterfaceName).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ProcPointerInit<Span> {
    NullInit,
    InitialProcTarget(InitialProcTarget<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "proc-pointer-init" #1517 :
    "is null-init"
    "or initial-proc-target",
)]
pub fn proc_pointer_init<S: Lexed>(source: S) -> PResult<ProcPointerInit<MultilineSpan>, S> {
    alt!(
        for S =>
        (kw!(null)).map(|_| ProcPointerInit::NullInit),
        initial_proc_target.map(|t| ProcPointerInit::InitialProcTarget(t)),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct InitialProcTarget<Span>(pub Name<Span>);

#[doc = s_rule!(
    F18V007r1 rule "initial-proc-target" #1518 : "is procedure-name",
)]
pub fn initial_proc_target<S: Lexed>(source: S) -> PResult<InitialProcTarget<MultilineSpan>, S> {
    name().map(InitialProcTarget).parse(source)
}

#[derive(Debug, Clone)]
pub struct IntrinsicStmt<Span> {
    pub intrinsic_procedure_name_list: Vec<Name<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "intrinsic-stmt" #1519 : "is INTRINSIC [ :: ] intrinsic-procedure-name-list",
)]
pub fn intrinsic_stmt_2<S: Lexed>(source: S) -> PResult<IntrinsicStmt<MultilineSpan>, S> {
    (
        kw!(intrinsic),
        double_colon().optional(),
        list(name(), 1..),
    ).map(|(_, _, intrinsic_procedure_name_list)| IntrinsicStmt {
        intrinsic_procedure_name_list,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct FunctionReference<Span> {
    pub procedure_designator: ProcedureDesignator<Span>,
    pub actual_arg_spec_list: Option<Vec<ActualArgSpec<Span>>>,
}

#[doc = s_rule!(
    F18V007r1 rule "function-reference" #1520 : "is procedure-designator ( [ actual-arg-spec-list ] )",
)]
pub fn function_reference<S: Lexed>(source: S) -> PResult<FunctionReference<MultilineSpan>, S> {
    (
        procedure_designator,
        (
            (delim('(')),
            list(actual_arg_spec, 0..),
            delim(')'),
        ).map(|(_, list, _)| list).optional(),
    ).map(|(procedure_designator, actual_arg_spec_list)| FunctionReference {
        procedure_designator,
        actual_arg_spec_list,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct CallStmt<Span> {
    pub procedure_designator: ProcedureDesignator<Span>,
    pub actual_arg_spec_list: Option<Vec<ActualArgSpec<Span>>>,
}

#[doc = s_rule!(
    F18V007r1 rule "call-stmt" #1521 : "is CALL procedure-designator [ ( [ actual-arg-spec-list ] ) ]",
)]
pub fn call_stmt<S: Lexed>(source: S) -> PResult<CallStmt<MultilineSpan>, S> {
    (
        kw!(call),
        procedure_designator,
        (
            delim('('),
            list(actual_arg_spec, 0..),
            delim(')'),
        ).map(|(_, list, _)| list).optional(),
    ).map(|(_, procedure_designator, actual_arg_spec_list)| CallStmt {
        procedure_designator,
        actual_arg_spec_list,
    }).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ProcedureDesignator<Span> {
    Name(Name<Span>),
    ProcComponentRef(Box<ProcComponentRef<Span>>),
    DataRef(DataRef<Span>, Name<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "procedure-designator" #1522 :
    "is procedure-name"
    "or proc-component-ref"
    "or data-ref % binding-name",
)]
pub fn procedure_designator<S: Lexed>(source: S) -> PResult<ProcedureDesignator<MultilineSpan>, S> {
    alt!(
        for S =>
        name().map(ProcedureDesignator::Name),
        proc_component_ref.map(|p| ProcedureDesignator::ProcComponentRef(Box::new(p))),
        (
            data_ref,
            percent(),
            name(),
        ).map(|(data_ref, _, binding_name)| ProcedureDesignator::DataRef(data_ref, binding_name)),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct ActualArgSpec<Span> {
    pub keyword: Option<Keyword<Span>>,
    pub actual_arg: ActualArg<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "actual-arg-spec" #1523 : "is [ keyword = ] actual-arg",
)]
pub fn actual_arg_spec<S: Lexed>(source: S) -> PResult<ActualArgSpec<MultilineSpan>, S> {
    // TODO test
    (
        (
            name_as_keyword(),
            equals(),
        ).map(|(keyword, _)| keyword).optional(),
        actual_arg,
    ).map(|(keyword, actual_arg)| ActualArgSpec {
        keyword,
        actual_arg,
    }).parse(source)
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

#[doc = s_rule!(
    F18V007r1 rule "actual-arg" #1524 :
    "is expr"
    "or variable"
    "or procedure-name"
    "or proc-component-ref"
    "or alt-return-spec",
)]
pub fn actual_arg<S: Lexed>(source: S) -> PResult<ActualArg<MultilineSpan>, S> {
    alt!(
        for S =>
        expr.map(ActualArg::Expr),
        variable(false).map(ActualArg::Variable),
        name().map(ActualArg::ProcedureName),
        proc_component_ref.map(ActualArg::ProcComponentRef),
        alt_return_spec.map(ActualArg::AltReturnSpec),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct AltReturnSpec<Span> {
    pub label: Label<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "alt-return-spec" #1525 : "is * label",
)]
pub fn alt_return_spec<S: Lexed>(source: S) -> PResult<AltReturnSpec<MultilineSpan>, S> {
    (
        asterisk(),
        label(),
    ).map(|(_, label)| AltReturnSpec {
        label,
    }).parse(source)
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

#[doc = s_rule!(
    F18V007r1 rule "prefix" #1526 : "is prefix-spec [ prefix-spec ] ...",
)]
pub fn prefix<S: Lexed>(source: S) -> PResult<Prefix<MultilineSpan>, S> {
    many(prefix_spec, 1..).map(|prefix_specs| Prefix {
        prefix_specs,
    }).parse(source)
}

#[doc = s_rule!(
    F18V007r1 rule "prefix-spec" #1527 :
    "is declaration-type-spec"
    "or ELEMENTAL"
    "or IMPURE"
    "or MODULE"
    "or NON_RECURSIVE"
    "or PURE"
    "or RECURSIVE",
)]
pub fn prefix_spec<S: Lexed>(source: S) -> PResult<PrefixSpec<MultilineSpan>, S> {
    alt!(
        for S =>
        declaration_type_spec.map(PrefixSpec::DeclarationTypeSpec),
        (kw!(elemental)).map(|_| PrefixSpec::Elemental),
        (kw!(impure)).map(|_| PrefixSpec::Impure),
        (kw!(module)).map(|_| PrefixSpec::Module),
        (kw!(non_recursive)).map(|_| PrefixSpec::NonRecursive),
        (kw!(pure)).map(|_| PrefixSpec::Pure),
        (kw!(recursive)).map(|_| PrefixSpec::Recursive),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct ProcLanguageBindingSpec<Span>(pub LanguageBindingSpec<Span>);

#[doc = s_rule!(
    F18V007r1 rule "proc-language-binding-spec" #1528 : "is language-binding-spec",
)]
pub fn proc_language_binding_spec<S: Lexed>(source: S) -> PResult<ProcLanguageBindingSpec<MultilineSpan>, S> {
    language_binding_spec.map(ProcLanguageBindingSpec).parse(source)
}

#[derive(Debug, Clone)]
pub struct FunctionStmt<Span> {
    pub prefix: Option<Prefix<Span>>,
    pub function_name: Name<Span>,
    pub dummy_arg_name_list: Vec<DummyArgName<Span>>,
    pub suffix: Option<Suffix<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "function-stmt" #1530 :
    "is [ prefix ] FUNCTION function-name ( [ dummy-arg-name-list ] ) [ suffix ]",
)]
pub fn function_stmt_2<S: Lexed>(source: S) -> PResult<FunctionStmt<MultilineSpan>, S> {
    (
        prefix.optional(),
        kw!(function),
        name(),
        delim('('),
        list(dummy_arg_name, 0..),
        delim(')'),
        suffix.optional(),
    ).map(|(prefix, _, function_name, _, dummy_arg_name_list, _, suffix)| FunctionStmt {
        prefix,
        function_name,
        dummy_arg_name_list,
        suffix,
    }).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum Suffix<Span> {
    Form1(ProcLanguageBindingSpec<Span>, Option<Name<Span>>),
    Form2(Name<Span>, Option<ProcLanguageBindingSpec<Span>>),
}

#[derive(Debug, Clone)]
pub struct DummyArgName<Span>(pub Name<Span>);

#[doc = s_rule!(
    F18V007r1 rule "dummy-arg-name" #1531 : "is name",
)]
pub fn dummy_arg_name<S: Lexed>(source: S) -> PResult<DummyArgName<MultilineSpan>, S> {
    name().map(DummyArgName).parse(source)
}

#[doc = s_rule!(
    F18V007r1 rule "suffix" #1532 :
    "is proc-language-binding-spec [ RESULT ( result-name ) ]"
    "or RESULT ( result-name ) [ proc-language-binding-spec ]",
)]
pub fn suffix<S: Lexed>(source: S) -> PResult<Suffix<MultilineSpan>, S> {
    alt!(
        for S =>
        (
            proc_language_binding_spec,
            (kw!(result), delim('('), name(), delim(')')),
        ).map(|(proc_language_binding_spec, (_, _, result_name, _))| Suffix::Form1(proc_language_binding_spec, Some(result_name))),
        (
            kw!(result), delim('('), name(), delim(')'),
            proc_language_binding_spec.optional(),
        ).map(|(_, _, result_name, _, proc_language_binding_spec)| Suffix::Form2(result_name, proc_language_binding_spec)),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct EndFunctionStmt<Span> {
    pub function_name: Option<Name<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "end-function-stmt" #1533 : "is END [ FUNCTION [ function-name ] ]",
)]
pub fn end_function_stmt<S: Lexed>(source: S) -> PResult<EndFunctionStmt<MultilineSpan>, S> {
    (
        kw!(end),
        (
            kw!(function),
            name().optional(),
        ).map(|(_, name)| name).optional(),
    ).map(|(_, function_name)| EndFunctionStmt {
        function_name: function_name.flatten(),
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct SubroutineStmt<Span> {
    pub prefix: Option<Prefix<Span>>,
    pub subroutine_name: Name<Span>,
    pub dummy_arg_list: Vec<DummyArg<Span>>,
    pub proc_language_binding_spec: Option<ProcLanguageBindingSpec<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "subroutine-stmt" #1535 :
    "is [ prefix ] SUBROUTINE subroutine-name [ ( [ dummy-arg-list ] ) [ proc-language-binding-spec ] ]",
)]
pub fn subroutine_stmt<S: Lexed>(source: S) -> PResult<SubroutineStmt<MultilineSpan>, S> {
    (
        prefix.optional(),
        kw!(subroutine),
        name(),
        (
            delim('('),
            list(dummy_arg, 0..),
            delim(')'),
            proc_language_binding_spec.optional(),
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
    }).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum DummyArg<Span> {
    Name(DummyArgName<Span>),
    Star,
}

#[doc = s_rule!(
    F18V007r1 rule "dummy-arg" #1536 :
    "is dummy-arg-name"
    "or *",
)]
pub fn dummy_arg<S: Lexed>(source: S) -> PResult<DummyArg<MultilineSpan>, S> {
    alt!(
        for S =>
        dummy_arg_name.map(DummyArg::Name),
        asterisk().map(|_| DummyArg::Star),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct EndSubroutineStmt<Span> {
    pub subroutine_name: Option<Name<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "end-subroutine-stmt" #1537 : "is END [ SUBROUTINE [ subroutine-name ] ]",
)]
pub fn end_subroutine_stmt<S: Lexed>(source: S) -> PResult<EndSubroutineStmt<MultilineSpan>, S> {
    (
        kw!(END),
        (
            kw!(SUBROUTINE),
            name().optional(),
        ).map(|(_, name)| name).optional(),
    ).map(|(_, subroutine_name)| EndSubroutineStmt {
        subroutine_name: subroutine_name.flatten(),
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct MpSubprogramStmt<Span> {
    pub procedure_name: Name<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "mp-subprogram-stmt" #1539 : "is MODULE PROCEDURE procedure-name",
)]
pub fn mp_subprogram_stmt_2<S: Lexed>(source: S) -> PResult<MpSubprogramStmt<MultilineSpan>, S> {
    (
        kw!(MODULE),
        kw!(PROCEDURE),
        name(),
    ).map(|(_, _, procedure_name)| MpSubprogramStmt {
        procedure_name,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct EndMpSubprogramStmt<Span> {
    pub procedure_name: Option<Name<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "end-mp-subprogram-stmt" #1540 : "is END [PROCEDURE [procedure-name]]",
)]
pub fn end_mp_subprogram_stmt<S: Lexed>(source: S) -> PResult<EndMpSubprogramStmt<MultilineSpan>, S> {
    (
        kw!(END),
        (
            kw!(PROCEDURE),
            name().optional(),
        ).map(|(_, name)| name).optional(),
    ).map(|(_, procedure_name)| EndMpSubprogramStmt {
        procedure_name: procedure_name.flatten(),
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct EntryStmt<Span> {
    pub entry_name: Name<Span>,
    pub dummy_arg_list: Vec<DummyArg<Span>>,
    pub suffix: Option<Suffix<Span>>,
}

/// ENTRY entry-name
#[doc = s_rule!(
    F18V007r1 rule "entry-stmt" #1541 :
    "is ENTRY entry-name [ ( [ dummy-arg-list ] ) [ suffix ] ]",
)]
pub fn entry_stmt_2<S: Lexed>(source: S) -> PResult<EntryStmt<MultilineSpan>, S> {
    (
        kw!(ENTRY),
        name(),
        (
            delim('('),
            list(dummy_arg, 0..),
            delim(')'),
            suffix.optional(),
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
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct ReturnStmt<Span> {
    // TODO maybe a label?
    pub expr: Option<IntExpr<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "return-stmt" #1542 : "is RETURN [ scalar-int-expr ]",
)]
pub fn return_stmt<S: Lexed>(source: S) -> PResult<ReturnStmt<MultilineSpan>, S> {
    (
        kw!(RETURN),
        int_expr.optional(),
    ).map(|(_, expr)| ReturnStmt {
        expr,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct ContainsStmt<Span>{
    pub match_: Keyword<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "contains-stmt" #1543 : "is CONTAINS",
)]
pub fn contains_stmt<S: Lexed>(source: S) -> PResult<ContainsStmt<MultilineSpan>, S> {
    // TODO test
    kw!(CONTAINS).map(|match_| ContainsStmt {
        match_,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct StmtFunctionStmt<Span> {
    pub function_name: Name<Span>,
    pub dummy_arg_ame_list: Vec<DummyArgName<Span>>,
    pub expr: Expr<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "stmt-function-stmt" #1544 : "is function-name ( [ dummy-arg-name-list ] ) = scalar-expr",
)]
pub fn stmt_function_stmt<S: Lexed>(source: S) -> PResult<StmtFunctionStmt<MultilineSpan>, S> {
    (
        name(),
        delim('('),
        list(dummy_arg_name, 0..),
        delim(')'),
        equals(),
        expr,
    ).map(|(function_name, _, dummy_arg_ame_list, _, _, expr)| StmtFunctionStmt {
        function_name,
        dummy_arg_ame_list,
        expr,
    }).parse(source)
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
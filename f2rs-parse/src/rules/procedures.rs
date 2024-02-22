use super::*;

#[derive(Debug, Clone)]
pub struct InterfaceBlock<Span> {
    pub interface_stmt: InterfaceStmt<Span>,
    pub interface_specification: Vec<InterfaceSpecification<Span>>,
    pub end_interface_stmt: Option<EndInterfaceStmt<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "interface-block" #1501 :
    "is interface-stmt"
    "    [ interface-specification ] ..."
    "    end-interface-stmt",
)]
pub fn interface_block<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = InterfaceBlock<S::Span>> + 'a {
    |source: S| {
        let (interface_stmt, source) = interface_stmt(cfg).parse(source)?;

        let ((interface_specification, end), source) = many_until(interface_specification(cfg), end_interface_stmt(cfg), 0..).parse(source)?;
        if let Some(end) = end {
            return Some((InterfaceBlock {
                interface_stmt,
                interface_specification,
                end_interface_stmt: Some(end),
            }, source));
        }

        let (end_interface_stmt, source) = end_interface_stmt(cfg).optional().parse(source)?;

        Some((InterfaceBlock {
            interface_stmt,
            interface_specification,
            end_interface_stmt,
        }, source))
    }
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum InterfaceSpecification<Span> {
    InterfaceBody(InterfaceBody<Span>),
    ProcedureStmt(ProcedureStmt<Span>),
}

#[syntax_rule(
    F18V007r1 rule "interface-specification" #1502 :
    "is interface-body"
    "or procedure-stmt",
)]
pub fn interface_specification<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = InterfaceSpecification<S::Span>> + 'a {
    alt!(
        interface_body(cfg).map(InterfaceSpecification::InterfaceBody),
        procedure_stmt(cfg).map(InterfaceSpecification::ProcedureStmt),
    )
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum InterfaceStmt<Span> {
    Interface {
        generic_spec: Option<GenericSpec<Span>>,
        comment: Option<LineComment<Span>>,
    },
    AbstractInterface {
        comment: Option<LineComment<Span>>,
    },
}

#[syntax_rule(
    F18V007r1 rule "interface-stmt" #1503 :
    "is INTERFACE [ generic-spec ]"
    "or ABSTRACT INTERFACE",
)]
pub fn interface_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = InterfaceStmt<S::Span>> + 'a {
    alt!(
        (
            space(0),
            kw("interface", cfg),
            (space(0), generic_spec(cfg)).map(|(_, generic_spec)| generic_spec).optional(),
            statement_termination(),
        ).map(|(_, _, generic_spec, comment)| InterfaceStmt::Interface {
            generic_spec,
            comment,
        }),
        (
            space(0),
            kw("abstract", cfg),
            space(0),
            kw("interface", cfg),
            statement_termination(),
        ).map(|(_, _, _, _, comment)| InterfaceStmt::AbstractInterface {
            comment,
        }),
    )
}

#[derive(Debug, Clone)]
pub struct EndInterfaceStmt<Span> {
    pub generic_spec: Option<GenericSpec<Span>>,
    pub comment: Option<LineComment<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "end-interface-stmt" #1504 : "is END INTERFACE [ generic-spec ]",
)]
pub fn end_interface_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = EndInterfaceStmt<S::Span>> + 'a {
    (
        space(0),
        kw("end", cfg),
        space(0),
        kw("interface", cfg),
        (space(0), generic_spec(cfg)).map(|(_, generic_spec)| generic_spec).optional(),
        statement_termination(),
    ).map(|(_, _, _, _, generic_spec, comment)| EndInterfaceStmt {
        generic_spec,
        comment,
    })
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum InterfaceBody<Span> {
    Function {
        function_stmt: FunctionStmt<Span>,
        specification_part: Option<SpecificationPart<Span>>,
        end_function_stmt: Option<EndFunctionStmt<Span>>,
    },
    Subroutine {
        subroutine_stmt: SubroutineStmt<Span>,
        specification_part: Option<SpecificationPart<Span>>,
        end_subroutine_stmt: Option<EndSubroutineStmt<Span>>,
    },
}

#[syntax_rule(
    F18V007r1 rule "interface-body" #1505 :
    "is function-stmt"
    "   [ specification-part ]"
    "   end-function-stmt"
    "or subroutine-stmt"
    "   [ specification-part ]"
    "   end-subroutine-stmt",
)]
pub fn interface_body<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = InterfaceBody<S::Span>> + 'a {
    let function = |source: S| {
        let (function_stmt, source) = function_stmt(cfg).parse(source)?;
        let mut specification = None;
        let mut end_function = None;

        let (r, source) = specification_part(cfg, end_function_stmt(cfg)).optional().parse(source)?;
        if let Some((specification_part, end)) = r {
            specification = Some(specification_part);
            if let Some(end) = end {
                end_function = Some(end);
                return Some((InterfaceBody::Function {
                    function_stmt,
                    specification_part: specification,
                    end_function_stmt: end_function,
                }, source));
            }
        }

        let (end_function_stmt, source) = end_function_stmt(cfg).optional().parse(source)?;

        Some((InterfaceBody::Function {
            function_stmt,
            specification_part: specification,
            end_function_stmt,
        }, source))
    };

    let subroutine = |source: S| {
        let (subroutine_stmt, source) = subroutine_stmt(cfg).parse(source)?;
        let mut specification = None;
        let mut end_subroutine = None;

        let (r, source) = specification_part(cfg, end_subroutine_stmt(cfg)).optional().parse(source)?;
        if let Some((specification_part, end)) = r {
            specification = Some(specification_part);
            if let Some(end) = end {
                end_subroutine = Some(end);
                return Some((InterfaceBody::Subroutine {
                    subroutine_stmt,
                    specification_part: specification,
                    end_subroutine_stmt: end_subroutine,
                }, source));
            }
        }

        let (end_subroutine_stmt, source) = end_subroutine_stmt(cfg).optional().parse(source)?;

        Some((InterfaceBody::Subroutine {
            subroutine_stmt,
            specification_part: specification,
            end_subroutine_stmt,
        }, source))
    };

    alt!(
        function,
        subroutine,
    )
}

#[derive(Debug, Clone)]
pub struct ProcedureStmt<Span> {
    pub module: bool,
    pub specific_procedure_list: Vec<SpecificProcedure<Span>>,
    pub comment: Option<LineComment<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "procedure-stmt" #1506 :
    "is [ MODULE ] PROCEDURE [ :: ] specific-procedure-list",
)]
pub fn procedure_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ProcedureStmt<S::Span>> + 'a {
    (
        space(0),
        (kw("module", cfg), space(0)).optional(),
        kw("procedure", cfg),
        (space(0), "::", space(0)).optional(),
        list(specific_procedure(cfg), 1..),
    ).map(|(_, module, _, _, specific_procedure_list)| ProcedureStmt {
        module: module.is_some(),
        specific_procedure_list,
        comment: None,
    })
}

#[derive(Debug, Clone)]
pub struct SpecificProcedure<Span>(pub Name<Span>);

#[syntax_rule(
    F18V007r1 rule "specific-procedure" #1507 : "is procedure-name",
)]
pub fn specific_procedure<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = SpecificProcedure<S::Span>> + 'a {
    name(cfg, false).map(SpecificProcedure)
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
pub struct GenericStmt<Span> {
    pub access_spec: Option<AccessSpec>,
    pub generic_spec: GenericSpec<Span>,
    pub specific_procedure_list: Vec<Name<Span>>,
    pub comment: Option<LineComment<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "generic-stmt" #1510 :
    "is GENERIC [ , access-spec ] :: generic-spec => specific-procedure-list",
)]
pub fn generic_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = GenericStmt<S::Span>> + 'a {
    (
        (space(0), kw("generic", cfg)),
        (
            (space(0), ',', space(0)),
            access_spec(cfg),
        ).map(|(_, access_spec)| access_spec).optional(),
        (space(0), "::", space(0)),
        generic_spec(cfg),
        (space(0), "=>", space(0)),
        list(name(cfg, false), 1..),
        statement_termination(),
    ).map(|(_, access_spec, _, generic_spec, _, specific_procedure_list, comment)| GenericStmt {
        access_spec,
        generic_spec,
        specific_procedure_list,
        comment,
    })
}

#[derive(Debug, Clone)]
pub struct ExternalStmt<Span> {
    pub external_name_list: Vec<Name<Span>>,
    pub comment: Option<LineComment<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "external-stmt" #1511 : "is EXTERNAL [ :: ] external-name-list",
)]
pub fn external_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ExternalStmt<S::Span>> + 'a {
    (
        space(0),
        kw("external", cfg),
        space(0),
        ("::", space(0)).optional(),
        list(name(cfg, false), 1..),
        statement_termination(),
    ).map(|(_, _, _, _, external_name_list, comment)| ExternalStmt {
        external_name_list,
        comment,
    })
}

#[derive(Debug, Clone)]
pub struct ProcedureDeclarationStmt<Span> {
    pub proc_interface: Option<ProcInterface<Span>>,
    pub proc_attr_spec_list: Option<Vec<ProcAttrSpec<Span>>>,
    pub proc_decl_list: Vec<ProcDecl<Span>>,
    pub comment: Option<LineComment<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "procedure-declaration-stmt" #1512 :
    "is PROCEDURE ( [ proc-interface ] ) [ [ , proc-attr-spec ] ... :: ] proc-decl-list",
)]
pub fn procedure_declaration_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ProcedureDeclarationStmt<S::Span>> + 'a {
    (
        (space(0), kw("procedure", cfg), space(0), '(', space(0)),
        proc_interface(cfg).optional(),
        (space(0), ')', space(0)),
        (
            (space(0), ',', space(0)),
            many(
                (space(0), ',', space(0), proc_attr_spec(cfg)).map(|(_, _, _, proc_attr_spec)| proc_attr_spec),
                0..,
            ),
            (space(0), "::", space(0)),
        ).map(|(_, proc_attr_spec_list, _)| proc_attr_spec_list).optional(),
        list(proc_decl(cfg), 1..),
        statement_termination(),
    ).map(|(_, proc_interface, _, proc_attr_spec_list, proc_decl_list, comment)| ProcedureDeclarationStmt {
        proc_interface,
        proc_attr_spec_list: proc_attr_spec_list,
        proc_decl_list,
        comment,
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
pub fn proc_interface<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ProcInterface<S::Span>> + 'a {
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
pub fn proc_attr_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ProcAttrSpec<S::Span>> + 'a {
    alt!(
        access_spec(cfg).map(ProcAttrSpec::AccessSpec),
        proc_language_binding_spec(cfg).map(ProcAttrSpec::ProcLanguageBindingSpec),
        (kw("intent", cfg), space(0), '(', space(0), intent_spec(cfg), (space(0), ')')).map(|(_, _, _, _, intent_spec, _)| ProcAttrSpec::Intent(intent_spec)),
        (kw("optional", cfg)).map(|_| ProcAttrSpec::Optional),
        (kw("pointer", cfg)).map(|_| ProcAttrSpec::Pointer),
        (kw("protected", cfg)).map(|_| ProcAttrSpec::Protected),
        (kw("save", cfg)).map(|_| ProcAttrSpec::Save),
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
pub fn proc_decl<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ProcDecl<S::Span>> + 'a {
    (
        name(cfg, false),
        (
            (space(0), "=>", space(0)),
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
pub fn interface_name<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = InterfaceName<S::Span>> + 'a {
    name(cfg, false).map(InterfaceName)
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
pub fn proc_pointer_init<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ProcPointerInit<S::Span>> + 'a {
    alt!(
        (kw("null", cfg)).map(|_| ProcPointerInit::NullInit),
        initial_proc_target(cfg).map(|t| ProcPointerInit::InitialProcTarget(t)),
    )
}

#[derive(Debug, Clone)]
pub struct InitialProcTarget<Span>(pub Name<Span>);

#[syntax_rule(
    F18V007r1 rule "initial-proc-target" #1518 : "is procedure-name",
)]
pub fn initial_proc_target<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = InitialProcTarget<S::Span>> + 'a {
    name(cfg, false).map(InitialProcTarget)
}

#[derive(Debug, Clone)]
pub struct IntrinsicStmt<Span> {
    pub intrinsic_procedure_name_list: Vec<Name<Span>>,
    pub comment: Option<LineComment<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "intrinsic-stmt" #1519 : "is INTRINSIC [ :: ] intrinsic-procedure-name-list",
)]
pub fn intrinsic_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = IntrinsicStmt<S::Span>> + 'a {
    (
        space(0),
        kw("intrinsic", cfg),
        (space(0), "::", space(0)).optional(),
        list(name(cfg, false), 1..),
        statement_termination(),
    ).map(|(_, _, _, intrinsic_procedure_name_list, comment)| IntrinsicStmt {
        intrinsic_procedure_name_list,
        comment,
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
pub fn prefix<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = Prefix<S::Span>> + 'a {
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
pub fn prefix_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = PrefixSpec<S::Span>> + 'a {
    alt!(
        declaration_type_spec(cfg).map(PrefixSpec::DeclarationTypeSpec),
        (kw("elemental", cfg)).map(|_| PrefixSpec::Elemental),
        (kw("impure", cfg)).map(|_| PrefixSpec::Impure),
        (kw("module", cfg)).map(|_| PrefixSpec::Module),
        (kw("non_recursive", cfg)).map(|_| PrefixSpec::NonRecursive),
        (kw("pure", cfg)).map(|_| PrefixSpec::Pure),
        (kw("recursive", cfg)).map(|_| PrefixSpec::Recursive),
    )
}

#[derive(Debug, Clone)]
pub struct ProcLanguageBindingSpec<Span>(pub LanguageBindingSpec<Span>);

#[syntax_rule(
    F18V007r1 rule "proc-language-binding-spec" #1528 : "is language-binding-spec",
)]
pub fn proc_language_binding_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ProcLanguageBindingSpec<S::Span>> + 'a {
    language_binding_spec(cfg).map(ProcLanguageBindingSpec)
}

#[derive(Debug, Clone)]
pub struct FunctionSubprogram<Span> {
    pub function_stmt: FunctionStmt<Span>,
    pub specification_part: Option<SpecificationPart<Span>>,
    pub execution_part: Option<ExecutionPart<Span>>,
    pub internal_subprogram_part: Option<InternalSubprogramPart<Span>>,
    pub end_function_stmt: EndFunctionStmt<Span>,
}

#[syntax_rule(
    F18V007r1 rule "function-subprogram" #1529 :
    "is function-stmt"
    "    [ specification-part ]"
    "    [ execution-part ]"
    "    [ internal-subprogram-part ]"
    "    end-function-stmt",
)]
pub fn function_subprogram<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = FunctionSubprogram<S::Span>> + 'a {
    |_| todo!("TODO: \"function_subprogram\" parser not implemented yet")
}

#[derive(Debug, Clone)]
pub struct FunctionStmt<Span> {
    pub prefix: Option<Prefix<Span>>,
    pub function_name: Name<Span>,
    pub dummy_arg_name_list: Vec<DummyArgName<Span>>,
    pub suffix: Option<Suffix<Span>>,
    pub comment: Option<LineComment<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "function-stmt" #1530 :
    "is [ prefix ] FUNCTION function-name ( [ dummy-arg-name-list ] ) [ suffix ]",
)]
pub fn function_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = FunctionStmt<S::Span>> + 'a {
    (
        space(0),
        prefix(cfg).optional(),
        kw("function", cfg),
        space(0),
        name(cfg, false),
        (space(0), '(', space(0)),
        list(dummy_arg_name(cfg), 0..),
        (space(0), ')', space(0)),
        suffix(cfg).optional(),
        statement_termination(),
    ).map(|(_, prefix, _, _, function_name, _, dummy_arg_name_list, _, suffix, comment)| FunctionStmt {
        prefix,
        function_name,
        dummy_arg_name_list,
        suffix,
        comment,
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

#[derive(Debug, Clone)]
pub struct EndFunctionStmt<Span> {
    pub function_name: Option<Name<Span>>,
    pub comment: Option<LineComment<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "end-function-stmt" #1533 : "is END [ FUNCTION [ function-name ] ]",
)]
pub fn end_function_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = EndFunctionStmt<S::Span>> + 'a {
    (
        space(0),
        kw("end", cfg),
        (
            space(0),
            kw("function", cfg),
            (
                space(0),
                name(cfg, false),
            ).map(|(_, name)| name).optional(),
        ).map(|(_, _, name)| name).optional(),
        statement_termination(),
    ).map(|(_, _, function_name, comment)| EndFunctionStmt {
        function_name: function_name.flatten(),
        comment,
    })
}

#[derive(Debug, Clone)]
pub struct SubroutineSubprogram<Span> {
    pub subroutine_stmt: SubroutineStmt<Span>,
    pub specification_part: Option<SpecificationPart<Span>>,
    pub execution_part: Option<ExecutionPart<Span>>,
    pub internal_subprogram_part: Option<InternalSubprogramPart<Span>>,
    pub end_subroutine_stmt: Option<EndSubroutineStmt<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "subroutine-subprogram" #1534 :
    "is subroutine-stmt"
    "    [ specification-part ]"
    "    [ execution-part ]"
    "    [ internal-subprogram-part ]"
    "    end-subroutine-stmt",
)]
pub fn subroutine_subprogram<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = SubroutineSubprogram<S::Span>> + 'a {
    move |source: S| {
        let (subroutine_stmt, source) = subroutine_stmt(cfg).parse(source)?;
        let mut result = SubroutineSubprogram {
            subroutine_stmt,
            specification_part: None,
            execution_part: None,
            internal_subprogram_part: None,
            end_subroutine_stmt: None,
        };

        let (r, source) = specification_part(cfg, end_subroutine_stmt(cfg)).optional().parse(source)?;
        if let Some((specification_part, e)) = r {
            result.specification_part = Some(specification_part);
            if let Some(e) = e {
                result.end_subroutine_stmt = Some(e);
                return Some((result, source));
            }
        }

        let (r, source) = execution_part(cfg, end_subroutine_stmt(cfg)).optional().parse(source)?;
        if let Some((execution_part, e)) = r {
            result.execution_part = Some(execution_part);
            if let Some(e) = e {
                result.end_subroutine_stmt = Some(e);
                return Some((result, source));
            }
        }

        let (r, source) = internal_subprogram_part(cfg, end_subroutine_stmt(cfg)).optional().parse(source)?;
        if let Some((internal_subprogram_part, e)) = r {
            result.internal_subprogram_part = Some(internal_subprogram_part);
            if let Some(e) = e {
                result.end_subroutine_stmt = Some(e);
                return Some((result, source));
            }
        }

        let (end_subroutine_stmt, source) = end_subroutine_stmt(cfg).optional().parse(source)?;
        result.end_subroutine_stmt = end_subroutine_stmt;

        Some((result, source))
    }
}

#[derive(Debug, Clone)]
pub struct SubroutineStmt<Span> {
    pub prefix: Option<Prefix<Span>>,
    pub subroutine_name: Name<Span>,
    pub dummy_arg_list: Vec<DummyArg<Span>>,
    pub proc_language_binding_spec: Option<ProcLanguageBindingSpec<Span>>,
    pub comment: Option<LineComment<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "subroutine-stmt" #1535 :
    "is [ prefix ] SUBROUTINE subroutine-name [ ( [ dummy-arg-list ] ) [ proc-language-binding-spec ] ]",
)]
pub fn subroutine_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = SubroutineStmt<S::Span>> + 'a {
    (
        space(0),
        (prefix(cfg), space(0)).map(|(prefix, _)| prefix).optional(),
        (kw("subroutine", cfg), space(0)),
        name(cfg, false), space(0),
        (
            (space(0), '(', space(0)),
            list(dummy_arg(cfg), 0..),
            (space(0), ')', space(0)),
            proc_language_binding_spec(cfg).optional(),
        ).map(|(_, list, _, proc_language_binding_spec)| (list, proc_language_binding_spec)).optional(),
        statement_termination(),
    ).map(|(_, prefix, _, subroutine_name, _, dummy_arg_list_proc_language_binding_spec, comment)| {
        let (dummy_arg_list, proc_language_binding_spec) = match dummy_arg_list_proc_language_binding_spec {
            Some((dummy_arg_list, proc_language_binding_spec)) => (dummy_arg_list, proc_language_binding_spec),
            None => (vec![], None),
        };
        SubroutineStmt {
            prefix,
            subroutine_name,
            dummy_arg_list,
            proc_language_binding_spec,
            comment,
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
pub fn dummy_arg<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DummyArg<S::Span>> + 'a {
    alt!(
        dummy_arg_name(cfg).map(DummyArg::Name),
        (SpecialCharacter::Asterisk).map(|_| DummyArg::Star),
    )
}

#[derive(Debug, Clone)]
pub struct EndSubroutineStmt<Span> {
    pub subroutine_name: Option<Name<Span>>,
    pub comment: Option<LineComment<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "end-subroutine-stmt" #1537 : "is END [ SUBROUTINE [ subroutine-name ] ]",
)]
pub fn end_subroutine_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = EndSubroutineStmt<S::Span>> + 'a {
    (
        space(0),
        kw("end", cfg),
        (
            space(0),
            kw("subroutine", cfg),
            (
                space(0),
                name(cfg, false),
            ).map(|(_, name)| name).optional(),
        ).map(|(_, _, name)| name).optional(),
        statement_termination(),
    ).map(|(_, _, subroutine_name, comment)| EndSubroutineStmt {
        subroutine_name: subroutine_name.flatten(),
        comment,
    })
}

#[derive(Debug, Clone)]
pub struct SeparateModuleSubprogram<Span> {
    pub mp_subprogram_stmt: MpSubprogramStmt<Span>,
    pub specification_part: Option<SpecificationPart<Span>>,
    pub execution_part: Option<ExecutionPart<Span>>,
    pub internal_subprogram_part: Option<InternalSubprogramPart<Span>>,
    pub end_mp_subprogram_stmt: Option<EndMpSubprogramStmt<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "separate-module-subprogram" #1538 :
    "is mp-subprogram-stmt"
    "    [ specification-part ]"
    "    [ execution-part ]"
    "    [ internal-subprogram-part ]"
    "    end-mp-subprogram-stmt",
)]
pub fn separate_module_subprogram<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = SeparateModuleSubprogram<S::Span>> + 'a {
    move |source: S| {
        let (mp_subprogram_stmt, source) = mp_subprogram_stmt(cfg).parse(source)?;
        let mut result = SeparateModuleSubprogram {
            mp_subprogram_stmt,
            specification_part: None,
            execution_part: None,
            internal_subprogram_part: None,
            end_mp_subprogram_stmt: None,
        };

        let (r, source) = specification_part(cfg, end_mp_subprogram_stmt(cfg)).optional().parse(source)?;
        if let Some((specification_part, e)) = r {
            result.specification_part = Some(specification_part);
            if let Some(e) = e {
                result.end_mp_subprogram_stmt = Some(e);
                return Some((result, source));
            }
        }

        let (r, source) = execution_part(cfg, end_mp_subprogram_stmt(cfg)).optional().parse(source)?;
        if let Some((execution_part, e)) = r {
            result.execution_part = Some(execution_part);
            if let Some(e) = e {
                result.end_mp_subprogram_stmt = Some(e);
                return Some((result, source));
            }
        }

        let (r, source) = internal_subprogram_part(cfg, end_mp_subprogram_stmt(cfg)).optional().parse(source)?;
        if let Some((internal_subprogram_part, e)) = r {
            result.internal_subprogram_part = Some(internal_subprogram_part);
            if let Some(e) = e {
                result.end_mp_subprogram_stmt = Some(e);
                return Some((result, source));
            }
        }

        let (end_mp_subprogram_stmt, source) = end_mp_subprogram_stmt(cfg).optional().parse(source)?;
        result.end_mp_subprogram_stmt = end_mp_subprogram_stmt;

        Some((result, source))
    }
}

#[derive(Debug, Clone)]
pub struct EntryStmt<Span> {
    pub entry_name: Name<Span>,
    pub dummy_arg_list: Vec<DummyArg<Span>>,
    pub suffix: Option<Suffix<Span>>,
    pub comment: Option<LineComment<Span>>,
}

#[derive(Debug, Clone)]
pub struct MpSubprogramStmt<Span> {
    pub procedure_name: Name<Span>,
    pub comment: Option<LineComment<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "mp-subprogram-stmt" #1539 : "is MODULE PROCEDURE procedure-name",
)]
pub fn mp_subprogram_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = MpSubprogramStmt<S::Span>> + 'a {
    (
        space(0),
        kw("module", cfg),
        space(0),
        kw("procedure", cfg),
        space(0),
        name(cfg, false),
        statement_termination(),
    ).map(|(_, _, _, _, _, procedure_name, comment)| MpSubprogramStmt {
        procedure_name,
        comment,
    })
}

#[derive(Debug, Clone)]
pub struct EndMpSubprogramStmt<Span> {
    pub procedure_name: Option<Name<Span>>,
    pub comment: Option<LineComment<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "end-mp-subprogram-stmt" #1540 : "is END [PROCEDURE [procedure-name]]",
)]
pub fn end_mp_subprogram_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = EndMpSubprogramStmt<S::Span>> + 'a {
    (
        space(0),
        kw("end", cfg),
        (
            space(0),
            kw("procedure", cfg),
            (
                space(0),
                name(cfg, false),
            ).map(|(_, name)| name).optional(),
        ).map(|(_, _, name)| name).optional(),
        statement_termination(),
    ).map(|(_, _, procedure_name, comment)| EndMpSubprogramStmt {
        procedure_name: procedure_name.flatten(),
        comment,
    })
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

use super::*;

// TODO construct
//#[derive(Debug, Clone)]
//pub struct DerivedTypeDef<Span> {
//    pub decl_stmt: DerivedTypeStmt<Span>,
//    pub type_param_def_stmts: Vec<MaybeStatement<TypeParamDefStmt<Span>, Span>>,
//    pub priv_or_seq: Vec<MaybeStatement<PrivateOrSequence<Span>, Span>>,
//    pub component_part: Option<ComponentPart<Span>>,
//    pub type_bound_procedure_part: Option<TypeBoundProcedurePart<Span>>,
//    pub end_type_stmt: Option<EndTypeStmt<Span>>,
//}
//
//// TODO test
//#[doc = s_rule!(
//    F18V007r1 rule "derived-type-def" #726:
//    "is derived-type-stmt"
//    "    [ type-param-def-stmt ] ..."
//    "    [ private-or-sequence ] ..."
//    "    [ component-part ]"
//    "    [ type-bound-procedure-part ]"
//    "    end-type-stmt",
//)]
//pub fn derived_type_def<S: Lexed>(source: S) -> PResult<DerivedTypeDef<MultilineSpan>, S> {
//    // TODO rewrite better
//    // TODO test
//    move |source: S| {
//        let (s, source) = derived_type_stmt
//            .parse(source)?;
//
//        let name = s.name.0.value();
//
//        let ((type_param_def_stmts, end), source) = many_until(
//            maybe_statement(type_param_def_stmt),
//            end_type_stmt(cfg, name),
//            0..,
//        )
//            .parse(source)?;
//
//        if let Some(end) = end {
//            return Some((DerivedTypeDef {
//                decl_stmt: s,
//                type_param_def_stmts,
//                priv_or_seq: vec![],
//                component_part: None,
//                type_bound_procedure_part: None,
//                end_type_stmt: Some(end),
//            }, source));
//        }
//
//        let ((priv_or_seq, end), source) = many_until(
//            maybe_statement(private_or_sequence),
//            end_type_stmt(cfg, name),
//            0..,
//        )
//            .parse(source)?;
//
//        if let Some(end) = end {
//            return Some((DerivedTypeDef {
//                decl_stmt: s,
//                type_param_def_stmts,
//                priv_or_seq,
//                component_part: None,
//                type_bound_procedure_part: None,
//                end_type_stmt: Some(end),
//            }, source));
//        }
//
//        let ((component_part, end), source) = component_part(cfg, end_type_stmt(cfg, name))
//            .parse(source)?;
//
//        if let Some(end) = end {
//            return Some((DerivedTypeDef {
//                decl_stmt: s,
//                type_param_def_stmts,
//                priv_or_seq,
//                component_part: Some(component_part),
//                type_bound_procedure_part: None,
//                end_type_stmt: Some(end),
//            }, source));
//        }
//
//        let ((type_bound_procedure_part, end), source) = type_bound_procedure_part(cfg, end_type_stmt(cfg, name))
//            .parse(source)?;
//
//        if let Some(end) = end {
//            return Some((DerivedTypeDef {
//                decl_stmt: s,
//                type_param_def_stmts,
//                priv_or_seq,
//                component_part: Some(component_part),
//                type_bound_procedure_part: Some(type_bound_procedure_part),
//                end_type_stmt: Some(end),
//            }, source));
//        }
//
//        // TODO maybe many unclassified or comment...
//
//        let (end, source) = end_type_stmt(cfg, name)
//            .map(|end| Some(end))
//            .or(eof().map(|_| None))
//            .map(|o| o.inner())
//            .parse(source)?;
//
//        Some((DerivedTypeDef {
//            decl_stmt: s,
//            type_param_def_stmts,
//            priv_or_seq,
//            component_part: Some(component_part),
//            type_bound_procedure_part: Some(type_bound_procedure_part),
//            end_type_stmt: end,
//        }, source))
//    }
//}

#[derive(Debug, Clone)]
pub struct DerivedTypeStmt<Span> {
    pub name: Name<Span>,
    pub attributes: Vec<TypeAttrSpec<Span>>,
    pub type_param_names: Vec<Name<Span>>,
}

// TODO test
#[doc = s_rule!(
    F18V007r1 rule "derived-type-stmt" #727 :
    "is TYPE [ [ , type-attr-spec-list ] :: ] type-name [ ( type-param-name-list ) ]",
)]
pub fn derived_type_stmt<S: Lexed>(source: S) -> PResult<DerivedTypeStmt<MultilineSpan>, S> {
    let type_attr_spec_list = || (
        comma(),
        separated(
            type_attr_spec,
            comma(),
            0..,
        ),
    ).map(|(_, l)| l);

    let type_param_name_list = || separated(
        name(),
        comma(),
        0..,
    );

    (
        kw!(TYPE),
        (
            type_attr_spec_list(),
            double_colon(),
        )
            .map(|(spec_list, _)| spec_list)
            .optional()
            .map(|spec_list| spec_list.unwrap_or(vec![])),
        name(),
        (
            delim('('),
            type_param_name_list(),
            delim(')'),
        )
            .map(|(_, names, _)| names)
            .optional()
            .map(|names| names.unwrap_or(vec![])),
    ).map(|(_, attrs, name, type_param_names)| DerivedTypeStmt {
        name,
        attributes: attrs,
        type_param_names,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub enum TypeAttrSpec<Span> {
    Abstract,
    Access(AccessSpec),
    BindC,
    Extends(Name<Span>),
}

// TODO test
#[doc = s_rule!(
    F18V007r1 rule "type-attr-spec" #728 : 
    "is ABSTRACT"
    "or access-spec"
    "or BIND (C)"
    "or EXTENDS ( parent-type-name )",
)]
pub fn type_attr_spec<S: Lexed>(source: S) -> PResult<TypeAttrSpec<MultilineSpan>, S> {
    alt!(
        for S =>
        kw!(abstract).map(|_| TypeAttrSpec::Abstract),
        access_spec.map(TypeAttrSpec::Access),
        (
            kw!(BIND),
            delim('('),
            kw!(C),
            delim(')'),
        ).map(|_| TypeAttrSpec::BindC),
        (
            kw!(EXTENDS),
            name(),
        ).map(|(_, name)| TypeAttrSpec::Extends(name)),
    ).parse(source)
}

#[derive(Debug, Clone, EnumAsInner, PartialEq, Eq, Hash)]
pub enum TypeParamAttrSpec {
    Kind,
    Len,
}

// TODO test
#[doc = s_rule!(
    F18V007r1 rule "type-param-attr-spec" #734 :
    "is KIND"
    "or LEN",
)]
pub fn type_param_attr_spec<S: Lexed>(source: S) -> PResult<TypeParamAttrSpec, S> {
    alt!(
        for S =>
        kw!(KIND).map(|_| TypeParamAttrSpec::Kind),
        kw!(LEN).map(|_| TypeParamAttrSpec::Len),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct TypeParamDecl<Span> {
    pub name: Name<Span>,
    pub init: Option<IntConstantExpr<Span>>,
}

// TODO test
#[doc = s_rule!(
    F18V007r1 rule "type-param-decl" #733 : "is type-param-name [ = scalar-int-constant-expr ]",
)]
pub fn type_param_decl<S: Lexed>(source: S) -> PResult<TypeParamDecl<MultilineSpan>, S> {
    (
        name(),
        (
            equals(),
            int_constant_expr,
        ).map(|(_, expr)| expr).optional(),
    ).map(|(name, init)| TypeParamDecl {
        name,
        init,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct TypeParamDefStmt<Span> {
    pub type_spec: IntegerTypeSpec<Span>,
    pub attr: TypeParamAttrSpec,
    pub decls: Vec<TypeParamDecl<Span>>,
}

// TODO test
#[doc = s_rule!(
    F18V007r1 rule "type-param-def-stmt" #732 : 
    "is integer-type-spec, type-param-attr-spec :: type-param-decl-list",
)]
pub fn type_param_def_stmt<S: Lexed>(source: S) -> PResult<TypeParamDefStmt<MultilineSpan>, S> {
    (
        integer_type_spec,
        comma(),
        type_param_attr_spec,
        double_colon(),
        separated(
            type_param_decl,
            comma(),
            1..,
        ),
    ).map(|(type_spec, _, attr, _, decls)| TypeParamDefStmt {
        type_spec,
        attr,
        decls,
    }).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ComponentArraySpec<Span> {
    Explicit(ExplicitShapeSpec<Span>),
    Deferred(DeferredShapeSpec),
}

// TODO test
#[doc = s_rule!(
    F18V007r1 rule "component-array-spec" #740 :
    "is explicit-shape-spec-list"
    "or deferred-shape-spec-list",
)]
pub fn component_array_spec<S: Lexed>(source: S) -> PResult<ComponentArraySpec<MultilineSpan>, S> {
    alt!(
        for S =>
        explicit_shape_spec.map(ComponentArraySpec::Explicit),
        deferred_shape_spec.map(ComponentArraySpec::Deferred),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct ComponentDecl<Span> {
    pub name: Name<Span>,
    pub array_spec: Option<ComponentArraySpec<Span>>,
    pub coarray_spec: Option<CoarraySpec<Span>>,
    pub char_length: Option<CharLength<Span>>,
    pub component_initialization: Option<ComponentInitialization<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "component-decl" #739 :
    "is component-name [ ( component-array-spec ) ] [ lbracket coarray-spec rbracket ] [ * char-length ] [ component-initialization ]",
)]
pub fn component_decl<S: Lexed>(source: S) -> PResult<ComponentDecl<MultilineSpan>, S> {
    // TODO test
    (
        name(),
        (
            delim('('),
            component_array_spec,
            delim(')'),
        ).map(|(_, a, _)| a).optional(),
        (
            delim('['),
            coarray_spec,
            delim(']'),
        ).map(|(_, a, _)| a).optional(),
        (
            asterisk(),
            char_length,
        ).map(|(_, a)| a).optional(),
        component_initialization.optional(),
    ).map(|(name, array_spec, coarray_spec, char_length, component_initialization)| ComponentDecl {
        name,
        array_spec,
        coarray_spec,
        char_length,
        component_initialization,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct InitialDataTarget<Span>(pub Designator<Span>);// TODO

#[doc = s_rule!(
    F18V007r1 rule "initial-data-target" #744 : "is designator",
)]
pub fn initial_data_target<S: Lexed>(source: S) -> PResult<InitialDataTarget<MultilineSpan>, S> {
    // TODO test
    designator(false).map(InitialDataTarget).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ComponentAttrSpec<Span> {
    Access(AccessSpec),
    Allocatable,
    Codimension(CoarraySpec<Span>),
    Contiguous,
    Dimension(ComponentArraySpec<Span>),
    Pointer,
}

#[doc = s_rule!(
    F18V007r1 rule "component-attr-spec" #738 :
    "is access-spec"
    "or ALLOCATABLE"
    "or CODIMENSION lbracket coarray-spec rbracket"
    "or CONTIGUOUS"
    "or DIMENSION ( component-array-spec )"
    "or POINTER",
)]

pub fn component_attr_spec<S: Lexed>(source: S) -> PResult<ComponentAttrSpec<MultilineSpan>, S> {
    // TODO test and check, written by copilot, check order
    alt!(
        for S =>
        access_spec.map(ComponentAttrSpec::Access),
        kw!(ALLOCATABLE).map(|_| ComponentAttrSpec::Allocatable),
        (
            kw!(CODIMENSION),
            coarray_spec,
        ).map(|(_, a)| ComponentAttrSpec::Codimension(a)),
        kw!(CONTIGUOUS).map(|_| ComponentAttrSpec::Contiguous),
        (
            kw!(DIMENSION),
            component_array_spec,
        ).map(|(_, a)| ComponentAttrSpec::Dimension(a)),
        kw!(POINTER).map(|_| ComponentAttrSpec::Pointer),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct DataComponentDefStmt<Span> {
    pub type_spec: DeclarationTypeSpec<Span>,
    pub attrs: Vec<ComponentAttrSpec<Span>>,
    pub component_decls: Vec<ComponentDecl<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "data-component-def-stmt" #737 :
    "is declaration-type-spec [ [ , component-attr-spec-list ] :: ] component-decl-list",
)]
pub fn data_component_def_stmt<S: Lexed>(source: S) -> PResult<DataComponentDefStmt<MultilineSpan>, S> {
    // TODO test
    (
        declaration_type_spec,
        (
            comma(),
            separated(
                component_attr_spec,
                comma(),
                0..,
            ),
            double_colon(),
        ).map(|(_, attrs, _)| attrs).optional().map(|attrs| attrs.unwrap_or(vec![])),
        separated(
            component_decl,
            comma(),
            1..,
        ),
    ).map(|(type_spec, attrs, component_decls)| DataComponentDefStmt {
        type_spec,
        attrs,
        component_decls,
    }).parse(source)
}

// TODO construct
//#[derive(Debug, Clone)]
//pub struct ComponentPart<Span>(pub Vec<MaybeStatement<ComponentDefStmt<Span>, Span>>);
//
//#[doc = s_rule!(
//    F18V007r1 rule "component-part" #735 : "is [ component-def-stmt ] ...",
//)]
//pub fn component_part<'a, S: Lexed + 'a, U: 'a>(
//    cfg: &'a Cfg,
//    until: impl Parser<S, Token = U> + 'a,
//) -> impl Parser<S, Token = (ComponentPart<S::Span>, Option<U>), S> {
//    // TODO test
//    many_until(
//        maybe_statement(component_def_stmt),
//        until,
//        0..
//    ).map(|(statements, until)| (ComponentPart(statements), until))
//}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ComponentDefStmt<Span> {
    Data(DataComponentDefStmt<Span>),
    Proc(ProcComponentDefStmt<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "component-def-stmt" #736 :
    "is data-component-def-stmt"
    "or proc-component-def-stmt",
)]
pub fn component_def_stmt<S: Lexed>(source: S) -> PResult<ComponentDefStmt<MultilineSpan>, S> {
    // TODO test
    alt!(
        for S =>
        data_component_def_stmt.map(ComponentDefStmt::Data),
        proc_component_def_stmt.map(ComponentDefStmt::Proc),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct ProcComponentDefStmt<Span> {
    pub interface: Option<ProcInterface<Span>>,
    pub attrs: Vec<ProcComponentAttrSpec<Span>>,
    pub decls: Vec<ProcDecl<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "proc-component-def-stmt" #741 :
    "is PROCEDURE ( [ proc-interface ] ) , proc-component-attr-spec-list :: proc-decl-list",
)]
pub fn proc_component_def_stmt<S: Lexed>(source: S) -> PResult<ProcComponentDefStmt<MultilineSpan>, S> {
    (
        kw!(PROCEDURE),
        delim('('),
        proc_interface.optional(),
        delim(')'),
        comma(),
        separated(
            proc_component_attr_spec,
            comma(),
            1..,
        ),
        double_colon(),
        separated(
            proc_decl,
            comma(),
            1..,
        ),
    ).map(|(_, _, interface, _, _, attrs, _, decls)| ProcComponentDefStmt {
        interface,
        attrs,
        decls,
    }).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ProcComponentAttrSpec<Span> {
    AccessSpec(AccessSpec),
    Nopass,
    Pass(Name<Span>),
    Pointer,
}

#[doc = s_rule!(
    F18V007r1 rule "proc-component-attr-spec" #742 :
    "is access-spec"
    "or NOPASS"
    "or PASS [ (arg-name) ]"
    "or POINTER",
)]
pub fn proc_component_attr_spec<S: Lexed>(source: S) -> PResult<ProcComponentAttrSpec<MultilineSpan>, S> {
    // TODO test
    alt!(
        for S =>
        access_spec.map(ProcComponentAttrSpec::AccessSpec),
        kw!(NOPASS).map(|_| ProcComponentAttrSpec::Nopass),
        (
            kw!(PASS),
            delim('('),
            name(),
            delim(')'),
        ).map(|(_, _, name, _)| ProcComponentAttrSpec::Pass(name)),
        kw!(POINTER).map(|_| ProcComponentAttrSpec::Pointer),
    ).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum TypeSpec<Span> {
    Intrinsic(IntrinsicTypeSpec<Span>),
    Derived(DerivedTypeSpec<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "type-spec" #702 :
    "is intrinsic-type-spec"
    "or derived-type-spec",
)]
pub fn type_spec<S: Lexed>(source: S) -> PResult<TypeSpec<MultilineSpan>, S> {
    alt!(
        for S =>
        intrinsic_type_spec.map(TypeSpec::Intrinsic),
        derived_type_spec.map(TypeSpec::Derived),
    ).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum DeclarationTypeSpec<Span> {
    Intrinsic, // TODO
    TypeIntrinsic, // TODO
    TypeDerived, // TODO
    ClassDerived, // TODO
    ClassStar,
    TypeStar,
    _Phantom(Span),
}

#[doc = s_rule!(
    F18V007r1 rule "declaration-type-spec" #703 :
    "is intrinsic-type-spec"
    "or TYPE ( intrinsic-type-spec )"
    "or TYPE ( derived-type-spec )"
    "or CLASS ( derived-type-spec )"
    "or CLASS ( * )"
    "or TYPE ( * )",
)]
pub fn declaration_type_spec<S: Lexed>(source: S) -> PResult<DeclarationTypeSpec<MultilineSpan>, S> {
    alt!(
        for S =>
        intrinsic_type_spec.map(|_| DeclarationTypeSpec::Intrinsic),
        (
            kw!(TYPE),
            delim('('),
            intrinsic_type_spec,
            delim(')'),
        ).map(|(_, _, _, _)| DeclarationTypeSpec::TypeIntrinsic),
        (
            kw!(TYPE),
            delim('('),
            derived_type_spec,
            delim(')'),
        ).map(|(_, _, _, _)| DeclarationTypeSpec::TypeDerived),
        (
            kw!(CLASS),
            delim('('),
            derived_type_spec,
            delim(')'),
        ).map(|(_, _, _, _)| DeclarationTypeSpec::ClassDerived),
        (
            kw!(CLASS),
            delim('('),
            asterisk(),
            delim(')'),
        ).map(|(_, _, _, _)| DeclarationTypeSpec::ClassStar),
        (
            kw!(TYPE),
            delim('('),
            asterisk(),
            delim(')'),
        ).map(|(_, _, _, _)| DeclarationTypeSpec::TypeStar),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct BindingPrivateStmt<Span> {
    pub keyword: Keyword<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "binding-private-stmt" #747 :
    "is PRIVATE",
)]
pub fn binding_private_stmt<S: Lexed>(source: S) -> PResult<BindingPrivateStmt<MultilineSpan>, S> {
    kw!(PRIVATE).map(|keyword| BindingPrivateStmt {
        keyword,
    }).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum TypeBoundProcBinding<Span> {
    TypeBoundProcedureStmt(TypeBoundProcedureStmt<Span>),
    TypeBoundGenericStmt(TypeBoundGenericStmt<Span>),
    FinalProcedureStmt(FinalProcedureStmt<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "type-bound-proc-binding" #748 :
    "is type-bound-procedure-stmt"
    "or type-bound-generic-stmt"
    "or final-procedure-stmt",
)]
pub fn type_bound_proc_binding<S: Lexed>(source: S) -> PResult<TypeBoundProcBinding<MultilineSpan>, S> {
    alt!(
        for S =>
        type_bound_procedure_stmt.map(TypeBoundProcBinding::TypeBoundProcedureStmt),
        type_bound_generic_stmt.map(TypeBoundProcBinding::TypeBoundGenericStmt),
        final_procedure_stmt.map(TypeBoundProcBinding::FinalProcedureStmt),
    ).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum TypeBoundProcedureStmt<Span> {
    Form1(Option<Vec<BindingAttr<Span>>>, Vec<TypeBoundProcDecl<Span>>),
    Form2(Name<Span>, Vec<BindingAttr<Span>>, Vec<Name<Span>>),
}

#[doc = s_rule!(
    F18V007r1 rule "type-bound-procedure-stmt" #749 :
    "is PROCEDURE [ [ , binding-attr-list ] :: ] type-bound-proc-decl-list"
    "or PROCEDURE (interface-name), binding-attr-list :: binding-name-list",
)]
pub fn type_bound_procedure_stmt<S: Lexed>(source: S) -> PResult<TypeBoundProcedureStmt<MultilineSpan>, S> {
    let form_1 = (
        kw!(PROCEDURE),
        (
            comma(),
            list(binding_attr, 0..),
            double_colon(),
        ).map(|(_, attrs, _)| attrs).optional(),
        list(type_bound_proc_decl, 0..),
    ).map(|(_, attrs, decls)| TypeBoundProcedureStmt::Form1(attrs, decls));

    let form_2 = (
        kw!(PROCEDURE),
        delim('('),
        name(),
        (delim(')'), comma()),
        list(binding_attr, 0..),
        double_colon(),
        list(name(), 0..),
    ).map(|(_, _, interface_name, _, attrs, _, names)| TypeBoundProcedureStmt::Form2(interface_name, attrs, names));

    alt!(
        for S =>
        form_1,
        form_2,
    ).parse(source)
}

#[derive(Debug, Clone)]
pub enum BindingAttr<Span> {
    AccessSpec(AccessSpec),
    Deferred,
    NonOverridable,
    Nopass,
    Pass(Option<Name<Span>>),
}

#[doc = s_rule!(
    F18V007r1 rule "binding-attr" #752 :
    "is access-spec"
    "or DEFERRED"
    "or NON_OVERRIDABLE"
    "or NOPASS"
    "or PASS [ (arg-name) ]",
)]
pub fn binding_attr<S: Lexed>(source: S) -> PResult<BindingAttr<MultilineSpan>, S> {
    alt!(
        for S =>
        access_spec.map(BindingAttr::AccessSpec),
        kw!(DEFERRED).map(|_| BindingAttr::Deferred),
        kw!(NON_OVERRIDABLE).map(|_| BindingAttr::NonOverridable),
        kw!(NOPASS).map(|_| BindingAttr::Nopass),
        (
            kw!(PASS),
            (
                delim('('),
                name(),
                delim(')'),
            ).map(|(_, name, _)| name).optional(),
        ).map(|(_, name)| BindingAttr::Pass(name)),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct FinalProcedureStmt<Span> {
    pub final_subroutine_name_list: Vec<Name<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "final-procedure-stmt" #753 : "is FINAL [ :: ] final-subroutine-name-list",
)]
pub fn final_procedure_stmt<S: Lexed>(source: S) -> PResult<FinalProcedureStmt<MultilineSpan>, S> {
    (
        kw!(FINAL),
        double_colon(),
        list(name(), 1..),
    ).map(|(_, _, names)| FinalProcedureStmt {
        final_subroutine_name_list: names,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct TypeBoundProcDecl<Span> {
    pub binding_name: Name<Span>,
    pub procedure_name: Option<Name<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "type-bound-proc-decl" #750 : "is binding-name [ => procedure-name ]",
)]
pub fn type_bound_proc_decl<S: Lexed>(source: S) -> PResult<TypeBoundProcDecl<MultilineSpan>, S> {
    (
        name(),
        (
            arrow(),
            name(),
        ).map(|(_, name)| name).optional(),
    ).map(|(binding_name, procedure_name)| TypeBoundProcDecl {
        binding_name,
        procedure_name,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct EndTypeStmt<Span>(Option<Name<Span>>);// TODO

#[doc = s_rule!(
    F18V007r1 rule "end-type-stmt" #730 : "is END TYPE [ type-name ]",
)]
pub fn end_type_stmt<S: Lexed>(source: S) -> PResult<EndTypeStmt<MultilineSpan>, S> {
    (
        kw!(END),
        kw!(TYPE),
        name().optional(),
    ).map(|(_, _, _name)| EndTypeStmt(_name)).parse(source)
}

#[derive(Debug, Clone)]
pub struct DerivedTypeSpec<Span> {
    pub name: Name<Span>,
    pub type_param_specifiers: Option<Vec<TypeParamSpec<Span>>>,
}

#[derive(Debug, Clone)]
pub struct TypeBoundGenericStmt<Span> {
    pub access_spec: Option<AccessSpec>,
    pub generic_spec: GenericSpec<Span>,
    pub binding_name_list: Vec<Name<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "type-bound-generic-stmt" #751 : "is GENERIC [ , access-spec ] :: generic-spec => binding-name-list",
)]
pub fn type_bound_generic_stmt<S: Lexed>(source: S) -> PResult<TypeBoundGenericStmt<MultilineSpan>, S> {
    (
        kw!(GENERIC),
        (
            comma(),
            access_spec,
        ).map(|(_, a)| a).optional(),
        double_colon(),
        generic_spec,
        arrow(),
        list(name(), 0..),
    ).map(|(_, access_spec, _, generic_spec, _, binding_name_list)| TypeBoundGenericStmt {
        access_spec,
        generic_spec,
        binding_name_list,
    }).parse(source)
}

#[doc = s_rule!(
    F18V007r1 rule "derived-type-spec" #754 : "is type-name [ ( type-param-spec-list ) ]",
)]
pub fn derived_type_spec<S: Lexed>(source: S) -> PResult<DerivedTypeSpec<MultilineSpan>, S> {
    // TODO test
    (
        name(),
        (
            delim('('),
            separated(
                type_param_spec,
                comma(),
                0..,
            ),
            delim(')'),
        ).map(|(_, specs, _)| specs).optional(),
    ).map(|(name, type_param_specifiers)| DerivedTypeSpec {
        name,
        type_param_specifiers: type_param_specifiers,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct TypeParamSpec<Span> {
    pub keyword: Option<Name<Span>>,
    pub value: TypeParamValue<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "type-param-spec" #755 : "is [ keyword = ] type-param-value",
)]
pub fn type_param_spec<S: Lexed>(source: S) -> PResult<TypeParamSpec<MultilineSpan>, S> {
    // TODO test
    (
        (
            name(), // TODO keyword
            equals(),
        ).map(|(k, _)| k).optional(),
        type_param_value,
    ).map(|(keyword, value)| TypeParamSpec {
        keyword,
        value,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct StructureConstructor<Span> {
    pub type_spec: DerivedTypeSpec<Span>,
    pub component_spec: Vec<ComponentSpec<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "structure-constructor" #756 : "is derived-type-spec ( [ component-spec-list ] )",
)]
pub fn structure_constructor<S: Lexed>(source: S) -> PResult<StructureConstructor<MultilineSpan>, S> {
    // TODO test
    (
        derived_type_spec,
        delim('('),
        separated(
            component_spec,
            comma(),
            0..,
        ),
        delim(')'),
    ).map(|(type_spec, _, component_spec, _)| StructureConstructor {
        type_spec,
        component_spec,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct ComponentSpec<Span> {
    pub keyword: Option<Keyword<Span>>,
    pub value: ComponentDataSource<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "component-spec" #757 : "is [ keyword = ] component-data-source",
)]
pub fn component_spec<S: Lexed>(source: S) -> PResult<ComponentSpec<MultilineSpan>, S> {
    // TODO test
    (
        (
            name_as_keyword(),
            equals(),
        ).map(|(k, _)| k).optional(),
        component_data_source,
    ).map(|(keyword, value)| ComponentSpec {
        keyword,
        value,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub enum ComponentDataSource<Span> {
    Expr(Expr<Span>),
    DataTarget(DataTarget<Span>),
    ProcTarget(ProcTarget<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "component-data-source" #758 :
    "is expr"
    "or data-target"
    "or proc-target",
)]
pub fn component_data_source<S: Lexed>(source: S) -> PResult<ComponentDataSource<MultilineSpan>, S> {
    alt!(
        for S =>
        expr.map(ComponentDataSource::Expr),
        data_target.map(ComponentDataSource::DataTarget),
        proc_target.map(ComponentDataSource::ProcTarget),
    ).parse(source)
}

#[cfg(test)]
mod test {
    //use crate::test_configs;

    //use super::*;

    // TODO #[test]
    //fn test_end_type_stmt() {
    //    for cfg in test_configs() {
    //        let parser = end_type_stmt(&cfg, "foo");
    //        assert_eq!(parser.parses("ENDTYPE"), true);
    //        assert_eq!(parser.parses("endtype"), true);
    //        assert_eq!(parser.parses("end type"), true);
    //        assert_eq!(parser.parses("end type foo"), true);
    //        assert_eq!(parser.parses("end type bar"), false);
    //    }
    //}
}
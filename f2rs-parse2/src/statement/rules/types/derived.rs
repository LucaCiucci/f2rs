use std::marker::PhantomData;

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
//#[syntax_rule(
//    F18V007r1 rule "derived-type-def" #726:
//    "is derived-type-stmt"
//    "    [ type-param-def-stmt ] ..."
//    "    [ private-or-sequence ] ..."
//    "    [ component-part ]"
//    "    [ type-bound-procedure-part ]"
//    "    end-type-stmt",
//)]
//pub fn derived_type_def<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DerivedTypeDef<MultilineSpan>> + 'a {
//    // TODO rewrite better
//    // TODO test
//    move |source: S| {
//        let (s, source) = derived_type_stmt(cfg)
//            .parse(source)?;
//
//        let name = s.name.0.value();
//
//        let ((type_param_def_stmts, end), source) = many_until(
//            maybe_statement(type_param_def_stmt(cfg)),
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
//            maybe_statement(private_or_sequence(cfg)),
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
#[syntax_rule(
    F18V007r1 rule "derived-type-stmt" #727 :
    "is TYPE [ [ , type-attr-spec-list ] :: ] type-name [ ( type-param-name-list ) ]",
)]
pub fn derived_type_stmt<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DerivedTypeStmt<MultilineSpan>> + 'a {
    let type_attr_spec_list = || (
        comma(),
        separated(
            type_attr_spec(cfg),
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
    })
}

#[derive(Debug, Clone)]
pub enum TypeAttrSpec<Span> {
    Abstract,
    Access(AccessSpec),
    BindC,
    Extends(Name<Span>),
}

// TODO test
#[syntax_rule(
    F18V007r1 rule "type-attr-spec" #728 : 
    "is ABSTRACT"
    "or access-spec"
    "or BIND (C)"
    "or EXTENDS ( parent-type-name )",
)]
pub fn type_attr_spec<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = TypeAttrSpec<MultilineSpan>> + 'a {
    alt!(
        kw!(abstract).map(|_| TypeAttrSpec::Abstract),
        access_spec(cfg).map(TypeAttrSpec::Access),
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
    )
}

#[derive(Debug, Clone, EnumAsInner, PartialEq, Eq, Hash)]
pub enum TypeParamAttrSpec {
    Kind,
    Len,
}

// TODO test
#[syntax_rule(
    F18V007r1 rule "type-param-attr-spec" #734 :
    "is KIND"
    "or LEN",
)]
pub fn type_param_attr_spec<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = TypeParamAttrSpec> + 'a {
    alt!(
        kw!(KIND).map(|_| TypeParamAttrSpec::Kind),
        kw!(LEN).map(|_| TypeParamAttrSpec::Len),
    )
}

#[derive(Debug, Clone)]
pub struct TypeParamDecl<Span> {
    pub name: Name<Span>,
    pub init: Option<IntConstantExpr<Span>>,
}

// TODO test
#[syntax_rule(
    F18V007r1 rule "type-param-decl" #733 : "is type-param-name [ = scalar-int-constant-expr ]",
)]
pub fn type_param_decl<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = TypeParamDecl<MultilineSpan>> + 'a {
    (
        name(),
        (
            equals(),
            int_constant_expr(cfg),
        ).map(|(_, expr)| expr).optional(),
    ).map(|(name, init)| TypeParamDecl {
        name,
        init,
    })
}

#[derive(Debug, Clone)]
pub struct TypeParamDefStmt<Span> {
    pub type_spec: IntegerTypeSpec<Span>,
    pub attr: TypeParamAttrSpec,
    pub decls: Vec<TypeParamDecl<Span>>,
}

// TODO test
#[syntax_rule(
    F18V007r1 rule "type-param-def-stmt" #732 : 
    "is integer-type-spec, type-param-attr-spec :: type-param-decl-list",
)]
pub fn type_param_def_stmt<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = TypeParamDefStmt<MultilineSpan>> + 'a {
    (
        integer_type_spec(cfg),
        comma(),
        type_param_attr_spec(cfg),
        double_colon(),
        separated(
            type_param_decl(cfg),
            comma(),
            1..,
        ),
    ).map(|(type_spec, _, attr, _, decls)| TypeParamDefStmt {
        type_spec,
        attr,
        decls,
    })
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ComponentArraySpec<Span> {
    Explicit(ExplicitShapeSpec<Span>),
    Deferred(DeferredShapeSpec),
}

// TODO test
#[syntax_rule(
    F18V007r1 rule "component-array-spec" #740 :
    "is explicit-shape-spec-list"
    "or deferred-shape-spec-list",
)]
pub fn component_array_spec<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ComponentArraySpec<MultilineSpan>> + 'a {
    alt!(
        explicit_shape_spec(cfg).map(ComponentArraySpec::Explicit),
        deferred_shape_spec(cfg).map(ComponentArraySpec::Deferred),
    )
}

#[derive(Debug, Clone)]
pub struct ComponentDecl<Span> {
    pub name: Name<Span>,
    pub array_spec: Option<ComponentArraySpec<Span>>,
    pub coarray_spec: Option<CoarraySpec<Span>>,
    pub char_length: Option<CharLength<Span>>,
    pub component_initialization: Option<ComponentInitialization<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "component-decl" #739 :
    "is component-name [ ( component-array-spec ) ] [ lbracket coarray-spec rbracket ] [ * char-length ] [ component-initialization ]",
)]
pub fn component_decl<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ComponentDecl<MultilineSpan>> + 'a {
    // TODO test
    (
        name(),
        (
            delim('('),
            component_array_spec(cfg),
            delim(')'),
        ).map(|(_, a, _)| a).optional(),
        (
            delim('['),
            coarray_spec(cfg),
            delim(']'),
        ).map(|(_, a, _)| a).optional(),
        (
            asterisk(),
            char_length(cfg),
        ).map(|(_, a)| a).optional(),
        component_initialization(cfg).optional(),
    ).map(|(name, array_spec, coarray_spec, char_length, component_initialization)| ComponentDecl {
        name,
        array_spec,
        coarray_spec,
        char_length,
        component_initialization,
    })
}

#[derive(Debug, Clone)]
pub struct InitialDataTarget<Span>(pub Designator<Span>);// TODO

#[syntax_rule(
    F18V007r1 rule "initial-data-target" #744 : "is designator",
)]
pub fn initial_data_target<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = InitialDataTarget<MultilineSpan>> + 'a {
    // TODO test
    designator(cfg, false).map(InitialDataTarget)
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

#[syntax_rule(
    F18V007r1 rule "component-attr-spec" #738 :
    "is access-spec"
    "or ALLOCATABLE"
    "or CODIMENSION lbracket coarray-spec rbracket"
    "or CONTIGUOUS"
    "or DIMENSION ( component-array-spec )"
    "or POINTER",
)]

pub fn component_attr_spec<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ComponentAttrSpec<MultilineSpan>> + 'a {
    // TODO test and check, written by copilot, check order
    alt!(
        access_spec(cfg).map(ComponentAttrSpec::Access),
        kw!(ALLOCATABLE).map(|_| ComponentAttrSpec::Allocatable),
        (
            kw!(CODIMENSION),
            coarray_spec(cfg),
        ).map(|(_, a)| ComponentAttrSpec::Codimension(a)),
        kw!(CONTIGUOUS).map(|_| ComponentAttrSpec::Contiguous),
        (
            kw!(DIMENSION),
            component_array_spec(cfg),
        ).map(|(_, a)| ComponentAttrSpec::Dimension(a)),
        kw!(POINTER).map(|_| ComponentAttrSpec::Pointer),
    )
}

#[derive(Debug, Clone)]
pub struct DataComponentDefStmt<Span> {
    pub type_spec: DeclarationTypeSpec<Span>,
    pub attrs: Vec<ComponentAttrSpec<Span>>,
    pub component_decls: Vec<ComponentDecl<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "data-component-def-stmt" #737 :
    "is declaration-type-spec [ [ , component-attr-spec-list ] :: ] component-decl-list",
)]
pub fn data_component_def_stmt<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DataComponentDefStmt<MultilineSpan>> + 'a {
    // TODO test
    (
        declaration_type_spec(cfg),
        (
            comma(),
            separated(
                component_attr_spec(cfg),
                comma(),
                0..,
            ),
            double_colon(),
        ).map(|(_, attrs, _)| attrs).optional().map(|attrs| attrs.unwrap_or(vec![])),
        separated(
            component_decl(cfg),
            comma(),
            1..,
        ),
    ).map(|(type_spec, attrs, component_decls)| DataComponentDefStmt {
        type_spec,
        attrs,
        component_decls,
    })
}

// TODO construct
//#[derive(Debug, Clone)]
//pub struct ComponentPart<Span>(pub Vec<MaybeStatement<ComponentDefStmt<Span>, Span>>);
//
//#[syntax_rule(
//    F18V007r1 rule "component-part" #735 : "is [ component-def-stmt ] ...",
//)]
//pub fn component_part<'a, S: Lexed + 'a, U: 'a>(
//    cfg: &'a Cfg,
//    until: impl Parser<S, Token = U> + 'a,
//) -> impl Parser<S, Token = (ComponentPart<S::Span>, Option<U>)> + 'a {
//    // TODO test
//    many_until(
//        maybe_statement(component_def_stmt(cfg)),
//        until,
//        0..
//    ).map(|(statements, until)| (ComponentPart(statements), until))
//}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ComponentDefStmt<Span> {
    Data(DataComponentDefStmt<Span>),
    Proc(ProcComponentDefStmt<Span>),
}

#[syntax_rule(
    F18V007r1 rule "component-def-stmt" #736 :
    "is data-component-def-stmt"
    "or proc-component-def-stmt",
)]
pub fn component_def_stmt<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ComponentDefStmt<MultilineSpan>> + 'a {
    // TODO test
    alt!(
        data_component_def_stmt(cfg).map(ComponentDefStmt::Data),
        proc_component_def_stmt(cfg).map(ComponentDefStmt::Proc),
    )
}

#[derive(Debug, Clone)]
pub struct ProcComponentDefStmt<Span>(std::marker::PhantomData<Span>);// TODO

#[syntax_rule(
    F18V007r1 rule "proc-component-def-stmt" #741 :
    "is PROCEDURE ( [ proc-interface ] ) , proc-component-attr-spec-list :: proc-decl-list",
)]
pub fn proc_component_def_stmt<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ProcComponentDefStmt<MultilineSpan>> + 'a {
    |_| todo!("TODO: parser not implemented yet")
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ProcComponentAttrSpec<Span> {
    AccessSpec(AccessSpec),
    Nopass,
    Pass(Name<Span>),
    Pointer,
}

#[syntax_rule(
    F18V007r1 rule "proc-component-attr-spec" #742 :
    "is access-spec"
    "or NOPASS"
    "or PASS [ (arg-name) ]"
    "or POINTER",
)]
pub fn proc_component_attr_spec<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ProcComponentAttrSpec<MultilineSpan>> + 'a {
    // TODO test
    alt!(
        access_spec(cfg).map(ProcComponentAttrSpec::AccessSpec),
        kw!(NOPASS).map(|_| ProcComponentAttrSpec::Nopass),
        (
            kw!(PASS),
            delim('('),
            name(),
            delim(')'),
        ).map(|(_, _, name, _)| ProcComponentAttrSpec::Pass(name)),
        kw!(POINTER).map(|_| ProcComponentAttrSpec::Pointer),
    )
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum TypeSpec<Span> {
    Intrinsic, // TODO
    Derived, // TODO
    _Phantom(Span),
}

#[syntax_rule(
    F18V007r1 rule "type-spec" #702 :
    "is intrinsic-type-spec"
    "or derived-type-spec",
)]
pub fn type_spec<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = TypeSpec<MultilineSpan>> + 'a {
    |_| todo!("TODO: parser not implemented yet")
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

#[syntax_rule(
    F18V007r1 rule "declaration-type-spec" #703 :
    "is intrinsic-type-spec"
    "or TYPE ( intrinsic-type-spec )"
    "or TYPE ( derived-type-spec )"
    "or CLASS ( derived-type-spec )"
    "or CLASS ( * )"
    "or TYPE ( * )",
)]
pub fn declaration_type_spec<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DeclarationTypeSpec<MultilineSpan>> + 'a {
    alt!(
        intrinsic_type_spec(cfg).map(|_| DeclarationTypeSpec::Intrinsic),
        (
            kw!(TYPE),
            delim('('),
            intrinsic_type_spec(cfg),
            delim(')'),
        ).map(|(_, _, _, _)| DeclarationTypeSpec::TypeIntrinsic),
        (
            kw!(TYPE),
            delim('('),
            derived_type_spec(cfg),
            delim(')'),
        ).map(|(_, _, _, _)| DeclarationTypeSpec::TypeDerived),
        (
            kw!(CLASS),
            delim('('),
            derived_type_spec(cfg),
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
    )
}

#[derive(Debug, Clone)]
pub struct TypeBoundProcedurePart<Span>(std::marker::PhantomData<Span>);// TODO

#[syntax_rule(
    F18V007r1 rule "type-bound-procedure-part" #746 :
    "is contains-stmt"
    "    [ binding-private-stmt ]"
    "    [ type-bound-proc-binding ] ...",
)]
pub fn type_bound_procedure_part<'a, S: Lexed + 'a, U: 'a>(
    cfg: &'a Cfg,
    _until: impl Parser<S, Token = U> + 'a,
) -> impl Parser<S, Token = (TypeBoundProcedurePart<S::Span>, Option<U>)> + 'a {
    |_| todo!("TODO: parser not implemented yet")
}

#[derive(Debug, Clone)]
pub struct BindingPrivateStmt<Span> {
    pub keyword: Keyword<Span>,
}

#[syntax_rule(
    F18V007r1 rule "binding-private-stmt" #747 :
    "is PRIVATE",
)]
pub fn binding_private_stmt<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = BindingPrivateStmt<MultilineSpan>> + 'a {
    kw!(PRIVATE).map(|keyword| BindingPrivateStmt {
        keyword,
    })
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum TypeBoundProcBinding<Span> {
    TypeBoundProcedureStmt(TypeBoundProcedureStmt<Span>),
    TypeBoundGenericStmt(TypeBoundGenericStmt<Span>),
    FinalProcedureStmt(FinalProcedureStmt<Span>),
}

#[syntax_rule(
    F18V007r1 rule "type-bound-proc-binding" #748 :
    "is type-bound-procedure-stmt"
    "or type-bound-generic-stmt"
    "or final-procedure-stmt",
)]
pub fn type_bound_proc_binding<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = TypeBoundProcBinding<MultilineSpan>> + 'a {
    alt!(
        type_bound_procedure_stmt(cfg).map(TypeBoundProcBinding::TypeBoundProcedureStmt),
        type_bound_generic_stmt(cfg).map(TypeBoundProcBinding::TypeBoundGenericStmt),
        final_procedure_stmt(cfg).map(TypeBoundProcBinding::FinalProcedureStmt),
    )
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum TypeBoundProcedureStmt<Span> {
    Form1(Option<Vec<BindingAttr<Span>>>, Vec<TypeBoundProcDecl<Span>>),
    Form2(Name<Span>, Vec<BindingAttr<Span>>, Vec<Name<Span>>),
}

#[syntax_rule(
    F18V007r1 rule "type-bound-procedure-stmt" #749 :
    "is PROCEDURE [ [ , binding-attr-list ] :: ] type-bound-proc-decl-list"
    "or PROCEDURE (interface-name), binding-attr-list :: binding-name-list",
)]
pub fn type_bound_procedure_stmt<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = TypeBoundProcedureStmt<MultilineSpan>> + 'a {
    let form_1 = (
        kw!(PROCEDURE),
        (
            comma(),
            list(binding_attr(cfg), 0..),
            double_colon(),
        ).map(|(_, attrs, _)| attrs).optional(),
        list(type_bound_proc_decl(cfg), 0..),
    ).map(|(_, attrs, decls)| TypeBoundProcedureStmt::Form1(attrs, decls));

    let form_2 = (
        kw!(PROCEDURE),
        delim('('),
        name(),
        (delim(')'), comma()),
        list(binding_attr(cfg), 0..),
        double_colon(),
        list(name(), 0..),
    ).map(|(_, _, interface_name, _, attrs, _, names)| TypeBoundProcedureStmt::Form2(interface_name, attrs, names));

    alt!(
        form_1,
        form_2,
    )
}

#[derive(Debug, Clone)]
pub enum BindingAttr<Span> {
    AccessSpec(AccessSpec),
    Deferred,
    NonOverridable,
    Nopass,
    Pass(Option<Name<Span>>),
}

#[syntax_rule(
    F18V007r1 rule "binding-attr" #752 :
    "is access-spec"
    "or DEFERRED"
    "or NON_OVERRIDABLE"
    "or NOPASS"
    "or PASS [ (arg-name) ]",
)]
pub fn binding_attr<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = BindingAttr<MultilineSpan>> + 'a {
    alt!(
        access_spec(cfg).map(BindingAttr::AccessSpec),
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
    )
}

#[derive(Debug, Clone)]
pub struct FinalProcedureStmt<Span> {
    pub final_subroutine_name_list: Vec<Name<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "final-procedure-stmt" #753 : "is FINAL [ :: ] final-subroutine-name-list",
)]
pub fn final_procedure_stmt<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = FinalProcedureStmt<MultilineSpan>> + 'a {
    (
        kw!(FINAL),
        double_colon(),
        list(name(), 1..),
    ).map(|(_, _, names)| FinalProcedureStmt {
        final_subroutine_name_list: names,
    })
}

#[derive(Debug, Clone)]
pub struct TypeBoundProcDecl<Span> {
    pub binding_name: Name<Span>,
    pub procedure_name: Option<Name<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "type-bound-proc-decl" #750 : "is binding-name [ => procedure-name ]",
)]
pub fn type_bound_proc_decl<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = TypeBoundProcDecl<MultilineSpan>> + 'a {
    (
        name(),
        (
            arrow(),
            name(),
        ).map(|(_, name)| name).optional(),
    ).map(|(binding_name, procedure_name)| TypeBoundProcDecl {
        binding_name,
        procedure_name,
    })
}

#[derive(Debug, Clone)]
pub struct EndTypeStmt<Span>(std::marker::PhantomData<Span>);// TODO

#[syntax_rule(
    F18V007r1 rule "end-type-stmt" #730 : "is END TYPE [ type-name ]",
)]
pub fn end_type_stmt<'a, S: Lexed + 'a>(
    cfg: &'a Cfg,
    type_name: impl AsRef<str>,
) -> impl Parser<S, Token = EndTypeStmt<MultilineSpan>> + 'a {
    let type_name = type_name.as_ref().to_string();

    (
        kw!(END),
        kw!(TYPE),
        name().optional(),
    ).condition(move |(_, _, parsed_name), _| {
        if let Some(parsed_name) = parsed_name {
            parsed_name.0.value() == &type_name
        } else {
            true
        }
    }).map(|(_, _, _name)| EndTypeStmt(std::marker::PhantomData))
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

#[syntax_rule(
    F18V007r1 rule "type-bound-generic-stmt" #751 : "is GENERIC [ , access-spec ] :: generic-spec => binding-name-list",
)]
pub fn type_bound_generic_stmt<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = TypeBoundGenericStmt<MultilineSpan>> + 'a {
    (
        kw!(GENERIC),
        (
            comma(),
            access_spec(cfg),
        ).map(|(_, a)| a).optional(),
        double_colon(),
        generic_spec(cfg),
        arrow(),
        list(name(), 0..),
    ).map(|(_, access_spec, _, generic_spec, _, binding_name_list)| TypeBoundGenericStmt {
        access_spec,
        generic_spec,
        binding_name_list,
    })
}

#[syntax_rule(
    F18V007r1 rule "derived-type-spec" #754 : "is type-name [ ( type-param-spec-list ) ]",
)]
pub fn derived_type_spec<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DerivedTypeSpec<MultilineSpan>> + 'a {
    // TODO test
    (
        name(),
        (
            delim('('),
            separated(
                type_param_spec(cfg),
                comma(),
                0..,
            ),
            delim(')'),
        ).map(|(_, specs, _)| specs).optional(),
    ).map(|(name, type_param_specifiers)| DerivedTypeSpec {
        name,
        type_param_specifiers: type_param_specifiers,
    })
}

#[derive(Debug, Clone)]
pub struct TypeParamSpec<Span> {
    pub keyword: Option<Name<Span>>,
    pub value: TypeParamValue<Span>,
}

#[syntax_rule(
    F18V007r1 rule "type-param-spec" #755 : "is [ keyword = ] type-param-value",
)]
pub fn type_param_spec<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = TypeParamSpec<MultilineSpan>> + 'a {
    // TODO test
    (
        (
            name(), // TODO keyword
            equals(),
        ).map(|(k, _)| k).optional(),
        type_param_value(cfg),
    ).map(|(keyword, value)| TypeParamSpec {
        keyword,
        value,
    })
}

#[derive(Debug, Clone)]
pub struct StructureConstructor<Span> {
    pub type_spec: DerivedTypeSpec<Span>,
    pub component_spec: Vec<ComponentSpec<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "structure-constructor" #756 : "is derived-type-spec ( [ component-spec-list ] )",
)]
pub fn structure_constructor<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = StructureConstructor<MultilineSpan>> + 'a {
    // TODO test
    (
        derived_type_spec(cfg),
        delim('('),
        separated(
            component_spec(cfg),
            comma(),
            0..,
        ),
        delim(')'),
    ).map(|(type_spec, _, component_spec, _)| StructureConstructor {
        type_spec,
        component_spec,
    })
}

#[derive(Debug, Clone)]
pub struct ComponentSpec<Span> {
    pub keyword: Option<Keyword<Span>>,
    pub value: ComponentDataSource<Span>,
}

#[syntax_rule(
    F18V007r1 rule "component-spec" #757 : "is [ keyword = ] component-data-source",
)]
pub fn component_spec<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ComponentSpec<MultilineSpan>> + 'a {
    // TODO test
    (
        (
            name_as_keyword(),
            equals(),
        ).map(|(k, _)| k).optional(),
        component_data_source(cfg),
    ).map(|(keyword, value)| ComponentSpec {
        keyword,
        value,
    })
}

#[derive(Debug, Clone)]
pub struct ComponentDataSource<Span>(PhantomData<Span>); // TODO

#[syntax_rule(
    F18V007r1 rule "component-data-source" #758 :
    "is expr"
    "or data-target"
    "or proc-target",
)]
pub fn component_data_source<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ComponentDataSource<MultilineSpan>> + 'a {
    |_| todo!("TODO: \"component_data_source\" parser not implemented yet")
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
use std::marker::PhantomData;

use crate::cfg;

use super::*;

#[derive(Debug, Clone)]
pub struct DerivedTypeDef<Span> {
    pub decl_stmt: DerivedTypeStmt<Span>,
    pub type_param_def_stmts: Vec<MaybeStatement<TypeParamDefStmt<Span>, Span>>,
    pub priv_or_seq: Vec<MaybeStatement<PrivateOrSequence<Span>, Span>>,
    pub component_part: Option<ComponentPart<Span>>,
    pub type_bound_procedure_part: Option<TypeBoundProcedurePart<Span>>,
    pub end_type_stmt: Option<EndTypeStmt<Span>>,
}

// TODO test
#[syntax_rule(
    F18V007r1 rule "derived-type-def" #726,
)]
pub fn derived_type_def<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DerivedTypeDef<S::Span>> + 'a {
    // TODO test
    move |source: S| {
        let orig_source = source.clone();

        let (s, source) = derived_type_stmt(cfg)
            .parse(source)
            .map_err(|_| orig_source.clone())?;

        let name = s.name.0.value();

        let ((type_param_def_stmts, end), source) = many_until(
            maybe_statement(type_param_def_stmt(cfg)),
            end_type_stmt(cfg, name),
            0..,
        )
            .parse(source)
            .map_err(|_| orig_source.clone())?;

        if let Some(end) = end {
            return Ok((DerivedTypeDef {
                decl_stmt: s,
                type_param_def_stmts,
                priv_or_seq: vec![],
                component_part: None,
                type_bound_procedure_part: None,
                end_type_stmt: Some(end),
            }, source));
        }

        let ((priv_or_seq, end), source) = many_until(
            maybe_statement(private_or_sequence(cfg)),
            end_type_stmt(cfg, name),
            0..,
        )
            .parse(source)
            .map_err(|_| orig_source.clone())?;

        if let Some(end) = end {
            return Ok((DerivedTypeDef {
                decl_stmt: s,
                type_param_def_stmts,
                priv_or_seq,
                component_part: None,
                type_bound_procedure_part: None,
                end_type_stmt: Some(end),
            }, source));
        }

        let ((component_part, end), source) = component_part(cfg, end_type_stmt(cfg, name))
            .parse(source)
            .map_err(|_| orig_source.clone())?;

        if let Some(end) = end {
            return Ok((DerivedTypeDef {
                decl_stmt: s,
                type_param_def_stmts,
                priv_or_seq,
                component_part: Some(component_part),
                type_bound_procedure_part: None,
                end_type_stmt: Some(end),
            }, source));
        }

        let ((type_bound_procedure_part, end), source) = type_bound_procedure_part(cfg, end_type_stmt(cfg, name))
            .parse(source)
            .map_err(|_| orig_source.clone())?;

        if let Some(end) = end {
            return Ok((DerivedTypeDef {
                decl_stmt: s,
                type_param_def_stmts,
                priv_or_seq,
                component_part: Some(component_part),
                type_bound_procedure_part: Some(type_bound_procedure_part),
                end_type_stmt: Some(end),
            }, source));
        }

        // TODO maybe many unclassified or comment...

        let (end, source) = end_type_stmt(cfg, name)
            .map(|end| Some(end))
            .or(eof().map(|_| None))
            .parse(source)
            .map_err(|_| orig_source.clone())?;

        Ok((DerivedTypeDef {
            decl_stmt: s,
            type_param_def_stmts,
            priv_or_seq,
            component_part: Some(component_part),
            type_bound_procedure_part: Some(type_bound_procedure_part),
            end_type_stmt: end,
        }, source))
    }
}

#[derive(Debug, Clone)]
pub struct DerivedTypeStmt<Span> {
    pub name: Name<Span>,
    pub attributes: Vec<TypeAttrSpec<Span>>,
    pub type_param_names: Vec<Name<Span>>,
}

// TODO test
#[syntax_rule(
    F18V007r1 rule "derived-type-stmt" #727,
)]
pub fn derived_type_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DerivedTypeStmt<S::Span>> + 'a {
    let type_attr_spec_list = || (
        ',',
        space(0),
        separated(
            (
                space(0),
                type_attr_spec(cfg),
                space(0)
            ).map(|(_, attr, _)| attr),
            ',',
            0..,
        ),
    ).map(|(_, _, l)| l);

    let type_param_name_list = || separated(
        (
            space(0),
            name(cfg, false),
            space(0)
        ).map(|(_, name, _)| name),
        ',',
        0..,
    );

    (
        StringMatch::exact("type", false),
        (
            space(0),
            type_attr_spec_list(),
            space(0),
            "::",
            space(0)
        )
            .map(|(_, spec_list, _, _, _)| spec_list)
            .optional()
            .map(|spec_list| spec_list.unwrap_or(vec![])),
        name(cfg, false),
        (
            space(0),
            '(',
            space(0),
            type_param_name_list(),
            space(0),
            ')',
        )
            .map(|(_, _, _, names, _, _)| names)
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
    F18V007r1 rule "type-attr-spec" #728,
)]
pub fn type_attr_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = TypeAttrSpec<S::Span>> + 'a {
    alt!(
        StringMatch::exact("abstract", false).map(|_| TypeAttrSpec::Abstract),
        access_spec(cfg).map(TypeAttrSpec::Access),
        (
            StringMatch::exact("bind", false),
            space(0),
            '(',
            space(0),
            StringMatch::exact("c", false),
            space(0),
            ')',
        ).map(|_| TypeAttrSpec::BindC),
        (
            StringMatch::exact("extends", false),
            space(1),
            name(cfg, false),
        ).map(|(_, _, name)| TypeAttrSpec::Extends(name)),
    )
}

#[derive(Debug, Clone, EnumAsInner, PartialEq, Eq, Hash)]
pub enum TypeParamAttrSpec {
    Kind,
    Len,
}

// TODO test
#[syntax_rule(
    F18V007r1 rule "type-param-attr-spec" #734,
)]
pub fn type_param_attr_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = TypeParamAttrSpec> + 'a {
    alt!(
        StringMatch::exact("kind", false).map(|_| TypeParamAttrSpec::Kind),
        StringMatch::exact("len", false).map(|_| TypeParamAttrSpec::Len),
    )
}

#[derive(Debug, Clone)]
pub struct TypeParamDecl<Span> {
    pub name: Name<Span>,
    pub init: Option<ScalarIntConstantExpr<Span>>,
}

// TODO test
#[syntax_rule(
    F18V007r1 rule "type-param-decl" #733,
)]
pub fn type_param_decl<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = TypeParamDecl<S::Span>> + 'a {
    (
        name(cfg, false),
        (
            space(0), '=', space(0),
            scalar_int_constant_expr(cfg),
        ).map(|(_, _, _, expr)| expr).optional(),
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
    pub comment: Option<LineComment<Span>>,
}

// TODO test
#[syntax_rule(
    F18V007r1 rule "type-param-def-stmt" #732,
)]
pub fn type_param_def_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = TypeParamDefStmt<S::Span>> + 'a {
    (
        space(0),
        integer_type_spec(cfg),
        space(0),
        type_param_attr_spec(cfg),
        (space(0), "::", space(0)),
        separated(
            (space(0), type_param_decl(cfg), space(0)).map(|(_, decl, _)| decl),
            ',',
            1..,
        ),
        statement_termination(),
    ).map(|(_, type_spec, _, attr, _, decls, comment)| TypeParamDefStmt {
        type_spec,
        attr,
        decls,
        comment,
    })
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ComponentArraySpec<Span> {
    Explicit(ExplicitShapeSpec<Span>),
    Deferred(DeferredShapeSpec),
}

// TODO test
#[syntax_rule(
    F18V007r1 rule "component-array-spec" #740,
)]
pub fn component_array_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ComponentArraySpec<S::Span>> + 'a {
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
    F18V007r1 rule "component-decl" #739,
)]
pub fn component_decl<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ComponentDecl<S::Span>> + 'a {
    // TODO test
    (
        name(cfg, false), space(0),
        (
            '(',
            space(0),
            component_array_spec(cfg),
            space(0),
            ')',
        ).map(|(_, _, a, _, _)| a).optional(),
        (
            lbracket(cfg),
            space(0),
            coarray_spec(cfg),
            space(0),
            rbracket(cfg),
        ).map(|(_, _, a, _, _)| a).optional(),
        (
            '*',
            space(0),
            char_length(cfg),
        ).map(|(_, _, a)| a).optional(),
        (
            space(0),
            component_initialization(cfg)
        ).map(|(_, a)| a).optional(),
    ).map(|(name, _, array_spec, coarray_spec, char_length, component_initialization)| ComponentDecl {
        name,
        array_spec,
        coarray_spec,
        char_length,
        component_initialization,
    })
}

#[derive(Debug, Clone)]
pub struct InitialDataTarget<Span>(std::marker::PhantomData<Span>);// TODO

#[syntax_rule(
    F18V007r1 rule "initial-data-target" #744,
)]
pub fn initial_data_target<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = InitialDataTarget<S::Span>> + 'a {
    |_| todo!("TODO: parser not implemented yet")
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
    F18V007r1 rule "component-attr-spec" #738,
)]

pub fn component_attr_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ComponentAttrSpec<S::Span>> + 'a {
    // TODO test and check, written by copilot, check order
    alt!(
        access_spec(cfg).map(ComponentAttrSpec::Access),
        StringMatch::exact("allocatable", false).map(|_| ComponentAttrSpec::Allocatable),
        (
            StringMatch::exact("codimension", false),
            space(0),
            coarray_spec(cfg),
        ).map(|(_, _, a)| ComponentAttrSpec::Codimension(a)),
        StringMatch::exact("contiguous", false).map(|_| ComponentAttrSpec::Contiguous),
        (
            StringMatch::exact("dimension", false),
            space(0),
            component_array_spec(cfg),
        ).map(|(_, _, a)| ComponentAttrSpec::Dimension(a)),
        StringMatch::exact("pointer", false).map(|_| ComponentAttrSpec::Pointer),
    )
}

#[derive(Debug, Clone)]
pub struct DataComponentDefStmt<Span> {
    pub type_spec: DeclarationTypeSpec<Span>,
    pub attrs: Vec<ComponentAttrSpec<Span>>,
    pub component_decls: Vec<ComponentDecl<Span>>,
    pub comment: Option<LineComment<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "data-component-def-stmt" #737,
)]
pub fn data_component_def_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DataComponentDefStmt<S::Span>> + 'a {
    // TODO test
    (
        declaration_type_spec(cfg),
        space(0),
        (
            ',', space(0),
            separated(
                (
                    space(0),
                    component_attr_spec(cfg),
                    space(0),
                ).map(|(_, attr, _)| attr),
                ',',
                0..,
            ),
            space(0), "::", space(0),
        ).map(|(_, _, attrs, _, _, _)| attrs).optional().map(|attrs| attrs.unwrap_or(vec![])),
        separated(
            (
                space(0),
                component_decl(cfg),
                space(0),
            ).map(|(_, decl, _)| decl),
            ',',
            1..,
        ),
        statement_termination(),
    ).map(|(type_spec, _, attrs, component_decls, comment)| DataComponentDefStmt {
        type_spec,
        attrs,
        component_decls,
        comment,
    })
}

#[derive(Debug, Clone)]
pub struct ComponentPart<Span>(pub Vec<MaybeStatement<ComponentDefStmt<Span>, Span>>);

#[syntax_rule(
    F18V007r1 rule "component-part" #735,
)]
pub fn component_part<'a, S: TextSource + 'a, U: 'a>(
    cfg: &'a Cfg,
    until: impl Parser<S, Token = U> + 'a,
) -> impl Parser<S, Token = (ComponentPart<S::Span>, Option<U>)> + 'a {
    // TODO test
    many_until(
        maybe_statement(component_def_stmt(cfg)),
        until,
        0..
    ).map(|(statements, until)| (ComponentPart(statements), until))
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ComponentDefStmt<Span> {
    Data(DataComponentDefStmt<Span>),
    Proc(ProcComponentDefStmt<Span>),
}

#[syntax_rule(
    F18V007r1 rule "component-def-stmt" #736,
)]
pub fn component_def_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ComponentDefStmt<S::Span>> + 'a {
    // TODO test
    alt!(
        data_component_def_stmt(cfg).map(ComponentDefStmt::Data),
        proc_component_def_stmt(cfg).map(ComponentDefStmt::Proc),
    )
}

#[derive(Debug, Clone)]
pub struct ProcComponentDefStmt<Span>(std::marker::PhantomData<Span>);// TODO

#[syntax_rule(
    F18V007r1 rule "proc-component-def-stmt" #741,
)]
pub fn proc_component_def_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ProcComponentDefStmt<S::Span>> + 'a {
    |_| todo!("TODO: parser not implemented yet")
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ProcComponentAttrSpec<Span> {
    AccessSpec(AccessSpec),
    Nopass,
    Pass(ArgName<Span>),
    Pointer,
}

#[syntax_rule(
    F18V007r1 rule "proc-component-attr-spec" #742,
)]
pub fn proc_component_attr_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ProcComponentAttrSpec<S::Span>> + 'a {
    // TODO test
    alt!(
        access_spec(cfg).map(ProcComponentAttrSpec::AccessSpec),
        StringMatch::exact("nopass", false).map(|_| ProcComponentAttrSpec::Nopass),
        (
            StringMatch::exact("pass", false),
            (space(0), '(', space(0)),
            arg_name(cfg),
            (space(0), ')', space(0)),
        ).map(|(_, _, name, _)| ProcComponentAttrSpec::Pass(name)),
        StringMatch::exact("pointer", false).map(|_| ProcComponentAttrSpec::Pointer),
    )
}

#[derive(Debug, Clone)]
pub struct ArgName<Span>(pub Name<Span>);

#[syntax_rule(
    F18V007r1 rule "arg-name",
)]
pub fn arg_name<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ArgName<S::Span>> + 'a {
    name(cfg, false).map(ArgName)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum TypeSpec<Span> {
    Intrinsic, // TODO
    Derived, // TODO
    _Phantom(Span),
}

#[syntax_rule(
    F18V007r1 rule "type-spec" #702,
)]
pub fn type_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = TypeSpec<S::Span>> + 'a {
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
    F18V007r1 rule "declaration-type-spec" #703,
)]
pub fn declaration_type_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DeclarationTypeSpec<S::Span>> + 'a {
    |_| todo!("TODO: parser not implemented yet")
}

#[derive(Debug, Clone)]
pub struct TypeBoundProcedurePart<Span>(std::marker::PhantomData<Span>);// TODO

#[syntax_rule(
    F18V007r1 rule "type-bound-procedure-part" #746,
)]
pub fn type_bound_procedure_part<'a, S: TextSource + 'a, U: 'a>(
    cfg: &'a Cfg,
    _until: impl Parser<S, Token = U> + 'a,
) -> impl Parser<S, Token = (TypeBoundProcedurePart<S::Span>, Option<U>)> + 'a {
    |_| todo!("TODO: parser not implemented yet")
}

#[derive(Debug, Clone)]
pub struct BindingPrivateStmt<Span> {
    pub keyword: Keyword<Span>,
    pub comment: Option<LineComment<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "binding-private-stmt" #747,
)]
pub fn binding_private_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = BindingPrivateStmt<S::Span>> + 'a {
    (
        space(0), kw("private", cfg),
        statement_termination(),
    ).map(|(_, keyword, comment)| BindingPrivateStmt {
        keyword,
        comment,
    })
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum TypeBoundProcBinding<Span> {
    TypeBoundProcedureStmt(TypeBoundProcedureStmt<Span>),
    TypeBoundGenericStmt(TypeBoundGenericStmt<Span>),
    FinalProcedureStmt(FinalProcedureStmt<Span>),
}

#[syntax_rule(
    F18V007r1 rule "type-bound-proc-binding" #748,
)]
pub fn type_bound_proc_binding<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = TypeBoundProcBinding<S::Span>> + 'a {
    alt!(
        type_bound_procedure_stmt(cfg).map(TypeBoundProcBinding::TypeBoundProcedureStmt),
        type_bound_generic_stmt(cfg).map(TypeBoundProcBinding::TypeBoundGenericStmt),
        final_procedure_stmt(cfg).map(TypeBoundProcBinding::FinalProcedureStmt),
    )
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum TypeBoundProcedureStmt<Span> {
    Form1(Option<Vec<BindingAttr<Span>>>, Vec<TypeBoundProcDecl<Span>>, Option<LineComment<Span>>),
    Form2(InterfaceName<Span>, Vec<BindingAttr<Span>>, Vec<BindingName<Span>>, Option<LineComment<Span>>),
}

#[syntax_rule(
    F18V007r1 rule "type-bound-procedure-stmt" #749,
)]
pub fn type_bound_procedure_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = TypeBoundProcedureStmt<S::Span>> + 'a {
    let form_1 = (
        space(0), kw("procedure", cfg), space(0),
        (
            (',', space(0)),
            list(binding_attr(cfg), 0..),
            (space(0), "::", space(0)),
        ).map(|(_, attrs, _)| attrs).optional(),
        list(type_bound_proc_decl(cfg), 0..),
        statement_termination(),
    ).map(|(_, _, _, attrs, decls, comment)| TypeBoundProcedureStmt::Form1(attrs, decls, comment));

    let form_2 = (
        kw("procedure", cfg),
        (space(0), '(', space(0)),
        interface_name(cfg),
        (space(0), ')', space(0), ',', space(0)),
        list(binding_attr(cfg), 0..),
        (space(0), "::", space(0)),
        list(binding_name(cfg), 0..),
        statement_termination(),
    ).map(|(_, _, interface_name, _, attrs, _, names, comment)| TypeBoundProcedureStmt::Form2(interface_name, attrs, names, comment));

    alt!(
        form_1,
        form_2,
    )
}

#[derive(Debug, Clone)]
pub struct InterfaceName<Span>(pub Name<Span>);

#[syntax_rule(
    F18V007r1 rule "interface-name",
)]
pub fn interface_name<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = InterfaceName<S::Span>> + 'a {
    name(cfg, false).map(InterfaceName)
}

#[derive(Debug, Clone)]
pub enum BindingAttr<Span> {
    AccessSpec(AccessSpec),
    Deferred,
    NonOverridable,
    Nopass,
    Pass(Option<ArgName<Span>>),
}

#[syntax_rule(
    F18V007r1 rule "binding-attr" #752,
)]
pub fn binding_attr<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = BindingAttr<S::Span>> + 'a {
    alt!(
        access_spec(cfg).map(BindingAttr::AccessSpec),
        kw("deferred", cfg).map(|_| BindingAttr::Deferred),
        kw("non_overridable", cfg).map(|_| BindingAttr::NonOverridable),
        kw("nopass", cfg).map(|_| BindingAttr::Nopass),
        (
            kw("pass", cfg),
            (
                (space(0), '(', space(0)),
                arg_name(cfg),
                (space(0), ')', space(0)),
            ).map(|(_, name, _)| name).optional(),
        ).map(|(_, name)| BindingAttr::Pass(name)),
    )
}

#[derive(Debug, Clone)]
pub struct FinalProcedureStmt<Span> {
    pub final_subroutine_name_list: Vec<FinalSubroutineName<Span>>,
    pub comment: Option<LineComment<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "final-procedure-stmt" #753,
)]
pub fn final_procedure_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = FinalProcedureStmt<S::Span>> + 'a {
    (
        (space(0), kw("final", cfg)),
        (space(0), "::", space(0)),
        list(final_subroutine_name(cfg), 1..),
        statement_termination(),
    ).map(|(_, _, names, comment)| FinalProcedureStmt {
        final_subroutine_name_list: names,
        comment,
    })
}

#[derive(Debug, Clone)]
pub struct FinalSubroutineName<Span>(pub Name<Span>);

#[syntax_rule(
    F18V007r1 rule "final-subroutine-name",
)]
pub fn final_subroutine_name<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = FinalSubroutineName<S::Span>> + 'a {
    name(cfg, false).map(FinalSubroutineName)
}

#[derive(Debug, Clone)]
pub struct TypeBoundProcDecl<Span> {
    pub binding_name: BindingName<Span>,
    pub procedure_name: Option<ProcedureName<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "type-bound-proc-decl" #750,
)]
pub fn type_bound_proc_decl<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = TypeBoundProcDecl<S::Span>> + 'a {
    (
        binding_name(cfg),
        (
            (space(0), "=>", space(0)),
            procedure_name(cfg),
        ).map(|(_, name)| name).optional(),
    ).map(|(binding_name, procedure_name)| TypeBoundProcDecl {
        binding_name,
        procedure_name,
    })
}

#[derive(Debug, Clone)]
pub struct EndTypeStmt<Span>(std::marker::PhantomData<Span>);// TODO

#[syntax_rule(
    F18V007r1 rule "end-type-stmt" #730,
)]
pub fn end_type_stmt<'a, S: TextSource + 'a>(
    cfg: &'a Cfg,
    type_name: impl AsRef<str>,
) -> impl Parser<S, Token = EndTypeStmt<S::Span>> + 'a {
    let type_name = type_name.as_ref().to_string();

    (
        space(0),
        StringMatch::exact("end", false),
        space(0),
        StringMatch::exact("type", false),
        (
            space(1),
            name(cfg, false),
        ).map(|(_, name)| name).optional(),
        statement_termination(),
    ).condition(move |(_, _, _, _, parsed_name, _), _| {
        if let Some(parsed_name) = parsed_name {
            parsed_name.0.value() == &type_name
        } else {
            true
        }
    }).map(|(_, _, _, _, _name, _comment)| EndTypeStmt(std::marker::PhantomData))
}

#[derive(Debug, Clone)]
pub struct TypeName<Span>(pub Name<Span>);

#[syntax_rule(
    F18V007r1 rule "type-name",
)]
pub fn type_name<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = TypeName<S::Span>> + 'a {
    name(cfg, false).map(TypeName)
}

#[derive(Debug, Clone)]
pub struct DerivedTypeSpec<Span> {
    pub name: TypeName<Span>,
    pub type_param_specifiers: Option<Vec<TypeParamSpec<Span>>>,
}

#[derive(Debug, Clone)]
pub struct TypeBoundGenericStmt<Span> {
    pub access_spec: Option<AccessSpec>,
    pub generic_spec: GenericSpec<Span>,
    pub binding_name_list: Vec<BindingName<Span>>,
    pub comment: Option<LineComment<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "type-bound-generic-stmt" #751,
)]
pub fn type_bound_generic_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = TypeBoundGenericStmt<S::Span>> + 'a {
    (
        space(0), kw("generic", cfg), space(0),
        (
            ',', space(0),
            access_spec(cfg), space(0),
        ).map(|(_, _, a, _)| a).optional(),
        "::", space(0),
        generic_spec(cfg),
        (space(0), "=>", space(0)),
        list(binding_name(cfg), 0..),
        statement_termination(),
    ).map(|(_, _, _, access_spec, _, _, generic_spec, _, binding_name_list, comment)| TypeBoundGenericStmt {
        access_spec,
        generic_spec,
        binding_name_list,
        comment,
    })
}

#[syntax_rule(
    F18V007r1 rule "derived-type-spec" #754,
)]
pub fn derived_type_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DerivedTypeSpec<S::Span>> + 'a {
    // TODO test
    (
        type_name(cfg),
        (
            space(0),
            '(',
            space(0),
            separated(
                (
                    space(0),
                    type_param_spec(cfg),
                    space(0),
                ).map(|(_, spec, _)| spec),
                ',',
                0..,
            ),
            space(0),
            ')',
        ).map(|(_, _, _, specs, _, _)| specs).optional(),
    ).map(|(name, type_param_specifiers)| DerivedTypeSpec {
        name,
        type_param_specifiers: type_param_specifiers,
    })
}

#[derive(Debug, Clone)]
pub struct TypeParamSpec<Span> {
    pub keyword: Option<Keyword<Span>>,
    pub value: TypeParamValue<Span>,
}

#[syntax_rule(
    F18V007r1 rule "type-param-spec" #755,
)]
pub fn type_param_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = TypeParamSpec<S::Span>> + 'a {
    // TODO test
    (
        (
            keyword(cfg),
            space(0), '=', space(0),
        ).map(|(k, _, _, _)| k).optional(),
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
    F18V007r1 rule "structure-constructor" #756,
)]
pub fn structure_constructor<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = StructureConstructor<S::Span>> + 'a {
    // TODO test
    (
        derived_type_spec(cfg),
        (space(0), '(', space(0)),
        separated(
            (
                space(0),
                component_spec(cfg),
                space(0),
            ).map(|(_, spec, _)| spec),
            ',',
            0..,
        ),
        (space(0), ')', space(0)),
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
    F18V007r1 rule "component-spec" #757,
)]
pub fn component_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ComponentSpec<S::Span>> + 'a {
    // TODO test
    (
        (
            keyword(cfg),
            space(0), '=', space(0),
        ).map(|(k, _, _, _)| k).optional(),
        component_data_source(cfg),
    ).map(|(keyword, value)| ComponentSpec {
        keyword,
        value,
    })
}

#[derive(Debug, Clone)]
pub struct ComponentDataSource<Span>(PhantomData<Span>); // TODO

#[syntax_rule(
    F18V007r1 rule "component-data-source" #758,
)]
pub fn component_data_source<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ComponentDataSource<S::Span>> + 'a {
    |_| todo!("TODO: \"component_data_source\" parser not implemented yet")
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_end_type_stmt() {
        for cfg in test_configs() {
            let parser = end_type_stmt(&cfg, "foo");
            assert_eq!(parser.parses("ENDTYPE"), true);
            assert_eq!(parser.parses("endtype"), true);
            assert_eq!(parser.parses("end type"), true);
            assert_eq!(parser.parses("end type foo"), true);
            assert_eq!(parser.parses("end type bar"), false);
        }
    }
}
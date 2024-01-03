use std::marker::PhantomData;

use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, EnumAsInner)]
pub enum AccessSpec {
    Public,
    Private,
}

// TODO test
#[syntax_rule(
    F18V007r1 rule "access-spec" #807 :
    "is PUBLIC"
    "or PRIVATE",
)]
pub fn access_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = AccessSpec> + 'a {
    alt!(
        StringMatch::exact("public", false).map(|_| AccessSpec::Public),
        StringMatch::exact("private", false).map(|_| AccessSpec::Private),
    )
}

#[derive(Debug, Clone)]
pub struct LowerBound<Span>(pub SpecificationExpr<Span>);

// TODO test
#[syntax_rule(
    F18V007r1 rule "lower-bound" #817 :
    "is specification-expr",
)]
pub fn lower_bound<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = LowerBound<S::Span>> + 'a {
    specification_expr(cfg).map(LowerBound)
}

#[derive(Debug, Clone)]
pub struct UpperBound<Span>(pub SpecificationExpr<Span>);

// TODO test
#[syntax_rule(
    F18V007r1 rule "upper-bound" #818 :
    "is specification-expr",
)]
pub fn upper_bound<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = UpperBound<S::Span>> + 'a {
    specification_expr(cfg).map(UpperBound)
}

#[derive(Debug, Clone)]
pub struct ExplicitShapeSpec<Span> {
    pub lower_bound: Option<LowerBound<Span>>,
    pub upper_bound: UpperBound<Span>,
}

// TODO test
#[syntax_rule(
    F18V007r1 rule "explicit-shape-spec" #816 :
    "is [ lower-bound : ] upper-bound",
)]
pub fn explicit_shape_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ExplicitShapeSpec<S::Span>> + 'a {
    (
        (lower_bound(cfg), space(0), ':', space(0)).map(|(lower_bound, _, _, _)| lower_bound).optional(),
        upper_bound(cfg),
    ).map(|(lower_bound, upper_bound)| ExplicitShapeSpec {
        lower_bound,
        upper_bound,
    })
}

#[derive(Debug, Clone)]
pub struct AssumedShapeSpec<Span> {
    pub lower_bound: Option<LowerBound<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "assumed-shape-spec" #819 :
    "is [ lower-bound ] :",
)]
pub fn assumed_shape_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = AssumedShapeSpec<S::Span>> + 'a {
    // TODO test
    (
        (lower_bound(cfg), space(0)).map(|(l, _)| l).optional(),
        ':', space(0),
    ).map(|(lower_bound, _, _)| AssumedShapeSpec {
        lower_bound,
    })
}

#[derive(Debug, Clone)]
pub struct DeferredShapeSpec;

// TODO test
#[syntax_rule(
    F18V007r1 rule "deferred-shape-spec" #820 :
    "is :",
)]
pub fn deferred_shape_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DeferredShapeSpec> + 'a {
    ':'.map(|_| DeferredShapeSpec)
}

#[derive(Debug, Clone)]
pub struct AssumedImpliedSpec<Span> {
    pub lower_bound: Option<LowerBound<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "assumed-implied-spec" #821 :
    "is [ lower-bound : ] *",
)]
pub fn assumed_implied_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = AssumedImpliedSpec<S::Span>> + 'a {
    // TODO test
    (
        (
            lower_bound(cfg),
            space(0), ':', space(0),
        ).map(|(lower_bound, _, _, _)| lower_bound).optional(),
        SpecialCharacter::Asterisk,
    ).map(|(lower_bound, _)| AssumedImpliedSpec {
        lower_bound,
    })
}

#[derive(Debug, Clone)]
pub struct AssumedSizeSpec<Span> {
    pub explicit: Vec<ExplicitShapeSpec<Span>>,
    pub implied: AssumedImpliedSpec<Span>,
}

#[syntax_rule(
    F18V007r1 rule "assumed-size-spec" #822 :
    "is explicit-shape-spec-list, assumed-implied-spec",
)]
pub fn assumed_size_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = AssumedSizeSpec<S::Span>> + 'a {
    // TODO test
    (
        (
            list(explicit_shape_spec(cfg), 0..),
            space(0), ',', space(0),
        )
            .map(|(explicit, _, _, _)| explicit)
            .optional()
            .map(|explicit| explicit.unwrap_or(vec![])),
        assumed_implied_spec(cfg),
    ).map(|(explicit, implied)| AssumedSizeSpec {
        explicit,
        implied,
    })
}

#[derive(Debug, Clone)]
pub struct ImpliedShapeOrAssumedSizeSpec<Span>(pub AssumedImpliedSpec<Span>);

#[syntax_rule(
    F18V007r1 rule "implied-shape-or-assumed-size-spec" #823 :
    "is assumed-implied-spec",
)]
pub fn implied_shape_or_assumed_size_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ImpliedShapeOrAssumedSizeSpec<S::Span>> + 'a {
    // TODO test
    assumed_implied_spec(cfg).map(ImpliedShapeOrAssumedSizeSpec)
}

#[derive(Debug, Clone)]
pub struct ImpliedShapeSpec<Span> {
    pub first: AssumedImpliedSpec<Span>,
    pub rest: Vec<AssumedImpliedSpec<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "implied-shape-spec" #824 :
    "is assumed-implied-spec, assumed-implied-spec-list",
)]
pub fn implied_shape_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ImpliedShapeSpec<S::Span>> + 'a {
    // TODO test
    (
        assumed_implied_spec(cfg),
        space(0), ',', space(0),
        list(assumed_implied_spec(cfg), 0..),
    ).map(|(first, _, _, _, rest)| ImpliedShapeSpec {
        first,
        rest,
    })
}

#[derive(Debug, Clone)]
pub struct AssumedRankSpec<Span>(PhantomData<Span>);

#[syntax_rule(
    F18V007r1 rule "assumed-rank-spec" #825 :
    "is ..",
)]
pub fn assumed_rank_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = AssumedRankSpec<S::Span>> + 'a {
    // TODO test
    StringMatch::exact("..", true).map(|_| AssumedRankSpec(PhantomData))
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum IntentSpec<Span> { // TODO use span
    In(Keyword<Span>),
    Out(Keyword<Span>),
    InOut(Keyword<Span>),
}

#[syntax_rule(
    F18V007r1 rule "intent-spec" #826 :
    "is IN"
    "or OUT"
    "or INOUT",
)]
pub fn intent_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = IntentSpec<S::Span>> + 'a {
    // TODO test
    alt!(
        kw("in", cfg).map(IntentSpec::In),
        kw("out", cfg).map(IntentSpec::Out),
        kw("inout", cfg).map(IntentSpec::InOut),
    )
}

//#[derive(Debug, Clone)]
//pub struct AccessStmt<Span> {
//    pub access_spec: AccessSpec,
//    pub ids: Vec<AccessId<Span>>,
//}

#[derive(Debug, Clone)]
pub struct DeferredCoShapeSpec;

// TODO test
#[syntax_rule(
    F18V007r1 rule "deferred-coshape-spec" #810 :
    "is :",
)]
pub fn deferred_coshape_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DeferredCoShapeSpec> + 'a {
    ':'.map(|_| DeferredCoShapeSpec)
}

#[derive(Debug, Clone)]
pub struct LowerCobound<Span>(pub SpecificationExpr<Span>);

// TODO test
#[syntax_rule(
    F18V007r1 rule "lower-cobound" #812:
    "is specification-expr",
)]
pub fn lower_cobound<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = LowerCobound<S::Span>> + 'a {
    specification_expr(cfg).map(LowerCobound)
}

#[derive(Debug, Clone)]
pub struct UpperCobound<Span>(pub SpecificationExpr<Span>);

// TODO test
#[syntax_rule(
    F18V007r1 rule "upper-cobound" #813 :
    "is specification-expr",
)]
pub fn upper_cobound<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = UpperCobound<S::Span>> + 'a {
    specification_expr(cfg).map(UpperCobound)
}

#[derive(Debug, Clone)]
pub struct DimensionSpec<Span> {
    pub array_spec: ArraySpec<Span>,
}

#[syntax_rule(
    F18V007r1 rule "dimension-spec" #814 :
    "is DIMENSION ( array-spec )",
)]
pub fn dimension_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DimensionSpec<S::Span>> + 'a {
    // TODO test
    (
        kw("dimension", cfg),
        (space(0), '(', space(0)),
        array_spec(cfg),
        (space(0), ')', space(0)),
    ).map(|(_, _, array_spec, _)| DimensionSpec {
        array_spec,
    })
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ArraySpec<Span> {
    ExplicitShapeSpecList(Vec<ExplicitShapeSpec<Span>>),
    AssumeShapeSpecList(Vec<AssumedShapeSpec<Span>>),
    DeferredShapeSpecList(Vec<DeferredShapeSpec>),
    AssumedSizeSpec(AssumedSizeSpec<Span>),
    ImpliedShapeSpec(ImpliedShapeSpec<Span>),
    ImpliedShapeOrAssumedSizeSpec(ImpliedShapeOrAssumedSizeSpec<Span>),
    AssumedRankSpec(AssumedRankSpec<Span>),
}

#[syntax_rule(
    F18V007r1 rule "array-spec" #815 :
    "is explicit-shape-spec-list"
    "or assumed-shape-spec-list"
    "or deferred-shape-spec-list"
    "or assumed-size-spec"
    "or implied-shape-spec"
    "or implied-shape-or-assumed-size-spec"
    "or assumed-rank-spec",
)]
pub fn array_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ArraySpec<S::Span>> + 'a {
    // TODO test
    alt!(
        list(explicit_shape_spec(cfg), 1..).map(ArraySpec::ExplicitShapeSpecList),
        list(assumed_shape_spec(cfg), 1..).map(ArraySpec::AssumeShapeSpecList),
        list(deferred_shape_spec(cfg), 1..).map(ArraySpec::DeferredShapeSpecList),
        assumed_size_spec(cfg).map(ArraySpec::AssumedSizeSpec),
        implied_shape_spec(cfg).map(ArraySpec::ImpliedShapeSpec),
        implied_shape_or_assumed_size_spec(cfg).map(ArraySpec::ImpliedShapeOrAssumedSizeSpec),
        assumed_rank_spec(cfg).map(ArraySpec::AssumedRankSpec),
    )
}

#[derive(Debug, Clone)]
pub struct ExplicitCoshapeSpec<Span> {
    pub list: Vec<(Option<LowerCobound<Span>>, UpperCobound<Span>)>,
    pub last: Option<UpperCobound<Span>>,
}

// TODO test
#[syntax_rule(
    F18V007r1 rule "explicit-coshape-spec" #811 :
    "is [ [ lower-cobound : ] upper-cobound, ]... [ lower-cobound : ] *",
)]
pub fn explicit_coshape_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ExplicitCoshapeSpec<S::Span>> + 'a {
    (
        (
            list(
                (
                    (lower_cobound(cfg), space(0), ':', space(0)).map(|(lower_cobound, _, _, _)| lower_cobound).optional(),
                    upper_cobound(cfg)
                ),
                0..
            ),
            space(0), ',', space(0),
        )
            .map(|(list, _, _, _)| list)
            .optional()
            .map(|list| list.unwrap_or(vec![])),
        upper_cobound(cfg).optional(),
        space(0), ':', space(0),
        SpecialCharacter::Asterisk,
    ).map(|(list, last, _, _, _, _)| ExplicitCoshapeSpec {
        list,
        last,
    })
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum CoarraySpec<Span> {
    Deferred(DeferredCoShapeSpec),
    Explicit(ExplicitCoshapeSpec<Span>),
}

// TODO test
#[syntax_rule(
    F18V007r1 rule "coarray-spec" #809:
    "is deferred-coshape-spec-list"
    "or explicit-coshape-spec",
)]
pub fn coarray_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = CoarraySpec<S::Span>> + 'a {
    alt!(
        deferred_coshape_spec(cfg).map(CoarraySpec::Deferred),
        explicit_coshape_spec(cfg).map(CoarraySpec::Explicit),
    )
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ComponentInitialization<Span> {
    Expr(ConstantExpr<Span>),
    NullInit(NullInit<Span>),
    InitialDataTarget(InitialDataTarget<Span>),
}

#[syntax_rule(
    F18V007r1 rule "component-initialization" #743 :
    "is = constant-expr"
    "or => null-init"
    "or => initial-data-target",
)]
pub fn component_initialization<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ComponentInitialization<S::Span>> + 'a {
    // TODO test
    alt!(
        ('=', space(0), constant_expr(cfg)).map(|(_, _, expr)| ComponentInitialization::Expr(expr)),
        ("=>", space(0), null_init(cfg)).map(|(_, _, null_init)| ComponentInitialization::NullInit(null_init)),
        ("=>", space(0), initial_data_target(cfg)).map(|(_, _, initial_data_target)| ComponentInitialization::InitialDataTarget(initial_data_target)),
    )
}

#[derive(Debug, Clone)]
pub struct TypeDeclarationStmt<Span> {
    pub declaration_type_spec: DeclarationTypeSpec<Span>,
    pub attr_spec_list: Option<Vec<AttrSpec<Span>>>,
    pub entity_decl_list: Vec<EntityDecl<Span>>,
    pub comment: Option<LineComment<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "type-declaration-stmt" #801 :
    "is declaration-type-spec [ [ , attr-spec ] ... :: ] entity-decl-list",
)]
pub fn type_declaration_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = TypeDeclarationStmt<S::Span>> + 'a {
    (
        space(0),
        declaration_type_spec(cfg),
        (
            many(
                (space(0), ',', space(0), attr_spec(cfg), space(0)).map(|(_, _, _, attr_spec, _)| attr_spec),
                0..,
            ),
            (space(0), "::", space(0)),
        ).map(|(attr_spec_list, (_, _, _))| attr_spec_list).optional(),
        list(entity_decl(cfg), 1..),
    ).map(|(_, declaration_type_spec, attr_spec_list, entity_decl_list)| TypeDeclarationStmt {
        declaration_type_spec,
        attr_spec_list,
        entity_decl_list,
        comment: None,
    })
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum AttrSpec<Span> {
    AccessSpec(AccessSpec),
    Allocatable,
    Asynchronous,
    Codimension(CoarraySpec<Span>),
    Contiguous,
    Dimension(ArraySpec<Span>),
    External,
    Intent(IntentSpec<Span>),
    Intrinsic,
    LanguageBindingSpec(LanguageBindingSpec<Span>),
    Optional,
    Parameter,
    Pointer,
    Protected,
    Save,
    Target,
    Value,
    Volatile,
}

#[syntax_rule(
    F18V007r1 rule "attr-spec" #802 :
    "is access-spec"
    "or ALLOCATABLE"
    "or ASYNCHRONOUS"
    "or CODIMENSION lbracket coarray-spec rbracket"
    "or CONTIGUOUS"
    "or DIMENSION ( array-spec )"
    "or EXTERNAL"
    "or INTENT ( intent-spec )"
    "or INTRINSIC"
    "or language-binding-spec"
    "or OPTIONAL"
    "or PARAMETER"
    "or POINTER"
    "or PROTECTED"
    "or SAVE"
    "or TARGET"
    "or VALUE"
    "or VOLATILE",
)]
pub fn attr_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = AttrSpec<S::Span>> + 'a {
    alt!(
        access_spec(cfg).map(AttrSpec::AccessSpec),
        kw("allocatable", cfg).map(|_| AttrSpec::Allocatable),
        kw("asynchronous", cfg).map(|_| AttrSpec::Asynchronous),
        (kw("codimension", cfg), space(0), lbracket(cfg), space(0), coarray_spec(cfg), space(0), rbracket(cfg)).map(|(_, _, _, _, coarray_spec, _, _)| AttrSpec::Codimension(coarray_spec)),
        kw("contiguous", cfg).map(|_| AttrSpec::Contiguous),
        (kw("dimension", cfg), space(0), '(', space(0), array_spec(cfg), space(0), ')').map(|(_, _, _, _, array_spec, _, _)| AttrSpec::Dimension(array_spec)),
        kw("external", cfg).map(|_| AttrSpec::External),
        (kw("intent", cfg), space(0), '(', space(0), intent_spec(cfg), space(0), ')').map(|(_, _, _, _, intent_spec, _, _)| AttrSpec::Intent(intent_spec)),
        kw("intrinsic", cfg).map(|_| AttrSpec::Intrinsic),
        language_binding_spec(cfg).map(AttrSpec::LanguageBindingSpec),
        kw("optional", cfg).map(|_| AttrSpec::Optional),
        kw("parameter", cfg).map(|_| AttrSpec::Parameter),
        kw("pointer", cfg).map(|_| AttrSpec::Pointer),
        kw("protected", cfg).map(|_| AttrSpec::Protected),
        kw("save", cfg).map(|_| AttrSpec::Save),
        kw("target", cfg).map(|_| AttrSpec::Target),
        kw("value", cfg).map(|_| AttrSpec::Value),
        kw("volatile", cfg).map(|_| AttrSpec::Volatile),
    )
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum EntityDecl<Span> {
    Form1 {
        object_name: ObjectName<Span>,
        array_spec: Option<ArraySpec<Span>>,
        coarray_spec: Option<CoarraySpec<Span>>,
        char_length: Option<CharLength<Span>>,
        initialization: Option<Initialization<Span>>,
    },
    Form2 {
        function_name: Name<Span>,
        char_length: Option<CharLength<Span>>,
    }
}

#[syntax_rule(
    F18V007r1 rule "entity-decl" #803 :
    "is object-name [ ( array-spec ) ] [ lbracket coarray-spec rbracket ] [ * char-length ] [ initialization ]"
    "or function-name [ * char-length ]",
)]
pub fn entity_decl<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = EntityDecl<S::Span>> + 'a {
    let form_1 = (
        object_name(cfg),
        space(1),
        (
            '(', space(0), array_spec(cfg), space(0), ')', space(0),
        ).map(|(_, _, array_spec, _, _, _)| array_spec).optional(),
        (
            lbracket(cfg), space(0),
            coarray_spec(cfg),
            space(0), rbracket(cfg), space(0),
        ).map(|(_, _, coarray_spec, _, _, _)| coarray_spec).optional(),
        (
            SpecialCharacter::Asterisk, space(0),
            char_length(cfg), space(0),
        ).map(|(_, _, char_length, _)| char_length).optional(),
        initialization(cfg).optional(),
    ).map(|(object_name, _, array_spec, coarray_spec, char_length, initialization)| EntityDecl::Form1 {
        object_name,
        array_spec,
        coarray_spec,
        char_length,
        initialization,
    });

    let form_2 = (
        name(cfg, false),
        (
            space(0),
            SpecialCharacter::Asterisk, space(0),
            char_length(cfg),
        ).map(|(_, _, _, char_length)| char_length).optional(),
    ).map(|(function_name, char_length)| EntityDecl::Form2 {
        function_name,
        char_length,
    });

    alt!(
        form_1,
        form_2,
    )
}

#[derive(Debug, Clone)]
pub struct ObjectName<Span>(pub Name<Span>);

#[syntax_rule(
    F18V007r1 rule "object-name" #804 :
    "is name",
)]
pub fn object_name<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ObjectName<S::Span>> + 'a {
    name(cfg, false).map(ObjectName)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum Initialization<Span> {
    ConstantExpr(ConstantExpr<Span>),
    NullInit(NullInit<Span>),
    InitialDataTarget(InitialDataTarget<Span>),
}

#[syntax_rule(
    F18V007r1 rule "initialization" #805 :
    "is = constant-expr"
    "or => null-init"
    "or => initial-data-target",
)]
pub fn initialization<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = Initialization<S::Span>> + 'a {
    // TODO test
    alt!(
        ('=', space(0), constant_expr(cfg)).map(|(_, _, expr)| Initialization::ConstantExpr(expr)),
        ("=>", space(0), null_init(cfg)).map(|(_, _, null_init)| Initialization::NullInit(null_init)),
        ("=>", space(0), initial_data_target(cfg)).map(|(_, _, initial_data_target)| Initialization::InitialDataTarget(initial_data_target)),
    )
}

#[derive(Debug, Clone)]
pub struct NullInit<Span>(std::marker::PhantomData<Span>); // TODO

#[syntax_rule(
    F18V007r1 rule "null-init" #806 :
    "is function-reference",
)]
pub fn null_init<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = NullInit<S::Span>> + 'a {
    |_| todo!("TODO: parser not implemented yet")
}

#[derive(Debug, Clone)]
pub struct LanguageBindingSpec<Span> {
    pub name: Option<DefaultCharConstantExpr<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "language-binding-spec" #808 :
    "is BIND (C [ , NAME = scalar-default-char-constant-expr ])",
)]
pub fn language_binding_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = LanguageBindingSpec<S::Span>> + 'a {
    // TODO test
    (
        kw("bind", cfg),
        space(0),
        '(',
        space(0),
        kw("c", cfg),
        space(0),
        (
            ',',
            space(0),
            kw("name", cfg),
            space(0),
            '=',
            space(0),
            default_char_constant_expr(cfg),
            space(0),
        ).map(|(_, _, _, _, _, _, name, _)| name).optional(),
        ')',
    ).map(|(_, _, _, _, _, _, name, _)| LanguageBindingSpec {
        name,
    })
}

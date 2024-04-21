use super::*;

#[derive(Debug, Clone)]
pub struct AccessStmt<Span> {
    pub access_spec: AccessSpec,
    pub access_id_list: Option<Vec<AccessId<Span>>>,
}

#[syntax_rule(
    F18V007r1 rule "access-stmt" #827 : "is access-spec [ [ :: ] access-id-list ]",
)]
pub fn access_stmt_2<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = AccessStmt<S::Span>> + 'a {
    (
        space(0), access_spec(cfg), space(0),
        (
            (space(0), "::", space(0)),
            list(access_id(cfg), 0..),
        ).map(|(_, access_id_list)| access_id_list).optional(),
    ).map(|(_, access_spec, _, access_id_list)| AccessStmt {
        access_spec,
        access_id_list,
    })
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum AccessId<Span> {
    AccessName(Name<Span>),
    GenericSpec(GenericSpec<Span>),
}

#[syntax_rule(
    F18V007r1 rule "access-id" #828 :
    "is access-name"
    "or generic-spec",
)]
pub fn access_id<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = AccessId<S::Span>> + 'a {
    alt!(
        name(cfg, false).map(AccessId::AccessName),
        generic_spec(cfg).map(AccessId::GenericSpec),
    )
}

#[derive(Debug, Clone)]
pub struct AllocatableStmt<Span> {
    pub allocatable_decl_list: Vec<AllocatableDecl<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "allocatable-stmt" #829 : "is ALLOCATABLE [ :: ] allocatable-decl-list",
)]
pub fn allocatable_stmt_2<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = AllocatableStmt<S::Span>> + 'a {
    (
        (space(0), kw("allocatable", cfg), space(0)),
        (space(0), "::", space(0)).optional(),
        list(allocatable_decl(cfg), 1..),
    ).map(|(_, _, allocatable_decl_list)| AllocatableStmt {
        allocatable_decl_list,
    })
}

#[derive(Debug, Clone)]
pub struct AllocatableDecl<Span> {
    pub object_name: ObjectName<Span>,
    pub array_spec: Option<ArraySpec<Span>>,
    pub coarray_spec: Option<CoarraySpec<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "allocatable-decl" #830 :
    "is object-name [ ( array-spec ) ] [ lbracket coarray-spec rbracket ]",
)]
pub fn allocatable_decl<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = AllocatableDecl<S::Span>> + 'a {
    (
        object_name(cfg),
        (
            (space(0), '(', space(0)),
            array_spec(cfg),
            (space(0), ')', space(0)),
        ).map(|(_, array_spec, _)| array_spec).optional(),
        (
            (space(0), '[', space(0)),
            coarray_spec(cfg),
            (space(0), ']', space(0)),
        ).map(|(_, coarray_spec, _)| coarray_spec).optional(),
    ).map(|(object_name, array_spec, coarray_spec)| AllocatableDecl {
        object_name,
        array_spec,
        coarray_spec,
    })
}

#[derive(Debug, Clone)]
pub struct AsynchronousStmt<Span> {
    pub object_name_list: Vec<ObjectName<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "asynchronous-stmt" #831 : "is ASYNCHRONOUS [ :: ] object-name-list",
)]
pub fn asynchronous_stmt_2<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = AsynchronousStmt<S::Span>> + 'a {
    (
        (space(0), kw("asynchronous", cfg), space(0)),
        (space(0), "::", space(0)).optional(),
        list(object_name(cfg), 1..),
    ).map(|(_, _, object_name_list)| AsynchronousStmt {
        object_name_list,
    })
}

#[derive(Debug, Clone)]
pub struct BindStmt<Span> {
    pub language_binding_spec: LanguageBindingSpec<Span>,
    pub bind_entity_list: Vec<BindEntity<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "bind-stmt" #832 : "is language-binding-spec [ :: ] bind-entity-list",
)]
pub fn bind_stmt_2<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = BindStmt<S::Span>> + 'a {
    (
        space(0),
        language_binding_spec(cfg),
        (space(0), "::", space(0)).optional(),
        list(bind_entity(cfg), 1..),
    ).map(|(_, language_binding_spec, _, bind_entity_list)| BindStmt {
        language_binding_spec,
        bind_entity_list,
    })
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum BindEntity<Span> {
    EntityName(Name<Span>),
    CommonBlockName(Name<Span>),
}

#[syntax_rule(
    F18V007r1 rule "bind-entity" #833 :
    "is entity-name"
    "or / common-block-name /",
)]
pub fn bind_entity<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = BindEntity<S::Span>> + 'a {
    alt!(
        name(cfg, false).map(BindEntity::EntityName),
        (
            (space(0), '/', space(0)),
            name(cfg, false),
            (space(0), '/', space(0)),
        ).map(|(_, common_block_name, _)| BindEntity::CommonBlockName(common_block_name)),
    )
}

#[derive(Debug, Clone)]
pub struct CodimensionStmt<Span> {
    pub codimension_decl_list: Vec<CodimensionDecl<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "codimension-stmt" #834 : "is CODIMENSION [ :: ] codimension-decl-list",
)]
pub fn codimension_stmt_2<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = CodimensionStmt<S::Span>> + 'a {
    (
        (space(0), kw("codimension", cfg), space(0)),
        (space(0), "::", space(0)).optional(),
        list(codimension_decl(cfg), 1..),
    ).map(|(_, _, codimension_decl_list)| CodimensionStmt {
        codimension_decl_list,
    })
}

#[derive(Debug, Clone)]
pub struct CodimensionDecl<Span> {
    pub coarray_name: Name<Span>,
    pub coarray_spec: CoarraySpec<Span>,
}

#[syntax_rule(
    F18V007r1 rule "codimension-decl" #835 : "is coarray-name lbracket coarray-spec rbracket",
)]
pub fn codimension_decl<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = CodimensionDecl<S::Span>> + 'a {
    (
        name(cfg, false),
        (
            (space(0), '[', space(0)),
            coarray_spec(cfg),
            (space(0), ']', space(0)),
        ).map(|(_, coarray_spec, _)| coarray_spec),
    ).map(|(coarray_name, coarray_spec)| CodimensionDecl {
        coarray_name,
        coarray_spec,
    })
}

#[derive(Debug, Clone)]
pub struct ContiguousStmt<Span> {
    pub object_name_list: Vec<Name<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "contiguous-stmt" #836 : "is CONTIGUOUS [ :: ] object-name-list",
)]
pub fn contiguous_stmt_2<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ContiguousStmt<S::Span>> + 'a {
    (
        (space(0), kw("contiguous", cfg), space(0)),
        (space(0), "::", space(0)).optional(),
        list(name(cfg, false), 1..),
    ).map(|(_, _, object_name_list)| ContiguousStmt {
        object_name_list,
    })
}

#[derive(Debug, Clone)]
pub struct DataStmt<Span> {
    pub data_stmt_set_list: Vec<DataStmtSet<Span>>,
    pub comment: Option<LineComment<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "data-stmt" #837 : "is DATA data-stmt-set [ [ , ] data-stmt-set ] ...",
)]
pub fn data_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DataStmt<S::Span>> + 'a {
    (
        space(0),
        kw("data", cfg), space(0),
        list(data_stmt_set(cfg), 1..),
        statement_termination(),
    ).map(|(_, _, _, data_stmt_set_list, comment)| DataStmt {
        data_stmt_set_list,
        comment,
    })
}

#[derive(Debug, Clone)]
pub struct DataStmtSet<Span> {
    pub data_stmt_object_list: Vec<DataStmtObject<Span>>,
    pub data_stmt_value_list: Vec<DataStmtValue<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "data-stmt-set" #838 : "is data-stmt-object-list / data-stmt-value-list /",
)]
pub fn data_stmt_set<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DataStmtSet<S::Span>> + 'a {
    (
        list(data_stmt_object(cfg), 0..),
        (space(0), '/', space(0)),
        list(data_stmt_value(cfg), 0..),
        (space(0), '/', space(0)),
    ).map(|(data_stmt_object_list, _, data_stmt_value_list, _)| DataStmtSet {
        data_stmt_object_list,
        data_stmt_value_list,
    })
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum DataStmtObject<Span> {
    Variable(Variable<Span>),
    DataImpliedDo(DataImpliedDo<Span>),
}

#[syntax_rule(
    F18V007r1 rule "data-stmt-object" #839 :
    "is variable"
    "or data-implied-do",
)]
pub fn data_stmt_object<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DataStmtObject<S::Span>> + 'a {
    alt!(
        variable(cfg, false).map(DataStmtObject::Variable),
        data_implied_do(cfg).map(DataStmtObject::DataImpliedDo),
    )
}

#[derive(Debug, Clone)]
pub struct DataImpliedDo<Span> {
    pub data_i_do_object_list: Vec<DataIDoObject<Span>>,
    pub integer_type_spec: Option<IntegerTypeSpec<Span>>,
    pub data_i_do_variable: DataIDoVariable<Span>,
    pub int_constant_expr1: IntConstantExpr<Span>,
    pub int_constant_expr2: IntConstantExpr<Span>,
    pub int_constant_expr3: Option<IntConstantExpr<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "data-implied-do" #840 :
    "is ( data-i-do-object-list , [ integer-type-spec :: ] data-i-do-variable = scalar-int-constant-expr , scalar-int-constant-expr [ , scalar-int-constant-expr ] )",
)]
pub fn data_implied_do<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DataImpliedDo<S::Span>> + 'a {
    (
        ('(', space(0)),
        list(data_i_do_object(cfg), 0..),
        (space(0), ',', space(0)),
        (
            integer_type_spec(cfg),
            (space(0), "::", space(0)),
        ).map(|(integer_type_spec, _)| integer_type_spec).optional(),
        data_i_do_variable(cfg),
        (space(0), '=', space(0)),
        int_constant_expr(cfg),
        (space(0), ',', space(0)),
        int_constant_expr(cfg),
        (
            (space(0), ',', space(0)),
            int_constant_expr(cfg),
        ).map(|(_, int_constant_expr)| int_constant_expr).optional(),
        (space(0), ')'),
    ).map(|(_, data_i_do_object_list, _, integer_type_spec, data_i_do_variable, _, int_constant_expr1, _, int_constant_expr2, int_constant_expr3, _)| DataImpliedDo {
        data_i_do_object_list,
        integer_type_spec,
        data_i_do_variable,
        int_constant_expr1,
        int_constant_expr2,
        int_constant_expr3,
    })
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum DataIDoObject<Span> {
    ArrayElement(ArrayElement<Span>),
    ScalarStructureComponent(StructureComponent<Span>),
    DataImpliedDo(DataImpliedDo<Span>),
}

#[syntax_rule(
    F18V007r1 rule "data-i-do-object" #841 :
    "is array-element"
    "or scalar-structure-component"
    "or data-implied-do",
)]
pub fn data_i_do_object<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DataIDoObject<S::Span>> + 'a {
    alt!(
        array_element(cfg).map(DataIDoObject::ArrayElement),
        structure_component(cfg).map(DataIDoObject::ScalarStructureComponent),
        data_implied_do(cfg).map(DataIDoObject::DataImpliedDo),
    )
}

#[derive(Debug, Clone)]
pub struct DataIDoVariable<Span>(pub DoVariable<Span>);

#[syntax_rule(
    F18V007r1 rule "data-i-do-variable" #842 : "is do-variable",
)]
pub fn data_i_do_variable<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DataIDoVariable<S::Span>> + 'a {
    do_variable(cfg).map(DataIDoVariable)
}

#[derive(Debug, Clone)]
pub struct DataStmtValue<Span> {
    pub data_stmt_repeat: Option<DataStmtRepeat<Span>>,
    pub data_stmt_constant: DataStmtConstant<Span>,
}

#[syntax_rule(
    F18V007r1 rule "data-stmt-value" #843 : "is [ data-stmt-repeat * ] data-stmt-constant",
)]
pub fn data_stmt_value<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DataStmtValue<S::Span>> + 'a {
    (
        (
            data_stmt_repeat(cfg),
            (space(0), '*', space(0)),
        ).map(|(data_stmt_repeat, _)| data_stmt_repeat).optional(),
        data_stmt_constant(cfg),
    ).map(|(data_stmt_repeat, data_stmt_constant)| DataStmtValue {
        data_stmt_repeat,
        data_stmt_constant,
    })
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum DataStmtRepeat<Span> {
    ScalarIntConstant(IntConstant<Span>),
    ScalarIntConstantSubobject(IntConstantSubobject<Span>),
}

#[syntax_rule(
    F18V007r1 rule "data-stmt-repeat" #844 :
    "is scalar-int-constant"
    "or scalar-int-constant-subobject",
)]
pub fn data_stmt_repeat<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DataStmtRepeat<S::Span>> + 'a {
    alt!(
        int_constant(cfg).map(DataStmtRepeat::ScalarIntConstant),
        int_constant_subobject(cfg).map(DataStmtRepeat::ScalarIntConstantSubobject),
    )
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum DataStmtConstant<Span> {
    ScalarConstant(Constant<Span>),
    ScalarConstantSubobject(ConstantSubobject<Span>),
    SignedIntLiteralConstant(SignedIntLiteralConstant<Span>),
    SignedRealLiteralConstant(SignedRealLiteralConstant<Span>),
    NullInit(NullInit<Span>),
    InitialDataTarget(InitialDataTarget<Span>),
    StructureConstructor(StructureConstructor<Span>),
}

#[syntax_rule(
    F18V007r1 rule "data-stmt-constant" #845 :
    "is scalar-constant"
    "or scalar-constant-subobject"
    "or signed-int-literal-constant"
    "or signed-real-literal-constant"
    "or null-init"
    "or initial-data-target"
    "or structure-constructor",
)]
pub fn data_stmt_constant<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DataStmtConstant<S::Span>> + 'a {
    alt!(
        constant(cfg).map(DataStmtConstant::ScalarConstant),
        constant_subobject(cfg).map(DataStmtConstant::ScalarConstantSubobject),
        signed_int_literal_constant(cfg).map(DataStmtConstant::SignedIntLiteralConstant),
        signed_real_literal_constant(cfg).map(DataStmtConstant::SignedRealLiteralConstant),
        null_init(cfg).map(DataStmtConstant::NullInit),
        initial_data_target(cfg).map(DataStmtConstant::InitialDataTarget),
        structure_constructor(cfg).map(DataStmtConstant::StructureConstructor),
    )
}

#[derive(Debug, Clone)]
pub struct IntConstantSubobject<Span>(pub ConstantSubobject<Span>);

#[syntax_rule(
    F18V007r1 rule "int-constant-subobject" #846 : "is constant-subobject",
)]
pub fn int_constant_subobject<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = IntConstantSubobject<S::Span>> + 'a {
    constant_subobject(cfg).map(IntConstantSubobject)
}

#[derive(Debug, Clone)]
pub struct ConstantSubobject<Span>(pub Designator<Span>);

#[syntax_rule(
    F18V007r1 rule "constant-subobject" #847 : "is designator",
)]
pub fn constant_subobject<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ConstantSubobject<S::Span>> + 'a {
    designator(cfg, false).map(ConstantSubobject)
}

#[derive(Debug, Clone)]
pub struct DimensionStmt<Span> {
    pub list: Vec<(Name<Span>, ArraySpec<Span>)>,
}

#[syntax_rule(
    F18V007r1 rule "dimension-stmt" #848 :
    "is DIMENSION [ :: ] array-name ( array-spec ) [ , array-name ( array-spec ) ] ...",
)]
pub fn dimension_stmt_2<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DimensionStmt<S::Span>> + 'a {
    (
        (space(0), kw("dimension", cfg), space(0)),
        (space(0), "::", space(0)).optional(),
        list(
            (
                name(cfg, false),
                (space(0), '(', space(0)),
                array_spec(cfg),
                (space(0), ')', space(0)),
            ).map(|(name, _, array_spec, _)| (name, array_spec)),
            1..,
        ),
    ).map(|(_, _, list)| DimensionStmt {
        list,
    })
}

#[derive(Debug, Clone)]
pub struct IntentStmt<Span> {
    pub intent_spec: IntentSpec<Span>,
    pub dummy_arg_name_list: Vec<DummyArgName<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "intent-stmt" #849 : "is INTENT ( intent-spec ) [ :: ] dummy-arg-name-list",
)]
pub fn intent_stmt_2<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = IntentStmt<S::Span>> + 'a {
    (
        (space(0), kw("intent", cfg), space(0), '(', space(0)),
        intent_spec(cfg),
        (space(0), ')', space(0)),
        (space(0), "::", space(0)).optional(),
        list(dummy_arg_name(cfg), 0..),
    ).map(|(_, intent_spec, _, _, dummy_arg_name_list)| IntentStmt {
        intent_spec,
        dummy_arg_name_list,
    })
}

#[derive(Debug, Clone)]
pub struct OptionalStmt<Span> {
    pub dummy_arg_name_list: Vec<DummyArgName<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "optional-stmt" #850 : "is OPTIONAL [ :: ] dummy-arg-name-list",
)]
pub fn optional_stmt_2<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = OptionalStmt<S::Span>> + 'a {
    (
        (space(0), kw("optional", cfg), space(0)),
        (space(0), "::", space(0)).optional(),
        list(dummy_arg_name(cfg), 0..),
    ).map(|(_, _, dummy_arg_name_list)| OptionalStmt {
        dummy_arg_name_list,
    })
}

#[derive(Debug, Clone)]
pub struct PointerStmt<Span> {
    pub pointer_decl_list: Vec<PointerDecl<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "pointer-stmt" #853 : "is POINTER [ :: ] pointer-decl-list",
)]
pub fn pointer_stmt_2<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = PointerStmt<S::Span>> + 'a {
    (
        (space(0), kw("pointer", cfg), space(0)),
        (space(0), "::", space(0)).optional(),
        list(pointer_decl(cfg), 1..),
    ).map(|(_, _, pointer_decl_list)| PointerStmt {
        pointer_decl_list,
    })
}

// TODO maybe enum
#[derive(Debug, Clone)]
pub struct PointerDecl<Span> {
    pub name: Name<Span>,
    pub deferred_shape_spec_list: Option<Vec<DeferredShapeSpec>>,
    pub comment: Option<LineComment<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "pointer-decl" #854 :
    "is object-name [ ( deferred-shape-spec-list ) ]"
    "or proc-entity-name", // TODO maybe this would never match in this form
)]
pub fn pointer_decl<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = PointerDecl<S::Span>> + 'a {
    (
        name(cfg, false),
        (
            (space(0), '(', space(0)),
            list(deferred_shape_spec(cfg), 0..),
            (space(0), ')', space(0)),
        ).map(|(_, deferred_shape_spec_list, _)| deferred_shape_spec_list).optional(),
        statement_termination(),
    ).map(|(name, deferred_shape_spec_list, comment)| PointerDecl {
        name,
        deferred_shape_spec_list,
        comment,
    })
}

#[derive(Debug, Clone)]
pub struct ProtectedStmt<Span> {
    pub entity_name_list: Vec<Name<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "protected-stmt" #855 : "is PROTECTED [ :: ] entity-name-list",
)]
pub fn protected_stmt_2<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ProtectedStmt<S::Span>> + 'a {
    (
        (space(0), kw("protected", cfg), space(0)),
        (space(0), "::", space(0)).optional(),
        list(name(cfg, false), 1..),
    ).map(|(_, _, entity_name_list)| ProtectedStmt {
        entity_name_list,
    })
}

#[derive(Debug, Clone)]
pub struct SaveStmt<Span> {
    pub saved_entity_list: Option<Vec<SavedEntity<Span>>>,
}

#[syntax_rule(
    F18V007r1 rule "save-stmt" #856 : "SAVE [ [ :: ] saved-entity-list ]",
)]
pub fn save_stmt_2<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = SaveStmt<S::Span>> + 'a {
    (
        (space(0), kw("save", cfg), space(0)),
        (
            (space(0), "::", space(0)),
            list(saved_entity(cfg), 0..),
        ).map(|(_, saved_entity_list)| saved_entity_list).optional(),
    ).map(|(_, saved_entity_list)| SaveStmt {
        saved_entity_list,
    })
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum SavedEntity<Span> {
    ObjectName(ObjectName<Span>),
    ProcPointerName(ProcPointerName<Span>),
    CommonBlockName(Name<Span>),
}

#[syntax_rule(
    F18V007r1 rule "saved-entity" #857 :
    "is object-name"
    "or proc-pointer-name"
    "or / common-block-name /",
)]
pub fn saved_entity<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = SavedEntity<S::Span>> + 'a {
    alt!(
        object_name(cfg).map(SavedEntity::ObjectName),
        proc_pointer_name(cfg).map(SavedEntity::ProcPointerName),
        (
            (space(0), '/', space(0)),
            name(cfg, false),
            (space(0), '/', space(0)),
        ).map(|(_, common_block_name, _)| SavedEntity::CommonBlockName(common_block_name)),
    )
}
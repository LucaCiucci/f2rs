
use super::*;

#[derive(Debug, Clone)]
pub struct AccessStmt<Span> {
    pub access_spec: AccessSpec,
    pub access_id_list: Option<Vec<AccessId<Span>>>,
}

#[syntax_rule(
    F18V007r1 rule "access-stmt" #827 : "is access-spec [ [ :: ] access-id-list ]",
)]
pub fn access_stmt_2<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = AccessStmt<MultilineSpan>> + 'a {
    (
        access_spec(cfg),
        (
            dot_dot(),
            list(access_id(cfg), 0..),
        ).map(|(_, access_id_list)| access_id_list).optional(),
    ).map(|(access_spec, access_id_list)| AccessStmt {
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
pub fn access_id<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = AccessId<MultilineSpan>> + 'a {
    alt!(
        name().map(AccessId::AccessName),
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
pub fn allocatable_stmt_2<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = AllocatableStmt<MultilineSpan>> + 'a {
    (
        kw!(allocatable),
        double_colon().optional(),
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
pub fn allocatable_decl<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = AllocatableDecl<MultilineSpan>> + 'a {
    (
        object_name(cfg),
        (
            delim('('),
            array_spec(cfg),
            delim(')'),
        ).map(|(_, array_spec, _)| array_spec).optional(),
        (
            delim('['),
            coarray_spec(cfg),
            delim(']'),
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
pub fn asynchronous_stmt_2<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = AsynchronousStmt<MultilineSpan>> + 'a {
    (
        kw!(asynchronous),
        double_colon().optional(),
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
pub fn bind_stmt_2<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = BindStmt<MultilineSpan>> + 'a {
    (
        language_binding_spec(cfg),
        double_colon().optional(),
        list(bind_entity(cfg), 1..),
    ).map(|(language_binding_spec, _, bind_entity_list)| BindStmt {
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
pub fn bind_entity<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = BindEntity<MultilineSpan>> + 'a {
    alt!(
        name().map(BindEntity::EntityName),
        (
            op("/"),
            name(),
            op("/"),
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
pub fn codimension_stmt_2<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = CodimensionStmt<MultilineSpan>> + 'a {
    (
        kw!(codimension),
        double_colon().optional(),
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
pub fn codimension_decl<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = CodimensionDecl<MultilineSpan>> + 'a {
    (
        name(),
        (
            delim('['),
            coarray_spec(cfg),
            delim(']'),
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
pub fn contiguous_stmt_2<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ContiguousStmt<MultilineSpan>> + 'a {
    (
        kw!(contiguous),
        double_colon().optional(),
        list(name(), 1..),
    ).map(|(_, _, object_name_list)| ContiguousStmt {
        object_name_list,
    })
}

#[derive(Debug, Clone)]
pub struct DataStmt<Span> {
    pub data_stmt_set_list: Vec<DataStmtSet<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "data-stmt" #837 : "is DATA data-stmt-set [ [ , ] data-stmt-set ] ...",
)]
pub fn data_stmt<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DataStmt<MultilineSpan>> + 'a {
    (
        kw!(data),
        list(data_stmt_set(cfg), 1..),
    ).map(|(_, data_stmt_set_list)| DataStmt {
        data_stmt_set_list,
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
pub fn data_stmt_set<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DataStmtSet<MultilineSpan>> + 'a {
    (
        list(data_stmt_object(cfg), 0..),
        op("/"),
        list(data_stmt_value(cfg), 0..),
        op("/"),
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
pub fn data_stmt_object<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DataStmtObject<MultilineSpan>> + 'a {
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
pub fn data_implied_do<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DataImpliedDo<MultilineSpan>> + 'a {
    (
        delim('('),
        list(data_i_do_object(cfg), 0..),
        comma(),
        (
            integer_type_spec(cfg),
            double_colon(),
        ).map(|(integer_type_spec, _)| integer_type_spec).optional(),
        data_i_do_variable(cfg),
        equals(),
        int_constant_expr(cfg),
        comma(),
        int_constant_expr(cfg),
        (
            comma(),
            int_constant_expr(cfg),
        ).map(|(_, int_constant_expr)| int_constant_expr).optional(),
        delim(')'),
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
pub fn data_i_do_object<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DataIDoObject<MultilineSpan>> + 'a {
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
pub fn data_i_do_variable<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DataIDoVariable<MultilineSpan>> + 'a {
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
pub fn data_stmt_value<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DataStmtValue<MultilineSpan>> + 'a {
    (
        (
            data_stmt_repeat(cfg),
            asterisk(),
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
pub fn data_stmt_repeat<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DataStmtRepeat<MultilineSpan>> + 'a {
    alt!(
        int_constant(cfg).map(DataStmtRepeat::ScalarIntConstant),
        int_constant_subobject(cfg).map(DataStmtRepeat::ScalarIntConstantSubobject),
    )
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum DataStmtConstant<Span> {
    ScalarConstant(Constant<Span>),
    ScalarConstantSubobject(ConstantSubobject<Span>),
    // TODO maybe scalar already handles this? SignedIntLiteralConstant(SignedIntLiteralConstant<Span>),
    // TODO maybe scalar already handles this? SignedRealLiteralConstant(SignedRealLiteralConstant<Span>),
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
pub fn data_stmt_constant<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DataStmtConstant<MultilineSpan>> + 'a {
    alt!(
        constant(cfg).map(DataStmtConstant::ScalarConstant),
        constant_subobject(cfg).map(DataStmtConstant::ScalarConstantSubobject),
        // TODO maybe scalar already handles this? signed_int_literal_constant(cfg).map(DataStmtConstant::SignedIntLiteralConstant),
        // TODO maybe scalar already handles this? signed_real_literal_constant(cfg).map(DataStmtConstant::SignedRealLiteralConstant),
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
pub fn int_constant_subobject<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = IntConstantSubobject<MultilineSpan>> + 'a {
    constant_subobject(cfg).map(IntConstantSubobject)
}

#[derive(Debug, Clone)]
pub struct ConstantSubobject<Span>(pub Designator<Span>);

#[syntax_rule(
    F18V007r1 rule "constant-subobject" #847 : "is designator",
)]
pub fn constant_subobject<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ConstantSubobject<MultilineSpan>> + 'a {
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
pub fn dimension_stmt_2<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DimensionStmt<MultilineSpan>> + 'a {
    (
        kw!(dimension),
        double_colon().optional(),
        list(
            (
                name(),
                delim('('),
                array_spec(cfg),
                delim(')'),
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
pub fn intent_stmt_2<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = IntentStmt<MultilineSpan>> + 'a {
    (
        (kw!(intent), delim('(')),
        intent_spec(cfg),
        delim(')'),
        double_colon().optional(),
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
pub fn optional_stmt_2<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = OptionalStmt<MultilineSpan>> + 'a {
    (
        kw!(optional),
        double_colon().optional(),
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
pub fn pointer_stmt_2<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = PointerStmt<MultilineSpan>> + 'a {
    (
        kw!(pointer),
        double_colon().optional(),
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
}

#[syntax_rule(
    F18V007r1 rule "pointer-decl" #854 :
    "is object-name [ ( deferred-shape-spec-list ) ]"
    "or proc-entity-name", // TODO maybe this would never match in this form
)]
pub fn pointer_decl<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = PointerDecl<MultilineSpan>> + 'a {
    (
        name(),
        (
            delim('('),
            list(deferred_shape_spec(cfg), 0..),
            delim(')'),
        ).map(|(_, deferred_shape_spec_list, _)| deferred_shape_spec_list).optional(),
    ).map(|(name, deferred_shape_spec_list)| PointerDecl {
        name,
        deferred_shape_spec_list,
    })
}

#[derive(Debug, Clone)]
pub struct ProtectedStmt<Span> {
    pub entity_name_list: Vec<Name<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "protected-stmt" #855 : "is PROTECTED [ :: ] entity-name-list",
)]
pub fn protected_stmt_2<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ProtectedStmt<MultilineSpan>> + 'a {
    (
        kw!(protected),
        double_colon().optional(),
        list(name(), 1..),
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
pub fn save_stmt_2<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = SaveStmt<MultilineSpan>> + 'a {
    (
        kw!(save),
        (
            double_colon(),
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
pub fn saved_entity<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = SavedEntity<MultilineSpan>> + 'a {
    alt!(
        object_name(cfg).map(SavedEntity::ObjectName),
        proc_pointer_name(cfg).map(SavedEntity::ProcPointerName),
        (
            op("/"),
            name(),
            op("/"),
        ).map(|(_, common_block_name, _)| SavedEntity::CommonBlockName(common_block_name)),
    )
}
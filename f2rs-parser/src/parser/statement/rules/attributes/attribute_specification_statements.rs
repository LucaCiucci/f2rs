
use super::*;

#[derive(Debug, Clone)]
pub struct AccessStmt<Span> {
    pub access_spec: AccessSpec,
    pub access_id_list: Option<Vec<AccessId<Span>>>,
}

#[doc = s_rule!(
    F18V007r1 rule "access-stmt" #827 : "is access-spec [ [ :: ] access-id-list ]",
)]
pub fn access_stmt_2<S: Lexed>(source: S) -> PResult<AccessStmt<MultilineSpan>, S> {
    (
        access_spec,
        (
            double_colon(),
            list(access_id, 0..),
        )
            .map(|(_, access_id_list)| access_id_list)
            .optional(),
    ).map(|(access_spec, access_id_list)| AccessStmt {
        access_spec,
        access_id_list,
    }).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum AccessId<Span> {
    AccessName(Name<Span>),
    GenericSpec(GenericSpec<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "access-id" #828 :
    "is access-name"
    "or generic-spec",
)]
pub fn access_id<S: Lexed>(source: S) -> PResult<AccessId<MultilineSpan>, S> {
    alt!(
        for S =>
        generic_spec.map(AccessId::GenericSpec),
        name().map(AccessId::AccessName),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct AllocatableStmt<Span> {
    pub allocatable_decl_list: Vec<AllocatableDecl<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "allocatable-stmt" #829 : "is ALLOCATABLE [ :: ] allocatable-decl-list",
)]
pub fn allocatable_stmt_2<S: Lexed>(source: S) -> PResult<AllocatableStmt<MultilineSpan>, S> {
    (
        kw!(allocatable),
        double_colon().optional(),
        list(allocatable_decl, 1..),
    ).map(|(_, _, allocatable_decl_list)| AllocatableStmt {
        allocatable_decl_list,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct AllocatableDecl<Span> {
    pub object_name: ObjectName<Span>,
    pub array_spec: Option<ArraySpec<Span>>,
    pub coarray_spec: Option<CoarraySpec<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "allocatable-decl" #830 :
    "is object-name [ ( array-spec ) ] [ lbracket coarray-spec rbracket ]",
)]
pub fn allocatable_decl<S: Lexed>(source: S) -> PResult<AllocatableDecl<MultilineSpan>, S> {
    (
        object_name,
        (
            delim('('),
            array_spec,
            delim(')'),
        ).map(|(_, array_spec, _)| array_spec).optional(),
        (
            delim('['),
            coarray_spec,
            delim(']'),
        ).map(|(_, coarray_spec, _)| coarray_spec).optional(),
    ).map(|(object_name, array_spec, coarray_spec)| AllocatableDecl {
        object_name,
        array_spec,
        coarray_spec,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct AsynchronousStmt<Span> {
    pub object_name_list: Vec<ObjectName<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "asynchronous-stmt" #831 : "is ASYNCHRONOUS [ :: ] object-name-list",
)]
pub fn asynchronous_stmt_2<S: Lexed>(source: S) -> PResult<AsynchronousStmt<MultilineSpan>, S> {
    (
        kw!(asynchronous),
        double_colon().optional(),
        list(object_name, 1..),
    ).map(|(_, _, object_name_list)| AsynchronousStmt {
        object_name_list,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct BindStmt<Span> {
    pub language_binding_spec: LanguageBindingSpec<Span>,
    pub bind_entity_list: Vec<BindEntity<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "bind-stmt" #832 : "is language-binding-spec [ :: ] bind-entity-list",
)]
pub fn bind_stmt_2<S: Lexed>(source: S) -> PResult<BindStmt<MultilineSpan>, S> {
    (
        language_binding_spec,
        double_colon().optional(),
        list(bind_entity, 1..),
    ).map(|(language_binding_spec, _, bind_entity_list)| BindStmt {
        language_binding_spec,
        bind_entity_list,
    }).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum BindEntity<Span> {
    EntityName(Name<Span>),
    CommonBlockName(Name<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "bind-entity" #833 :
    "is entity-name"
    "or / common-block-name /",
)]
pub fn bind_entity<S: Lexed>(source: S) -> PResult<BindEntity<MultilineSpan>, S> {
    alt!(
        for S =>
        name().map(BindEntity::EntityName),
        (
            op("/"),
            name(),
            op("/"),
        ).map(|(_, common_block_name, _)| BindEntity::CommonBlockName(common_block_name)),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct CodimensionStmt<Span> {
    pub codimension_decl_list: Vec<CodimensionDecl<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "codimension-stmt" #834 : "is CODIMENSION [ :: ] codimension-decl-list",
)]
pub fn codimension_stmt_2<S: Lexed>(source: S) -> PResult<CodimensionStmt<MultilineSpan>, S> {
    (
        kw!(codimension),
        double_colon().optional(),
        list(codimension_decl, 1..),
    ).map(|(_, _, codimension_decl_list)| CodimensionStmt {
        codimension_decl_list,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct CodimensionDecl<Span> {
    pub coarray_name: Name<Span>,
    pub coarray_spec: CoarraySpec<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "codimension-decl" #835 : "is coarray-name lbracket coarray-spec rbracket",
)]
pub fn codimension_decl<S: Lexed>(source: S) -> PResult<CodimensionDecl<MultilineSpan>, S> {
    (
        name(),
        (
            delim('['),
            coarray_spec,
            delim(']'),
        ).map(|(_, coarray_spec, _)| coarray_spec),
    ).map(|(coarray_name, coarray_spec)| CodimensionDecl {
        coarray_name,
        coarray_spec,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct ContiguousStmt<Span> {
    pub object_name_list: Vec<Name<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "contiguous-stmt" #836 : "is CONTIGUOUS [ :: ] object-name-list",
)]
pub fn contiguous_stmt_2<S: Lexed>(source: S) -> PResult<ContiguousStmt<MultilineSpan>, S> {
    (
        kw!(contiguous),
        double_colon().optional(),
        list(name(), 1..),
    ).map(|(_, _, object_name_list)| ContiguousStmt {
        object_name_list,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct DataStmt<Span> {
    pub data_stmt_set_list: Vec<DataStmtSet<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "data-stmt" #837 : "is DATA data-stmt-set [ [ , ] data-stmt-set ] ...",
)]
pub fn data_stmt<S: Lexed>(source: S) -> PResult<DataStmt<MultilineSpan>, S> {
    (
        kw!(data),
        list(data_stmt_set, 1..),
    ).map(|(_, data_stmt_set_list)| DataStmt {
        data_stmt_set_list,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct DataStmtSet<Span> {
    pub data_stmt_object_list: Vec<DataStmtObject<Span>>,
    pub data_stmt_value_list: Vec<DataStmtValue<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "data-stmt-set" #838 : "is data-stmt-object-list / data-stmt-value-list /",
)]
pub fn data_stmt_set<S: Lexed>(source: S) -> PResult<DataStmtSet<MultilineSpan>, S> {
    (
        list(data_stmt_object, 0..),
        op("/"),
        list(data_stmt_value, 0..),
        op("/"),
    ).map(|(data_stmt_object_list, _, data_stmt_value_list, _)| DataStmtSet {
        data_stmt_object_list,
        data_stmt_value_list,
    }).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum DataStmtObject<Span> {
    Variable(Variable<Span>),
    DataImpliedDo(DataImpliedDo<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "data-stmt-object" #839 :
    "is variable"
    "or data-implied-do",
)]
pub fn data_stmt_object<S: Lexed>(source: S) -> PResult<DataStmtObject<MultilineSpan>, S> {
    alt!(
        for S =>
        variable(false).map(DataStmtObject::Variable),
        data_implied_do.map(DataStmtObject::DataImpliedDo),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct DataImpliedDo<Span> {
    pub data_i_do_object_list: Vec<DataIDoObject<Span>>,
    pub data_implied_do_control: DataImpliedDoControl<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "data-implied-do" #840 :
    "is ( data-i-do-object-list , [ integer-type-spec :: ] data-i-do-variable = scalar-int-constant-expr , scalar-int-constant-expr [ , scalar-int-constant-expr ] )",
)]
pub fn data_implied_do<S: Lexed>(source: S) -> PResult<DataImpliedDo<MultilineSpan>, S> {
    (
        delim('('),
        data_implied_do_inner,
        delim(')'),
    ).map(|(_, data_implied_do, _)| data_implied_do).parse(source)
}

fn data_implied_do_inner<S: Lexed>(source: S) -> PResult<DataImpliedDo<MultilineSpan>, S> {
    let (r, source) = many_until(
        (data_i_do_object, comma()).map(|(data_i_do_object, _)| data_i_do_object),
        data_implied_do_control,
        0..,
    ).parse(source)?;
    let (data_i_do_object_list, data_implied_do_control) = r;
    let Some(data_implied_do_control) = data_implied_do_control else {
        return None;
    };
    Some((DataImpliedDo {
        data_i_do_object_list,
        data_implied_do_control,
    }, source))
}

#[derive(Debug, Clone)]
pub struct DataImpliedDoControl<Span> {
    pub integer_type_spec: Option<IntegerTypeSpec<Span>>,
    pub data_i_do_variable: DataIDoVariable<Span>,
    pub int_constant_expr1: IntConstantExpr<Span>,
    pub int_constant_expr2: IntConstantExpr<Span>,
    pub int_constant_expr3: Option<IntConstantExpr<Span>>,
}

fn data_implied_do_control<S: Lexed>(source: S) -> PResult<DataImpliedDoControl<MultilineSpan>, S> {
    (
        (integer_type_spec, double_colon()).map(|(integer_type_spec, _)| integer_type_spec).optional(),
        data_i_do_variable,
        equals(),
        int_constant_expr,
        comma(),
        int_constant_expr,
        (
            comma(),
            int_constant_expr,
        ).map(|(_, int_constant_expr)| int_constant_expr).optional(),
    ).map(|(integer_type_spec, data_i_do_variable, _, int_constant_expr1, _, int_constant_expr2, int_constant_expr3)| DataImpliedDoControl {
        integer_type_spec,
        data_i_do_variable,
        int_constant_expr1,
        int_constant_expr2,
        int_constant_expr3,
    }).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum DataIDoObject<Span> {
    ArrayElement(ArrayElement<Span>),
    ScalarStructureComponent(StructureComponent<Span>),
    DataImpliedDo(DataImpliedDo<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "data-i-do-object" #841 :
    "is array-element"
    "or scalar-structure-component"
    "or data-implied-do",
)]
pub fn data_i_do_object<S: Lexed>(source: S) -> PResult<DataIDoObject<MultilineSpan>, S> {
    alt!(
        for S =>
        array_element.map(DataIDoObject::ArrayElement),
        structure_component.map(DataIDoObject::ScalarStructureComponent),
        data_implied_do.map(DataIDoObject::DataImpliedDo),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct DataIDoVariable<Span>(pub DoVariable<Span>);

#[doc = s_rule!(
    F18V007r1 rule "data-i-do-variable" #842 : "is do-variable",
)]
pub fn data_i_do_variable<S: Lexed>(source: S) -> PResult<DataIDoVariable<MultilineSpan>, S> {
    do_variable.map(DataIDoVariable).parse(source)
}

#[derive(Debug, Clone)]
pub struct DataStmtValue<Span> {
    pub data_stmt_repeat: Option<DataStmtRepeat<Span>>,
    pub data_stmt_constant: DataStmtConstant<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "data-stmt-value" #843 : "is [ data-stmt-repeat * ] data-stmt-constant",
)]
pub fn data_stmt_value<S: Lexed>(source: S) -> PResult<DataStmtValue<MultilineSpan>, S> {
    (
        (
            data_stmt_repeat,
            asterisk(),
        ).map(|(data_stmt_repeat, _)| data_stmt_repeat).optional(),
        data_stmt_constant,
    ).map(|(data_stmt_repeat, data_stmt_constant)| DataStmtValue {
        data_stmt_repeat,
        data_stmt_constant,
    }).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum DataStmtRepeat<Span> {
    ScalarIntConstant(IntConstant<Span>),
    ScalarIntConstantSubobject(IntConstantSubobject<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "data-stmt-repeat" #844 :
    "is scalar-int-constant"
    "or scalar-int-constant-subobject",
)]
pub fn data_stmt_repeat<S: Lexed>(source: S) -> PResult<DataStmtRepeat<MultilineSpan>, S> {
    alt!(
        for S =>
        int_constant.map(DataStmtRepeat::ScalarIntConstant),
        int_constant_subobject.map(DataStmtRepeat::ScalarIntConstantSubobject),
    ).parse(source)
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

#[doc = s_rule!(
    F18V007r1 rule "data-stmt-constant" #845 :
    "is scalar-constant"
    "or scalar-constant-subobject"
    "or signed-int-literal-constant"
    "or signed-real-literal-constant"
    "or null-init"
    "or initial-data-target"
    "or structure-constructor",
)]
pub fn data_stmt_constant<S: Lexed>(source: S) -> PResult<DataStmtConstant<MultilineSpan>, S> {
    alt!(
        for S =>
        constant.map(DataStmtConstant::ScalarConstant),
        constant_subobject.map(DataStmtConstant::ScalarConstantSubobject),
        // TODO maybe scalar already handles this? signed_int_literal_constant.map(DataStmtConstant::SignedIntLiteralConstant),
        // TODO maybe scalar already handles this? signed_real_literal_constant.map(DataStmtConstant::SignedRealLiteralConstant),
        null_init.map(DataStmtConstant::NullInit),
        initial_data_target.map(DataStmtConstant::InitialDataTarget),
        structure_constructor.map(DataStmtConstant::StructureConstructor),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct IntConstantSubobject<Span>(pub ConstantSubobject<Span>);

#[doc = s_rule!(
    F18V007r1 rule "int-constant-subobject" #846 : "is constant-subobject",
)]
pub fn int_constant_subobject<S: Lexed>(source: S) -> PResult<IntConstantSubobject<MultilineSpan>, S> {
    constant_subobject.map(IntConstantSubobject).parse(source)
}

#[derive(Debug, Clone)]
pub struct ConstantSubobject<Span>(pub Designator<Span>);

#[doc = s_rule!(
    F18V007r1 rule "constant-subobject" #847 : "is designator",
)]
pub fn constant_subobject<S: Lexed>(source: S) -> PResult<ConstantSubobject<MultilineSpan>, S> {
    designator(false).map(ConstantSubobject).parse(source)
}

#[derive(Debug, Clone)]
pub struct DimensionStmt<Span> {
    pub list: Vec<(Name<Span>, ArraySpec<Span>)>,
}

#[doc = s_rule!(
    F18V007r1 rule "dimension-stmt" #848 :
    "is DIMENSION [ :: ] array-name ( array-spec ) [ , array-name ( array-spec ) ] ...",
)]
pub fn dimension_stmt_2<S: Lexed>(source: S) -> PResult<DimensionStmt<MultilineSpan>, S> {
    (
        kw!(dimension),
        double_colon().optional(),
        list(
            (
                name(),
                delim('('),
                array_spec,
                delim(')'),
            ).map(|(name, _, array_spec, _)| (name, array_spec)),
            1..,
        ),
    ).map(|(_, _, list)| DimensionStmt {
        list,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct IntentStmt<Span> {
    pub intent_spec: IntentSpec<Span>,
    pub dummy_arg_name_list: Vec<DummyArgName<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "intent-stmt" #849 : "is INTENT ( intent-spec ) [ :: ] dummy-arg-name-list",
)]
pub fn intent_stmt_2<S: Lexed>(source: S) -> PResult<IntentStmt<MultilineSpan>, S> {
    (
        (kw!(intent), delim('(')),
        intent_spec,
        delim(')'),
        double_colon().optional(),
        list(dummy_arg_name, 0..),
    ).map(|(_, intent_spec, _, _, dummy_arg_name_list)| IntentStmt {
        intent_spec,
        dummy_arg_name_list,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct OptionalStmt<Span> {
    pub dummy_arg_name_list: Vec<DummyArgName<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "optional-stmt" #850 : "is OPTIONAL [ :: ] dummy-arg-name-list",
)]
pub fn optional_stmt_2<S: Lexed>(source: S) -> PResult<OptionalStmt<MultilineSpan>, S> {
    (
        kw!(optional),
        double_colon().optional(),
        list(dummy_arg_name, 0..),
    ).map(|(_, _, dummy_arg_name_list)| OptionalStmt {
        dummy_arg_name_list,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct PointerStmt<Span> {
    pub pointer_decl_list: Vec<PointerDecl<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "pointer-stmt" #853 : "is POINTER [ :: ] pointer-decl-list",
)]
pub fn pointer_stmt_2<S: Lexed>(source: S) -> PResult<PointerStmt<MultilineSpan>, S> {
    (
        kw!(pointer),
        double_colon().optional(),
        list(pointer_decl, 1..),
    ).map(|(_, _, pointer_decl_list)| PointerStmt {
        pointer_decl_list,
    }).parse(source)
}

// TODO maybe enum
#[derive(Debug, Clone)]
pub struct PointerDecl<Span> {
    pub name: Name<Span>,
    pub deferred_shape_spec_list: Option<Vec<DeferredShapeSpec>>,
}

#[doc = s_rule!(
    F18V007r1 rule "pointer-decl" #854 :
    "is object-name [ ( deferred-shape-spec-list ) ]"
    "or proc-entity-name", // TODO maybe this would never match in this form
)]
pub fn pointer_decl<S: Lexed>(source: S) -> PResult<PointerDecl<MultilineSpan>, S> {
    (
        name(),
        (
            delim('('),
            list(deferred_shape_spec, 0..),
            delim(')'),
        ).map(|(_, deferred_shape_spec_list, _)| deferred_shape_spec_list).optional(),
    ).map(|(name, deferred_shape_spec_list)| PointerDecl {
        name,
        deferred_shape_spec_list,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct ProtectedStmt<Span> {
    pub entity_name_list: Vec<Name<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "protected-stmt" #855 : "is PROTECTED [ :: ] entity-name-list",
)]
pub fn protected_stmt_2<S: Lexed>(source: S) -> PResult<ProtectedStmt<MultilineSpan>, S> {
    (
        kw!(protected),
        double_colon().optional(),
        list(name(), 1..),
    ).map(|(_, _, entity_name_list)| ProtectedStmt {
        entity_name_list,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct SaveStmt<Span> {
    pub saved_entity_list: Option<Vec<SavedEntity<Span>>>,
}

#[doc = s_rule!(
    F18V007r1 rule "save-stmt" #856 : "SAVE [ [ :: ] saved-entity-list ]",
)]
pub fn save_stmt_2<S: Lexed>(source: S) -> PResult<SaveStmt<MultilineSpan>, S> {
    (
        kw!(save),
        (
            double_colon().optional(),
            list(saved_entity, 0..),
        ).map(|(_, saved_entity_list)| saved_entity_list).optional(),
    ).map(|(_, saved_entity_list)| SaveStmt {
        saved_entity_list,
    }).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum SavedEntity<Span> {
    ObjectName(ObjectName<Span>),
    ProcPointerName(ProcPointerName<Span>),
    CommonBlockName(Name<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "saved-entity" #857 :
    "is object-name"
    "or proc-pointer-name"
    "or / common-block-name /",
)]
pub fn saved_entity<S: Lexed>(source: S) -> PResult<SavedEntity<MultilineSpan>, S> {
    alt!(
        for S =>
        object_name.map(SavedEntity::ObjectName),
        proc_pointer_name.map(SavedEntity::ProcPointerName),
        (
            op("/"),
            name(),
            op("/"),
        ).map(|(_, common_block_name, _)| SavedEntity::CommonBlockName(common_block_name)),
    ).parse(source)
}

#[cfg(test)]
mod tests {
    use crate::rule_test;

    use super::*;

    rule_test! {
        access_stmt(F18V007r1 827) {
            examples(|s| access_stmt_2(s), [
                "public",
                "public :: a",
                "public :: a, b",
            ]);
        }
    }

    rule_test! {
        access_id(F18V007r1 828) {
            examples(|s| access_id(s), [
                "a",
                "b",
                "OPERATOR (.foo.)",
                "ASSIGNMENT (=)",
                "READ (FORMATTED)",
                "READ (UNFORMATTED)",
                "WRITE (FORMATTED)",
                "WRITE (UNFORMATTED)",
            ]);
        }
    }

    rule_test! {
        allocatable_stmt(F18V007r1 829) {
            examples(|s| allocatable_stmt_2(s), [
                "allocatable a",
                "allocatable a, b",
                "allocatable :: a",
                "allocatable :: a, b",
            ]);
        }
    }

    rule_test! {
        allocatable_decl(F18V007r1 830) {
            examples(|s| allocatable_decl(s), [
                "a",
                "a(1)",
                "a(1, 2)",
                "a(1, 2, 3)",
                "a(1, 2, 3)[*]",
                "a(1, 2, 3)[1,*]",
                "a(1, 2, 3)[1:1,*]",
            ]);
        }
    }

    rule_test! {
        asynchronous_stmt(F18V007r1 831) {
            examples(|s| asynchronous_stmt_2(s), [
                "asynchronous a",
                "asynchronous a, b",
                "asynchronous :: a",
                "asynchronous :: a, b",
            ]);
        }
    }

    rule_test! {
        bind_stmt(F18V007r1 832) {
            examples(|s| bind_stmt_2(s), [
                "bind(c) a",
                "bind(c) :: a",
                "bind(c) :: a, /b/",
            ]);
        }
    }

    rule_test! {
        bind_entity(F18V007r1 833) {
            examples(|s| bind_entity(s), [
                "a",
                "/b/",
            ]);
        }
    }

    rule_test! {
        codimension_stmt(F18V007r1 834) {
            examples(|s| codimension_stmt_2(s), [
                "codimension a[*]",
                "codimension a[:]",
                "codimension :: a[*]",
                "codimension :: a[1:*]",
                "codimension :: a[1:*], b[1:1,*]",
            ]);
        }
    }

    rule_test! {
        codimension_decl(F18V007r1 835) {
            examples(|s| codimension_decl(s), [
                "a[*]",
                "a[:]",
                "a[1:*]",
                "a[1:*]",
            ]);
        }
    }

    rule_test! {
        contiguous_stmt(F18V007r1 836) {
            examples(|s| contiguous_stmt_2(s), [
                "contiguous a",
                "contiguous a, b",
                "contiguous :: a",
                "contiguous :: a, b",
            ]);
        }
    }

    rule_test! {
        data_stmt(F18V007r1 837) {
            examples(|s| data_stmt(s), [
                "data a / 1 /",
                "data a / 1 /, b / 2 /",
                // TODO ...
            ]);
        }
    }

    rule_test! {
        data_stmt_set(F18V007r1 838) {
            examples(|s| data_stmt_set(s), [
                "a / 1 /",
                "a / 1, 2 /",
                // TODO ...
            ]);
        }
    }

    rule_test! {
        data_stmt_object(F18V007r1 839) {
            examples(|s| data_stmt_object(s), [
                "a",
                "(a, I = 1, 1, 1 )",
                // TODO ...
            ]);
        }
    }

    rule_test! {
        data_implied_do(F18V007r1 840) {
            examples(|s| data_implied_do(s), [
                "(a, I = 1, 1, 1 )",
            ]);
        }
    }

    rule_test! {
        data_implied_do_control() {
            examples(|s| data_implied_do_control(s), [
                "I = 1, 1, 1",
                "I = 1, 1",
            ]);
        }
    }

    rule_test! {
        data_i_do_object(F18V007r1 841) {
            examples(|s| data_i_do_object(s), [
                "a",
                "(a, I = 1, 1, 1 )",
                // TODO ...
            ]);
        }
    }

    rule_test! {
        data_i_do_variable(F18V007r1 842) {
            examples(|s| data_i_do_variable(s), [
                "I",
            ]);
        }
    }

    rule_test! {
        data_stmt_value(F18V007r1 843) {
            examples(|s| data_stmt_value(s), [
                "1",
                "1* 1",
                // TODO ...
            ]);
        }
    }

    rule_test! {
        data_stmt_repeat(F18V007r1 844) {
            examples(|s| data_stmt_repeat(s), [
                "1",
                // TODO ...
            ]);
        }
    }

    rule_test! {
        data_stmt_constant(F18V007r1 845) {
            examples(|s| data_stmt_constant(s), [
                "1",
                "1.0",
                "1.0d0",
            ]);
        }
    }

    rule_test! {
        int_constant_subobject(F18V007r1 846) {
            examples(|s| int_constant_subobject(s), [
                "a",
                // TODO ...
            ]);
        }
    }

    rule_test! {
        constant_subobject(F18V007r1 847) {
            examples(|s| constant_subobject(s), [
                "a",
                // TODO ...
            ]);
        }
    }

    rule_test! {
        dimension_stmt(F18V007r1 848) {
            examples(|s| dimension_stmt_2(s), [
                "dimension a(1)",
                "dimension a(1), b(1)",
                "dimension :: a(1)",
                "dimension :: a(1), b(1)",
            ]);
        }
    }

    rule_test! {
        intent_stmt(F18V007r1 849) {
            examples(|s| intent_stmt_2(s), [
                "intent(in) a",
                "intent(in) a, b",
                "intent(in) :: a",
                "intent(in) :: a, b",
                "intent(inout) a",
                "intent(inout) a, b",
                "intent(inout) :: a",
                "intent(inout) :: a, b",
                "intent(out) a",
                "intent(out) a, b",
                "intent(out) :: a",
                "intent(out) :: a, b",
            ]);
        }
    }

    rule_test! {
        optional_stmt(F18V007r1 850) {
            examples(|s| optional_stmt_2(s), [
                "optional a",
                "optional a, b",
                "optional :: a",
                "optional :: a, b",
            ]);
        }
    }

    rule_test! {
        pointer_stmt(F18V007r1 853) {
            examples(|s| pointer_stmt_2(s), [
                "pointer a",
                "pointer a, b",
                "pointer :: a",
                "pointer :: a, b",
                "pointer :: a(:), b",
                // TODO ...
            ]);
        }
    }

    rule_test! {
        pointer_decl(F18V007r1 854) {
            examples(|s| pointer_decl(s), [
                "a",
                "a(:)",
                // TODO ...
            ]);
        }
    }

    rule_test! {
        protected_stmt(F18V007r1 855) {
            examples(|s| protected_stmt_2(s), [
                "protected a",
                "protected a, b",
                "protected :: a",
                "protected :: a, b",
                // TODO ...
            ]);
        }
    }

    rule_test! {
        save_stmt(F18V007r1 856) {
            examples(|s| save_stmt_2(s), [
                "save a",
                "save a, b",
                "save :: a",
                "save :: a, b",
                "save :: a, /b/",
                // TODO ...
            ]);
        }
    }

    rule_test! {
        saved_entity(F18V007r1 857) {
            examples(|s| saved_entity(s), [
                "a",
                "b",
                "/c/",
                // TODO ...
            ]);
        }
    }
}
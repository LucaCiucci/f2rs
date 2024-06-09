
use crate::tokens::rules::{DefinedUnaryOrBinaryOp, PowerOp};

use super::*;

#[derive(Debug, Clone)]
pub struct Expr<Span> {
    pub left: Option<(Box<Expr<Span>>, DefinedUnaryOrBinaryOp<Span>)>,
    pub right: Level5Expr<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "expr" #1022 :
    "is [ expr defined-binary-op ] level-5-expr",
)]
pub fn expr<S: Lexed>(source: S) -> PResult<Expr<MultilineSpan>, S> {
    let e5 = level_5_expr;
    let bin_op = defined_unary_or_binary_op(); // TODO binary
    let right_part = {
        let e5 = e5.clone();
        (
            bin_op,
            e5
        ).map(|(op, expr)| (op, expr))
    };

    let (right, mut source) = e5.parse(source)?;
    let mut expr = Expr {
        left: None,
        right,
    };
    loop {
        match right_part.parse(source.clone()) {
            Some(((op, right), new_source)) => {
                source = new_source;
                expr = Expr {
                    left: Some((Box::new(expr), op)),
                    right,
                };
            },
            None => {
                break
            },
        }
    }

    Some((expr, source))
}

#[derive(Debug, Clone)]
pub struct IntExpr<Span>(pub Expr<Span>);


#[doc = s_rule!(
    F18V007r1 rule "int-expr" #1026 :
    "is expr",
)]
pub fn int_expr<S: Lexed>(source: S) -> PResult<IntExpr<MultilineSpan>, S> {
    expr.map(IntExpr).parse(source)
}

#[derive(Debug, Clone)]
pub struct IntConstantExpr<Span>(pub IntExpr<Span>);


#[doc = s_rule!(
    F18V007r1 rule "int-constant-expr" #1031 :
    "is int-expr",
)]
pub fn int_constant_expr<S: Lexed>(source: S) -> PResult<IntConstantExpr<MultilineSpan>, S> {
    int_expr.map(IntConstantExpr).parse(source)
}

#[derive(Debug, Clone)]
pub struct ConstantExpr<Span>(pub Expr<Span>);


#[doc = s_rule!(
    F18V007r1 rule "constant-expr" #1029 :
    "is expr",
)]
pub fn constant_expr<S: Lexed>(source: S) -> PResult<ConstantExpr<MultilineSpan>, S> {
    expr.map(ConstantExpr).parse(source)
}

#[derive(Debug, Clone)]
pub struct DefaultCharConstantExpr<Span>(pub DefaultCharExpr<Span>);


#[doc = s_rule!(
    F18V007r1 rule "default-char-constant-expr" #1030 :
    "is default-char-expr",
)]
pub fn default_char_constant_expr<S: Lexed>(source: S) -> PResult<DefaultCharConstantExpr<MultilineSpan>, S> {
    default_char_expr.map(DefaultCharConstantExpr).parse(source)
}

#[derive(Debug, Clone)]
pub struct SpecificationExpr<Span>(pub IntExpr<Span>);


#[doc = s_rule!(
    F18V007r1 rule "specification-expr" #1028 :
    "is scalar-int-expr",
)]
pub fn specification_expr<S: Lexed>(source: S) -> PResult<SpecificationExpr<MultilineSpan>, S> {
    int_expr.map(SpecificationExpr).parse(source)
}


#[doc = s_rule!(
    F18V007r1 rule "logical-expr" #1024 :
    "is expr",
)]
pub fn logical_expr<S: Lexed>(source: S) -> PResult<Expr<MultilineSpan>, S> {
    expr.parse(source)
}

#[derive(Debug, Clone)]
pub struct DefaultCharExpr<Span>(pub Expr<Span>);


#[doc = s_rule!(
    F18V007r1 rule "default-char-expr" #1025 :
    "is expr",
)]
pub fn default_char_expr<S: Lexed>(source: S) -> PResult<DefaultCharExpr<MultilineSpan>, S> {
    expr.map(DefaultCharExpr).parse(source)
}


#[doc = s_rule!(
    F18V007r1 rule "numeric-expr" #1027 :
    "is expr",
)]
pub fn numeric_expr<S: Lexed>(source: S) -> PResult<Expr<MultilineSpan>, S> {
    expr.parse(source)
}

/// Primary expression
#[derive(Debug, Clone, EnumAsInner)]
pub enum Primary<Span> {
    Literal(LiteralConstant<Span>),
    Designator(Designator<Span>),
    ArrayConstructor(ArrayConstructor<Span>),
    StructureConstructor(StructureConstructor<Span>),
    FunctionReference(FunctionReference<Span>),
    TypeParamInquiry(TypeParamInquiry<Span>),
    TypeParamName(Name<Span>),
    ParenthesizedExpr(Box<Expr<Span>>),
}


#[doc = s_rule!(
    F18V007r1 rule "primary" #1001 :
    "is literal-constant"
    "or designator"
    "or array-constructor"
    "or structure-constructor"
    "or function-reference"
    "or type-param-inquiry"
    "or type-param-name"
    "or ( expr )",
)]
pub fn primary<S: Lexed>(source: S) -> PResult<Primary<MultilineSpan>, S> {
    alt! {
        for S =>
        literal_constant.map(Primary::Literal),
        designator(false).map(Primary::Designator),
        array_constructor.map(Primary::ArrayConstructor),
        structure_constructor.map(Primary::StructureConstructor),
        function_reference.map(Primary::FunctionReference),
        type_param_inquiry.map(Primary::TypeParamInquiry),
        name().map(Primary::TypeParamName),
        (
            delim('('),
            expr,
            delim(')'),
        ).map(|(_, expr, _)| Primary::ParenthesizedExpr(Box::new(expr))),
    }.parse(source)
}

#[derive(Debug, Clone)]
pub struct Level1Expr<Span> {
    pub operator: Option<DefinedUnaryOrBinaryOp<Span>>,
    pub primary: Box<Primary<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "level-1-expr" #1002 : "is [ defined-unary-op ] primary",
)]
pub fn level_1_expr<S: Lexed>(source: S) -> PResult<Level1Expr<MultilineSpan>, S> {
    (
        defined_unary_or_binary_op().optional(), // TODO unary
        primary,
    ).map(|(operator, primary)| Level1Expr {
        operator,
        primary: Box::new(primary),
    })
    .parse(source)
}

#[derive(Debug, Clone)]
pub struct MultOperand<Span> {
    pub expr: Level1Expr<Span>,
    pub exp: Option<(PowerOp<Span>, Box<MultOperand<Span>>)>,
}

#[doc = s_rule!(
    F18V007r1 rule "mult-operand" #1004 : "is level-1-expr [ power-op mult-operand ]",
)]
pub fn mult_operand<S: Lexed>(source: S) -> PResult<MultOperand<MultilineSpan>, S> {
    (
        level_1_expr,
        (
            power_op(),
            mult_operand,
        ).map(|(power_op, mult_operand)| (power_op, Box::new(mult_operand))).optional(),
    ).map(|(expr, exp)| MultOperand {
        expr,
        exp,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct AddOperand<Span> {
    pub left: Option<(Box<AddOperand<Span>>, MultOp<Span>)>,
    pub right: MultOperand<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "add-operand" #1005 : "is [ add-operand mult-op ] mult-operand",
)]
pub fn add_operand<S: Lexed>(source: S) -> PResult<AddOperand<MultilineSpan>, S> {
    let mult_operand = mult_operand;
    let add_op = mult_op();
    let right_part = {
        let mult_operand = mult_operand.clone();
        (
            add_op,
            mult_operand,
        ).map(|(op, expr)| (op, expr))
    };

    let (right, mut source) = mult_operand.parse(source)?;
    let mut expr = AddOperand {
        left: None,
        right,
    };
    loop {
        match right_part.parse(source.clone()) {
            Some(((op, right), new_source)) => {
                source = new_source;
                expr = AddOperand {
                    left: Some((Box::new(expr), op)),
                    right,
                };
            },
            None => {
                break
            },
        }
    }

    Some((expr, source))
}

#[derive(Debug, Clone)]
pub struct Level2Expr<Span> {
    pub left: Option<(Option<Box<Level2Expr<Span>>>, AddOp<Span>)>,
    pub right: AddOperand<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "level-2-expr" #1006 : "is [ [ level-2-expr ] add-op ] add-operand",
)]
pub fn level_2_expr<S: Lexed>(source: S) -> PResult<Level2Expr<MultilineSpan>, S> {
    let add_operand = add_operand;
    let add_op = add_op();
    let right_part = {
        let add_op = add_op.clone();
        let add_operand = add_operand.clone();
        (
            add_op,
            add_operand,
        ).map(|(op, expr)| (op, expr))
    };

    let (lop, source) = match add_op.parse(source.clone()) {
        Some((op, source)) => (Some(op), source),
        None => (None, source),
    };
    let (right, mut source) = add_operand.parse(source)?;
    let mut expr = Level2Expr {
        left: lop.map(|op| (None, op)),
        right,
    };
    loop {
        match right_part.parse(source.clone()) {
            Some(((op, right), new_source)) => {
                source = new_source;
                expr = Level2Expr {
                    left: Some((Some(Box::new(expr)), op)),
                    right,
                };
            },
            None => {
                break
            },
        }
    }

    Some((expr, source))
}

#[derive(Debug, Clone)]
pub struct Level3Expr<Span> {
    pub left: Option<(Box<Level3Expr<Span>>, ConcatOp<Span>)>,
    pub right: Level2Expr<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "level-3-expr" #1010 : "is [ level-3-expr concat-op ] level-2-expr",
)]
pub fn level_3_expr<S: Lexed>(source: S) -> PResult<Level3Expr<MultilineSpan>, S> {
    let level_2_expr = level_2_expr;
    let concat_op = concat_op();
    let right_part = {
        let level_2_expr = level_2_expr.clone();
        (
            concat_op,
            level_2_expr,
        ).map(|(op, expr)| (op, expr))
    };

    let (right, mut source) = level_2_expr.parse(source)?;
    let mut expr = Level3Expr {
        left: None,
        right,
    };
    loop {
        match right_part.parse(source.clone()) {
            Some(((op, right), new_source)) => {
                source = new_source;
                expr = Level3Expr {
                    left: Some((Box::new(expr), op)),
                    right,
                };
            },
            None => {
                break
            },
        }
    }

    Some((expr, source))
}

#[derive(Debug, Clone)]
pub struct Level4Expr<Span> {
    pub left: Option<(Box<Level4Expr<Span>>, RelOp<Span>)>,
    pub right: Level3Expr<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "level-4-expr" #1012 : "is [ level-3-expr rel-op ] level-3-expr",
)]
pub fn level_4_expr<S: Lexed>(source: S) -> PResult<Level4Expr<MultilineSpan>, S> {
    let level_3_expr = level_3_expr;
    let rel_op = rel_op();
    let right_part = {
        let level_3_expr = level_3_expr.clone();
        (
            rel_op,
            level_3_expr,
        ).map(|(op, expr)| (op, expr))
    };

    let (right, mut source) = level_3_expr.parse(source)?;
    let mut expr = Level4Expr {
        left: None,
        right,
    };
    loop {
        match right_part.parse(source.clone()) {
            Some(((op, right), new_source)) => {
                source = new_source;
                expr = Level4Expr {
                    left: Some((Box::new(expr), op)),
                    right,
                };
            },
            None => {
                break
            },
        }
    }

    Some((expr, source))
}

#[derive(Debug, Clone)]
pub struct AndOperand<Span> {
    pub operator: Option<NotOp<Span>>,
    pub expr: Level4Expr<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "and-operand" #1014 : "is [ not-op ] level-4-expr",
)]
pub fn and_operand<S: Lexed>(source: S) -> PResult<AndOperand<MultilineSpan>, S> {
    (
        not_op().optional(),
        level_4_expr,
    ).map(|(operator, expr)| AndOperand {
        operator,
        expr,
    })
    .parse(source)
}

#[derive(Debug, Clone)]
pub struct OrOperand<Span> {
    pub left: Option<(Box<OrOperand<Span>>, AndOp<Span>)>,
    pub right: AndOperand<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "or-operand" #1015 : "is [ or-operand and-op ] and-operand",
)]
pub fn or_operand<S: Lexed>(source: S) -> PResult<OrOperand<MultilineSpan>, S> {
    let and_operand = and_operand;
    let and_op = and_op();
    let right_part = {
        let and_operand = and_operand.clone();
        (
            and_op,
            and_operand,
        ).map(|(op, expr)| (op, expr))
    };

    let (right, mut source) = and_operand.parse(source)?;
    let mut expr = OrOperand {
        left: None,
        right,
    };
    loop {
        match right_part.parse(source.clone()) {
            Some(((op, right), new_source)) => {
                source = new_source;
                expr = OrOperand {
                    left: Some((Box::new(expr), op)),
                    right,
                };
            },
            None => {
                break
            },
        }
    }

    Some((expr, source))
}

#[derive(Debug, Clone)]
pub struct EquivOperand<Span> {
    pub left: Option<(Box<EquivOperand<Span>>, OrOp<Span>)>,
    pub right: OrOperand<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "equiv-operand" #1016 : "is [ equiv-operand or-op ] or-operand",
)]
pub fn equiv_operand<S: Lexed>(source: S) -> PResult<EquivOperand<MultilineSpan>, S> {
    let or_operand = or_operand;
    let or_op = or_op();
    let right_part = {
        let or_operand = or_operand.clone();
        (
            or_op,
            or_operand,
        ).map(|(op, expr)| (op, expr))
    };

    let (right, mut source) = or_operand.parse(source)?;
    let mut expr = EquivOperand {
        left: None,
        right,
    };
    loop {
        match right_part.parse(source.clone()) {
            Some(((op, right), new_source)) => {
                source = new_source;
                expr = EquivOperand {
                    left: Some((Box::new(expr), op)),
                    right,
                };
            },
            None => {
                break
            },
        }
    }

    Some((expr, source))
}

#[derive(Debug, Clone)]
pub struct Level5Expr<Span> {
    pub left: Option<(Box<Level5Expr<Span>>, EquivOp<Span>)>,
    pub right: EquivOperand<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "level-5-expr" #1017 : "is [ level-5-expr equiv-op ] equiv-operand",
)]
pub fn level_5_expr<S: Lexed>(source: S) -> PResult<Level5Expr<MultilineSpan>, S> {
    let equiv_operand = equiv_operand;
    let equiv_op = equiv_op();
    let right_part = {
        let equiv_operand = equiv_operand.clone();
        (
            equiv_op,
            equiv_operand,
        ).map(|(op, expr)| (op, expr))
    };

    let (right, mut source) = equiv_operand.parse(source)?;
    let mut expr = Level5Expr {
        left: None,
        right,
    };
    loop {
        match right_part.parse(source.clone()) {
            Some(((op, right), new_source)) => {
                source = new_source;
                expr = Level5Expr {
                    left: Some((Box::new(expr), op)),
                    right,
                };
            },
            None => {
                break
            },
        }
    }

    Some((expr, source))
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum Designator<Span> {
    ObjectName(ObjectName<Span>),
    ArrayElement(ArrayElement<Span>),
    ArraySection(ArraySection<Span>),
    CoindexedNamedObject(CoindexedNamedObject<Span>),
    ComplexPartDesignator(ComplexPartDesignator<Span>),
    StructureComponent(StructureComponent<Span>),
    Substring(Substring<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "designator" #901 :
    "is object-name"
    "or array-element"
    "or array-section"
    "or coindexed-named-object"
    "or complex-part-designator"
    "or structure-component"
    "or substring",
)]
pub fn designator<S: Lexed>(
    not_complex_part_designator: bool,
) -> impl Parser<S, Token = Designator<MultilineSpan>> {
    alt!(
        for S =>
        object_name.map(Designator::ObjectName),
        array_element.map(Designator::ArrayElement),
        array_section(not_complex_part_designator).map(Designator::ArraySection),
        coindexed_named_object.map(Designator::CoindexedNamedObject),
        complex_part_designator.map(Designator::ComplexPartDesignator).if_(!not_complex_part_designator),
        structure_component.map(Designator::StructureComponent),
        substring.map(Designator::Substring),
    )
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum Variable<Span> {
    Designator(Designator<Span>),
    FunctionReference(FunctionReference<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "variable" #902 :
    "is designator"
    "or function-reference",
)]
pub fn variable<S: Lexed>(
    not_function_reference: bool,
) -> impl Parser<S, Token = Variable<MultilineSpan>> {
    alt!(
        for S =>
        designator(false).map(Variable::Designator),
        function_reference.map(Variable::FunctionReference).if_(!not_function_reference),
    )
}

#[derive(Debug, Clone)]
pub struct VariableName<Span>(pub Name<Span>);

#[doc = s_rule!(
    F18V007r1 rule "variable-name" #903 : "is name",
)]
pub fn variable_name<S: Lexed>(source: S) -> PResult<VariableName<MultilineSpan>, S> {
    name().map(VariableName).parse(source)
}

#[derive(Debug, Clone)]
pub struct LogicalVariable<Span>(pub Variable<Span>);

#[doc = s_rule!(
    F18V007r1 rule "logical-variable" #904 : "is variable",
)]
pub fn logical_variable<S: Lexed>(
    not_function_reference: bool,
) -> impl Parser<S, Token = LogicalVariable<MultilineSpan>> {
    variable(not_function_reference).map(LogicalVariable)
}

#[derive(Debug, Clone)]
pub struct CharVariable<Span>(pub Variable<Span>);

#[doc = s_rule!(
    F18V007r1 rule "char-variable" #905 : "is variable",
)]
pub fn char_variable<S: Lexed>(
    not_function_reference: bool,
) -> impl Parser<S, Token = CharVariable<MultilineSpan>> {
    variable(not_function_reference).map(CharVariable)
}

#[derive(Debug, Clone)]
pub struct DefaultCharVariable<Span>(pub Variable<Span>);

#[doc = s_rule!(
    F18V007r1 rule "default-char-variable" #906 : "is variable",
)]
pub fn default_char_variable<S: Lexed>(
    not_function_reference: bool,
) -> impl Parser<S, Token = DefaultCharVariable<MultilineSpan>> {
    variable(not_function_reference).map(DefaultCharVariable)
}

#[derive(Debug, Clone)]
pub struct IntVariable<Span>(pub Variable<Span>);

#[doc = s_rule!(
    F18V007r1 rule "int-variable" #907 : "is variable",
)]
pub fn int_variable<S: Lexed>(
    not_function_reference: bool,
) -> impl Parser<S, Token = IntVariable<MultilineSpan>> {
    variable(not_function_reference).map(IntVariable)
}

#[derive(Debug, Clone)]
pub struct Substring<Span> {
    pub parent: ParentString<Span>,
    pub range: SubstringRange<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "substring" #908 : "is parent-string ( substring-range )",
)]
pub fn substring<S: Lexed>(source: S) -> PResult<Substring<MultilineSpan>, S> {
    (
        parent_string,
        delim('('),
        substring_range,
        delim(')'),
    ).map(|(parent, _, range, _)| Substring {
        parent,
        range,
    }).parse(source)
}



#[derive(Debug, Clone, EnumAsInner)]
pub enum ParentString<Span> {
    ScalarVariable(VariableName<Span>),
    ArrayElement(ArrayElement<Span>),
    CoindexedNamedObject(CoindexedNamedObject<Span>),
    ScalarStructureComponent(StructureComponent<Span>),
    ScalarConstant(Constant<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "parent-string" #909 :
    "is scalar-variable-name"
    "or array-element"
    "or coindexed-named-object"
    "or scalar-structure-component"
    "or scalar-constant",
)]
pub fn parent_string<S: Lexed>(source: S) -> PResult<ParentString<MultilineSpan>, S> {
    alt!(
        for S =>
        variable_name.map(ParentString::ScalarVariable),
        array_element.map(ParentString::ArrayElement),
        coindexed_named_object.map(ParentString::CoindexedNamedObject),
        structure_component.map(ParentString::ScalarStructureComponent),
        constant.map(ParentString::ScalarConstant),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct SubstringRange<Span> {
    pub left: Option<IntExpr<Span>>,
    pub right: Option<IntExpr<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "substring-range" #910 : "is [ scalar-int-expr ] : [ scalar-int-expr ]",
)]
pub fn substring_range<S: Lexed>(source: S) -> PResult<SubstringRange<MultilineSpan>, S> {
    (
        int_expr.optional(),
        colon(),
        int_expr.optional(),
    ).map(|(left, _, right)| SubstringRange {
        left,
        right,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct DataRef<Span> {
    pub part: PartRef<Span>,
    pub selectors: Vec<PartRef<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "data-ref" #911 : "is part-ref [ % part-ref ] ...",
)]
pub fn data_ref<S: Lexed>(source: S) -> PResult<DataRef<MultilineSpan>, S> {
    (
        part_ref,
        list(
            (percent(), part_ref).map(|(_, part_ref)| part_ref),
            0..,
        ),
    ).map(|(part, selectors)| DataRef {
        part,
        selectors,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct PartRef<Span> {
    pub part_name: Name<Span>,
    pub section_subscript_list: Option<Vec<Subscript<Span>>>,
    pub image_selector: Option<ImageSelector<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "part-ref" #912 : "is part-name [ ( section-subscript-list ) ] [ image-selector ]",
)]
pub fn part_ref<S: Lexed>(source: S) -> PResult<PartRef<MultilineSpan>, S> {
    (
        name(),
        (
            delim('('),
            list(
                subscript,
                0..,
            ),
            delim(')'),
        ).map(|(_, list, _)| list).optional(),
        image_selector.optional(),
    ).map(|(part_name, section_subscript_list, image_selector)| PartRef {
        part_name,
        section_subscript_list,
        image_selector,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct StructureComponent<Span>(pub DataRef<Span>);

#[doc = s_rule!(
    F18V007r1 rule "structure-component" #913 : "is data-ref",
)]
pub fn structure_component<S: Lexed>(source: S) -> PResult<StructureComponent<MultilineSpan>, S> {
    data_ref.map(StructureComponent).parse(source)
}

#[derive(Debug, Clone)]
pub struct CoindexedNamedObject<Span>(pub DataRef<Span>);

#[doc = s_rule!(
    F18V007r1 rule "coindexed-named-object" #914 : "is data-ref",
)]
pub fn coindexed_named_object<S: Lexed>(source: S) -> PResult<CoindexedNamedObject<MultilineSpan>, S> {
    data_ref.map(CoindexedNamedObject).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ComplexPartDesignator<Span> {
    Re(Box<Designator<Span>>),
    Im(Box<Designator<Span>>),
}

#[doc = s_rule!(
    F18V007r1 rule "complex-part-designator" #915 :
    "is designator % RE"
    "or designator % IM",
)]
pub fn complex_part_designator<S: Lexed>(source: S) -> PResult<ComplexPartDesignator<MultilineSpan>, S> {
    alt!(
        for S =>
        (designator(true), percent(), kw!(re)).map(|(designator, _, _)| ComplexPartDesignator::Re(Box::new(designator))),
        (designator(true), percent(), kw!(im)).map(|(designator, _, _)| ComplexPartDesignator::Im(Box::new(designator))),
    )
    .parse(source)
}

#[derive(Debug, Clone)]
pub struct TypeParamInquiry<Span> {
    pub designator: Designator<Span>,
    pub type_param_name: Name<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "type-param-inquiry" #916 : "is designator % type-param-name",
)]
pub fn type_param_inquiry<S: Lexed>(source: S) -> PResult<TypeParamInquiry<MultilineSpan>, S> {
    (
        designator(true),
        percent(),
        name(),
    ).map(|(designator, _, type_param_name)| TypeParamInquiry {
        designator,
        type_param_name,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct ArrayElement<Span>(pub DataRef<Span>);

#[doc = s_rule!(
    F18V007r1 rule "array-element" #917 : "is data-ref",
)]
pub fn array_element<S: Lexed>(source: S) -> PResult<ArrayElement<MultilineSpan>, S> {
    data_ref.map(ArrayElement).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ArraySection<Span> {
    Data(DataRef<Span>, Option<SubstringRange<Span>>),
    ComplexPartDesignator(ComplexPartDesignator<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "array-section" #918 :
    "is data-ref [ ( substring-range ) ]"
    "or complex-part-designator",
)]
pub fn array_section<S: Lexed>(
    not_complex_part_designator: bool,
) -> impl Parser<S, Token = ArraySection<MultilineSpan>> {
    alt!(
        for S =>
        (
            data_ref,
            (
                delim('('),
                substring_range,
                delim(')'),
            ).map(|(_, range, _)| range).optional(),
        ).map(|(data_ref, range)| ArraySection::Data(data_ref, range)),
        complex_part_designator.map(ArraySection::ComplexPartDesignator).if_(!not_complex_part_designator),
    )
}

#[derive(Debug, Clone)]
pub struct Subscript<Span>(pub IntExpr<Span>);

#[doc = s_rule!(
    F18V007r1 rule "subscript" #919 : "is scalar-int-expr",
)]
pub fn subscript<S: Lexed>(source: S) -> PResult<Subscript<MultilineSpan>, S> {
    int_expr.map(Subscript).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum SectionSubscript<Span> {
    Subscript(Subscript<Span>),
    SubscriptTriplet(SubscriptTriplet<Span>),
    VectorSubscript(VectorSubscript<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "section-subscript" #920 :
    "is subscript"
    "or subscript-triplet"
    "or vector-subscript",
)]
pub fn section_subscript<S: Lexed>(source: S) -> PResult<SectionSubscript<MultilineSpan>, S> {
    alt!(
        for S =>
        subscript.map(SectionSubscript::Subscript),
        subscript_triplet.map(SectionSubscript::SubscriptTriplet),
        vector_subscript.map(SectionSubscript::VectorSubscript),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct SubscriptTriplet<Span> {
    pub lower: Option<Subscript<Span>>,
    pub upper: Option<Subscript<Span>>,
    pub stride: Option<Stride<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "subscript-triplet" #921 : "is [ subscript ] : [ subscript ] [ : stride ]",
)]
pub fn subscript_triplet<S: Lexed>(source: S) -> PResult<SubscriptTriplet<MultilineSpan>, S> {
    (
        subscript.optional(),
        colon(),
        subscript.optional(),
        (
            colon(),
            stride,
        ).map(|(_, subscript)| subscript).optional(),
    ).map(|(lower, _, upper, stride)| SubscriptTriplet {
        lower,
        upper,
        stride,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct Stride<Span>(pub IntExpr<Span>);

#[doc = s_rule!(
    F18V007r1 rule "stride" #922 : "is scalar-int-expr",
)]
pub fn stride<S: Lexed>(source: S) -> PResult<Stride<MultilineSpan>, S> {
    int_expr.map(Stride).parse(source)
}

#[derive(Debug, Clone)]
pub struct VectorSubscript<Span>(pub IntExpr<Span>);

#[doc = s_rule!(
    F18V007r1 rule "vector-subscript" #923 : "is int-expr",
)]
pub fn vector_subscript<S: Lexed>(source: S) -> PResult<VectorSubscript<MultilineSpan>, S> {
    int_expr.map(VectorSubscript).parse(source)
}

#[derive(Debug, Clone)]
pub struct ImageSelector<Span> {
    pub cosubscript_list: Vec<Cosubscript<Span>>,
    pub image_selector_spec_list: Vec<ImageSelectorSpec<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "image-selector" #924 : "is lbracket cosubscript-list [ , image-selector-spec-list ] rbracket",
)]
pub fn image_selector<S: Lexed>(source: S) -> PResult<ImageSelector<MultilineSpan>, S> {
    (
        delim('['),
        list(cosubscript, 0..),
        (
            comma(),
            list(image_selector_spec, 0..),
        ).map(|(_, image_selector_spec_list)| image_selector_spec_list),
        delim(']'),
    ).map(|(_, cosubscript_list, image_selector_spec_list, _)| ImageSelector {
        cosubscript_list,
        image_selector_spec_list,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct Cosubscript<Span>(pub IntExpr<Span>);

#[doc = s_rule!(
    F18V007r1 rule "cosubscript" #925 : "is scalar-int-expr",
)]
pub fn cosubscript<S: Lexed>(source: S) -> PResult<Cosubscript<MultilineSpan>, S> {
    int_expr.map(Cosubscript).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ImageSelectorSpec<Span> {
    Stat(StatVariable<Span>),
    Team(TeamValue<Span>),
    TeamNumber(IntExpr<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "image-selector-spec" #926 :
    "is STAT = stat-variable"
    "or TEAM = team-value"
    "or TEAM_NUMBER = scalar-int-expr",
)]
pub fn image_selector_spec<S: Lexed>(source: S) -> PResult<ImageSelectorSpec<MultilineSpan>, S> {
    alt!(
        for S =>
        (kw!(stat), equals(), stat_variable).map(|(_, _, stat_variable)| ImageSelectorSpec::Stat(stat_variable)),
        (kw!(team), equals(), team_value).map(|(_, _, team_value)| ImageSelectorSpec::Team(team_value)),
        (kw!(team_number), equals(), int_expr).map(|(_, _, int_texpr)| ImageSelectorSpec::TeamNumber(int_texpr)),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct TeamValue<Span>(pub Expr<Span>);

#[doc = s_rule!(
    F18V007r1 rule "team-value" #1115 : "is scalar-expr",
)]
pub fn team_value<S: Lexed>(source: S) -> PResult<TeamValue<MultilineSpan>, S> {
    expr.map(TeamValue).parse(source)
}

#[derive(Debug, Clone)]
pub struct AllocateStmt<Span> {
    pub type_spec: Option<TypeSpec<Span>>,
    pub allocation_list: Vec<Allocation<Span>>,
    pub alloc_opt_list: Option<Vec<AllocOpt<Span>>>,
}

#[doc = s_rule!(
    F18V007r1 rule "allocate-stmt" #927 :
    "is ALLOCATE ( [ type-spec :: ] allocation-list [ , alloc-opt-list ] )",
)]
pub fn allocate_stmt_2<S: Lexed>(source: S) -> PResult<AllocateStmt<MultilineSpan>, S> {
    (
        (kw!(allocate)),
        delim('('),
        (
            type_spec,
            double_colon(),
        ).map(|(type_spec, _, )| type_spec).optional(),
        list(allocation, 1..),
        (
            comma(),
            list(alloc_opt, 0..),
        ).map(|(_, alloc_opt_list)| alloc_opt_list).optional(),
        delim(')'),
    ).map(|(_, _, type_spec, allocation_list, alloc_opt_list, _)| AllocateStmt {
        type_spec,
        allocation_list,
        alloc_opt_list,
    }).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum AllocOpt<Span> {
    Errmsg(ErrmsgVariable<Span>),
    Mold(SourceExpr<Span>),
    Source(SourceExpr<Span>),
    Stat(StatVariable<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "alloc-opt" #928 :
    "is ERRMSG = errmsg-variable"
    "or MOLD = source-expr"
    "or SOURCE = source-expr"
    "or STAT = stat-variable",
)]
pub fn alloc_opt<S: Lexed>(source: S) -> PResult<AllocOpt<MultilineSpan>, S> {
    alt!(
        for S =>
        (kw!(ERRMSG), equals(), errmsg_variable).map(|(_, _, errmsg_variable)| AllocOpt::Errmsg(errmsg_variable)),
        (kw!(MOLD), equals(), source_expr).map(|(_, _,  source_expr)| AllocOpt::Mold(source_expr)),
        (kw!(SOURCE), equals(), source_expr).map(|(_, _, source_expr)| AllocOpt::Source(source_expr)),
        (kw!(STAT), equals(), stat_variable).map(|(_, _, stat_variable)| AllocOpt::Stat(stat_variable)),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct ErrmsgVariable<Span>(pub DefaultCharVariable<Span>);

#[doc = s_rule!(
    F18V007r1 rule "errmsg-variable" #929 : "is scalar-default-char-variable",
)]
pub fn errmsg_variable<S: Lexed>(source: S) -> PResult<ErrmsgVariable<MultilineSpan>, S> {
    default_char_variable(false).map(ErrmsgVariable).parse(source)
}

#[derive(Debug, Clone)]
pub struct SourceExpr<Span>(pub Expr<Span>);

#[doc = s_rule!(
    F18V007r1 rule "source-expr" #930 : "is expr",
)]
pub fn source_expr<S: Lexed>(source: S) -> PResult<SourceExpr<MultilineSpan>, S> {
    expr.map(SourceExpr).parse(source)
}

#[derive(Debug, Clone)]
pub struct Allocation<Span> {
    pub object: AllocateObject<Span>,
    pub allocate_shape_spec_list: Option<Vec<AllocateShapeSpec<Span>>>,
    pub allocate_coarray_spec: Option<AllocateCoarraySpec<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "allocation" #931 :
    "is allocate-object [ ( allocate-shape-spec-list ) ] [ lbracket allocate-coarray-spec rbracket ]",
)]
pub fn allocation<S: Lexed>(source: S) -> PResult<Allocation<MultilineSpan>, S> {
    (
        allocate_object,
        (
            delim('('),
            list(allocate_shape_spec, 0..),
            delim(')'),
        ).map(|(_, allocate_shape_spec_list, _)| allocate_shape_spec_list).optional(),
        (
            delim('['),
            allocate_coarray_spec,
            delim(']'),
        ).map(|(_, allocate_coarray_spec, _)| allocate_coarray_spec).optional(),
    ).map(|(object, allocate_shape_spec_list, allocate_coarray_spec)| Allocation {
        object,
        allocate_shape_spec_list,
        allocate_coarray_spec,
    }).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum AllocateObject<Span> {
    VariableName(VariableName<Span>),
    StructureComponent(StructureComponent<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "allocate-object" #932 :
    "is variable-name"
    "or structure-component",
)]
pub fn allocate_object<S: Lexed>(source: S) -> PResult<AllocateObject<MultilineSpan>, S> {
    alt!(
        for S =>
        variable_name.map(AllocateObject::VariableName),
        structure_component.map(AllocateObject::StructureComponent),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct AllocateShapeSpec<Span> {
    pub lower_bound: Option<LowerBoundExpr<Span>>,
    pub upper_bound: UpperBoundExpr<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "allocate-shape-spec" #933 : "is [ lower-bound-expr : ] upper-bound-expr",
)]
pub fn allocate_shape_spec<S: Lexed>(source: S) -> PResult<AllocateShapeSpec<MultilineSpan>, S> {
    (
        (
            lower_bound_expr,
            colon(),
        ).map(|(lower_bound, _)| lower_bound).optional(),
        upper_bound_expr,
    ).map(|(lower_bound, upper_bound)| AllocateShapeSpec {
        lower_bound,
        upper_bound,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct LowerBoundExpr<Span>(pub IntExpr<Span>);

#[doc = s_rule!(
    F18V007r1 rule "lower-bound-expr" #934 : "is scalar-int-expr",
)]
pub fn lower_bound_expr<S: Lexed>(source: S) -> PResult<LowerBoundExpr<MultilineSpan>, S> {
    int_expr.map(LowerBoundExpr).parse(source)
}

#[derive(Debug, Clone)]
pub struct UpperBoundExpr<Span>(pub IntExpr<Span>);

#[doc = s_rule!(
    F18V007r1 rule "upper-bound-expr" #935 : "is scalar-int-expr",
)]
pub fn upper_bound_expr<S: Lexed>(source: S) -> PResult<UpperBoundExpr<MultilineSpan>, S> {
    int_expr.map(UpperBoundExpr).parse(source)
}

#[derive(Debug, Clone)]
pub struct AllocateCoarraySpec<Span> {
    pub allocate_coshape_spec_list: Vec<AllocateCoshapeSpec<Span>>,
    pub lower_bound: Option<LowerBoundExpr<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "allocate-coarray-spec" #936 : "is [ allocate-coshape-spec-list , ] [ lower-bound-expr : ] *",
)]
pub fn allocate_coarray_spec<S: Lexed>(source: S) -> PResult<AllocateCoarraySpec<MultilineSpan>, S> {
    (
        (
            list(allocate_coshape_spec, 0..),
            comma(),
        ).map(|(allocate_coshape_spec_list, _)| allocate_coshape_spec_list).optional(),
        (
            lower_bound_expr,
            colon(),
        ).map(|(lower_bound, _)| lower_bound).optional(),
        asterisk(),
    ).map(|(allocate_coshape_spec_list, lower_bound, _)| AllocateCoarraySpec {
        allocate_coshape_spec_list: allocate_coshape_spec_list.unwrap_or_default(),
        lower_bound,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct AllocateCoshapeSpec<Span> {
    pub lower_bound: Option<LowerBoundExpr<Span>>,
    pub upper_bound: UpperBoundExpr<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "allocate-coshape-spec" #937 : "is [ lower-bound-expr : ] upper-bound-expr",
)]
pub fn allocate_coshape_spec<S: Lexed>(source: S) -> PResult<AllocateCoshapeSpec<MultilineSpan>, S> {
    (
        (
            lower_bound_expr,
            colon(),
        ).map(|(lower_bound, _)| lower_bound).optional(),
        upper_bound_expr,
    ).map(|(lower_bound, upper_bound)| AllocateCoshapeSpec {
        lower_bound,
        upper_bound,
    }).parse(source)
}


#[derive(Debug, Clone)]
pub struct StatVariable<Span>(pub IntVariable<Span>);

#[doc = s_rule!(
    F18V007r1 rule "stat-variable" #942 : "is scalar-int-variable",
)]
pub fn stat_variable<S: Lexed>(source: S) -> PResult<StatVariable<MultilineSpan>, S> {
    int_variable(false).map(StatVariable).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ProcPointerObject<Span> {
    ProcPointerName(ProcPointerName<Span>),
    ProcComponentRef(ProcComponentRef<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "proc-pointer-object" #1038 :
    "is proc-pointer-name"
    "or proc-component-ref",
)]
pub fn proc_pointer_object<S: Lexed>(source: S) -> PResult<ProcPointerObject<MultilineSpan>, S> {
    alt!(
        for S =>
        proc_pointer_name.map(ProcPointerObject::ProcPointerName),
        proc_component_ref.map(ProcPointerObject::ProcComponentRef),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct ProcPointerName<Span>(pub Name<Span>);

#[doc = s_rule!(
    F18V007r1 rule "proc-pointer-name" #858 : "is name",
)]
pub fn proc_pointer_name<S: Lexed>(source: S) -> PResult<ProcPointerName<MultilineSpan>, S> {
    name().map(ProcPointerName).parse(source)
}

#[derive(Debug, Clone)]
pub struct AssignmentStmt<Span> {
    pub variable: Variable<Span>,
    pub expr: Expr<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "assignment-stmt" #1032 : "is variable = expr",
)]
pub fn assignment_stmt_2<S: Lexed>(source: S) -> PResult<AssignmentStmt<MultilineSpan>, S> {
    (
        variable(true),
        (equals()),
        expr,
    ).map(|(variable, _, expr)| AssignmentStmt {
        variable,
        expr,
    }).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum PointerAssignmentStmt<Span> {
    Form1 {
        data_pointer_object: DataPointerObject<Span>,
        bounds_spec_list: Option<Vec<BoundsSpec<Span>>>,
        data_target: DataTarget<Span>,
    },
    Form2 {
        data_pointer_object: DataPointerObject<Span>,
        bounds_remapping_list: Vec<BoundsRemapping<Span>>,
        data_target: DataTarget<Span>,
    },
    Form3 {
        proc_pointer_object: ProcPointerObject<Span>,
        proc_target: ProcTarget<Span>,
    },
}

#[doc = s_rule!(
    F18V007r1 rule "pointer-assignment-stmt" #1033 :
    "is data-pointer-object [ (bounds-spec-list) ] => data-target"
    "or data-pointer-object (bounds-remapping-list ) => data-target"
    "or proc-pointer-object => proc-target",
)]
pub fn pointer_assignment_stmt<S: Lexed>(source: S) -> PResult<PointerAssignmentStmt<MultilineSpan>, S> {
    alt!(
        for S =>
        (
            data_pointer_object,
            (
                delim('('),
                list(bounds_spec, 0..),
                delim(')'),
            ).map(|(_, bounds_spec_list, _)| bounds_spec_list).optional(),
            arrow(),
            data_target,
        ).map(|(data_pointer_object, bounds_spec_list, _, data_target)| PointerAssignmentStmt::Form1 {
            data_pointer_object,
            bounds_spec_list,
            data_target,
        }),
        (
            data_pointer_object,
            delim('('),
            list(bounds_remapping, 0..),
            delim(')'),
            arrow(),
            data_target,
        ).map(|(data_pointer_object, _, bounds_remapping_list, _, _, data_target)| PointerAssignmentStmt::Form2 {
            data_pointer_object,
            bounds_remapping_list,
            data_target,
        }),
        (
            proc_pointer_object,
            arrow(),
            proc_target,
        ).map(|(proc_pointer_object, _, proc_target)| PointerAssignmentStmt::Form3 {
            proc_pointer_object,
            proc_target,
        }),
    ).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum DataPointerObject<Span> {
    VariableName(VariableName<Span>),
    Component {
        scalar_variable: Variable<Span>,
        data_pointer_component_name: Name<Span>,
    },
}

#[doc = s_rule!(
    F18V007r1 rule "data-pointer-object" #1034 :
    "is variable-name"
    "or scalar-variable % data-pointer-component-name",
)]
pub fn data_pointer_object<S: Lexed>(source: S) -> PResult<DataPointerObject<MultilineSpan>, S> {
    alt!(
        for S =>
        variable_name.map(DataPointerObject::VariableName),
        (
            variable(true),
            percent(),
            name(),
        ).map(|(scalar_variable, _, data_pointer_component_name)| DataPointerObject::Component {
            scalar_variable,
            data_pointer_component_name,
        }),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct BoundsSpec<Span> {
    pub lower_bound: LowerBoundExpr<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "bounds-spec" #1035 : "is lower-bound-expr :",
)]
pub fn bounds_spec<S: Lexed>(source: S) -> PResult<BoundsSpec<MultilineSpan>, S> {
    (
        lower_bound_expr,
        colon(),
    ).map(|(lower_bound, _)| BoundsSpec {
        lower_bound,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct BoundsRemapping<Span> {
    pub lower_bound_expr: LowerBoundExpr<Span>,
    pub upper_bound_expr: UpperBoundExpr<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "bounds-remapping" #1036 : "is lower-bound-expr : upper-bound-expr",
)]
pub fn bounds_remapping<S: Lexed>(source: S) -> PResult<BoundsRemapping<MultilineSpan>, S> {
    (
        lower_bound_expr,
        colon(),
        upper_bound_expr,
    ).map(|(lower_bound_expr, _, upper_bound_expr)| BoundsRemapping {
        lower_bound_expr,
        upper_bound_expr,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct DataTarget<Span>(pub Expr<Span>);

#[doc = s_rule!(
    F18V007r1 rule "data-target" #1037 : "is expr",
)]
pub fn data_target<S: Lexed>(source: S) -> PResult<DataTarget<MultilineSpan>, S> {
    expr.map(DataTarget).parse(source)
}

#[derive(Debug, Clone)]
pub struct WhereStmt<Span> {
    pub mask_expr: MaskExpr<Span>,
    pub where_assignment_stmt: WhereAssignmentStmt<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "where-stmt" #1041 : "is WHERE ( mask-expr ) where-assignment-stmt",
)]
pub fn where_stmt<S: Lexed>(source: S) -> PResult<WhereStmt<MultilineSpan>, S> {
    (
        (kw!(WHERE), delim('(')),
        mask_expr,
        delim(')'),
        where_assignment_stmt,
    ).map(|(_, mask_expr, _, where_assignment_stmt)| WhereStmt {
        mask_expr,
        where_assignment_stmt,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct WhereConstructStmt<Span> {
    pub where_construct_name: Option<Name<Span>>,
    pub mask_expr: MaskExpr<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "where-construct-stmt" #1043 : "is [where-construct-name:] WHERE ( mask-expr )",
)]
pub fn where_construct_stmt<S: Lexed>(source: S) -> PResult<WhereConstructStmt<MultilineSpan>, S> {
    (
        (
            name(),
            colon(),
        ).map(|(name, _)| name).optional(),
        (kw!(WHERE), delim('(')),
        mask_expr,
        delim(')'),
    ).map(|(where_construct_name, _, mask_expr, _)| WhereConstructStmt {
        where_construct_name,
        mask_expr,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct WhereAssignmentStmt<Span> {
    pub assignment_stmt: AssignmentStmt<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "where-assignment-stmt" #1045 : "is assignment-stmt",
)]
pub fn where_assignment_stmt<S: Lexed>(source: S) -> PResult<WhereAssignmentStmt<MultilineSpan>, S> {
    assignment_stmt_2
        .map(|assignment_stmt| WhereAssignmentStmt { assignment_stmt })
        .parse(source)
}

#[derive(Debug, Clone)]
pub struct MaskedElsewhereStmt<Span> {
    pub mask_expr: MaskExpr<Span>,
    pub where_construct_name: Option<Name<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "masked-elsewhere-stmt" #1047 : "is ELSEWHERE (mask-expr) [where-construct-name]",
)]
pub fn masked_elsewhere_stmt<S: Lexed>(source: S) -> PResult<MaskedElsewhereStmt<MultilineSpan>, S> {
    (
        (kw!(ELSEWHERE), delim('(')),
        mask_expr,
        delim(')'),
        name().optional(),
    ).map(|(_, mask_expr, _, where_construct_name)| MaskedElsewhereStmt {
        mask_expr,
        where_construct_name,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct ElsewhereStmt<Span> {
    pub where_construct_name: Option<Name<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "elsewhere-stmt" #1048 : "is ELSEWHERE [where-construct-name]",
)]
pub fn elsewhere_stmt<S: Lexed>(source: S) -> PResult<ElsewhereStmt<MultilineSpan>, S> {
    (
        kw!(ELSEWHERE),
        name().optional(),
    ).map(|(_, where_construct_name)| ElsewhereStmt {
        where_construct_name,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct EndWhereStmt<Span> {
    pub where_construct_name: Option<Name<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "end-where-stmt" #1049 : "is END WHERE [where-construct-name]",
)]
pub fn end_where_stmt<S: Lexed>(source: S) -> PResult<EndWhereStmt<MultilineSpan>, S> {
    (
        kw!(END), kw!(WHERE),
        name().optional(),
    ).map(|(_, _, where_construct_name)| EndWhereStmt {
        where_construct_name,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct ForallConstructStmt<Span> {
    pub forall_construct_name: Option<Name<Span>>,
    pub concurrent_header: ConcurrentHeader<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "forall-construct-stmt" #1051 : "is [forall-construct-name :] FORALL concurrent-header",
)]
pub fn forall_construct_stmt<S: Lexed>(source: S) -> PResult<ForallConstructStmt<MultilineSpan>, S> {
    (
        (name(), colon()).map(|(name, _)| name).optional(),
        kw!(forall),
        concurrent_header,
    ).map(|(forall_construct_name, _, concurrent_header)| ForallConstructStmt {
        forall_construct_name,
        concurrent_header,
    }).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum IoUnit<Span> {
    FileUnitNumber(FileUnitNumber<Span>),
    Star,
    InternalFileVariable(InternalFileVariable<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "io-unit" #1201 :
    "is file-unit-number"
    "or *"
    "or internal-file-variable",
)]
pub fn io_unit<S: Lexed>(source: S) -> PResult<IoUnit<MultilineSpan>, S> {
    alt!(
        for S =>
        file_unit_number.map(IoUnit::FileUnitNumber),
        asterisk().map(|_| IoUnit::Star),
        internal_file_variable.map(IoUnit::InternalFileVariable),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct FileUnitNumber<Span>(pub IntExpr<Span>);

#[doc = s_rule!(
    F18V007r1 rule "file-unit-number" #1202 : "is scalar-int-expr",
)]
pub fn file_unit_number<S: Lexed>(source: S) -> PResult<FileUnitNumber<MultilineSpan>, S> {
    int_expr.map(FileUnitNumber).parse(source)
}

#[derive(Debug, Clone)]
pub struct InternalFileVariable<Span>(pub CharVariable<Span>);

#[doc = s_rule!(
    F18V007r1 rule "internal-file-variable" #1203 : "is char-variable",
)]
pub fn internal_file_variable<S: Lexed>(source: S) -> PResult<InternalFileVariable<MultilineSpan>, S> {
    char_variable(true).map(InternalFileVariable).parse(source) // TODO true???
}

#[derive(Debug, Clone)]
pub struct OpenStmt<Span> {
    pub connect_spec_list: Vec<ConnectSpec<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "open-stmt" #1204 : "is OPEN ( connect-spec-list )",
)]
pub fn open_stmt<S: Lexed>(source: S) -> PResult<OpenStmt<MultilineSpan>, S> {
    (
        (kw!(open), delim('(')),
        list(connect_spec, 1..),
        delim(')'),
    ).map(|(_, connect_spec_list, _)| OpenStmt {
        connect_spec_list,
    })
    .parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ConnectSpec<Span> {
    FileUnitNumber(FileUnitNumber<Span>),
    Access(DefaultCharExpr<Span>),
    Action(DefaultCharExpr<Span>),
    Asynchronous(DefaultCharExpr<Span>),
    Blank(DefaultCharExpr<Span>),
    Decimal(DefaultCharExpr<Span>),
    Delim(DefaultCharExpr<Span>),
    Encoding(DefaultCharExpr<Span>),
    Err(Label<Span>),
    File(FileNameExpr<Span>),
    Form(DefaultCharExpr<Span>),
    Iomsg(IomsgVariable<Span>),
    Iostat(StatVariable<Span>),
    Newunit(IntVariable<Span>),
    Pad(DefaultCharExpr<Span>),
    Position(DefaultCharExpr<Span>),
    Recl(IntExpr<Span>),
    Round(DefaultCharExpr<Span>),
    Sign(DefaultCharExpr<Span>),
    Status(DefaultCharExpr<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "connect-spec" #1205 :
    "is [ UNIT = ] file-unit-number"
    "or ACCESS = scalar-default-char-expr"
    "or ACTION = scalar-default-char-expr"
    "or ASYNCHRONOUS = scalar-default-char-expr"
    "or BLANK = scalar-default-char-expr"
    "or DECIMAL = scalar-default-char-expr"
    "or DELIM = scalar-default-char-expr"
    "or ENCODING = scalar-default-char-expr"
    "or ERR = label"
    "or FILE = file-name-expr"
    "or FORM = scalar-default-char-expr"
    "or IOMSG = iomsg-variable"
    "or IOSTAT = stat-variable"
    "or NEWUNIT = scalar-int-variable"
    "or PAD = scalar-default-char-expr"
    "or POSITION = scalar-default-char-expr"
    "or RECL = scalar-int-expr"
    "or ROUND = scalar-default-char-expr"
    "or SIGN = scalar-default-char-expr"
    "or STATUS = scalar-default-char-expr",
)]
pub fn connect_spec<S: Lexed>(source: S) -> PResult<ConnectSpec<MultilineSpan>, S> {
    alt!(
        for S =>
        (
            (kw!(unit), equals()).optional(),
            file_unit_number,
        ).map(|(_, file_unit_number)| ConnectSpec::FileUnitNumber(file_unit_number)),
        (kw!(access), equals(), default_char_expr).map(|(_, _, scalar_default_char_expr)| ConnectSpec::Access(scalar_default_char_expr)),
        (kw!(action), equals(), default_char_expr).map(|(_, _, scalar_default_char_expr)| ConnectSpec::Action(scalar_default_char_expr)),
        (kw!(asynchronous), equals(), default_char_expr).map(|(_, _, scalar_default_char_expr)| ConnectSpec::Asynchronous(scalar_default_char_expr)),
        (kw!(blank), equals(), default_char_expr).map(|(_, _, scalar_default_char_expr)| ConnectSpec::Blank(scalar_default_char_expr)),
        (kw!(decimal), equals(), default_char_expr).map(|(_, _, scalar_default_char_expr)| ConnectSpec::Decimal(scalar_default_char_expr)),
        (kw!(delim), equals(), default_char_expr).map(|(_, _, scalar_default_char_expr)| ConnectSpec::Delim(scalar_default_char_expr)),
        (kw!(encoding), equals(), default_char_expr).map(|(_, _, scalar_default_char_expr)| ConnectSpec::Encoding(scalar_default_char_expr)),
        (kw!(err), equals(), label()).map(|(_, _, label)| ConnectSpec::Err(label)),
        (kw!(file), equals(), file_name_expr).map(|(_, _, file_name_expr)| ConnectSpec::File(file_name_expr)),
        (kw!(form), equals(), default_char_expr).map(|(_, _, scalar_default_char_expr)| ConnectSpec::Form(scalar_default_char_expr)),
        (kw!(iomsg), equals(), iomsg_variable).map(|(_, _, iomsg_variable)| ConnectSpec::Iomsg(iomsg_variable)),
        (kw!(iostat), equals(), stat_variable).map(|(_, _, stat_variable)| ConnectSpec::Iostat(stat_variable)),
        (kw!(newunit), equals(), int_variable(false)).map(|(_, _, scalar_int_variable)| ConnectSpec::Newunit(scalar_int_variable)), // TODO false???
        (kw!(pad), equals(), default_char_expr).map(|(_, _, scalar_default_char_expr)| ConnectSpec::Pad(scalar_default_char_expr)),
        (kw!(position), equals(), default_char_expr).map(|(_, _, scalar_default_char_expr)| ConnectSpec::Position(scalar_default_char_expr)),
        (kw!(recl), equals(), int_expr).map(|(_, _, scalar_int_expr)| ConnectSpec::Recl(scalar_int_expr)),
        (kw!(round), equals(), default_char_expr).map(|(_, _, scalar_default_char_expr)| ConnectSpec::Round(scalar_default_char_expr)),
        (kw!(sign), equals(), default_char_expr).map(|(_, _, scalar_default_char_expr)| ConnectSpec::Sign(scalar_default_char_expr)),
        (kw!(status), equals(), default_char_expr).map(|(_, _, scalar_default_char_expr)| ConnectSpec::Status(scalar_default_char_expr)),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct FileNameExpr<Span>(pub DefaultCharExpr<Span>);

#[doc = s_rule!(
    F18V007r1 rule "file-name-expr" #1206 : "is scalar-default-char-expr",
)]
pub fn file_name_expr<S: Lexed>(source: S) -> PResult<FileNameExpr<MultilineSpan>, S> {
    default_char_expr.map(FileNameExpr).parse(source)
}

#[derive(Debug, Clone)]
pub struct IomsgVariable<Span>(pub DefaultCharVariable<Span>);

#[doc = s_rule!(
    F18V007r1 rule "iomsg-variable" #1207 : "is scalar-default-char-variable",
)]
pub fn iomsg_variable<S: Lexed>(source: S) -> PResult<IomsgVariable<MultilineSpan>, S> {
    default_char_variable(false).map(IomsgVariable).parse(source)
}

#[derive(Debug, Clone)]
pub struct CloseStmt<Span> {
    pub close_spec_list: Vec<CloseSpec<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "close-stmt" #1208 : "is CLOSE ( close-spec-list )",
)]
pub fn close_stmt<S: Lexed>(source: S) -> PResult<CloseStmt<MultilineSpan>, S> {
    (
        (kw!(close), delim('(')),
        list(close_spec, 1..),
        delim(')'),
    ).map(|(_, close_spec_list, _)| CloseStmt {
        close_spec_list,
    }).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum CloseSpec<Span> {
    FileUnitNumber(FileUnitNumber<Span>),
    Iostat(StatVariable<Span>),
    Iomsg(IomsgVariable<Span>),
    Err(Label<Span>),
    Status(DefaultCharExpr<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "close-spec" #1209 :
    "is [ UNIT = ] file-unit-number"
    "or IOSTAT = stat-variable"
    "or IOMSG = iomsg-variable"
    "or ERR = label"
    "or STATUS = scalar-default-char-expr",
)]
pub fn close_spec<S: Lexed>(source: S) -> PResult<CloseSpec<MultilineSpan>, S> {
    alt!(
        for S =>
        (
            (kw!(unit), equals()).optional(),
            file_unit_number,
        ).map(|(_, file_unit_number)| CloseSpec::FileUnitNumber(file_unit_number)),
        (kw!(iostat), equals(), stat_variable).map(|(_, _, stat_variable)| CloseSpec::Iostat(stat_variable)),
        (kw!(iomsg), equals(), iomsg_variable).map(|(_, _, iomsg_variable)| CloseSpec::Iomsg(iomsg_variable)),
        (kw!(err), equals(), label()).map(|(_, _, label)| CloseSpec::Err(label)),
        (kw!(status), equals(), default_char_expr).map(|(_, _, scalar_default_char_expr)| CloseSpec::Status(scalar_default_char_expr)),
    ).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ReadStmt<Span> {
    IoControlSpecList(Vec<IoControlSpec<Span>>, Vec<InputItem<Span>>),
    Format(Format<Span>, Option<Vec<InputItem<Span>>>),
}

#[doc = s_rule!(
    F18V007r1 rule "read-stmt" #1210 :
    "is READ ( io-control-spec-list ) [ input-item-list ]"
    "or READ format [ , input-item-list ]",
)]
pub fn read_stmt<S: Lexed>(source: S) -> PResult<ReadStmt<MultilineSpan>, S> {
    alt!(
        for S =>
        (
            (kw!(read), delim('(')),
            list(io_control_spec, 1..),
            delim(')'),
            list(input_item, 0..).map(Some),
        ).map(|(_, io_control_spec_list, _, input_item_list)| ReadStmt::IoControlSpecList(io_control_spec_list, input_item_list.unwrap_or_default())),
        (
            kw!(read),
            format,
            (
                comma(),
                list(input_item, 0..),
            ).map(|(_, input_item_list)| input_item_list).optional(),
        ).map(|(_, format, input_item_list)| ReadStmt::Format(format, input_item_list)),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct WriteStmt<Span> {
    pub io_control_spec_list: Vec<IoControlSpec<Span>>,
    pub output_item_list: Option<Vec<OutputItem<Span>>>,
}

#[doc = s_rule!(
    F18V007r1 rule "write-stmt" #1211 : "is WRITE ( io-control-spec-list ) [ output-item-list ]",
)]
pub fn write_stmt<S: Lexed>(source: S) -> PResult<WriteStmt<MultilineSpan>, S> {
    (
        (kw!(write), delim('(')),
        list(io_control_spec, 1..),
        delim(')'),
        list(output_item, 1..).optional(),
    ).map(|(_, io_control_spec_list, _, output_item_list)| WriteStmt {
        io_control_spec_list,
        output_item_list,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct PrintStmt<Span> {
    pub format: Format<Span>,
    pub output_item_list: Option<Vec<OutputItem<Span>>>,
}

#[doc = s_rule!(
    F18V007r1 rule "print-stmt" #1212 : "is PRINT format [ , output-item-list ]",
)]
pub fn print_stmt<S: Lexed>(source: S) -> PResult<PrintStmt<MultilineSpan>, S> {
    (
        kw!(print),
        format,
        (
            comma(),
            list(output_item, 1..),
        ).map(|(_, output_item_list)| output_item_list).optional(),
    ).map(|(_, format, output_item_list)| PrintStmt {
        format,
        output_item_list,
    }).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum IoControlSpec<Span> {
    Unit(IoUnit<Span>),
    Fmt(Format<Span>),
    Nml(Name<Span>),
    Advance(DefaultCharExpr<Span>),
    Asynchronous(DefaultCharConstantExpr<Span>),
    Blank(DefaultCharExpr<Span>),
    Decimal(DefaultCharExpr<Span>),
    Delim(DefaultCharExpr<Span>),
    End(Label<Span>),
    Eor(Label<Span>),
    Err(Label<Span>),
    Id(IdVariable<Span>),
    Iomsg(IomsgVariable<Span>),
    Iostat(StatVariable<Span>),
    Pad(DefaultCharExpr<Span>),
    Pos(IntExpr<Span>),
    Rec(IntExpr<Span>),
    Round(DefaultCharExpr<Span>),
    Sign(DefaultCharExpr<Span>),
    Size(IntVariable<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "io-control-spec" #1213 :
    "is [ UNIT = ] io-unit"
    "or [ FMT = ] format"
    "or [ NML = ] namelist-group-name"
    "or ADVANCE = scalar-default-char-expr"
    "or ASYNCHRONOUS = scalar-default-char-constant-expr"
    "or BLANK = scalar-default-char-expr"
    "or DECIMAL = scalar-default-char-expr"
    "or DELIM = scalar-default-char-expr"
    "or END = label"
    "or EOR = label"
    "or ERR = label"
    "or ID = id-variable"
    "or IOMSG = iomsg-variable"
    "or IOSTAT = stat-variable"
    "or PAD = scalar-default-char-expr"
    "or POS = scalar-int-expr"
    "or REC = scalar-int-expr"
    "or ROUND = scalar-default-char-expr"
    "or SIGN = scalar-default-char-expr"
    "or SIZE = scalar-int-variable",
)]
pub fn io_control_spec<S: Lexed>(source: S) -> PResult<IoControlSpec<MultilineSpan>, S> {
    alt!(
        for S =>
        (
            (kw!(UNIT), equals()).optional(),
            io_unit,
        ).map(|(_, io_unit)| IoControlSpec::Unit(io_unit)),
        (
            (kw!(FMT), equals()).optional(),
            format,
        ).map(|(_, format)| IoControlSpec::Fmt(format)),
        (
            (kw!(NML), equals()).optional(),
            name(),
        ).map(|(_, namelist_group_name)| IoControlSpec::Nml(namelist_group_name)),
        (kw!(ADVANCE), equals(), default_char_expr).map(|(_, _, scalar_default_char_expr)| IoControlSpec::Advance(scalar_default_char_expr)),
        (kw!(ASYNCHRONOUS), equals(), default_char_constant_expr).map(|(_, _, scalar_default_char_constant_expr)| IoControlSpec::Asynchronous(scalar_default_char_constant_expr)),
        (kw!(BLANK), equals(), default_char_expr).map(|(_, _, scalar_default_char_expr)| IoControlSpec::Blank(scalar_default_char_expr)),
        (kw!(DECIMAL), equals(), default_char_expr).map(|(_, _, scalar_default_char_expr)| IoControlSpec::Decimal(scalar_default_char_expr)),
        (kw!(DELIM), equals(), default_char_expr).map(|(_, _, scalar_default_char_expr)| IoControlSpec::Delim(scalar_default_char_expr)),
        (kw!(END), equals(), label()).map(|(_, _, label)| IoControlSpec::End(label)),
        (kw!(EOR), equals(), label()).map(|(_, _, label)| IoControlSpec::Eor(label)),
        (kw!(ERR), equals(), label()).map(|(_, _, label)| IoControlSpec::Err(label)),
        (kw!(ID), equals(), id_variable).map(|(_, _, id_variable)| IoControlSpec::Id(id_variable)),
        (kw!(IOMSG), equals(), iomsg_variable).map(|(_, _, iomsg_variable)| IoControlSpec::Iomsg(iomsg_variable)),
        (kw!(IOSTAT), equals(), stat_variable).map(|(_, _, stat_variable)| IoControlSpec::Iostat(stat_variable)),
        (kw!(PAD), equals(), default_char_expr).map(|(_, _, scalar_default_char_expr)| IoControlSpec::Pad(scalar_default_char_expr)),
        (kw!(POS), equals(), int_expr).map(|(_, _, scalar_int_expr)| IoControlSpec::Pos(scalar_int_expr)),
        (kw!(REC), equals(), int_expr).map(|(_, _, scalar_int_expr)| IoControlSpec::Rec(scalar_int_expr)),
        (kw!(ROUND), equals(), default_char_expr).map(|(_, _, scalar_default_char_expr)| IoControlSpec::Round(scalar_default_char_expr)),
        (kw!(SIGN), equals(), default_char_expr).map(|(_, _, scalar_default_char_expr)| IoControlSpec::Sign(scalar_default_char_expr)),
        (kw!(SIZE), equals(), int_variable(true)).map(|(_, _, scalar_int_variable)| IoControlSpec::Size(scalar_int_variable)), // TODO true???
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct IdVariable<Span>(pub IntVariable<Span>);

#[doc = s_rule!(
    F18V007r1 rule "id-variable" #1214 : "is scalar-int-variable",
)]
pub fn id_variable<S: Lexed>(source: S) -> PResult<IdVariable<MultilineSpan>, S> {
    int_variable(false).map(IdVariable).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum Format<Span> {
    DefaultCharExpr(DefaultCharExpr<Span>),
    Label(Label<Span>),
    Star,
}

#[doc = s_rule!(
    F18V007r1 rule "format" #1215 :
    "is default-char-expr"
    "or label"
    "or *",
)]
pub fn format<S: Lexed>(source: S) -> PResult<Format<MultilineSpan>, S> {
    alt!(
        for S =>
        default_char_expr.map(Format::DefaultCharExpr),
        label().map(Format::Label),
        asterisk().map(|_| Format::Star),
    ).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum InputItem<Span> {
    Variable(Variable<Span>),
    IoImpliedDo(IoImpliedDo<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "input-item" #1216 :
    "is variable"
    "or io-implied-do",
)]
pub fn input_item<S: Lexed>(source: S) -> PResult<InputItem<MultilineSpan>, S> {
    alt!(
        for S =>
        variable(true).map(InputItem::Variable),
        io_implied_do.map(InputItem::IoImpliedDo),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub enum OutputItem<Span> {
    Expr(Expr<Span>),
    IoImpliedDo(IoImpliedDo<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "output-item" #1217 :
    "is expr"
    "or io-implied-do",
)]
pub fn output_item<S: Lexed>(source: S) -> PResult<OutputItem<MultilineSpan>, S> {
    alt!(
        for S =>
        expr.map(OutputItem::Expr),
        io_implied_do.map(OutputItem::IoImpliedDo),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct IoImpliedDo<Span> {
    pub io_implied_do_object_list: Vec<IoImpliedDoObject<Span>>,
    pub io_implied_do_control: IoImpliedDoControl<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "io-implied-do" #1218 : "is ( io-implied-do-object-list , io-implied-do-control )",
)]
pub fn io_implied_do<S: Lexed>(source: S) -> PResult<IoImpliedDo<MultilineSpan>, S> {
    (
        delim('('),
        list(io_implied_do_object, 1..),
        comma(),
        io_implied_do_control,
        delim(')'),
    ).map(|(_, io_implied_do_object_list, _, io_implied_do_control, _)| IoImpliedDo {
        io_implied_do_object_list,
        io_implied_do_control,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub enum IoImpliedDoObject<Span> {
    InputItem(InputItem<Span>),
    OutputItem(OutputItem<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "io-implied-do-object" #1219 :
    "is input-item"
    "or output-item",
)]
pub fn io_implied_do_object<S: Lexed>(source: S) -> PResult<IoImpliedDoObject<MultilineSpan>, S> {
    alt!(
        for S =>
        input_item.map(IoImpliedDoObject::InputItem),
        output_item.map(IoImpliedDoObject::OutputItem),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct IoImpliedDoControl<Span> {
    pub do_variable: Variable<Span>,
    pub scalar_int_expr_list: Vec<IntExpr<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "io-implied-do-control" #1220 :
    "is do-variable = scalar-int-expr ,"
    "    scalar-int-expr [ , scalar-int-expr ]",
)]
pub fn io_implied_do_control<S: Lexed>(source: S) -> PResult<IoImpliedDoControl<MultilineSpan>, S> {
    (
        variable(true),
        (equals()),
        list(int_expr, 2..),
    ).map(|(do_variable, _, scalar_int_expr_list)| IoImpliedDoControl {
        do_variable,
        scalar_int_expr_list,
    }).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum DtvTypeSpec<Span> {
    Type(DerivedTypeSpec<Span>),
    Class(DerivedTypeSpec<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "dtv-type-spec" #1221 :
    "is TYPE( derived-type-spec )"
    "or CLASS( derived-type-spec )",
)]
pub fn dtv_type_spec<S: Lexed>(source: S) -> PResult<DtvTypeSpec<MultilineSpan>, S> {
    alt!(
        for S =>
        (kw!(TYPE), delim('('), derived_type_spec, delim(')')).map(|(_, _, derived_type_spec, _)| DtvTypeSpec::Type(derived_type_spec)),
        (kw!(CLASS), delim('('), derived_type_spec, delim(')')).map(|(_, _, derived_type_spec, _)| DtvTypeSpec::Class(derived_type_spec)),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct WaitStmt<Span> {
    pub wait_spec_list: Vec<WaitSpec<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "wait-stmt" #1222 : "is WAIT (wait-spec-list)",
)]
pub fn wait_stmt<S: Lexed>(source: S) -> PResult<WaitStmt<MultilineSpan>, S> {
    (
        (kw!(wait), delim('(')),
        list(wait_spec, 1..),
        delim(')'),
    ).map(|(_, wait_spec_list, _)| WaitStmt {
        wait_spec_list,
    }).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum WaitSpec<Span> {
    FileUnitNumber(FileUnitNumber<Span>),
    End(Label<Span>),
    Eor(Label<Span>),
    Err(Label<Span>),
    Id(IntExpr<Span>),
    Iomsg(IomsgVariable<Span>),
    Iostat(StatVariable<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "wait-spec" #1223 :
    "is [ UNIT = ] file-unit-number"
    "or END = label"
    "or EOR = label"
    "or ERR = label"
    "or ID = scalar-int-expr"
    "or IOMSG = iomsg-variable"
    "or IOSTAT = stat-variable",
)]
pub fn wait_spec<S: Lexed>(source: S) -> PResult<WaitSpec<MultilineSpan>, S> {
    alt!(
        for S =>
        (
            (kw!(UNIT), equals()).optional(),
            file_unit_number,
        ).map(|(_, file_unit_number)| WaitSpec::FileUnitNumber(file_unit_number)),
        (kw!(END), equals(), label()).map(|(_, _, label)| WaitSpec::End(label)),
        (kw!(EOR), equals(), label()).map(|(_, _, label)| WaitSpec::Eor(label)),
        (kw!(ERR), equals(), label()).map(|(_, _, label)| WaitSpec::Err(label)),
        (kw!(ID), equals(), int_expr).map(|(_, _, scalar_int_expr)| WaitSpec::Id(scalar_int_expr)),
        (kw!(IOMSG), equals(), iomsg_variable).map(|(_, _, iomsg_variable)| WaitSpec::Iomsg(iomsg_variable)),
        (kw!(IOSTAT), equals(), stat_variable).map(|(_, _, stat_variable)| WaitSpec::Iostat(stat_variable)),
    ).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum EndfileStmt<Span> {
    FileUnitNumber(FileUnitNumber<Span>),
    PositionSpecList(Vec<PositionSpec<Span>>),
}

#[doc = s_rule!(
    F18V007r1 rule "endfile-stmt" #1225 :
    "is ENDFILE file-unit-number"
    "or ENDFILE ( position-spec-list )",
)]
pub fn endfile_stmt<S: Lexed>(source: S) -> PResult<EndfileStmt<MultilineSpan>, S> {
    alt!(
        for S =>
        (kw!(ENDFILE), file_unit_number).map(|(_, file_unit_number)| EndfileStmt::FileUnitNumber(file_unit_number)),
        (kw!(ENDFILE), delim('('), list(position_spec, 1..), delim(')')).map(|(_, _, position_spec_list, _)| EndfileStmt::PositionSpecList(position_spec_list)),
    ).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum RewindStmt<Span> {
    FileUnitNumber(FileUnitNumber<Span>),
    PositionSpecList(Vec<PositionSpec<Span>>),
}

#[doc = s_rule!(
    F18V007r1 rule "rewind-stmt" #1226 :
    "is REWIND file-unit-number"
    "or REWIND ( position-spec-list )",
)]
pub fn rewind_stmt<S: Lexed>(source: S) -> PResult<RewindStmt<MultilineSpan>, S> {
    alt!(
        for S =>
        (kw!(REWIND), file_unit_number).map(|(_, file_unit_number)| RewindStmt::FileUnitNumber(file_unit_number)),
        (kw!(REWIND), delim('('), list(position_spec, 1..), delim(')')).map(|(_, _, position_spec_list, _)| RewindStmt::PositionSpecList(position_spec_list)),
    ).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum PositionSpec<Span> {
    FileUnitNumber(FileUnitNumber<Span>),
    Iomsg(IomsgVariable<Span>),
    Iostat(StatVariable<Span>),
    Err(Label<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "position-spec" #1227 :
    "is [ UNIT = ] file-unit-number"
    "or IOMSG = iomsg-variable"
    "or IOSTAT = stat-variable"
    "or ERR = label",
)]
pub fn position_spec<S: Lexed>(source: S) -> PResult<PositionSpec<MultilineSpan>, S> {
    alt!(
        for S =>
        (
            (kw!(UNIT), equals()).optional(),
            file_unit_number,
        ).map(|(_, file_unit_number)| PositionSpec::FileUnitNumber(file_unit_number)),
        (kw!(IOMSG), equals(), iomsg_variable).map(|(_, _, iomsg_variable)| PositionSpec::Iomsg(iomsg_variable)),
        (kw!(IOSTAT), equals(), stat_variable).map(|(_, _, stat_variable)| PositionSpec::Iostat(stat_variable)),
        (kw!(ERR), equals(), label()).map(|(_, _, label)| PositionSpec::Err(label)),
    ).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum FlushStmt<Span> {
    FileUnitNumber(FileUnitNumber<Span>),
    FlushSpecList(Vec<FlushSpec<Span>>),
}

#[doc = s_rule!(
    F18V007r1 rule "flush-stmt" #1228 :
    "is FLUSH file-unit-number"
    "or FLUSH ( flush-spec-list )",
)]
pub fn flush_stmt<S: Lexed>(source: S) -> PResult<FlushStmt<MultilineSpan>, S> {
    alt!(
        for S =>
        (kw!(FLUSH), file_unit_number).map(|(_, file_unit_number)| FlushStmt::FileUnitNumber(file_unit_number)),
        (kw!(FLUSH), delim('('), list(flush_spec, 1..), delim(')')).map(|(_, _, flush_spec_list, _)| FlushStmt::FlushSpecList(flush_spec_list)),
    ).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum FlushSpec<Span> {
    FileUnitNumber(FileUnitNumber<Span>),
    Iostat(StatVariable<Span>),
    Iomsg(IomsgVariable<Span>),
    Err(Label<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "flush-spec" #1229 :
    "is [UNIT =] file-unit-number"
    "or IOSTAT = stat-variable"
    "or IOMSG = iomsg-variable"
    "or ERR = label",
)]
pub fn flush_spec<S: Lexed>(source: S) -> PResult<FlushSpec<MultilineSpan>, S> {
    alt!(
        for S =>
        (
            (kw!(UNIT), equals()).optional(),
            file_unit_number,
        ).map(|(_, file_unit_number)| FlushSpec::FileUnitNumber(file_unit_number)),
        (kw!(IOSTAT), equals(), stat_variable).map(|(_, _, stat_variable)| FlushSpec::Iostat(stat_variable)),
        (kw!(IOMSG), equals(), iomsg_variable).map(|(_, _, iomsg_variable)| FlushSpec::Iomsg(iomsg_variable)),
        (kw!(ERR), equals(), label()).map(|(_, _, label)| FlushSpec::Err(label)),
    ).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum InquireStmt<Span> {
    InquireSpecList(Vec<InquireSpec<Span>>),
    IoLength(IntVariable<Span>),
    OutputItemList(Vec<OutputItem<Span>>),
}

#[doc = s_rule!(
    F18V007r1 rule "inquire-stmt" #1230 :
    "is INQUIRE ( inquire-spec-list )"
    "or INQUIRE ( IOLENGTH = scalar-int-variable )"
    "output-item-list",
)]
pub fn inquire_stmt<S: Lexed>(source: S) -> PResult<InquireStmt<MultilineSpan>, S> {
    alt!(
        for S =>
        (
            (kw!(INQUIRE), delim('(')),
            list(inquire_spec, 1..),
            delim(')'),
        ).map(|(_, inquire_spec_list, _)| InquireStmt::InquireSpecList(inquire_spec_list)),
        (
            (kw!(INQUIRE), delim('('), kw!(IOLENGTH), equals()),
            int_variable(false), // TODO false???
            delim(')'),
        ).map(|(_, scalar_int_variable, _)| InquireStmt::IoLength(scalar_int_variable)),
        list(output_item, 1..).map(InquireStmt::OutputItemList),
    ).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum InquireSpec<Span> {
    FileUnitNumber(FileUnitNumber<Span>),
    File(FileNameExpr<Span>),
    Access(DefaultCharVariable<Span>),
    Action(DefaultCharVariable<Span>),
    Asynchronous(DefaultCharVariable<Span>),
    Blank(DefaultCharVariable<Span>),
    Decimal(DefaultCharVariable<Span>),
    Delim(DefaultCharVariable<Span>),
    Direct(DefaultCharVariable<Span>),
    Encoding(DefaultCharVariable<Span>),
    Err(Label<Span>),
    Exist(LogicalVariable<Span>),
    Form(DefaultCharVariable<Span>),
    Formatted(DefaultCharVariable<Span>),
    Id(IntExpr<Span>),
    Iomsg(IomsgVariable<Span>),
    Iostat(StatVariable<Span>),
    Name(DefaultCharVariable<Span>),
    Named(LogicalVariable<Span>),
    Nextrec(IntVariable<Span>),
    Number(IntVariable<Span>),
    Opened(LogicalVariable<Span>),
    Pad(DefaultCharVariable<Span>),
    Pending(LogicalVariable<Span>),
    Pos(IntVariable<Span>),
    Position(DefaultCharVariable<Span>),
    Read(DefaultCharVariable<Span>),
    Readwrite(DefaultCharVariable<Span>),
    Recl(IntVariable<Span>),
    Round(DefaultCharVariable<Span>),
    Sequential(DefaultCharVariable<Span>),
    Sign(DefaultCharVariable<Span>),
    Size(IntVariable<Span>),
    Stream(DefaultCharVariable<Span>),
    Unformatted(DefaultCharVariable<Span>),
    Write(DefaultCharVariable<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "inquire-spec" #1231 :
    "is [ UNIT = ] file-unit-number"
    "or FILE = file-name-expr"
    "or ACCESS = scalar-default-char-variable"
    "or ACTION = scalar-default-char-variable"
    "or ASYNCHRONOUS = scalar-default-char-variable"
    "or BLANK = scalar-default-char-variable"
    "or DECIMAL = scalar-default-char-variable"
    "or DELIM = scalar-default-char-variable"
    "or DIRECT = scalar-default-char-variable"
    "or ENCODING = scalar-default-char-variable"
    "or ERR = label"
    "or EXIST = scalar-logical-variable"
    "or FORM = scalar-default-char-variable"
    "or FORMATTED = scalar-default-char-variable"
    "or ID = scalar-int-expr"
    "or IOMSG = iomsg-variable"
    "or IOSTAT = stat-variable"
    "or NAME = scalar-default-char-variable"
    "or NAMED = scalar-logical-variable"
    "or NEXTREC = scalar-int-variable"
    "or NUMBER = scalar-int-variable"
    "or OPENED = scalar-logical-variable"
    "or PAD = scalar-default-char-variable"
    "or PENDING = scalar-logical-variable"
    "or POS = scalar-int-variable"
    "or POSITION = scalar-default-char-variable"
    "or READ = scalar-default-char-variable"
    "or READWRITE = scalar-default-char-variable"
    "or RECL = scalar-int-variable"
    "or ROUND = scalar-default-char-variable"
    "or SEQUENTIAL = scalar-default-char-variable"
    "or SIGN = scalar-default-char-variable"
    "or SIZE = scalar-int-variable"
    "or STREAM = scalar-default-char-variable"
    "or UNFORMATTED = scalar-default-char-variable"
    "or WRITE = scalar-default-char-variable",
)]
pub fn inquire_spec<S: Lexed>(source: S) -> PResult<InquireSpec<MultilineSpan>, S> {
    alt!(
        for S =>
        (
            (kw!(unit), equals()).optional(),
            file_unit_number,
        ).map(|(_, file_unit_number)| InquireSpec::FileUnitNumber(file_unit_number)),
        (kw!(FILE), equals(), file_name_expr).map(|(_, _, file_name_expr)| InquireSpec::File(file_name_expr)),
        (kw!(ACCESS), equals(), default_char_variable(false)).map(|(_, _, scalar_default_char_variable)| InquireSpec::Access(scalar_default_char_variable)),
        (kw!(ACTION), equals(), default_char_variable(false)).map(|(_, _, scalar_default_char_variable)| InquireSpec::Action(scalar_default_char_variable)),
        (kw!(ASYNCHRONOUS), equals(), default_char_variable(false)).map(|(_, _, scalar_default_char_variable)| InquireSpec::Asynchronous(scalar_default_char_variable)),
        (kw!(BLANK), equals(), default_char_variable(false)).map(|(_, _, scalar_default_char_variable)| InquireSpec::Blank(scalar_default_char_variable)),
        (kw!(DECIMAL), equals(), default_char_variable(false)).map(|(_, _, scalar_default_char_variable)| InquireSpec::Decimal(scalar_default_char_variable)),
        (kw!(DELIM), equals(), default_char_variable(false)).map(|(_, _, scalar_default_char_variable)| InquireSpec::Delim(scalar_default_char_variable)),
        (kw!(DIRECT), equals(), default_char_variable(false)).map(|(_, _, scalar_default_char_variable)| InquireSpec::Direct(scalar_default_char_variable)),
        (kw!(ENCODING), equals(), default_char_variable(false)).map(|(_, _, scalar_default_char_variable)| InquireSpec::Encoding(scalar_default_char_variable)),
        (kw!(ERR), equals(), label()).map(|(_, _, label)| InquireSpec::Err(label)),
        (kw!(EXIST), equals(), logical_variable(false)).map(|(_, _, scalar_logical_variable)| InquireSpec::Exist(scalar_logical_variable)), // TODO false???
        (kw!(FORM), equals(), default_char_variable(false)).map(|(_, _, scalar_default_char_variable)| InquireSpec::Form(scalar_default_char_variable)),
        (kw!(FORMATTED), equals(), default_char_variable(false)).map(|(_, _, scalar_default_char_variable)| InquireSpec::Formatted(scalar_default_char_variable)),
        (kw!(ID), equals(), int_expr).map(|(_, _, scalar_int_expr)| InquireSpec::Id(scalar_int_expr)),
        (kw!(IOMSG), equals(), iomsg_variable).map(|(_, _, iomsg_variable)| InquireSpec::Iomsg(iomsg_variable)),
        (kw!(IOSTAT), equals(), stat_variable).map(|(_, _, stat_variable)| InquireSpec::Iostat(stat_variable)),
        (kw!(NAME), equals(), default_char_variable(false)).map(|(_, _, scalar_default_char_variable)| InquireSpec::Name(scalar_default_char_variable)),
        (kw!(NAMED), equals(), logical_variable(false)).map(|(_, _, scalar_logical_variable)| InquireSpec::Named(scalar_logical_variable)),
        (kw!(NEXTREC), equals(), int_variable(false)).map(|(_, _, scalar_int_variable)| InquireSpec::Nextrec(scalar_int_variable)),
        (kw!(NUMBER), equals(), int_variable(false)).map(|(_, _, scalar_int_variable)| InquireSpec::Number(scalar_int_variable)),
        (kw!(OPENED), equals(), logical_variable(false)).map(|(_, _, scalar_logical_variable)| InquireSpec::Opened(scalar_logical_variable)),
        (kw!(PAD), equals(), default_char_variable(false)).map(|(_, _, scalar_default_char_variable)| InquireSpec::Pad(scalar_default_char_variable)),
        (kw!(PENDING), equals(), logical_variable(false)).map(|(_, _, scalar_logical_variable)| InquireSpec::Pending(scalar_logical_variable)),
        (kw!(POS), equals(), int_variable(false)).map(|(_, _, scalar_int_variable)| InquireSpec::Pos(scalar_int_variable)),
        (kw!(POSITION), equals(), default_char_variable(false)).map(|(_, _, scalar_default_char_variable)| InquireSpec::Position(scalar_default_char_variable)),
        (kw!(READ), equals(), default_char_variable(false)).map(|(_, _, scalar_default_char_variable)| InquireSpec::Read(scalar_default_char_variable)),
        (kw!(READWRITE), equals(), default_char_variable(false)).map(|(_, _, scalar_default_char_variable)| InquireSpec::Readwrite(scalar_default_char_variable)),
        (kw!(RECL), equals(), int_variable(false)).map(|(_, _, scalar_int_variable)| InquireSpec::Recl(scalar_int_variable)),
        (kw!(ROUND), equals(), default_char_variable(false)).map(|(_, _, scalar_default_char_variable)| InquireSpec::Round(scalar_default_char_variable)),
        (kw!(SEQUENTIAL), equals(), default_char_variable(false)).map(|(_, _, scalar_default_char_variable)| InquireSpec::Sequential(scalar_default_char_variable)),
        (kw!(SIGN), equals(), default_char_variable(false)).map(|(_, _, scalar_default_char_variable)| InquireSpec::Sign(scalar_default_char_variable)),
        (kw!(SIZE), equals(), int_variable(false)).map(|(_, _, scalar_int_variable)| InquireSpec::Size(scalar_int_variable)),
        (kw!(STREAM), equals(), default_char_variable(false)).map(|(_, _, scalar_default_char_variable)| InquireSpec::Stream(scalar_default_char_variable)),
        (kw!(UNFORMATTED), equals(), default_char_variable(false)).map(|(_, _, scalar_default_char_variable)| InquireSpec::Unformatted(scalar_default_char_variable)),
        (kw!(WRITE), equals(), default_char_variable(false)).map(|(_, _, scalar_default_char_variable)| InquireSpec::Write(scalar_default_char_variable)),
    ).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum BackspaceStmt<Span> {
    FileUnitNumber(FileUnitNumber<Span>),
    PositionSpecList(Vec<PositionSpec<Span>>),
}

#[doc = s_rule!(
    F18V007r1 rule "backspace-stmt" #1224 :
    "is BACKSPACE file-unit-number"
    "or BACKSPACE ( position-spec-list )",
)]
pub fn backspace_stmt_2<S: Lexed>(source: S) -> PResult<BackspaceStmt<MultilineSpan>, S> {
    alt!(
        for S =>
        (kw!(backspace), file_unit_number).map(|(_, file_unit_number)| BackspaceStmt::FileUnitNumber(file_unit_number)),
        (kw!(backspace), delim('('), list(position_spec, 1..), delim(')')).map(|(_, _, position_spec_list, _)| BackspaceStmt::PositionSpecList(position_spec_list)),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct ProcComponentRef<Span> {
    pub scalar_variable: Variable<Span>,
    pub procedure_component_name: Name<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "proc-component-ref" #1039 : "is scalar-variable % procedure-component-name",
)]
pub fn proc_component_ref<S: Lexed>(source: S) -> PResult<ProcComponentRef<MultilineSpan>, S> {
    (
        variable(true),
        percent(),
        name(),
    ).map(|(scalar_variable, _, procedure_component_name)| ProcComponentRef {
        scalar_variable,
        procedure_component_name,
    }).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ProcTarget<Span> {
    Expr(Expr<Span>),
    ProcedureName(Name<Span>),
    ProcComponentRef(ProcComponentRef<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "proc-target" #1040 :
    "is expr"
    "or procedure-name"
    "or proc-component-ref",
)]
pub fn proc_target<S: Lexed>(source: S) -> PResult<ProcTarget<MultilineSpan>, S> {
    alt!(
        for S =>
        expr.map(ProcTarget::Expr),
        name().map(ProcTarget::ProcedureName),
        proc_component_ref.map(ProcTarget::ProcComponentRef),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct MaskExpr<Span>(pub Expr<Span>);

#[doc = s_rule!(
    F18V007r1 rule "mask-expr" #1046 : "is logical-expr",
)]
pub fn mask_expr<S: Lexed>(source: S) -> PResult<MaskExpr<MultilineSpan>, S> {
    logical_expr.map(MaskExpr).parse(source)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn tmp() {
        let expr = |s: &str| expr(tokenize(s).as_slice()).map(|(r, _)| r);

        assert!(expr("1").is_some());
        assert!(expr("1").unwrap().right.right.right.right.expr.right.right.right.right.expr.primary.is_literal());
        assert!(expr("2.1 + 3.4 + 4.9").is_some());
        assert!(expr("2.1 * 3.4 * 4.9").is_some());
        assert!(expr("2.1 / 3.4 / 4.9").is_some());
        assert!(expr("2 ** 3 ** 4").is_some());
        assert!(expr("'AB' // 'CD' // 'EF'").is_some());
    }
}
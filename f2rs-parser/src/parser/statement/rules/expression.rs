use f2rs_parser_combinator::seq;
use crate::tokenizer::rules::{DefinedUnaryOrBinaryOp, PowerOp};

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
        seq!((
            op: bin_op,
            expr: e5,
        ) => (op, expr))
    };

    let (right, mut source) = e5.parse(source)?;
    let mut expr = Expr { left: None, right };
    loop {
        match right_part.parse(source.clone()) {
            Some(((op, right), new_source)) => {
                source = new_source;
                expr = Expr {
                    left: Some((Box::new(expr), op)),
                    right,
                };
            }
            None => break,
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
pub fn default_char_constant_expr<S: Lexed>(
    source: S,
) -> PResult<DefaultCharConstantExpr<MultilineSpan>, S> {
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
    //TypeParamInquiry(TypeParamInquiry<Span>),
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
    // NOTE: type-param-inquiry is syntactically equivalent to a designator (specifically,
    // anything that leads to a structure component, i.e. 

    alt! {
        for S =>
        seq!((
            _: delim('('),
            expr: expr,
            _: delim(')'),
        ) => Primary::ParenthesizedExpr(Box::new(expr))),
        //type_param_inquiry.map(Primary::TypeParamInquiry),
        structure_constructor.map(Primary::StructureConstructor),
        designator(false).map(Primary::Designator),
        function_reference.map(Primary::FunctionReference),
        array_constructor.map(Primary::ArrayConstructor),
        literal_constant.map(Primary::Literal),
        name().map(Primary::TypeParamName),
    }
    .parse(source)
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
    seq!((
        operator: defined_unary_or_binary_op().opt(), // TODO unary
        primary: primary,
    ) => Level1Expr {
        operator,
        primary: Box::new(primary),
    }).parse(source)
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
    seq!((
        expr: level_1_expr,
        exp: seq!((
            power_op: power_op(),
            mult_operand: mult_operand
        ) => (power_op, Box::new(mult_operand))).opt(),
    ) => MultOperand { expr, exp }).parse(source)
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
        seq!((
            op: add_op,
            expr: mult_operand,
        ) => (op, expr))
    };

    let (right, mut source) = mult_operand.parse(source)?;
    let mut expr = AddOperand { left: None, right };
    loop {
        match right_part.parse(source.clone()) {
            Some(((op, right), new_source)) => {
                source = new_source;
                expr = AddOperand {
                    left: Some((Box::new(expr), op)),
                    right,
                };
            }
            None => break,
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
        seq!((
            op: add_op,
            expr: add_operand,
        ) => (op, expr))
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
            }
            None => break,
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
        seq!((
            op: concat_op,
            expr: level_2_expr,
        ) => (op, expr))
    };

    let (right, mut source) = level_2_expr.parse(source)?;
    let mut expr = Level3Expr { left: None, right };
    loop {
        match right_part.parse(source.clone()) {
            Some(((op, right), new_source)) => {
                source = new_source;
                expr = Level3Expr {
                    left: Some((Box::new(expr), op)),
                    right,
                };
            }
            None => break,
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
        seq!((
            op: rel_op,
            expr: level_3_expr,
        ) => (op, expr))
    };

    let (right, mut source) = level_3_expr.parse(source)?;
    let mut expr = Level4Expr { left: None, right };
    loop {
        match right_part.parse(source.clone()) {
            Some(((op, right), new_source)) => {
                source = new_source;
                expr = Level4Expr {
                    left: Some((Box::new(expr), op)),
                    right,
                };
            }
            None => break,
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
    seq!((
        operator: not_op().opt(),
        expr: level_4_expr,
    ) => AndOperand { operator, expr }).parse(source)
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
        seq!((
            op: and_op,
            expr: and_operand,
        ) => (op, expr))
    };

    let (right, mut source) = and_operand.parse(source)?;
    let mut expr = OrOperand { left: None, right };
    loop {
        match right_part.parse(source.clone()) {
            Some(((op, right), new_source)) => {
                source = new_source;
                expr = OrOperand {
                    left: Some((Box::new(expr), op)),
                    right,
                };
            }
            None => break,
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
        seq!((
            op: or_op,
            expr: or_operand,
        ) => (op, expr))
    };

    let (right, mut source) = or_operand.parse(source)?;
    let mut expr = EquivOperand { left: None, right };
    loop {
        match right_part.parse(source.clone()) {
            Some(((op, right), new_source)) => {
                source = new_source;
                expr = EquivOperand {
                    left: Some((Box::new(expr), op)),
                    right,
                };
            }
            None => break,
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
        seq!((
            op: equiv_op,
            expr: equiv_operand,
        ) => (op, expr))
    };

    let (right, mut source) = equiv_operand.parse(source)?;
    let mut expr = Level5Expr { left: None, right };
    loop {
        match right_part.parse(source.clone()) {
            Some(((op, right), new_source)) => {
                source = new_source;
                expr = Level5Expr {
                    left: Some((Box::new(expr), op)),
                    right,
                };
            }
            None => break,
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
        array_section(not_complex_part_designator).map(Designator::ArraySection),
        array_element.map(Designator::ArrayElement),
        substring.map(Designator::Substring),
        object_name.map(Designator::ObjectName),
        coindexed_named_object.map(Designator::CoindexedNamedObject),
        complex_part_designator.map(Designator::ComplexPartDesignator).if_(!not_complex_part_designator),
        structure_component.map(Designator::StructureComponent),
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
    seq!((
        parent: parent_string,
        _: delim('('),
        range: substring_range,
        _: delim(')'),
    ) => Substring { parent, range }).parse(source)
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
    )
    .parse(source)
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
    seq!((
        left: int_expr.opt(),
        _: colon(),
        right: int_expr.opt(),
    ) => SubstringRange { left, right }).parse(source)
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
    seq!((
        part: part_ref,
        selectors: list(seq!((
            _: percent(),
            part_ref: part_ref,
        ) => part_ref), 0..),
    ) => DataRef { part, selectors }).parse(source)
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
    seq!((
        part_name: name(),
        section_subscript_list: seq!((_: delim('('), list: list(subscript, 0..), _: delim(')')) => list).opt(),
        image_selector: image_selector.opt()
    ) => PartRef {
                part_name,
                section_subscript_list,
                image_selector,
            })
        .parse(source)
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
pub fn coindexed_named_object<S: Lexed>(
    source: S,
) -> PResult<CoindexedNamedObject<MultilineSpan>, S> {
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
pub fn complex_part_designator<S: Lexed>(
    source: S,
) -> PResult<ComplexPartDesignator<MultilineSpan>, S> {
    alt!(
        for S =>
        seq!(
            (designator: designator(true), _: percent(), _: kw!(re)) => ComplexPartDesignator::Re(Box::new(designator))
        ),
        seq!(
            (designator: designator(true), _: percent(), _: kw!(im)) => ComplexPartDesignator::Im(Box::new(designator))
        ),
    )
    .parse(source)
}

//#[derive(Debug, Clone)]
//pub struct TypeParamInquiry<Span> {
//    pub designator: Designator<Span>,
//    pub type_param_name: Name<Span>,
//}

//#[doc = s_rule!(
//    F18V007r1 rule "type-param-inquiry" #916 : "is designator % type-param-name",
//)]
//pub fn type_param_inquiry<S: Lexed>(source: S) -> PResult<TypeParamInquiry<MultilineSpan>, S> {
//    (designator(true), percent(), name())
//        .map(|(designator, _, type_param_name)| TypeParamInquiry {
//            designator,
//            type_param_name,
//        })
//        .parse(source)
//}

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
        seq!(
            (data_ref: data_ref, 
             range: seq!(
                (_: delim('('), range: substring_range, _: delim(')')) => range
            ).opt()) 
            => ArraySection::Data(data_ref, range)
        ),
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
        subscript_triplet.map(SectionSubscript::SubscriptTriplet),
        subscript.map(SectionSubscript::Subscript),
        vector_subscript.map(SectionSubscript::VectorSubscript),
    )
    .parse(source)
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
    seq!(
        (lower: subscript.opt(),
         _: colon(),
         upper: subscript.opt(),
         stride: seq!((_: colon(), subscript: stride) => subscript).opt())
        => SubscriptTriplet {
            lower,
            upper,
            stride,
        }
    )
        .parse(source)
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
    seq!(
        (_: delim('['),
         cosubscript_list: list(cosubscript, 0..),
         image_selector_spec_list: seq!(
            (_: comma(), image_selector_spec_list: list(image_selector_spec, 0..)) => image_selector_spec_list
         ),
         _: delim(']'))
        => ImageSelector {
            cosubscript_list,
            image_selector_spec_list,
        }
    )
        .parse(source)
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
        seq!((_: kw!(stat), _: equals(), stat_variable: stat_variable) => ImageSelectorSpec::Stat(stat_variable)),
        seq!((_: kw!(team), _: equals(), team_value: team_value) => ImageSelectorSpec::Team(team_value)),
        seq!((_: kw!(team_number), _: equals(), int_expr: int_expr) => ImageSelectorSpec::TeamNumber(int_expr)),
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
    seq!(
        (_: kw!(allocate),
         _: delim('('),
         type_spec: seq!((type_spec: type_spec, _: double_colon()) => type_spec).opt(),
         allocation_list: list(allocation, 1..),
         alloc_opt_list: seq!((_: comma(), alloc_opt_list: list(alloc_opt, 0..)) => alloc_opt_list).opt(),
         _: delim(')'))
        => AllocateStmt {
            type_spec,
            allocation_list,
            alloc_opt_list,
        }
    )
        .parse(source)
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
        seq!((_: kw!(ERRMSG), _: equals(), errmsg_variable: errmsg_variable) => AllocOpt::Errmsg(errmsg_variable)),
        seq!((_: kw!(MOLD), _: equals(), source_expr: source_expr) => AllocOpt::Mold(source_expr)),
        seq!((_: kw!(SOURCE), _: equals(), source_expr: source_expr) => AllocOpt::Source(source_expr)),
        seq!((_: kw!(STAT), _: equals(), stat_variable: stat_variable) => AllocOpt::Stat(stat_variable)),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct ErrmsgVariable<Span>(pub DefaultCharVariable<Span>);

#[doc = s_rule!(
    F18V007r1 rule "errmsg-variable" #929 : "is scalar-default-char-variable",
)]
pub fn errmsg_variable<S: Lexed>(source: S) -> PResult<ErrmsgVariable<MultilineSpan>, S> {
    default_char_variable(false)
        .map(ErrmsgVariable)
        .parse(source)
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
    seq!(
        (object: allocate_object,
         allocate_shape_spec_list: seq!(
            (_: delim('('), allocate_shape_spec_list: list(allocate_shape_spec, 0..), _: delim(')')) => allocate_shape_spec_list
         ).opt(),
         allocate_coarray_spec: seq!(
            (_: delim('['), allocate_coarray_spec: allocate_coarray_spec, _: delim(']')) => allocate_coarray_spec
         ).opt())
        => Allocation {
            object,
            allocate_shape_spec_list,
            allocate_coarray_spec,
        }
    )
        .parse(source)
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
    )
    .parse(source)
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
    seq!(
        (lower_bound: seq!((lower_bound: lower_bound_expr, _: colon()) => lower_bound).opt(),
         upper_bound: upper_bound_expr)
        => AllocateShapeSpec {
            lower_bound,
            upper_bound,
        }
    )
        .parse(source)
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
pub fn allocate_coarray_spec<S: Lexed>(
    source: S,
) -> PResult<AllocateCoarraySpec<MultilineSpan>, S> {
    seq!((
        allocate_coshape_spec_list: seq!((list: list(allocate_coshape_spec, 0..), _: comma()) => list).opt(),
        lower_bound: seq!((lower_bound: lower_bound_expr, _: colon()) => lower_bound).opt(),
        _: asterisk()
    ) => AllocateCoarraySpec {
                allocate_coshape_spec_list: allocate_coshape_spec_list.unwrap_or_default(),
                lower_bound,
            })
        .parse(source)
}

#[derive(Debug, Clone)]
pub struct AllocateCoshapeSpec<Span> {
    pub lower_bound: Option<LowerBoundExpr<Span>>,
    pub upper_bound: UpperBoundExpr<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "allocate-coshape-spec" #937 : "is [ lower-bound-expr : ] upper-bound-expr",
)]
pub fn allocate_coshape_spec<S: Lexed>(
    source: S,
) -> PResult<AllocateCoshapeSpec<MultilineSpan>, S> {
    seq!((
        lower_bound: seq!((lower_bound: lower_bound_expr, _: colon()) => lower_bound).opt(),
        upper_bound: upper_bound_expr
    ) => AllocateCoshapeSpec {
            lower_bound,
            upper_bound,
        })
        .parse(source)
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
    )
    .parse(source)
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
    seq!((variable: variable(true), _: equals(), expr: expr) => AssignmentStmt { variable, expr })
        .parse(source)
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
pub fn pointer_assignment_stmt<S: Lexed>(
    source: S,
) -> PResult<PointerAssignmentStmt<MultilineSpan>, S> {
    alt!(
        for S =>
        seq!((
            data_pointer_object: data_pointer_object,
            bounds_spec_list: seq!((
                _: delim('('),
                bounds_spec_list: list(bounds_spec, 0..),
                _: delim(')'),
            ) => bounds_spec_list).opt(),
            _: arrow(),
            data_target: data_target,
        ) => PointerAssignmentStmt::Form1 {
            data_pointer_object,
            bounds_spec_list,
            data_target,
        }),
        seq!((
            data_pointer_object: data_pointer_object,
            _: delim('('),
            bounds_remapping_list: list(bounds_remapping, 0..),
            _: delim(')'),
            _: arrow(),
            data_target: data_target,
        ) => PointerAssignmentStmt::Form2 {
            data_pointer_object,
            bounds_remapping_list,
            data_target,
        }),
        seq!((
            proc_pointer_object: proc_pointer_object,
            _: arrow(),
            proc_target: proc_target,
        ) => PointerAssignmentStmt::Form3 {
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
        seq!((
            scalar_variable: variable(true),
            _: percent(),
            data_pointer_component_name: name(),
        ) => DataPointerObject::Component {
            scalar_variable,
            data_pointer_component_name,
        }),
    )
    .parse(source)
}

#[derive(Debug, Clone)]
pub struct BoundsSpec<Span> {
    pub lower_bound: LowerBoundExpr<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "bounds-spec" #1035 : "is lower-bound-expr :",
)]
pub fn bounds_spec<S: Lexed>(source: S) -> PResult<BoundsSpec<MultilineSpan>, S> {
    seq!((lower_bound: lower_bound_expr, _: colon()) => BoundsSpec { lower_bound })
        .parse(source)
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
    seq!((lower_bound_expr: lower_bound_expr, _: colon(), upper_bound_expr: upper_bound_expr) => BoundsRemapping {
        lower_bound_expr,
        upper_bound_expr,
    })
    .parse(source)
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
    seq!((
        _: kw!(WHERE),
        _: delim('('),
        mask_expr: mask_expr,
        _: delim(')'),
        where_assignment_stmt: where_assignment_stmt,
    ) => WhereStmt {
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
    seq!((
        where_construct_name: seq!((n: name(), _: colon()) => n).opt(),
        _: kw!(WHERE),
        _: delim('('),
        mask_expr: mask_expr,
        _: delim(')'),
    ) => WhereConstructStmt {
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
pub fn where_assignment_stmt<S: Lexed>(
    source: S,
) -> PResult<WhereAssignmentStmt<MultilineSpan>, S> {
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
pub fn masked_elsewhere_stmt<S: Lexed>(
    source: S,
) -> PResult<MaskedElsewhereStmt<MultilineSpan>, S> {
    seq!((
        _: kw!(ELSEWHERE),
        _: delim('('),
        mask_expr: mask_expr,
        _: delim(')'),
        where_construct_name: name().opt(),
    ) => MaskedElsewhereStmt {
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
    seq!((
        _: kw!(ELSEWHERE),
        where_construct_name: name().opt(),
    ) => ElsewhereStmt {
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
    seq!((
        _: kw!(END),
        _: kw!(WHERE),
        where_construct_name: name().opt(),
    ) => EndWhereStmt {
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
pub fn forall_construct_stmt<S: Lexed>(
    source: S,
) -> PResult<ForallConstructStmt<MultilineSpan>, S> {
    seq!((
        forall_construct_name: seq!((n: name(), _: colon()) => n).opt(),
        _: kw!(forall),
        concurrent_header: concurrent_header,
    ) => ForallConstructStmt {
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
    )
    .parse(source)
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
pub fn internal_file_variable<S: Lexed>(
    source: S,
) -> PResult<InternalFileVariable<MultilineSpan>, S> {
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
    seq!((
        _: kw!(open),
        _: delim('('),
        connect_spec_list: list(connect_spec, 1..),
        _: delim(')'),
    ) => OpenStmt {
        connect_spec_list
    }).parse(source)
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
        seq!(
            (_: seq!((_: kw!(unit), _: equals()) => ()).opt(),
             file_unit_number: file_unit_number)
            => ConnectSpec::FileUnitNumber(file_unit_number)
        ),
        seq!((_: kw!(access), _: equals(), scalar_default_char_expr: default_char_expr) => ConnectSpec::Access(scalar_default_char_expr)),
        seq!((_: kw!(action), _: equals(), scalar_default_char_expr: default_char_expr) => ConnectSpec::Action(scalar_default_char_expr)),
        seq!((_: kw!(asynchronous), _: equals(), scalar_default_char_expr: default_char_expr) => ConnectSpec::Asynchronous(scalar_default_char_expr)),
        seq!((_: kw!(blank), _: equals(), scalar_default_char_expr: default_char_expr) => ConnectSpec::Blank(scalar_default_char_expr)),
        seq!((_: kw!(decimal), _: equals(), scalar_default_char_expr: default_char_expr) => ConnectSpec::Decimal(scalar_default_char_expr)),
        seq!((_: kw!(delim), _: equals(), scalar_default_char_expr: default_char_expr) => ConnectSpec::Delim(scalar_default_char_expr)),
        seq!((_: kw!(encoding), _: equals(), scalar_default_char_expr: default_char_expr) => ConnectSpec::Encoding(scalar_default_char_expr)),
        seq!((_: kw!(err), _: equals(), label: label()) => ConnectSpec::Err(label)),
        seq!((_: kw!(file), _: equals(), file_name_expr: file_name_expr) => ConnectSpec::File(file_name_expr)),
        seq!((_: kw!(form), _: equals(), scalar_default_char_expr: default_char_expr) => ConnectSpec::Form(scalar_default_char_expr)),
        seq!((_: kw!(iomsg), _: equals(), iomsg_variable: iomsg_variable) => ConnectSpec::Iomsg(iomsg_variable)),
        seq!((_: kw!(iostat), _: equals(), stat_variable: stat_variable) => ConnectSpec::Iostat(stat_variable)),
        seq!((_: kw!(newunit), _: equals(), scalar_int_variable: int_variable(false)) => ConnectSpec::Newunit(scalar_int_variable)), // TODO false???
        seq!((_: kw!(pad), _: equals(), scalar_default_char_expr: default_char_expr) => ConnectSpec::Pad(scalar_default_char_expr)),
        seq!((_: kw!(position), _: equals(), scalar_default_char_expr: default_char_expr) => ConnectSpec::Position(scalar_default_char_expr)),
        seq!((_: kw!(recl), _: equals(), scalar_int_expr: int_expr) => ConnectSpec::Recl(scalar_int_expr)),
        seq!((_: kw!(round), _: equals(), scalar_default_char_expr: default_char_expr) => ConnectSpec::Round(scalar_default_char_expr)),
        seq!((_: kw!(sign), _: equals(), scalar_default_char_expr: default_char_expr) => ConnectSpec::Sign(scalar_default_char_expr)),
        seq!((_: kw!(status), _: equals(), scalar_default_char_expr: default_char_expr) => ConnectSpec::Status(scalar_default_char_expr)),
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
    default_char_variable(false)
        .map(IomsgVariable)
        .parse(source)
}

#[derive(Debug, Clone)]
pub struct CloseStmt<Span> {
    pub close_spec_list: Vec<CloseSpec<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "close-stmt" #1208 : "is CLOSE ( close-spec-list )",
)]
pub fn close_stmt<S: Lexed>(source: S) -> PResult<CloseStmt<MultilineSpan>, S> {
    seq!((
        _: kw!(close),
        _: delim('('),
        close_spec_list: list(close_spec, 1..),
        _: delim(')')
    ) => CloseStmt { close_spec_list })
        .parse(source)
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
        seq!((_: seq!((_: kw!(unit), _: equals()) => ()).opt(), file_unit_number: file_unit_number) => CloseSpec::FileUnitNumber(file_unit_number)),
        seq!((_: kw!(iostat), _: equals(), stat_variable: stat_variable) => CloseSpec::Iostat(stat_variable)),
        seq!((_: kw!(iomsg), _: equals(), iomsg_variable: iomsg_variable) => CloseSpec::Iomsg(iomsg_variable)),
        seq!((_: kw!(err), _: equals(), label: label()) => CloseSpec::Err(label)),
        seq!((_: kw!(status), _: equals(), scalar_default_char_expr: default_char_expr) => CloseSpec::Status(scalar_default_char_expr)),
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
        seq!((_: (kw!(read), delim('(')), io_control_spec_list: list(io_control_spec, 1..), _: delim(')'), input_item_list: list(input_item, 0..).map(Some)) => ReadStmt::IoControlSpecList(io_control_spec_list, input_item_list.unwrap_or_default())),
        seq!((_: kw!(read), format: format, input_item_list: seq!((_: comma(), input_item_list: list(input_item, 0..)) => input_item_list).opt()) => ReadStmt::Format(format, input_item_list)),
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
    seq!((_: (kw!(write), delim('(')), io_control_spec_list: list(io_control_spec, 1..), _: delim(')'), output_item_list: list(output_item, 1..).opt()) => WriteStmt {
        io_control_spec_list,
        output_item_list,
    })
        .parse(source)
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
    seq!((_: kw!(print), format: format, output_item_list: seq!((_: comma(), output_item_list: list(output_item, 1..)) => output_item_list).opt()) => PrintStmt {
        format,
        output_item_list,
    })
        .parse(source)
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
        seq!((_: seq!((_: kw!(UNIT), _: equals()) => ()).opt(), io_unit: io_unit) => IoControlSpec::Unit(io_unit)),
        seq!((_: seq!((_: kw!(FMT), _: equals()) => ()).opt(), format: format) => IoControlSpec::Fmt(format)),
        seq!((_: seq!((_: kw!(NML), _: equals()) => ()).opt(), namelist_group_name: name()) => IoControlSpec::Nml(namelist_group_name)),
        seq!((_: kw!(ADVANCE), _: equals(), scalar_default_char_expr: default_char_expr) => IoControlSpec::Advance(scalar_default_char_expr)),
        seq!((_: kw!(ASYNCHRONOUS), _: equals(), scalar_default_char_constant_expr: default_char_constant_expr) => IoControlSpec::Asynchronous(scalar_default_char_constant_expr)),
        seq!((_: kw!(BLANK), _: equals(), scalar_default_char_expr: default_char_expr) => IoControlSpec::Blank(scalar_default_char_expr)),
        seq!((_: kw!(DECIMAL), _: equals(), scalar_default_char_expr: default_char_expr) => IoControlSpec::Decimal(scalar_default_char_expr)),
        seq!((_: kw!(DELIM), _: equals(), scalar_default_char_expr: default_char_expr) => IoControlSpec::Delim(scalar_default_char_expr)),
        seq!((_: kw!(END), _: equals(), label: label()) => IoControlSpec::End(label)),
        seq!((_: kw!(EOR), _: equals(), label: label()) => IoControlSpec::Eor(label)),
        seq!((_: kw!(ERR), _: equals(), label: label()) => IoControlSpec::Err(label)),
        seq!((_: kw!(ID), _: equals(), id_variable: id_variable) => IoControlSpec::Id(id_variable)),
        seq!((_: kw!(IOMSG), _: equals(), iomsg_variable: iomsg_variable) => IoControlSpec::Iomsg(iomsg_variable)),
        seq!((_: kw!(IOSTAT), _: equals(), stat_variable: stat_variable) => IoControlSpec::Iostat(stat_variable)),
        seq!((_: kw!(PAD), _: equals(), scalar_default_char_expr: default_char_expr) => IoControlSpec::Pad(scalar_default_char_expr)),
        seq!((_: kw!(POS), _: equals(), scalar_int_expr: int_expr) => IoControlSpec::Pos(scalar_int_expr)),
        seq!((_: kw!(REC), _: equals(), scalar_int_expr: int_expr) => IoControlSpec::Rec(scalar_int_expr)),
        seq!((_: kw!(ROUND), _: equals(), scalar_default_char_expr: default_char_expr) => IoControlSpec::Round(scalar_default_char_expr)),
        seq!((_: kw!(SIGN), _: equals(), scalar_default_char_expr: default_char_expr) => IoControlSpec::Sign(scalar_default_char_expr)),
        seq!((_: kw!(SIZE), _: equals(), scalar_int_variable: int_variable(true)) => IoControlSpec::Size(scalar_int_variable)), // TODO true???
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
    )
    .parse(source)
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
    )
    .parse(source)
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
    )
    .parse(source)
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
    seq!((_: delim('('), io_implied_do_object_list: list(io_implied_do_object, 1..), _: comma(), io_implied_do_control: io_implied_do_control, _: delim(')')) => IoImpliedDo {
        io_implied_do_object_list,
        io_implied_do_control,
    })
        .parse(source)
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
    )
    .parse(source)
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
    seq!((do_variable: variable(true), _: equals(), scalar_int_expr_list: list(int_expr, 2..)) => IoImpliedDoControl {
        do_variable,
        scalar_int_expr_list,
    })
        .parse(source)
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
        seq!((_: kw!(TYPE), _: delim('('), derived_type_spec: derived_type_spec, _: delim(')')) => DtvTypeSpec::Type(derived_type_spec)),
        seq!((_: kw!(CLASS), _: delim('('), derived_type_spec: derived_type_spec, _: delim(')')) => DtvTypeSpec::Class(derived_type_spec)),
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
    seq!((_: (kw!(wait), delim('(')), wait_spec_list: list(wait_spec, 1..), _: delim(')')) => WaitStmt { wait_spec_list })
        .parse(source)
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
        seq!((_: seq!((_: kw!(UNIT), _: equals()) => ()).opt(), file_unit_number: file_unit_number) => WaitSpec::FileUnitNumber(file_unit_number)),
        seq!((_: kw!(END), _: equals(), label: label()) => WaitSpec::End(label)),
        seq!((_: kw!(EOR), _: equals(), label: label()) => WaitSpec::Eor(label)),
        seq!((_: kw!(ERR), _: equals(), label: label()) => WaitSpec::Err(label)),
        seq!((_: kw!(ID), _: equals(), scalar_int_expr: int_expr) => WaitSpec::Id(scalar_int_expr)),
        seq!((_: kw!(IOMSG), _: equals(), iomsg_variable: iomsg_variable) => WaitSpec::Iomsg(iomsg_variable)),
        seq!((_: kw!(IOSTAT), _: equals(), stat_variable: stat_variable) => WaitSpec::Iostat(stat_variable)),
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
        seq!((_: kw!(ENDFILE), file_unit_number: file_unit_number) => EndfileStmt::FileUnitNumber(file_unit_number)),
        seq!((_: kw!(ENDFILE), _: delim('('), position_spec_list: list(position_spec, 1..), _: delim(')')) => EndfileStmt::PositionSpecList(position_spec_list)),
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
        seq!((_: kw!(REWIND), file_unit_number: file_unit_number) => RewindStmt::FileUnitNumber(file_unit_number)),
        seq!((_: kw!(REWIND), _: delim('('), position_spec_list: list(position_spec, 1..), _: delim(')')) => RewindStmt::PositionSpecList(position_spec_list)),
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
        seq!((_: seq!((_: kw!(UNIT), _: equals()) => ()).opt(), file_unit_number: file_unit_number) => PositionSpec::FileUnitNumber(file_unit_number)),
        seq!((_: kw!(IOMSG), _: equals(), iomsg_variable: iomsg_variable) => PositionSpec::Iomsg(iomsg_variable)),
        seq!((_: kw!(IOSTAT), _: equals(), stat_variable: stat_variable) => PositionSpec::Iostat(stat_variable)),
        seq!((_: kw!(ERR), _: equals(), label: label()) => PositionSpec::Err(label)),
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
        seq!((_: kw!(FLUSH), file_unit_number: file_unit_number) => FlushStmt::FileUnitNumber(file_unit_number)),
        seq!((_: kw!(FLUSH), _: delim('('), flush_spec_list: list(flush_spec, 1..), _: delim(')')) => FlushStmt::FlushSpecList(flush_spec_list)),
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
        seq!((_: seq!((_: kw!(UNIT), _: equals()) => ()).opt(), file_unit_number: file_unit_number) => FlushSpec::FileUnitNumber(file_unit_number)),
        seq!((_: kw!(IOSTAT), _: equals(), stat_variable: stat_variable) => FlushSpec::Iostat(stat_variable)),
        seq!((_: kw!(IOMSG), _: equals(), iomsg_variable: iomsg_variable) => FlushSpec::Iomsg(iomsg_variable)),
        seq!((_: kw!(ERR), _: equals(), label: label()) => FlushSpec::Err(label)),
    ).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum InquireStmt<Span> {
    InquireSpecList(Vec<InquireSpec<Span>>),
    IoLength(IntVariable<Span>, Vec<OutputItem<Span>>),
}

#[doc = s_rule!(
    F18V007r1 rule "inquire-stmt" #1230 :
    "is INQUIRE ( inquire-spec-list )"
    "or INQUIRE ( IOLENGTH = scalar-int-variable ) output-item-list",
)]
pub fn inquire_stmt<S: Lexed>(source: S) -> PResult<InquireStmt<MultilineSpan>, S> {
    alt!(
        for S =>
        seq!((_: (kw!(INQUIRE), delim('(')), inquire_spec_list: list(inquire_spec, 1..), _: delim(')')) => InquireStmt::InquireSpecList(inquire_spec_list)),
        seq!((kw_delim_kw_eq: (kw!(INQUIRE), delim('('), kw!(IOLENGTH), equals()), scalar_int_variable: int_variable(false), _: delim(')'), output_item_list: list(output_item, 0..)) => InquireStmt::IoLength(scalar_int_variable, output_item_list)),
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
        seq!((_: seq!((_: kw!(unit), _: equals()) => ()).opt(), file_unit_number: file_unit_number) => InquireSpec::FileUnitNumber(file_unit_number)),
        seq!((_: kw!(FILE), _: equals(), file_name_expr: file_name_expr) => InquireSpec::File(file_name_expr)),
        seq!((_: kw!(ACCESS), _: equals(), scalar_default_char_variable: default_char_variable(false)) => InquireSpec::Access(scalar_default_char_variable)),
        seq!((_: kw!(ACTION), _: equals(), scalar_default_char_variable: default_char_variable(false)) => InquireSpec::Action(scalar_default_char_variable)),
        seq!((_: kw!(ASYNCHRONOUS), _: equals(), scalar_default_char_variable: default_char_variable(false)) => InquireSpec::Asynchronous(scalar_default_char_variable)),
        seq!((_: kw!(BLANK), _: equals(), scalar_default_char_variable: default_char_variable(false)) => InquireSpec::Blank(scalar_default_char_variable)),
        seq!((_: kw!(DECIMAL), _: equals(), scalar_default_char_variable: default_char_variable(false)) => InquireSpec::Decimal(scalar_default_char_variable)),
        seq!((_: kw!(DELIM), _: equals(), scalar_default_char_variable: default_char_variable(false)) => InquireSpec::Delim(scalar_default_char_variable)),
        seq!((_: kw!(DIRECT), _: equals(), scalar_default_char_variable: default_char_variable(false)) => InquireSpec::Direct(scalar_default_char_variable)),
        seq!((_: kw!(ENCODING), _: equals(), scalar_default_char_variable: default_char_variable(false)) => InquireSpec::Encoding(scalar_default_char_variable)),
        seq!((_: kw!(ERR), _: equals(), label: label()) => InquireSpec::Err(label)),
        seq!((_: kw!(EXIST), _: equals(), scalar_logical_variable: logical_variable(false)) => InquireSpec::Exist(scalar_logical_variable)), // TODO false???
        seq!((_: kw!(FORM), _: equals(), scalar_default_char_variable: default_char_variable(false)) => InquireSpec::Form(scalar_default_char_variable)),
        seq!((_: kw!(FORMATTED), _: equals(), scalar_default_char_variable: default_char_variable(false)) => InquireSpec::Formatted(scalar_default_char_variable)),
        seq!((_: kw!(ID), _: equals(), scalar_int_expr: int_expr) => InquireSpec::Id(scalar_int_expr)),
        seq!((_: kw!(IOMSG), _: equals(), iomsg_variable: iomsg_variable) => InquireSpec::Iomsg(iomsg_variable)),
        seq!((_: kw!(IOSTAT), _: equals(), stat_variable: stat_variable) => InquireSpec::Iostat(stat_variable)),
        seq!((_: kw!(NAME), _: equals(), scalar_default_char_variable: default_char_variable(false)) => InquireSpec::Name(scalar_default_char_variable)),
        seq!((_: kw!(NAMED), _: equals(), scalar_logical_variable: logical_variable(false)) => InquireSpec::Named(scalar_logical_variable)),
        seq!((_: kw!(NEXTREC), _: equals(), scalar_int_variable: int_variable(false)) => InquireSpec::Nextrec(scalar_int_variable)),
        seq!((_: kw!(NUMBER), _: equals(), scalar_int_variable: int_variable(false)) => InquireSpec::Number(scalar_int_variable)),
        seq!((_: kw!(OPENED), _: equals(), scalar_logical_variable: logical_variable(false)) => InquireSpec::Opened(scalar_logical_variable)),
        seq!((_: kw!(PAD), _: equals(), scalar_default_char_variable: default_char_variable(false)) => InquireSpec::Pad(scalar_default_char_variable)),
        seq!((_: kw!(PENDING), _: equals(), scalar_logical_variable: logical_variable(false)) => InquireSpec::Pending(scalar_logical_variable)),
        seq!((_: kw!(POS), _: equals(), scalar_int_variable: int_variable(false)) => InquireSpec::Pos(scalar_int_variable)),
        seq!((_: kw!(POSITION), _: equals(), scalar_default_char_variable: default_char_variable(false)) => InquireSpec::Position(scalar_default_char_variable)),
        seq!((_: kw!(READ), _: equals(), scalar_default_char_variable: default_char_variable(false)) => InquireSpec::Read(scalar_default_char_variable)),
        seq!((_: kw!(READWRITE), _: equals(), scalar_default_char_variable: default_char_variable(false)) => InquireSpec::Readwrite(scalar_default_char_variable)),
        seq!((_: kw!(RECL), _: equals(), scalar_int_variable: int_variable(false)) => InquireSpec::Recl(scalar_int_variable)),
        seq!((_: kw!(ROUND), _: equals(), scalar_default_char_variable: default_char_variable(false)) => InquireSpec::Round(scalar_default_char_variable)),
        seq!((_: kw!(SEQUENTIAL), _: equals(), scalar_default_char_variable: default_char_variable(false)) => InquireSpec::Sequential(scalar_default_char_variable)),
        seq!((_: kw!(SIGN), _: equals(), scalar_default_char_variable: default_char_variable(false)) => InquireSpec::Sign(scalar_default_char_variable)),
        seq!((_: kw!(SIZE), _: equals(), scalar_int_variable: int_variable(false)) => InquireSpec::Size(scalar_int_variable)),
        seq!((_: kw!(STREAM), _: equals(), scalar_default_char_variable: default_char_variable(false)) => InquireSpec::Stream(scalar_default_char_variable)),
        seq!((_: kw!(UNFORMATTED), _: equals(), scalar_default_char_variable: default_char_variable(false)) => InquireSpec::Unformatted(scalar_default_char_variable)),
        seq!((_: kw!(WRITE), _: equals(), scalar_default_char_variable: default_char_variable(false)) => InquireSpec::Write(scalar_default_char_variable)),
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
        seq!((_: kw!(backspace), file_unit_number: file_unit_number) => BackspaceStmt::FileUnitNumber(file_unit_number)),
        seq!((_: kw!(backspace), _: delim('('), position_spec_list: list(position_spec, 1..), _: delim(')')) => BackspaceStmt::PositionSpecList(position_spec_list)),
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
    seq!((
        scalar_variable: variable(true),
        _: percent(),
        procedure_component_name: name(),
    ) => ProcComponentRef {
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
    )
    .parse(source)
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
    use crate::rule_test;

    use super::*;

    #[test]
    fn tmp() {
        let expr = |s: &str| expr(tokenize(s).as_slice()).map(|(r, _)| r);

        assert!(expr("1").is_some());
        assert!(expr("1")
            .unwrap()
            .right
            .right
            .right
            .right
            .expr
            .right
            .right
            .right
            .right
            .expr
            .primary
            .is_literal());
        assert!(expr("2.1 + 3.4 + 4.9").is_some());
        assert!(expr("2.1 * 3.4 * 4.9").is_some());
        assert!(expr("2.1 / 3.4 / 4.9").is_some());
        assert!(expr("2 ** 3 ** 4").is_some());
        assert!(expr("'AB' // 'CD' // 'EF'").is_some());
    }

    rule_test! {
        array_element(F18V007r1 917) {
            examples(|s| array_element(s), [
                "arr(1)",
                // TODO ...
            ]);
        }
    }

    rule_test! {
        array_section(F18V007r1 918) {
            examples(|s| array_section(false).parse(s), [
                "arr(1)",
                "arr(:1)",
                "arr(1:1)",
                "arr(:)",
                "arr%field(1:1)",
                // TODO ...
            ]);
        }
    }

    rule_test! {
        subscript(F18V007r1 919) {
            examples(|s| subscript(s), [
                "1+1**1",
                // TODO ...
            ]);
        }
    }

    rule_test! {
        section_subscript(F18V007r1 920) {
            examples(|s| section_subscript(s), [
                "1",
                "1:1:1",
                // TODO ...
            ]);
        }
    }

    rule_test! {
        subscript_triplet(F18V007r1 921) {
            examples(|s| subscript_triplet(s), [
                ":",
                "1:",
                ":1",
                "1:1",
                "1:1:1",
                ":1:1",
                "1::1",
                "::1",
            ]);
        }
    }

    rule_test! {
        stride(F18V007r1 922) {
            examples(|s| stride(s), [
                "1",
                "42",
                "1+(1**1)"
            ]);
        }
    }

    rule_test! {
        vector_subscript(F18V007r1 923) {
            examples(|s| vector_subscript(s), [
                "1",
                "42",
                "1+(1**1)"
            ]);
        }
    }

    //rule_test! {
    //    // TODO tmp
    //    type_param_inquiry(F18V007r1 924) {
    //        examples(|s| designator(true).parse(s), [
    //            "A % B",
    //        ]);
    //        examples(|s| percent().parse(s), [
    //            "%",
    //        ]);
    //        examples(|s| name().parse(s), [
    //            "B",
    //        ]);
    //        examples(|s| type_param_inquiry(s), [
    //            "A % B",
    //        ]);
    //    }
    //}

    rule_test! {
        primary(F18V007r1 1001) {
            examples(|s| primary(s), [
                "A",
                "B(1:1)"
            ]);

            // from the standard
            assert!(example(|s| primary(s), "1.0").is_literal());
            // TODO assert!(example(|s| primary(s), "'ABCDEFGHIJKLMNOPQRSTUVWXYZ' (I:I)").is_literal());
            assert!(example(|s| primary(s), "[ 1.0, 2.0 ]").is_array_constructor());
            assert!(example(|s| primary(s), "PERSON ('Jones', 12)").is_structure_constructor());
            // TODO assert!(example(|s| primary(s), "F (X, Y)").is_function_reference());
            //assert!(example(|s| primary(s), "X%KIND").is_type_param_inquiry(), "is {:#?}", example(|s| primary(s), "X%KIND"));
            // TODO assert!(example(|s| primary(s), "KIND").is_type_param_name());
            assert!(example(|s| primary(s), "(S + T)").is_parenthesized_expr());

            examples(|s| primary(s), [
                "1.0",
                "'ABCDEFGHIJKLMNOPQRSTUVWXYZ' (I:I)",
                "[ 1.0, 2.0 ]",
                "PERSON ('Jones', 12)",
                "F (X, Y)",
                "X%KIND",
                "KIND",
                "(S + T)",
            ]);
        }
    }

    rule_test! {
        level_1_expr(F18V007r1 1002) {
            // from the standard
            examples(|s| level_1_expr(s), [
                "A",
                ".INVERSE. B",
                ".INVERSE. (A + B)",
            ]);
        }
    }

    rule_test! {
        mult_operand(F18V007r1 1004) {
            // NOTE: mult-operand is level-1-expr [ power-op mult-operand ]
            // so every level-1-expr is a valid mult-operand and we can combine them with power-op
            // and still be a valid mult-operand. We do not test all combinations of the previous
            // rules to avoid exponential growth of the number of cases.
            examples(|s| mult_operand(s), [
                "A",
                ".INVERSE. B",
                ".INVERSE. (A + B)",
                "A ** A",
                ".INVERSE. B ** .INVERSE. B",
                ".INVERSE. (A + B) ** .INVERSE. (A + B)",
            ]);
        }
    }

    rule_test! {
        add_operand(F18V007r1 1005) {
            examples(|s| add_operand(s), [
                "A ** A",
                ".INVERSE. B ** .INVERSE. B",
                ".INVERSE. (A + B) ** .INVERSE. (A + B)",
                "A ** A * A ** A",
                ".INVERSE. B ** .INVERSE. B * .INVERSE. B ** .INVERSE. B",
                ".INVERSE. (A + B) ** .INVERSE. (A + B) * .INVERSE. (A + B) ** .INVERSE. (A + B)",
            ]);
        }
    }

    rule_test! {
        level_2_expr(F18V007r1 1006) {
            examples(|s| level_2_expr(s), [
                "A ** A * A ** A",
                ".INVERSE. B ** .INVERSE. B * .INVERSE. B ** .INVERSE. B",
                ".INVERSE. (A + B) ** .INVERSE. (A + B) * .INVERSE. (A + B) ** .INVERSE. (A + B)",
                "A ** A * A ** A + A ** A * A ** A",
                ".INVERSE. B ** .INVERSE. B * .INVERSE. B ** .INVERSE. B + .INVERSE. B ** .INVERSE. B * .INVERSE. B ** .INVERSE. B",
                ".INVERSE. (A + B) ** .INVERSE. (A + B) * .INVERSE. (A + B) ** .INVERSE. (A + B) + .INVERSE. (A + B) ** .INVERSE. (A + B) * .INVERSE. (A + B) ** .INVERSE. (A + B)",
            ]);

            // from the standard
            examples(|s| level_2_expr(s), [
                "A",
                "B ** C",
                "D * E",
                "+1",
                "F - I",
                "- A + D * E + B ** C",
            ]);
        }
    }

    rule_test! {
        level_3_expr(F18V007r1 1010) {
            examples(|s| level_3_expr(s), [
                "A",
                "B ** C",
                "D * E",
                "+1",
                "F - I",
                "- A + D * E + B ** C",
                "A // A",
                "B ** C // B ** C",
                "D * E // D * E",
                "+1 // +1",
                "F - I // F - I",
                "- A + D * E + B ** C // - A + D * E + B ** C",
            ]);

            // from the standard
            examples(|s| level_3_expr(s), [
                "A",
                "B // C",
                "X // Y // 'ABCD'",
            ]);
        }
    }

    rule_test! {
        level_4_expr(F18V007r1 1012) {
            examples(|s| level_4_expr(s), [
                "A",
                "B // C",
                "X // Y // 'ABCD'",
                "A .EQ. A",
                "B // C .EQ. B // C",
                "X // Y // 'ABCD' .EQ. X // Y // 'ABCD'",
                "A .NE. A",
                "B // C .NE. B // C",
                "X // Y // 'ABCD' .NE. X // Y // 'ABCD'",
                "A .LT. A",
                "B // C .LT. B // C",
                "X // Y // 'ABCD' .LT. X // Y // 'ABCD'",
                "A .LE. A",
                "B // C .LE. B // C",
                "X // Y // 'ABCD' .LE. X // Y // 'ABCD'",
                "A .GT. A",
                "B // C .GT. B // C",
                "X // Y // 'ABCD' .GT. X // Y // 'ABCD'",
                "A .GE. A",
                "B // C .GE. B // C",
                "X // Y // 'ABCD' .GE. X // Y // 'ABCD'",
                "A == A",
                "B // C == B // C",
                "X // Y // 'ABCD' == X // Y // 'ABCD'",
                "A /= A",
                "B // C /= B // C",
                "X // Y // 'ABCD' /= X // Y // 'ABCD'",
                "A < A",
                "B // C < B // C",
                "X // Y // 'ABCD' < X // Y // 'ABCD'",
                "A <= A",
                "B // C <= B // C",
                "X // Y // 'ABCD' <= X // Y // 'ABCD'",
                "A > A",
                "B // C > B // C",
                "X // Y // 'ABCD' > X // Y // 'ABCD'",
                "A >= A",
                "B // C >= B // C",
                "X // Y // 'ABCD' >= X // Y // 'ABCD'",
            ]);

            // from the standard
            examples(|s| level_4_expr(s), [
                "A",
                "B == C",
                "D < E",
                "(A + B) /= C",
            ]);
        }
    }

    rule_test! {
        and_operand(F18V007r1 1014) {
            examples(|s| and_operand(s), [
                "A",
                "B == C",
                "D < E",
                "(A + B) /= C",
                ".not. A",
                ".not. B == C",
                ".not. D < E",
                ".not. (A + B) /= C",
            ]);
        }
    }

    rule_test! {
        or_operand(F18V007r1 1015) {
            examples(|s| or_operand(s), [
                "A",
                "B == C",
                "D < E",
                "(A + B) /= C",
                ".not. A",
                ".not. B == C",
                ".not. D < E",
                ".not. (A + B) /= C",
                "A .AND. A",
                "B == C .AND. B == C",
                "D < E .AND. D < E",
                "(A + B) /= C .AND. (A + B) /= C",
                ".not. A .AND. .not. A",
                ".not. B == C .AND. .not. B == C",
                ".not. D < E .AND. .not. D < E",
                ".not. (A + B) /= C .AND. .not. (A + B) /= C",
            ]);
        }
    }

    rule_test! {
        equiv_operand(F18V007r1 1016) {
            examples(|s| equiv_operand(s), [
                "A",
                "B == C",
                "D < E",
                "(A + B) /= C",
                ".not. A",
                ".not. B == C",
                ".not. D < E",
                ".not. (A + B) /= C",
                "A .AND. A",
                "B == C .AND. B == C",
                "D < E .AND. D < E",
                "(A + B) /= C .AND. (A + B) /= C",
                ".not. A .AND. .not. A",
                ".not. B == C .AND. .not. B == C",
                ".not. D < E .AND. .not. D < E",
                ".not. (A + B) /= C .AND. .not. (A + B) /= C",
                "A .OR. A",
                "B == C .OR. B == C",
                "D < E .OR. D < E",
                "(A + B) /= C .OR. (A + B) /= C",
                ".not. A .OR. .not. A",
                ".not. B == C .OR. .not. B == C",
                ".not. D < E .OR. .not. D < E",
                ".not. (A + B) /= C .OR. .not. (A + B) /= C",
                "A .AND. A .OR. A .AND. A",
                "B == C .AND. B == C .OR. B == C .AND. B == C",
                "D < E .AND. D < E .OR. D < E .AND. D < E",
                "(A + B) /= C .AND. (A + B) /= C .OR. (A + B) /= C .AND. (A + B) /= C",
                ".not. A .AND. .not. A .OR. .not. A .AND. .not. A",
                ".not. B == C .AND. .not. B == C .OR. .not. B == C .AND. .not. B == C",
                ".not. D < E .AND. .not. D < E .OR. .not. D < E .AND. .not. D < E",
                ".not. (A + B) /= C .AND. .not. (A + B) /= C .OR. .not. (A + B) /= C .AND. .not. (A + B) /= C",
            ]);
        }
    }

    rule_test! {
        level_5_expr(F18V007r1 1017) {
            examples(|s| level_5_expr(s), [
                "A",
                "B == C",
                "D < E",
                "(A + B) /= C",
                ".not. A",
                ".not. B == C",
                ".not. D < E",
                ".not. (A + B) /= C",
                "A .AND. A",
                "B == C .AND. B == C",
                "D < E .AND. D < E",
                "(A + B) /= C .AND. (A + B) /= C",
                ".not. A .AND. .not. A",
                ".not. B == C .AND. .not. B == C",
                ".not. D < E .AND. .not. D < E",
                ".not. (A + B) /= C .AND. .not. (A + B) /= C",
                "A .OR. A",
                "B == C .OR. B == C",
                "D < E .OR. D < E",
                "(A + B) /= C .OR. (A + B) /= C",
                ".not. A .OR. .not. A",
                ".not. B == C .OR. .not. B == C",
                ".not. D < E .OR. .not. D < E",
                ".not. (A + B) /= C .OR. .not. (A + B) /= C",
                "A .AND. A .OR. A .AND. A",
                "B == C .AND. B == C .OR. B == C .AND. B == C",
                "D < E .AND. D < E .OR. D < E .AND. D < E",
                "(A + B) /= C .AND. (A + B) /= C .OR. (A + B) /= C .AND. (A + B) /= C",
                ".not. A .AND. .not. A .OR. .not. A .AND. .not. A",
                ".not. B == C .AND. .not. B == C .OR. .not. B == C .AND. .not. B == C",
                ".not. D < E .AND. .not. D < E .OR. .not. D < E .AND. .not. D < E",
                ".not. (A + B) /= C .AND. .not. (A + B) /= C .OR. .not. (A + B) /= C .AND. .not. (A + B) /= C",
                "A .EQV. A",
                "B == C .EQV. B == C",
                "D < E .EQV. D < E",
                "(A + B) /= C .EQV. (A + B) /= C",
                ".not. A .EQV. .not. A",
                ".not. B == C .EQV. .not. B == C",
                ".not. D < E .EQV. .not. D < E",
                ".not. (A + B) /= C .EQV. .not. (A + B) /= C",
                "A .AND. A .EQV. A .AND. A",
                "B == C .AND. B == C .EQV. B == C .AND. B == C",
                "D < E .AND. D < E .EQV. D < E .AND. D < E",
                "(A + B) /= C .AND. (A + B) /= C .EQV. (A + B) /= C .AND. (A + B) /= C",
                ".not. A .AND. .not. A .EQV. .not. A .AND. .not. A",
                ".not. B == C .AND. .not. B == C .EQV. .not. B == C .AND. .not. B == C",
                ".not. D < E .AND. .not. D < E .EQV. .not. D < E .AND. .not. D < E",
                ".not. (A + B) /= C .AND. .not. (A + B) /= C .EQV. .not. (A + B) /= C .AND. .not. (A + B) /= C",
                "A .OR. A .EQV. A .OR. A",
                "B == C .OR. B == C .EQV. B == C .OR. B == C",
                "D < E .OR. D < E .EQV. D < E .OR. D < E",
                "(A + B) /= C .OR. (A + B) /= C .EQV. (A + B) /= C .OR. (A + B) /= C",
                ".not. A .OR. .not. A .EQV. .not. A .OR. .not. A",
                ".not. B == C .OR. .not. B == C .EQV. .not. B == C .OR. .not. B == C",
                ".not. D < E .OR. .not. D < E .EQV. .not. D < E .OR. .not. D < E",
                ".not. (A + B) /= C .OR. .not. (A + B) /= C .EQV. .not. (A + B) /= C .OR. .not. (A + B) /= C",
                "A .AND. A .OR. A .AND. A .EQV. A .AND. A .OR. A .AND. A",
                "B == C .AND. B == C .OR. B == C .AND. B == C .EQV. B == C .AND. B == C .OR. B == C .AND. B == C",
                "D < E .AND. D < E .OR. D < E .AND. D < E .EQV. D < E .AND. D < E .OR. D < E .AND. D < E",
                "(A + B) /= C .AND. (A + B) /= C .OR. (A + B) /= C .AND. (A + B) /= C .EQV. (A + B) /= C .AND. (A + B) /= C .OR. (A + B) /= C .AND. (A + B) /= C",
                ".not. A .AND. .not. A .OR. .not. A .AND. .not. A .EQV. .not. A .AND. .not. A .OR. .not. A .AND. .not. A",
                ".not. B == C .AND. .not. B == C .OR. .not. B == C .AND. .not. B == C .EQV. .not. B == C .AND. .not. B == C .OR. .not. B == C .AND. .not. B == C",
                ".not. D < E .AND. .not. D < E .OR. .not. D < E .AND. .not. D < E .EQV. .not. D < E .AND. .not. D < E .OR. .not. D < E .AND. .not. D < E",
                ".not. (A + B) /= C .AND. .not. (A + B) /= C .OR. .not. (A + B) /= C .AND. .not. (A + B) /= C .EQV. .not. (A + B) /= C .AND. .not. (A + B) /= C .OR. .not. (A + B) /= C .AND. .not. (A + B) /= C",
                "A .NEQV. A",
                "B == C .NEQV. B == C",
                "D < E .NEQV. D < E",
                "(A + B) /= C .NEQV. (A + B) /= C",
                ".not. A .NEQV. .not. A",
                ".not. B == C .NEQV. .not. B == C",
                ".not. D < E .NEQV. .not. D < E",
                ".not. (A + B) /= C .NEQV. .not. (A + B) /= C",
                "A .AND. A .NEQV. A .AND. A",
                "B == C .AND. B == C .NEQV. B == C .AND. B == C",
                "D < E .AND. D < E .NEQV. D < E .AND. D < E",
                "(A + B) /= C .AND. (A + B) /= C .NEQV. (A + B) /= C .AND. (A + B) /= C",
                ".not. A .AND. .not. A .NEQV. .not. A .AND. .not. A",
                ".not. B == C .AND. .not. B == C .NEQV. .not. B == C .AND. .not. B == C",
                ".not. D < E .AND. .not. D < E .NEQV. .not. D < E .AND. .not. D < E",
                ".not. (A + B) /= C .AND. .not. (A + B) /= C .NEQV. .not. (A + B) /= C .AND. .not. (A + B) /= C",
                "A .OR. A .NEQV. A .OR. A",
                "B == C .OR. B == C .NEQV. B == C .OR. B == C",
                "D < E .OR. D < E .NEQV. D < E .OR. D < E",
                "(A + B) /= C .OR. (A + B) /= C .NEQV. (A + B) /= C .OR. (A + B) /= C",
                ".not. A .OR. .not. A .NEQV. .not. A .OR. .not. A",
                ".not. B == C .OR. .not. B == C .NEQV. .not. B == C .OR. .not. B == C",
                ".not. D < E .OR. .not. D < E .NEQV. .not. D < E .OR. .not. D < E",
                ".not. (A + B) /= C .OR. .not. (A + B) /= C .NEQV. .not. (A + B) /= C .OR. .not. (A + B) /= C",
                "A .AND. A .OR. A .AND. A .NEQV. A .AND. A .OR. A .AND. A",
                "B == C .AND. B == C .OR. B == C .AND. B == C .NEQV. B == C .AND. B == C .OR. B == C .AND. B == C",
                "D < E .AND. D < E .OR. D < E .AND. D < E .NEQV. D < E .AND. D < E .OR. D < E .AND. D < E",
                "(A + B) /= C .AND. (A + B) /= C .OR. (A + B) /= C .AND. (A + B) /= C .NEQV. (A + B) /= C .AND. (A + B) /= C .OR. (A + B) /= C .AND. (A + B) /= C",
                ".not. A .AND. .not. A .OR. .not. A .AND. .not. A .NEQV. .not. A .AND. .not. A .OR. .not. A .AND. .not. A",
                ".not. B == C .AND. .not. B == C .OR. .not. B == C .AND. .not. B == C .NEQV. .not. B == C .AND. .not. B == C .OR. .not. B == C .AND. .not. B == C",
                ".not. D < E .AND. .not. D < E .OR. .not. D < E .AND. .not. D < E .NEQV. .not. D < E .AND. .not. D < E .OR. .not. D < E .AND. .not. D < E",
                ".not. (A + B) /= C .AND. .not. (A + B) /= C .OR. .not. (A + B) /= C .AND. .not. (A + B) /= C .NEQV. .not. (A + B) /= C .AND. .not. (A + B) /= C .OR. .not. (A + B) /= C .AND. .not. (A + B) /= C",
            ]);

            // from the standard
            examples(|s| level_5_expr(s), [
                "A",
                ".NOT. B",
                "C .AND. D",
                "E .OR. F",
                "G .EQV. H",
                "S .NEQV. T",
                "A .AND. B .EQV. .NOT. C",
            ]);
        }
    }

    //rule_test! {
    //    designator(F18V007r1 901) {
    //        todo!()
    //    }
    //}

    rule_test! {
        expr(F18V007r1 1022, F18V007r1 1024, F18V007r1 1025, F18V007r1 1026, F18V007r1 1027, F18V007r1 1028, F18V007r1 1029, F18V007r1 1030, F18V007r1 1031) {
            examples(|s| expr(s), [
                "A",
                ".NOT. B",
                "C .AND. D",
                "E .OR. F",
                "G .EQV. H",
                "S .NEQV. T",
                "A .AND. B .EQV. .NOT. C",
                "A .foo. A",
                ".NOT. B .foo. .NOT. B",
                "C .AND. D .foo. C .AND. D",
                "E .OR. F .foo. E .OR. F",
                "G .EQV. H .foo. G .EQV. H",
                "S .NEQV. T .foo. S .NEQV. T",
                "A .AND. B .EQV. .NOT. C .foo. A .AND. B .EQV. .NOT. C",
            ]);

            // from the standard
            examples(|s| expr(s), [
                "A",
                "B.UNION.C",
                "(B .INTERSECT. C) .UNION. (X - Y)",
                "A + B == C * D",
                ".INVERSE. (A + B)",
                "A + B .AND. C * D",
                "E // G == H (1:10)",
            ]);
        }
    }
}

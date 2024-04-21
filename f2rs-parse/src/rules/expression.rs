use super::*;

#[derive(Debug, Clone)]
pub struct Expr<Span> {
    pub left: Option<(Box<Expr<Span>>, DefinedUnaryOrBinaryOp<Span>)>,
    pub right: Level5Expr<Span>,
}

// TODO test
#[syntax_rule(
    F18V007r1 rule "expr" #1022 :
    "is [ expr defined-binary-op ] level-5-expr",
)]
pub fn expr<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = Expr<S::Span>> + 'a {
    // TODO test
    move |source: S| {
        let e5 = level_5_expr(cfg);
        let bin_op = defined_binary_op(cfg);
        let right_part = {
            let e5 = e5.clone();
            (
                space(0),
                bin_op,
                space(0),
                e5
            ).map(|(_, op, _, expr)| (op, expr))
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
}

#[derive(Debug, Clone)]
pub struct IntExpr<Span>(pub Expr<Span>);

// TODO test
#[syntax_rule(
    F18V007r1 rule "int-expr" #1026 :
    "is expr",
)]
pub fn int_expr<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = IntExpr<S::Span>> + 'a {
    expr(cfg).map(IntExpr)
}

#[derive(Debug, Clone)]
pub struct IntConstantExpr<Span>(pub IntExpr<Span>);

// TODO test
#[syntax_rule(
    F18V007r1 rule "int-constant-expr" #1031 :
    "is int-expr",
)]
pub fn int_constant_expr<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = IntConstantExpr<S::Span>> + 'a {
    int_expr(cfg).map(IntConstantExpr)
}

#[derive(Debug, Clone)]
pub struct ConstantExpr<Span>(pub Expr<Span>);

// TODO test
#[syntax_rule(
    F18V007r1 rule "constant-expr" #1029 :
    "is expr",
)]
pub fn constant_expr<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ConstantExpr<S::Span>> + 'a {
    expr(cfg).map(ConstantExpr)
}

#[derive(Debug, Clone)]
pub struct DefaultCharConstantExpr<Span>(pub DefaultCharExpr<Span>);

// TODO test
#[syntax_rule(
    F18V007r1 rule "default-char-constant-expr" #1030 :
    "is default-char-expr",
)]
pub fn default_char_constant_expr<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DefaultCharConstantExpr<S::Span>> + 'a {
    default_char_expr(cfg).map(DefaultCharConstantExpr)
}

#[derive(Debug, Clone)]
pub struct SpecificationExpr<Span>(pub IntExpr<Span>);

// TODO test
#[syntax_rule(
    F18V007r1 rule "specification-expr" #1028 :
    "is scalar-int-expr",
)]
pub fn specification_expr<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = SpecificationExpr<S::Span>> + 'a {
    int_expr(cfg).map(SpecificationExpr)
}

// TODO test
#[syntax_rule(
    F18V007r1 rule "logical-expr" #1024 :
    "is expr",
)]
pub fn logical_expr<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = Expr<S::Span>> + 'a {
    expr(cfg)
}

#[derive(Debug, Clone)]
pub struct DefaultCharExpr<Span>(pub Expr<Span>);

// TODO test
#[syntax_rule(
    F18V007r1 rule "default-char-expr" #1025 :
    "is expr",
)]
pub fn default_char_expr<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DefaultCharExpr<S::Span>> + 'a {
    expr(cfg).map(DefaultCharExpr)
}

// TODO test
#[syntax_rule(
    F18V007r1 rule "numeric-expr" #1027 :
    "is expr",
)]
pub fn numeric_expr<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = Expr<S::Span>> + 'a {
    expr(cfg)
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

// TODO test
#[syntax_rule(
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
pub fn primary<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = Primary<S::Span>> + 'a {
    alt! {
        literal_constant(cfg).map(Primary::Literal),
        designator(cfg, false).map(Primary::Designator),
        array_constructor(cfg).map(Primary::ArrayConstructor),
        structure_constructor(cfg).map(Primary::StructureConstructor),
        function_reference(cfg).map(Primary::FunctionReference),
        type_param_inquiry(cfg).map(Primary::TypeParamInquiry),
        name(cfg, false).map(Primary::TypeParamName),
        (
            '(',
            space(0),
            expr(cfg),
            space(0),
            ')',
        ).map(|(_, _, expr, _, _)| Primary::ParenthesizedExpr(Box::new(expr))),
    }
}

#[derive(Debug, Clone)]
pub struct Level1Expr<Span> {
    pub operator: Option<DefinedUnaryOrBinaryOp<Span>>,
    pub primary: Box<Primary<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "level-1-expr" #1002 : "is [ defined-unary-op ] primary",
)]
pub fn level_1_expr<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = Level1Expr<S::Span>> + 'a {
    // TODO test
    (
        defined_unary_op(cfg).optional(),
        space(0),
        primary(cfg),
    ).map(|(operator, _, primary)| Level1Expr {
        operator,
        primary: Box::new(primary),
    })
}

#[derive(Debug, Clone)]
pub struct MultOperand<Span> {
    pub expr: Level1Expr<Span>,
    pub exp: Option<(PowerOp<Span>, Box<MultOperand<Span>>)>,
}

#[syntax_rule(
    F18V007r1 rule "mult-operand" #1004 : "is level-1-expr [ power-op mult-operand ]",
)]
pub fn mult_operand<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = MultOperand<S::Span>> + 'a {
    // TODO test
    move |source: S| (
        level_1_expr(cfg),
        space(0),
        (
            power_op(cfg),
            space(0),
            mult_operand(cfg),
        ).map(|(power_op, _, mult_operand)| (power_op, Box::new(mult_operand))).optional(),
    ).map(|(expr, _, exp)| MultOperand {
        expr,
        exp,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct AddOperand<Span> {
    pub left: Option<(Box<AddOperand<Span>>, MultOp<Span>)>,
    pub right: MultOperand<Span>,
}

#[syntax_rule(
    F18V007r1 rule "add-operand" #1005 : "is [ add-operand mult-op ] mult-operand",
)]
pub fn add_operand<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = AddOperand<S::Span>> + 'a {
    // TODO test
    move |source: S| {
        let mult_operand = mult_operand(cfg);
        let add_op = mult_op(cfg);
        let right_part = {
            let mult_operand = mult_operand.clone();
            (
                space(0),
                add_op,
                space(0),
                mult_operand,
            ).map(|(_, op, _, expr)| (op, expr))
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
}

#[derive(Debug, Clone)]
pub struct Level2Expr<Span> {
    pub left: Option<(Option<Box<Level2Expr<Span>>>, AddOp<Span>)>,
    pub right: AddOperand<Span>,
}

#[syntax_rule(
    F18V007r1 rule "level-2-expr" #1006 : "is [ [ level-2-expr ] add-op ] add-operand",
)]
pub fn level_2_expr<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = Level2Expr<S::Span>> + 'a {
    // TODO test
    move |source: S| {
        let add_operand = add_operand(cfg);
        let add_op = add_op(cfg);
        let right_part = {
            let add_op = add_op.clone();
            let add_operand = add_operand.clone();
            (
                space(0),
                add_op,
                space(0),
                add_operand,
            ).map(|(_, op, _, expr)| (op, expr))
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
}

#[derive(Debug, Clone)]
pub struct Level3Expr<Span> {
    pub left: Option<(Box<Level3Expr<Span>>, ConcatOp<Span>)>,
    pub right: Level2Expr<Span>,
}

#[syntax_rule(
    F18V007r1 rule "level-3-expr" #1010 : "is [ level-3-expr concat-op ] level-2-expr",
)]
pub fn level_3_expr<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = Level3Expr<S::Span>> + 'a {
    // TODO test
    move |source: S| {
        let level_2_expr = level_2_expr(cfg);
        let concat_op = concat_op(cfg);
        let right_part = {
            let level_2_expr = level_2_expr.clone();
            (
                space(0),
                concat_op,
                space(0),
                level_2_expr,
            ).map(|(_, op, _, expr)| (op, expr))
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
}

#[derive(Debug, Clone)]
pub struct Level4Expr<Span> {
    pub left: Option<(Box<Level4Expr<Span>>, RelOp<Span>)>,
    pub right: Level3Expr<Span>,
}

#[syntax_rule(
    F18V007r1 rule "level-4-expr" #1012 : "is [ level-3-expr rel-op ] level-3-expr",
)]
pub fn level_4_expr<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = Level4Expr<S::Span>> + 'a {
    // TODO test
    move |source: S| {
        let level_3_expr = level_3_expr(cfg);
        let rel_op = rel_op(cfg);
        let right_part = {
            let level_3_expr = level_3_expr.clone();
            (
                space(0),
                rel_op,
                space(0),
                level_3_expr,
            ).map(|(_, op, _, expr)| (op, expr))
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
}

#[derive(Debug, Clone)]
pub struct AndOperand<Span> {
    pub operator: Option<NotOp<Span>>,
    pub expr: Level4Expr<Span>,
}

#[syntax_rule(
    F18V007r1 rule "and-operand" #1014 : "is [ not-op ] level-4-expr",
)]
pub fn and_operand<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = AndOperand<S::Span>> + 'a {
    // TODO test
    (
        not_op(cfg).optional(),
        level_4_expr(cfg),
    ).map(|(operator, expr)| AndOperand {
        operator,
        expr,
    })
}

#[derive(Debug, Clone)]
pub struct OrOperand<Span> {
    pub left: Option<(Box<OrOperand<Span>>, AndOp<Span>)>,
    pub right: AndOperand<Span>,
}

#[syntax_rule(
    F18V007r1 rule "or-operand" #1015 : "is [ or-operand and-op ] and-operand",
)]
pub fn or_operand<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = OrOperand<S::Span>> + 'a {
    // TODO test
    move |source: S| {
        let and_operand = and_operand(cfg);
        let and_op = and_op(cfg);
        let right_part = {
            let and_operand = and_operand.clone();
            (
                space(0),
                and_op,
                space(0),
                and_operand,
            ).map(|(_, op, _, expr)| (op, expr))
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
}

#[derive(Debug, Clone)]
pub struct EquivOperand<Span> {
    pub left: Option<(Box<EquivOperand<Span>>, OrOp<Span>)>,
    pub right: OrOperand<Span>,
}

#[syntax_rule(
    F18V007r1 rule "equiv-operand" #1016 : "is [ equiv-operand or-op ] or-operand",
)]
pub fn equiv_operand<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = EquivOperand<S::Span>> + 'a {
    // TODO test
    move |source: S| {
        let or_operand = or_operand(cfg);
        let or_op = or_op(cfg);
        let right_part = {
            let or_operand = or_operand.clone();
            (
                space(0),
                or_op,
                space(0),
                or_operand,
            ).map(|(_, op, _, expr)| (op, expr))
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
}

#[derive(Debug, Clone)]
pub struct Level5Expr<Span> {
    pub left: Option<(Box<Level5Expr<Span>>, EquivOp<Span>)>,
    pub right: EquivOperand<Span>,
}

#[syntax_rule(
    F18V007r1 rule "level-5-expr" #1017 : "is [ level-5-expr equiv-op ] equiv-operand",
)]
pub fn level_5_expr<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = Level5Expr<S::Span>> + 'a {
    // TODO test
    move |source: S| {
        let equiv_operand = equiv_operand(cfg);
        let equiv_op = equiv_op(cfg);
        let right_part = {
            let equiv_operand = equiv_operand.clone();
            (
                space(0),
                equiv_op,
                space(0),
                equiv_operand,
            ).map(|(_, op, _, expr)| (op, expr))
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

#[syntax_rule(
    F18V007r1 rule "designator" #901 :
    "is object-name"
    "or array-element"
    "or array-section"
    "or coindexed-named-object"
    "or complex-part-designator"
    "or structure-component"
    "or substring",
)]
pub fn designator<'a, S: TextSource + 'a>(
    cfg: &'a Cfg,
    not_complex_part_designator: bool,
) -> impl Parser<S, Token = Designator<S::Span>> + 'a {
    // TODO test
    alt!(
        object_name(cfg).map(Designator::ObjectName),
        array_element(cfg).map(Designator::ArrayElement),
        array_section(cfg, not_complex_part_designator).map(Designator::ArraySection),
        coindexed_named_object(cfg).map(Designator::CoindexedNamedObject),
        complex_part_designator(cfg).map(Designator::ComplexPartDesignator).if_(!not_complex_part_designator),
        structure_component(cfg).map(Designator::StructureComponent),
        substring(cfg).map(Designator::Substring),
    )
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum Variable<Span> {
    Designator(Designator<Span>),
    FunctionReference(FunctionReference<Span>),
}

#[syntax_rule(
    F18V007r1 rule "variable" #902 :
    "is designator"
    "or function-reference",
)]
pub fn variable<'a, S: TextSource + 'a>(
    cfg: &'a Cfg,
    not_function_reference: bool,
) -> impl Parser<S, Token = Variable<S::Span>> + 'a {
    // TODO test
    alt!(
        designator(cfg, false).map(Variable::Designator),
        function_reference(cfg).map(Variable::FunctionReference).if_(!not_function_reference),
    )
}

#[derive(Debug, Clone)]
pub struct VariableName<Span>(pub Name<Span>);

#[syntax_rule(
    F18V007r1 rule "variable-name" #903 : "is name",
)]
pub fn variable_name<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = VariableName<S::Span>> + 'a {
    name(cfg, false).map(VariableName)
}

#[derive(Debug, Clone)]
pub struct LogicalVariable<Span>(pub Variable<Span>);

#[syntax_rule(
    F18V007r1 rule "logical-variable" #904 : "is variable",
)]
pub fn logical_variable<'a, S: TextSource + 'a>(
    cfg: &'a Cfg,
    not_function_reference: bool,
) -> impl Parser<S, Token = LogicalVariable<S::Span>> + 'a {
    // TODO test
    variable(cfg, not_function_reference).map(LogicalVariable)
}

#[derive(Debug, Clone)]
pub struct CharVariable<Span>(pub Variable<Span>);

#[syntax_rule(
    F18V007r1 rule "char-variable" #905 : "is variable",
)]
pub fn char_variable<'a, S: TextSource + 'a>(
    cfg: &'a Cfg,
    not_function_reference: bool,
) -> impl Parser<S, Token = CharVariable<S::Span>> + 'a {
    // TODO test
    variable(cfg, not_function_reference).map(CharVariable)
}

#[derive(Debug, Clone)]
pub struct DefaultCharVariable<Span>(pub Variable<Span>);

#[syntax_rule(
    F18V007r1 rule "default-char-variable" #906 : "is variable",
)]
pub fn default_char_variable<'a, S: TextSource + 'a>(
    cfg: &'a Cfg,
    not_function_reference: bool,
) -> impl Parser<S, Token = DefaultCharVariable<S::Span>> + 'a {
    // TODO test
    variable(cfg, not_function_reference).map(DefaultCharVariable)
}

#[derive(Debug, Clone)]
pub struct IntVariable<Span>(pub Variable<Span>);

#[syntax_rule(
    F18V007r1 rule "int-variable" #907 : "is variable",
)]
pub fn int_variable<'a, S: TextSource + 'a>(
    cfg: &'a Cfg,
    not_function_reference: bool,
) -> impl Parser<S, Token = IntVariable<S::Span>> + 'a {
    // TODO test
    variable(cfg, not_function_reference).map(IntVariable)
}

#[derive(Debug, Clone)]
pub struct Substring<Span> {
    pub parent: ParentString<Span>,
    pub range: SubstringRange<Span>,
}

#[syntax_rule(
    F18V007r1 rule "substring" #908 : "is parent-string ( substring-range )",
)]
pub fn substring<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = Substring<S::Span>> + 'a {
    // TODO test
    (
        parent_string(cfg),
        (space(0), '(', space(0)),
        substring_range(cfg),
        (space(0), ')', space(0)),
    ).map(|(parent, _, range, _)| Substring {
        parent,
        range,
    })
}



#[derive(Debug, Clone, EnumAsInner)]
pub enum ParentString<Span> {
    ScalarVariable(VariableName<Span>),
    ArrayElement(ArrayElement<Span>),
    CoindexedNamedObject(CoindexedNamedObject<Span>),
    ScalarStructureComponent(StructureComponent<Span>),
    ScalarConstant(Constant<Span>),
}

#[syntax_rule(
    F18V007r1 rule "parent-string" #909 :
    "is scalar-variable-name"
    "or array-element"
    "or coindexed-named-object"
    "or scalar-structure-component"
    "or scalar-constant",
)]
pub fn parent_string<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ParentString<S::Span>> + 'a {
    // TODO test
    alt!(
        variable_name(cfg).map(ParentString::ScalarVariable),
        array_element(cfg).map(ParentString::ArrayElement),
        coindexed_named_object(cfg).map(ParentString::CoindexedNamedObject),
        structure_component(cfg).map(ParentString::ScalarStructureComponent),
        constant(cfg).map(ParentString::ScalarConstant),
    )
}

#[derive(Debug, Clone)]
pub struct SubstringRange<Span> {
    pub left: Option<IntExpr<Span>>,
    pub right: Option<IntExpr<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "substring-range" #910 : "is [ scalar-int-expr ] : [ scalar-int-expr ]",
)]
pub fn substring_range<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = SubstringRange<S::Span>> + 'a {
    // TODO test
    (
        int_expr(cfg).optional(),
        space(0),
        ':',
        space(0),
        int_expr(cfg).optional(),
    ).map(|(left, _, _, _, right)| SubstringRange {
        left,
        right,
    })
}

#[derive(Debug, Clone)]
pub struct DataRef<Span> {
    pub part: PartRef<Span>,
    pub selectors: Vec<PartRef<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "data-ref" #911 : "is part-ref [ % part-ref ] ...",
)]
pub fn data_ref<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DataRef<S::Span>> + 'a {
    (
        part_ref(cfg),
        list(
            (space(0), '%', space(0), part_ref(cfg)).map(|(_, _, _, part_ref)| part_ref),
            0..,
        ),
    ).map(|(part, selectors)| DataRef {
        part,
        selectors,
    })
}

#[derive(Debug, Clone)]
pub struct PartRef<Span> {
    pub part_name: Name<Span>,
    pub section_subscript_list: Option<Vec<Subscript<Span>>>,
    pub image_selector: Option<ImageSelector<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "part-ref" #912 : "is part-name [ ( section-subscript-list ) ] [ image-selector ]",
)]
pub fn part_ref<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = PartRef<S::Span>> + 'a {
    // TODO test
    (
        name(cfg, false),
        (
            (space(0), '(', space(0)),
            list(
                (subscript(cfg), space(0)).map(|(subscript, _)| subscript),
                0..,
            ),
            (space(0), ')', space(0)),
        ).map(|(_, list, _)| list).optional(),
        image_selector(cfg).optional(),
    ).map(|(part_name, section_subscript_list, image_selector)| PartRef {
        part_name,
        section_subscript_list,
        image_selector,
    })
}

#[derive(Debug, Clone)]
pub struct StructureComponent<Span>(pub DataRef<Span>);

#[syntax_rule(
    F18V007r1 rule "structure-component" #913 : "is data-ref",
)]
pub fn structure_component<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = StructureComponent<S::Span>> + 'a {
    // TODO test
    data_ref(cfg).map(StructureComponent)
}

#[derive(Debug, Clone)]
pub struct CoindexedNamedObject<Span>(pub DataRef<Span>);

#[syntax_rule(
    F18V007r1 rule "coindexed-named-object" #914 : "is data-ref",
)]
pub fn coindexed_named_object<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = CoindexedNamedObject<S::Span>> + 'a {
    // TODO test
    data_ref(cfg).map(CoindexedNamedObject)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ComplexPartDesignator<Span> {
    Re(Box<Designator<Span>>),
    Im(Box<Designator<Span>>),
}

#[syntax_rule(
    F18V007r1 rule "complex-part-designator" #915 :
    "is designator % RE"
    "or designator % IM",
)]
pub fn complex_part_designator<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ComplexPartDesignator<S::Span>> + 'a {
    alt!(
        (designator(cfg, true), space(0), '%', space(0), kw("re", cfg)).map(|(designator, _, _, _, _)| ComplexPartDesignator::Re(Box::new(designator))),
        (designator(cfg, true), space(0), '%', space(0), kw("im", cfg)).map(|(designator, _, _, _, _)| ComplexPartDesignator::Im(Box::new(designator))),
    )
}

#[derive(Debug, Clone)]
pub struct TypeParamInquiry<Span> {
    pub designator: Designator<Span>,
    pub type_param_name: Name<Span>,
}

#[syntax_rule(
    F18V007r1 rule "type-param-inquiry" #916 : "is designator % type-param-name",
)]
pub fn type_param_inquiry<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = TypeParamInquiry<S::Span>> + 'a {
    // TODO test
    (
        designator(cfg, true),
        space(0), '%' , space(0),
        name(cfg, false),
    ).map(|(designator, _, _, _, type_param_name)| TypeParamInquiry {
        designator,
        type_param_name,
    })
}

#[derive(Debug, Clone)]
pub struct ArrayElement<Span>(pub DataRef<Span>);

#[syntax_rule(
    F18V007r1 rule "array-element" #917 : "is data-ref",
)]
pub fn array_element<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ArrayElement<S::Span>> + 'a {
    // TODO test
    data_ref(cfg).map(ArrayElement)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ArraySection<Span> {
    Data(DataRef<Span>, Option<SubstringRange<Span>>),
    ComplexPartDesignator(ComplexPartDesignator<Span>),
}

#[syntax_rule(
    F18V007r1 rule "array-section" #918 :
    "is data-ref [ ( substring-range ) ]"
    "or complex-part-designator",
)]
pub fn array_section<'a, S: TextSource + 'a>(
    cfg: &'a Cfg,
    not_complex_part_designator: bool,
) -> impl Parser<S, Token = ArraySection<S::Span>> + 'a {
    alt!(
        (
            data_ref(cfg), space(0),
            (
                (space(0), '(', space(0)),
                substring_range(cfg),
                (space(0), ')', space(0)),
            ).map(|(_, range, _)| range).optional(),
        ).map(|(data_ref, _, range)| ArraySection::Data(data_ref, range)),
        complex_part_designator(cfg).map(ArraySection::ComplexPartDesignator).if_(!not_complex_part_designator),
    )
}

#[derive(Debug, Clone)]
pub struct Subscript<Span>(pub IntExpr<Span>);

#[syntax_rule(
    F18V007r1 rule "subscript" #919 : "is scalar-int-expr",
)]
pub fn subscript<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = Subscript<S::Span>> + 'a {
    // TODO test
    int_expr(cfg).map(Subscript)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum SectionSubscript<Span> {
    Subscript(Subscript<Span>),
    SubscriptTriplet(SubscriptTriplet<Span>),
    VectorSubscript(VectorSubscript<Span>),
}

#[syntax_rule(
    F18V007r1 rule "section-subscript" #920 :
    "is subscript"
    "or subscript-triplet"
    "or vector-subscript",
)]
pub fn section_subscript<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = SectionSubscript<S::Span>> + 'a {
    // TODO test
    alt!(
        subscript(cfg).map(SectionSubscript::Subscript),
        subscript_triplet(cfg).map(SectionSubscript::SubscriptTriplet),
        vector_subscript(cfg).map(SectionSubscript::VectorSubscript),
    )
}

#[derive(Debug, Clone)]
pub struct SubscriptTriplet<Span> {
    pub lower: Option<Subscript<Span>>,
    pub upper: Option<Subscript<Span>>,
    pub stride: Option<Stride<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "subscript-triplet" #921 : "is [ subscript ] : [ subscript ] [ : stride ]",
)]
pub fn subscript_triplet<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = SubscriptTriplet<S::Span>> + 'a {
    (
        subscript(cfg).optional(),
        (space(0), ':', space(0)),
        subscript(cfg).optional(),
        (
            (space(0), ':', space(0)),
            stride(cfg),
        ).map(|(_, subscript)| subscript).optional(),
    ).map(|(lower, _, upper, stride)| SubscriptTriplet {
        lower,
        upper,
        stride,
    })
}

#[derive(Debug, Clone)]
pub struct Stride<Span>(pub IntExpr<Span>);

#[syntax_rule(
    F18V007r1 rule "stride" #922 : "is scalar-int-expr",
)]
pub fn stride<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = Stride<S::Span>> + 'a {
    // TODO test
    int_expr(cfg).map(Stride)
}

#[derive(Debug, Clone)]
pub struct VectorSubscript<Span>(pub IntExpr<Span>);

#[syntax_rule(
    F18V007r1 rule "vector-subscript" #923 : "is int-expr",
)]
pub fn vector_subscript<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = VectorSubscript<S::Span>> + 'a {
    // TODO test
    int_expr(cfg).map(VectorSubscript)
}

#[derive(Debug, Clone)]
pub struct ImageSelector<Span> {
    pub cosubscript_list: Vec<Cosubscript<Span>>,
    pub image_selector_spec_list: Vec<ImageSelectorSpec<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "image-selector" #924 : "is lbracket cosubscript-list [ , image-selector-spec-list ] rbracket",
)]
pub fn image_selector<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ImageSelector<S::Span>> + 'a {
    (
        (lbracket(cfg), space(0)),
        list(cosubscript(cfg), 0..),
        (
            space(0), ',', space(0),
            list(image_selector_spec(cfg), 0..),
        ).map(|(_, _, _, image_selector_spec_list)| image_selector_spec_list),
        (space(0), rbracket(cfg), space(0)),
    ).map(|(_, cosubscript_list, image_selector_spec_list, _)| ImageSelector {
        cosubscript_list,
        image_selector_spec_list,
    })
}

#[derive(Debug, Clone)]
pub struct Cosubscript<Span>(pub IntExpr<Span>);

#[syntax_rule(
    F18V007r1 rule "cosubscript" #925 : "is scalar-int-expr",
)]
pub fn cosubscript<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = Cosubscript<S::Span>> + 'a {
    // TODO test
    int_expr(cfg).map(Cosubscript)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ImageSelectorSpec<Span> {
    Stat(StatVariable<Span>),
    Team(TeamValue<Span>),
    TeamNumber(IntExpr<Span>),
}

#[syntax_rule(
    F18V007r1 rule "image-selector-spec" #926 :
    "is STAT = stat-variable"
    "or TEAM = team-value"
    "or TEAM_NUMBER = scalar-int-expr",
)]
pub fn image_selector_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ImageSelectorSpec<S::Span>> + 'a {
    // TODO test
    alt!(
        (kw("stat", cfg), space(0), '=', space(0), stat_variable(cfg)).map(|(_, _, _, _, stat_variable)| ImageSelectorSpec::Stat(stat_variable)),
        (kw("team", cfg), space(0), '=', space(0), team_value(cfg)).map(|(_, _, _, _, team_value)| ImageSelectorSpec::Team(team_value)),
        (kw("team_number", cfg), space(0), '=', space(0), int_expr(cfg)).map(|(_, _, _, _, int_texpr)| ImageSelectorSpec::TeamNumber(int_texpr)),
    )
}

#[derive(Debug, Clone)]
pub struct TeamValue<Span>(pub Expr<Span>);

#[syntax_rule(
    F18V007r1 rule "team-value" #1115 : "is scalar-expr",
)]
pub fn team_value<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = TeamValue<S::Span>> + 'a {
    // TODO test
    expr(cfg).map(TeamValue)
}

#[derive(Debug, Clone)]
pub struct AllocateStmt<Span> {
    pub type_spec: Option<TypeSpec<Span>>,
    pub allocation_list: Vec<Allocation<Span>>,
    pub alloc_opt_list: Option<Vec<AllocOpt<Span>>>,
}

#[syntax_rule(
    F18V007r1 rule "allocate-stmt" #927 :
    "is ALLOCATE ( [ type-spec :: ] allocation-list [ , alloc-opt-list ] )",
)]
pub fn allocate_stmt_2<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = AllocateStmt<S::Span>> + 'a {
    (
        (kw("allocate", cfg)),
        (space(0), '(', space(0)),
        (
            type_spec(cfg),
            space(0), "::", space(0),
        ).map(|(type_spec, _, _, _)| type_spec).optional(),
        list(allocation(cfg), 1..),
        (
            space(0), ',', space(0),
            list(alloc_opt(cfg), 0..),
        ).map(|(_, _, _, alloc_opt_list)| alloc_opt_list).optional(),
        (space(0), ')', space(0)),
    ).map(|(_, _, type_spec, allocation_list, alloc_opt_list, _)| AllocateStmt {
        type_spec,
        allocation_list,
        alloc_opt_list,
    })
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum AllocOpt<Span> {
    Errmsg(ErrmsgVariable<Span>),
    Mold(SourceExpr<Span>),
    Source(SourceExpr<Span>),
    Stat(StatVariable<Span>),
}

#[syntax_rule(
    F18V007r1 rule "alloc-opt" #928 :
    "is ERRMSG = errmsg-variable"
    "or MOLD = source-expr"
    "or SOURCE = source-expr"
    "or STAT = stat-variable",
)]
pub fn alloc_opt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = AllocOpt<S::Span>> + 'a {
    // TODO test
    alt!(
        (kw("errmsg", cfg), space(0), '=', space(0), errmsg_variable(cfg)).map(|(_, _, _, _, errmsg_variable)| AllocOpt::Errmsg(errmsg_variable)),
        (kw("mold", cfg), space(0), '=', space(0), source_expr(cfg)).map(|(_, _, _, _, source_expr)| AllocOpt::Mold(source_expr)),
        (kw("source", cfg), space(0), '=', space(0), source_expr(cfg)).map(|(_, _, _, _, source_expr)| AllocOpt::Source(source_expr)),
        (kw("stat", cfg), space(0), '=', space(0), stat_variable(cfg)).map(|(_, _, _, _, stat_variable)| AllocOpt::Stat(stat_variable)),
    )
}

#[derive(Debug, Clone)]
pub struct ErrmsgVariable<Span>(pub DefaultCharVariable<Span>);

#[syntax_rule(
    F18V007r1 rule "errmsg-variable" #929 : "is scalar-default-char-variable",
)]
pub fn errmsg_variable<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ErrmsgVariable<S::Span>> + 'a {
    // TODO test
    default_char_variable(cfg, false).map(ErrmsgVariable)
}

#[derive(Debug, Clone)]
pub struct SourceExpr<Span>(pub Expr<Span>);

#[syntax_rule(
    F18V007r1 rule "source-expr" #930 : "is expr",
)]
pub fn source_expr<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = SourceExpr<S::Span>> + 'a {
    // TODO test
    expr(cfg).map(SourceExpr)
}

#[derive(Debug, Clone)]
pub struct Allocation<Span> {
    pub object: AllocateObject<Span>,
    pub allocate_shape_spec_list: Option<Vec<AllocateShapeSpec<Span>>>,
    pub allocate_coarray_spec: Option<AllocateCoarraySpec<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "allocation" #931 :
    "is allocate-object [ ( allocate-shape-spec-list ) ] [ lbracket allocate-coarray-spec rbracket ]",
)]
pub fn allocation<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = Allocation<S::Span>> + 'a {
    // TODO test
    (
        allocate_object(cfg), space(0),
        (
            '(', space(0),
            list(allocate_shape_spec(cfg), 0..),
            space(0), ')', space(0),
        ).map(|(_, _, allocate_shape_spec_list, _, _, _)| allocate_shape_spec_list).optional(),
        (
            lbracket(cfg), space(0),
            allocate_coarray_spec(cfg),
            space(0), rbracket(cfg), space(0),
        ).map(|(_, _, allocate_coarray_spec, _, _, _)| allocate_coarray_spec).optional(),
    ).map(|(object, _, allocate_shape_spec_list, allocate_coarray_spec)| Allocation {
        object,
        allocate_shape_spec_list,
        allocate_coarray_spec,
    })
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum AllocateObject<Span> {
    VariableName(VariableName<Span>),
    StructureComponent(StructureComponent<Span>),
}

#[syntax_rule(
    F18V007r1 rule "allocate-object" #932 :
    "is variable-name"
    "or structure-component",
)]
pub fn allocate_object<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = AllocateObject<S::Span>> + 'a {
    alt!(
        variable_name(cfg).map(AllocateObject::VariableName),
        structure_component(cfg).map(AllocateObject::StructureComponent),
    )
}

#[derive(Debug, Clone)]
pub struct AllocateShapeSpec<Span> {
    pub lower_bound: Option<LowerBoundExpr<Span>>,
    pub upper_bound: UpperBoundExpr<Span>,
}

#[syntax_rule(
    F18V007r1 rule "allocate-shape-spec" #933 : "is [ lower-bound-expr : ] upper-bound-expr",
)]
pub fn allocate_shape_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = AllocateShapeSpec<S::Span>> + 'a {
    // TODO test
    (
        (
            lower_bound_expr(cfg),
            space(0), ':', space(0),
        ).map(|(lower_bound, _, _, _)| lower_bound).optional(),
        upper_bound_expr(cfg),
    ).map(|(lower_bound, upper_bound)| AllocateShapeSpec {
        lower_bound,
        upper_bound,
    })
}

#[derive(Debug, Clone)]
pub struct LowerBoundExpr<Span>(pub IntExpr<Span>);

#[syntax_rule(
    F18V007r1 rule "lower-bound-expr" #934 : "is scalar-int-expr",
)]
pub fn lower_bound_expr<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = LowerBoundExpr<S::Span>> + 'a {
    // TODO test
    int_expr(cfg).map(LowerBoundExpr)
}

#[derive(Debug, Clone)]
pub struct UpperBoundExpr<Span>(pub IntExpr<Span>);

#[syntax_rule(
    F18V007r1 rule "upper-bound-expr" #935 : "is scalar-int-expr",
)]
pub fn upper_bound_expr<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = UpperBoundExpr<S::Span>> + 'a {
    // TODO test
    int_expr(cfg).map(UpperBoundExpr)
}

#[derive(Debug, Clone)]
pub struct AllocateCoarraySpec<Span> {
    pub allocate_coshape_spec_list: Vec<AllocateCoshapeSpec<Span>>,
    pub lower_bound: Option<LowerBoundExpr<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "allocate-coarray-spec" #936 : "is [ allocate-coshape-spec-list , ] [ lower-bound-expr : ] *",
)]
pub fn allocate_coarray_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = AllocateCoarraySpec<S::Span>> + 'a {
    (
        (
            list(allocate_coshape_spec(cfg), 0..),
            space(0), ',', space(0),
        ).map(|(allocate_coshape_spec_list, _, _, _)| allocate_coshape_spec_list).optional(),
        (
            lower_bound_expr(cfg),
            space(0), ':', space(0),
        ).map(|(lower_bound, _, _, _)| lower_bound).optional(),
        SpecialCharacter::Asterisk,
    ).map(|(allocate_coshape_spec_list, lower_bound, _)| AllocateCoarraySpec {
        allocate_coshape_spec_list: allocate_coshape_spec_list.unwrap_or_default(),
        lower_bound,
    })
}

#[derive(Debug, Clone)]
pub struct AllocateCoshapeSpec<Span> {
    pub lower_bound: Option<LowerBoundExpr<Span>>,
    pub upper_bound: UpperBoundExpr<Span>,
}

#[syntax_rule(
    F18V007r1 rule "allocate-coshape-spec" #937 : "is [ lower-bound-expr : ] upper-bound-expr",
)]
pub fn allocate_coshape_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = AllocateCoshapeSpec<S::Span>> + 'a {
    // TODO test
    (
        (
            lower_bound_expr(cfg),
            space(0), ':', space(0),
        ).map(|(lower_bound, _, _, _)| lower_bound).optional(),
        upper_bound_expr(cfg),
    ).map(|(lower_bound, upper_bound)| AllocateCoshapeSpec {
        lower_bound,
        upper_bound,
    })
}


#[derive(Debug, Clone)]
pub struct StatVariable<Span>(pub IntVariable<Span>);

#[syntax_rule(
    F18V007r1 rule "stat-variable" #942 : "is scalar-int-variable",
)]
pub fn stat_variable<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = StatVariable<S::Span>> + 'a {
    // TODO test
    int_variable(cfg, false).map(StatVariable)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ProcPointerObject<Span> {
    ProcPointerName(ProcPointerName<Span>),
    ProcComponentRef(ProcComponentRef<Span>),
}

#[syntax_rule(
    F18V007r1 rule "proc-pointer-object" #1038 :
    "is proc-pointer-name"
    "or proc-component-ref",
)]
pub fn proc_pointer_object<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ProcPointerObject<S::Span>> + 'a {
    // TODO test
    alt!(
        proc_pointer_name(cfg).map(ProcPointerObject::ProcPointerName),
        proc_component_ref(cfg).map(ProcPointerObject::ProcComponentRef),
    )
}

#[derive(Debug, Clone)]
pub struct ProcPointerName<Span>(pub Name<Span>);

#[syntax_rule(
    F18V007r1 rule "proc-pointer-name" #858 : "is name",
)]
pub fn proc_pointer_name<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ProcPointerName<S::Span>> + 'a {
    // TODO test
    name(cfg, false).map(ProcPointerName)
}

#[derive(Debug, Clone)]
pub struct AssignmentStmt<Span> {
    pub variable: Variable<Span>,
    pub expr: Expr<Span>,
}

#[syntax_rule(
    F18V007r1 rule "assignment-stmt" #1032 : "is variable = expr",
)]
pub fn assignment_stmt_2<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = AssignmentStmt<S::Span>> + 'a {
    // TODO test
    (
        variable(cfg, true),
        (space(0), '=', space(0)),
        expr(cfg),
    ).map(|(variable, _, expr)| AssignmentStmt {
        variable,
        expr,
    })
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

#[syntax_rule(
    F18V007r1 rule "pointer-assignment-stmt" #1033 :
    "is data-pointer-object [ (bounds-spec-list) ] => data-target"
    "or data-pointer-object (bounds-remapping-list ) => data-target"
    "or proc-pointer-object => proc-target",
)]
pub fn pointer_assignment_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = PointerAssignmentStmt<S::Span>> + 'a {
    alt!(
        (
            data_pointer_object(cfg),
            (
                (space(0), '(', space(0)),
                list(bounds_spec(cfg), 0..),
                (space(0), ')', space(0)),
            ).map(|(_, bounds_spec_list, _)| bounds_spec_list).optional(),
            (space(0), "=>", space(0)),
            data_target(cfg),
        ).map(|(data_pointer_object, bounds_spec_list, _, data_target)| PointerAssignmentStmt::Form1 {
            data_pointer_object,
            bounds_spec_list,
            data_target,
        }),
        (
            data_pointer_object(cfg),
            (space(0), '(', space(0)),
            list(bounds_remapping(cfg), 0..),
            (space(0), ')', space(0)),
            (space(0), "=>", space(0)),
            data_target(cfg),
        ).map(|(data_pointer_object, _, bounds_remapping_list, _, _, data_target)| PointerAssignmentStmt::Form2 {
            data_pointer_object,
            bounds_remapping_list,
            data_target,
        }),
        (
            proc_pointer_object(cfg),
            (space(0), "=>", space(0)),
            proc_target(cfg),
        ).map(|(proc_pointer_object, _, proc_target)| PointerAssignmentStmt::Form3 {
            proc_pointer_object,
            proc_target,
        }),
    )
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum DataPointerObject<Span> {
    VariableName(VariableName<Span>),
    Component {
        scalar_variable: Variable<Span>,
        data_pointer_component_name: Name<Span>,
    },
}

#[syntax_rule(
    F18V007r1 rule "data-pointer-object" #1034 :
    "is variable-name"
    "or scalar-variable % data-pointer-component-name",
)]
pub fn data_pointer_object<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DataPointerObject<S::Span>> + 'a {
    alt!(
        variable_name(cfg).map(DataPointerObject::VariableName),
        (
            variable(cfg, true),
            space(0), '%', space(0),
            name(cfg, false),
        ).map(|(scalar_variable, _, _, _, data_pointer_component_name)| DataPointerObject::Component {
            scalar_variable,
            data_pointer_component_name,
        }),
    )
}

#[derive(Debug, Clone)]
pub struct BoundsSpec<Span> {
    pub lower_bound: LowerBoundExpr<Span>,
}

#[syntax_rule(
    F18V007r1 rule "bounds-spec" #1035 : "is lower-bound-expr :",
)]
pub fn bounds_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = BoundsSpec<S::Span>> + 'a {
    // TODO test
    (
        lower_bound_expr(cfg),
        (space(0), ':', space(0)),
    ).map(|(lower_bound, _)| BoundsSpec {
        lower_bound,
    })
}

#[derive(Debug, Clone)]
pub struct BoundsRemapping<Span> {
    pub lower_bound_expr: LowerBoundExpr<Span>,
    pub upper_bound_expr: UpperBoundExpr<Span>,
}

#[syntax_rule(
    F18V007r1 rule "bounds-remapping" #1036 : "is lower-bound-expr : upper-bound-expr",
)]
pub fn bounds_remapping<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = BoundsRemapping<S::Span>> + 'a {
    // TODO test
    (
        lower_bound_expr(cfg),
        (space(0), ':', space(0)),
        upper_bound_expr(cfg),
    ).map(|(lower_bound_expr, _, upper_bound_expr)| BoundsRemapping {
        lower_bound_expr,
        upper_bound_expr,
    })
}

#[derive(Debug, Clone)]
pub struct DataTarget<Span>(pub Expr<Span>);

#[syntax_rule(
    F18V007r1 rule "data-target" #1037 : "is expr",
)]
pub fn data_target<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DataTarget<S::Span>> + 'a {
    expr(cfg).map(DataTarget)
}

#[derive(Debug, Clone)]
pub struct WhereStmt<Span> {
    pub mask_expr: MaskExpr<Span>,
    pub where_assignment_stmt: WhereAssignmentStmt<Span>,
}

#[syntax_rule(
    F18V007r1 rule "where-stmt" #1041 : "is WHERE ( mask-expr ) where-assignment-stmt",
)]
pub fn where_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = WhereStmt<S::Span>> + 'a {
    (
        (kw("where", cfg), space(0), '(', space(0)),
        mask_expr(cfg),
        (space(0), ')', space(0)),
        where_assignment_stmt(cfg),
    ).map(|(_, mask_expr, _, where_assignment_stmt)| WhereStmt {
        mask_expr,
        where_assignment_stmt,
    })
}

#[derive(Debug, Clone)]
pub struct WhereConstructStmt<Span> {
    pub where_construct_name: Option<Name<Span>>,
    pub mask_expr: MaskExpr<Span>,
}

#[syntax_rule(
    F18V007r1 rule "where-construct-stmt" #1043 : "is [where-construct-name:] WHERE ( mask-expr )",
)]
pub fn where_construct_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = WhereConstructStmt<S::Span>> + 'a {
    (
        (
            name(cfg, false),
            (space(0), ':', space(0)),
        ).map(|(name, (_, _, _))| name).optional(),
        (kw("where", cfg), space(0), '(', space(0)),
        mask_expr(cfg),
        (space(0), ')', space(0)),
    ).map(|(where_construct_name, _, mask_expr, _)| WhereConstructStmt {
        where_construct_name,
        mask_expr,
    })
}

#[derive(Debug, Clone)]
pub struct WhereAssignmentStmt<Span> {
    pub assignment_stmt: AssignmentStmt<Span>,
}

#[syntax_rule(
    F18V007r1 rule "where-assignment-stmt" #1045 : "is assignment-stmt",
)]
pub fn where_assignment_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = WhereAssignmentStmt<S::Span>> + 'a {
    |_| todo!("TODO: \"where_assignment_stmt\" parser not implemented yet")
}

#[derive(Debug, Clone)]
pub struct MaskedElsewhereStmt<Span> {
    pub mask_expr: MaskExpr<Span>,
    pub where_construct_name: Option<Name<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "masked-elsewhere-stmt" #1047 : "is ELSEWHERE (mask-expr) [where-construct-name]",
)]
pub fn masked_elsewhere_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = MaskedElsewhereStmt<S::Span>> + 'a {
    (
        (kw("elsewhere", cfg), space(0), '(', space(0)),
        mask_expr(cfg),
        (space(0), ')', space(0)),
        name(cfg, false).optional(),
    ).map(|(_, mask_expr, _, where_construct_name)| MaskedElsewhereStmt {
        mask_expr,
        where_construct_name,
    })
}

#[derive(Debug, Clone)]
pub struct ElsewhereStmt<Span> {
    pub where_construct_name: Option<Name<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "elsewhere-stmt" #1048 : "is ELSEWHERE [where-construct-name]",
)]
pub fn elsewhere_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ElsewhereStmt<S::Span>> + 'a {
    (
        kw("elsewhere", cfg),
        name(cfg, false).optional(),
    ).map(|(_, where_construct_name)| ElsewhereStmt {
        where_construct_name,
    })
}

#[derive(Debug, Clone)]
pub struct EndWhereStmt<Span> {
    pub where_construct_name: Option<Name<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "end-where-stmt" #1049 : "is END WHERE [where-construct-name]",
)]
pub fn end_where_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = EndWhereStmt<S::Span>> + 'a {
    (
        kw("end", cfg), space(0), kw("where", cfg),
        name(cfg, false).optional(),
    ).map(|(_, _, _, where_construct_name)| EndWhereStmt {
        where_construct_name,
    })
}

#[derive(Debug, Clone)]
pub struct ForallConstructStmt<Span> {
    pub forall_construct_name: Option<Name<Span>>,
    pub concurrent_header: ConcurrentHeader<Span>,
}

#[syntax_rule(
    F18V007r1 rule "forall-construct-stmt" #1051 : "is [forall-construct-name :] FORALL concurrent-header",
)]
pub fn forall_construct_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ForallConstructStmt<S::Span>> + 'a {
    (
        (name(cfg, false), (space(0), ':', space(0))).map(|(name, (_, _, _))| name).optional(),
        kw("forall", cfg),
        concurrent_header(cfg),
    ).map(|(forall_construct_name, _, concurrent_header)| ForallConstructStmt {
        forall_construct_name,
        concurrent_header,
    })
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum IoUnit<Span> {
    FileUnitNumber(FileUnitNumber<Span>),
    Star,
    InternalFileVariable(InternalFileVariable<Span>),
}

#[syntax_rule(
    F18V007r1 rule "io-unit" #1201 :
    "is file-unit-number"
    "or *"
    "or internal-file-variable",
)]
pub fn io_unit<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = IoUnit<S::Span>> + 'a {
    |_| todo!("TODO: \"io_unit\" parser not implemented yet")
}

#[derive(Debug, Clone)]
pub struct FileUnitNumber<Span>(pub IntExpr<Span>);

#[syntax_rule(
    F18V007r1 rule "file-unit-number" #1202 : "is scalar-int-expr",
)]
pub fn file_unit_number<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = FileUnitNumber<S::Span>> + 'a {
    // TODO test
    int_expr(cfg).map(FileUnitNumber)
}

#[derive(Debug, Clone)]
pub struct InternalFileVariable<Span>(pub CharVariable<Span>);

#[syntax_rule(
    F18V007r1 rule "internal-file-variable" #1203 : "is char-variable",
)]
pub fn internal_file_variable<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = InternalFileVariable<S::Span>> + 'a {
    // TODO test
    char_variable(cfg, true).map(InternalFileVariable) // TODO true???
}

#[derive(Debug, Clone)]
pub struct OpenStmt<Span> {
    pub connect_spec_list: Vec<ConnectSpec<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "open-stmt" #1204 : "is OPEN ( connect-spec-list )",
)]
pub fn open_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = OpenStmt<S::Span>> + 'a {
    // TODO test
    (
        (kw("open", cfg), space(0), '(', space(0)),
        list(connect_spec(cfg), 1..),
        (space(0), ')', space(0)),
    ).map(|(_, connect_spec_list, _)| OpenStmt {
        connect_spec_list,
    })
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

#[syntax_rule(
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
pub fn connect_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ConnectSpec<S::Span>> + 'a {
    // TODO test
    alt!(
        (
            (kw("unit", cfg), space(0), '=', space(0)).optional(),
            file_unit_number(cfg),
        ).map(|(_, file_unit_number)| ConnectSpec::FileUnitNumber(file_unit_number)),
        (kw("access", cfg), space(0), '=', space(0), default_char_expr(cfg)).map(|(_, _, _, _, scalar_default_char_expr)| ConnectSpec::Access(scalar_default_char_expr)),
        (kw("action", cfg), space(0), '=', space(0), default_char_expr(cfg)).map(|(_, _, _, _, scalar_default_char_expr)| ConnectSpec::Action(scalar_default_char_expr)),
        (kw("asynchronous", cfg), space(0), '=', space(0), default_char_expr(cfg)).map(|(_, _, _, _, scalar_default_char_expr)| ConnectSpec::Asynchronous(scalar_default_char_expr)),
        (kw("blank", cfg), space(0), '=', space(0), default_char_expr(cfg)).map(|(_, _, _, _, scalar_default_char_expr)| ConnectSpec::Blank(scalar_default_char_expr)),
        (kw("decimal", cfg), space(0), '=', space(0), default_char_expr(cfg)).map(|(_, _, _, _, scalar_default_char_expr)| ConnectSpec::Decimal(scalar_default_char_expr)),
        (kw("delim", cfg), space(0), '=', space(0), default_char_expr(cfg)).map(|(_, _, _, _, scalar_default_char_expr)| ConnectSpec::Delim(scalar_default_char_expr)),
        (kw("encoding", cfg), space(0), '=', space(0), default_char_expr(cfg)).map(|(_, _, _, _, scalar_default_char_expr)| ConnectSpec::Encoding(scalar_default_char_expr)),
        (kw("err", cfg), space(0), '=', space(0), label(cfg)).map(|(_, _, _, _, label)| ConnectSpec::Err(label)),
        (kw("file", cfg), space(0), '=', space(0), file_name_expr(cfg)).map(|(_, _, _, _, file_name_expr)| ConnectSpec::File(file_name_expr)),
        (kw("form", cfg), space(0), '=', space(0), default_char_expr(cfg)).map(|(_, _, _, _, scalar_default_char_expr)| ConnectSpec::Form(scalar_default_char_expr)),
        (kw("iomsg", cfg), space(0), '=', space(0), iomsg_variable(cfg)).map(|(_, _, _, _, iomsg_variable)| ConnectSpec::Iomsg(iomsg_variable)),
        (kw("iostat", cfg), space(0), '=', space(0), stat_variable(cfg)).map(|(_, _, _, _, stat_variable)| ConnectSpec::Iostat(stat_variable)),
        (kw("newunit", cfg), space(0), '=', space(0), int_variable(cfg, false)).map(|(_, _, _, _, scalar_int_variable)| ConnectSpec::Newunit(scalar_int_variable)), // TODO false???
        (kw("pad", cfg), space(0), '=', space(0), default_char_expr(cfg)).map(|(_, _, _, _, scalar_default_char_expr)| ConnectSpec::Pad(scalar_default_char_expr)),
        (kw("position", cfg), space(0), '=', space(0), default_char_expr(cfg)).map(|(_, _, _, _, scalar_default_char_expr)| ConnectSpec::Position(scalar_default_char_expr)),
        (kw("recl", cfg), space(0), '=', space(0), int_expr(cfg)).map(|(_, _, _, _, scalar_int_expr)| ConnectSpec::Recl(scalar_int_expr)),
        (kw("round", cfg), space(0), '=', space(0), default_char_expr(cfg)).map(|(_, _, _, _, scalar_default_char_expr)| ConnectSpec::Round(scalar_default_char_expr)),
        (kw("sign", cfg), space(0), '=', space(0), default_char_expr(cfg)).map(|(_, _, _, _, scalar_default_char_expr)| ConnectSpec::Sign(scalar_default_char_expr)),
        (kw("status", cfg), space(0), '=', space(0), default_char_expr(cfg)).map(|(_, _, _, _, scalar_default_char_expr)| ConnectSpec::Status(scalar_default_char_expr)),
    )
}

#[derive(Debug, Clone)]
pub struct FileNameExpr<Span>(pub DefaultCharExpr<Span>);

#[syntax_rule(
    F18V007r1 rule "file-name-expr" #1206 : "is scalar-default-char-expr",
)]
pub fn file_name_expr<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = FileNameExpr<S::Span>> + 'a {
    // TODO test
    default_char_expr(cfg).map(FileNameExpr)
}

#[derive(Debug, Clone)]
pub struct IomsgVariable<Span>(pub DefaultCharVariable<Span>);

#[syntax_rule(
    F18V007r1 rule "iomsg-variable" #1207 : "is scalar-default-char-variable",
)]
pub fn iomsg_variable<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = IomsgVariable<S::Span>> + 'a {
    // TODO test
    default_char_variable(cfg, false).map(IomsgVariable)
}

#[derive(Debug, Clone)]
pub struct CloseStmt<Span> {
    pub close_spec_list: Vec<CloseSpec<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "close-stmt" #1208 : "is CLOSE ( close-spec-list )",
)]
pub fn close_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = CloseStmt<S::Span>> + 'a {
    // TODO test
    (
        (kw("close", cfg), space(0), '(', space(0)),
        list(close_spec(cfg), 1..),
        (space(0), ')', space(0)),
    ).map(|(_, close_spec_list, _)| CloseStmt {
        close_spec_list,
    })
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum CloseSpec<Span> {
    FileUnitNumber(FileUnitNumber<Span>),
    Iostat(StatVariable<Span>),
    Iomsg(IomsgVariable<Span>),
    Err(Label<Span>),
    Status(DefaultCharExpr<Span>),
}

#[syntax_rule(
    F18V007r1 rule "close-spec" #1209 :
    "is [ UNIT = ] file-unit-number"
    "or IOSTAT = stat-variable"
    "or IOMSG = iomsg-variable"
    "or ERR = label"
    "or STATUS = scalar-default-char-expr",
)]
pub fn close_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = CloseSpec<S::Span>> + 'a {
    // TODO test
    alt!(
        (
            (kw("unit", cfg), space(0), '=', space(0)).optional(),
            file_unit_number(cfg),
        ).map(|(_, file_unit_number)| CloseSpec::FileUnitNumber(file_unit_number)),
        (kw("iostat", cfg), space(0), '=', space(0), stat_variable(cfg)).map(|(_, _, _, _, stat_variable)| CloseSpec::Iostat(stat_variable)),
        (kw("iomsg", cfg), space(0), '=', space(0), iomsg_variable(cfg)).map(|(_, _, _, _, iomsg_variable)| CloseSpec::Iomsg(iomsg_variable)),
        (kw("err", cfg), space(0), '=', space(0), label(cfg)).map(|(_, _, _, _, label)| CloseSpec::Err(label)),
        (kw("status", cfg), space(0), '=', space(0), default_char_expr(cfg)).map(|(_, _, _, _, scalar_default_char_expr)| CloseSpec::Status(scalar_default_char_expr)),
    )
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ReadStmt<Span> {
    IoControlSpecList(Vec<IoControlSpec<Span>>),
    Format(Format<Span>, Option<Vec<InputItem<Span>>>),
}

#[syntax_rule(
    F18V007r1 rule "read-stmt" #1210 :
    "is READ ( io-control-spec-list ) [ input-item-list ]"
    "or READ format [ , input-item-list ]",
)]
pub fn read_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ReadStmt<S::Span>> + 'a {
    // TODO test
    alt!(
        (
            (kw("read", cfg), space(0), '(', space(0)),
            list(io_control_spec(cfg), 1..),
            (space(0), ')', space(0)),
            list(input_item(cfg), 0..).map(Some),
        ).map(|(_, io_control_spec_list, _, input_item_list)| ReadStmt::IoControlSpecList(io_control_spec_list)),
        (
            (kw("read", cfg), space(0)),
            format(cfg),
            (
                space(0), ',', space(0),
                list(input_item(cfg), 0..),
            ).map(|(_, _, _, input_item_list)| input_item_list).optional(),
        ).map(|(_, format, input_item_list)| ReadStmt::Format(format, input_item_list)),
    )
}

#[derive(Debug, Clone)]
pub struct WriteStmt<Span> {
    pub io_control_spec_list: Vec<IoControlSpec<Span>>,
    pub output_item_list: Option<Vec<OutputItem<Span>>>,
}

#[syntax_rule(
    F18V007r1 rule "write-stmt" #1211 : "is WRITE ( io-control-spec-list ) [ output-item-list ]",
)]
pub fn write_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = WriteStmt<S::Span>> + 'a {
    // TODO test
    (
        (kw("write", cfg), space(0), '(', space(0)),
        list(io_control_spec(cfg), 1..),
        (space(0), ')', space(0)),
        list(output_item(cfg), 1..).optional(),
    ).map(|(_, io_control_spec_list, _, output_item_list)| WriteStmt {
        io_control_spec_list,
        output_item_list,
    })
}

#[derive(Debug, Clone)]
pub struct PrintStmt<Span> {
    pub format: Format<Span>,
    pub output_item_list: Option<Vec<OutputItem<Span>>>,
}

#[syntax_rule(
    F18V007r1 rule "print-stmt" #1212 : "is PRINT format [ , output-item-list ]",
)]
pub fn print_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = PrintStmt<S::Span>> + 'a {
    // TODO test
    (
        (kw("print", cfg), space(0)),
        format(cfg),
        (
            space(0), ',', space(0),
            list(output_item(cfg), 1..),
        ).map(|(_, _, _, output_item_list)| output_item_list).optional(),
    ).map(|(_, format, output_item_list)| PrintStmt {
        format,
        output_item_list,
    })
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

#[syntax_rule(
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
pub fn io_control_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = IoControlSpec<S::Span>> + 'a {
    // TODO test
    alt!(
        (
            (kw("unit", cfg), space(0), '=', space(0)).optional(),
            io_unit(cfg),
        ).map(|(_, io_unit)| IoControlSpec::Unit(io_unit)),
        (
            (kw("fmt", cfg), space(0), '=', space(0)).optional(),
            format(cfg),
        ).map(|(_, format)| IoControlSpec::Fmt(format)),
        (
            (kw("nml", cfg), space(0), '=', space(0)).optional(),
            name(cfg, false),
        ).map(|(_, namelist_group_name)| IoControlSpec::Nml(namelist_group_name)),
        (kw("advance", cfg), space(0), '=', space(0), default_char_expr(cfg)).map(|(_, _, _, _, scalar_default_char_expr)| IoControlSpec::Advance(scalar_default_char_expr)),
        (kw("asynchronous", cfg), space(0), '=', space(0), default_char_constant_expr(cfg)).map(|(_, _, _, _, scalar_default_char_constant_expr)| IoControlSpec::Asynchronous(scalar_default_char_constant_expr)),
        (kw("blank", cfg), space(0), '=', space(0), default_char_expr(cfg)).map(|(_, _, _, _, scalar_default_char_expr)| IoControlSpec::Blank(scalar_default_char_expr)),
        (kw("decimal", cfg), space(0), '=', space(0), default_char_expr(cfg)).map(|(_, _, _, _, scalar_default_char_expr)| IoControlSpec::Decimal(scalar_default_char_expr)),
        (kw("delim", cfg), space(0), '=', space(0), default_char_expr(cfg)).map(|(_, _, _, _, scalar_default_char_expr)| IoControlSpec::Delim(scalar_default_char_expr)),
        (kw("end", cfg), space(0), '=', space(0), label(cfg)).map(|(_, _, _, _, label)| IoControlSpec::End(label)),
        (kw("eor", cfg), space(0), '=', space(0), label(cfg)).map(|(_, _, _, _, label)| IoControlSpec::Eor(label)),
        (kw("err", cfg), space(0), '=', space(0), label(cfg)).map(|(_, _, _, _, label)| IoControlSpec::Err(label)),
        (kw("id", cfg), space(0), '=', space(0), id_variable(cfg)).map(|(_, _, _, _, id_variable)| IoControlSpec::Id(id_variable)),
        (kw("iomsg", cfg), space(0), '=', space(0), iomsg_variable(cfg)).map(|(_, _, _, _, iomsg_variable)| IoControlSpec::Iomsg(iomsg_variable)),
        (kw("iostat", cfg), space(0), '=', space(0), stat_variable(cfg)).map(|(_, _, _, _, stat_variable)| IoControlSpec::Iostat(stat_variable)),
        (kw("pad", cfg), space(0), '=', space(0), default_char_expr(cfg)).map(|(_, _, _, _, scalar_default_char_expr)| IoControlSpec::Pad(scalar_default_char_expr)),
        (kw("pos", cfg), space(0), '=', space(0), int_expr(cfg)).map(|(_, _, _, _, scalar_int_expr)| IoControlSpec::Pos(scalar_int_expr)),
        (kw("rec", cfg), space(0), '=', space(0), int_expr(cfg)).map(|(_, _, _, _, scalar_int_expr)| IoControlSpec::Rec(scalar_int_expr)),
        (kw("round", cfg), space(0), '=', space(0), default_char_expr(cfg)).map(|(_, _, _, _, scalar_default_char_expr)| IoControlSpec::Round(scalar_default_char_expr)),
        (kw("sign", cfg), space(0), '=', space(0), default_char_expr(cfg)).map(|(_, _, _, _, scalar_default_char_expr)| IoControlSpec::Sign(scalar_default_char_expr)),
        (kw("size", cfg), space(0), '=', space(0), int_variable(cfg, true)).map(|(_, _, _, _, scalar_int_variable)| IoControlSpec::Size(scalar_int_variable)), // TODO true???
    )
}

#[derive(Debug, Clone)]
pub struct IdVariable<Span>(pub IntVariable<Span>);

#[syntax_rule(
    F18V007r1 rule "id-variable" #1214 : "is scalar-int-variable",
)]
pub fn id_variable<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = IdVariable<S::Span>> + 'a {
    // TODO test
    int_variable(cfg, false).map(IdVariable)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum Format<Span> {
    DefaultCharExpr(DefaultCharExpr<Span>),
    Label(Label<Span>),
    Star,
}

#[syntax_rule(
    F18V007r1 rule "format" #1215 :
    "is default-char-expr"
    "or label"
    "or *",
)]
pub fn format<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = Format<S::Span>> + 'a {
    // TODO test
    alt!(
        default_char_expr(cfg).map(Format::DefaultCharExpr),
        label(cfg).map(Format::Label),
        (SpecialCharacter::Asterisk).map(|_| Format::Star),
    )
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum InputItem<Span> {
    Variable(Variable<Span>),
    IoImpliedDo(IoImpliedDo<Span>),
}

#[syntax_rule(
    F18V007r1 rule "input-item" #1216 :
    "is variable"
    "or io-implied-do",
)]
pub fn input_item<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = InputItem<S::Span>> + 'a {
    // TODO test
    alt!(
        variable(cfg, true).map(InputItem::Variable),
        io_implied_do(cfg).map(InputItem::IoImpliedDo),
    )
}

#[derive(Debug, Clone)]
pub enum OutputItem<Span> {
    Expr(Expr<Span>),
    IoImpliedDo(IoImpliedDo<Span>),
}

#[syntax_rule(
    F18V007r1 rule "output-item" #1217 :
    "is expr"
    "or io-implied-do",
)]
pub fn output_item<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = OutputItem<S::Span>> + 'a {
    // TODO test
    alt!(
        expr(cfg).map(OutputItem::Expr),
        io_implied_do(cfg).map(OutputItem::IoImpliedDo),
    )
}

#[derive(Debug, Clone)]
pub struct IoImpliedDo<Span> {
    pub io_implied_do_object_list: Vec<IoImpliedDoObject<Span>>,
    pub io_implied_do_control: IoImpliedDoControl<Span>,
}

#[syntax_rule(
    F18V007r1 rule "io-implied-do" #1218 : "is ( io-implied-do-object-list , io-implied-do-control )",
)]
pub fn io_implied_do<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = IoImpliedDo<S::Span>> + 'a {
    // TODO test
    (
        (SpecialCharacter::LeftParenthesis, space(0)),
        list(io_implied_do_object(cfg), 1..),
        (space(0), ',', space(0)),
        io_implied_do_control(cfg),
        (space(0), SpecialCharacter::RightParenthesis, space(0)),
    ).map(|(_, io_implied_do_object_list, _, io_implied_do_control, _)| IoImpliedDo {
        io_implied_do_object_list,
        io_implied_do_control,
    })
}

#[derive(Debug, Clone)]
pub enum IoImpliedDoObject<Span> {
    InputItem(InputItem<Span>),
    OutputItem(OutputItem<Span>),
}

#[syntax_rule(
    F18V007r1 rule "io-implied-do-object" #1219 :
    "is input-item"
    "or output-item",
)]
pub fn io_implied_do_object<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = IoImpliedDoObject<S::Span>> + 'a {
    // TODO test
    alt!(
        input_item(cfg).map(IoImpliedDoObject::InputItem),
        output_item(cfg).map(IoImpliedDoObject::OutputItem),
    )
}

#[derive(Debug, Clone)]
pub struct IoImpliedDoControl<Span> {
    pub do_variable: Variable<Span>,
    pub scalar_int_expr_list: Vec<IntExpr<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "io-implied-do-control" #1220 :
    "is do-variable = scalar-int-expr ,"
    "    scalar-int-expr [ , scalar-int-expr ]",
)]
pub fn io_implied_do_control<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = IoImpliedDoControl<S::Span>> + 'a {
    // TODO test
    (
        variable(cfg, true),
        (space(0), '=', space(0)),
        list(int_expr(cfg), 2..),
    ).map(|(do_variable, _, scalar_int_expr_list)| IoImpliedDoControl {
        do_variable,
        scalar_int_expr_list,
    })
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum DtvTypeSpec<Span> {
    Type(DerivedTypeSpec<Span>),
    Class(DerivedTypeSpec<Span>),
}

#[syntax_rule(
    F18V007r1 rule "dtv-type-spec" #1221 :
    "is TYPE( derived-type-spec )"
    "or CLASS( derived-type-spec )",
)]
pub fn dtv_type_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DtvTypeSpec<S::Span>> + 'a {
    // TODO test
    alt!(
        (kw("type", cfg), space(0), '(', space(0), derived_type_spec(cfg), (space(0), ')', space(0))).map(|(_, _, _, _, derived_type_spec, _)| DtvTypeSpec::Type(derived_type_spec)),
        (kw("class", cfg), space(0), '(', space(0), derived_type_spec(cfg), (space(0), ')', space(0))).map(|(_, _, _, _, derived_type_spec, _)| DtvTypeSpec::Class(derived_type_spec)),
    )
}

#[derive(Debug, Clone)]
pub struct WaitStmt<Span> {
    pub wait_spec_list: Vec<WaitSpec<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "wait-stmt" #1222 : "is WAIT (wait-spec-list)",
)]
pub fn wait_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = WaitStmt<S::Span>> + 'a {
    // TODO test
    (
        (kw("wait", cfg), space(0), '(', space(0)),
        list(wait_spec(cfg), 1..),
        (space(0), ')', space(0)),
    ).map(|(_, wait_spec_list, _)| WaitStmt {
        wait_spec_list,
    })
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

#[syntax_rule(
    F18V007r1 rule "wait-spec" #1223 :
    "is [ UNIT = ] file-unit-number"
    "or END = label"
    "or EOR = label"
    "or ERR = label"
    "or ID = scalar-int-expr"
    "or IOMSG = iomsg-variable"
    "or IOSTAT = stat-variable",
)]
pub fn wait_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = WaitSpec<S::Span>> + 'a {
    // TODO test
    alt!(
        (
            (kw("unit", cfg), space(0), '=', space(0)).optional(),
            file_unit_number(cfg),
        ).map(|(_, file_unit_number)| WaitSpec::FileUnitNumber(file_unit_number)),
        (kw("end", cfg), space(0), '=', space(0), label(cfg)).map(|(_, _, _, _, label)| WaitSpec::End(label)),
        (kw("eor", cfg), space(0), '=', space(0), label(cfg)).map(|(_, _, _, _, label)| WaitSpec::Eor(label)),
        (kw("err", cfg), space(0), '=', space(0), label(cfg)).map(|(_, _, _, _, label)| WaitSpec::Err(label)),
        (kw("id", cfg), space(0), '=', space(0), int_expr(cfg)).map(|(_, _, _, _, scalar_int_expr)| WaitSpec::Id(scalar_int_expr)),
        (kw("iomsg", cfg), space(0), '=', space(0), iomsg_variable(cfg)).map(|(_, _, _, _, iomsg_variable)| WaitSpec::Iomsg(iomsg_variable)),
        (kw("iostat", cfg), space(0), '=', space(0), stat_variable(cfg)).map(|(_, _, _, _, stat_variable)| WaitSpec::Iostat(stat_variable)),
    )
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum EndfileStmt<Span> {
    FileUnitNumber(FileUnitNumber<Span>),
    PositionSpecList(Vec<PositionSpec<Span>>),
}

#[syntax_rule(
    F18V007r1 rule "endfile-stmt" #1225 :
    "is ENDFILE file-unit-number"
    "or ENDFILE ( position-spec-list )",
)]
pub fn endfile_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = EndfileStmt<S::Span>> + 'a {
    // TODO test
    alt!(
        (kw("endfile", cfg), space(0), file_unit_number(cfg)).map(|(_, _, file_unit_number)| EndfileStmt::FileUnitNumber(file_unit_number)),
        (kw("endfile", cfg), space(0), '(', space(0), list(position_spec(cfg), 1..), (space(0), ')', space(0))).map(|(_, _, _, _, position_spec_list, _)| EndfileStmt::PositionSpecList(position_spec_list)),
    )
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum RewindStmt<Span> {
    FileUnitNumber(FileUnitNumber<Span>),
    PositionSpecList(Vec<PositionSpec<Span>>),
}

#[syntax_rule(
    F18V007r1 rule "rewind-stmt" #1226 :
    "is REWIND file-unit-number"
    "or REWIND ( position-spec-list )",
)]
pub fn rewind_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = RewindStmt<S::Span>> + 'a {
    // TODO test
    alt!(
        (kw("rewind", cfg), space(0), file_unit_number(cfg)).map(|(_, _, file_unit_number)| RewindStmt::FileUnitNumber(file_unit_number)),
        (kw("rewind", cfg), space(0), '(', space(0), list(position_spec(cfg), 1..), (space(0), ')', space(0))).map(|(_, _, _, _, position_spec_list, _)| RewindStmt::PositionSpecList(position_spec_list)),
    )
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum PositionSpec<Span> {
    FileUnitNumber(FileUnitNumber<Span>),
    Iomsg(IomsgVariable<Span>),
    Iostat(StatVariable<Span>),
    Err(Label<Span>),
}

#[syntax_rule(
    F18V007r1 rule "position-spec" #1227 :
    "is [ UNIT = ] file-unit-number"
    "or IOMSG = iomsg-variable"
    "or IOSTAT = stat-variable"
    "or ERR = label",
)]
pub fn position_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = PositionSpec<S::Span>> + 'a {
    // TODO test
    alt!(
        (
            (kw("unit", cfg), space(0), '=', space(0)).optional(),
            file_unit_number(cfg),
        ).map(|(_, file_unit_number)| PositionSpec::FileUnitNumber(file_unit_number)),
        (kw("iomsg", cfg), space(0), '=', space(0), iomsg_variable(cfg)).map(|(_, _, _, _, iomsg_variable)| PositionSpec::Iomsg(iomsg_variable)),
        (kw("iostat", cfg), space(0), '=', space(0), stat_variable(cfg)).map(|(_, _, _, _, stat_variable)| PositionSpec::Iostat(stat_variable)),
        (kw("err", cfg), space(0), '=', space(0), label(cfg)).map(|(_, _, _, _, label)| PositionSpec::Err(label)),
    )
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum FlushStmt<Span> {
    FileUnitNumber(FileUnitNumber<Span>),
    FlushSpecList(Vec<FlushSpec<Span>>),
}

#[syntax_rule(
    F18V007r1 rule "flush-stmt" #1228 :
    "is FLUSH file-unit-number"
    "or FLUSH ( flush-spec-list )",
)]
pub fn flush_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = FlushStmt<S::Span>> + 'a {
    // TODO test
    alt!(
        (kw("flush", cfg), space(0), file_unit_number(cfg)).map(|(_, _, file_unit_number)| FlushStmt::FileUnitNumber(file_unit_number)),
        (kw("flush", cfg), space(0), '(', space(0), list(flush_spec(cfg), 1..), (space(0), ')', space(0))).map(|(_, _, _, _, flush_spec_list, _)| FlushStmt::FlushSpecList(flush_spec_list)),
    )
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum FlushSpec<Span> {
    FileUnitNumber(FileUnitNumber<Span>),
    Iostat(StatVariable<Span>),
    Iomsg(IomsgVariable<Span>),
    Err(Label<Span>),
}

#[syntax_rule(
    F18V007r1 rule "flush-spec" #1229 :
    "is [UNIT =] file-unit-number"
    "or IOSTAT = stat-variable"
    "or IOMSG = iomsg-variable"
    "or ERR = label",
)]
pub fn flush_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = FlushSpec<S::Span>> + 'a {
    // TODO test
    alt!(
        (
            (kw("unit", cfg), space(0), '=', space(0)).optional(),
            file_unit_number(cfg),
        ).map(|(_, file_unit_number)| FlushSpec::FileUnitNumber(file_unit_number)),
        (kw("iostat", cfg), space(0), '=', space(0), stat_variable(cfg)).map(|(_, _, _, _, stat_variable)| FlushSpec::Iostat(stat_variable)),
        (kw("iomsg", cfg), space(0), '=', space(0), iomsg_variable(cfg)).map(|(_, _, _, _, iomsg_variable)| FlushSpec::Iomsg(iomsg_variable)),
        (kw("err", cfg), space(0), '=', space(0), label(cfg)).map(|(_, _, _, _, label)| FlushSpec::Err(label)),
    )
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum InquireStmt<Span> {
    InquireSpecList(Vec<InquireSpec<Span>>),
    IoLength(IntVariable<Span>),
    OutputItemList(Vec<OutputItem<Span>>),
}

#[syntax_rule(
    F18V007r1 rule "inquire-stmt" #1230 :
    "is INQUIRE ( inquire-spec-list )"
    "or INQUIRE ( IOLENGTH = scalar-int-variable )"
    "output-item-list",
)]
pub fn inquire_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = InquireStmt<S::Span>> + 'a {
    // TODO test
    alt!(
        (
            (kw("inquire", cfg), space(0), '(', space(0)),
            list(inquire_spec(cfg), 1..),
            (space(0), ')', space(0)),
        ).map(|(_, inquire_spec_list, _)| InquireStmt::InquireSpecList(inquire_spec_list)),
        (
            (kw("inquire", cfg), space(0), '(', space(0), kw("iolength", cfg), space(0), '=', space(0)),
            int_variable(cfg, false), // TODO false???
            (space(0), ')', space(0)),
        ).map(|(_, scalar_int_variable, _)| InquireStmt::IoLength(scalar_int_variable)),
        list(output_item(cfg), 1..).map(InquireStmt::OutputItemList),
    )
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

#[syntax_rule(
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
pub fn inquire_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = InquireSpec<S::Span>> + 'a {
    // TODO test
    alt!(
        (
            (kw("unit", cfg), space(0), '=', space(0)).optional(),
            file_unit_number(cfg),
        ).map(|(_, file_unit_number)| InquireSpec::FileUnitNumber(file_unit_number)),
        (kw("file", cfg), space(0), '=', space(0), file_name_expr(cfg)).map(|(_, _, _, _, file_name_expr)| InquireSpec::File(file_name_expr)),
        (kw("access", cfg), space(0), '=', space(0), default_char_variable(cfg, false)).map(|(_, _, _, _, scalar_default_char_variable)| InquireSpec::Access(scalar_default_char_variable)),
        (kw("action", cfg), space(0), '=', space(0), default_char_variable(cfg, false)).map(|(_, _, _, _, scalar_default_char_variable)| InquireSpec::Action(scalar_default_char_variable)),
        (kw("asynchronous", cfg), space(0), '=', space(0), default_char_variable(cfg, false)).map(|(_, _, _, _, scalar_default_char_variable)| InquireSpec::Asynchronous(scalar_default_char_variable)),
        (kw("blank", cfg), space(0), '=', space(0), default_char_variable(cfg, false)).map(|(_, _, _, _, scalar_default_char_variable)| InquireSpec::Blank(scalar_default_char_variable)),
        (kw("decimal", cfg), space(0), '=', space(0), default_char_variable(cfg, false)).map(|(_, _, _, _, scalar_default_char_variable)| InquireSpec::Decimal(scalar_default_char_variable)),
        (kw("delim", cfg), space(0), '=', space(0), default_char_variable(cfg, false)).map(|(_, _, _, _, scalar_default_char_variable)| InquireSpec::Delim(scalar_default_char_variable)),
        (kw("direct", cfg), space(0), '=', space(0), default_char_variable(cfg, false)).map(|(_, _, _, _, scalar_default_char_variable)| InquireSpec::Direct(scalar_default_char_variable)),
        (kw("encoding", cfg), space(0), '=', space(0), default_char_variable(cfg, false)).map(|(_, _, _, _, scalar_default_char_variable)| InquireSpec::Encoding(scalar_default_char_variable)),
        (kw("err", cfg), space(0), '=', space(0), label(cfg)).map(|(_, _, _, _, label)| InquireSpec::Err(label)),
        (kw("exist", cfg), space(0), '=', space(0), logical_variable(cfg, false)).map(|(_, _, _, _, scalar_logical_variable)| InquireSpec::Exist(scalar_logical_variable)), // TODO false???
        (kw("form", cfg), space(0), '=', space(0), default_char_variable(cfg, false)).map(|(_, _, _, _, scalar_default_char_variable)| InquireSpec::Form(scalar_default_char_variable)),
        (kw("formatted", cfg), space(0), '=', space(0), default_char_variable(cfg, false)).map(|(_, _, _, _, scalar_default_char_variable)| InquireSpec::Formatted(scalar_default_char_variable)),
        (kw("id", cfg), space(0), '=', space(0), int_expr(cfg)).map(|(_, _, _, _, scalar_int_expr)| InquireSpec::Id(scalar_int_expr)),
        (kw("iomsg", cfg), space(0), '=', space(0), iomsg_variable(cfg)).map(|(_, _, _, _, iomsg_variable)| InquireSpec::Iomsg(iomsg_variable)),
        (kw("iostat", cfg), space(0), '=', space(0), stat_variable(cfg)).map(|(_, _, _, _, stat_variable)| InquireSpec::Iostat(stat_variable)),
        (kw("name", cfg), space(0), '=', space(0), default_char_variable(cfg, false)).map(|(_, _, _, _, scalar_default_char_variable)| InquireSpec::Name(scalar_default_char_variable)),
        (kw("named", cfg), space(0), '=', space(0), logical_variable(cfg, false)).map(|(_, _, _, _, scalar_logical_variable)| InquireSpec::Named(scalar_logical_variable)),
        (kw("nextrec", cfg), space(0), '=', space(0), int_variable(cfg, false)).map(|(_, _, _, _, scalar_int_variable)| InquireSpec::Nextrec(scalar_int_variable)),
        (kw("number", cfg), space(0), '=', space(0), int_variable(cfg, false)).map(|(_, _, _, _, scalar_int_variable)| InquireSpec::Number(scalar_int_variable)),
        (kw("opened", cfg), space(0), '=', space(0), logical_variable(cfg, false)).map(|(_, _, _, _, scalar_logical_variable)| InquireSpec::Opened(scalar_logical_variable)),
        (kw("pad", cfg), space(0), '=', space(0), default_char_variable(cfg, false)).map(|(_, _, _, _, scalar_default_char_variable)| InquireSpec::Pad(scalar_default_char_variable)),
        (kw("pending", cfg), space(0), '=', space(0), logical_variable(cfg, false)).map(|(_, _, _, _, scalar_logical_variable)| InquireSpec::Pending(scalar_logical_variable)),
        (kw("pos", cfg), space(0), '=', space(0), int_variable(cfg, false)).map(|(_, _, _, _, scalar_int_variable)| InquireSpec::Pos(scalar_int_variable)),
        (kw("position", cfg), space(0), '=', space(0), default_char_variable(cfg, false)).map(|(_, _, _, _, scalar_default_char_variable)| InquireSpec::Position(scalar_default_char_variable)),
        (kw("read", cfg), space(0), '=', space(0), default_char_variable(cfg, false)).map(|(_, _, _, _, scalar_default_char_variable)| InquireSpec::Read(scalar_default_char_variable)),
        (kw("readwrite", cfg), space(0), '=', space(0), default_char_variable(cfg, false)).map(|(_, _, _, _, scalar_default_char_variable)| InquireSpec::Readwrite(scalar_default_char_variable)),
        (kw("recl", cfg), space(0), '=', space(0), int_variable(cfg, false)).map(|(_, _, _, _, scalar_int_variable)| InquireSpec::Recl(scalar_int_variable)),
        (kw("round", cfg), space(0), '=', space(0), default_char_variable(cfg, false)).map(|(_, _, _, _, scalar_default_char_variable)| InquireSpec::Round(scalar_default_char_variable)),
        (kw("sequential", cfg), space(0), '=', space(0), default_char_variable(cfg, false)).map(|(_, _, _, _, scalar_default_char_variable)| InquireSpec::Sequential(scalar_default_char_variable)),
        (kw("sign", cfg), space(0), '=', space(0), default_char_variable(cfg, false)).map(|(_, _, _, _, scalar_default_char_variable)| InquireSpec::Sign(scalar_default_char_variable)),
        (kw("size", cfg), space(0), '=', space(0), int_variable(cfg, false)).map(|(_, _, _, _, scalar_int_variable)| InquireSpec::Size(scalar_int_variable)),
        (kw("stream", cfg), space(0), '=', space(0), default_char_variable(cfg, false)).map(|(_, _, _, _, scalar_default_char_variable)| InquireSpec::Stream(scalar_default_char_variable)),
        (kw("unformatted", cfg), space(0), '=', space(0), default_char_variable(cfg, false)).map(|(_, _, _, _, scalar_default_char_variable)| InquireSpec::Unformatted(scalar_default_char_variable)),
        (kw("write", cfg), space(0), '=', space(0), default_char_variable(cfg, false)).map(|(_, _, _, _, scalar_default_char_variable)| InquireSpec::Write(scalar_default_char_variable)),
    )
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum BackspaceStmt<Span> {
    FileUnitNumber(FileUnitNumber<Span>),
    PositionSpecList(Vec<PositionSpec<Span>>),
}

#[syntax_rule(
    F18V007r1 rule "backspace-stmt" #1224 :
    "is BACKSPACE file-unit-number"
    "or BACKSPACE ( position-spec-list )",
)]
pub fn backspace_stmt_2<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = BackspaceStmt<S::Span>> + 'a {
    // TODO test
    alt!(
        (kw("backspace", cfg), space(0), file_unit_number(cfg)).map(|(_, _, file_unit_number)| BackspaceStmt::FileUnitNumber(file_unit_number)),
        (kw("backspace", cfg), space(0), '(', space(0), list(position_spec(cfg), 1..), (space(0), ')', space(0))).map(|(_, _, _, _, position_spec_list, _)| BackspaceStmt::PositionSpecList(position_spec_list)),
    )
}

#[derive(Debug, Clone)]
pub struct ProcComponentRef<Span> {
    pub scalar_variable: Variable<Span>,
    pub procedure_component_name: Name<Span>,
}

#[syntax_rule(
    F18V007r1 rule "proc-component-ref" #1039 : "is scalar-variable % procedure-component-name",
)]
pub fn proc_component_ref<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ProcComponentRef<S::Span>> + 'a {
    // TODO test
    (
        variable(cfg, true),
        (space(0), '%', space(0)),
        name(cfg, false),
    ).map(|(scalar_variable, _, procedure_component_name)| ProcComponentRef {
        scalar_variable,
        procedure_component_name,
    })
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ProcTarget<Span> {
    Expr(Expr<Span>),
    ProcedureName(Name<Span>),
    ProcComponentRef(ProcComponentRef<Span>),
}

#[syntax_rule(
    F18V007r1 rule "proc-target" #1040 :
    "is expr"
    "or procedure-name"
    "or proc-component-ref",
)]
pub fn proc_target<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ProcTarget<S::Span>> + 'a {
    // TODO test
    alt!(
        expr(cfg).map(ProcTarget::Expr),
        name(cfg, false).map(ProcTarget::ProcedureName),
        proc_component_ref(cfg).map(ProcTarget::ProcComponentRef),
    )
}

#[derive(Debug, Clone)]
pub struct MaskExpr<Span>(pub Expr<Span>);

#[syntax_rule(
    F18V007r1 rule "mask-expr" #1046 : "is logical-expr",
)]
pub fn mask_expr<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = MaskExpr<S::Span>> + 'a {
    // TODO test
    logical_expr(cfg).map(MaskExpr)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn tmp() {
        for cfg in test_configs() {
            let p = expr(&cfg);
            assert!(p.parses("1"));
            assert!(p.parses("2.1 + 3.4 + 4.9"));
            assert!(p.parses("2.1 * 3.4 * 4.9"));
            assert!(p.parses("2.1 / 3.4 / 4.9"));
            assert!(p.parses("2 ** 3 ** 4"));
            assert!(p.parses("'AB' // 'CD' // 'EF'"));

            let p = p.parse("1 + 2**e * 3");
            println!("{:#?}", p);
        }
    }
}
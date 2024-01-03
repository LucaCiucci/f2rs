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
            match right_part.parse(source) {
                Ok(((op, right), new_source)) => {
                    source = new_source;
                    expr = Expr {
                        left: Some((Box::new(expr), op)),
                        right,
                    };
                },
                Err(s) => {
                    source = s;
                    break
                },
            }
        }

        Ok((expr, source))
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
            match right_part.parse(source) {
                Ok(((op, right), new_source)) => {
                    source = new_source;
                    expr = AddOperand {
                        left: Some((Box::new(expr), op)),
                        right,
                    };
                },
                Err(s) => {
                    source = s;
                    break
                },
            }
        }

        Ok((expr, source))
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

        let (lop, source) = match add_op.parse(source) {
            Ok((op, source)) => (Some(op), source),
            Err(source) => (None, source),
        };
        let (right, mut source) = add_operand.parse(source)?;
        let mut expr = Level2Expr {
            left: lop.map(|op| (None, op)),
            right,
        };
        loop {
            match right_part.parse(source) {
                Ok(((op, right), new_source)) => {
                    source = new_source;
                    expr = Level2Expr {
                        left: Some((Some(Box::new(expr)), op)),
                        right,
                    };
                },
                Err(s) => {
                    source = s;
                    break
                },
            }
        }

        Ok((expr, source))
    
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
            match right_part.parse(source) {
                Ok(((op, right), new_source)) => {
                    source = new_source;
                    expr = Level3Expr {
                        left: Some((Box::new(expr), op)),
                        right,
                    };
                },
                Err(s) => {
                    source = s;
                    break
                },
            }
        }

        Ok((expr, source))
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
            match right_part.parse(source) {
                Ok(((op, right), new_source)) => {
                    source = new_source;
                    expr = Level4Expr {
                        left: Some((Box::new(expr), op)),
                        right,
                    };
                },
                Err(s) => {
                    source = s;
                    break
                },
            }
        }

        Ok((expr, source))
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
            match right_part.parse(source) {
                Ok(((op, right), new_source)) => {
                    source = new_source;
                    expr = OrOperand {
                        left: Some((Box::new(expr), op)),
                        right,
                    };
                },
                Err(s) => {
                    source = s;
                    break
                },
            }
        }

        Ok((expr, source))
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
            match right_part.parse(source) {
                Ok(((op, right), new_source)) => {
                    source = new_source;
                    expr = EquivOperand {
                        left: Some((Box::new(expr), op)),
                        right,
                    };
                },
                Err(s) => {
                    source = s;
                    break
                },
            }
        }

        Ok((expr, source))
    
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
            match right_part.parse(source) {
                Ok(((op, right), new_source)) => {
                    source = new_source;
                    expr = Level5Expr {
                        left: Some((Box::new(expr), op)),
                        right,
                    };
                },
                Err(s) => {
                    source = s;
                    break
                },
            }
        }

        Ok((expr, source))
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
    pub comment: Option<LineComment<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "allocate-stmt" #927 :
    "is ALLOCATE ( [ type-spec :: ] allocation-list [ , alloc-opt-list ] )",
)]
pub fn allocate_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = AllocateStmt<S::Span>> + 'a {
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
        statement_termination(),
    ).map(|(_, _, type_spec, allocation_list, alloc_opt_list, _, comment)| AllocateStmt {
        type_spec,
        allocation_list,
        alloc_opt_list,
        comment,
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
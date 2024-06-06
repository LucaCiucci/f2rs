use crate::tokens::rules::IntLiteralConstant;

use super::*;

#[derive(Debug, Clone, EnumAsInner)]
pub enum TypeParamValue<Span> {
    Expr(IntExpr<Span>),
    Asterisk(StringMatch<Span>),
    Colon(Colon<Span>),
}

// TODO test
#[syntax_rule(
    F18V007r1 rule "type-param-value" #701 :
    "is scalar-int-expr"
    "or *"
    "or :",
)]
pub fn type_param_value<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = TypeParamValue<MultilineSpan>> + 'a {
    alt!(
        int_expr(cfg).map(TypeParamValue::Expr),
        asterisk().map(TypeParamValue::Asterisk),
        colon().map(TypeParamValue::Colon),
    )
}

#[derive(Debug, Clone)]
pub struct KindSelector<Span> {
    pub value: IntConstantExpr<Span>,
}

// TODO test
#[syntax_rule(
    F18V007r1 rule "kind-selector" #706 : "is ( [ KIND = ] scalar-int-constant-expr )",
)]
pub fn kind_selector<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = KindSelector<MultilineSpan>> + 'a {
    // TODO support the alternate form as an extension

    let inner = || (
        (kw!(KIND), equals()).optional(),
        int_constant_expr(cfg), // TODO the standard says this, but maybe kind_param should be used instead?
    ).map(|(_, expr)| KindSelector { value: expr });

    let with_parenthesis = move || (
        delim('('), inner(), delim(')'),
    ).map(|(_, kind_selector, _)| kind_selector);

    let with_asterisk = move || (
        asterisk(), inner(),
    ).map(|(_, kind_selector)| kind_selector);

    alt!(
        with_parenthesis(),
        with_asterisk(),
    )
}

#[derive(Debug, Clone)]
pub struct IntegerTypeSpec<Span> {
    pub kind_selector: Option<KindSelector<Span>>,
}

// TODO test
#[syntax_rule(
    F18V007r1 rule "integer-type-spec" #705 : "is INTEGER [ kind-selector ]",
)]
pub fn integer_type_spec<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = IntegerTypeSpec<MultilineSpan>> + 'a {
    (
        kw!(INTEGER),
        kind_selector(cfg).optional(),
    ).map(|(_, kind_selector)| IntegerTypeSpec { kind_selector })
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum IntrinsicTypeSpec<Span> {
    Integer(IntegerTypeSpec<Span>),
    Real(Option<KindSelector<Span>>),
    DoublePrecision,
    Complex(Option<KindSelector<Span>>),
    Character(Option<CharSelector<Span>>),
    Logical(Option<KindSelector<Span>>),
}

// TODO test
#[syntax_rule(
    F18V007r1 rule "intrinsic-type-spec" #704 :
    "is integer-type-spec"
    "or REAL [ kind-selector ]"
    "or DOUBLE PRECISION"
    "or COMPLEX [ kind-selector ]"
    "or CHARACTER [ char-selector ]"
    "or LOGICAL [ kind-selector ]",
)]
pub fn intrinsic_type_spec<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = IntrinsicTypeSpec<MultilineSpan>> + 'a {
    alt!(
        integer_type_spec(cfg).map(IntrinsicTypeSpec::Integer),
        (
            kw!(real),
            kind_selector(cfg).optional(),
        ).map(|(_, kind_selector)| IntrinsicTypeSpec::Real(kind_selector)),
        (
            kw!(double),
            kw!(precision),
        ).map(|_| IntrinsicTypeSpec::DoublePrecision),
        (
            kw!(complex),
            kind_selector(cfg).optional(),
        ).map(|(_, kind_selector)| IntrinsicTypeSpec::Complex(kind_selector)),
        (
            kw!(character),
            char_selector(cfg).optional(),
        ).map(|(_, char_selector)| IntrinsicTypeSpec::Character(char_selector)),
        (
            kw!(logical),
            kind_selector(cfg).optional(),
        ).map(|(_, kind_selector)| IntrinsicTypeSpec::Logical(kind_selector)),
    )
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum CharLength<Span> {
    TypeParamValue(TypeParamValue<Span>),
    Int(IntLiteralConstant<Span>),
}

// TODO test
#[syntax_rule(
    F18V007r1 rule "char-length" #723 :
    "is ( type-param-value )"
    "or int-literal-constant",
)]
pub fn char_length<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = CharLength<MultilineSpan>> + 'a {
    alt!(
        (delim('('), type_param_value(cfg), delim(')')).map(|(_, type_param_value, _)| CharLength::TypeParamValue(type_param_value)),
        int_literal_constant().map(CharLength::Int),
    )
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum LengthSelector<Span> {
    Parenthesized(TypeParamValue<Span>),
    Asterisk(CharLength<Span>),
}

// TODO test
#[syntax_rule(
    F18V007r1 rule "length-selector" #722 :
    "is ( [ LEN = ] type-param-value )"
    "or * char-length [ , ]",
)]
pub fn length_selector<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = LengthSelector<MultilineSpan>> + 'a {
    alt!(
        (delim('('), type_param_value(cfg), delim(')')).map(|(_, type_param_value, _)| LengthSelector::Parenthesized(type_param_value)),
        (asterisk(), char_length(cfg), comma().optional()).map(|(_, char_length, _)| LengthSelector::Asterisk(char_length)),
    )
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum CharSelector<Span> {
    Len(LengthSelector<Span>),
    LenKind(TypeParamValue<Span>, IntConstantExpr<Span>),
    Kind(IntConstantExpr<Span>),
}

// TODO test
#[syntax_rule(
    F18V007r1 rule "char-selector" #721 :
    "is length-selector"
    "or ( LEN = type-param-value , KIND = scalar-int-constant-expr )"
    "or ( type-param-value , [ KIND = ] scalar-int-constant-expr )"
    "or ( KIND = scalar-int-constant-expr [ , LEN =type-param-value ] )",
)]
pub fn char_selector<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = CharSelector<MultilineSpan>> + 'a {
    alt!(
        length_selector(cfg).map(CharSelector::Len),
        (
            delim('('),
            (
                kw!(len),
                equals(),
                type_param_value(cfg),
            ).map(|(_, _, len)| len),
            comma(),
            (
                kw!(kind),
                equals(),
                int_constant_expr(cfg)
            ).map(|(_, _, kind)| kind),
            delim(')'),
        ).map(|(_,len, _, kind, _)| CharSelector::LenKind(len, kind)),
        (
            delim('('),
            type_param_value(cfg),
            comma(),
            (
                (
                    kw!(kind),
                    equals(),
                ).optional(),
                int_constant_expr(cfg)
            ).map(|(_, kind)| kind),
            delim(')'),
        ).map(|(_, len, _, kind, _)| CharSelector::LenKind(len, kind)),
        (
            delim('('),
            (
                kw!(kind),
                equals(),
                int_constant_expr(cfg)
            ).map(|(_, _, kind)| kind),
            (
                comma(),
                kw!(len),
                equals(),
                type_param_value(cfg),
            ).map(|(_, _, _, len)| len).optional(),
            delim(')'),
        ).map(|(_, kind, len, _)| match len {
            Some(len) => CharSelector::LenKind(len, kind),
            None => CharSelector::Kind(kind),
        }),
    )
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ArrayConstructor<Span> {
    Parenthesized(AcSpec<Span>),
    Bracketed(AcSpec<Span>),
}

#[syntax_rule(
    F18V007r1 rule "array-constructor" #769 :
    "is (/ ac-spec /)"
    "or lbracket ac-spec rbracket",
)]
pub fn array_constructor<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ArrayConstructor<MultilineSpan>> + 'a {
    // TODO test
    alt!(
        (
            delim('('), op("/"),
            ac_spec(cfg),
            op("/"), delim(')'),
        ).map(|(_, _,  ac_spec, _, _)| ArrayConstructor::Parenthesized(ac_spec)),
        (
            delim('['),
            ac_spec(cfg),
            delim(']'),
        ).map(|(_, ac_spec, _)| ArrayConstructor::Bracketed(ac_spec)),
    )
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum AcSpec<Span> {
    Type(TypeSpec<Span>),
    TypeWithValueList(Option<TypeSpec<Span>>, Vec<AcValue<Span>>),
}

#[syntax_rule(
    F18V007r1 rule "ac-spec" #770 :
    "is type-spec ::"
    "or [type-spec ::] ac-value-list",
)]
pub fn ac_spec<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = AcSpec<MultilineSpan>> + 'a {
    // TODO test
    alt!(
        (
            (
                type_spec(cfg),
                double_colon(),
            ).map(|(type_spec, _)| type_spec).optional(),
            separated(
                ac_value(cfg),
                comma(),
                0..,
            ),
        ).map(|(type_spec, ac_values)| AcSpec::TypeWithValueList(type_spec, ac_values)),
        (
            type_spec(cfg),
            double_colon(),
        ).map(|(type_spec, _)| AcSpec::Type(type_spec)),
    )
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum AcValue<Span> {
    Expr(Expr<Span>),
    AcImpliedDo(AcImpliedDo<Span>),
}

#[syntax_rule(
    F18V007r1 rule "ac-value" #773 :
    "is expr"
    "or ac-implied-do",
)]
pub fn ac_value<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = AcValue<MultilineSpan>> + 'a {
    // TODO test
    alt!(
        expr(cfg).map(AcValue::Expr),
        ac_implied_do(cfg).map(AcValue::AcImpliedDo),
    )
}

#[derive(Debug, Clone)]
pub struct AcImpliedDo<Span> {
    pub ac_values: Vec<AcValue<Span>>,
    pub ac_implied_do_control: AcImpliedDoControl<Span>,
}

#[syntax_rule(
    F18V007r1 rule "ac-implied-do" #774 : "is ( ac-value-list , ac-implied-do-control )",
)]
pub fn ac_implied_do<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = AcImpliedDo<MultilineSpan>> + 'a {
    // TODO test
    (
        delim('('),
        separated(
            ac_value(cfg),
            comma(),
            0..,
        ),
        comma(),
        ac_implied_do_control(cfg),
        delim(')'),
    ).map(|(_, ac_values, _, ac_implied_do_control, _)| AcImpliedDo {
        ac_values,
        ac_implied_do_control,
    })
}

#[derive(Debug, Clone)]
pub struct AcImpliedDoControl<Span> {
    pub spec: Option<IntegerTypeSpec<Span>>,
    pub variable: AcDoVariable<Span>,
    pub start: IntExpr<Span>,
    pub end: IntExpr<Span>,
    pub stride: Option<IntExpr<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "ac-implied-do-control" #775 :
    "is [ integer-type-spec :: ] ac-do-variable = scalar-int-expr , scalar-int-expr [ , scalar-int-expr ]",
)]
pub fn ac_implied_do_control<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = AcImpliedDoControl<MultilineSpan>> + 'a {
    // TODO test
    (
        (
            integer_type_spec(cfg),
            double_colon(),
        ).map(|(spec, _)| spec).optional(),
        ac_do_variable(cfg),
        equals(),
        int_expr(cfg),
        comma(),
        int_expr(cfg),
        (comma(), int_expr(cfg)).map(|(_, stride)| stride).optional(),
    ).map(|(spec, variable, _, start, _, end, stride)| AcImpliedDoControl {
        spec,
        variable,
        start,
        end,
        stride,
    })
}

#[derive(Debug, Clone)]
pub struct AcDoVariable<Span>(pub DoVariable<Span>);

#[syntax_rule(
    F18V007r1 rule "ac-do-variable" #776 : "is do-variable",
)]
pub fn ac_do_variable<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = AcDoVariable<MultilineSpan>> + 'a {
    // TODO test
    do_variable(cfg).map(AcDoVariable)
}

/*#[derive(Debug, Clone)]
pub struct DoVariable<Span>(pub Name<Span>);

#[syntax_rule(
    F18V007r1 rule "do-variable" #1124 :
    "is scalar-int-variable-name",
)]
pub fn do_variable<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DoVariable<MultilineSpan>> + 'a {
    // TODO test
    name().map(DoVariable)
}*/

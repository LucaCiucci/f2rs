use super::*;

#[derive(Debug, Clone, EnumAsInner)]
pub enum TypeParamValue<Span> {
    Expr(IntExpr<Span>),
    Asterisk(SpecialCharacterMatch<Span>),
    Colon(SpecialCharacterMatch<Span>),
}

// TODO test
#[syntax_rule(
    F18V007r1 rule "type-param-value" #701 :
    "is scalar-int-expr"
    "or *"
    "or :",
)]
pub fn type_param_value<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = TypeParamValue<S::Span>> + 'a {
    alt!(
        int_expr(cfg).map(TypeParamValue::Expr),
        SpecialCharacter::Asterisk.map(TypeParamValue::Asterisk),
        SpecialCharacter::Colon.map(TypeParamValue::Colon),
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
pub fn kind_selector<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = KindSelector<S::Span>> + 'a {
    // TODO support the alternate form as an extension

    let inner = || (
        (StringMatch::exact("kind", false), space(0), '=').optional(),
        int_constant_expr(cfg), // TODO the standard says this, but maybe kind_param should be used instead?
    ).map(|(_, expr)| KindSelector { value: expr });

    let with_parenthesis = move || (
        '(', space(0), inner(), space(0), ')',
    ).map(|(_, _, kind_selector, _, _)| kind_selector);

    let with_asterisk = move || (
        '*', space(0), inner(),
    ).map(|(_, _, kind_selector)| kind_selector);

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
pub fn integer_type_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = IntegerTypeSpec<S::Span>> + 'a {
    (
        StringMatch::exact("integer", false),
        space(0),
        kind_selector(cfg).optional(),
    ).map(|(_, _, kind_selector)| IntegerTypeSpec { kind_selector })
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
pub fn intrinsic_type_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = IntrinsicTypeSpec<S::Span>> + 'a {
    alt!(
        integer_type_spec(cfg).map(IntrinsicTypeSpec::Integer),
        (
            StringMatch::exact("real", false),
            space(0),
            kind_selector(cfg).optional(),
        ).map(|(_, _, kind_selector)| IntrinsicTypeSpec::Real(kind_selector)),
        (
            StringMatch::exact("double", false),
            space(0),
            StringMatch::exact("precision", false),
        ).map(|_| IntrinsicTypeSpec::DoublePrecision),
        (
            StringMatch::exact("complex", false),
            space(0),
            kind_selector(cfg).optional(),
        ).map(|(_, _, kind_selector)| IntrinsicTypeSpec::Complex(kind_selector)),
        (
            StringMatch::exact("character", false),
            space(0),
            char_selector(cfg).optional(),
        ).map(|(_, _, char_selector)| IntrinsicTypeSpec::Character(char_selector)),
        (
            StringMatch::exact("logical", false),
            space(0),
            kind_selector(cfg).optional(),
        ).map(|(_, _, kind_selector)| IntrinsicTypeSpec::Logical(kind_selector)),
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
pub fn char_length<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = CharLength<S::Span>> + 'a {
    alt!(
        ('(', space(0), type_param_value(cfg), space(0), ')').map(|(_, _, type_param_value, _, _)| CharLength::TypeParamValue(type_param_value)),
        int_literal_constant(cfg).map(CharLength::Int),
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
pub fn length_selector<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = LengthSelector<S::Span>> + 'a {
    alt!(
        ('(', space(0), type_param_value(cfg), space(0), ')').map(|(_, _, type_param_value, _, _)| LengthSelector::Parenthesized(type_param_value)),
        ('*', space(0), char_length(cfg), (space(0), ',').optional()).map(|(_, _, char_length, _)| LengthSelector::Asterisk(char_length)),
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
pub fn char_selector<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = CharSelector<S::Span>> + 'a {
    alt!(
        length_selector(cfg).map(CharSelector::Len),
        (
            Char::exact('('),
            space(0),
            (
                StringMatch::exact("len", false),
                space(0),
                Char::exact('='),
                space(0),
                type_param_value(cfg),
            ).map(|(_, _, _, _, len)| len),
            space(0),
            Char::exact(','),
            space(0),
            (
                StringMatch::exact("kind", false),
                space(0),
                Char::exact('='),
                space(0),
                int_constant_expr(cfg)
            ).map(|(_, _, _, _, kind)| kind),
            space(0),
            ')',
        ).map(|(_, _, len, _, _, _, kind, _, _)| CharSelector::LenKind(len, kind)),
        (
            Char::exact('('),
            space(0),
            type_param_value(cfg),
            space(0),
            Char::exact(','),
            space(0),
            (
                (
                    StringMatch::exact("kind", false),
                    space(0),
                    Char::exact('='),
                ).optional(),
                space(0),
                int_constant_expr(cfg)
            ).map(|(_, _, kind)| kind),
            space(0),
            ')',
        ).map(|(_, _, len, _, _, _, kind, _, _)| CharSelector::LenKind(len, kind)),
        (
            Char::exact('('),
            space(0),
            (
                StringMatch::exact("kind", false),
                space(0),
                Char::exact('='),
                space(0),
                int_constant_expr(cfg)
            ).map(|(_, _, _, _, kind)| kind),
            (
                space(0),
                Char::exact(','),
                space(0),
                StringMatch::exact("len", false),
                space(0),
                Char::exact('='),
                space(0),
                type_param_value(cfg),
            ).map(|(_, _, _, _, _, _, _, len)| len).optional(),
            space(0),
            ')',
        ).map(|(_, _, kind, len, _, _)| match len {
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
pub fn array_constructor<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ArrayConstructor<S::Span>> + 'a {
    // TODO test
    alt!(
        (
            '(', space(0), '/', space(0),
            ac_spec(cfg),
            space(0), '/', space(0), ')',
        ).map(|(_, _, _, _, ac_spec, _, _, _, _)| ArrayConstructor::Parenthesized(ac_spec)),
        (
            '[', space(0),
            ac_spec(cfg),
            space(0), ']',
        ).map(|(_, _, ac_spec, _, _)| ArrayConstructor::Bracketed(ac_spec)),
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
pub fn ac_spec<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = AcSpec<S::Span>> + 'a {
    // TODO test
    alt!(
        (
            (
                type_spec(cfg),
                space(0), "::", space(0),
            ).map(|(type_spec, _, _, _)| type_spec).optional(),
            separated(
                ac_value(cfg),
                (space(0), ',', space(0)),
                0..,
            ),
        ).map(|(type_spec, ac_values)| AcSpec::TypeWithValueList(type_spec, ac_values)),
        (
            type_spec(cfg),
            space(0), "::", space(0),
        ).map(|(type_spec, _, _, _)| AcSpec::Type(type_spec)),
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
pub fn ac_value<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = AcValue<S::Span>> + 'a {
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
pub fn ac_implied_do<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = AcImpliedDo<S::Span>> + 'a {
    // TODO test
    (
        ('(', space(0)),
        separated(
            ac_value(cfg),
            (space(0), ',', space(0)),
            0..,
        ),
        (space(0), ',', space(0)),
        ac_implied_do_control(cfg),
        (space(0), ')'),
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
pub fn ac_implied_do_control<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = AcImpliedDoControl<S::Span>> + 'a {
    // TODO test
    (
        (
            integer_type_spec(cfg),
            space(0), "::", space(0),
        ).map(|(spec, _, _, _)| spec).optional(),
        ac_do_variable(cfg),
        (space(0), '=', space(0)),
        int_expr(cfg),
        (space(0), ',', space(0)),
        int_expr(cfg),
        (space(0), ',', space(0), int_expr(cfg)).map(|(_, _, _, stride)| stride).optional(),
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
pub fn ac_do_variable<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = AcDoVariable<S::Span>> + 'a {
    // TODO test
    do_variable(cfg).map(AcDoVariable)
}

#[derive(Debug, Clone)]
pub struct DoVariable<Span>(pub Name<Span>);

#[syntax_rule(
    F18V007r1 rule "do-variable" #1124 :
    "is scalar-int-variable-name",
)]
pub fn do_variable<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DoVariable<S::Span>> + 'a {
    // TODO test
    name(cfg, false).map(DoVariable)
}

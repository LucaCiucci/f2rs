use crate::tokens::rules::IntLiteralConstant;

use super::*;

#[derive(Debug, Clone, EnumAsInner)]
pub enum TypeParamValue<Span> {
    Expr(IntExpr<Span>),
    Asterisk(StringMatch<Span>),
    Colon(Colon<Span>),
}

// TODO test
#[doc = s_rule!(
    F18V007r1 rule "type-param-value" #701 :
    "is scalar-int-expr"
    "or *"
    "or :",
)]
pub fn type_param_value<S: Lexed>(source: S) -> PResult<TypeParamValue<MultilineSpan>, S> {
    alt!(
        for S =>
        int_expr.map(TypeParamValue::Expr),
        asterisk().map(TypeParamValue::Asterisk),
        colon().map(TypeParamValue::Colon),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct KindSelector<Span> {
    pub value: IntConstantExpr<Span>,
}

// TODO test
#[doc = s_rule!(
    F18V007r1 rule "kind-selector" #706 : "is ( [ KIND = ] scalar-int-constant-expr )",
)]
pub fn kind_selector<S: Lexed>(source: S) -> PResult<KindSelector<MultilineSpan>, S> {
    // TODO support the alternate form as an extension

    let inner = || (
        (kw!(KIND), equals()).optional(),
        int_constant_expr, // TODO the standard says this, but maybe kind_param should be used instead?
    ).map(|(_, expr)| KindSelector { value: expr });

    let with_parenthesis = move || (
        delim('('), inner(), delim(')'),
    ).map(|(_, kind_selector, _)| kind_selector);

    let with_asterisk = move || (
        asterisk(), inner(),
    ).map(|(_, kind_selector)| kind_selector);

    alt!(
        for S =>
        with_parenthesis(),
        with_asterisk(),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct IntegerTypeSpec<Span> {
    pub kind_selector: Option<KindSelector<Span>>,
}

// TODO test
#[doc = s_rule!(
    F18V007r1 rule "integer-type-spec" #705 : "is INTEGER [ kind-selector ]",
)]
pub fn integer_type_spec<S: Lexed>(source: S) -> PResult<IntegerTypeSpec<MultilineSpan>, S> {
    (
        kw!(INTEGER),
        kind_selector.optional(),
    ).map(|(_, kind_selector)| IntegerTypeSpec { kind_selector })
    .parse(source)
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
#[doc = s_rule!(
    F18V007r1 rule "intrinsic-type-spec" #704 :
    "is integer-type-spec"
    "or REAL [ kind-selector ]"
    "or DOUBLE PRECISION"
    "or COMPLEX [ kind-selector ]"
    "or CHARACTER [ char-selector ]"
    "or LOGICAL [ kind-selector ]",
)]
pub fn intrinsic_type_spec<S: Lexed>(source: S) -> PResult<IntrinsicTypeSpec<MultilineSpan>, S> {
    alt!(
        for S =>
        integer_type_spec.map(IntrinsicTypeSpec::Integer),
        (
            kw!(real),
            kind_selector.optional(),
        ).map(|(_, kind_selector)| IntrinsicTypeSpec::Real(kind_selector)),
        (
            kw!(double),
            kw!(precision),
        ).map(|_| IntrinsicTypeSpec::DoublePrecision),
        (
            kw!(complex),
            kind_selector.optional(),
        ).map(|(_, kind_selector)| IntrinsicTypeSpec::Complex(kind_selector)),
        (
            kw!(character),
            char_selector.optional(),
        ).map(|(_, char_selector)| IntrinsicTypeSpec::Character(char_selector)),
        (
            kw!(logical),
            kind_selector.optional(),
        ).map(|(_, kind_selector)| IntrinsicTypeSpec::Logical(kind_selector)),
    ).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum CharLength<Span> {
    TypeParamValue(TypeParamValue<Span>),
    Int(IntLiteralConstant<Span>),
}

// TODO test
#[doc = s_rule!(
    F18V007r1 rule "char-length" #723 :
    "is ( type-param-value )"
    "or int-literal-constant",
)]
pub fn char_length<S: Lexed>(source: S) -> PResult<CharLength<MultilineSpan>, S> {
    alt!(
        for S =>
        (delim('('), type_param_value, delim(')')).map(|(_, type_param_value, _)| CharLength::TypeParamValue(type_param_value)),
        int_literal_constant().map(CharLength::Int),
    ).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum LengthSelector<Span> {
    Parenthesized(TypeParamValue<Span>),
    Asterisk(CharLength<Span>),
}

// TODO test
#[doc = s_rule!(
    F18V007r1 rule "length-selector" #722 :
    "is ( [ LEN = ] type-param-value )"
    "or * char-length [ , ]",
)]
pub fn length_selector<S: Lexed>(source: S) -> PResult<LengthSelector<MultilineSpan>, S> {
    alt!(
        for S =>
        (
            delim('('), (kw!(len), equals()).optional(), type_param_value, delim(')')).map(|(_, _, type_param_value, _)| LengthSelector::Parenthesized(type_param_value)),
        (asterisk(), char_length, comma().optional()).map(|(_, char_length, _)| LengthSelector::Asterisk(char_length)),
    ).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum CharSelector<Span> {
    Len(LengthSelector<Span>),
    LenKind(TypeParamValue<Span>, IntConstantExpr<Span>),
    Kind(IntConstantExpr<Span>),
}

// TODO test
#[doc = s_rule!(
    F18V007r1 rule "char-selector" #721 :
    "is length-selector"
    "or ( LEN = type-param-value , KIND = scalar-int-constant-expr )"
    "or ( type-param-value , [ KIND = ] scalar-int-constant-expr )"
    "or ( KIND = scalar-int-constant-expr [ , LEN =type-param-value ] )",
)]
pub fn char_selector<S: Lexed>(source: S) -> PResult<CharSelector<MultilineSpan>, S> {
    alt!(
        for S =>
        length_selector.map(CharSelector::Len),
        (
            delim('('),
            (
                kw!(len),
                equals(),
                type_param_value,
            ).map(|(_, _, len)| len),
            comma(),
            (
                kw!(kind),
                equals(),
                int_constant_expr
            ).map(|(_, _, kind)| kind),
            delim(')'),
        ).map(|(_,len, _, kind, _)| CharSelector::LenKind(len, kind)),
        (
            delim('('),
            type_param_value,
            comma(),
            (
                (
                    kw!(kind),
                    equals(),
                ).optional(),
                int_constant_expr
            ).map(|(_, kind)| kind),
            delim(')'),
        ).map(|(_, len, _, kind, _)| CharSelector::LenKind(len, kind)),
        (
            delim('('),
            (
                kw!(kind),
                equals(),
                int_constant_expr
            ).map(|(_, _, kind)| kind),
            (
                comma(),
                kw!(len),
                equals(),
                type_param_value,
            ).map(|(_, _, _, len)| len).optional(),
            delim(')'),
        ).map(|(_, kind, len, _)| match len {
            Some(len) => CharSelector::LenKind(len, kind),
            None => CharSelector::Kind(kind),
        }),
    ).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ArrayConstructor<Span> {
    Parenthesized(AcSpec<Span>),
    Bracketed(AcSpec<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "array-constructor" #769 :
    "is (/ ac-spec /)"
    "or lbracket ac-spec rbracket",
)]
pub fn array_constructor<S: Lexed>(source: S) -> PResult<ArrayConstructor<MultilineSpan>, S> {
    // TODO test
    alt!(
        for S =>
        (
            delim('('), op("/"),
            ac_spec,
            op("/"), delim(')'),
        ).map(|(_, _,  ac_spec, _, _)| ArrayConstructor::Parenthesized(ac_spec)),
        (
            delim('['),
            ac_spec,
            delim(']'),
        ).map(|(_, ac_spec, _)| ArrayConstructor::Bracketed(ac_spec)),
    ).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum AcSpec<Span> {
    Type(TypeSpec<Span>),
    TypeWithValueList(Option<TypeSpec<Span>>, Vec<AcValue<Span>>),
}

#[doc = s_rule!(
    F18V007r1 rule "ac-spec" #770 :
    "is type-spec ::"
    "or [type-spec ::] ac-value-list",
)]
pub fn ac_spec<S: Lexed>(source: S) -> PResult<AcSpec<MultilineSpan>, S> {
    // TODO test
    alt!(
        for S =>
        (
            (
                type_spec,
                double_colon(),
            ).map(|(type_spec, _)| type_spec).optional(),
            separated(
                ac_value,
                comma(),
                0..,
            ),
        ).map(|(type_spec, ac_values)| AcSpec::TypeWithValueList(type_spec, ac_values)),
        (
            type_spec,
            double_colon(),
        ).map(|(type_spec, _)| AcSpec::Type(type_spec)),
    ).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum AcValue<Span> {
    Expr(Expr<Span>),
    AcImpliedDo(AcImpliedDo<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "ac-value" #773 :
    "is expr"
    "or ac-implied-do",
)]
pub fn ac_value<S: Lexed>(source: S) -> PResult<AcValue<MultilineSpan>, S> {
    // TODO test
    alt!(
        for S =>
        expr.map(AcValue::Expr),
        ac_implied_do.map(AcValue::AcImpliedDo),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct AcImpliedDo<Span> {
    pub ac_values: Vec<AcValue<Span>>,
    pub ac_implied_do_control: AcImpliedDoControl<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "ac-implied-do" #774 : "is ( ac-value-list , ac-implied-do-control )",
)]
pub fn ac_implied_do<S: Lexed>(source: S) -> PResult<AcImpliedDo<MultilineSpan>, S> {
    // OLD:
    //// TODO test
    //(
    //    delim('('),
    //    separated(
    //        ac_value,
    //        comma(),
    //        0..,
    //    ),
    //    comma(),
    //    ac_implied_do_control,
    //    delim(')'),
    //).map(|(_, ac_values, _, ac_implied_do_control, _)| AcImpliedDo {
    //    ac_values,
    //    ac_implied_do_control,
    //}).parse(source)

    (
        delim('('),
        ac_implied_do_inner,
        delim(')'),
    ).map(|(_, ac_implied_do, _)| ac_implied_do).parse(source)
}

fn ac_implied_do_inner<S: Lexed>(source: S) -> PResult<AcImpliedDo<MultilineSpan>, S> {
    let (r, source) = many_until(
        (ac_value, comma()).map(|(ac_value, _)| ac_value),
        ac_implied_do_control,
        0..,
    ).parse(source)?;
    let (ac_values, ac_implied_do_control) = r;
    let Some(ac_implied_do_control) = ac_implied_do_control else {
        return None
    };
    Some((AcImpliedDo {
        ac_values,
        ac_implied_do_control,
    }, source))
}

#[derive(Debug, Clone)]
pub struct AcImpliedDoControl<Span> {
    pub spec: Option<IntegerTypeSpec<Span>>,
    pub variable: AcDoVariable<Span>,
    pub start: IntExpr<Span>,
    pub end: IntExpr<Span>,
    pub stride: Option<IntExpr<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "ac-implied-do-control" #775 :
    "is [ integer-type-spec :: ] ac-do-variable = scalar-int-expr , scalar-int-expr [ , scalar-int-expr ]",
)]
pub fn ac_implied_do_control<S: Lexed>(source: S) -> PResult<AcImpliedDoControl<MultilineSpan>, S> {
    // TODO test
    (
        (
            integer_type_spec,
            double_colon(),
        ).map(|(spec, _)| spec).optional(),
        ac_do_variable,
        equals(),
        int_expr,
        comma(),
        int_expr,
        (comma(), int_expr).map(|(_, stride)| stride).optional(),
    ).map(|(spec, variable, _, start, _, end, stride)| AcImpliedDoControl {
        spec,
        variable,
        start,
        end,
        stride,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct AcDoVariable<Span>(pub DoVariable<Span>);

#[doc = s_rule!(
    F18V007r1 rule "ac-do-variable" #776 : "is do-variable",
)]
pub fn ac_do_variable<S: Lexed>(source: S) -> PResult<AcDoVariable<MultilineSpan>, S> {
    // TODO test
    do_variable.map(AcDoVariable).parse(source)
}

/*#[derive(Debug, Clone)]
pub struct DoVariable<Span>(pub Name<Span>);

#[doc = s_rule!(
    F18V007r1 rule "do-variable" #1124 :
    "is scalar-int-variable-name",
)]
pub fn do_variable<S: Lexed>(source: S) -> PResult<DoVariable<MultilineSpan>, S> {
    // TODO test
    name().map(DoVariable)
}*/


#[cfg(test)]
mod tests {
    use crate::rule_test;
    use super::*;

    rule_test! {
        // TODO more tests

        type_param_value(F18V007r1 701) {
            examples(|s| type_param_value.parse(s), [
                "1",
                "42",
                "*",
                ":",
            ]);
        }
    }

    rule_test! {
        kind_selector(F18V007r1 706) {
            // TODO more tests

            examples(|s| kind_selector.parse(s), [
                "(1)",
                "(kind=1)",
                "(KIND = 1)",
            ]);
        }
    }

    rule_test! {
        integer_type_spec(F18V007r1 705) {
            // TODO more tests

            examples(|s| integer_type_spec.parse(s), [
                "INTEGER",
                "INTEGER(1)",
                "INTEGER(kind=1)",
            ]);
        }
    }

    rule_test! {
        intrinsic_type_spec(F18V007r1 704) {
            // TODO more tests

            examples(|s| intrinsic_type_spec.parse(s), [
                "INTEGER",
                "REAL",
                "REAL(kind=1)",
                "DOUBLE PRECISION",
                "COMPLEX",
                "COMPLEX(kind=1)",
                "CHARACTER",
                "CHARACTER(1)",
                "CHARACTER(kind=1)",
                "CHARACTER(len=1)",
                "CHARACTER(len=1, kind=1)",
                "CHARACTER(kind=1, len=1)",
                "LOGICAL",
                "LOGICAL(1)",
                "LOGICAL(kind=1)",
            ]);
        }
    }

    rule_test! {
        char_length(F18V007r1 723) {
            // TODO more tests

            examples(|s| char_length.parse(s), [
                "(1)",
                "(*)",
                "(:)",
                "1",
            ]);
        }
    }

    rule_test! {
        length_selector(F18V007r1 722) {
            // TODO more tests

            examples(|s| length_selector.parse(s), [
                "(1)",
                "(len=1)",
                "* 1",
                "*1,",
                "* (1),",
                "*(:),",
                "*(:),",
            ]);
        }
    }

    rule_test! {
        char_selector(F18V007r1 721) {
            // TODO more tests

            examples(|s| char_selector.parse(s), [
                "(1)",
                "(len=1)",
                "(len=1, kind=1)",
                "(1, kind=1)",
                "(kind=1)",
                "(kind=1, len=1)",
            ]);
        }
    }

    rule_test! {
        array_constructor(F18V007r1 769) {
            // TODO more tests

            examples(|s| array_constructor.parse(s), [
                "(/ 1 /)",
                "(/ REAL :: /)",
                "(/ REAL :: 1, 1, 1 /)",
                "(/ 1, 1, 1 /)",
                "[1]",
                "[REAL :: 1, 1, 1 ]",
            ]);

            // from the standard
            examples(|s| array_constructor.parse(s), [
                "(/ (I, I = 1, 1075) /)",
                "[ 3.6, (3.6 / I, I = 1, N) ]",
                "[ PERSON (40,'SMITH'), PERSON (20, 'JONES') ]",
            ]);
        }
    }

    rule_test! {
        ac_spec(F18V007r1 770) {
            // TODO more tests

            examples(|s| ac_spec.parse(s), [
                "REAL ::",
                "REAL :: 1, 1, 1",
                "1, 1, 1",
            ]);
        }
    }

    rule_test! {
        ac_value(F18V007r1 773) {
            // TODO more tests

            examples(|s| ac_value.parse(s), [
                "1",
                "(1, 2, 3, I = 1, 2, 3)",
                "PERSON (40,'SMITH')",
            ]);
        }
    }

    rule_test! {
        ac_implied_do(F18V007r1 774) {
            // TODO more tests

            examples(|s| ac_implied_do.parse(s), [
                "(1, 2, 3, I = 1, 2, 3)",
            ]);
        }
    }

    rule_test! {
        ac_implied_do_control(F18V007r1 775) {
            // TODO more tests

            examples(|s| ac_implied_do_control.parse(s), [
                "INTEGER :: i = 1, 1 + 2, (1 * 3)",
                "i = 1, 1 + 2",
            ]);
        }
    }

    rule_test! {
        ac_do_variable(F18V007r1 776) {
            // TODO more tests

            examples(|s| ac_do_variable.parse(s), [
                "i",
            ]);
        }
    }

    rule_test! {
        // TODO test delim instead, this is just a placeholder
        lbracket(F18V007r1 771) {
            examples(|s| delim('[').parse(s), [
                "[",
            ]);
        }
    }

    rule_test! {
        // TODO test delim instead, this is just a placeholder
        rbracket(F18V007r1 772) {
            examples(|s| delim(']').parse(s), [
                "]",
            ]);
        }
    }
}
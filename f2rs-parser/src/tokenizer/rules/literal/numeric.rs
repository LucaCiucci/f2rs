pub use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Sign<Span> { // TODO span
    Plus(Span),
    Minus(Span),
}

impl<Span> Sign<Span> {
    pub fn span(&self) -> &Span {
        match self {
            Self::Plus(span) => span,
            Self::Minus(span) => span,
        }
    }
}

impl<Span> MapSpan<Span> for Sign<Span> {
    type Spanned<T> = Sign<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        match self {
            Sign::Plus(span) => Sign::Plus(f(span)),
            Sign::Minus(span) => Sign::Minus(f(span)),
        }
    }
}

#[doc = s_rule!(
    F18V007r1 rule "sign" #712 :
    "is +"
    "or -",
)]
pub fn sign<S: TextSource>(source: S) -> PResult<Sign<S::Span>, S> {
    Char::exact('+')
        .or(Char::exact('-')).map(|o| o.inner())
        .map(|c| match &c.value {
            '+' => Sign::Plus(c.span),
            '-' => Sign::Minus(c.span),
            _ => unreachable!(),
        })
        .parse(source)
}

#[derive(Debug, Clone)]
pub struct SignedIntLiteralConstant<Span> {
    pub span: Span,
    pub sign: Option<Sign<Span>>,
    pub int_literal_constant: IntLiteralConstant<Span>,
}

impl<Span> Spanned<Span> for SignedIntLiteralConstant<Span> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl<Span> MapSpan<Span> for SignedIntLiteralConstant<Span> {
    type Spanned<T> = SignedIntLiteralConstant<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        SignedIntLiteralConstant {
            span: f(self.span),
            sign: self.sign.map(|s| s.map_span(f)),
            int_literal_constant: self.int_literal_constant.map_span(f),
        }
    }
}

#[doc = s_rule!(
    F18V007r1 rule "signed-int-literal-constant" #707 : "is [ sign ] int-literal-constant",
)]
pub fn signed_int_literal_constant<S: TextSource>(source: S) -> PResult<SignedIntLiteralConstant<S::Span>, S> {
    (
        sign.opt(),
        int_literal_constant,
    )
        .map(|(sign, int_literal_constant): (Option<Sign<<S as Source>::Span>>, IntLiteralConstant<<S as Source>::Span>)| {
            let span = if let Some(sign) = &sign {
                S::Span::merge(sign.span().clone(), int_literal_constant.span.clone())
            } else {
                int_literal_constant.span.clone()
            };
            SignedIntLiteralConstant {
                span,
                sign,
                int_literal_constant,
            }
        })
        .parse(source)
}

#[derive(Debug, Clone)]
pub struct IntLiteralConstant<Span> {
    pub span: Span,
    pub digits: StringMatch<Span>,
    pub kind_param: Option<KindParam<Span>>,
}

impl<Span> Spanned<Span> for IntLiteralConstant<Span> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl<Span> MapSpan<Span> for IntLiteralConstant<Span> {
    type Spanned<T> = IntLiteralConstant<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        IntLiteralConstant {
            span: f(self.span),
            digits: self.digits.map_span(f),
            kind_param: self.kind_param.map(|k| k.map_span(f)),
        }
    }
}

#[doc = s_rule!(
    F18V007r1 rule "int-literal-constant" #708 : "is digit-string [ _ kind-param ]",
)]
pub fn int_literal_constant<S: TextSource>(source: S) -> PResult<IntLiteralConstant<S::Span>, S> {
    // TODO maybe relax R708 to allow for trailing underscores if a kind param is missing:
    //`digit-string [ _ kind-param ]` --> `digit-string [ _ ]  [ kind-param ]`

    (
        digit_string::<S>,
        (
            underscore::<S>,
            kind_param(false),
        )
            .map(|(_, kind_param)| kind_param)
            .opt(),
    ).map(|(digits, kind_param)| {

        let span = if let Some(kind_param) = &kind_param {
            S::Span::merge(
                digits.span.clone(),
                match &kind_param {
                    KindParam::DigitString(d) => d.span.clone(),
                    KindParam::ScalarIntConstantName(n) => n.0.span.clone(),
                }
            )
        } else {
            digits.span.clone()
        };

        IntLiteralConstant {
            span,
            digits,
            kind_param,
        }
    })
    .parse(source)
}

/// As defined in J3/18-007r1 §7.4.3.1 R709
#[derive(Debug, Clone, EnumAsInner)]
pub enum KindParam<Span> {
    DigitString(StringMatch<Span>),
    ScalarIntConstantName(Name<Span>),
}

impl<Span> Spanned<Span> for KindParam<Span> {
    fn span(&self) -> &Span {
        match self {
            KindParam::DigitString(s) => s.span(),
            KindParam::ScalarIntConstantName(n) => n.span(),
        }
    }
}

impl<Span> MapSpan<Span> for KindParam<Span> {
    type Spanned<T> = KindParam<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        match self {
            KindParam::DigitString(s) => KindParam::DigitString(s.map_span(f)),
            KindParam::ScalarIntConstantName(n) => KindParam::ScalarIntConstantName(n.map_span(f)),
        }
    }
}

#[doc = s_rule!(
    F18V007r1 rule "kind-param" #709 :
    "is digit-string"
    "or scalar-int-constant-name",
)]
pub fn kind_param<'a, S: TextSource + 'a>(drop_last_underscore: bool) -> impl Parser<S, Token = KindParam<S::Span>> {
    // TODO implement J3/18-007r1 §7.4.3.1 C713
    move |source: S| None
        .or_else(|| digit_string.map(KindParam::DigitString).parse(source.clone()))
        .or_else(|| name(drop_last_underscore).map(KindParam::ScalarIntConstantName).parse(source))
}

#[derive(Debug, Clone)]
pub struct SignedDigitString<Span> {
    pub span: Span,
    pub sign: Option<Sign<Span>>,
    pub digits: StringMatch<Span>,
}

impl<Span> MapSpan<Span> for SignedDigitString<Span> {
    type Spanned<T> = SignedDigitString<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        SignedDigitString {
            span: f(self.span),
            sign: self.sign.map(|s| s.map_span(f)),
            digits: self.digits.map_span(f),
        }
    }
}

#[doc = s_rule!(
    F18V007r1 rule "signed-digit-string" #710 : "is [ sign ] digit-string",
)]
pub fn signed_digit_string<S: TextSource>(source: S) -> PResult<SignedDigitString<S::Span>, S> {
    (
        sign::<S>.opt(),
        digit_string,
    )
        .map(|(sign, digits)| {
            let span = if let Some(sign) = &sign {
                S::Span::merge(sign.span().clone(), digits.span.clone())
            } else {
                digits.span.clone()
            };

            SignedDigitString {
                span,
                sign,
                digits,
            }
        })
        .parse(source)
}

#[doc = s_rule!(
    F18V007r1 rule "digit-string" #711 : "is digit [ digit ] ...",
)]
pub fn digit_string<S: TextSource>(source: S) -> PResult<StringMatch<S::Span>, S> {
    digit
        .then(|first| fold_many(
            digit,
            move || StringMatch::from_char(first.clone()),
            |mut string, d| {
                string.push_char(d);
                (string, true)
            },
            0..,
        ))
        .parse(source)
}

/// As defined in J3/18-007r1 §7.4.3.2 R713
#[derive(Debug, Clone)]
pub struct SignedRealLiteralConstant<Span> {
    pub sign: Option<Sign<Span>>,
    pub real_literal_constant: RealLiteralConstant<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "signed-real-literal-constant" #713 : "is [ sign ] real-literal-constant",
)]
pub fn signed_real_literal_constant<S: TextSource>(source: S) -> PResult<SignedRealLiteralConstant<S::Span>, S> {
    (
        sign.opt(),
        real_literal_constant,
    )
        .map(|(sign, real_literal_constant)| SignedRealLiteralConstant {
            sign,
            real_literal_constant,
        })
        .parse(source)
}

/// As defined in J3/18-007r1 §7.4.3.2 R714
#[derive(Debug, Clone, EnumAsInner)]
pub enum RealLiteralConstant<Span> {
    StartsWithSignificand {
        significand: Significand<Span>,
        exponent_letter_and_exponent: Option<(ExponentLetter<Span>, SignedDigitString<Span>)>,
        kind_param: Option<KindParam<Span>>,
        span: Span,
    },
    StartsWithDigits {
        digits_string: StringMatch<Span>,
        exponent_letter: ExponentLetter<Span>,
        exponent: SignedDigitString<Span>,
        kind_param: Option<KindParam<Span>>,
        span: Span,
    }
}

impl<Span> MapSpan<Span> for RealLiteralConstant<Span> {
    type Spanned<T> = RealLiteralConstant<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        match self {
            RealLiteralConstant::StartsWithSignificand { significand, exponent_letter_and_exponent, kind_param, span } => RealLiteralConstant::StartsWithSignificand {
                significand: significand.map_span(f),
                exponent_letter_and_exponent: exponent_letter_and_exponent.map(|(e, d)| (e.map_span(f), d.map_span(f))),
                kind_param: kind_param.map(|k| k.map_span(f)),
                span: f(span),
            },
            RealLiteralConstant::StartsWithDigits { digits_string, exponent_letter, exponent, kind_param, span } => RealLiteralConstant::StartsWithDigits {
                digits_string: digits_string.map_span(f),
                exponent_letter: exponent_letter.map_span(f),
                exponent: exponent.map_span(f),
                kind_param: kind_param.map(|k| k.map_span(f)),
                span: f(span),
            },
        }
    }
}

impl<Span> RealLiteralConstant<Span> {
    pub fn span(&self) -> &Span {
        match self {
            RealLiteralConstant::StartsWithSignificand { span, .. } => span,
            RealLiteralConstant::StartsWithDigits { span, .. } => span,
        }
    }
}

#[doc = s_rule!(
    F18V007r1 rule "real-literal-constant" #714 :
    "is significand [ exponent-letter exponent ] [ _ kind-param ]"
    "or digit-string exponent-letter exponent [ _ kind-param ]",
)]
pub fn real_literal_constant<S: TextSource>(source: S) -> PResult<RealLiteralConstant<S::Span>, S> {
    // TODO implement §7.4.3.2 C716

    alt!(
        for S =>
        (
            significand,
            (
                exponent_letter,
                exponent,
            ).opt(),
            (
                underscore,
                kind_param(false),
            ).map(|(_, k)| k).opt(),
        )
            .map(|(significand, exponent_letter_and_exponent, kind_param): (Significand<S::Span>, Option<(ExponentLetter<S::Span>, SignedDigitString<S::Span>)>, Option<KindParam<S::Span>>)| {
                let mut span = significand.span().clone();
                if let Some((exponent_letter, exponent)) = &exponent_letter_and_exponent {
                    span = S::Span::merge(span, exponent_letter.span().clone());
                    span = S::Span::merge(span, exponent.span.clone());
                }
                RealLiteralConstant::StartsWithSignificand {
                    significand,
                    exponent_letter_and_exponent,
                    kind_param,
                    span,
                }
            }),
        (
            digit_string,
            exponent_letter,
            exponent,
            (
                underscore,
                kind_param(false),
            ).map(|(_, k)| k).opt(),
        )
            .map(|(digits_string, exponent_letter, exponent, kind_param): (StringMatch<S::Span>, ExponentLetter<S::Span>, SignedDigitString<S::Span>, Option<KindParam<S::Span>>)| {
                let span = S::Span::merge(
                    digits_string.span.clone(),
                    S::Span::merge(
                        exponent_letter.span().clone(),
                        exponent.span.clone(),
                    ),
                );
                RealLiteralConstant::StartsWithDigits {
                    digits_string,
                    exponent_letter,
                    exponent,
                    kind_param,
                    span,
                }
            }),
    ).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ExponentLetter<Span> {
    D(Span),
    E(Span),
}

impl<Span> ExponentLetter<Span> {
    pub fn span(&self) -> &Span {
        match self {
            ExponentLetter::D(span) => span,
            ExponentLetter::E(span) => span,
        }
    }
}

impl<Span> MapSpan<Span> for ExponentLetter<Span> {
    type Spanned<T> = ExponentLetter<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        match self {
            ExponentLetter::D(span) => ExponentLetter::D(f(span)),
            ExponentLetter::E(span) => ExponentLetter::E(f(span)),
        }
    }
}

#[doc = s_rule!(
    F18V007r1 rule "exponent-letter" #716 :
    "is E"
    "or D",
)]
pub fn exponent_letter<S: TextSource>(source: S) -> PResult<ExponentLetter<S::Span>, S> {
    alt!(
        for S =>
        Char::exact_case_insensitive('d').map(|c| ExponentLetter::D(c.span)),
        Char::exact_case_insensitive('e').map(|c| ExponentLetter::E(c.span)),
    ).parse(source)
}

#[doc = s_rule!(
    F18V007r1 rule "exponent" #717 : "is signed-digit-string",
)]
pub fn exponent<S: TextSource>(source: S) -> PResult<SignedDigitString<S::Span>, S> {
    signed_digit_string.parse(source)
}

/// As defined in J3/18-007r1 §7.4.3.2 R715
#[derive(Debug, Clone, EnumAsInner)]
pub enum Significand<Span> {
    // TODO span
    DotAfter(StringMatch<Span>, Option<StringMatch<Span>>, Span), // digit-string . [ digit-string ]
    DotBefore(StringMatch<Span>), // . digit-string
}

impl<Span> Significand<Span> {
    pub fn span(&self) -> &Span {
        match self {
            Significand::DotAfter(_, _, span) => span,
            Significand::DotBefore(second) => &second.span,
        }
    }
}

impl<Span> MapSpan<Span> for Significand<Span> {
    type Spanned<T> = Significand<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        match self {
            Significand::DotAfter(first, second, span) => Significand::DotAfter(
                first.map_span(f),
                second.map(|s| s.map_span(f)),
                f(span),
            ),
            Significand::DotBefore(second) => Significand::DotBefore(second.map_span(f)),
        }
    }
}

#[doc = s_rule!(
    F18V007r1 rule "significand" #715 :
    "is digit-string . [ digit-string ]"
    "or . digit-string",
)]
pub fn significand<S: TextSource>(source: S) -> PResult<Significand<S::Span>, S> {
    alt! {
        for S =>
        (
            digit_string,
            '.',
            digit_string.opt(),
        ).map(|(first, _, second): (StringMatch<S::Span>, Char<S::Span>, Option<StringMatch<S::Span>>)| {
            let mut span = first.span.clone();
            if let Some(second) = &second {
                span = S::Span::merge(span, second.span.clone());
            }
            Significand::DotAfter(first, second, span)
        }),
        (
            '.',
            digit_string,
        ).map(|(_, second)| Significand::DotBefore(second)),
    }.parse(source)
}

/// Binary, octal, and hexadecimal literal constant
#[derive(Debug, Clone, EnumAsInner)]
pub enum BozLiteralConstant<Span> {
    Binary(StringMatch<Span>),
    Octal(StringMatch<Span>),
    Hex(StringMatch<Span>),
}

impl<Span> BozLiteralConstant<Span> {
    pub fn span(&self) -> &Span {
        match self {
            BozLiteralConstant::Binary(m) => &m.span,
            BozLiteralConstant::Octal(m) => &m.span,
            BozLiteralConstant::Hex(m) => &m.span,
        }
    }
}

impl<Span> MapSpan<Span> for BozLiteralConstant<Span> {
    type Spanned<T> = BozLiteralConstant<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        match self {
            BozLiteralConstant::Binary(m) => BozLiteralConstant::Binary(m.map_span(f)),
            BozLiteralConstant::Octal(m) => BozLiteralConstant::Octal(m.map_span(f)),
            BozLiteralConstant::Hex(m) => BozLiteralConstant::Hex(m.map_span(f)),
        }
    }
}

/// Binary, octal, and hexadecimal literal constant
#[doc = s_rule!(
    F18V007r1 rule "boz-literal-constant" #764 :
    "is binary-constant"
    "or octal-constant"
    "or hex-constant",
)]
pub fn boz_literal_constant<S: TextSource>(source: S) -> PResult<BozLiteralConstant<S::Span>, S> {
    alt!(
        for S =>
        binary_constant.map(BozLiteralConstant::Binary),
        octal_constant.map(BozLiteralConstant::Octal),
        hex_constant.map(BozLiteralConstant::Hex),
    ).parse(source)
}

#[doc = s_rule!(
    F18V007r1 rule "binary-constant" #765 :
    "is B ' digit [ digit ] ... '"
    "or B \" digit [ digit ] ... \"",
)]
pub fn binary_constant<S: TextSource>(source: S) -> PResult<StringMatch<S::Span>, S> {
    let binary_digits = || fold_many(
        Char::any_of("01".chars()),
        || StringMatch::empty(),
        |mut string, d| {
            string.push_char(d);
            (string, true)
        },
        1..,
    );

    alt! {
        for S =>
        (
            Char::any_of("bB".chars()),
            Char::exact('\''),
            binary_digits(),
            Char::exact('\'').opt(),
        ),
        (
            Char::any_of("bB".chars()),
            Char::exact('"'),
            binary_digits(),
            Char::exact('"').opt(),
        ),
    }.map(|(_, _, digits, _)| digits)
    .parse(source)
}

#[doc = s_rule!(
    F18V007r1 rule "octal-constant" #766 :
    "is O ' digit [ digit ] ... '"
    "or O \" digit [ digit ] ... \"",
)]
pub fn octal_constant<S: TextSource>(source: S) -> PResult<StringMatch<S::Span>, S> {
    let octal_digits = || fold_many(
        Char::any_of("01234567".chars()),
        || StringMatch::empty(),
        |mut string, d| {
            string.push_char(d);
            (string, true)
        },
        1..,
    );

    alt! {
        for S =>
        (
            Char::any_of("oO".chars()),
            Char::exact('\''),
            octal_digits(),
            Char::exact('\'').opt(),
        ),
        (
            Char::any_of("oO".chars()),
            Char::exact('"'),
            octal_digits(),
            Char::exact('"').opt(),
        ),
    }.map(|(_, _, digits, _)| digits)
    .parse(source)
}

#[doc = s_rule!(
    F18V007r1 rule "hex-constant" #767 :
    "is Z ' hex-digit [ hex-digit ] ... '"
    "or Z \" hex-digit [ hex-digit ] ... \"",
)]
pub fn hex_constant<S: TextSource>(source: S) -> PResult<StringMatch<S::Span>, S> {
    let hex_digits = || fold_many(
        hex_digit,
        || StringMatch::empty(),
        |mut string, d| {
            string.push_char(d);
            (string, true)
        },
        1..,
    );

    alt! {
        for S =>
        (
            Char::any_of("zZ".chars()),
            Char::exact('\''),
            hex_digits(),
            Char::exact('\'').opt(),
        ),
        (
            Char::any_of("zZ".chars()),
            Char::exact('"'),
            hex_digits(),
            Char::exact('"').opt(),
        ),
    }.map(|(_, _, digits, _)| digits)
    .parse(source)
}

#[doc = s_rule!(
    F18V007r1 rule "hex-digit" #768 :
    "is digit"
    "or A"
    "or B"
    "or C"
    "or D"
    "or E"
    "or F",
)]
pub fn hex_digit<S: TextSource>(source: S) -> PResult<Char<S::Span>, S> {
    alt!(
        for S =>
        digit,
        Char::any_of("abcdefABCDEF".chars()),
    ).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum NonComplexLiteralConstant<Span> {
    Int(IntLiteralConstant<Span>),
    Real(RealLiteralConstant<Span>),

    // NOTE: NO COMPLEX!!!

    Logical(LogicalLiteralConstant<Span>),
    Char(CharLiteralConstant<Span>),
    Boz(BozLiteralConstant<Span>),
}

impl<Span> NonComplexLiteralConstant<Span> {
    pub fn span(&self) -> &Span {
        match self {
            NonComplexLiteralConstant::Int(l) => l.span(),
            NonComplexLiteralConstant::Real(l) => l.span(),
            NonComplexLiteralConstant::Logical(l) => l.span(),
            NonComplexLiteralConstant::Char(l) => l.span(),
            NonComplexLiteralConstant::Boz(l) => l.span(),
        }
    }
}

impl<Span> MapSpan<Span> for NonComplexLiteralConstant<Span> {
    type Spanned<T> = NonComplexLiteralConstant<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        match self {
            NonComplexLiteralConstant::Int(l) => NonComplexLiteralConstant::Int(l.map_span(f)),
            NonComplexLiteralConstant::Real(l) => NonComplexLiteralConstant::Real(l.map_span(f)),
            NonComplexLiteralConstant::Logical(l) => NonComplexLiteralConstant::Logical(l.map_span(f)),
            NonComplexLiteralConstant::Char(l) => NonComplexLiteralConstant::Char(l.map_span(f)),
            NonComplexLiteralConstant::Boz(l) => NonComplexLiteralConstant::Boz(l.map_span(f)),
        }
    }
}

pub fn non_complex_literal_constant<S: TextSource>(source: S) -> PResult<NonComplexLiteralConstant<S::Span>, S> {
    alt! {
        for S =>
        // note: we try to parse the most specific first, this is the reason for real to be first, otherwise int would always be parsed
        real_literal_constant.map(NonComplexLiteralConstant::Real),
        int_literal_constant.map(NonComplexLiteralConstant::Int),
        logical_literal_constant.map(NonComplexLiteralConstant::Logical),
        char_literal_constant.map(NonComplexLiteralConstant::Char),
        boz_literal_constant.map(NonComplexLiteralConstant::Boz),
    }.parse(source)
}

#[cfg(test)]
mod test {

    use crate::rule_test;
    //use super::super::examples;

    use super::*;

    rule_test! {
        non_complex_literal_constant() {
            assert_eq!(non_complex_literal_constant.parses("a"), false);
            assert!(non_complex_literal_constant.parse("42").unwrap().0.is_int());
            assert!(non_complex_literal_constant.parse("42.0").unwrap().0.is_real());
            assert!(non_complex_literal_constant.parse("42e1").unwrap().0.is_real());
            assert!(non_complex_literal_constant.parse(".TRUE.").unwrap().0.is_logical());
            assert!(non_complex_literal_constant.parse("'a'").unwrap().0.is_char());
            assert!(non_complex_literal_constant.parse("B'1010'").unwrap().0.is_boz());
            assert!(non_complex_literal_constant.parse("O'123'").unwrap().0.is_boz());
            assert!(non_complex_literal_constant.parse("Z'ABC'").unwrap().0.is_boz());
        }
    }

    rule_test! {
        digit_string(F18V007r1 711, F18V007r1 611) {
            assert_eq!(digit_string.parses(""), false);
            assert_eq!(digit_string.parse("1").unwrap().0.value, "1");
            assert_eq!(digit_string.parses("a"), false);
            assert_eq!(digit_string.parse("42").unwrap().0.value, "42");
            assert_eq!(digit_string.parse("42 ").unwrap().0.value, "42");
            assert_eq!(digit_string.parse("42.0").unwrap().0.value, "42");
            assert_eq!(digit_string.parse("42e1").unwrap().0.value, "42");
            assert_eq!(digit_string.parses(" 42.0 "), false);
            assert_eq!(digit_string.parses("-42"), false);
            assert_eq!(digit_string.parses("+42.0"), false);
            assert_eq!(digit_string.parses("a42.0"), false);

            // examples from "label" (F18V007r1§6.2.5)
            examples(digit_string, [
                "99999",
                "10",
                "010",
            ]);
        }
    }

    rule_test! {
        signed_digit_string(F18V007r1 710) {
            assert_eq!(signed_digit_string.parses(""), false);
            assert_eq!(signed_digit_string.parse("1").unwrap().0.digits.value, "1");
            assert_eq!(signed_digit_string.parse("1").unwrap().0.sign.is_some(), false);
            assert_eq!(signed_digit_string.parses("a"), false);
            assert_eq!(signed_digit_string.parse("+1").unwrap().0.digits.value, "1");
            assert_eq!(signed_digit_string.parse("+1").unwrap().0.sign.unwrap().is_plus(), true);
            assert_eq!(signed_digit_string.parse("-1").unwrap().0.digits.value, "1");
            assert_eq!(signed_digit_string.parse("-1").unwrap().0.sign.unwrap().is_minus(), true);
            assert_eq!(signed_digit_string.parse("42").unwrap().0.digits.value, "42");
            assert_eq!(signed_digit_string.parse("42").unwrap().0.sign.is_some(), false);
            assert_eq!(signed_digit_string.parse("+42").unwrap().0.digits.value, "42");
            assert_eq!(signed_digit_string.parse("+42").unwrap().0.sign.unwrap().is_plus(), true);
            assert_eq!(signed_digit_string.parse("-42").unwrap().0.digits.value, "42");
            assert_eq!(signed_digit_string.parse("-42").unwrap().0.sign.unwrap().is_minus(), true);
            assert_eq!(signed_digit_string.parse("42.0").unwrap().0.digits.value, "42");
            assert_eq!(signed_digit_string.parse("42.0").unwrap().0.sign.is_some(), false);
            assert_eq!(signed_digit_string.parse("+42e1").unwrap().0.digits.value, "42");
            assert_eq!(signed_digit_string.parse("+42e1").unwrap().0.sign.unwrap().is_plus(), true);
            assert_eq!(signed_digit_string.parses("a42.0"), false);
        }
    }

    rule_test! {
        sign(F18V007r1 712) {
            assert_eq!(sign.parses(""), false);
            assert_eq!(sign.parse("+").unwrap().0.is_plus(), true);
            assert_eq!(sign.parse("-").unwrap().0.is_minus(), true);
            assert_eq!(sign.parses("a"), false);
            assert_eq!(sign.parses("1"), false);
        }
    }

    rule_test! {
        // From the standard
        signed_int_literal_constant(F18V007r1 707) {
            examples(signed_int_literal_constant, [
                "473",
                "+56",
                "-101",
                "21_2",
                "21_SHORT",
                "1976354279568241_8",
            ]);
        }
    }

    rule_test! {
        int_literal_constant(F18V007r1 708) {
            assert_eq!(int_literal_constant.parses(""), false);
            assert_eq!(int_literal_constant.parse("42").unwrap().0.digits.value, "42");
            assert_eq!(int_literal_constant.parse("0").unwrap().0.digits.value, "0");
            assert_eq!(int_literal_constant.parse("1234567890").unwrap().0.digits.value, "1234567890");
            assert_eq!(int_literal_constant.parse("42 ").unwrap().0.digits.value, "42");
            assert_eq!(int_literal_constant.parse("42_").unwrap().0.digits.value, "42");
            assert_eq!(int_literal_constant.parse("42_").unwrap().0.kind_param.is_none(), true);
            assert_eq!(int_literal_constant.parse("42_123").unwrap().0.digits.value, "42");
            assert_eq!(int_literal_constant.parse("42_123").unwrap().0.kind_param.is_some(), true);
            assert_eq!(int_literal_constant.parse("42_123").unwrap().0.kind_param.unwrap().as_digit_string().unwrap().value, "123");
            assert_eq!(int_literal_constant.parse("42_abc").unwrap().0.kind_param.unwrap().as_scalar_int_constant_name().unwrap().0.value, "abc");
            assert_eq!(int_literal_constant.parse("42_q").unwrap().0.kind_param.unwrap().as_scalar_int_constant_name().unwrap().0.value, "q");
            assert_eq!(int_literal_constant.parses(" 42"), false);
            assert_eq!(int_literal_constant.parses("a42"), false);
            assert_eq!(int_literal_constant.parses("-42"), false);
            assert_eq!(int_literal_constant.parses("+42"), false);
        }
    }

    rule_test! {
        kind_param(F18V007r1 709) {
            assert_eq!(kind_param(false).parses(""), false);
            assert_eq!(kind_param(false).parse("42").unwrap().0.is_digit_string(), true);
            assert_eq!(kind_param(false).parse("42").unwrap().0.as_digit_string().unwrap().value, "42");
            assert_eq!(kind_param(false).parses("_42"), false);
            assert_eq!(kind_param(false).parses("a_b"), true);
            assert_eq!(kind_param(false).parses("_a_b"), false);
            assert_eq!(kind_param(false).parse("a_b_").unwrap().0.as_scalar_int_constant_name().unwrap().0.value, "a_b_");
            assert_eq!(kind_param(true).parse("a_b_").unwrap().0.as_scalar_int_constant_name().unwrap().0.value, "a_b");
            assert_eq!(kind_param(false).parses(" a_b"), false);
        }
    }

    rule_test! {
        real_literal_constant(F18V007r1 714) {
            assert_eq!(real_literal_constant.parses(""), false);
            assert_eq!(real_literal_constant.parses("42"), false);
            assert_eq!(real_literal_constant.parses("+42"), false);
            assert_eq!(real_literal_constant.parses("-42"), false);
            assert_eq!(real_literal_constant.parses("-1.1"), false);
            assert_eq!(real_literal_constant.parses("-1e1"), false);
            assert_eq!(real_literal_constant.parses("1.1"), true);
            assert_eq!(real_literal_constant.parses("1e1"), true);
            assert_eq!(real_literal_constant.parses("1_foo"), false);
            assert_eq!(real_literal_constant.parses("12345_11"), false);
            assert_eq!(real_literal_constant.parses("1234567890e"), false);
            let m = real_literal_constant.parse("1234567890e42").unwrap();
            let m = m.0.as_starts_with_digits().unwrap();
            assert_eq!(m.0.value, "1234567890");
            assert_eq!(m.1.is_e(), true);
            assert_eq!(m.2.sign.is_some(), false);
            assert_eq!(m.2.digits.value, "42");
            assert_eq!(m.3.is_some(), false);
            let m = real_literal_constant.parse("1234567890E42").unwrap();
            let m = m.0.as_starts_with_digits().unwrap();
            assert_eq!(m.0.value, "1234567890");
            assert_eq!(m.1.is_e(), true);
            assert_eq!(m.2.sign.is_some(), false);
            assert_eq!(m.2.digits.value, "42");
            assert_eq!(m.3.is_some(), false);
            let m = real_literal_constant.parse("1234567890d42").unwrap();
            let m = m.0.as_starts_with_digits().unwrap();
            assert_eq!(m.0.value, "1234567890");
            assert_eq!(m.1.is_d(), true);
            assert_eq!(m.2.sign.is_some(), false);
            assert_eq!(m.2.digits.value, "42");
            assert_eq!(m.3.is_some(), false);
            let m = real_literal_constant.parse("1234567890D42").unwrap();
            let m = m.0.as_starts_with_digits().unwrap();
            assert_eq!(m.0.value, "1234567890");
            assert_eq!(m.1.is_d(), true);
            assert_eq!(m.2.sign.is_some(), false);
            assert_eq!(m.2.digits.value, "42");
            assert_eq!(m.3.is_some(), false);
            let m = real_literal_constant.parse("1234567890e+42").unwrap();
            let m = m.0.as_starts_with_digits().unwrap();
            assert_eq!(m.0.value, "1234567890");
            assert_eq!(m.1.is_e(), true);
            assert_eq!(m.2.sign.is_some(), true);
            assert_eq!(m.2.sign.unwrap().is_plus(), true);
            assert_eq!(m.2.digits.value, "42");
            assert_eq!(m.3.is_some(), false);
            let m = real_literal_constant.parse("1234567890e-42").unwrap();
            let m = m.0.as_starts_with_digits().unwrap();
            assert_eq!(m.0.value, "1234567890");
            assert_eq!(m.1.is_e(), true);
            assert_eq!(m.2.sign.is_some(), true);
            assert_eq!(m.2.sign.unwrap().is_minus(), true);
            assert_eq!(m.2.digits.value, "42");
            assert_eq!(m.3.is_some(), false);
            let m = real_literal_constant.parse("1234567890e-42_").unwrap();
            let m = m.0.as_starts_with_digits().unwrap();
            assert_eq!(m.0.value, "1234567890");
            assert_eq!(m.1.is_e(), true);
            assert_eq!(m.2.sign.is_some(), true);
            assert_eq!(m.2.sign.unwrap().is_minus(), true);
            assert_eq!(m.2.digits.value, "42");
            assert_eq!(m.3.is_some(), false);
            let m = real_literal_constant.parse("1234567890e-42_foo").unwrap();
            let m = m.0.as_starts_with_digits().unwrap();
            assert_eq!(m.0.value, "1234567890");
            assert_eq!(m.1.is_e(), true);
            assert_eq!(m.2.sign.is_some(), true);
            assert_eq!(m.2.sign.unwrap().is_minus(), true);
            assert_eq!(m.2.digits.value, "42");
            assert_eq!(m.3.as_ref().unwrap().as_scalar_int_constant_name().unwrap().0.value, "foo");
            let m = real_literal_constant.parse("1234567890e-42_11").unwrap();
            let m = m.0.as_starts_with_digits().unwrap();
            assert_eq!(m.0.value, "1234567890");
            assert_eq!(m.1.is_e(), true);
            assert_eq!(m.2.sign.is_some(), true);
            assert_eq!(m.2.sign.unwrap().is_minus(), true);
            assert_eq!(m.2.digits.value, "42");
            assert_eq!(m.3.as_ref().unwrap().as_digit_string().unwrap().value, "11");
            let m = real_literal_constant.parse("12345.67890").unwrap();
            let m = m.0.as_starts_with_significand().unwrap();
            assert_eq!(m.0.as_dot_after().unwrap().0.value, "12345");
            assert_eq!(m.0.as_dot_after().unwrap().1.as_ref().unwrap().value, "67890");
            let m = real_literal_constant.parse("12345.67890e+42").unwrap();
            let m = m.0.as_starts_with_significand().unwrap();
            assert_eq!(m.0.as_dot_after().unwrap().0.value, "12345");
            assert_eq!(m.0.as_dot_after().as_ref().unwrap().1.as_ref().unwrap().value, "67890");
            assert_eq!(m.1.as_ref().unwrap().0.is_e(), true);
            assert_eq!(m.1.as_ref().unwrap().1.digits.value, "42");
            let m = real_literal_constant.parse("12345.e+42").unwrap();
            let m = m.0.as_starts_with_significand().unwrap();
            assert_eq!(m.0.as_dot_after().unwrap().0.value, "12345");
            assert_eq!(m.1.as_ref().unwrap().0.is_e(), true);
            assert_eq!(m.1.as_ref().unwrap().1.digits.value, "42");
            let m = real_literal_constant.parse("12345.e+42_foo").unwrap();
            let m = m.0.as_starts_with_significand().unwrap();
            assert_eq!(m.0.as_dot_after().unwrap().0.value, "12345");
            assert_eq!(m.1.as_ref().unwrap().0.is_e(), true);
            assert_eq!(m.1.as_ref().unwrap().1.digits.value, "42");
            assert_eq!(m.2.as_ref().unwrap().as_scalar_int_constant_name().unwrap().0.value, "foo");
        }
    }

    rule_test! {
        signed_real_literal_constant(F18V007r1 713) {
            assert_eq!(signed_real_literal_constant.parses(""), false);
            assert_eq!(signed_real_literal_constant.parses("42"), false);
            assert_eq!(signed_real_literal_constant.parses("+42"), false);
            assert_eq!(signed_real_literal_constant.parses("-42"), false);
            assert_eq!(signed_real_literal_constant.parses("-1.1"), true);
            assert_eq!(signed_real_literal_constant.parses("1e1"), true);
            assert_eq!(signed_real_literal_constant.parses("1_foo"), false);
            assert_eq!(signed_real_literal_constant.parses("12345_11"), false);
            assert_eq!(signed_real_literal_constant.parses("1234567890e"), false);
            let m = signed_real_literal_constant.parse("-12345.67890e42_11").unwrap();
            assert_eq!(m.0.sign.is_some(), true);
            assert_eq!(m.0.sign.unwrap().is_minus(), true);
            let m = m.0;
            let m = m.real_literal_constant.as_starts_with_significand().unwrap();
            assert_eq!(m.0.as_dot_after().unwrap().0.value, "12345");
            assert_eq!(m.0.as_dot_after().as_ref().unwrap().1.as_ref().unwrap().value, "67890");
            assert_eq!(m.1.as_ref().unwrap().0.is_e(), true);
            assert_eq!(m.1.as_ref().unwrap().1.digits.value, "42");
            assert_eq!(m.2.as_ref().unwrap().as_digit_string().unwrap().value, "11");

            // Examples from the standard
            examples(signed_real_literal_constant, [
                "-12.78",
                "+1.6E3",
                "2.1",
                "-16.E4_8",
                "0.45D-4",
                "10.93E7_QUAD",
                ".123",
                "3E4",
            ]);
        }
    }

    rule_test! {
        significand(F18V007r1 715) {
            assert_eq!(significand.parses(""), false);
            assert_eq!(significand.parses("42"), false);
            assert_eq!(significand.parses("42.0"), true);
            assert_eq!(significand.parses("1."), true);
            assert_eq!(significand.parses("1.2"), true);
            assert_eq!(significand.parses(".2"), true);
            assert_eq!(significand.parses("."), false);
            let m = significand.parse("42.0").unwrap().0;
            let m = m.as_dot_after().unwrap();
            assert_eq!(m.0.value, "42");
            assert_eq!(m.1.as_ref().unwrap().value, "0");
            assert_eq!(significand.parses("42.0e1"), true);
            let m = significand.parse("42.0e1").unwrap().0;
            let m = m.as_dot_after().unwrap();
            assert_eq!(m.0.value, "42");
            assert_eq!(m.1.as_ref().unwrap().value, "0");
            let m = significand.parse("42.").unwrap().0;
            let m = m.as_dot_after().unwrap();
            assert_eq!(m.0.value, "42");
            assert_eq!(m.1.is_none(), true);
            let m = significand.parse(".42").unwrap().0;
            let m = m.as_dot_before().unwrap();
            assert_eq!(m.value, "42");
        }
    }

    rule_test! {
        exponent_letter(F18V007r1 716) {
            assert_eq!(exponent_letter.parses(""), false);
            assert_eq!(exponent_letter.parse("e").unwrap().0.is_e(), true);
            assert_eq!(exponent_letter.parse("E").unwrap().0.is_e(), true);
            assert_eq!(exponent_letter.parse("d").unwrap().0.is_d(), true);
            assert_eq!(exponent_letter.parse("D").unwrap().0.is_d(), true);
            assert_eq!(exponent_letter.parses("a"), false);
            assert_eq!(exponent_letter.parses("1"), false);
        }
    }

    rule_test! {
        exponent(F18V007r1 717) {
            assert_eq!(exponent.parses(""), false);
            assert_eq!(exponent.parses("42"), true);
            assert_eq!(exponent.parses("+42"), true);
            assert_eq!(exponent.parses("-42"), true);
            assert_eq!(exponent.parses("-"), false);
        }
    }

    rule_test! {
        boz_literal_constant(F18V007r1 764) {
            assert!(boz_literal_constant.parse("b'1010'").unwrap().0.is_binary());
            assert!(boz_literal_constant.parse("o'123'").unwrap().0.is_octal());
            assert!(boz_literal_constant.parse("z'ABC'").unwrap().0.is_hex());
        }
    }

    rule_test! {
        binary_constant(F18V007r1 765) {
            assert_eq!(binary_constant.parses(""), false);
            assert_eq!(binary_constant.parses("B"), false);
            assert_eq!(binary_constant.parses("B101"), false);
            assert_eq!(binary_constant.parses("b'101"), true);
            assert_eq!(binary_constant.parses("B\"101\""), true);
            assert_eq!(binary_constant.parses("B\"141"), true);
        }
    }

    rule_test! {
        octal_constant(F18V007r1 766) {
            assert_eq!(octal_constant.parses(""), false);
            assert_eq!(octal_constant.parses("O"), false);
            assert_eq!(octal_constant.parses("O123"), false);
            assert_eq!(octal_constant.parses("o'123"), true);
            assert_eq!(octal_constant.parses("O\"123\""), true);
            assert_eq!(octal_constant.parses("O\"123"), true);
        }
    }

    rule_test! {
        hex_constant(F18V007r1 767) {
            assert_eq!(hex_constant.parses(""), false);
            assert_eq!(hex_constant.parses("Z"), false);
            assert_eq!(hex_constant.parses("ZABC"), false);
            assert_eq!(hex_constant.parses("z'ABC"), true);
            assert_eq!(hex_constant.parses("Z\"ABC\""), true);
            assert_eq!(hex_constant.parses("Z\"AB1C"), true);
            let m = hex_constant.parse("Z\"AB1C").unwrap();
            let m = m.0;
            assert_eq!(m.value, "AB1C");
        }
    }

    rule_test! {
        hex_digit(F18V007r1 768) {
            assert_eq!(hex_digit.parses(""), false);
            assert_eq!(hex_digit.parses("A"), true);
            assert_eq!(hex_digit.parses("B"), true);
            assert_eq!(hex_digit.parses("C"), true);
            assert_eq!(hex_digit.parses("D"), true);
            assert_eq!(hex_digit.parses("e"), true);
            assert_eq!(hex_digit.parses("F"), true);
            assert_eq!(hex_digit.parses("G"), false);
            assert_eq!(hex_digit.parses("1"), true);
        }
    }
}
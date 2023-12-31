use super::*;

#[syntax_rule(
    F18V007r1 rule "digit-string" #711,
)]
pub fn digit_string<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = StringMatch<S::Span>> + 'a {
    digit(cfg)
        .then(|first| fold_many(
            digit(cfg),
            move || StringMatch::from_char(first.clone()),
            |mut string, digit| {
                string.push_char::<S>(digit);
                (string, true)
            },
            0..,
        ))
}

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

#[syntax_rule(
    F18V007r1 rule "sign" #712,
)]
pub fn sign<S: TextSource>(cfg: &Cfg) -> impl Parser<S, Token = Sign<S::Span>> {
    Char::exact('+')
        .or(Char::exact('-'))
        .map(|c| match &c.value {
            '+' => Sign::Plus(c.span),
            '-' => Sign::Minus(c.span),
            _ => unreachable!(),
        })
}

#[derive(Debug, Clone)]
pub struct SignedDigitString<Span> {
    pub span: Span,
    pub sign: Option<Sign<Span>>,
    pub digits: StringMatch<Span>,
}

#[syntax_rule(
    F18V007r1 rule "signed-digit-string" #710,
)]
pub fn signed_digit_string<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = SignedDigitString<S::Span>> + 'a {
    (
        sign::<S>(cfg).optional(),
        digit_string(cfg),
    )
        .map(|(sign, digits)| {
            let span = if let Some(sign) = &sign {
                S::merge_span(sign.span().clone(), digits.span.clone())
            } else {
                digits.span.clone()
            };

            SignedDigitString {
                span,
                sign,
                digits,
            }
        })
}

/// As defined in J3/18-007r1 §7.4.3.1 R709
#[derive(Debug, Clone, EnumAsInner)]
pub enum KindParam<Span> {
    DigitString(StringMatch<Span>),
    ScalarIntConstantName(Name<Span>),
}

#[syntax_rule(
    F18V007r1 rule "kind-param" #709,
)]
pub fn kind_param<'a, S: TextSource + 'a>(cfg: &'a Cfg, drop_last_underscore: bool) -> impl Parser<S, Token = KindParam<S::Span>> + 'a {
    // TODO implement J3/18-007r1 §7.4.3.1 C713
    // This might be a temporary implementation
    let scalar_int_constant_name = name;

    alt!(
        digit_string(cfg).map(KindParam::DigitString),
        scalar_int_constant_name(cfg, drop_last_underscore).map(KindParam::ScalarIntConstantName),
    )
}

#[derive(Debug, Clone)]
pub struct IntLiteralConstant<Span> {
    pub span: Span,
    pub digits: StringMatch<Span>,
    pub kind_param: Option<KindParam<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "int-literal-constant" #708,
)]
pub fn int_literal_constant<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = IntLiteralConstant<S::Span>> + 'a {
    // TODO maybe relax R708 to allow for trailing underscores if a kind param is missing:
    //`digit-string [ _ kind-param ]` --> `digit-string [ _ ]  [ kind-param ]`

    (
        digit_string::<S>(cfg),
        (
            underscore(cfg),
            kind_param(cfg, false),
        )
            .map(|(_, kind_param)| kind_param)
            .optional(),
    ).map(|(digits, kind_param)| {

        let span = if let Some(kind_param) = &kind_param {
            S::merge_span(
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
}

#[derive(Debug, Clone)]
pub struct SignedIntLiteralConstant<Span> {
    // TODO span
    pub sign: Option<Sign<Span>>,
    pub int_literal_constant: IntLiteralConstant<Span>,
}

#[syntax_rule(
    F18V007r1 rule "signed-int-literal-constant" #707,
)]
pub fn signed_int_literal_constant<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = SignedIntLiteralConstant<S::Span>> + 'a {
    (
        sign(cfg).optional(),
        int_literal_constant(cfg),
    )
        .map(|(sign, int_literal_constant)| SignedIntLiteralConstant {
            sign,
            int_literal_constant,
        })
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ExponentLetter<Span> {
    D(Span),
    E(Span),
}

#[syntax_rule(
    F18V007r1 rule "exponent-letter" #716,
)]
pub fn exponent_letter<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ExponentLetter<S::Span>> + 'a {
    alt!(
        Char::exact_case_insensitive('d').map(|c| ExponentLetter::D(c.span)),
        Char::exact_case_insensitive('e').map(|c| ExponentLetter::E(c.span)),
    )
}

#[syntax_rule(
    F18V007r1 rule "exponent" #717,
)]
pub fn exponent<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = SignedDigitString<S::Span>> + 'a {
    signed_digit_string(cfg)
}

/// As defined in J3/18-007r1 §7.4.3.2 R715
#[derive(Debug, Clone, EnumAsInner)]
pub enum Significand<Span> {
    // TODO span
    DotAfter(StringMatch<Span>, Option<StringMatch<Span>>), // digit-string . [ digit-string ]
    DotBefore(StringMatch<Span>), // . digit-string
}

#[syntax_rule(
    F18V007r1 rule "significand" #715,
)]
pub fn significand<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = Significand<S::Span>> + 'a {
    alt! {
        (
            digit_string(cfg),
            '.',
            digit_string(cfg).optional(),
        ).map(|(first, _, second)| Significand::DotAfter(first, second)),
        (
            '.',
            digit_string(cfg),
        ).map(|(_, second)| Significand::DotBefore(second)),
    }
}

/// As defined in J3/18-007r1 §7.4.3.2 R714
#[derive(Debug, Clone, EnumAsInner)]
pub enum RealLiteralConstant<Span> {
    StartsWithSignificand {
        significand: Significand<Span>,
        exponent_letter_and_exponent: Option<(ExponentLetter<Span>, SignedDigitString<Span>)>,
        kind_param: Option<KindParam<Span>>,
    },
    StartsWithDigits {
        digits_string: StringMatch<Span>,
        exponent_letter: ExponentLetter<Span>,
        exponent: SignedDigitString<Span>,
        kind_param: Option<KindParam<Span>>,
    }
}

#[syntax_rule(
    F18V007r1 rule "real-literal-constant" #714,
)]
pub fn real_literal_constant<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = RealLiteralConstant<S::Span>> + 'a {
    // TODO implement §7.4.3.2 C716

    alt!(
        (
            significand(cfg),
            (
                exponent_letter(cfg),
                exponent(cfg),
            ).optional(),
            (
                underscore(cfg),
                kind_param(cfg, false),
            ).map(|(_, k)| k).optional(),
        )
            .map(|(significand, exponent_letter_and_exponent, kind_param)| RealLiteralConstant::StartsWithSignificand {
                significand,
                exponent_letter_and_exponent,
                kind_param,
            }),
        (
            digit_string(cfg),
            exponent_letter(cfg),
            exponent(cfg),
            (
                underscore(cfg),
                kind_param(cfg, false),
            ).map(|(_, k)| k).optional(),
        )
            .map(|(digits_string, exponent_letter, exponent, kind_param)| RealLiteralConstant::StartsWithDigits {
                digits_string,
                exponent_letter,
                exponent,
                kind_param,
            }),
    )
}

/// As defined in J3/18-007r1 §7.4.3.2 R713
#[derive(Debug, Clone)]
pub struct SignedRealLiteralConstant<Span> {
    pub sign: Option<Sign<Span>>,
    pub real_literal_constant: RealLiteralConstant<Span>,
}

#[syntax_rule(
    F18V007r1 rule "signed-real-literal-constant" #713,
)]
pub fn signed_real_literal_constant<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = SignedRealLiteralConstant<S::Span>> + 'a {
    (
        sign(cfg).optional(),
        real_literal_constant(cfg),
    )
        .map(|(sign, real_literal_constant)| SignedRealLiteralConstant {
            sign,
            real_literal_constant,
        })
}

/// As defined in J3/18-007r1 §7.4.3.3 R719 and R720
#[derive(Debug, Clone, EnumAsInner)]
pub enum ComplexPart<Span> {
    Int(SignedIntLiteralConstant<Span>),
    Real(SignedRealLiteralConstant<Span>),
    Name(Name<Span>),
}

#[syntax_rule(
    F18V007r1 rule "real-part" #719,
    F18V007r1 rule "imag-part" #720,
)]
pub fn complex_part<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ComplexPart<S::Span>> + 'a {
    // Note that the order is different from the standard to avoid ambiguity
    alt! {
        signed_real_literal_constant(cfg).map(ComplexPart::Real),
        signed_int_literal_constant(cfg).map(ComplexPart::Int),
        name(cfg, false).map(ComplexPart::Name),
    }
}

#[derive(Debug, Clone)]
pub struct ComplexLiteralConstant<Span> {
    pub real_part: ComplexPart<Span>,
    pub imaginary_part: ComplexPart<Span>,
}

#[syntax_rule(
    F18V007r1 rule "complex-literal-constant" #718,
)]
pub fn complex_literal_constant<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ComplexLiteralConstant<S::Span>> + 'a {
    (
        '(', space(0),
        complex_part(cfg),
        space(0), ',', space(0),
        complex_part(cfg),
        space(0), ')',
    )
        .map(|(_, _, real_part, _, _, _, imaginary_part, _, _)| ComplexLiteralConstant {
            real_part,
            imaginary_part,
        })
}

#[derive(Debug, Clone)]
pub enum StringElement<Span> {
    Char(Char<Span>),
    EscapeSequence(StringMatch<Span>, &'static str),
}

impl<Span> StringElement<Span> {
    pub fn value(&self) -> String {
        match self {
            Self::Char(c) => c.value.to_string(),
            Self::EscapeSequence(s, _) => s.value.clone(),
        }
    }
}

pub fn string_element<'a, S: TextSource + 'a>(termination: char, escape: &'static str, into: &'static str) -> impl Parser<S, Token = StringElement<S::Span>> + 'a {
    alt!(
        StringMatch::exact(escape, true).map(|s| StringElement::EscapeSequence(s, into)),
        Char::parse(|c| c != termination).map(|c| StringElement::Char(c)),
    )
}

#[derive(Debug, Clone)]
pub struct CharLiteralConstant<Span> {
    pub kind_param: Option<KindParam<Span>>,
    pub delimiter: char,
    pub content: Vec<StringElement<Span>>,
}

impl<Span> CharLiteralConstant<Span> {
    pub fn value(&self) -> String {
        self.content.iter().map(|e| e.value()).collect()
    }
}

#[syntax_rule(
    F18V007r1 rule "char-literal-constant" #724,
)]
pub fn char_literal_constant<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = CharLiteralConstant<S::Span>> + 'a {
    alt! {
        (
            (kind_param(cfg, true), underscore(cfg)).map(|(k, _)| k).optional(),
            Char::exact('\''),
            fold_many(
                string_element('\'', "''", "'"),
                || Vec::new(),
                |mut content, element| {
                    content.push(element);
                    (content, true)
                },
                0..,
            ),
            Char::exact('\''),
        )
            .map(|(kind_param, _, content, _)| CharLiteralConstant {
                kind_param,
                delimiter: '\'',
                content,
            }),
        (
            (kind_param(cfg, true), underscore(cfg)).map(|(k, _)| k).optional(),
            Char::exact('"'),
            fold_many(
                string_element('"', "\"\"", "\""),
                || Vec::new(),
                |mut content, element| {
                    content.push(element);
                    (content, true)
                },
                0..,
            ),
            Char::exact('"'),
        )
            .map(|(kind_param, _, content, _)| CharLiteralConstant {
                kind_param,
                delimiter: '"',
                content,
            }),
    }
}

#[derive(Debug, Clone)]
pub struct LogicalLiteralConstant<Span> {
    pub value: bool,
    pub kind: KindParam<Span>,
}

#[syntax_rule(
    F18V007r1 rule "logical-literal-constant" #725,
)]
pub fn logical_literal_constant<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = LogicalLiteralConstant<S::Span>> + 'a {
    alt! {
        StringMatch::exact(".TRUE.", false),
        StringMatch::exact(".FALSE.", false),
    }.map(|s| LogicalLiteralConstant {
        value: match s.value.to_uppercase().as_str() {
            ".TRUE." => true,
            ".FALSE." => false,
            _ => unreachable!(),
        },
        kind: KindParam::DigitString(s),
    })
}

#[syntax_rule(
    F18V007r1 rule "hex-digit" #768,
)]
pub fn hex_digit<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = Char<S::Span>> + 'a {
    alt!(
        digit(cfg),
        Char::any_of("abcdefABCDEF".chars()),
    )
}

#[syntax_rule(
    F18V007r1 rule "hex-constant" #767,
)]
pub fn hex_constant<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = StringMatch<S::Span>> + 'a {
    let hex_digits = || fold_many(
        hex_digit(cfg),
        || StringMatch::empty::<S>(),
        |mut string, digit| {
            string.push_char::<S>(digit);
            (string, true)
        },
        1..,
    );

    alt! {
        (
            Char::any_of("zZ".chars()),
            Char::exact('\''),
            hex_digits(),
            Char::exact('\''),
        ),
        (
            Char::any_of("zZ".chars()),
            Char::exact('"'),
            hex_digits(),
            Char::exact('"'),
        ),
    }.map(|(_, _, digits, _)| digits)
}

#[syntax_rule(
    F18V007r1 rule "octal-constant" #766,
)]
pub fn octal_constant<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = StringMatch<S::Span>> + 'a {
    let octal_digits = || fold_many(
        Char::any_of("01234567".chars()),
        || StringMatch::empty::<S>(),
        |mut string, digit| {
            string.push_char::<S>(digit);
            (string, true)
        },
        1..,
    );

    alt! {
        (
            Char::any_of("oO".chars()),
            Char::exact('\''),
            octal_digits(),
            Char::exact('\''),
        ),
        (
            Char::any_of("oO".chars()),
            Char::exact('"'),
            octal_digits(),
            Char::exact('"'),
        ),
    }.map(|(_, _, digits, _)| digits)
}

#[syntax_rule(
    F18V007r1 rule "binary-constant" #765,
)]
pub fn binary_constant<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = StringMatch<S::Span>> + 'a {
    let binary_digits = || fold_many(
        Char::any_of("01".chars()),
        || StringMatch::empty::<S>(),
        |mut string, digit| {
            string.push_char::<S>(digit);
            (string, true)
        },
        1..,
    );

    alt! {
        (
            Char::any_of("bB".chars()),
            Char::exact('\''),
            binary_digits(),
            Char::exact('\''),
        ),
        (
            Char::any_of("bB".chars()),
            Char::exact('"'),
            binary_digits(),
            Char::exact('"'),
        ),
    }.map(|(_, _, digits, _)| digits)
}

/// Binary, octal, and hexadecimal literal constant
#[derive(Debug, Clone, EnumAsInner)]
pub enum BozLiteralConstant<Span> {
    Binary(StringMatch<Span>),
    Octal(StringMatch<Span>),
    Hex(StringMatch<Span>),
}

/// Binary, octal, and hexadecimal literal constant
#[syntax_rule(
    F18V007r1 rule "boz-literal-constant" #764,
)]
pub fn boz_literal_constant<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = BozLiteralConstant<S::Span>> + 'a {
    alt!(
        binary_constant(cfg).map(BozLiteralConstant::Binary),
        octal_constant(cfg).map(BozLiteralConstant::Octal),
        hex_constant(cfg).map(BozLiteralConstant::Hex),
    )
}

#[cfg(test)]
mod test {

    use super::*;

    #[test]
    fn test_digit_string() {
        for cfg in test_configs() {
            let parser = digit_string(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parses(" 1"), false);
            assert_eq!(parser.parses("a"), false);
            assert_eq!(parser.parse("1").unwrap().0.value(), "1");
            assert_eq!(parser.parse("123").unwrap().0.value(), "123");
            assert_eq!(parser.parse("123a").unwrap().0.value(), "123");
            assert_eq!(parser.parse("123 ").unwrap().0.value(), "123");
        }
    }

    #[test]
    fn test_sign() {
        for cfg in test_configs() {
            let parser = sign(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parses(" +"), false);
            assert_eq!(parser.parses(" -"), false);
            assert!(parser.parse("+").unwrap().0.is_plus());
            assert_eq!(parser.parse("+ ").unwrap().0.is_plus(), true);
            assert_eq!(parser.parse("-").unwrap().0.is_minus(), true);
            assert_eq!(parser.parse("- ").unwrap().0.is_minus(), true);
            assert_eq!(parser.parse("+1").unwrap().0.is_plus(), true);
            assert_eq!(parser.parse("-1").unwrap().0.is_minus(), true);
        }
    }

    #[test]
    fn test_signed_digit_string() {
        for cfg in test_configs() {
            let parser = signed_digit_string(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parses(" +"), false);
            assert_eq!(parser.parses(" -"), false);
            assert_eq!(parser.parses(" 1"), false);
            assert_eq!(parser.parses("+ "), false);
            assert_eq!(parser.parses("+ "), false);
            assert_eq!(parser.parses("- "), false);
            assert_eq!(parser.parses("- "), false);
            assert!(parser.parse("+1 ").unwrap().0.sign.unwrap().is_plus());
            assert_eq!(parser.parse("+1 ").unwrap().0.digits.value(), "1");
            assert!(parser.parse("-1 ").unwrap().0.sign.unwrap().is_minus());
            assert_eq!(parser.parse("-1 ").unwrap().0.digits.value(), "1");
            assert!(parser.parse("+1234 ").unwrap().0.sign.unwrap().is_plus());
            assert_eq!(parser.parse("+1234 ").unwrap().0.digits.value(), "1234");
        }
    }

    #[test]
    fn test_kind_param() {
        for cfg in test_configs() {
            let parser = kind_param(&cfg, false);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parses(" 1"), false);
            assert_eq!(parser.parses(" a"), false);
            assert_eq!(parser.parse("1").unwrap().0.as_digit_string().unwrap().value(), "1");
            assert_eq!(parser.parse("123").unwrap().0.as_digit_string().unwrap().value(), "123");
            assert_eq!(parser.parse("123a").unwrap().0.as_digit_string().unwrap().value(), "123");
            assert_eq!(parser.parse("123 ").unwrap().0.as_digit_string().unwrap().value(), "123");
            assert_eq!(parser.parses("+"), false);
            assert_eq!(parser.parse("a").unwrap().0.as_scalar_int_constant_name().unwrap().0.value(), "a");
            assert_eq!(parser.parse("a ").unwrap().0.as_scalar_int_constant_name().unwrap().0.value(), "a");
            assert_eq!(parser.parse("a1").unwrap().0.as_scalar_int_constant_name().unwrap().0.value(), "a1");
            assert_eq!(parser.parse("a1 ").unwrap().0.as_scalar_int_constant_name().unwrap().0.value(), "a1");
        }
    }

    #[test]
    fn test_int_literal_constant() {
        for cfg in test_configs() {
            let parser = int_literal_constant(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parses(" 1"), false);
            assert_eq!(parser.parses(" a"), false);
            assert_eq!(parser.parses("a"), false);
            assert_eq!(parser.parses("_"), false);
            assert_eq!(parser.parses("_a"), false);
            assert_eq!(parser.parses("_1"), false);
            assert_eq!(parser.parse("1_").unwrap().0.digits.value(), "1");
            assert!(parser.parse("1_").unwrap().0.kind_param.is_none());
            assert_eq!(parser.parse("1_2").unwrap().0.digits.value(), "1");
            assert_eq!(parser.parse("1_2").unwrap().0.kind_param.unwrap().as_digit_string().unwrap().value(), "2");
            assert_eq!(parser.parse("1_a").unwrap().0.digits.value(), "1");
            assert_eq!(parser.parse("1_a").unwrap().0.kind_param.unwrap().as_scalar_int_constant_name().unwrap().0.value(), "a");
        }
    }

    #[test]
    fn test_signed_int_literal_constant() {
        for cfg in test_configs() {
            let parser = signed_int_literal_constant(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parse("1").unwrap().0.sign, None);
            assert!(parser.parse("+1").unwrap().0.sign.unwrap().is_plus());
            assert!(parser.parse("-1").unwrap().0.sign.unwrap().is_minus());
            assert_eq!(parser.parses("+1_a"), true);
        }
    }

    #[test]
    fn examples_in_18_007r1_section_7_4_3_1() {
        for cfg in test_configs() {
            let parser = signed_int_literal_constant(&cfg);
            assert_eq!(parser.parse("473 ").unwrap().1, " ");
            assert_eq!(parser.parse("+56 ").unwrap().1, " ");
            assert_eq!(parser.parse("-101 ").unwrap().1, " ");
            assert_eq!(parser.parse("21_2 ").unwrap().1, " ");
            assert_eq!(parser.parse("21_SHORT ").unwrap().1, " ");
            assert_eq!(parser.parse("1976354279568241_8 ").unwrap().1, " ");
        }
    }

    #[test]
    fn test_exponent_letter() {
        for cfg in test_configs() {
            let parser = exponent_letter(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parses("a"), false);
            assert_eq!(parser.parses("1"), false);
            assert!(parser.parse("d").unwrap().0.is_d());
            assert!(parser.parse("D").unwrap().0.is_d());
            assert!(parser.parse("e").unwrap().0.is_e());
            assert!(parser.parse("E").unwrap().0.is_e());
        }
    }

    #[test]
    fn test_exponent() {
        for cfg in test_configs() {
            let parser = exponent(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parses("a"), false);
            assert_eq!(parser.parses("1"), true);
            assert_eq!(parser.parses("d"), false);
            assert_eq!(parser.parses("D"), false);
            assert_eq!(parser.parses("e"), false);
            assert_eq!(parser.parses("E"), false);
            assert_eq!(parser.parse("+123_").unwrap().1, "_");
        }
    }

    #[test]
    fn test_significand() {
        for cfg in test_configs() {
            let parser = significand(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parses("+1"), false);
            assert_eq!(parser.parses("1"), false);
            assert_eq!(parser.parses("1."), true);
            assert_eq!(parser.parses("+1."), false);
            assert_eq!(parser.parses("1.1"), true);
            assert_eq!(parser.parses(".1"), true);
            assert_eq!(parser.parses("+.1"), false);
        }
    }

    #[test]
    fn test_real_literal_constant() {
        for cfg in test_configs() {
            let parser = real_literal_constant(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parses("1"), false);
            assert_eq!(parser.parses("1."), true);
            assert_eq!(parser.parses("1.1"), true);
            assert_eq!(parser.parses("1.1e3"), true);
            assert_eq!(parser.parses("1.e3"), true);
            assert_eq!(parser.parses("1.e+3"), true);
            assert_eq!(parser.parses("1.e-3"), true);
            assert_eq!(parser.parses("1.d-3"), true);
            assert_eq!(parser.parses("1.D-3"), true);
            assert_eq!(parser.parses("+1.D-3"), false);
        }
    }

    #[test]
    fn test_signed_real_literal_constant() {
        for cfg in test_configs() {
            let parser = signed_real_literal_constant(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parses("1"), false);
            assert_eq!(parser.parses("1."), true);
            assert_eq!(parser.parses("1.1"), true);
            assert_eq!(parser.parses("1.1e3"), true);
            assert_eq!(parser.parses("1.e3"), true);
            assert_eq!(parser.parses("1.e+3"), true);
            assert_eq!(parser.parses("1.e-3"), true);
            assert_eq!(parser.parses("1.d-3"), true);
            assert_eq!(parser.parses("1.D-3"), true);
            assert_eq!(parser.parses("+1.D-3"), true);
        }
    }

    #[test]
    fn examples_in_18_007r1_section_7_4_3_2() {
        for cfg in test_configs() {
            let parser = signed_real_literal_constant(&cfg);
            assert_eq!(parser.parse("-12.78 ").unwrap().1, " ");
            assert_eq!(parser.parse("+1.6E3 ").unwrap().1, " ");
            assert_eq!(parser.parse("2.1 ").unwrap().1, " ");
            assert_eq!(parser.parse("-16.E4_8 ").unwrap().1, " ");
            assert_eq!(parser.parse("0.45D-4 ").unwrap().1, " ");
            assert_eq!(parser.parse("10.93E7_QUAD ").unwrap().1, " ");
            assert_eq!(parser.parse(".123 ").unwrap().1, " ");
            assert_eq!(parser.parse("3E4 ").unwrap().1, " ");
        }
    }

    #[test]
    fn test_complex_part() {
        for cfg in test_configs() {
            let parser = complex_part(&cfg);
            assert_eq!(parser.parses(""), false);
            assert!(parser.parse("1").unwrap().0.is_int());
            assert!(parser.parse("+1").unwrap().0.is_int());
            assert!(parser.parse("1_a").unwrap().0.is_int());
            assert!(parser.parse("1.").unwrap().0.is_real());
            assert!(parser.parse("1e3").unwrap().0.is_real());
            assert!(parser.parse("1.1").unwrap().0.is_real());
            assert!(parser.parse("+1.1e3_a").unwrap().0.is_real());
            assert!(parser.parse("a").unwrap().0.is_name());
            assert!(parser.parse("abc").unwrap().0.is_name());
        }
    }

    #[test]
    fn test_complex_literal_constant() {
        for cfg in test_configs() {
            let parser = complex_literal_constant(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parses("1"), false);
            assert_eq!(parser.parses("()"), false);
            assert_eq!(parser.parses("(a)"), false);
            assert_eq!(parser.parses("(a,b)"), true);
            assert_eq!(parser.parses("(a,b,c)"), false);
            assert_eq!(parser.parses("( a , b )"), true);
            assert_eq!(parser.parses("( +1.3 , b )"), true);
            assert_eq!(parser.parses("( +1.3e-2 , b )"), true);
        }
    }

    #[test]
    fn examples_in_18_007r1_section_7_4_3_3() {
        for cfg in test_configs() {
            let parser = complex_literal_constant(&cfg);
            assert_eq!(parser.parse("(1.0, -1.0) ").unwrap().1, " ");
            assert_eq!(parser.parse("(3, 3.1E6) ").unwrap().1, " ");
            assert_eq!(parser.parse("(4.0_4, 3.6E7_8) ").unwrap().1, " ");
            assert_eq!(parser.parse("( 0., PI) ").unwrap().1, " ");
        }
    }

    #[test]
    fn test_char_literal_constant() {
        for cfg in test_configs() {
            let parser = char_literal_constant(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parse(r##""ciao" "##).unwrap().1, " ");
            assert_eq!(parser.parse(r##""ci'ao" "##).unwrap().1, " ");
            assert_eq!(parser.parse(r##""ci""ao" "##).unwrap().1, " ");
            assert_eq!(parser.parse(r##"'ciao' "##).unwrap().1, " ");
            assert_eq!(parser.parse(r##"'ci"ao' "##).unwrap().1, " ");
            assert_eq!(parser.parse(r##"'ci''ao' "##).unwrap().1, " ");
            assert_eq!(parser.parse(r##"10_"ciao" "##).unwrap().1, " ");
            assert_eq!(parser.parse(r##"abc_"ciao" "##).unwrap().1, " ");
        }
    }

    #[test]
    fn test_logical_literal_constant() {
        for cfg in test_configs() {
            let parser = logical_literal_constant(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parse(".true. ").unwrap().1, " ");
            assert_eq!(parser.parse(".true.").unwrap().0.value, true);
            assert_eq!(parser.parse(".TRUE.").unwrap().0.value, true);
            assert_eq!(parser.parse(".TrUe.").unwrap().0.value, true);
            assert_eq!(parser.parse(".false.").unwrap().0.value, false);
            assert_eq!(parser.parse(".FALSE.").unwrap().0.value, false);
            assert_eq!(parser.parse(".FaLsE.").unwrap().0.value, false);
        }
    }

    #[test]
    fn test_hex_digit() {
        for cfg in test_configs() {
            let parser = hex_digit(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parses("g"), false);
            assert_eq!(parser.parses("G"), false);
            assert_eq!(parser.parses("a"), true);
            assert_eq!(parser.parses("A"), true);
            assert_eq!(parser.parses("f"), true);
            assert_eq!(parser.parses("F"), true);
            assert_eq!(parser.parses("0"), true);
            assert_eq!(parser.parses("9"), true);
        }
    }

    #[test]
    fn test_hex_constant() {
        for cfg in test_configs() {
            let parser = hex_constant(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parses("z"), false);
            assert_eq!(parser.parses("'1'"), false);
            assert_eq!(parser.parse("z'123abcdef'").unwrap().0.value(), "123abcdef");
            assert_eq!(parser.parse("z\"123abcdef\"").unwrap().0.value(), "123abcdef");
            assert_eq!(parser.parse("Z'123abcDef'").unwrap().0.value(), "123abcDef");
        }
    }

    #[test]
    fn test_octal_constant() {
        for cfg in test_configs() {
            let parser = octal_constant(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parses("o"), false);
            assert_eq!(parser.parses("'1'"), false);
            assert_eq!(parser.parse("o'1234567'").unwrap().0.value(), "1234567");
            assert_eq!(parser.parse("o\"1234567\"").unwrap().0.value(), "1234567");
            assert_eq!(parser.parse("O'1234567'").unwrap().0.value(), "1234567");
        }
    }

    #[test]
    fn test_binary_constant() {
        for cfg in test_configs() {
            let parser = binary_constant(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parses("b"), false);
            assert_eq!(parser.parses("'1'"), false);
            assert_eq!(parser.parse("b'1010101'").unwrap().0.value(), "1010101");
            assert_eq!(parser.parse("b\"1010101\"").unwrap().0.value(), "1010101");
            assert_eq!(parser.parse("B'1010101'").unwrap().0.value(), "1010101");
        }
    }

    #[test]
    fn test_boz_literal_constant() {
        for cfg in test_configs() {
            let parser = boz_literal_constant(&cfg);
            assert!(parser.parse("b'1010101'").unwrap().0.is_binary());
            assert!(parser.parse("o'1234567'").unwrap().0.is_octal());
            assert!(parser.parse("z'123abcdef'").unwrap().0.is_hex());
            assert!(parser.parse("B\"1010101\"").unwrap().0.is_binary());
            assert!(parser.parse("O\"1234567\"").unwrap().0.is_octal());
            assert!(parser.parse("Z\"123abcdef\"").unwrap().0.is_hex());
        }
    }
}
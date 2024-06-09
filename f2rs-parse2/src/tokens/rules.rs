use std::ops::RangeBounds;

use f2rs_parser_combinator::prelude::*;

use crate::s_rule;
use super::*;

/// Fortran special character
///
/// Conforms to:
/// - J3/18-007r1§6.1.5
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, EnumAsInner)]
pub enum SpecialCharacter {
    /// ` ` (blank)
    Blank,

    Tab, // NOTE: added for convenience

    /// `=` (equals sign)
    Equals,
    /// `+` (plus sign)
    Plus,
    /// `-` (minus sign)
    Minus,
    /// `*` (asterisk)
    Asterisk,
    /// `/` (slash)
    Slash,
    /// `\` (backslash)
    Backslash,
    /// `(` (left parenthesis)
    LeftParenthesis,
    /// `)` (right parenthesis)
    RightParenthesis,
    /// `[` (left square bracket)
    LeftSquareBracket,
    /// `]` (right square bracket)
    RightSquareBracket,
    /// `{` (left curly bracket)
    LeftCurlyBracket,
    /// `}` (right curly bracket)
    RightCurlyBracket,
    /// `,` (comma)
    Comma,
    /// `.` (decimal point or period)
    DecimalPointOrPeriod,
    /// `:` (colon)
    Colon,
    /// `;` (semicolon)
    SemiColon,
    /// `!` (exclamation point)
    ExclamationPoint,
    /// `"` (quotation mark or quote)
    QuotationMarkOrQuote,
    /// `%` (percent sign)
    Percent,
    /// `&` (ampersand)
    Ampersand,
    /// `~` (tilde)
    Tilde,
    /// `<` (less-than sign)
    LessThan,
    /// `>` (greater-than sign)
    GreaterThan,
    /// `?` (question mark)
    QuestionMark,
    /// `'` (apostrophe)
    Apostrophe,
    /// `` ` `` (grave accent)
    GraveAccent,
    /// `^` (circumflex accent)
    CircumflexAccent,
    /// `|` (vertical line)
    VerticalLine,
    /// `¤` (currency symbol)
    CurrencySymbol,
    /// `#` (number sign)
    NumberSign,
    /// `@` (commercial at)
    CommercialAt,
}

impl SpecialCharacter {
    const ALL: &'static [Self] = &[
        Self::Blank,
        Self::Tab,
        Self::Equals,
        Self::Plus,
        Self::Minus,
        Self::Asterisk,
        Self::Slash,
        Self::Backslash,
        Self::LeftParenthesis,
        Self::RightParenthesis,
        Self::LeftSquareBracket,
        Self::RightSquareBracket,
        Self::LeftCurlyBracket,
        Self::RightCurlyBracket,
        Self::Comma,
        Self::DecimalPointOrPeriod,
        Self::Colon,
        Self::SemiColon,
        Self::ExclamationPoint,
        Self::QuotationMarkOrQuote,
        Self::Percent,
        Self::Ampersand,
        Self::Tilde,
        Self::LessThan,
        Self::GreaterThan,
        Self::QuestionMark,
        Self::Apostrophe,
        Self::GraveAccent,
        Self::CircumflexAccent,
        Self::VerticalLine,
        Self::CurrencySymbol,
        Self::NumberSign,
        Self::CommercialAt,
    ];

    pub fn character(&self) -> char {
        use SpecialCharacter::*;
        match self {
            // TODO to check
            Blank => ' ',
            Tab => '\t',
            Equals => '=',
            Plus => '+',
            Minus => '-',
            Asterisk => '*',
            Slash => '/',
            Backslash => '\\',
            LeftParenthesis => '(',
            RightParenthesis => ')',
            LeftSquareBracket => '[',
            RightSquareBracket => ']',
            LeftCurlyBracket => '{',
            RightCurlyBracket => '}',
            Comma => ',',
            DecimalPointOrPeriod => '.',
            Colon => ':',
            SemiColon => ';',
            ExclamationPoint => '!',
            QuotationMarkOrQuote => '"',
            Percent => '%',
            Ampersand => '&',
            Tilde => '~',
            LessThan => '<',
            GreaterThan => '>',
            QuestionMark => '?',
            Apostrophe => '\'',
            GraveAccent => '`',
            CircumflexAccent => '^',
            VerticalLine => '|',
            CurrencySymbol => '¤',
            NumberSign => '#',
            CommercialAt => '@',
        }
    }

    pub fn from_char(c: char) -> Option<Self> {
        use SpecialCharacter::*;
        Some(match c {
            ' ' => Blank,
            '\t' => Tab,
            '=' => Equals,
            '+' => Plus,
            '-' => Minus,
            '*' => Asterisk,
            '/' => Slash,
            '\\' => Backslash,
            '(' => LeftParenthesis,
            ')' => RightParenthesis,
            '[' => LeftSquareBracket,
            ']' => RightSquareBracket,
            '{' => LeftCurlyBracket,
            '}' => RightCurlyBracket,
            ',' => Comma,
            '.' => DecimalPointOrPeriod,
            ':' => Colon,
            ';' => SemiColon,
            '!' => ExclamationPoint,
            '"' => QuotationMarkOrQuote,
            '%' => Percent,
            '&' => Ampersand,
            '~' => Tilde,
            '<' => LessThan,
            '>' => GreaterThan,
            '?' => QuestionMark,
            '\'' => Apostrophe,
            '`' => GraveAccent,
            '^' => CircumflexAccent,
            '|' => VerticalLine,
            '¤' => CurrencySymbol,
            '#' => NumberSign,
            '@' => CommercialAt,
            _ => return None,
        })
    }
}

impl From<char> for SpecialCharacter {
    fn from(c: char) -> Self {
        Self::from_char(c).expect("Not a special character")
    }
}

impl<S: TextSource> ParserCore<S> for SpecialCharacter {
    type Token = SpecialCharacterMatch<S::Span>;
    fn parse(&self, source: S) -> PResult<Self::Token, S> {
        Char::exact(self.character())
            .map(|c| {
                assert_eq!(c.value, self.character());
                SpecialCharacterMatch {
                    character: *self,
                    span: c.span,
                }
            })
            .parse(source)
    }
}

pub fn blanks<S: TextSource>(range: impl RangeBounds<usize> + Clone + 'static) -> impl Parser<S, Token = ()> {
    fold_many(
        Parser::<S>::or(SpecialCharacter::Blank, SpecialCharacter::Tab), // NOTE: added Tab for convenience
        || (),
        |_, _| ((), true),
        range,
    )
}

pub fn space<'a, S: TextSource + 'a>(min: usize) -> impl Parser<S, Token = ()> {
    // OLD 1
    //separated(
    //    blanks(0..),
    //    continuation(),
    //    0..,
    //).map(|_| ())

    // OLD 2
    //(
    //    blanks(min..),
    //    separated(
    //        blanks(0..),
    //        continuation(),
    //        0..,
    //    ),
    //).map(|_| ())

    blanks(min..).map(|_| ())
}

// OLD
//pub fn alphanumeric_character<'a, S: TextSource>(cfg: &'a Cfg) -> impl Parser<S, Token = Char<S::Span>> {
//    alt!(
//        letter,
//        digit,
//        underscore,
//    )
//}

#[doc = s_rule!(
    F18V007r1 rule "alphanumeric-character" #601 :
    "is letter"
    "or digit"
    "or underscore",
)]
pub fn alphanumeric_character<S: TextSource>(source: S) -> PResult<Char<S::Span>, S> {
    alt!(
        for S =>
        letter,
        digit,
        underscore,
    ).parse(source)
}

#[doc = s_rule!(
    F18V007r1 rule "letter" section "6.1.2",
)]
pub fn letter<S: TextSource>(source: S) -> PResult<Char<S::Span>, S> {
    //Char::any_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".chars()).parse(source)
    Char::parse(|ref c| ('a'..='z').contains(c) || ('A'..='Z').contains(c)).parse(source)
}

#[doc = s_rule!(
    F18V007r1 rule "digit" section "6.1.3",
)]
pub fn digit<S: TextSource>(source: S) -> PResult<Char<S::Span>, S> {
    Char::any_of("0123456789".chars()).parse(source)
}

#[doc = s_rule!(
    F18V007r1 rule "underscore" #602 : "is _",
)]
pub fn underscore<S: TextSource>(source: S) -> PResult<Char<S::Span>, S> {
    Char::exact('_').parse(source)
}

#[derive(Debug, Clone)]
pub struct SpecialCharacterMatch<Span> {
    pub character: SpecialCharacter,
    pub span: Span,
}

impl<Span> Spanned<Span> for SpecialCharacterMatch<Span> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl<Span> MapSpan<Span> for SpecialCharacterMatch<Span> {
    type Spanned<T> = SpecialCharacterMatch<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        SpecialCharacterMatch {
            character: self.character,
            span: f(self.span),
        }
    }
}

/// Fortran special character
#[doc = s_rule!(
    F18V007r1 rule "special-character" section "6.1.5",
)]
pub fn special_character<S: TextSource>(source: S) -> PResult<SpecialCharacterMatch<S::Span>, S> {
    // TODO use any
    for sc in SpecialCharacter::ALL{
        let r = sc.parse(source.clone());
        if r.is_some() {
            return r;
        }
    }

    return None;
}

#[doc = s_rule!(
    F18V007r1 rule "name" #603 : "is letter [ alphanumeric-character ] ...",
)]
pub fn name<'a, S: TextSource + 'a>(drop_last_underscore: bool) -> impl Parser<S, Token = Name<S::Span>> { // TODO remove bound on source
    move |source: S| {
        if drop_last_underscore {
            letter
                .then(move |first| fold_many(
                    alphanumeric_character
                        .condition(|c, s: &S| if c.value == '_' { alphanumeric_character.parses(s.clone()) } else { true }),
                    move || StringMatch::from_char(first.clone()),
                    |mut name, c| {
                        name.push_char(c);
                        (name, true)
                    },
                    0..,
                ))
                .map(|m| Name(m))
                .parse(source)
        } else {
            (
                letter,
                many(alphanumeric_character, 0..),
            ).map(move |(first, rest)| {
                Name(StringMatch::from_chars::<S>(
                    std::iter::once(first).chain(rest.into_iter().map(|c| c)),
                ))
            }).parse(source)
        }
    }
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
            .optional(),
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

    alt!(
        for S =>
        digit_string.map(KindParam::DigitString),
        name(drop_last_underscore).map(KindParam::ScalarIntConstantName),
    )
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
        sign::<S>.optional(),
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
        sign.optional(),
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
            ).optional(),
            (
                underscore,
                kind_param(false),
            ).map(|(_, k)| k).optional(),
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
            ).map(|(_, k)| k).optional(),
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
            digit_string.optional(),
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

    pub fn span(&self) -> &Span {
        match self {
            Self::Char(c) => &c.span,
            Self::EscapeSequence(s, _) => &s.span,
        }
    }
}

impl<Span> MapSpan<Span> for StringElement<Span> {
    type Spanned<T> = StringElement<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        match self {
            Self::Char(c) => StringElement::Char(c.map_span(f)),
            Self::EscapeSequence(s, into) => StringElement::EscapeSequence(s.map_span(f), into),
        }
    }
}

pub fn string_element<'a, S: TextSource + 'a>(termination: char, escape: &'static str, into: &'static str) -> impl Parser<S, Token = StringElement<S::Span>> {
    alt!(
        for S =>
        StringMatch::exact(escape, true).map(|s| StringElement::EscapeSequence(s, into)),
        Char::parse(|c| c != termination).map(|c| StringElement::Char(c)),
    )
}

#[derive(Debug, Clone)]
pub struct CharLiteralConstant<Span> {
    pub kind_param: Option<KindParam<Span>>,
    pub delimiter: char,
    pub open_quote: Char<Span>,
    pub content: Vec<StringElement<Span>>,
    pub close_quote: Option<Char<Span>>,
    pub span: Span,
}

impl<Span> CharLiteralConstant<Span> {
    pub fn value(&self) -> String {
        self.content.iter().map(|e| e.value()).collect()
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

impl<Span> MapSpan<Span> for CharLiteralConstant<Span> {
    type Spanned<T> = CharLiteralConstant<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        CharLiteralConstant {
            kind_param: self.kind_param.map(|k| k.map_span(f)),
            delimiter: self.delimiter,
            open_quote: self.open_quote.map_span(f),
            content: self.content.into_iter().map(|e| e.map_span(f)).collect(),
            close_quote: self.close_quote.map(|c| c.map_span(f)),
            span: f(self.span),
        }
    }
}

#[doc = s_rule!(
    F18V007r1 rule "char-literal-constant" #724 :
    "is [ kind-param _ ] ' [ rep-char ] ... '"
    "or [ kind-param _ ] \" [ rep-char ] ... \"",
)]
pub fn char_literal_constant<S: TextSource>(source: S) -> PResult<CharLiteralConstant<S::Span>, S> {
    alt! {
        for S =>
        (
            (kind_param(true), underscore).map(|(k, _)| k).optional(),
            Char::<S::Span>::exact('\''),
            fold_many(
                string_element('\'', "''", "'"),
                || Vec::new(),
                |mut content, element: StringElement<S::Span>| {
                    content.push(element);
                    (content, true)
                },
                0..,
            ),
            Char::<S::Span>::exact('\'').optional(),
        )
            .map(|(kind_param, open_quote, content, close_quote)| {
                let mut span = open_quote.span.clone();
                if let Some(close_quote) = &close_quote {
                    span = S::Span::merge(span, close_quote.span.clone());
                } else if let Some(last) = content.last() {
                    span = S::Span::merge(span, last.span().clone());
                }
                CharLiteralConstant {
                    kind_param,
                    delimiter: '\'',
                    open_quote,
                    content,
                    close_quote,
                    span,
                }
            }),
        (
            (kind_param(true), underscore).map(|(k, _)| k).optional(),
            Char::<S::Span>::exact('"'),
            fold_many(
                string_element('"', "\"\"", "\""),
                || Vec::new(),
                |mut content, element: StringElement<S::Span>| {
                    content.push(element);
                    (content, true)
                },
                0..,
            ),
            Char::<S::Span>::exact('"').optional(),
        )
            .map(|(kind_param, open_quote, content, close_quote)| {
                let mut span = open_quote.span.clone();
                if let Some(close_quote) = &close_quote {
                    span = S::Span::merge(span, close_quote.span.clone());
                } else if let Some(last) = content.last() {
                    span = S::Span::merge(span, last.span().clone());
                }
                CharLiteralConstant {
                    kind_param,
                    delimiter: '"',
                    open_quote,
                    content,
                    close_quote,
                    span, // TODO ...
                }
            }),
    }.parse(source)
}

#[derive(Debug, Clone)]
pub struct LogicalLiteralConstant<Span> {
    pub value_match: StringMatch<Span>,
    pub value: bool,
    pub kind: Option<KindParam<Span>>,
    span: Span,
}

impl<Span> LogicalLiteralConstant<Span> {
    pub fn span(&self) -> &Span {
        &self.span
    }
}

impl<Span> MapSpan<Span> for LogicalLiteralConstant<Span> {
    type Spanned<T> = LogicalLiteralConstant<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        LogicalLiteralConstant {
            value_match: self.value_match.map_span(f),
            value: self.value,
            kind: self.kind.map(|k| k.map_span(f)),
            span: f(self.span),
        }
    }
}

#[doc = s_rule!(
    F18V007r1 rule "logical-literal-constant" #725 :
    "is .TRUE. [ _ kind-param ]"
    "or .FALSE. [ _ kind-param ]",
)]
pub fn logical_literal_constant<S: TextSource>(source: S) -> PResult<LogicalLiteralConstant<S::Span>, S> {
    (
        alt!(
            for S =>
            StringMatch::exact(".TRUE.", false).map(|m| (m, true)),
            StringMatch::exact(".FALSE.", false).map(|m| (m, false)),
        ),
        (space(0), underscore, space(0), kind_param(true)).map(|(_, _, _, k)| k).optional(),
    ).map(|(value, kind): ((StringMatch<S::Span>, bool), Option<KindParam<S::Span>>)| {
        let mut span = value.0.span.clone();
        if let Some(kind) = &kind {
            span = S::Span::merge(span, kind.span().clone());
        }
        LogicalLiteralConstant {
            value_match: value.0,
            value: value.1,
            kind,
            span,
        }
    }).parse(source)
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
            Char::exact('\'').optional(),
        ),
        (
            Char::any_of("bB".chars()),
            Char::exact('"'),
            binary_digits(),
            Char::exact('"').optional(),
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
            Char::exact('\'').optional(),
        ),
        (
            Char::any_of("oO".chars()),
            Char::exact('"'),
            octal_digits(),
            Char::exact('"').optional(),
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
            Char::exact('\'').optional(),
        ),
        (
            Char::any_of("zZ".chars()),
            Char::exact('"'),
            hex_digits(),
            Char::exact('"').optional(),
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

#[derive(Debug, Clone)]
pub struct PowerOp<Span>(pub StringMatch<Span>);

impl<Span> ToString for PowerOp<Span> {
    fn to_string(&self) -> String {
        self.0.value.clone()
    }
}

impl<Span> MapSpan<Span> for PowerOp<Span> {
    type Spanned<T> = PowerOp<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        PowerOp(self.0.map_span(f))
    }
}

#[doc = s_rule!(
    F18V007r1 rule "power-op" #1007 : "is **",
)]
pub fn power_op<S: TextSource>(source: S) -> PResult<PowerOp<S::Span>, S> {
    StringMatch::exact("**", true).map(PowerOp).parse(source)
}

#[derive(Debug, Clone)]
pub struct MultOp<Span>(pub StringMatch<Span>);

impl<Span> ToString for MultOp<Span> {
    fn to_string(&self) -> String {
        self.0.value.clone()
    }
}

impl<Span> MapSpan<Span> for MultOp<Span> {
    type Spanned<T> = MultOp<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        MultOp(self.0.map_span(f))
    }
}

#[doc = s_rule!(
    F18V007r1 rule "mult-op" #1008 :
    "is *"
    "or /",
)]
pub fn mult_op<S: TextSource>(source: S) -> PResult<MultOp<S::Span>, S> {
    alt!(
        for S =>
        StringMatch::exact("*", true),
        StringMatch::exact("/", true),
    ).map(MultOp).parse(source)
}

#[derive(Debug, Clone)]
pub struct AddOp<Span>(pub StringMatch<Span>);

impl<Span> ToString for AddOp<Span> {
    fn to_string(&self) -> String {
        self.0.value.clone()
    }
}

impl<Span> MapSpan<Span> for AddOp<Span> {
    type Spanned<T> = AddOp<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        AddOp(self.0.map_span(f))
    }
}

#[doc = s_rule!(
    F18V007r1 rule "add-op" #1009 :
    "is +"
    "or -",
)]
pub fn add_op<S: TextSource>(source: S) -> PResult<AddOp<S::Span>, S> {
    alt!(
        for S =>
        StringMatch::exact("+", true),
        StringMatch::exact("-", true),
    ).map(AddOp)
    .parse(source)
}

#[derive(Debug, Clone)]
pub struct ConcatOp<Span>(pub StringMatch<Span>);

impl<Span> ToString for ConcatOp<Span> {
    fn to_string(&self) -> String {
        self.0.value.clone()
    }
}

impl<Span> MapSpan<Span> for ConcatOp<Span> {
    type Spanned<T> = ConcatOp<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        ConcatOp(self.0.map_span(f))
    }
}

#[doc = s_rule!(
    F18V007r1 rule "concat-op" #1011 : "is //",
)]
pub fn concat_op<S: TextSource>(source: S) -> PResult<ConcatOp<S::Span>, S> {
    StringMatch::exact("//", true).map(ConcatOp).parse(source)
}

#[derive(Debug, Clone)]
pub struct RelOp<Span>(pub StringMatch<Span>);

impl<Span> ToString for RelOp<Span> {
    fn to_string(&self) -> String {
        self.0.value.clone()
    }
}

impl<Span> MapSpan<Span> for RelOp<Span> {
    type Spanned<T> = RelOp<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        RelOp(self.0.map_span(f))
    }
}

#[doc = s_rule!(
    F18V007r1 rule "rel-op" #1013 :
    "is .EQ."
    "or .NE."
    "or .LT."
    "or .LE."
    "or .GT."
    "or .GE."
    "or =="
    "or /="
    "or <"
    "or <="
    "or >"
    "or >=",
)]
pub fn rel_op<S: TextSource>(source: S) -> PResult<RelOp<S::Span>, S> {
    // NOTE: The order of the alternatives is important, is different from the standard
    alt!(
        for S =>
        StringMatch::exact(".eq.", false),
        StringMatch::exact(".ne.", false),
        StringMatch::exact(".lt.", false),
        StringMatch::exact(".le.", false),
        StringMatch::exact(".gt.", false),
        StringMatch::exact(".ge.", false),
        StringMatch::exact("==", false),
        StringMatch::exact("/=", false),
        StringMatch::exact("<=", false),
        StringMatch::exact(">=", false),
        StringMatch::exact("<", false),
        StringMatch::exact(">", false),
    ).map(RelOp)
    .parse(source)
}

#[derive(Debug, Clone)]
pub struct NotOp<Span>(pub StringMatch<Span>);

impl<Span> ToString for NotOp<Span> {
    fn to_string(&self) -> String {
        self.0.value.clone()
    }
}

impl<Span> MapSpan<Span> for NotOp<Span> {
    type Spanned<T> = NotOp<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        NotOp(self.0.map_span(f))
    }
}

#[doc = s_rule!(
    F18V007r1 rule "not-op" #1018 : "is .NOT.",
)]
pub fn not_op<S: TextSource>(source: S) -> PResult<NotOp<S::Span>, S> {
    StringMatch::exact(".not.", false).map(NotOp).parse(source)
}

#[derive(Debug, Clone)]
pub struct AndOp<Span>(pub StringMatch<Span>);

impl<Span> ToString for AndOp<Span> {
    fn to_string(&self) -> String {
        self.0.value.clone()
    }
}

impl<Span> MapSpan<Span> for AndOp<Span> {
    type Spanned<T> = AndOp<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        AndOp(self.0.map_span(f))
    }
}

#[doc = s_rule!(
    F18V007r1 rule "and-op" #1019 : "is .AND.",
)]
pub fn and_op<S: TextSource>(source: S) -> PResult<AndOp<S::Span>, S> {
    StringMatch::exact(".and.", false).map(AndOp).parse(source)
}

#[derive(Debug, Clone)]
pub struct OrOp<Span>(pub StringMatch<Span>);

impl<Span> ToString for OrOp<Span> {
    fn to_string(&self) -> String {
        self.0.value.clone()
    }
}

impl<Span> MapSpan<Span> for OrOp<Span> {
    type Spanned<T> = OrOp<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        OrOp(self.0.map_span(f))
    }
}

#[doc = s_rule!(
    F18V007r1 rule "or-op" #1020 : "is .OR.",
)]
pub fn or_op<S: TextSource>(source: S) -> PResult<OrOp<S::Span>, S> {
    StringMatch::exact(".or.", false).map(OrOp).parse(source)
}

#[derive(Debug, Clone)]
pub struct EquivOp<Span>(pub StringMatch<Span>);

impl<Span> ToString for EquivOp<Span> {
    fn to_string(&self) -> String {
        self.0.value.clone()
    }
}

impl<Span> MapSpan<Span> for EquivOp<Span> {
    type Spanned<T> = EquivOp<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        EquivOp(self.0.map_span(f))
    }
}

#[doc = s_rule!(
    F18V007r1 rule "equiv-op" #1021 :
    "is .EQV."
    "or .NEQV.",
)]
pub fn equiv_op<S: TextSource>(source: S) -> PResult<EquivOp<S::Span>, S> {
    alt!(
        for S =>
        StringMatch::exact(".eqv.", false),
        StringMatch::exact(".neqv.", false),
    ).map(EquivOp)
    .parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum IntrinsicOperator<Span> {
    PowerOp(PowerOp<Span>),
    MultOp(MultOp<Span>),
    AddOp(AddOp<Span>),
    ConcatOp(ConcatOp<Span>),
    RelOp(RelOp<Span>),
    NotOp(NotOp<Span>),
    AndOp(AndOp<Span>),
    OrOp(OrOp<Span>),
    EquivOp(EquivOp<Span>),
}

impl<Span> IntrinsicOperator<Span> {
    pub fn span(&self) -> &Span {
        match self {
            Self::PowerOp(op) => &op.0.span,
            Self::MultOp(op) => &op.0.span,
            Self::AddOp(op) => &op.0.span,
            Self::ConcatOp(op) => &op.0.span,
            Self::RelOp(op) => &op.0.span,
            Self::NotOp(op) => &op.0.span,
            Self::AndOp(op) => &op.0.span,
            Self::OrOp(op) => &op.0.span,
            Self::EquivOp(op) => &op.0.span,
        }
    }
}

impl<Span> ToString for IntrinsicOperator<Span> {
    fn to_string(&self) -> String {
        match self {
            IntrinsicOperator::PowerOp(op) => op.to_string(),
            IntrinsicOperator::MultOp(op) => op.to_string(),
            IntrinsicOperator::AddOp(op) => op.to_string(),
            IntrinsicOperator::ConcatOp(op) => op.to_string(),
            IntrinsicOperator::RelOp(op) => op.to_string(),
            IntrinsicOperator::NotOp(op) => op.to_string(),
            IntrinsicOperator::AndOp(op) => op.to_string(),
            IntrinsicOperator::OrOp(op) => op.to_string(),
            IntrinsicOperator::EquivOp(op) => op.to_string(),
        }
    }
}

impl<Span> MapSpan<Span> for IntrinsicOperator<Span> {
    type Spanned<T> = IntrinsicOperator<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        match self {
            IntrinsicOperator::PowerOp(op) => IntrinsicOperator::PowerOp(op.map_span(f)),
            IntrinsicOperator::MultOp(op) => IntrinsicOperator::MultOp(op.map_span(f)),
            IntrinsicOperator::AddOp(op) => IntrinsicOperator::AddOp(op.map_span(f)),
            IntrinsicOperator::ConcatOp(op) => IntrinsicOperator::ConcatOp(op.map_span(f)),
            IntrinsicOperator::RelOp(op) => IntrinsicOperator::RelOp(op.map_span(f)),
            IntrinsicOperator::NotOp(op) => IntrinsicOperator::NotOp(op.map_span(f)),
            IntrinsicOperator::AndOp(op) => IntrinsicOperator::AndOp(op.map_span(f)),
            IntrinsicOperator::OrOp(op) => IntrinsicOperator::OrOp(op.map_span(f)),
            IntrinsicOperator::EquivOp(op) => IntrinsicOperator::EquivOp(op.map_span(f)),
        }
    }
}

#[doc = s_rule!(
    F18V007r1 rule "intrinsic-operator" #608 :
    "is power-op"
    "or mult-op"
    "or add-op"
    "or concat-op"
    "or rel-op"
    "or not-op"
    "or and-op"
    "or or-op"
    "or equiv-op",
)]
pub fn intrinsic_operator<S: TextSource>(source: S) -> PResult<IntrinsicOperator<S::Span>, S> {
    // NOTE: The order of the alternatives is important, is different from the standard
    alt!(
        for S =>
        power_op.map(IntrinsicOperator::PowerOp),
        rel_op.map(IntrinsicOperator::RelOp),
        concat_op.map(IntrinsicOperator::ConcatOp),
        mult_op.map(IntrinsicOperator::MultOp),
        add_op.map(IntrinsicOperator::AddOp),
        not_op.map(IntrinsicOperator::NotOp),
        and_op.map(IntrinsicOperator::AndOp),
        or_op.map(IntrinsicOperator::OrOp),
        equiv_op.map(IntrinsicOperator::EquivOp),
    ).parse(source)
}

// TODO test?
#[doc = s_rule!(
    F18V007r1 rule "extended-intrinsic-op" #610 : "is intrinsic-operator",
)]
pub fn extended_intrinsic_op<S: TextSource>(source: S) -> PResult<IntrinsicOperator<S::Span>, S> {
    intrinsic_operator.parse(source)
}

//#[derive(Debug, Clone)]
//pub struct Label<Span> {
//    pub digits: StringMatch<Span>,
//    pub value: u32,
//}
//
//impl<Span> Spanned<Span> for Label<Span> {
//    fn span(&self) -> &Span {
//        &self.digits.span
//    }
//}
//
//impl<Span> Spannable<Span> for Label<Span> {
//    type Spanned<T> = Label<T>;
//
//    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
//        Label {
//            digits: self.digits.map_span(f),
//            value: self.value,
//        }
//    }
//}
//
//#[doc = s_rule!(
//    F18V007r1 rule "label" #611 : "is digit [ digit [ digit [ digit [ digit ] ] ] ]",
//)]
//pub fn label<S: TextSource>(source: S) -> PResult<Label<S::Span>, S> {
//    digit
//        .then(|first| fold_many(
//            digit,
//            move || StringMatch::from_char(first.clone()),
//            |mut string, digit| {
//                string.push_char(digit);
//                (string, true)
//            },
//            0..=4,
//        ))
//        .map(|digits| {
//            let value = digits.value.parse::<u32>().unwrap();
//            Label { digits, value }
//        })
//}

#[derive(Debug, Clone)]
pub struct Label<Span>(pub IntLiteralConstant<Span>);

impl<Span> Spanned<Span> for Label<Span> {
    fn span(&self) -> &Span {
        &self.0.span
    }
}

impl<Span> MapSpan<Span> for Label<Span> {
    type Spanned<T> = Label<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        Label(self.0.map_span(f))
    }
}

#[doc = s_rule!(
    F18V007r1 rule "label" #611 : "is digit [ digit [ digit [ digit [ digit ] ] ] ]",
)]
pub fn label<S: TextSource>(source: S) -> PResult<Label<S::Span>, S> {
    // TODO implement rule (max 5 digits and no kind) and clause somewhere else
    digit_string::<S>.map(|s| Label({
        let span = s.span.clone();
        IntLiteralConstant {
            span,
            digits: s,
            kind_param: None,
        }
    }))
    .parse(source)
}

#[derive(Debug, Clone)]
pub struct DefinedUnaryOrBinaryOp<Span> {
    pub m: StringMatch<Span>,
}

impl<Span> MapSpan<Span> for DefinedUnaryOrBinaryOp<Span> {
    type Spanned<T> = DefinedUnaryOrBinaryOp<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        DefinedUnaryOrBinaryOp {
            m: self.m.map_span(f),
        }
    }
}

#[doc = s_rule!(
    F18V007r1 rule "defined-unary-op" #1003 : "is . letter [ letter ] ... .",
)]
pub fn defined_unary_op<S: TextSource>(source: S) -> PResult<DefinedUnaryOrBinaryOp<S::Span>, S> {
    (
        '.',
        fold_many(
            letter,
            || StringMatch::empty(),
            |mut m, l| {
                m.push_char(l);
                (m, true)
            },
            1..,
        ),
        '.',
    ).map(|(o, mut m, c)| {
        m.push_front_char(o);
        m.push_char(c);
        m
    })
    .map(|m| DefinedUnaryOrBinaryOp { m })
    .parse(source)
}

#[doc = s_rule!(
    F18V007r1 rule "defined-binary-op" #1023 : "is . letter [ letter ] ... .",
)]
pub fn defined_binary_op<S: TextSource>(source: S) -> PResult<DefinedUnaryOrBinaryOp<S::Span>, S> {
    (
        '.',
        fold_many(
            letter,
            || StringMatch::empty(),
            |mut m, l| {
                m.push_char(l);
                (m, true)
            },
            1..,
        ),
        '.',
    ).map(|(o, mut m, c)| {
        m.push_front_char(o);
        m.push_char(c);
        m
    })
    .map(|m| DefinedUnaryOrBinaryOp { m })
    .parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum DefinedOperator<Span> {
    DefinedUnaryOrBinary(DefinedUnaryOrBinaryOp<Span>),
    IntrinsicEx(IntrinsicOperator<Span>),
}

impl<Span> DefinedOperator<Span> {
    pub fn span(&self) -> &Span {
        match self {
            Self::DefinedUnaryOrBinary(op) => &op.m.span,
            Self::IntrinsicEx(op) => op.span(),
        }
    }
}

impl<Span> ToString for DefinedOperator<Span> {
    fn to_string(&self) -> String {
        match self {
            DefinedOperator::DefinedUnaryOrBinary(op) => format!(".{}.", op.m.value),
            DefinedOperator::IntrinsicEx(op) => op.to_string(),
        }
    }
}

impl<Span> MapSpan<Span> for DefinedOperator<Span> {
    type Spanned<T> = DefinedOperator<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        match self {
            DefinedOperator::DefinedUnaryOrBinary(op) => DefinedOperator::DefinedUnaryOrBinary(op.map_span(f)),
            DefinedOperator::IntrinsicEx(op) => DefinedOperator::IntrinsicEx(op.map_span(f)),
        }
    }
}

#[doc = s_rule!(
    F18V007r1 rule "defined-operator" #609 :
    "is defined-unary-op"
    "or defined-binary-op"
    "or extended-intrinsic-op",
)]
pub fn defined_operator<S: TextSource>(source: S) -> PResult<DefinedOperator<S::Span>, S> {
    alt!(
        for S =>
        extended_intrinsic_op.map(DefinedOperator::IntrinsicEx),
        defined_unary_op.map(DefinedOperator::DefinedUnaryOrBinary),
        defined_binary_op.map(DefinedOperator::DefinedUnaryOrBinary),
    ).parse(source)
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

// TODO use cfg
pub fn delimiter<S: TextSource>(source: S) -> PResult<SpecialCharacterMatch<S::Span>, S> {
    alt!(
        for S =>
        SpecialCharacter::LeftParenthesis,
        SpecialCharacter::RightParenthesis,
        SpecialCharacter::LeftSquareBracket,
        SpecialCharacter::RightSquareBracket,
        SpecialCharacter::LeftCurlyBracket,
        SpecialCharacter::RightCurlyBracket,
    )
    .parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum LexicalToken<Span> {
    /// Name or keyword
    ///
    /// Note that keywords are not distinguished from names at this level.
    /// Also note that in FORTRAN keywords can actually be used as names, for example variable names.
    Name(Name<Span>),

    /// Literal constant
    ///
    /// # Note:
    /// This is a non-complex literal constant, complex literals are expressed as a pair of
    /// non-complex literals at a higher level of the grammar.
    LiteralConstant(NonComplexLiteralConstant<Span>),

    Operator(DefinedOperator<Span>),

    //StatementLabel,

    Delimiter(SpecialCharacterMatch<Span>),

    /// `,`
    Comma(Comma<Span>),

    /// `=`
    Equals(Equals<Span>),

    /// `=>`
    Arrow(Arrow<Span>),

    /// `:`
    Colon(Colon<Span>),

    /// `::`
    DoubleColon(DoubleColon<Span>),

    /// `;`
    Semicolon(Semicolon<Span>),

    /// `..`
    DotDot(DotDot<Span>),

    /// `%`
    Percent(Percent<Span>),

    // TODO not in the standard but necessary ???
    Dot(Dot<Span>),

    // TODO use StringMatch instead
    Error(Char<Span>),
}

impl<Span> Spanned<Span> for LexicalToken<Span> {
    fn span(&self) -> &Span {
        match self {
            LexicalToken::Name(n) => n.span(),
            LexicalToken::LiteralConstant(l) => l.span(),
            LexicalToken::Operator(o) => o.span(),
            LexicalToken::Delimiter(d) => d.span(),
            LexicalToken::Comma(c) => c.span(),
            LexicalToken::Equals(e) => e.span(),
            LexicalToken::Arrow(a) => a.span(),
            LexicalToken::Colon(c) => c.span(),
            LexicalToken::DoubleColon(c) => c.span(),
            LexicalToken::Semicolon(s) => s.span(),
            LexicalToken::DotDot(d) => d.span(),
            LexicalToken::Percent(p) => p.span(),
            LexicalToken::Dot(d) => d.span(),
            LexicalToken::Error(e) => e.span(),
        }
    }
}

impl<Span> MapSpan<Span> for LexicalToken<Span> {
    type Spanned<T> = LexicalToken<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        match self {
            LexicalToken::Name(n) => LexicalToken::Name(n.map_span(f)),
            LexicalToken::LiteralConstant(l) => LexicalToken::LiteralConstant(l.map_span(f)),
            LexicalToken::Operator(o) => LexicalToken::Operator(o.map_span(f)),
            LexicalToken::Delimiter(d) => LexicalToken::Delimiter(d.map_span(f)),
            LexicalToken::Comma(c) => LexicalToken::Comma(c.map_span(f)),
            LexicalToken::Equals(e) => LexicalToken::Equals(e.map_span(f)),
            LexicalToken::Arrow(a) => LexicalToken::Arrow(a.map_span(f)),
            LexicalToken::Colon(c) => LexicalToken::Colon(c.map_span(f)),
            LexicalToken::DoubleColon(c) => LexicalToken::DoubleColon(c.map_span(f)),
            LexicalToken::Semicolon(s) => LexicalToken::Semicolon(s.map_span(f)),
            LexicalToken::DotDot(d) => LexicalToken::DotDot(d.map_span(f)),
            LexicalToken::Percent(p) => LexicalToken::Percent(p.map_span(f)),
            LexicalToken::Dot(d) => LexicalToken::Dot(d.map_span(f)),
            LexicalToken::Error(e) => LexicalToken::Error(e.map_span(f)),
        }
    }
}

macro_rules! just_spanned {
    ($name:ident) => {
        #[derive(Debug, Clone)]
        pub struct $name<Span> {
            span: Span,
        }

        impl<Span> $name<Span> {
            pub fn new_spanned(span: Span) -> Self {
                $name { span }
            }
        }

        impl<Span> Spanned<Span> for $name<Span> {
            fn span(&self) -> &Span {
                &self.span
            }
        }

        impl<Span> MapSpan<Span> for $name<Span> {
            type Spanned<T> = $name<T>;

            fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
                $name {
                    span: f(self.span),
                }
            }
        }

        impl<Span> TokenTree<Span> for $name<Span> {
        }
    };
}

just_spanned!(Comma);
just_spanned!(Equals);
just_spanned!(Arrow);
just_spanned!(Colon);
just_spanned!(DoubleColon);
just_spanned!(Semicolon);
just_spanned!(DotDot);
just_spanned!(Percent);
just_spanned!(Dot);

macro_rules! just_wraps {
    ($name:ident($ty:ident)) => {
        #[derive(Debug, Clone)]
        pub struct $name<Span>(pub $ty<Span>);

        impl<Span> Spanned<Span> for $name<Span> {
            fn span(&self) -> &Span {
                self.0.span()
            }
        }
    
        impl<Span> MapSpan<Span> for $name<Span> {
            type Spanned<T> = $name<T>;
    
            fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
                $name(self.0.map_span(f))
            }
        }
    
        impl<Span> TokenTree<Span> for $name<Span> {
        }
    };
}

just_wraps!(Name(StringMatch));

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

#[doc = s_rule!(
    F18V007r1 rule "lexical-token" section "6.2.1",
)]
pub fn lexical_token<S: TextSource>(source: S) -> PResult<LexicalToken<S::Span>, S> {
    alt!{
        for S =>
        non_complex_literal_constant.map(LexicalToken::LiteralConstant),
        name(false).map(LexicalToken::Name),
        delimiter.map(LexicalToken::Delimiter),
        defined_operator.map(LexicalToken::Operator),
        (SpecialCharacter::Equals, SpecialCharacter::GreaterThan).map(|(c1, c2): (SpecialCharacterMatch<S::Span>, SpecialCharacterMatch<S::Span>)| LexicalToken::Arrow(Arrow::new_spanned(S::Span::merge(c1.span, c2.span)))),
        (SpecialCharacter::Colon, SpecialCharacter::Colon).map(|(c1, c2)| LexicalToken::DoubleColon(DoubleColon::new_spanned(S::Span::merge(c1.span, c2.span)))),
        (SpecialCharacter::DecimalPointOrPeriod, SpecialCharacter::DecimalPointOrPeriod).map(|(c1, c2)| LexicalToken::DotDot(DotDot::new_spanned(S::Span::merge(c1.span, c2.span)))),
        SpecialCharacter::Comma.map(|c| LexicalToken::Comma(Comma::new_spanned(c.span))),
        SpecialCharacter::Equals.map(|c| LexicalToken::Equals(Equals::new_spanned(c.span))),
        SpecialCharacter::Colon.map(|c| LexicalToken::Colon(Colon::new_spanned(c.span))),
        SpecialCharacter::SemiColon.map(|c| LexicalToken::Semicolon(Semicolon::new_spanned(c.span))),
        SpecialCharacter::Percent.map(|c| LexicalToken::Percent(Percent::new_spanned(c.span))),
        SpecialCharacter::DecimalPointOrPeriod.map(|c| LexicalToken::Dot(Dot::new_spanned(c.span))),
        Char::any().map(LexicalToken::Error),
    }.parse(source)
}

// TODO ???
#[derive(Debug, Clone)]
pub struct LineComment<Span> {
    pub text: String,
    pub span: Span,
}

impl<Span> Spanned<Span> for LineComment<Span> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl<Span> MapSpan<Span> for LineComment<Span> {
    type Spanned<T> = LineComment<T>;
    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        LineComment {
            text: self.text,
            span: f(self.span),
        }
    }
}

// TODO ???
pub fn comment_start<S: TextSource>(source: S) -> PResult<S::Span, S> {
    // we accept both ! and c as comment starters
    //ExactMatch::exact("!", false).or(ExactMatch::exact("c ", true))
    fold_many(
        StringMatch::exact("!", false),
        || S::Span::new_null(),
        |s, m| (S::Span::merge(s, m.span), true),
        1..,
    )
    .parse(source)
}

// TODO ???
pub fn line_comment<S: TextSource>(source: S) -> PResult<LineComment<S::Span>, S> {
    comment_start.then(|bang_span: S::Span| {
        many_until(Char::<S::Span>::any(), eol.do_not_consume(), 0..).map(move |(chars, _newline)| {
            let bang_span = bang_span.clone();
            let span = if let Some(last) = chars.last() {
                S::Span::merge(bang_span, last.span.clone())
            } else {
                bang_span
            };

            LineComment {
                text: chars.into_iter().map(|c| c.value).collect(),
                span,
            }
        })
    })
    .parse(source)
}

// TODO ???
pub fn nl<S: TextSource>(source: S) -> PResult<(), S> {
    alt! {
        for S =>
        "\n\r".map(|_| ()),
        "\n".map(|_| ()),
        "\r".map(|_| ()),
    }
        .map(|_| ())
        .parse(source)
}

// TODO ???
/// End of line
pub fn eol<S: TextSource>(source: S) -> PResult<(), S> {
    alt! {
        for S =>
        StringMatch::exact("\r\n", true),
        StringMatch::exact("\n", true),
    }
    .map(|_| ())
    .or(eof())
    .map(|o| o.inner())
    .parse(source)
}

#[cfg(test)]
mod test {

    use super::*;

    #[test]
    pub fn test_alphanumeric_character() {
        assert_eq!(alphanumeric_character.parses("a"), true);
        assert_eq!(alphanumeric_character.parses("A"), true);
        assert_eq!(alphanumeric_character.parses("3"), true);
        assert_eq!(alphanumeric_character.parses("_"), true);
        assert_eq!(alphanumeric_character.parses(" "), false);
        assert_eq!(alphanumeric_character.parses("="), false);
    }

    #[test]
    pub fn test_letter() {
        assert_eq!(letter.parses("a"), true);
        assert_eq!(letter.parses("A"), true);
        assert_eq!(letter.parses("3"), false);
        assert_eq!(letter.parses("_"), false);
    }

    #[test]
    pub fn test_digit() {
        assert_eq!(digit.parses("a"), false);
        assert_eq!(digit.parses("A"), false);
        assert_eq!(digit.parses("3"), true);
        assert_eq!(digit.parses("_"), false);
    }

    #[test]
    pub fn test_underscore() {
        assert_eq!(underscore.parses("a"), false);
        assert_eq!(underscore.parses("A"), false);
        assert_eq!(underscore.parses("3"), false);
        assert_eq!(underscore.parses("_"), true);
    }

    #[test]
    pub fn test_special_character() {
        use SpecialCharacter::*;
        assert_eq!(special_character.parse(" ").unwrap().0.character, Blank);
        assert_eq!(special_character.parse("=").unwrap().0.character, Equals);
        assert_eq!(special_character.parse("+").unwrap().0.character, Plus);
        assert_eq!(special_character.parse("-").unwrap().0.character, Minus);
        assert_eq!(special_character.parse("*").unwrap().0.character, Asterisk);
        assert_eq!(special_character.parse("/").unwrap().0.character, Slash);
        assert_eq!(special_character.parse("\\").unwrap().0.character, Backslash);
        assert_eq!(special_character.parse("(").unwrap().0.character, LeftParenthesis);
        assert_eq!(special_character.parse(")").unwrap().0.character, RightParenthesis);
        assert_eq!(special_character.parse("[").unwrap().0.character, LeftSquareBracket);
        assert_eq!(special_character.parse("]").unwrap().0.character, RightSquareBracket);
        assert_eq!(special_character.parse("{").unwrap().0.character, LeftCurlyBracket);
        assert_eq!(special_character.parse("}").unwrap().0.character, RightCurlyBracket);
        assert_eq!(special_character.parse(",").unwrap().0.character, Comma);
        assert_eq!(special_character.parse(".").unwrap().0.character, DecimalPointOrPeriod);
        assert_eq!(special_character.parse(":").unwrap().0.character, Colon);
        assert_eq!(special_character.parse(";").unwrap().0.character, SemiColon);
        assert_eq!(special_character.parse("!").unwrap().0.character, ExclamationPoint);
        assert_eq!(special_character.parse("\"").unwrap().0.character, QuotationMarkOrQuote);
        assert_eq!(special_character.parse("%").unwrap().0.character, Percent);
        assert_eq!(special_character.parse("&").unwrap().0.character, Ampersand);
        assert_eq!(special_character.parse("~").unwrap().0.character, Tilde);
        assert_eq!(special_character.parse("<").unwrap().0.character, LessThan);
        assert_eq!(special_character.parse(">").unwrap().0.character, GreaterThan);
        assert_eq!(special_character.parse("?").unwrap().0.character, QuestionMark);
        assert_eq!(special_character.parse("'").unwrap().0.character, Apostrophe);
        assert_eq!(special_character.parse("`").unwrap().0.character, GraveAccent);
        assert_eq!(special_character.parse("^").unwrap().0.character, CircumflexAccent);
        assert_eq!(special_character.parse("|").unwrap().0.character, VerticalLine);
        assert_eq!(special_character.parse("¤").unwrap().0.character, CurrencySymbol);
        assert_eq!(special_character.parse("#").unwrap().0.character, NumberSign);
        assert_eq!(special_character.parse("@").unwrap().0.character, CommercialAt);
        assert_eq!(special_character.parses("a"), false);
        assert_eq!(special_character.parses("A"), false);
        assert_eq!(special_character.parses("3"), false);
        assert_eq!(special_character.parses("_"), false);
    }

    #[test]
    pub fn test_name() {
        assert_eq!(name(false).parse("some_name").unwrap().0.0.value, "some_name");
        assert_eq!(name(false).parse("some_name ").unwrap().0.0.value, "some_name");
        assert_eq!(name(false).parses(" some_name"), false);

        assert_eq!(name(false).parse("A1 ").unwrap().0.0.value, "A1");
        assert_eq!(name(false).parse("NAME_LENGTH ").unwrap().0.0.value, "NAME_LENGTH");
        assert_eq!(name(false).parse("S_P_R_E_A_D__O_U_T ").unwrap().0.0.value, "S_P_R_E_A_D__O_U_T");
        assert_eq!(name(false).parse("TRAILER_ ").unwrap().0.0.value, "TRAILER_");
        assert_eq!(name(true).parse("TRAILER_ ").unwrap().0.0.value, "TRAILER");
        assert_eq!(name(true).parse("TRAILER_ ").unwrap().1, "_ ");

        assert_eq!(name(false).parse("a").unwrap().0.0.value, "a");
        assert_eq!(name(false).parse("a ").unwrap().0.0.value, "a");
        assert_eq!(name(false).parse("A%").unwrap().0.0.value, "A");
        assert_eq!(name(false).parse("A ").unwrap().0.0.value, "A");
        assert_eq!(name(false).parses("3+"), false);
        assert_eq!(name(false).parses("_"), false);
        assert_eq!(name(false).parse("a3|").unwrap().0.0.value, "a3");
        assert_eq!(name(false).parse("a3 ").unwrap().0.0.value, "a3");
        assert_eq!(name(false).parse("a_").unwrap().0.0.value, "a_");
        assert_eq!(name(false).parse("a_! ").unwrap().0.0.value, "a_");
        assert_eq!(name(false).parse("a3_").unwrap().0.0.value, "a3_");
        assert_eq!(name(false).parse("a3_ ").unwrap().0.0.value, "a3_");
    }

    #[test]
    fn test_power_op() {
        assert_eq!(power_op.parses(""), false);
        assert_eq!(power_op.parses("*"), false);
        assert_eq!(power_op.parses("**"), true);
    }

    #[test]
    fn test_mult_op() {
        assert_eq!(mult_op.parses(""), false);
        assert_eq!(mult_op.parses("*"), true);
        assert_eq!(mult_op.parses("/"), true);
        assert_eq!(mult_op.parses("**"), true); // TODO maybe false??
        assert_eq!(mult_op.parses("//"), true); // TODO maybe false??
    }

    #[test]
    fn test_add_op() {
        assert_eq!(add_op.parses(""), false);
        assert_eq!(add_op.parses("+"), true);
        assert_eq!(add_op.parses("-"), true);
        assert_eq!(add_op.parses("**"), false);
    }

    #[test]
    fn test_concat_op() {
        assert_eq!(concat_op.parses(""), false);
        assert_eq!(concat_op.parses("//"), true);
        assert_eq!(concat_op.parses("**"), false);
    }

    #[test]
    fn test_rel_op() {
        assert_eq!(rel_op.parses(""), false);
        assert_eq!(rel_op.parse(".eq. ").unwrap().0.0.value(), ".eq.");
        assert_eq!(rel_op.parse(".ne. ").unwrap().0.0.value(), ".ne.");
        assert_eq!(rel_op.parse(".lt. ").unwrap().0.0.value(), ".lt.");
        assert_eq!(rel_op.parse(".le. ").unwrap().0.0.value(), ".le.");
        assert_eq!(rel_op.parse(".gt. ").unwrap().0.0.value(), ".gt.");
        assert_eq!(rel_op.parse(".ge. ").unwrap().0.0.value(), ".ge.");
        assert_eq!(rel_op.parse("== ").unwrap().0.0.value(), "==");
        assert_eq!(rel_op.parse("/= ").unwrap().0.0.value(), "/=");
        assert_eq!(rel_op.parse("<= ").unwrap().0.0.value(), "<=");
        assert_eq!(rel_op.parse(">= ").unwrap().0.0.value(), ">=");
        assert_eq!(rel_op.parse("< ").unwrap().0.0.value(), "<");
        assert_eq!(rel_op.parse("> ").unwrap().0.0.value(), ">");
        assert_eq!(rel_op.parses("**"), false);
    }

    #[test]
    fn test_not_op() {
        assert_eq!(not_op.parses(""), false);
        assert_eq!(not_op.parse(".not. ").unwrap().0.0.value(), ".not.");
        assert_eq!(not_op.parses("**"), false);
    } 

    #[test]
    fn test_and_op() {
        assert_eq!(and_op.parses(""), false);
        assert_eq!(and_op.parse(".and. ").unwrap().0.0.value(), ".and.");
        assert_eq!(and_op.parses("**"), false);
    }

    #[test]
    fn test_or_op() {
        assert_eq!(or_op.parses(""), false);
        assert_eq!(or_op.parse(".or. ").unwrap().0.0.value(), ".or.");
        assert_eq!(or_op.parses("**"), false);
    }

    #[test]
    fn test_equiv_op() {
        assert_eq!(equiv_op.parses(""), false);
        assert_eq!(equiv_op.parse(".eqv. ").unwrap().0.0.value(), ".eqv.");
        assert_eq!(equiv_op.parse(".neqv. ").unwrap().0.0.value(), ".neqv.");
        assert_eq!(equiv_op.parses("**"), false);
    }

    #[test]
    fn test_intrinsic_operator() {
        assert_eq!(intrinsic_operator.parses(""), false);
        assert_eq!(intrinsic_operator.parse(".eqv. ").unwrap().0.is_equiv_op(), true);
        assert_eq!(intrinsic_operator.parse(".neqv. ").unwrap().0.is_equiv_op(), true);
        assert_eq!(intrinsic_operator.parse(".eq. ").unwrap().0.is_rel_op(), true);
        assert_eq!(intrinsic_operator.parse(".ne. ").unwrap().0.is_rel_op(), true);
        assert_eq!(intrinsic_operator.parse(".lt. ").unwrap().0.is_rel_op(), true);
        assert_eq!(intrinsic_operator.parse(".le. ").unwrap().0.is_rel_op(), true);
        assert_eq!(intrinsic_operator.parse(".gt. ").unwrap().0.is_rel_op(), true);
        assert_eq!(intrinsic_operator.parse(".ge. ").unwrap().0.is_rel_op(), true);
        assert_eq!(intrinsic_operator.parse("== ").unwrap().0.is_rel_op(), true);
        assert_eq!(intrinsic_operator.parse("/= ").unwrap().0.is_rel_op(), true);
        assert_eq!(intrinsic_operator.parse("<= ").unwrap().0.is_rel_op(), true);
        assert_eq!(intrinsic_operator.parse(">= ").unwrap().0.is_rel_op(), true);
        assert_eq!(intrinsic_operator.parse("< ").unwrap().0.is_rel_op(), true);
        assert_eq!(intrinsic_operator.parse("> ").unwrap().0.is_rel_op(), true);
        assert_eq!(intrinsic_operator.parse(".not. ").unwrap().0.is_not_op(), true);
        assert_eq!(intrinsic_operator.parse(".and. ").unwrap().0.is_and_op(), true);
        assert_eq!(intrinsic_operator.parse(".or. ").unwrap().0.is_or_op(), true);
        assert_eq!(intrinsic_operator.parse("**").unwrap().0.is_power_op(), true);
        assert_eq!(intrinsic_operator.parse("*").unwrap().0.is_mult_op(), true);
        assert_eq!(intrinsic_operator.parse("/").unwrap().0.is_mult_op(), true);
        assert_eq!(intrinsic_operator.parse("+").unwrap().0.is_add_op(), true);
        assert_eq!(intrinsic_operator.parse("-").unwrap().0.is_add_op(), true);
        assert_eq!(intrinsic_operator.parse("//").unwrap().0.is_concat_op(), true);
    }

    #[test]
    fn test_defined_unary_op() {
        assert_eq!(defined_unary_op.parses(""), false);
        assert_eq!(defined_unary_op.parse(".foo.").unwrap().0.m.value(), ".foo.");
        assert_eq!(defined_unary_op.parses(".foo"), false);
        assert_eq!(defined_unary_op.parse(".foo.bar.").unwrap().0.m.value(), ".foo.");
    }

    #[test]
    fn test_defined_binary_op() {
        assert_eq!(defined_binary_op.parses(""), false);
        assert_eq!(defined_binary_op.parse(".foo.").unwrap().0.m.value(), ".foo.");
        assert_eq!(defined_binary_op.parses(".foo"), false);
        assert_eq!(defined_binary_op.parse(".foo.bar.").unwrap().0.m.value(), ".foo.");
    }

    #[test]
    fn test_defined_operator() {
        assert_eq!(defined_operator.parses(""), false);
        assert_eq!(defined_operator.parse(".foo.").unwrap().0.is_defined_unary_or_binary(), true);
        assert_eq!(defined_operator.parse(".eqv. ").unwrap().0.is_intrinsic_ex(), true);
        assert_eq!(defined_operator.parse("+ ").unwrap().0.is_intrinsic_ex(), true);
    }

    #[test]
    pub fn test_non_complex_literal_constant() {
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

    #[test]
    pub fn test_lexical_token() {
        assert!(lexical_token.parse(".").unwrap().0.is_dot());
        assert!(lexical_token.parse("ciao").unwrap().0.is_name());
        assert!(lexical_token.parse("42").unwrap().0.is_literal_constant());
        assert!(lexical_token.parse("'42'").unwrap().0.as_literal_constant().unwrap().is_char());
        assert!(lexical_token.parse(".true.").unwrap().0.as_literal_constant().unwrap().is_logical());
        assert_eq!(
            lexical_token
                .parse(">=").unwrap()
                .0.as_operator().unwrap()
                .as_intrinsic_ex().unwrap()
                .as_rel_op().unwrap()
                .0.value,
            ">=",
        );
        assert_eq!(
            lexical_token
                .parse("==").unwrap()
                .0.as_operator().unwrap()
                .as_intrinsic_ex().unwrap()
                .as_rel_op().unwrap()
                .0.value,
            "==",
        );
        
        assert!(lexical_token.parse(",").unwrap().0.is_comma());
        assert!(lexical_token.parse("=").unwrap().0.is_equals());
        assert!(lexical_token.parse("=>").unwrap().0.is_arrow());
        assert!(lexical_token.parse(":").unwrap().0.is_colon());
        assert!(lexical_token.parse("::").unwrap().0.is_double_colon());
        assert!(lexical_token.parse(";").unwrap().0.is_semicolon());
        assert!(lexical_token.parse("..").unwrap().0.is_dot_dot());
        assert!(lexical_token.parse("%").unwrap().0.is_percent());
    }
}
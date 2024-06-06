use std::ops::RangeBounds;

use f2rs_parse_derive::syntax_rule;
use f2rs_parser_combinator::prelude::*;

use crate::{Cfg, Standard::*};
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

pub fn blanks<'a, S: TextSource + 'a>(range: impl RangeBounds<usize> + Clone + 'a) -> impl Parser<S, Token = ()> + 'a {
    fold_many(
        Parser::<S>::or(SpecialCharacter::Blank, SpecialCharacter::Tab), // NOTE: added Tab for convenience
        || (),
        |_, _| ((), true),
        range,
    )
}

pub fn space<'a, S: TextSource + 'a>(min: usize) -> impl Parser<S, Token = ()> + 'a {
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

#[syntax_rule(
    F18V007r1 rule "alphanumeric-character" #601 :
    "is letter"
    "or digit"
    "or underscore",
)]
pub fn alphanumeric_character<'a, S: TextSource>(cfg: &'a Cfg) -> impl Parser<S, Token = Char<S::Span>> + 'a {
    alt!(
        letter(cfg),
        digit(cfg),
        underscore(cfg),
    )
}

#[syntax_rule(
    F18V007r1 rule "letter" section "6.1.2",
)]
pub fn letter<S: TextSource>(cfg: &Cfg) -> impl Parser<S, Token = Char<S::Span>> {
    Char::parse(|ref c| ('a'..='z').contains(c) || ('A'..='Z').contains(c))
}

#[syntax_rule(
    F18V007r1 rule "digit" section "6.1.3",
)]
pub fn digit<S: TextSource>(cfg: &Cfg) -> impl Parser<S, Token = Char<S::Span>> {
    Char::any_of("0123456789".chars())
}

#[syntax_rule(
    F18V007r1 rule "underscore" #602 : "is _",
)]
pub fn underscore<S: TextSource>(cfg: &Cfg) -> impl Parser<S, Token = Char<S::Span>> {
    Char::exact('_')
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
#[syntax_rule(
    F18V007r1 rule "special-character" section "6.1.5",
)]
pub fn special_character<S: TextSource>(cfg: &Cfg) -> impl Parser<S, Token = SpecialCharacterMatch<S::Span>> {
    // TODO use any
    move |source: S| {
        for sc in SpecialCharacter::ALL{
            let r = sc.parse(source.clone());
            if r.is_some() {
                return r;
            }
        }

        return None;
    }
}

#[syntax_rule(
    F18V007r1 rule "name" #603 : "is letter [ alphanumeric-character ] ...",
)]
pub fn name<'a, S: TextSource + 'a>(cfg: &'a Cfg, drop_last_underscore: bool) -> impl Parser<S, Token = Name<S::Span>> + 'a { // TODO remove bound on source
    move |source: S| {
        if drop_last_underscore {
            letter(cfg)
                .then(move |first| fold_many(
                    alphanumeric_character::<S>(cfg)
                        .condition(|c, s| if c.value == '_' { alphanumeric_character::<S>(cfg).parses(s.clone()) } else { true }),
                    move || StringMatch::from_char(first.clone()),
                    |mut name, c| {
                        name.push_char::<S>(c);
                        (name, true)
                    },
                    0..,
                ))
                .map(|m| Name(m))
                .parse(source)
        } else {
            (
                letter(cfg),
                many(alphanumeric_character(cfg), 0..),
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

#[syntax_rule(
    F18V007r1 rule "sign" #712 :
    "is +"
    "or -",
)]
pub fn sign<S: TextSource>(cfg: &Cfg) -> impl Parser<S, Token = Sign<S::Span>> {
    Char::exact('+')
        .or(Char::exact('-')).map(|o| o.inner())
        .map(|c| match &c.value {
            '+' => Sign::Plus(c.span),
            '-' => Sign::Minus(c.span),
            _ => unreachable!(),
        })
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

#[syntax_rule(
    F18V007r1 rule "int-literal-constant" #708 : "is digit-string [ _ kind-param ]",
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

#[syntax_rule(
    F18V007r1 rule "kind-param" #709 :
    "is digit-string"
    "or scalar-int-constant-name",
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

#[syntax_rule(
    F18V007r1 rule "signed-digit-string" #710 : "is [ sign ] digit-string",
)]
pub fn signed_digit_string<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = SignedDigitString<S::Span>> + 'a {
    (
        sign::<S>(cfg).optional(),
        digit_string(cfg),
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
}

#[syntax_rule(
    F18V007r1 rule "digit-string" #711 : "is digit [ digit ] ...",
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

/// As defined in J3/18-007r1 §7.4.3.2 R713
#[derive(Debug, Clone)]
pub struct SignedRealLiteralConstant<Span> {
    pub sign: Option<Sign<Span>>,
    pub real_literal_constant: RealLiteralConstant<Span>,
}

#[syntax_rule(
    F18V007r1 rule "signed-real-literal-constant" #713 : "is [ sign ] real-literal-constant",
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

#[syntax_rule(
    F18V007r1 rule "real-literal-constant" #714 :
    "is significand [ exponent-letter exponent ] [ _ kind-param ]"
    "or digit-string exponent-letter exponent [ _ kind-param ]",
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
            digit_string(cfg),
            exponent_letter(cfg),
            exponent(cfg),
            (
                underscore(cfg),
                kind_param(cfg, false),
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
    )
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

#[syntax_rule(
    F18V007r1 rule "exponent-letter" #716 :
    "is E"
    "or D",
)]
pub fn exponent_letter<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ExponentLetter<S::Span>> + 'a {
    alt!(
        Char::exact_case_insensitive('d').map(|c| ExponentLetter::D(c.span)),
        Char::exact_case_insensitive('e').map(|c| ExponentLetter::E(c.span)),
    )
}

#[syntax_rule(
    F18V007r1 rule "exponent" #717 : "is signed-digit-string",
)]
pub fn exponent<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = SignedDigitString<S::Span>> + 'a {
    signed_digit_string(cfg)
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

#[syntax_rule(
    F18V007r1 rule "significand" #715 :
    "is digit-string . [ digit-string ]"
    "or . digit-string",
)]
pub fn significand<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = Significand<S::Span>> + 'a {
    alt! {
        (
            digit_string(cfg),
            '.',
            digit_string(cfg).optional(),
        ).map(|(first, _, second): (StringMatch<S::Span>, Char<S::Span>, Option<StringMatch<S::Span>>)| {
            let mut span = first.span.clone();
            if let Some(second) = &second {
                span = S::Span::merge(span, second.span.clone());
            }
            Significand::DotAfter(first, second, span)
        }),
        (
            '.',
            digit_string(cfg),
        ).map(|(_, second)| Significand::DotBefore(second)),
    }
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

#[syntax_rule(
    F18V007r1 rule "char-literal-constant" #724 :
    "is [ kind-param _ ] ' [ rep-char ] ... '"
    "or [ kind-param _ ] \" [ rep-char ] ... \"",
)]
pub fn char_literal_constant<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = CharLiteralConstant<S::Span>> + 'a {
    alt! {
        (
            (kind_param(cfg, true), underscore(cfg)).map(|(k, _)| k).optional(),
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
            (kind_param(cfg, true), underscore(cfg)).map(|(k, _)| k).optional(),
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
    }
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

#[syntax_rule(
    F18V007r1 rule "logical-literal-constant" #725 :
    "is .TRUE. [ _ kind-param ]"
    "or .FALSE. [ _ kind-param ]",
)]
pub fn logical_literal_constant<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = LogicalLiteralConstant<S::Span>> + 'a {
    (
        alt!(
            StringMatch::exact(".TRUE.", false).map(|m| (m, true)),
            StringMatch::exact(".FALSE.", false).map(|m| (m, false)),
        ),
        (space(0), underscore(cfg), space(0), kind_param(cfg, true)).map(|(_, _, _, k)| k).optional(),
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
    })
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
#[syntax_rule(
    F18V007r1 rule "boz-literal-constant" #764 :
    "is binary-constant"
    "or octal-constant"
    "or hex-constant",
)]
pub fn boz_literal_constant<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = BozLiteralConstant<S::Span>> + 'a {
    alt!(
        binary_constant(cfg).map(BozLiteralConstant::Binary),
        octal_constant(cfg).map(BozLiteralConstant::Octal),
        hex_constant(cfg).map(BozLiteralConstant::Hex),
    )
}

#[syntax_rule(
    F18V007r1 rule "binary-constant" #765 :
    "is B ' digit [ digit ] ... '"
    "or B \" digit [ digit ] ... \"",
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
            Char::exact('\'').optional(),
        ),
        (
            Char::any_of("bB".chars()),
            Char::exact('"'),
            binary_digits(),
            Char::exact('"').optional(),
        ),
    }.map(|(_, _, digits, _)| digits)
}

#[syntax_rule(
    F18V007r1 rule "octal-constant" #766 :
    "is O ' digit [ digit ] ... '"
    "or O \" digit [ digit ] ... \"",
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
            Char::exact('\'').optional(),
        ),
        (
            Char::any_of("oO".chars()),
            Char::exact('"'),
            octal_digits(),
            Char::exact('"').optional(),
        ),
    }.map(|(_, _, digits, _)| digits)
}

#[syntax_rule(
    F18V007r1 rule "hex-constant" #767 :
    "is Z ' hex-digit [ hex-digit ] ... '"
    "or Z \" hex-digit [ hex-digit ] ... \"",
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
            Char::exact('\'').optional(),
        ),
        (
            Char::any_of("zZ".chars()),
            Char::exact('"'),
            hex_digits(),
            Char::exact('"').optional(),
        ),
    }.map(|(_, _, digits, _)| digits)
}

#[syntax_rule(
    F18V007r1 rule "hex-digit" #768 :
    "is digit"
    "or A"
    "or B"
    "or C"
    "or D"
    "or E"
    "or F",
)]
pub fn hex_digit<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = Char<S::Span>> + 'a {
    alt!(
        digit(cfg),
        Char::any_of("abcdefABCDEF".chars()),
    )
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

#[syntax_rule(
    F18V007r1 rule "power-op" #1007 : "is **",
)]
pub fn power_op<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = PowerOp<S::Span>> + 'a {
    StringMatch::exact("**", true).map(PowerOp)
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

#[syntax_rule(
    F18V007r1 rule "mult-op" #1008 :
    "is *"
    "or /",
)]
pub fn mult_op<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = MultOp<S::Span>> + 'a {
    alt!(
        StringMatch::exact("*", true),
        StringMatch::exact("/", true),
    ).map(MultOp)
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

#[syntax_rule(
    F18V007r1 rule "add-op" #1009 :
    "is +"
    "or -",
)]
pub fn add_op<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = AddOp<S::Span>> + 'a {
    alt!(
        StringMatch::exact("+", true),
        StringMatch::exact("-", true),
    ).map(AddOp)
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

#[syntax_rule(
    F18V007r1 rule "concat-op" #1011 : "is //",
)]
pub fn concat_op<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ConcatOp<S::Span>> + 'a {
    StringMatch::exact("//", true).map(ConcatOp)
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

#[syntax_rule(
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
pub fn rel_op<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = RelOp<S::Span>> + 'a {
    // NOTE: The order of the alternatives is important, is different from the standard
    alt!(
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

#[syntax_rule(
    F18V007r1 rule "not-op" #1018 : "is .NOT.",
)]
pub fn not_op<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = NotOp<S::Span>> + 'a {
    StringMatch::exact(".not.", false).map(NotOp)
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

#[syntax_rule(
    F18V007r1 rule "and-op" #1019 : "is .AND.",
)]
pub fn and_op<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = AndOp<S::Span>> + 'a {
    StringMatch::exact(".and.", false).map(AndOp)
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

#[syntax_rule(
    F18V007r1 rule "or-op" #1020 : "is .OR.",
)]
pub fn or_op<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = OrOp<S::Span>> + 'a {
    StringMatch::exact(".or.", false).map(OrOp)
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

#[syntax_rule(
    F18V007r1 rule "equiv-op" #1021 :
    "is .EQV."
    "or .NEQV.",
)]
pub fn equiv_op<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = EquivOp<S::Span>> + 'a {
    alt!(
        StringMatch::exact(".eqv.", false),
        StringMatch::exact(".neqv.", false),
    ).map(EquivOp)
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

#[syntax_rule(
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
pub fn intrinsic_operator<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = IntrinsicOperator<S::Span>> + 'a {
    // NOTE: The order of the alternatives is important, is different from the standard
    alt!(
        power_op(cfg).map(IntrinsicOperator::PowerOp),
        rel_op(cfg).map(IntrinsicOperator::RelOp),
        concat_op(cfg).map(IntrinsicOperator::ConcatOp),
        mult_op(cfg).map(IntrinsicOperator::MultOp),
        add_op(cfg).map(IntrinsicOperator::AddOp),
        not_op(cfg).map(IntrinsicOperator::NotOp),
        and_op(cfg).map(IntrinsicOperator::AndOp),
        or_op(cfg).map(IntrinsicOperator::OrOp),
        equiv_op(cfg).map(IntrinsicOperator::EquivOp),
    )
}

// TODO test?
#[syntax_rule(
    F18V007r1 rule "extended-intrinsic-op" #610 : "is intrinsic-operator",
)]
pub fn extended_intrinsic_op<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = IntrinsicOperator<S::Span>> + 'a {
    intrinsic_operator(cfg)
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
//#[syntax_rule(
//    F18V007r1 rule "label" #611 : "is digit [ digit [ digit [ digit [ digit ] ] ] ]",
//)]
//pub fn label<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = Label<S::Span>> + 'a {
//    digit(cfg)
//        .then(|first| fold_many(
//            digit(cfg),
//            move || StringMatch::from_char(first.clone()),
//            |mut string, digit| {
//                string.push_char::<S>(digit);
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

#[syntax_rule(
    F18V007r1 rule "label" #611 : "is digit [ digit [ digit [ digit [ digit ] ] ] ]",
)]
pub fn label<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = Label<S::Span>> + 'a {
    // TODO implement rule (max 5 digits and no kind) and clause somewhere else
    digit_string::<S>(cfg).map(|s| Label({
        let span = s.span.clone();
        IntLiteralConstant {
            span,
            digits: s,
            kind_param: None,
        }
    }))
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

#[syntax_rule(
    F18V007r1 rule "defined-unary-op" #1003 : "is . letter [ letter ] ... .",
)]
pub fn defined_unary_op<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DefinedUnaryOrBinaryOp<S::Span>> + 'a {
    (
        '.',
        fold_many(
            letter(cfg),
            || StringMatch::empty::<S>(),
            |mut m, l| {
                m.push_char::<S>(l);
                (m, true)
            },
            1..,
        ),
        '.',
    ).map(|(o, mut m, c)| {
        m.push_front_char::<S>(o);
        m.push_char::<S>(c);
        m
    })
    .map(|m| DefinedUnaryOrBinaryOp { m })
}

#[syntax_rule(
    F18V007r1 rule "defined-binary-op" #1023 : "is . letter [ letter ] ... .",
)]
pub fn defined_binary_op<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DefinedUnaryOrBinaryOp<S::Span>> + 'a {
    (
        '.',
        fold_many(
            letter(cfg),
            || StringMatch::empty::<S>(),
            |mut m, l| {
                m.push_char::<S>(l);
                (m, true)
            },
            1..,
        ),
        '.',
    ).map(|(o, mut m, c)| {
        m.push_front_char::<S>(o);
        m.push_char::<S>(c);
        m
    })
    .map(|m| DefinedUnaryOrBinaryOp { m })
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

#[syntax_rule(
    F18V007r1 rule "defined-operator" #609 :
    "is defined-unary-op"
    "or defined-binary-op"
    "or extended-intrinsic-op",
)]
pub fn defined_operator<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DefinedOperator<S::Span>> + 'a {
    alt!(
        extended_intrinsic_op(cfg).map(DefinedOperator::IntrinsicEx),
        defined_unary_op(cfg).map(DefinedOperator::DefinedUnaryOrBinary),
        defined_binary_op(cfg).map(DefinedOperator::DefinedUnaryOrBinary),
    )
}

pub fn non_complex_literal_constant<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = NonComplexLiteralConstant<S::Span>> + 'a {
    alt! {
        // note: we try to parse the most specific first, this is the reason for real to be first, otherwise int would always be parsed
        real_literal_constant(cfg).map(NonComplexLiteralConstant::Real),
        int_literal_constant(cfg).map(NonComplexLiteralConstant::Int),
        logical_literal_constant(cfg).map(NonComplexLiteralConstant::Logical),
        char_literal_constant(cfg).map(NonComplexLiteralConstant::Char),
        boz_literal_constant(cfg).map(NonComplexLiteralConstant::Boz),
    }
}

// TODO use cfg
pub fn delimiter<'a, S: TextSource + 'a>(_cfg: &'a Cfg) -> impl Parser<S, Token = SpecialCharacterMatch<S::Span>> + 'a {
    alt!(
        SpecialCharacter::LeftParenthesis,
        SpecialCharacter::RightParenthesis,
        SpecialCharacter::LeftSquareBracket,
        SpecialCharacter::RightSquareBracket,
        SpecialCharacter::LeftCurlyBracket,
        SpecialCharacter::RightCurlyBracket,
    )
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

#[syntax_rule(
    F18V007r1 rule "lexical-token" section "6.2.1",
)]
pub fn lexical_token<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = LexicalToken<S::Span>> + 'a {
    alt!{
        non_complex_literal_constant(cfg).map(LexicalToken::LiteralConstant),
        name(cfg, false).map(LexicalToken::Name),
        delimiter(cfg).map(LexicalToken::Delimiter),
        defined_operator(cfg).map(LexicalToken::Operator),
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
    }
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
pub fn comment_start<S: TextSource>() -> impl Parser<S, Token = S::Span> {
    // we accept both ! and c as comment starters
    //ExactMatch::exact("!", false).or(ExactMatch::exact("c ", true))
    fold_many(
        StringMatch::exact("!", false),
        || S::Span::new_null(),
        |s, m| (S::Span::merge(s, m.span), true),
        1..,
    )
}

// TODO ???
pub fn line_comment<S: TextSource>() -> impl Parser<S, Token = LineComment<S::Span>> {
    comment_start().then(|bang_span: S::Span| {
        many_until(Char::<S::Span>::any(), eol().do_not_consume(), 0..).map(move |(chars, _newline)| {
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
}

// TODO ???
pub fn nl<S: TextSource>(source: S) -> PResult<(), S> {
    alt! {
        "\n\r".map(|_| ()),
        "\n".map(|_| ()),
        "\r".map(|_| ()),
    }
        .map(|_| ())
        .parse(source)
}

// TODO ???
/// End of line
pub fn eol<S: TextSource>() -> impl Parser<S, Token = ()> {
    alt! {
        StringMatch::exact("\r\n", true),
        StringMatch::exact("\n", true),
    }
    .map(|_| ())
    .or(eof())
    .map(|o| o.inner())
}

#[cfg(test)]
mod test {
    use crate::test_configs;

    use super::*;

    #[test]
    pub fn test_alphanumeric_character() {
        for ref cfg in test_configs() {
            assert_eq!(alphanumeric_character(cfg).parses("a"), true);
            assert_eq!(alphanumeric_character(cfg).parses("A"), true);
            assert_eq!(alphanumeric_character(cfg).parses("3"), true);
            assert_eq!(alphanumeric_character(cfg).parses("_"), true);
            assert_eq!(alphanumeric_character(cfg).parses(" "), false);
            assert_eq!(alphanumeric_character(cfg).parses("="), false);
        }
    }

    #[test]
    pub fn test_letter() {
        for ref cfg in test_configs() {
            assert_eq!(letter(cfg).parses("a"), true);
            assert_eq!(letter(cfg).parses("A"), true);
            assert_eq!(letter(cfg).parses("3"), false);
            assert_eq!(letter(cfg).parses("_"), false);
        }
    }

    #[test]
    pub fn test_digit() {
        for ref cfg in test_configs() {
            assert_eq!(digit(cfg).parses("a"), false);
            assert_eq!(digit(cfg).parses("A"), false);
            assert_eq!(digit(cfg).parses("3"), true);
            assert_eq!(digit(cfg).parses("_"), false);
        }
    }

    #[test]
    pub fn test_underscore() {
        for ref cfg in test_configs() {
            assert_eq!(underscore(cfg).parses("a"), false);
            assert_eq!(underscore(cfg).parses("A"), false);
            assert_eq!(underscore(cfg).parses("3"), false);
            assert_eq!(underscore(cfg).parses("_"), true);
        }
    }

    #[test]
    pub fn test_special_character() {
        use SpecialCharacter::*;
        for ref cfg in test_configs() {
            assert_eq!(special_character(cfg).parse(" ").unwrap().0.character, Blank);
            assert_eq!(special_character(cfg).parse("=").unwrap().0.character, Equals);
            assert_eq!(special_character(cfg).parse("+").unwrap().0.character, Plus);
            assert_eq!(special_character(cfg).parse("-").unwrap().0.character, Minus);
            assert_eq!(special_character(cfg).parse("*").unwrap().0.character, Asterisk);
            assert_eq!(special_character(cfg).parse("/").unwrap().0.character, Slash);
            assert_eq!(special_character(cfg).parse("\\").unwrap().0.character, Backslash);
            assert_eq!(special_character(cfg).parse("(").unwrap().0.character, LeftParenthesis);
            assert_eq!(special_character(cfg).parse(")").unwrap().0.character, RightParenthesis);
            assert_eq!(special_character(cfg).parse("[").unwrap().0.character, LeftSquareBracket);
            assert_eq!(special_character(cfg).parse("]").unwrap().0.character, RightSquareBracket);
            assert_eq!(special_character(cfg).parse("{").unwrap().0.character, LeftCurlyBracket);
            assert_eq!(special_character(cfg).parse("}").unwrap().0.character, RightCurlyBracket);
            assert_eq!(special_character(cfg).parse(",").unwrap().0.character, Comma);
            assert_eq!(special_character(cfg).parse(".").unwrap().0.character, DecimalPointOrPeriod);
            assert_eq!(special_character(cfg).parse(":").unwrap().0.character, Colon);
            assert_eq!(special_character(cfg).parse(";").unwrap().0.character, SemiColon);
            assert_eq!(special_character(cfg).parse("!").unwrap().0.character, ExclamationPoint);
            assert_eq!(special_character(cfg).parse("\"").unwrap().0.character, QuotationMarkOrQuote);
            assert_eq!(special_character(cfg).parse("%").unwrap().0.character, Percent);
            assert_eq!(special_character(cfg).parse("&").unwrap().0.character, Ampersand);
            assert_eq!(special_character(cfg).parse("~").unwrap().0.character, Tilde);
            assert_eq!(special_character(cfg).parse("<").unwrap().0.character, LessThan);
            assert_eq!(special_character(cfg).parse(">").unwrap().0.character, GreaterThan);
            assert_eq!(special_character(cfg).parse("?").unwrap().0.character, QuestionMark);
            assert_eq!(special_character(cfg).parse("'").unwrap().0.character, Apostrophe);
            assert_eq!(special_character(cfg).parse("`").unwrap().0.character, GraveAccent);
            assert_eq!(special_character(cfg).parse("^").unwrap().0.character, CircumflexAccent);
            assert_eq!(special_character(cfg).parse("|").unwrap().0.character, VerticalLine);
            assert_eq!(special_character(cfg).parse("¤").unwrap().0.character, CurrencySymbol);
            assert_eq!(special_character(cfg).parse("#").unwrap().0.character, NumberSign);
            assert_eq!(special_character(cfg).parse("@").unwrap().0.character, CommercialAt);
            assert_eq!(special_character(cfg).parses("a"), false);
            assert_eq!(special_character(cfg).parses("A"), false);
            assert_eq!(special_character(cfg).parses("3"), false);
            assert_eq!(special_character(cfg).parses("_"), false);
        }
    }

    #[test]
    pub fn test_name() {
        for ref cfg in test_configs() {
            assert_eq!(name(cfg, false).parse("some_name").unwrap().0.0.value, "some_name");
            assert_eq!(name(cfg, false).parse("some_name ").unwrap().0.0.value, "some_name");
            assert_eq!(name(cfg, false).parses(" some_name"), false);

            assert_eq!(name(cfg, false).parse("A1 ").unwrap().0.0.value, "A1");
            assert_eq!(name(cfg, false).parse("NAME_LENGTH ").unwrap().0.0.value, "NAME_LENGTH");
            assert_eq!(name(cfg, false).parse("S_P_R_E_A_D__O_U_T ").unwrap().0.0.value, "S_P_R_E_A_D__O_U_T");
            assert_eq!(name(cfg, false).parse("TRAILER_ ").unwrap().0.0.value, "TRAILER_");
            assert_eq!(name(cfg, true).parse("TRAILER_ ").unwrap().0.0.value, "TRAILER");
            assert_eq!(name(cfg, true).parse("TRAILER_ ").unwrap().1, "_ ");

            assert_eq!(name(cfg, false).parse("a").unwrap().0.0.value, "a");
            assert_eq!(name(cfg, false).parse("a ").unwrap().0.0.value, "a");
            assert_eq!(name(cfg, false).parse("A%").unwrap().0.0.value, "A");
            assert_eq!(name(cfg, false).parse("A ").unwrap().0.0.value, "A");
            assert_eq!(name(cfg, false).parses("3+"), false);
            assert_eq!(name(cfg, false).parses("_"), false);
            assert_eq!(name(cfg, false).parse("a3|").unwrap().0.0.value, "a3");
            assert_eq!(name(cfg, false).parse("a3 ").unwrap().0.0.value, "a3");
            assert_eq!(name(cfg, false).parse("a_").unwrap().0.0.value, "a_");
            assert_eq!(name(cfg, false).parse("a_! ").unwrap().0.0.value, "a_");
            assert_eq!(name(cfg, false).parse("a3_").unwrap().0.0.value, "a3_");
            assert_eq!(name(cfg, false).parse("a3_ ").unwrap().0.0.value, "a3_");
        }
    }

    #[test]
    fn test_power_op() {
        for cfg in test_configs() {
            let parser = power_op(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parses("*"), false);
            assert_eq!(parser.parses("**"), true);
        }
    }

    #[test]
    fn test_mult_op() {
        for cfg in test_configs() {
            let parser = mult_op(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parses("*"), true);
            assert_eq!(parser.parses("/"), true);
            assert_eq!(parser.parses("**"), true); // TODO maybe false??
            assert_eq!(parser.parses("//"), true); // TODO maybe false??
        }
    }

    #[test]
    fn test_add_op() {
        for cfg in test_configs() {
            let parser = add_op(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parses("+"), true);
            assert_eq!(parser.parses("-"), true);
            assert_eq!(parser.parses("**"), false);
        }
    }

    #[test]
    fn test_concat_op() {
        for cfg in test_configs() {
            let parser = concat_op(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parses("//"), true);
            assert_eq!(parser.parses("**"), false);
        }
    }

    #[test]
    fn test_rel_op() {
        for cfg in test_configs() {
            let parser = rel_op(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parse(".eq. ").unwrap().0.0.value(), ".eq.");
            assert_eq!(parser.parse(".ne. ").unwrap().0.0.value(), ".ne.");
            assert_eq!(parser.parse(".lt. ").unwrap().0.0.value(), ".lt.");
            assert_eq!(parser.parse(".le. ").unwrap().0.0.value(), ".le.");
            assert_eq!(parser.parse(".gt. ").unwrap().0.0.value(), ".gt.");
            assert_eq!(parser.parse(".ge. ").unwrap().0.0.value(), ".ge.");
            assert_eq!(parser.parse("== ").unwrap().0.0.value(), "==");
            assert_eq!(parser.parse("/= ").unwrap().0.0.value(), "/=");
            assert_eq!(parser.parse("<= ").unwrap().0.0.value(), "<=");
            assert_eq!(parser.parse(">= ").unwrap().0.0.value(), ">=");
            assert_eq!(parser.parse("< ").unwrap().0.0.value(), "<");
            assert_eq!(parser.parse("> ").unwrap().0.0.value(), ">");
            assert_eq!(parser.parses("**"), false);
        }
    }

    #[test]
    fn test_not_op() {
        for cfg in test_configs() {
            let parser = not_op(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parse(".not. ").unwrap().0.0.value(), ".not.");
            assert_eq!(parser.parses("**"), false);
        }
    } 

    #[test]
    fn test_and_op() {
        for cfg in test_configs() {
            let parser = and_op(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parse(".and. ").unwrap().0.0.value(), ".and.");
            assert_eq!(parser.parses("**"), false);
        }
    }

    #[test]
    fn test_or_op() {
        for cfg in test_configs() {
            let parser = or_op(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parse(".or. ").unwrap().0.0.value(), ".or.");
            assert_eq!(parser.parses("**"), false);
        }
    }

    #[test]
    fn test_equiv_op() {
        for cfg in test_configs() {
            let parser = equiv_op(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parse(".eqv. ").unwrap().0.0.value(), ".eqv.");
            assert_eq!(parser.parse(".neqv. ").unwrap().0.0.value(), ".neqv.");
            assert_eq!(parser.parses("**"), false);
        }
    }

    #[test]
    fn test_intrinsic_operator() {
        for cfg in test_configs() {
            let parser = intrinsic_operator(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parse(".eqv. ").unwrap().0.is_equiv_op(), true);
            assert_eq!(parser.parse(".neqv. ").unwrap().0.is_equiv_op(), true);
            assert_eq!(parser.parse(".eq. ").unwrap().0.is_rel_op(), true);
            assert_eq!(parser.parse(".ne. ").unwrap().0.is_rel_op(), true);
            assert_eq!(parser.parse(".lt. ").unwrap().0.is_rel_op(), true);
            assert_eq!(parser.parse(".le. ").unwrap().0.is_rel_op(), true);
            assert_eq!(parser.parse(".gt. ").unwrap().0.is_rel_op(), true);
            assert_eq!(parser.parse(".ge. ").unwrap().0.is_rel_op(), true);
            assert_eq!(parser.parse("== ").unwrap().0.is_rel_op(), true);
            assert_eq!(parser.parse("/= ").unwrap().0.is_rel_op(), true);
            assert_eq!(parser.parse("<= ").unwrap().0.is_rel_op(), true);
            assert_eq!(parser.parse(">= ").unwrap().0.is_rel_op(), true);
            assert_eq!(parser.parse("< ").unwrap().0.is_rel_op(), true);
            assert_eq!(parser.parse("> ").unwrap().0.is_rel_op(), true);
            assert_eq!(parser.parse(".not. ").unwrap().0.is_not_op(), true);
            assert_eq!(parser.parse(".and. ").unwrap().0.is_and_op(), true);
            assert_eq!(parser.parse(".or. ").unwrap().0.is_or_op(), true);
            assert_eq!(parser.parse("**").unwrap().0.is_power_op(), true);
            assert_eq!(parser.parse("*").unwrap().0.is_mult_op(), true);
            assert_eq!(parser.parse("/").unwrap().0.is_mult_op(), true);
            assert_eq!(parser.parse("+").unwrap().0.is_add_op(), true);
            assert_eq!(parser.parse("-").unwrap().0.is_add_op(), true);
            assert_eq!(parser.parse("//").unwrap().0.is_concat_op(), true);
        }
    }

    #[test]
    fn test_defined_unary_op() {
        for cfg in test_configs() {
            let parser = defined_unary_op(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parse(".foo.").unwrap().0.m.value(), ".foo.");
            assert_eq!(parser.parses(".foo"), false);
            assert_eq!(parser.parse(".foo.bar.").unwrap().0.m.value(), ".foo.");
        }
    }

    #[test]
    fn test_defined_binary_op() {
        for cfg in test_configs() {
            let parser = defined_binary_op(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parse(".foo.").unwrap().0.m.value(), ".foo.");
            assert_eq!(parser.parses(".foo"), false);
            assert_eq!(parser.parse(".foo.bar.").unwrap().0.m.value(), ".foo.");
        }
    }

    #[test]
    fn test_defined_operator() {
        for cfg in test_configs() {
            let parser = defined_operator(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parse(".foo.").unwrap().0.is_defined_unary_or_binary(), true);
            assert_eq!(parser.parse(".eqv. ").unwrap().0.is_intrinsic_ex(), true);
            assert_eq!(parser.parse("+ ").unwrap().0.is_intrinsic_ex(), true);
        }
    }

    #[test]
    pub fn test_non_complex_literal_constant() {
        for ref cfg in test_configs() {
            assert_eq!(non_complex_literal_constant(cfg).parses("a"), false);
            assert!(non_complex_literal_constant(cfg).parse("42").unwrap().0.is_int());
            assert!(non_complex_literal_constant(cfg).parse("42.0").unwrap().0.is_real());
            assert!(non_complex_literal_constant(cfg).parse("42e1").unwrap().0.is_real());
            assert!(non_complex_literal_constant(cfg).parse(".TRUE.").unwrap().0.is_logical());
            assert!(non_complex_literal_constant(cfg).parse("'a'").unwrap().0.is_char());
            assert!(non_complex_literal_constant(cfg).parse("B'1010'").unwrap().0.is_boz());
            assert!(non_complex_literal_constant(cfg).parse("O'123'").unwrap().0.is_boz());
            assert!(non_complex_literal_constant(cfg).parse("Z'ABC'").unwrap().0.is_boz());
        }
    }

    #[test]
    pub fn test_lexical_token() {
        for ref cfg in test_configs() {
            assert!(lexical_token(cfg).parse(".").unwrap().0.is_error());
            assert!(lexical_token(cfg).parse("ciao").unwrap().0.is_name());
            assert!(lexical_token(cfg).parse("42").unwrap().0.is_literal_constant());
            assert!(lexical_token(cfg).parse("'42'").unwrap().0.as_literal_constant().unwrap().is_char());
            assert!(lexical_token(cfg).parse(".true.").unwrap().0.as_literal_constant().unwrap().is_logical());
            assert_eq!(
                lexical_token(cfg)
                    .parse(">=").unwrap()
                    .0.as_operator().unwrap()
                    .as_intrinsic_ex().unwrap()
                    .as_rel_op().unwrap()
                    .0.value,
                ">=",
            );
            assert_eq!(
                lexical_token(cfg)
                    .parse("==").unwrap()
                    .0.as_operator().unwrap()
                    .as_intrinsic_ex().unwrap()
                    .as_rel_op().unwrap()
                    .0.value,
                "==",
            );
            
            assert!(lexical_token(cfg).parse(",").unwrap().0.is_comma());
            assert!(lexical_token(cfg).parse("=").unwrap().0.is_equals());
            assert!(lexical_token(cfg).parse("=>").unwrap().0.is_arrow());
            assert!(lexical_token(cfg).parse(":").unwrap().0.is_colon());
            assert!(lexical_token(cfg).parse("::").unwrap().0.is_double_colon());
            assert!(lexical_token(cfg).parse(";").unwrap().0.is_semicolon());
            assert!(lexical_token(cfg).parse("..").unwrap().0.is_dot_dot());
            assert!(lexical_token(cfg).parse("%").unwrap().0.is_percent());
        }
    }
}
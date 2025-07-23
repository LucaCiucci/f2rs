use std::ops::RangeBounds;

use enum_as_inner::EnumAsInner;
use f2rs_parser_combinator::prelude::*;

use crate::s_rule;

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

// TODO remove
pub fn blanks<S: TextSource>(range: impl RangeBounds<usize> + Clone + 'static) -> impl Parser<S, Token = ()> {
    fold_many(
        Parser::<S>::or(SpecialCharacter::Blank, SpecialCharacter::Tab), // NOTE: added Tab for convenience
        || (),
        |_, _| ((), true),
        range,
    )
}

// TODO remove
pub fn space<'a, S: TextSource + 'a>(min: usize) -> impl Parser<S, Token = ()> {
    blanks(min..).map(|_| ())
}

macro_rules! define_special_character {
    ($(
        $(#[$meta:meta])*
        $variant:ident = $character:expr,
    )*) => {
        /// Fortran special character
        ///
        /// Conforms to:
        /// - J3/18-007r1§6.1.5
        #[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, EnumAsInner)]
        pub enum SpecialCharacter {
            $(
                $(#[$meta])*
                $variant,
            )*
        }

        impl SpecialCharacter {
            pub const ALL: &'static [Self] = &[
                $(
                    SpecialCharacter::$variant,
                )*
            ];

            pub fn character(&self) -> char {
                use SpecialCharacter::*;
                match self {
                    $(
                        $variant => $character,
                    )*
                }
            }

            pub fn from_char(c: char) -> Option<Self> {
                use SpecialCharacter::*;
                Some(match c {
                    $(
                        $character => $variant,
                    )*
                    _ => return None,
                })
            }
        }
    };
}

define_special_character! {
    /// ` ` (blank)
    Blank = ' ',
    Tab = '\t', // NOTE: added for convenience
    /// `=` (equals sign)
    Equals = '=',
    /// `+` (plus sign)
    Plus = '+',
    /// `-` (minus sign)
    Minus = '-',
    /// `*` (asterisk)
    Asterisk = '*',
    /// `/` (slash)
    Slash = '/',
    /// `\` (backslash)
    Backslash = '\\',
    /// `(` (left parenthesis)
    LeftParenthesis = '(',
    /// `)` (right parenthesis)
    RightParenthesis = ')',
    /// `[` (left square bracket)
    LeftSquareBracket = '[',
    /// `]` (right square bracket)
    RightSquareBracket = ']',
    /// `{` (left curly bracket)
    LeftCurlyBracket = '{',
    /// `}` (right curly bracket)
    RightCurlyBracket = '}',
    /// `,` (comma)
    Comma = ',',
    /// `.` (decimal point or period)
    DecimalPointOrPeriod = '.',
    /// `:` (colon)
    Colon = ':',
    /// `;` (semicolon)
    SemiColon = ';',
    /// `!` (exclamation point)
    ExclamationPoint = '!',
    /// `"` (quotation mark or quote)
    QuotationMarkOrQuote = '"',
    /// `%` (percent sign)
    Percent = '%',
    /// `&` (ampersand)
    Ampersand = '&',
    /// `~` (tilde)
    Tilde = '~',
    /// `<` (less-than sign)
    LessThan = '<',
    /// `>` (greater-than sign)
    GreaterThan = '>',
    /// `?` (question mark)
    QuestionMark = '?',
    /// `'` (apostrophe)
    Apostrophe = '\'',
    /// `` ` `` (grave accent)
    GraveAccent = '`',
    /// `^` (circumflex accent)
    CircumflexAccent = '^',
    /// `|` (vertical line)
    VerticalLine = '|',
    /// `¤` (currency symbol)
    CurrencySymbol = '¤',
    /// `#` (number sign)
    NumberSign = '#',
    /// `@` (commercial at)
    CommercialAt = '@',
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
    F18V007r1 rule "alphanumeric-character" #601 :
    "is letter"
    "or digit"
    "or underscore",
)]
pub fn alphanumeric_character<S: TextSource>(source: S) -> PResult<Char<S::Span>, S> {
    None
        .or_else(|| letter.parse(source.clone()))
        .or_else(|| digit.parse(source.clone()))
        .or_else(|| underscore.parse(source))
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

just_wraps!(Name(StringMatch));

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
            seq!((
                first: letter,
                rest: many(alphanumeric_character, 0..),
            ) => {
                Name(StringMatch::from_chars::<S>(
                    std::iter::once(first).chain(rest.into_iter().map(|c| c)),
                ))
            }).parse(source)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{rule_test, tokenizer::examples};
    use super::*;

    rule_test! {
        special_character() {
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
    }

    rule_test! {
        alphanumeric_character(F18V007r1 601) {
            assert_eq!(alphanumeric_character.parses("a"), true);
            assert_eq!(alphanumeric_character.parses("A"), true);
            assert_eq!(alphanumeric_character.parses("3"), true);
            assert_eq!(alphanumeric_character.parses("_"), true);
            assert_eq!(alphanumeric_character.parses(" "), false);
            assert_eq!(alphanumeric_character.parses("="), false);
        }
    }

    rule_test! {
        letter() {
            assert_eq!(letter.parses("a"), true);
            assert_eq!(letter.parses("A"), true);
            assert_eq!(letter.parses("3"), false);
            assert_eq!(letter.parses("_"), false);
        }
    }

    rule_test! {
        digit() {
            assert_eq!(digit.parses("a"), false);
            assert_eq!(digit.parses("A"), false);
            assert_eq!(digit.parses("3"), true);
            assert_eq!(digit.parses("_"), false);
        }
    }

    rule_test! {
        underscore(F18V007r1 602) {
            assert_eq!(underscore.parses("a"), false);
            assert_eq!(underscore.parses("A"), false);
            assert_eq!(underscore.parses("3"), false);
            assert_eq!(underscore.parses("_"), true);
        }
    }

    rule_test! {
        name(F18V007r1 603) {
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

            // Examples from the standard
            examples(name(false), [
                "A1",
                "NAME_LENGTH",
                "S_P_R_E_A_D__O_U_T",
                "TRAILER_"
            ]);
        }
    }
}
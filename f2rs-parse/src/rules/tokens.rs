use std::{iter::once, ops::RangeBounds};

use enum_as_inner::EnumAsInner;
use enum_iterator::Sequence;
use f2rs_parser_combinator::prelude::*;
use f2rs_parse_derive::syntax_rule;

use crate::{Cfg, Standard::*};

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

/// Fortran underscore
#[syntax_rule(
    F18V007r1 rule "underscore" #602 : "is _",
)]
pub fn underscore<S: TextSource>(cfg: &Cfg) -> impl Parser<S, Token = Char<S::Span>> {
    Char::exact('_')
}

/// Fortran special character
///
/// Conforms to:
/// - J3/18-007r1§6.1.5
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, EnumAsInner, Sequence)]
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

/// Fortran special character
#[syntax_rule(
    F18V007r1 rule "special-character" section "6.1.5",
)]
pub fn special_character<S: TextSource>(cfg: &Cfg) -> impl Parser<S, Token = SpecialCharacterMatch<S::Span>> {
    // TODO use any
    move |source: S| {
        let mut sc = SpecialCharacter::Blank;
        loop {
            let r = sc.parse(source.clone());
            if r.is_some() {
                return r;
            }
            if let Some(next) = sc.next() {
                sc = next;
            } else {
                return None;
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Name<Span>(pub StringMatch<Span>);

/// Fortran name
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
                    once(first).chain(rest.into_iter().map(|c| c)),
                ))
            }).parse(source)
        }
    }
}

#[derive(Debug, Clone)]
pub struct Keyword<Span>(pub Name<Span>);

#[syntax_rule(
    F18V007r1 rule "keyword" #516 : "is name",
)]
pub fn keyword<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = Keyword<S::Span>> + 'a {
    name(cfg, false).map(Keyword)
}

#[syntax_rule(
    F18V007r1,
)]
pub fn kw<'a, S: TextSource + 'a>(keyword: &'static str, cfg: &'a Cfg) -> impl Parser<S, Token = Keyword<S::Span>> + 'a {
    StringMatch::exact(keyword, false).map(|m| Keyword(Name(m)))
}

pub fn blanks<'a, S: TextSource + 'a>(range: impl RangeBounds<usize> + Clone + 'a) -> impl Parser<S, Token = ()> + 'a {
    fold_many(
        Parser::<S>::or(SpecialCharacter::Blank, SpecialCharacter::Tab), // NOTE: added Tab for convenience
        || (),
        |_, _| ((), true),
        range,
    )
}

// TODO ???
#[derive(Debug, Clone)]
pub struct LineComment<Span> {
    pub text: String,
    pub span: Span,
}

// TODO ???
pub fn comment_start<S: TextSource>() -> impl Parser<S, Token = S::Span> {
    // we accept both ! and c as comment starters
    //ExactMatch::exact("!", false).or(ExactMatch::exact("c ", true))
    fold_many(
        StringMatch::exact("!", false),
        || S::null_span(),
        |s, m| (S::merge_span(s, m.span), true),
        1..,
    )
}

// TODO ???
pub fn line_comment<S: TextSource>() -> impl Parser<S, Token = LineComment<S::Span>> {
    comment_start().then(|bang_span: S::Span| {
        many_until(Char::<S::Span>::any(), '\n', 0..).map(move |(chars, _newline)| {
            let bang_span = bang_span.clone();
            let span = if let Some(last) = chars.last() {
                S::merge_span(bang_span, last.span.clone())
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
pub fn continuation<'a, S: TextSource + 'a>() -> impl Parser<S, Token = ()> + 'a {
    (
        '&', blanks(0..), nl.map(|_| ()).or(line_comment().map(|_| ())), // TODO keep comments
        blanks(0..), Char::exact('&').optional(),
        blanks(0..) // TODO remove
    ).map(|_| ())
}

// TODO ???
pub fn space<'a, S: TextSource + 'a>(min: usize) -> impl Parser<S, Token = ()> + 'a {
    //separated(
    //    blanks(0..),
    //    continuation(),
    //    0..,
    //).map(|_| ())
    (
        blanks(min..),
        separated(
            blanks(0..),
            continuation(),
            0..,
        ),
    ).map(|_| ())
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

// TODO ???
pub fn eol_or_comment<S: TextSource>() -> impl Parser<S, Token = Option<LineComment<S::Span>>> {
    alt! {
        eol().map(|_| None),
        (space(0), line_comment()).map(|(_, c)| Some(c)),
    }
}

// TODO ???
pub fn statement_termination<'a, S: TextSource + 'a>() -> impl Parser<S, Token = Option<LineComment<S::Span>>> + 'a {
    alt!(
        eol_or_comment(),
        (
            space(0),
            Char::exact(';'),
            space(0),
            eol_or_comment(),
        ).map(|(_, _, _, c)| c),
    )
}

// TODO test
#[syntax_rule(
    F18V007r1 rule "lbracket" #771 : "is [",
)]
pub fn lbracket<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ()> + 'a {
    '['.map(|_| ())
}

// TODO test
#[syntax_rule(
    F18V007r1 rule "rbracket" #772 : "is ]",
)]
pub fn rbracket<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ()> + 'a {
    ']'.map(|_| ())
}

#[derive(Debug, Clone)]
pub struct EmptyLines {
    pub count: usize,
}

pub fn empty_lines<'a, S: TextSource + 'a>() -> impl Parser<S, Token = EmptyLines> + 'a {
    fold_many(
        (many(' ', 0..), nl),
        || 0,
        |count, _| (count + 1, true),
        1..,
    )
    .map(|count| EmptyLines { count })
}

#[cfg(test)]
mod test {
    use crate::rules::test_configs;

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

    // TODO ???
    #[test]
    fn test_comment_start() {
        comment_start().parse("!").expect("failed to parse");
    }

    // TODO ???
    #[test]
    fn test_line_comment() {
        let r = line_comment().parse("! hello world\n").unwrap().0;
        assert_eq!(r.text, " hello world");
    }
}

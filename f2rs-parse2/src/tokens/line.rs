use std::ops::Range;

use colored::Color;
use enum_as_inner::EnumAsInner;
use f2rs_parser_combinator::prelude::*;

use super::rules::*;

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
            LexicalToken::Semicolon(s) => LexicalToken::Semicolon(s.map_span(f)),
            LexicalToken::DotDot(d) => LexicalToken::DotDot(d.map_span(f)),
            LexicalToken::Percent(p) => LexicalToken::Percent(p.map_span(f)),
            LexicalToken::Dot(d) => LexicalToken::Dot(d.map_span(f)),
            LexicalToken::Error(e) => LexicalToken::Error(e.map_span(f)),
        }
    }
}


#[derive(Debug, Clone)]
pub struct Line {
    pub chars: Vec<char>,
    pub content: TokenizedFreeLine<Range<usize>>,
}

#[derive(Debug, Clone)]
pub struct TokenizedFreeLine<Span> {
    /// "20"
    pub label: Option<Label<Span>>,
    /// "&"
    pub start_continuation_sign: Option<SpecialCharacterMatch<Span>>,

    pub tokens: Vec<LexicalToken<Span>>,

    /// "&"
    pub end_continuation_sign: Option<SpecialCharacterMatch<Span>>,

    /// "comment"
    pub comment: Option<LineComment<Span>>,
}

impl TokenizedFreeLine<Range<usize>> {
    pub fn parse_chars(chars: &[char]) -> Option<Self> {
        let (content, _) = tokenized_free_line()
            .parse(Chars::new(chars, 0))?;
        Some(content)
    }

    /// Prints the line
    pub fn dump(
        &self,
        chars: &[char],
        continuation_color: Option<Color>,
        comment_color: Option<Color>,
        name_color: Option<Color>,
        op_color: Option<Color>,
        delimiters_and_other_symbols: Option<Color>,
        literal_color: Option<Color>,
        error_color: Option<Color>,
    ) -> String {
        let mut chars = chars.iter().map(|c| c.to_string()).collect::<Vec<_>>();

        let mut colorize = |span: &Range<usize>, color: Option<Color>, on: bool| {
            let Some(color) = color else { return; };
            if span.is_null() { return; }
            let start = span.start;
            let end = span.end;
            use colored::Colorize;
            for i in start..end {
                if on {
                    chars[i] = chars[i].on_color(color).to_string();
                } else {
                    chars[i] = chars[i].color(color).to_string();
                }
            }
        };

        if let Some(l) = &self.label {
            colorize(l.span(), continuation_color, false);
        }

        if let Some(c) = &self.start_continuation_sign {
            colorize(c.span(), continuation_color, false);
        }

        if let Some(c) = &self.end_continuation_sign {
            colorize(c.span(), continuation_color, false);
        }

        if let Some(c) = &self.comment {
            colorize(c.span(), comment_color, false);
        }

        for token in &self.tokens {
            match token {
                LexicalToken::Name(name) => {
                    colorize(name.span(), name_color, false);
                },
                LexicalToken::Operator(op) => {
                    colorize(op.span(), op_color, false);
                },
                LexicalToken::Delimiter(t) => colorize(t.span(), delimiters_and_other_symbols, false),
                LexicalToken::Comma(t) => colorize(t.span(), delimiters_and_other_symbols, false),
                LexicalToken::Equals(t) => colorize(t.span(), delimiters_and_other_symbols, false),
                LexicalToken::Colon(t) => colorize(t.span(), delimiters_and_other_symbols, false),
                LexicalToken::Semicolon(t) => colorize(t.span(), delimiters_and_other_symbols, false),
                LexicalToken::Percent(t) => colorize(t.span(), delimiters_and_other_symbols, false),
                LexicalToken::Arrow(t) => colorize(t.span(), delimiters_and_other_symbols, false),
                LexicalToken::DotDot(t) => colorize(t.span(), delimiters_and_other_symbols, false),
                LexicalToken::LiteralConstant(t) => colorize(t.span(), literal_color, false),
                LexicalToken::Dot(t) => colorize(t.span(), delimiters_and_other_symbols, false),
                LexicalToken::Error(e) => colorize(e.span(), error_color, true),
            }
        }

        chars.join("")
    }
}

impl<Span> TokenizedFreeLine<Span> {
    pub fn is_empty_line(&self) -> bool {
        self.tokens.is_empty() && self.label.is_none() // Note: comment is not considered
    }

    /// Tries to group the lines into a group
    ///
    /// A group is a sequence of lines that are connected by an "end_continuation_sign"
    /// and possibly separated by empty lines. Returns the number of lines in the group.
    pub fn group<'a>(lines: impl IntoIterator<Item = &'a Self>) -> usize
    where
        Span: 'a,
    {
        let mut iter = lines.into_iter();

        let Some(line) = iter.next() else { return 0; };

        let mut count = 1;
        let mut continuation = line.end_continuation_sign.is_some();
        while continuation {
            let Some(line) = iter.next() else { break; };
            count += 1;
            continuation = line.end_continuation_sign.is_some() || line.tokens.is_empty();
        }

        count
    }
}

// TODO where is this defined?
//#[syntax_rule(
//    F18V007r1 rule 
//)]
pub fn tokenized_free_line<'a, S: TextSource + 'a>() -> impl Parser<S, Token = TokenizedFreeLine<S::Span>> {
    use f2rs_parser_combinator::prelude::*;
    use crate::tokens::rules::*;

    (
        space(0),
        label.optional(),
        space(0),
        special_character.condition(|c, _| c.character.is_ampersand()).optional(), // TODO use SpecialCharacter::Ampersand.optional(),
        space(0),
        many_until(
            (lexical_token, space(0)).map(|(t, _)| t),
            (
                (
                    SpecialCharacter::Ampersand,
                    space(0),
                ).map(|(c, _)| c).optional(),
                line_comment.optional(),
                eol,
            ),
            0..,
        )
    ).map(|(_, label, _, start_continuation_sign, _, (tokens, tail))| {
        let (end_continuation_sign, comment) = match tail {
            Some((c, comment, _)) => (c, comment),
            None => (None, None),
        };

        TokenizedFreeLine {
            label,
            start_continuation_sign,
            tokens,
            end_continuation_sign,
            comment,
        }
    })
}
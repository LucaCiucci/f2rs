

pub mod phases;
pub mod tokens;
pub mod statement;
mod cfg; use std::{char, ops::Range, rc::Rc};

pub use cfg::*;
use colored::Color;
use f2rs_parser_combinator::{provided::text::Chars, tokenization::{MapSpan, Parser, ParserCore, Source, SourceSpan, Spanned, TextSource}};
use statement::MultilineSpan;
use tokens::rules::{Label, LexicalToken, LineComment, SpecialCharacterMatch};

macro_rules! decl_rule {
    (
        $(#[$meta:meta])*
        $name:ident:
            $(
                $standard:ident rule $rule_name:literal $(# $rule_number:literal)? $(section $rule_section:literal)? $(: $($rule_text:literal)*)?,
            )*
    ) => {
        $(#[$meta])*
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        #[allow(non_camel_case_types)]
        pub struct $name;
    };
}
pub(crate) use decl_rule;

pub trait LexRule: Clone + Copy {
    type Token: MapSpan<Range<usize>>;
    fn parse<S: TextSource>(self, source: S) -> Option<Self::Token>;
}

decl_rule! {
    ignored_example_rule:
        F18V007r1 rule "xyz-list" #401 : "is xyz [ , xyz ] ...",
        F18V007r1 rule "xyz-name" #402 : "is name",
        F18V007r1 rule "scalar-xyz" #403 : "is xyz",
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
        let (content, _) = tokenized_free_line(&Cfg::f2018())
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
                LexicalToken::DoubleColon(t) => colorize(t.span(), delimiters_and_other_symbols, false),
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
pub fn tokenized_free_line<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = TokenizedFreeLine<S::Span>> + 'a {
    use f2rs_parser_combinator::prelude::*;
    use crate::tokens::rules::*;

    (
        space(0),
        label(cfg).optional(),
        space(0),
        special_character(cfg).condition(|c, _| c.character.is_ampersand()).optional(), // TODO use SpecialCharacter::Ampersand.optional(),
        space(0),
        many_until(
            (lexical_token(cfg), space(0)).map(|(t, _)| t),
            (
                (
                    SpecialCharacter::Ampersand,
                    space(0),
                ).map(|(c, _)| c).optional(),
                line_comment().optional(),
                eol(),
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

#[derive(Debug, Clone, Copy)]
pub struct LexSource<'a> {
    offset: usize,
    tokens: &'a [LexicalToken<MultilineSpan>],
}

impl<'a> Source for LexSource<'a> {
    type Element = LexicalToken<MultilineSpan>;
    type Index = usize;
    type Span = MultilineSpan;

    fn get_at<'s>(&'s self, index: &Self::Index) -> Option<Self::Element> {
        self.tokens.get(*index).cloned()
    }

    fn start(&self) -> Self::Index {
        0
    }

    fn full_span(&self) -> Self::Span {
        if self.empty() {
            MultilineSpan::new_null()
        } else {
            let start = self.tokens[0].span().clone();
            let end = self.tokens[self.tokens.len() - 1].span().clone();
            MultilineSpan::merge(start, end)
        }
    }

    fn next(&self, index: Self::Index, count: usize) -> Self::Index {
        (index + count).min(self.tokens.len())
    }

    fn make_span(&self, start: Self::Index, end: Self::Index) -> Self::Span {
        if start >= end {
            MultilineSpan::new_null()
        } else {
            let start = self.tokens[start].span().clone();
            let end = self.tokens[end - 1].span().clone();
            MultilineSpan::merge(start, end)
        }
    }

    fn tail(self, end: Self::Index) -> Self {
        LexSource {
            offset: self.offset + end,
            tokens: &self.tokens[end..],
        }
    }
}
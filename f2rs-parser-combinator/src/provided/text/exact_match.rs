

use std::{iter::once, ops::{Bound, RangeBounds}};

use serde::{Deserialize, Serialize};

use crate::tokenization::{PResult, ParserCore};

use super::*;

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct Char<Span> {
    pub value: char,
    pub span: Span,
}

impl<Span> Char<Span> {
    pub fn parse<S: TextSource<Span = Span>>(condition: impl Fn(char) -> bool + Clone) -> impl Parser<S, Token = Self> {
        move |source: S| {
            let value = source.get_at(&source.start());
            if let Some(value) = value {
                if condition(value) {
                    let end = source.next(source.start(), 1);
                    source.parsed_result(
                        end,
                        |span| Char {
                            value,
                            span,
                        }
                    )
                } else {
                    None
                }
            } else {
                None
            }
        }
    }

    pub fn exact<S: TextSource<Span = Span>>(value: char) -> impl Parser<S, Token = Self> {
        Self::parse(move |c| c == value)
    }

    pub fn exact_case_insensitive<S: TextSource<Span = Span>>(value: char) -> impl Parser<S, Token = Self> {
        // TODO to check, unicode?
        Self::parse(move |c| c.to_ascii_lowercase() == value.to_ascii_lowercase())
    }

    pub fn any<S: TextSource<Span = Span>>() -> impl Parser<S, Token = Self> {
        Self::parse(|_| true)
    }

    pub fn any_of<S: TextSource<Span = Span>>(chars: impl IntoIterator<Item = char> + Clone) -> impl Parser<S, Token = Self> {
        Self::parse(move |c| {
            for char in chars.clone() {
                if c == char {
                    return true;
                }
            }
            false
        })
    }

    pub fn any_except<S: TextSource<Span = Span>>(chars: impl IntoIterator<Item = char> + Clone) -> impl Parser<S, Token = Self> {
        Self::parse(move |c| {
            for char in chars.clone() {
                if c == char {
                    return false;
                }
            }
            true
        })
    }

    pub fn white<S: TextSource<Span = Span>>() -> impl Parser<S, Token = Self> {
        Self::any_of(" \t\n\r".chars())
    }

    pub fn space<S: TextSource<Span = Span>>() -> impl Parser<S, Token = Self> {
        Self::any_of(" \t".chars())
    }

    pub fn digit<S: TextSource<Span = Span>>() -> impl Parser<S, Token = Self> {
        Self::parse(|c| c.is_digit(10))
    }

    pub fn alpha<S: TextSource<Span = Span>>() -> impl Parser<S, Token = Self> {
        Self::parse(|c| c.is_alphabetic() || c == '_')
    }

    pub fn alphanumeric<S: TextSource<Span = Span>>() -> impl Parser<S, Token = Self> {
        Self::parse(|c| c.is_alphanumeric() || c == '_')
    }
}

impl<S: TextSource> ParserCore<S> for char {
    type Token = Char<S::Span>;
    fn parse(&self, source: S) -> PResult<Self::Token, S> {
        Char::exact(*self).parse(source)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StringMatch<Span> {
    pub span: Span,
    pub value: String,
}

impl<Span: Clone> StringMatch<Span> {
    pub fn empty<S: Source<Span = Span>>() -> Self {
        Self {
            span: S::null_span(),
            value: String::new(),
        }
    }

    pub fn from_char(char: Char<Span>) -> Self {
        Self {
            span: char.span,
            value: String::from_iter(once(char.value)),
        }
    }

    pub fn from_chars<S: Source<Span = Span>>(
        chars: impl IntoIterator<Item = Char<Span>>,
    ) -> Self {
        let mut s = S::null_span();
        let mut value = String::new();
        for c in chars {
            value.push(c.value);
            s = S::merge_span(s, c.span);
        }
        Self {
            span: s,
            value,
        }
    }

    pub fn push_char<S: Source<Span = Span>>(&mut self, c: Char<Span>) {
        self.span = S::merge_span(self.span.clone(), c.span);
        self.value.push(c.value);
    }

    pub fn push_front_char<S: Source<Span = Span>>(&mut self, c: Char<Span>) {
        self.span = S::merge_span(c.span, self.span.clone());
        self.value.insert(0, c.value);
    }

    pub fn exact<'a, S: TextSource<Span = Span>>(value: &'a str, case_sensitive: bool) -> impl Parser<S, Token = Self> + 'a {
        move |source: S| {
            let mut index = source.start();
            for c in value.chars() {
                if case_sensitive {
                    if source.get_at(&index) != Some(c) {
                        return None;
                    }
                } else {
                    if source.get_at(&index).map(|c| c.to_ascii_lowercase()) != Some(c.to_ascii_lowercase()) {
                        return None;
                    }
                }
                index = source.next(index, 1);
            }

            source.parsed_result(
                index,
                |span| StringMatch {
                    span,
                    value: value.to_string(),
                }
            )
        }
    }

    pub fn match_while<'a, S: TextSource<Span = Span>>(
        condition: impl Fn(char, usize) -> bool + Clone + 'a,
        len: impl RangeBounds<usize> + Clone + 'a,
    ) -> impl Parser<S, Token = Self> + 'a {
        move |mut source: S| {
            let mut count = 0;
            let mut value = String::new();
            let mut span = source.make_span(source.start(), source.start());
            while let Some(c) = source.get_at(&source.start()) {
                match len.end_bound() {
                    Bound::Included(&max) if count + 1 > max => break,
                    Bound::Excluded(&max) if count + 1 >= max => break,
                    _ => {}
                }

                if !condition(c, count) {
                    break;
                }
                value.push(c);
                let next = source.next(source.start(), 1);
                let new_span = source.make_span(source.start(), next.clone());
                span = S::merge_span(span, new_span);
                source = source.tail(next);
                count += 1;
            }

            if !len.contains(&count) {
                return None;
            }

            Some((
                StringMatch {
                    span,
                    value,
                },
                source,
            ))
        }
    }

    pub fn value(&self) -> &str {
        &self.value
    }
}

impl<S: TextSource> ParserCore<S> for &'static str {
    type Token = StringMatch<S::Span>;
    fn parse(&self, source: S) -> PResult<Self::Token, S> {
        StringMatch::exact(*self, true).parse(source)
    }
}

impl<Span> TokenTree<Span> for StringMatch<Span> {
    fn span<'s>(&'s self) -> &'s Span {
        &self.span
    }
}
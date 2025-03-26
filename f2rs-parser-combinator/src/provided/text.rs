use std::{fmt::Display, ops::Range};

use crate::tokenization::{Parser, Source, TokenTree, TextSource};

mod exact_match; pub use exact_match::*;
mod space; pub use space::*;

#[derive(Debug, Clone)]
pub struct CharSource<'a> {
    chars: &'a[char],
    offset: usize,
}

impl<'a> CharSource<'a> {
    pub fn new(chars: &'a[char], offset: usize) -> Self {
        CharSource { chars, offset }
    }

    pub fn chars(&self) -> &'a[char] {
        self.chars
    }
}

impl Display for CharSource<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.chars.iter().collect::<String>())
    }
}

impl<'a> Source for CharSource<'a> {
    type Element = char;
    type Index = usize;
    type Span = Range<usize>;

    fn get_at<'s>(&'s self, index: &Self::Index) -> Option<Self::Element> {
        self.chars.get(*index).cloned()
    }
    fn start(&self) -> Self::Index {
        0
    }
    fn next(&self, index: Self::Index, count: usize) -> Self::Index {
        (index + count).min(self.chars.len())
    }
    fn make_span(&self, start: Self::Index, end: Self::Index) -> Self::Span {
        (self.offset + start)..(self.offset + end)
    }

    fn tail(self, end: Self::Index) -> Self {
        CharSource {
            chars: &self.chars[end..],
            offset: self.offset + end,
        }
    }

    fn full_span(&self) -> Self::Span {
        self.make_span(0, self.chars.len())
    }
}

impl<'a> TextSource for CharSource<'a> {
}

impl Source for &str {
    type Element = char;
    type Index = usize;
    type Span = ();

    fn get_at<'s>(&'s self, index: &Self::Index) -> Option<Self::Element> {
        let s = &self[*index..];
        s.chars().next()
    }

    fn start(&self) -> Self::Index {
        0
    }

    fn next(&self, index: Self::Index, count: usize) -> Self::Index {
        if index >= self.len() {
            self.len()
        } else {
            let s = &self[index..];
            s.char_indices().nth(count).map(|(i, _)| i + index).unwrap_or(self.len())
        }
    }

    fn make_span(&self, _start: Self::Index, _end: Self::Index) -> Self::Span {
        ()
    }

    fn tail(self, end: Self::Index) -> Self {
        // TODO !!! print!("{}", &self[..end]);
        &self[end..]
    }

    fn full_span(&self) -> Self::Span {
        ()
    }
}

impl TextSource for &str {
}

// TODO impl Source for (usize, &str) {
//}
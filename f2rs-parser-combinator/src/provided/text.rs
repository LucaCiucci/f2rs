use crate::tokenization::{Parser, Source, TokenTree, TextSource};

mod exact_match; pub use exact_match::*;
mod space; pub use space::*;
pub mod rusty; use rusty::*;

pub struct TextSrc<'a> {
    _text: &'a str,
    char_indices: Vec<(usize, char)>,
}

impl<'a> TextSrc<'a> {
    pub fn new(text: &'a str) -> Self {
        TextSrc {
            _text: text,
            char_indices: text.char_indices().collect(),
        }
    }

    pub fn text(&self) -> Text {
        Text {
            char_indices: &self.char_indices,
            offset: 0,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Text<'a> {
    char_indices: &'a[(usize, char)],
    offset: usize,
}

impl<'a> Source for Text<'a> {
    type Element = char;
    type Index = usize;
    type Span = Option<(usize, usize)>;

    fn get_at<'s>(&'s self, index: &Self::Index) -> Option<Self::Element> {
        self.char_indices.get(*index).map(|(_, c)| *c)
    }
    fn start(&self) -> Self::Index {
        0
    }
    fn next(&self, index: Self::Index, count: usize) -> Self::Index {
        (index + count).min(self.char_indices.len())
    }
    fn make_span(&self, start: Self::Index, end: Self::Index) -> Self::Span {
        Some((start + self.offset, end + self.offset))
    }
    fn merge_span(a: Self::Span, b: Self::Span) -> Self::Span {
        //(a.0.min(b.0), a.1.max(b.1))
        match (a, b) {
            (Some(a), Some(b)) => Some((a.0.min(b.0), a.1.max(b.1))),
            (Some(a), None) => Some(a),
            (None, Some(b)) => Some(b),
            (None, None) => None,
        }
    }
    fn tail(self, end: Self::Index) -> Self {
        Text {
            char_indices: &self.char_indices[end..],
            offset: self.offset + end,
        }
    }
    fn null_span() -> Self::Span {
        None
    }
}

impl<'a> TextSource for Text<'a> {
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

    fn merge_span(_a: Self::Span, _b: Self::Span) -> Self::Span {
        ()
    }

    fn tail(self, end: Self::Index) -> Self {
        // TODO !!! print!("{}", &self[..end]);
        &self[end..]
    }

    fn null_span() -> Self::Span {
        ()
    }
}

impl TextSource for &str {
}
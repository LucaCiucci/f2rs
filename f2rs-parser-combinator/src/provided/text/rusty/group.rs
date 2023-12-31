use crate::{provided::text::Char, tokenization::{Parser, TextSource}};

use super::{Token, tokens_until};


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Delimiter {
    /// `( ... )`
    Parenthesis,
    /// `[ ... ]`
    Bracket,
    /// `{ ... }`
    Brace,
}

#[derive(Debug, Clone)]
pub struct Group<Span> {
    pub open: Char<Span>,
    pub inner: Vec<Token<Span>>,
    pub close: Option<Char<Span>>,
    pub delimiter: Delimiter,
}

pub fn group<S: TextSource>() -> impl Parser<S, Token = Group<S::Span>> {
    Char::any_of("([{".chars())
        .then(|open| {
            let close = match open.value {
                '(' => ')',
                '[' => ']',
                '{' => '}',
                _ => unreachable!(),
            };

            (tokens_until(close)).map(move |(inner, close)| (open.clone(), inner, close))
        })
        .map(|(open, inner, close)| {
            let delimiter = match open.value {
                '(' => Delimiter::Parenthesis,
                '[' => Delimiter::Bracket,
                '{' => Delimiter::Brace,
                _ => unreachable!(),
            };

            Group {
                open,
                inner,
                close,
                delimiter,
            }
        })
}
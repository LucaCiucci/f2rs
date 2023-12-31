use std::ops::RangeBounds;

use crate::{provided::{text::Char, common::many}, tokenization::{TextSource, ParserCore, Parser}, alt};


#[derive(Debug, Clone)]
pub struct StringLiteral<Span> {
    pub opening: Char<Span>,
    pub content: Vec<StringElement<Span>>,
    pub closing: Char<Span>,
}

impl<Span> StringLiteral<Span> {
    pub fn value(&self) -> String {
        let mut s = String::new();
        for element in &self.content {
            match element {
                StringElement::Char(c) => s.push(c.value),
                StringElement::EscapeSequence(e) => s.push(e.value),
            }
        }
        s
    }
}

#[derive(Debug, Clone)]
pub enum StringElement<Span> {
    Char(Char<Span>),
    EscapeSequence(EscapeSequence<Span>),
}

#[derive(Debug, Clone)]
pub struct EscapeSequence<Span> {
    pub span: Span,
    pub value: char,
}

pub fn string_character<S: TextSource>(not: char) -> impl Parser<S, Token = Char<S::Span>> {
    Char::parse(move |c| c != not)
}

pub fn string_escape<S: TextSource>() -> impl Parser<S, Token = EscapeSequence<S::Span>> {
    let map = |from: char, to: char| from.map(move |c| (to, c.span));

    (
        '\\',
        alt!(
            map( 'n', '\n'),
            map( 'r', '\r'),
            map( 't', '\t'),
            map('\\', '\\'),
            map( '"',  '"'),
            map('\'', '\''),
            map( '`',  '`'),
            map( '0', '\0'),
            map( 'x', 'x' ),
            // TODO unicode escapes
        ),
    ).map(
        |(backslash, (value, span))| EscapeSequence {
            span: S::merge_span(backslash.span, span),
            value,
        }
    )
}

pub fn string_element<S: TextSource>(not: char) -> impl Parser<S, Token = StringElement<S::Span>> {
    alt! {
        string_escape().map(|escape| StringElement::EscapeSequence(escape)),
        string_character(not).map(|char| StringElement::Char(char)),
    }
}

pub fn string_literal_impl<S: TextSource, R: RangeBounds<usize> + Clone>(
    len: impl Fn(char) -> R + 'static + Clone,
) -> impl Parser<S, Token = StringLiteral<S::Span>> {
    Char::any_of(r##""'`"##.chars())
        .then(move |opening| {
            many(string_element(opening.value), len(opening.value))
                .map(move |content| (opening.clone(), content))
        })
        .then(|(opening, content)| {
            Char::exact(opening.value)
                .map(move |closing| (opening.clone(), content.clone(), closing))
        })
        .map(|(opening, content, closing)| StringLiteral {
            opening,
            content,
            closing,
        })
}

pub fn string_literal<S: TextSource>() -> impl Parser<S, Token = StringLiteral<S::Span>> {
    string_literal_impl(|c| if c == '\'' { 1..=1 } else { 0..=usize::MAX })
}
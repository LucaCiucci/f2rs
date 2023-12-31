use crate::{tokenization::{Parser, TextSource}, provided::{text::Char, common::{many, fold_many}}};


#[derive(Debug, Clone)]
pub struct Identifier<Span> {
    pub span: Span,
    pub value: String,
}

pub fn identifier<S: TextSource>() -> impl Parser<S, Token = Identifier<S::Span>> {
    // TODO optimize
    (
        Char::alpha(),
        many(Char::alphanumeric::<S>(), 0..),
    ).map(|(first, rest)| {
        let first_span = first.span;
        let last_span = if let Some(last) = rest.last() {
            last.span.clone()
        } else {
            first_span.clone()
        };
        let value = std::iter::once(first.value)
            .chain(rest.into_iter().map(|c| c.value))
            .collect::<String>();
        Identifier {
            span: S::merge_span(first_span, last_span),
            value,
        }
    })
}

pub fn identifier2<S: TextSource>() -> impl Parser<S, Token = Identifier<S::Span>> {
    Char::alpha()
        .then(|first: Char<S::Span>| {
            fold_many(
                Char::alphanumeric::<S>(),
                move || {
                    // TODO: this speeds up a little bit:
                    //let mut s = String::with_capacity(10);
                    //s.push(first.value);
                    //(first.span.clone(), s)
                    (first.span.clone(), String::from_iter(std::iter::once(first.value)))
                },
                |(span, mut string), c| {
                    let span = S::merge_span(span, c.span.clone());
                    string.push(c.value);
                    ((span, string), true)
                },
                0..,
            )
        })
        .map(|(span, value)| Identifier { span, value })
}
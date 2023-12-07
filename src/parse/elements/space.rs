use std::ops::RangeBounds;

use riddle::{alt, tokenization::{TextSource, Parser}, provided::{common::{fold_many, many, separated}, text::Char}};
use riddle::prelude::*;

pub fn nl<S: TextSource>() -> impl Parser<S, Token = ()> {
    alt! {
        "\n\r".map(|_| ()),
        "\n".map(|_| ()),
        "\r".map(|_| ()),
    }.map(|_| ())
}

pub fn sp<S: TextSource>(range: impl RangeBounds<usize> + Clone) -> impl Parser<S, Token = ()> {
    fold_many(
        Char::any_of(" \t".chars()).map(|_| ()),
        || (),
        |_, _| ((), true),
        range,
    )
}

pub fn continuation<S: TextSource>() -> impl Parser<S, Token = ()> {
    ('&', sp(0..), nl()).map(|_| ())
}

pub fn space<S: TextSource>() -> impl Parser<S, Token = ()> {
    separated(
        sp(0..),
        continuation(),
        0..,
    ).map(|_| ())
}

pub fn spaced<S: TextSource, T: Parser<S>>(parser: T) -> impl Parser<S, Token = T::Token> {
    (
        space(),
        parser,
        space(),
    ).map(|(_, token, _)| token)
}

#[derive(Debug, Clone)]
pub struct EmptyLines {
    pub count: usize,
}

pub fn empty_lines<S: TextSource>() -> impl Parser<S, Token = EmptyLines> {
    fold_many((many(' ', 0..), nl()), || 0, |count, _| (count + 1, true), 1..).map(|count| EmptyLines { count })
}

// TODO tests
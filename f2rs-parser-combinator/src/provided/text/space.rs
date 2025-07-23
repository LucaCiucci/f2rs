
use std::ops::RangeBounds;

use crate::{provided::common::fold_many, seq};

use super::*;

pub fn white_space<S: TextSource>(range: impl RangeBounds<usize> + Clone) -> impl Parser<S, Token = ()> {
    fold_many(
        Char::white(),
        || (),
        |_, _| ((), true),
        range,
    )
}

pub fn white_spaced<S: TextSource, T: Parser<S>>(parser: T) -> impl Parser<S, Token = T::Token> {
    seq!((
        _: white_space(0..),
        token: parser,
        _: white_space(0..),
    ) => token)
}

pub fn white_space_no_newline<S: TextSource>(range: impl RangeBounds<usize> + Clone) -> impl Parser<S, Token = ()> {
    fold_many(
        Char::any_of(" \t".chars()),
        || (),
        |_, _| ((), true),
        range,
    )
}

pub fn white_spaced_no_newline<S: TextSource, T: Parser<S>>(parser: T) -> impl Parser<S, Token = T::Token> {
    seq!((
        _: white_space_no_newline(0..),
        token: parser,
        _: white_space_no_newline(0..),
    ) => token)
}

pub fn eof<S: Source>() -> impl Parser<S, Token = ()> {
    move |source: S| {
        if source.empty() {
            let s = source.start();
            source.parsed_result(s, |_| ())
        } else {
            None
        }
    }
}
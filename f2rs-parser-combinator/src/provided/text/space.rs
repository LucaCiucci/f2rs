
use std::ops::RangeBounds;

use crate::provided::common::fold_many;

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
    (
        white_space(0..),
        parser,
        white_space(0..),
    ).map(|(_, token, _)| token)
}

pub fn eof<S: Source>() -> impl Parser<S, Token = ()> {
    move |source: S| {
        if source.empty() {
            let s = source.start();
            source.parsed_result(s, |_| ())
        } else {
            source.unparsed_result()
        }
    }
}
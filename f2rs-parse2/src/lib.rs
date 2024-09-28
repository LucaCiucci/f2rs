

pub mod phases;
pub mod tokens;
pub mod statement;
mod cfg; use std::{char, ops::Range};

pub use cfg::*;
use colored::Color;
use f2rs_parser_combinator::{provided::text::Chars, tokenization::{Parser, ParserCore, Source, SourceSpan, Spanned, TextSource}};
use statement::MultilineSpan;
use tokens::{rules::{Label, LineComment, SpecialCharacterMatch}, LexicalToken};

macro_rules! s_rule {
    (
        $(
            $standard:ident rule $rule_name:literal $(# $rule_number:literal)? $(section $rule_section:literal)? $(: $($rule_text:literal)*)?,
        )*
    ) => {
        "**TODO**"
        //paste::paste!{
        //    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        //    #[allow(non_camel_case_types)]
        //    pub struct [<_$name>];
//
        //    $(#[$meta])*
        //    pub const $name: [<_$name>] = [<_$name>];
        //}
    };
}
pub(crate) use s_rule;

#[doc = s_rule!(
    F18V007r1 rule "xyz-list" #401 : "is xyz [ , xyz ] ...",
    F18V007r1 rule "xyz-name" #402 : "is name",
    F18V007r1 rule "scalar-xyz" #403 : "is xyz",
)]
pub fn ignored_example_rules() {}

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

macro_rules! rule_test {
    ($name: ident ($(
        $rule_name:ident $rule_number:literal
    ),*) { $($code:tt)* }) => {
        paste::paste! {
            #[test]
            #[allow(non_snake_case)]
            fn [<test_ $name $(_ $rule_name _ $rule_number)*>]() {
                $($code)*
            }
        }
    };
}
pub(crate) use rule_test;

#[cfg(test)]
mod tests {
    rule_test! {
        foo(F18V007r1 401, F18V007r1 402, F18V007r1 403) {
            assert_eq!(1, 1);
        }
    }
}
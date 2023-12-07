use riddle::prelude::*;


pub fn keyword<S: TextSource>(keyword: &'static str) -> impl Parser<S, Token = S::Span> {
    ExactMatch::exact(keyword, false).map(|m| m.span)
}
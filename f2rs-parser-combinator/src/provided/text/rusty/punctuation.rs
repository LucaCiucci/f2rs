use crate::{tokenization::{TextSource, Parser}, provided::text::Char};



pub fn punctuation<S: TextSource>() -> impl Parser<S, Token = Char<S::Span>> {
    Char::any_of(".,;:!?+-*/%&|<>=#".chars())
}
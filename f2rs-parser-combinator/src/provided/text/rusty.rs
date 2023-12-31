

mod identifier;

pub use identifier::*;
mod literal; pub use literal::*;
mod punctuation; pub use punctuation::*;
mod group; pub use group::*;

use crate::{tokenization::{TextSource, ParserCore, Parser}, alt, provided::common::{many_until, Never}};

use super::{white_space, Char};

#[derive(Debug, Clone)]
pub enum Token<Span> {
    Group(Group<Span>),
    Punctuation(Char<Span>),
    Literal(Literal<Span>),
    Identifier(Identifier<Span>),
    Invalid(Span),
}

pub fn token<S: TextSource>() -> impl Parser<S, Token = Token<S::Span>> {
    alt! {
        group().map(Token::Group),
        punctuation().map(Token::Punctuation),
        literal().map(Token::Literal),
        identifier2().map(Token::Identifier),
        Char::any().map(|c| Token::Invalid(c.span)),
    }
}

pub fn tokens_until<S: TextSource, U: Parser<S>>(until: U) -> impl Parser<S, Token = (Vec<Token<S::Span>>, Option<U::Token>)> {
    many_until(
        (
            white_space(0..),
            token(),
            white_space(0..)
        ).map(|(_, token, _)| token),
        until,
        0..
    )
}

pub fn tokens<S: TextSource>() -> impl Parser<S, Token = Vec<Token<S::Span>>> {
    tokens_until(Never).map(|(tokens, _)| tokens)
}


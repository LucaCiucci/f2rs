use riddle::{prelude::*, provided::text::rusty::numeric_literal};

pub fn integer_literal<S: TextSource>() -> impl Parser<S, Token = i128> {
    numeric_literal().map(|lit| lit.value.parse::<i128>().unwrap())
}
use f2rs_parser_combinator::{prelude::*, provided::text::rusty::numeric_literal};

pub fn integer_literal<S: TextSource>() -> impl Parser<S, Token = i128> {
    // TODO handle negative numbers
    numeric_literal().map_if(|lit| {
        //println!("lit: {:?}", lit.value);
        lit.value.parse::<i128>().ok()
    })
}

// TODO tests

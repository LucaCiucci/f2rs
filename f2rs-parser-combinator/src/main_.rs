
use nom::{IResult, bytes::complete::tag};
use f2rs_parser_combinator::{provided::{text::{ExactMatch, TextSrc, Char}, common::map}, tokenization::{TokenParser, PResult}};


const SRC: &str = "srcsrc ciao";

macro_rules! aaa {
    (
        $(($($args:tt)*) => { $($body:tt)* })+
    ) => {
        ()
    };
}

macro_rules! match_piece {
    // keyword
    ($id:ident) => {
        nom::bytes::complete::tag(stringify!($id))
    };
    (<$id:ident) => {
        nom::bytes::complete::tag(stringify!($id))
    }
}

fn parse_input(input: &str) -> IResult<&str, &str> {
    match_piece!(src)(input)
}

fn main() {
    println!("Hello, world!");

    let r = parse_input(SRC);
    println!("r: {:?}", r);

    //let p = nom::multi::
    let a = aaa! {
        (
            if <cond:expr> {
                <then_block:stmt>
            }
        ) => {
        }

        (
            if [cond:expr] {
                <then_block:stmt>
            } else {
                <else_block:stmt>
            }
        ) => {
        }
    };

    let text = TextSrc::new(SRC);
    let text = text.text();

    let e = f2rs_parser_combinator::provided::common::many(map(ExactMatch::parser("src"), |_| 42), 0);
    let PResult(m, s) = e.parse(text);
    println!("m: {:?}, s: {:?}", m, s);

    //let b = match_piece!(src);
    //b(SRC);
}
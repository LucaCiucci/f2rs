
use std::io::Write;

use f2rs_parser_combinator::{provided::{text::{TextSrc, rusty::{tokens, Token}}, common::many}, tokenization::ParserCore, match_variant};
use f2rs_parser_combinator::prelude::*;


fn main() {
    println!("Hello, world!");

    //bench();

    let src = "foo bar baz 13 13.75 { 42_2_foo }";
    //let a = 42_2.0;

    let r = tokens().parse(src);
    println!("{r:?}");
    let r = format!("{:#?}", r);

    let tokens = tokens().parse(src).unwrap().0;
    let r2 = many(match_variant!(Token::Identifier), 0..).parse(tokens.as_slice()).unwrap().0;
    println!("{r2:?}");

    // write in a.rs
    let file = std::fs::File::create("a.rs").unwrap();
    let mut writer = std::io::BufWriter::new(file);
    writer.write_all(r.as_bytes()).unwrap();

    // write in b.rs
    let file = std::fs::File::create("b.rs").unwrap();
    let mut writer = std::io::BufWriter::new(file);
    write!(writer, "{:#?}", r2).unwrap();
}

pub fn bench() {
    let n = 10000000;
    let s = "foo ".repeat(n);
    let start = std::time::Instant::now();
    let _r = tokens().parse(s.as_str());
    let end = std::time::Instant::now();
    let dur = end - start;
    //println!("{r:?}");
    println!("{}ns/token, {:?} total, {} bytes", dur.as_nanos() / n as u128, dur, s.len());

    let start = std::time::Instant::now();
    let _r = tokens().parse(s.as_str());
    let end = std::time::Instant::now();
    let dur = end - start;
    //println!("{r:?}");
    println!("{}ns/token, {:?} total, {} bytes", dur.as_nanos() / n as u128, dur, s.len());

    let s = s.as_str();
    let text = TextSrc::new(s);
    let s = text.text();

    let start = std::time::Instant::now();
    let _r = tokens().parse(s);
    let end = std::time::Instant::now();
    let dur = end - start;
    //println!("{r:?}");
    println!("{}ns/token, {:?} total", dur.as_nanos() / n as u128, dur);

    //let b = match_piece!(src);
    //b(SRC);
}
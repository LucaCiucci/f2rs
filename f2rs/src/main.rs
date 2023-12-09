use std::{io::Write, path::Path};

use riddle::prelude::*;

use crate::code_gen::file_2_rs;

mod code_gen;
mod parse;

use clap::Parser;

#[derive(Debug, Clone, Parser)]
struct Args {
    file: String,
}

fn main() {
    let args = Args::parse();

    let src = load_file(&args.file);

    //println!("{}", src.lines().next().unwrap());

    let start_time = std::time::Instant::now();
    let file = parse::items().parse(&src[..]);
    println!("time: {:?}", start_time.elapsed());

    //println!("{:#?}", file);

    //println!("{:#?}", '_'.is_alphanumeric());

    // write to file
    let out = std::fs::File::create("parsed.rs").unwrap();
    let mut out = std::io::BufWriter::new(out);
    let file = file.0.unwrap();
    write!(out, "{file:#?}").unwrap();

    let start_time = std::time::Instant::now();
    let file = file_2_rs(&file);
    println!("time: {:?}", start_time.elapsed());
    let out = std::fs::File::create("out.rs").unwrap();
    let mut out = std::io::BufWriter::new(out);
    write!(out, "{}", file).unwrap();
}

fn load_file(path: impl AsRef<Path>) -> String {
    let path = path.as_ref();

    let src = std::fs::read_to_string(path).unwrap().replace("\r", "");
    //.replace("\\", "\\\\")
    //.replace("//", "\\//")
    //.replace("!", "//")
    //.replace("\\//", "!")
    //.replace("\\\\", "\\");

    src
}

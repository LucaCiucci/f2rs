use std::{path::Path, io::Write};

use riddle::prelude::*;

mod parse;

fn main() {
    let src = load_file("in.f90");

    //println!("{}", src.lines().next().unwrap());

    let start_time = std::time::Instant::now();
    let file = parse::items().parse(&src[..]);
    println!("time: {:?}", start_time.elapsed());

    //println!("{:#?}", file);

    //println!("{:#?}", '_'.is_alphanumeric());

    // write to file
    let out = std::fs::File::create("out.rs").unwrap();
    let mut out = std::io::BufWriter::new(out);
    let file = file.0.unwrap();
    write!(out, "{file:#?}").unwrap();
}

fn load_file(
    path: impl AsRef<Path>,
) -> String {
    let path = path.as_ref();

    let src = std::fs::read_to_string(path)
        .unwrap()
        .replace("\r", "");
        //.replace("\\", "\\\\")
        //.replace("//", "\\//")
        //.replace("!", "//")
        //.replace("\\//", "!")
        //.replace("\\\\", "\\");

    src
}
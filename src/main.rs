use std::str::FromStr;

use proc_macro2::{TokenStream, TokenTree};
use syn::{parse_macro_input, parse2};

use crate::program::Program;

mod program;
//mod _indent;
mod statement;

fn main() {
    println!("Hello, world!");

    ciao();

    let src = std::fs::read_to_string("in.f90")
        .unwrap()
        .replace("\r", "")
        .replace("\\", "\\\\")
        .replace("//", "\\//")
        .replace("!", "//")
        .replace("\\//", "!")
        .replace("\\\\", "\\");

    std::fs::write("in.txt", &src).unwrap();

    let lines = src
        .lines()
        //.map(|l| l.trim())
        .collect::<Vec<_>>();

    let mut composed_lines = vec![];
    let mut current_line = String::new();
    for i in 0..lines.len() {
        let line = lines[i];
        let line = if line.trim_start().starts_with("&") {
            &line.trim_start()[1..]
        } else {
            line
        };
        if line.trim_end().ends_with("&") {
            current_line += &line.trim_end()[..line.len() - 1];
        } else {
            current_line += line;
            composed_lines.push(current_line);
            current_line = String::new();
        }
    }
    if current_line.len() > 0 {
        composed_lines.push(current_line);
    }

    let lines = composed_lines.iter().map(|l| l.as_str()).collect::<Vec<_>>();

    let out_file = std::fs::File::create("out.rs").unwrap();
    let out_file: &mut dyn std::io::Write = &mut std::io::BufWriter::new(out_file);

    let mut line_index = 0;
    loop {
        if line_index >= lines.len() {
            break;
        }
        let line = lines[line_index];

        if let Some(comment) = parse_comment(line) {
            writeln!(out_file, "// {}", comment).unwrap();
            line_index += 1;
        } else if let Some(parsed) = Program::parse(&lines, line_index) {
            let program = &parsed.parsed;
            program.write(out_file).unwrap();
            line_index = parsed.next;
        } else {
            writeln!(out_file, "// UNPARSED: {}", line).unwrap();
            line_index += 1;
        }
    }

    println!("Done!");
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Parsed<T> {
    pub parsed: T,
    pub next: usize,
}

impl<T> Parsed<T> {
    pub fn new(parsed: T, next_line: usize) -> Self {
        Self {
            parsed,
            next: next_line,
        }
    }
}


pub fn parse_implicit_none(line: &str) -> bool {
    let tokens = match parse_token_vec(line) {
        Some(tokens) => tokens,
        None => return false,
    };

    if tokens.len() != 2 {
        return false;
    }

    if let TokenTree::Ident(ident) = &tokens[0] {
        if ident.to_string() != "implicit" {
            return false;
        }
    } else {
        return false;
    }

    if let TokenTree::Ident(ident) = &tokens[1] {
        if ident.to_string() != "none" {
            return false;
        }
    } else {
        return false;
    }

    true
}

pub fn parse_token_vec(src: &str) -> Option<Vec<TokenTree>> {
    Some(
        TokenStream::from_str(src)
            .ok()?
            .into_iter()
            .collect::<Vec<_>>()
    )
}

fn parse_comment(line: &str) -> Option<String> {
    let line = line.trim();
    
    if line.starts_with("//") {
        return Some(line[2..].trim().to_string());
    }

    None
}

fn ciao() {
    println!("Ciao!");

    let mut a: ndarray::Array<i32, ndarray::Dim<[usize; 2]>> = ndarray::array![[1, 2]];
    let b = a.clone();
    let b = b.column(0);
    let d = a.dim();
    a.push_column(b).unwrap();
    ndarray::Array::<i32, ndarray::Dim<[usize; 2]>>::from((2, 3));
    println!("a = {:?}", a);
    println!("a = {:?}", a.column(1).dim());

    //a.fil + 3;
    println!("a = {:?}", a);
    
    let a = "42".parse::<TokenStream>().unwrap();
    let i = syn::parse2::<syn::Expr>(a).unwrap();
    //println!("i = {:?}", i.);
}

macro_rules! aa {
    (aa) => {
        3.0
    };
    ($a:ident) => {
        1
    };
}

fn ciao2() {
    let a = aa!(a);
}
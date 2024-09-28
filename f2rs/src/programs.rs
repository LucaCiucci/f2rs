
use std::io::Write;

use clap::Parser;

use colored::Colorize;
use console::{Key, Term};
use f2rs_parse2::{statement::{classify_stmt, rules::implicit_part_stmt_2, MultilineSpan}, tokens::TokenizedFreeLine};
use f2rs_parser_combinator::tokenization::MapSpan;

use crate::ColorArg;

#[derive(Debug, Parser)]
pub struct InteractiveTokenization {
    #[clap(long, default_value = "magenta")]
    continuation_color: ColorArg,

    #[clap(long, default_value = "green")]
    comment_color: ColorArg,
    
    #[clap(long, default_value = "bright-yellow")]
    name_color: ColorArg,

    #[clap(long, default_value = "bright-blue")]
    op_color: ColorArg,

    #[clap(long, default_value = "bright-black")]
    delimiters_and_other_symbols: ColorArg,

    #[clap(long, default_value = "bright-cyan")]
    literal_color: ColorArg,

    #[clap(long, default_value = "red")]
    error_color: ColorArg,
}

pub fn interactive_tokenization(args: InteractiveTokenization) {
    println!("{}", "Interactive Tokenization".green());
    println!("Press type using the keyboard, press {} to submit, {} to exit, have fun!\n", "<ENTER>".blue(), "<ESC>".blue());

    let mut term = Term::stdout();

    //std::thread::sleep(Duration::from_millis(1000));
    //term.write_all("Hello World!".as_bytes()).unwrap();
    //term.flush().unwrap();
    //std::thread::sleep(Duration::from_millis(1000));
    //term.clear_line().unwrap();

    loop {
        let mut line = Vec::<char>::new();

        loop {
            let Ok(k) = term.read_key() else {
                continue;
            };

            match k {
                Key::Char(c) => line.push(c),
                Key::Backspace => {
                    line.pop();
                },
                Key::Escape => return,
                Key::Enter => break,
                _ => {}
            };

            let r = if let Some(parsed) = TokenizedFreeLine::parse_chars(&line) {
                parsed.dump(
                    &line,
                    Some(args.continuation_color.to_colored()),
                    Some(args.comment_color.to_colored()),
                    Some(args.name_color.to_colored()),
                    Some(args.op_color.to_colored()),
                    Some(args.delimiters_and_other_symbols.to_colored()),
                    Some(args.literal_color.to_colored()),
                    Some(args.error_color.to_colored()),
                )
            } else {
                line.iter().collect::<String>()
            };
    
            term.clear_line().unwrap();
            term.write_all(r.as_bytes()).unwrap();
        }

        let tokens = TokenizedFreeLine::parse_chars(&line).unwrap().tokens.map_span(&|span| MultilineSpan::from_line_span(0, span));

        //println!("CLASSIFYING");
        let classified = classify_stmt(&tokens[..]);

        term.write_line("").unwrap();
        term.clear_line().unwrap();
        if let Some(stmt) = classified.stmt {
            println!("OK");
            term.write_line(format!("{}", stmt.statement_kind_name().green()).as_str()).unwrap();
        } else {
            println!("KO");
            term.write_line(&format!("{}", "UNCLASSIFIED STATEMENT".on_red())).unwrap()
        }
        term.write_line("---").unwrap();
    }
}

#[test]
fn afjnejfkenwjfnefwneifjwenfewinrigengrllllllllllllllllllllllllllll() {
    let tokens = TokenizedFreeLine::parse_chars(&"inquire()".chars().collect::<Vec<_>>()[..]).unwrap().tokens.map_span(&|span| MultilineSpan::from_line_span(0, span));

    let classified = classify_stmt(&tokens[..]);
}
use std::{io::Write, ops::Range, time::Duration};

use clap::Parser;
use colored::{Color, Colorize};
use console::{Key, Term};
use f2rs_parse2::{statement::{rules::implicit_part_stmt_2, MultilineSpan, StatementValue}, Cfg, TokenizedFreeLine};
use f2rs_parser_combinator::tokenization::{MapSpan, ParserCore, Spanned};
use log::LevelFilter;

#[derive(Debug, Parser)]
enum Command {
    InteractiveTokenization(InteractiveTokenization),
    InspectFreeSource,
}

fn main() {
    env_logger::builder()
        //.filter_level(LevelFilter::Trace)
        .init();

    let command = Command::parse();

    match command {
        Command::InteractiveTokenization(args) => interactive_tokenization(args),
        //Command::InspectFreeSource => inspect_free_source(),
        _ => unimplemented!(),
    }
}

#[derive(Debug, Parser)]
pub struct InteractiveTokenization {

}

fn interactive_tokenization(args: InteractiveTokenization) {
    let mut term = Term::stdout();

    //std::thread::sleep(Duration::from_millis(1000));
    //term.write_all("Hello World!".as_bytes()).unwrap();
    //term.flush().unwrap();
    //std::thread::sleep(Duration::from_millis(1000));
    //term.clear_line().unwrap();

    let mut line = Vec::<char>::new();

    while let Ok(k) = term.read_key() {
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
                Some(Color::Magenta),
                Some(Color::Green),
                Some(Color::BrightYellow),
                Some(Color::BrightBlue),
                Some(Color::BrightBlack),
                Some(Color::BrightCyan),
                Some(Color::Red),
            )
        } else {
            line.iter().collect::<String>()
        };

        term.clear_line().unwrap();
        term.write_all(r.as_bytes()).unwrap();
    }

    let tokens = TokenizedFreeLine::parse_chars(&line).unwrap().tokens.map_span(&|span| MultilineSpan::from_line_span(0, span));

    let cfg = Cfg::f2018();

    let options = implicit_part_stmt_2(&cfg, &tokens[..])
        .into_iter()
        .map(|s| s.0.statement_name())
        .collect::<Vec<_>>();

    term.clear_line().unwrap();
    term.write_line("Options:").unwrap();
    for (i, option) in options.iter().enumerate() {
        term.write_line(format!("{i}. {option}").as_str()).unwrap();
    }
}
/*
fn inspect_free_source() {
    let raw_source = include_str!("congruent.f90").repeat(1);

    let start_time = std::time::Instant::now();
    let lines = raw_source
        .lines()
        .map(|line| line.chars().collect::<Vec<_>>())
        .map(|chars| (TokenizedFreeLine::parse_chars(&chars).unwrap(), chars))
        .enumerate()
        .map(|(number, (content, chars))| Line { number, chars, content })
        .collect::<Vec<_>>();
    let elapsed = start_time.elapsed();
    println!("Number of lines: {} tokenized in {elapsed:?}", lines.len());

    let groups = {
        let mut groups = Vec::new();
        let mut index = 0;

        while index < lines.len() {
            let lines = &lines[index..];
            let count = TokenizedFreeLine::group(lines.iter().map(|line| &line.content));
            assert!(count > 0);
            groups.push(index..index + count);
            index += count;
        }

        groups
    };

    let number_length = lines.len().to_string().len();

    for group in groups {
        let lines = &lines[group];
        for (i, line) in lines.iter().enumerate() {
            let number = line.number.to_string();
            let number = " ".repeat(number_length - number.len()) + &number;

            let sign = if lines.len() > 1 {
                if i == 0 {
                    "┌".bright_black()
                } else if i == lines.len() - 1 {
                    "└".bright_black()
                } else {
                    "│".bright_black()
                }
            } else {
                " ".normal()
            };

            let number = format!("{number}{sign}| ",);
            let content = line.content.dump(
                &line.chars,
                Some(Color::Magenta),
                Some(Color::Green),
                Some(Color::BrightYellow),
                Some(Color::BrightBlue),
                Some(Color::BrightBlack),
                Some(Color::BrightCyan),
                Some(Color::Red),
            );
            println!("{}{}", number, content);
        }
    }

    println!("Number of lines: {} tokenized in {elapsed:?}", lines.len());

    #[derive(Debug, Clone)]
    struct Group {
        lines: Vec<Line>,
    }

    let groups = {
        let mut groups = Vec::new();
        let mut index = 0;

        while index < lines.len() {
            let lines = &lines[index..];
            let count = TokenizedFreeLine::group(lines.iter().map(|line| &line.content));
            assert!(count > 0);
            groups.push(Group {
                lines: lines[..count].to_vec(),
            });
            index += count;
        }

        groups
    };

    println!("Number of groups: {}", groups.len());

    let start_time = std::time::Instant::now();
    for g in groups {
        let line_number = g.lines[0].number;

        let tokens = g.lines
            .into_iter()
            .map(|line| line.content.tokens.map_span(&|span| MultilineSpan::from_line_span(line.number, span)))
            .flatten()
            .collect::<Vec<_>>();

        if let Some(s) = StatementValue::parse(&tokens[..], &Cfg::f2018()) {
            //println!("parsed! line {line_number}");
            assert!(s.as_import().unwrap().data.is_none());
            let s = s.as_import().unwrap().kw.span();
            //println!("module name: {s:?}");
        }
    }
    let elapsed = start_time.elapsed();
    println!("classified_statements in {elapsed:?}");
}

/*
Some(Color::Magenta),
Some(Color::Green),
Some(Color::BrightYellow),
Some(Color::BrightBlue),
Some(Color::BrightBlack),
Some(Color::BrightCyan),
Some(Color::Red),
*/
*/
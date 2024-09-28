/*!
Tokens and tokenization utilities.

# References
- [J3/18-007r1 §6 "Lexical tokens and source form"](https://j3-fortran.org/doc/year/18/18-007r1.pdf#chapter.399)
*/

use enum_as_inner::EnumAsInner;
use f2rs_parser_combinator::prelude::*;

pub mod rules;
pub mod line;

pub use line::*;

#[cfg(test)]
fn parse_all<'s, T>(source: &'s str, parser: impl Parser<&'s str, Token = T>) -> T {
    let (token, tail) = parser.parse(source).expect("Does not parse");
    assert!(tail.is_empty());
    token
}

#[cfg(test)]
fn examples<const N: usize>(
    rule: impl Parser<&'static str>,
    examples: [&'static str; N],
) {
    use f2rs_parser_combinator::prelude::Parser;

    for example in examples {
        eprintln!("Testing match with: \"{example}\"");
        let (_, tail) = rule.parse(example).expect("Rule did not parse");
        let parsed = &example[0..example.len() - tail.len()];
        assert!(tail.is_empty(), "Rule did not parse all the source: \"{parsed}\" --- \"{tail}\"");
    }
}
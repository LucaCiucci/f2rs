#[cfg(test)]
use f2rs_parser_combinator::prelude::*;

pub mod rules;
pub mod line;

#[macro_export] // TODO in f2rs-parser-common
macro_rules! rule_test {
    ($name: ident ($(
        $rule_name:ident $rule_number:literal
    ),*) { $($code:tt)* }) => {
        paste::paste! {
            #[test]
            #[allow(non_snake_case)]
            fn [<test_ $name $(_ $rule_name _ $rule_number)*>]() {
                $($code)*
            }
        }
    };
}

#[cfg(test)]
fn parse_all<T>(rule: &impl Parser<&'static str, Token = T>, example: &'static str) -> T {
    eprintln!("Testing match with: \"{example}\"");
    let (token, tail) = rule.parse(example).expect("Rule did not parse");
    let parsed = &example[0..example.len() - tail.len()];
    assert!(tail.empty(), "Rule did not parse all the source: \"{parsed}\" --- \"{tail}\"");
    token
}

#[cfg(test)]
fn examples<const N: usize>(
    rule: impl Parser<&'static str>,
    examples: [&'static str; N],
) {
    for example in examples {
        parse_all(&rule, example);
    }
}
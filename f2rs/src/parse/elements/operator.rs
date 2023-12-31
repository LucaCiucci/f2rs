use f2rs_parser_combinator::{
    provided::text::StringMatch,
    tokenization::{Parser, TextSource},
};

pub const OPERATORS: [(&str, usize); 25] = [
    // TODO check_priorities
    ("%", 1000), // TODO priority???? textual equivalent?? maybe .mod.?
    ("**", 60),
    ("*", 50),
    ("/", 50),
    ("+", 40),
    ("-", 40),
    ("//", 30),
    (".eq.", 20),
    ("==", 20),
    (".ne.", 20),
    ("!=", 20),
    (".lt.", 20),
    ("<", 20),
    (".le.", 20),
    ("<=", 20),
    (".gt.", 20),
    (">", 20),
    (".ge.", 20),
    (">=", 20),
    (".and.", 10),
    (".or.", 10),
    (".not.", 10),
    (".eqv.", 10),
    (".neqv.", 10),
    ("=", 0),
];

pub fn operator<S: TextSource>() -> impl Parser<S, Token = StringMatch<S::Span>> {
    //pub fn op<S: TextSource>(op: &'static str) -> impl Parser<S, Token = ExactMatch<S::Span>> {
    //    ExactMatch::exact(op, true)
    //}

    f2rs_parser_combinator::provided::common::alt(|| {
        let mut operators = OPERATORS.clone().map(|(op, _)| op);
        // longest first
        operators.sort_by(|a, b| b.len().cmp(&a.len()));
        operators.into_iter().map(|op| StringMatch::exact(op, false))
    })
}

// TODO tests

use riddle::{
    provided::text::ExactMatch,
    tokenization::{Parser, TextSource},
};

pub const OPERATORS: [(&str, usize); 24] = [
    // TODO check_priorities
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

pub fn operator<S: TextSource>() -> impl Parser<S, Token = ExactMatch<S::Span>> {
    //pub fn op<S: TextSource>(op: &'static str) -> impl Parser<S, Token = ExactMatch<S::Span>> {
    //    ExactMatch::exact(op, true)
    //}

    riddle::provided::common::alt(|| {
        let mut operators = OPERATORS.clone().map(|(op, _)| op);
        // longest first
        operators.sort_by(|a, b| b.len().cmp(&a.len()));
        operators
    })
}

// TODO tests

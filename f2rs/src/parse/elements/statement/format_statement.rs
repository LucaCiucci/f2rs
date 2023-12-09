use riddle::tokenization::{TextSource, Parser};

use crate::parse::elements::integer_literal;

use super::{SpecialFunction, special_function};

#[derive(Debug, Clone)]
pub struct FormatStatement<Span> {
    pub number: i128,
    pub special_function_statement: SpecialFunction<Span>
}

pub fn format_statement<S: TextSource>() -> impl Parser<S, Token = FormatStatement<S::Span>> {
    (
        integer_literal(),
        special_function().condition(|s, _| s.name.value == "format"),
    ).map(|(number, special_function_statement)| FormatStatement { number, special_function_statement })
}
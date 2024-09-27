use f2rs_parser_combinator::tokenization::{Parser, TextSource};

use crate::parse::elements::integer_literal;

use super::{special_statement_function, SpecialStatementFunction};

#[derive(Debug, Clone)]
pub struct FormatStatement<Span> {
    pub number: i128,
    pub special_function_statement: SpecialStatementFunction<Span>,
}

pub fn format_statement<S: TextSource>() -> impl Parser<S, Token = FormatStatement<S::Span>> {
    (
        integer_literal(),
        special_statement_function().condition(|s, _| s.name.value == "format"),
    )
        .map(|(number, special_function_statement)| FormatStatement {
            number,
            special_function_statement,
        })
}

use super::*;

pub fn call_statement<S: TextSource>() -> impl Parser<S, Token = Statement<S::Span>> {
    (
        spaced(keyword("call")),
        spaced(expression()),
        eol_or_comment(),
    ).map(|(_, expr, _)| Statement::CallStatement(expr))
}
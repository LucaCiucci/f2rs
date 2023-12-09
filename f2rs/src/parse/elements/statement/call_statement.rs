use super::*;

pub fn call_statement<S: TextSource>() -> impl Parser<S, Token = Statement<S::Span>> {
    (
        spaced(keyword("call")),
        spaced(expression()),
        eol_or_comment(),
    ).map(|(_, expr, _)| Statement::CallStatement(expr))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_call_statement() {
        let src = "call foo(bar)";
        let r = call_statement().parse(src).0.unwrap();

        assert_eq!(
            r
                .as_call_statement().unwrap()
                .as_call_or_indexing().unwrap()
                .function.as_identifier().unwrap().value,
            "foo"
        );
        assert_eq!(r.as_call_statement().unwrap().as_call_or_indexing().unwrap().arguments.len(), 1);
        assert_eq!(
            r.as_call_statement().unwrap().as_call_or_indexing().unwrap().arguments[0].as_identifier().unwrap().value,
            "bar"
        );
    }
}
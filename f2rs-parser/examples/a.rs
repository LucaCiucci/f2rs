use f2rs_lexer::tokenizer::TokenizedFreeLine;


fn main() {
    let src = include_str!("../../example-project/fortran/a.f90");
    let lines = src.lines()
        .map(|line| line.chars().collect::<Vec<_>>())
        .map(|chars| TokenizedFreeLine::parse_chars(&chars))
        .enumerate()
        .map(|(i, line)| line.expect(&format!("Failed to parse line: {i}")))
        .collect::<Vec<_>>();

        
    let stmts = TokenizedFreeLine::group_stmt_lines(&lines);
    println!("Parsed {} lines", stmts);
}
# `f2rs-parser`
A _not so good_ FORTRAN parser written in Rust.

Structure:
1. *tokenizer*: Tokenizes the input FORTRAN code (e.g. `foo(bar + 2.0d1)` into `foo`, `(`, `bar`, `+`, `2.0d1`, `)`)
1. *lexer*: converts token to `TokenTree`s (e.g. matches groups with delimiters)
1. *statement* parser: parses single statements
1. *parser*: parses statements into an Abstract Syntax Tree (AST)
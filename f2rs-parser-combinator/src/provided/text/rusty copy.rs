

mod identifier; use std::{fmt::Display, io::Write};

pub use identifier::*;
mod literal; pub use literal::*;
mod punctuation; pub use punctuation::*;
mod group; pub use group::*;

use crate::{tokenization::{TextSource, TokenParser, Source}, alt, provided::common::{many_until, Never}, match_variant};

use super::{space, Char};

#[derive(Debug, Clone)]
pub enum Token<Span> {
    Group(Group<Span>),
    Punctuation(Char<Span>),
    Literal(Literal<Span>),
    Identifier(Identifier<Span>),
    Invalid(Span),
}

pub fn token<S: TextSource>() -> impl TokenParser<S, Token = Token<S::Span>> {
    alt! {
        group().map(Token::Group),
        punctuation().map(Token::Punctuation),
        literal().map(Token::Literal),
        identifier2().map(Token::Identifier),
        Char::any().map(|c| Token::Invalid(c.span)),
    }
}

pub fn tokens_until<S: TextSource, U: TokenParser<S>>(until: U) -> impl TokenParser<S, Token = (Vec<Token<S::Span>>, Option<U::Token>)> {
    many_until(
        (
            space(0..),
            token(),
            space(0..)
        ).map(|(_, token, _)| token),
        until,
        0..
    )
}

pub fn tokens<S: TextSource>() -> impl TokenParser<S, Token = Vec<Token<S::Span>>> {
    tokens_until(Never).map(|(tokens, _)| tokens)
}


fn f_keyword<TS, S: Source<Element = Token<TS>>>(k: &'static str) -> impl TokenParser<S, Token = ()> {
    move |source: S| {
        let t = source.get_at(&source.start());
        let t = match t {
            Some(Token::Identifier(id)) => if id.value == k { () } else { return source.unparsed_result(); },
            _ => return source.unparsed_result(),
        };
        let next = source.next(source.start(), 1);
        PResult::parsed(t, source.tail(next))
    }
}

fn f_group<TS, S: Source<Element = Token<TS>>>(delimiter: Delimiter) -> impl TokenParser<S, Token = Group<TS>> {
    move |source: S| {
        let t = source.get_at(&source.start());
        let g = match t {
            Some(Token::Group(g)) => if g.delimiter == delimiter { g } else { return source.unparsed_result(); },
            _ => return source.unparsed_result(),
        };
        let next = source.next(source.start(), 1);
        PResult::parsed(g, source.tail(next))
    }
}

macro_rules! build_parser_for_piece {
    ($keyword:ident) => {
        |tuple| f_keyword(stringify!($keyword))
            //.map(move |k| t.clone().append(k))
            .map(move |_k| tuple.clone())
    };
    (#) => {
        |tuple| match_variant!(Token::Punctuation).condition(|p, _| p.element.value == '#').map(move |_p| tuple.clone())
    };
    (#($f:expr)) => {
        |tuple| $f().map(move |p| tuple.clone().append(p))
    };
    (,) => {
        |tuple| match_variant!(Token::Punctuation).condition(|p, _| p.element.value == ',').map(move |_p| tuple.clone())
    };
    (<) => {
        |tuple| match_variant!(Token::Punctuation).condition(|p, _| p.element.value == '<').map(move |_p| tuple.clone())
    };
    (>) => {
        |tuple| match_variant!(Token::Punctuation).condition(|p, _| p.element.value == '>').map(move |_p| tuple.clone())
    };
    (($($t:tt)*)) => {
        |tuple| {
            f_group(Delimiter::Parenthesis)
            .map_if(move |g| {
                let tokens = &g.inner[..];

                let parser = parser_chain!(
                    { ().map(|_| tuple.clone()) }
                    {$($t)*}
                );

                // TODO use new_source
                if let PResult(Some(parsed), _new_source) = parser.parse(tokens) {
                    Some(parsed)
                } else {
                    None
                }
            })
        }
    };
    ({$($t:tt)*}) => {
        |tuple| {
            f_group(Delimiter::Brace)
            .map_if(move |g| {
                let tokens = &g.inner[..];

                let parser = parser_chain!(
                    { ().map(|_| tuple.clone()) }
                    {$($t)*}
                );

                // TODO use new_source
                if let PResult(Some(parsed), _new_source) = parser.parse(tokens) {
                    Some(parsed)
                } else {
                    None
                }
            })
        }
    };
    ([$($t:tt)*]) => {
        |tuple| {
            f_group(Delimiter::Bracket)
            .map_if(move |g| {
                let tokens = &g.inner[..];

                let parser = parser_chain!(
                    { ().map(|_| tuple.clone()) }
                    {$($t)*}
                );

                // TODO use new_source
                if let PResult(Some(parsed), _new_source) = parser.parse(tokens) {
                    Some(parsed)
                } else {
                    None
                }
            })
        }
    };
}

// use $crate::tokenization::AppendType;

macro_rules! parser_chain {
    ({} {}) => {
        ()
    };
    //({} {$to_process:tt $($tail:tt)*}) => {
    //    parser_chain!(
    //        { parser_chain!({} {}).then(build_parser_for_piece!($to_process)) }
    //        {$($tail)*}
    //    )
    //};
    ({$($processed:tt)*} {## $($tail:tt)*}) => {
        parser_chain!(
            { parser_chain!({$($processed)*} {}).then(build_parser_for_piece!(#)) }
            {$($tail)*}
        )
    };
    ({$($processed:tt)*} {#($to_process:expr) $($tail:tt)*}) => {
        parser_chain!(
            { parser_chain!({$($processed)*} {}).then(build_parser_for_piece!(#($to_process))) }
            {$($tail)*}
        )
    };
    ({$($processed:tt)*} {$to_process:tt $($tail:tt)*}) => {
        parser_chain!(
            { parser_chain!({$($processed)*} {}).then(build_parser_for_piece!($to_process)) }
            {$($tail)*}
        )
    };
    ({$($processed:tt)*} {}) => {
        $($processed)*
    };
}

macro_rules! parser_for_syntax {
    ($($tokens:tt)*) => {
        {
            use $crate::tokenization::AppendType;
            parser_chain!({} {$($tokens)*})
        }
    };
    //($a:expr : ) => {}
}

use crate::tokenization::PResult;

pub fn foo() {

    fn f_expression<TS, S: Source<Element = Token<TS>>>() -> impl TokenParser<S, Token = i32> {
        |source: S| {
            let t = source.get_at(&source.start());
            let t = match t {
                Some(Token::Identifier(_)) => 42,
                _ => return source.unparsed_result(),
            };
            let next = source.next(source.start(), 1);
            PResult::parsed(t, source.tail(next))
        }
    }

    let tokens = tokens().parse("if (ciao()[i]) as bool { b } else { b }").0.unwrap();
    let tokens = &tokens[..];

    //use f_expression as expression;
    use f_expression as statements;
    use rustic::*;

    let p = parser_for_syntax!(
        if #(expr) {
            #(statements)
        } else {
            #(statements)
        }
    )
    //.map(|data| {
    //})
    .parse(tokens);

    println!("RRRR {:?}", p);
    println!("RRRR {}", "a".red());
    let f = "a".red();
    let f = format!("{f}");
    println!("{f}");

    let e = p.0.unwrap().0;

    use colored::Colorize;

    fn show_type(t: &Type<()>) -> String {
        match t {
            Type::Identifier(id) => id.value.to_string().cyan().to_string(),
            Type::Embraced(t) => {
                let t = show_type(&t);
                format!("<{t}>")
            },
        }
    }

    fn show_expression(e: &Expression<()>) -> String {
        match e {
            Expression::Call { function, arguments } => {
                let f = show_expression(&function);
                let args = arguments.iter().map(|a| show_expression(a)).collect::<Vec<_>>().join(",");
                format!("{f}({args})")
            },
            Expression::Identifier(id) => id.value.bright_blue().to_string(),
            Expression::Cast { castee, to } => {
                let castee = show_expression(&castee);
                let to = show_type(&to);
                format!("{castee} {} {to}", "as".red().to_string())
            },
            Expression::Indexing { indexee, indexes } => {
                let indexee = show_expression(&indexee);
                let indexes = indexes.iter().map(|i| show_expression(i)).collect::<Vec<_>>().join(",");
                format!("{indexee}[{indexes}]")
            },
            Expression::Parenthesis(e) => {
                let e = show_expression(&e);
                format!("({e})")
            },
        }
    }

    println!("RRRR {}", show_expression(&e));

    // write to file
    let file = std::fs::File::create("out.ans").unwrap();
    let mut file = std::io::BufWriter::new(file);
    write!(file, "{}", show_expression(&e));

    //aaa!(
    //    
    //);
}

mod rustic {
    use crate::provided::common::{many, separated};

    use super::*;

    pub fn identifier<TS: Clone, S: Source<Element = Token<TS>>>() -> impl TokenParser<S, Token = Identifier<TS>> {
        match_variant!(Token::Identifier)
            .map(|id| id.element)
    }

    #[derive(Debug, Clone)]
    pub enum Type<TS> {
        Identifier(Identifier<TS>),
        Embraced(Box<Type<TS>>),
        // TODO as
        // TODO template list
    }

    #[derive(Debug, Clone)]
    pub enum Expression<TS> {
        Identifier(Identifier<TS>),
        Parenthesis(Box<Expression<TS>>),
        Call {
            function: Box<Expression<TS>>,
            arguments: Vec<Expression<TS>>,
        },
        Indexing {
            indexee: Box<Expression<TS>>,
            indexes: Vec<Expression<TS>>,
        },
        Cast {
            castee: Box<Expression<TS>>,
            to: Type<TS>,
        }
    }

    enum CallOrIndexing<TS> {
        Call {
            arguments: Vec<Expression<TS>>,
        },
        Indexing {
            indexes: Vec<Expression<TS>>,
        },
    }

    pub fn ty<TS: Clone, S: Source<Element = Token<TS>>>() -> impl TokenParser<S, Token = Type<TS>> {
        alt!(
            identifier().map(|id| Type::Identifier(id)),
            parser_for_syntax!(< #(ty) >).map(|(ty,)| Type::Embraced(Box::new(ty))),
        )
    }

    fn call_or_indexing<TS: Clone, S: Source<Element = Token<TS>>>() -> impl TokenParser<S, Token = CallOrIndexing<TS>> {
        use CallOrIndexing::*;

        fn args<TS: Clone, S: Source<Element = Token<TS>>>() -> impl TokenParser<S, Token = Vec<Expression<TS>>> {
            separated(
                expr(),
                match_variant!(Token::Punctuation).condition(|p, _| p.element.value == ','),
                0..
            )
        }

        alt!(
            parser_for_syntax!(
                (#(args))
            ).map(|(args,)| Call {
                arguments: args,
            }),
            parser_for_syntax!(
                [#(args)]
            ).map(|(indexes,)| Indexing {
                indexes,
            }),
        )
    }

    pub fn expr_monome<TS: Clone, S: Source<Element = Token<TS>>>() -> impl TokenParser<S, Token = Expression<TS>> {
        use Expression::*;
        alt!(
            parser_for_syntax!(
                (#(expr))
            ).map(|(e,)| Parenthesis(Box::new(e))),
            parser_for_syntax!(
                #(identifier)
            ).map(|(id,)| Identifier(id)),
        )
            .then(|e| many(call_or_indexing(), 0..).map(move |ci| {
                let mut e = e.clone();
                for c in ci {
                    match c {
                        CallOrIndexing::Call { arguments } => {
                            e = Call {
                                function: Box::new(e),
                                arguments,
                            };
                        },
                        CallOrIndexing::Indexing { indexes } => {
                            e = Indexing {
                                indexee: Box::new(e),
                                indexes,
                            };
                        },
                    }
                }
                e
            }))
            .then(|e| many(parser_for_syntax!(as #(ty)), 0..).map(move |types| {
                let mut e = e.clone();
                for (ty,) in types {
                    e = Cast { castee: Box::new(e), to: ty }
                }
                e
            }))

    }

    pub fn expr<TS: Clone, S: Source<Element = Token<TS>>>() -> impl TokenParser<S, Token = Expression<TS>> {
        alt!(
            parser_for_syntax!(
                #(expr_monome)
            ).map(|(
                e,
            )| e),
        )
    }
}
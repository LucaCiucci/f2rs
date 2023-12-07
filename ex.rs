#![feature(prelude_import)]
#[prelude_import]
use std::prelude::rust_2021::*;
#[macro_use]
extern crate std;
use std::{path::Path, io::Write};
use riddle::tokenization::Parser;
mod parse {
    use riddle::{
        tokenization::{TextSource, Parser},
        provided::{
            common::{many, many_until},
            text::Char,
        },
    };
    use crate::parse::elements::*;
    use self::elements::{LineComment, empty_lines, EmptyLines, line_comment};
    use riddle::alt;
    pub mod elements {
        pub use riddle::provided::text::rusty::identifier;
        mod literals {
            use riddle::{
                prelude::*,
                provided::text::rusty::{
                    string_literal_impl, StringLiteral, NumericLiteral, numeric_literal,
                },
            };
            pub enum Literal<Span> {
                String(StringLiteral<Span>),
                Number(NumericLiteral<Span>),
                True,
                False,
            }
            #[automatically_derived]
            impl<Span: ::core::fmt::Debug> ::core::fmt::Debug for Literal<Span> {
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    match self {
                        Literal::String(__self_0) => {
                            ::core::fmt::Formatter::debug_tuple_field1_finish(
                                f,
                                "String",
                                &__self_0,
                            )
                        }
                        Literal::Number(__self_0) => {
                            ::core::fmt::Formatter::debug_tuple_field1_finish(
                                f,
                                "Number",
                                &__self_0,
                            )
                        }
                        Literal::True => ::core::fmt::Formatter::write_str(f, "True"),
                        Literal::False => ::core::fmt::Formatter::write_str(f, "False"),
                    }
                }
            }
            #[automatically_derived]
            impl<Span: ::core::clone::Clone> ::core::clone::Clone for Literal<Span> {
                #[inline]
                fn clone(&self) -> Literal<Span> {
                    match self {
                        Literal::String(__self_0) => {
                            Literal::String(::core::clone::Clone::clone(__self_0))
                        }
                        Literal::Number(__self_0) => {
                            Literal::Number(::core::clone::Clone::clone(__self_0))
                        }
                        Literal::True => Literal::True,
                        Literal::False => Literal::False,
                    }
                }
            }
            pub fn string_literal<S: TextSource>() -> impl Parser<
                S,
                Token = StringLiteral<S::Span>,
            > {
                string_literal_impl(|_| 0..)
            }
            pub fn fortran_literal<S: TextSource>() -> impl Parser<
                S,
                Token = Literal<S::Span>,
            > {
                move |source: S| {
                    if false {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = ExactMatch::exact(
                            ".true.",
                            false,
                        )
                        .map(|_| Literal::True)
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = ExactMatch::exact(
                            ".false.",
                            false,
                        )
                        .map(|_| Literal::False)
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = string_literal()
                        .map(Literal::String)
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = numeric_literal()
                        .map(Literal::Number)
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else {
                        ::riddle::tokenization::PResult::unparsed(source)
                    }
                }
            }
        }
        pub use literals::*;
        mod comment {
            use riddle::{
                tokenization::{TextSource, Parser},
                provided::{common::many_until, text::Char},
            };
            use super::*;
            pub struct LineComment<Span> {
                pub text: String,
                pub span: Span,
            }
            #[automatically_derived]
            impl<Span: ::core::fmt::Debug> ::core::fmt::Debug for LineComment<Span> {
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::debug_struct_field2_finish(
                        f,
                        "LineComment",
                        "text",
                        &self.text,
                        "span",
                        &&self.span,
                    )
                }
            }
            #[automatically_derived]
            impl<Span: ::core::clone::Clone> ::core::clone::Clone for LineComment<Span> {
                #[inline]
                fn clone(&self) -> LineComment<Span> {
                    LineComment {
                        text: ::core::clone::Clone::clone(&self.text),
                        span: ::core::clone::Clone::clone(&self.span),
                    }
                }
            }
            pub fn line_comment<S: TextSource>() -> impl Parser<
                S,
                Token = LineComment<S::Span>,
            > {
                spaced(
                    '!'
                        .then(|bang| {
                            many_until(Char::<S::Span>::any(), '\n', 0..)
                                .map(move |(chars, _newline)| {
                                    let bang = bang.clone();
                                    let span = if let Some(last) = chars.last() {
                                        S::joint_span(bang.span, last.span.clone())
                                    } else {
                                        bang.span
                                    };
                                    LineComment {
                                        text: chars.into_iter().map(|c| c.value).collect(),
                                        span,
                                    }
                                })
                        }),
                )
            }
        }
        pub use comment::*;
        mod space {
            use std::ops::RangeBounds;
            use riddle::{
                alt, tokenization::{TextSource, Parser},
                provided::{
                    common::{fold_many, many, separated},
                    text::Char,
                },
            };
            pub fn nl<S: TextSource>() -> impl Parser<S, Token = ()> {
                (move |source: S| {
                    if false {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = "\n\r"
                        .map(|_| ())
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = "\n"
                        .map(|_| ())
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = "\r"
                        .map(|_| ())
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else {
                        ::riddle::tokenization::PResult::unparsed(source)
                    }
                })
                    .map(|_| ())
            }
            pub fn sp<S: TextSource>(
                range: impl RangeBounds<usize> + Clone,
            ) -> impl Parser<S, Token = ()> {
                fold_many(
                    Char::any_of(" \t".chars()).map(|_| ()),
                    || (),
                    |_, _| ((), true),
                    range,
                )
            }
            pub fn continuation<S: TextSource>() -> impl Parser<S, Token = ()> {
                ('&', sp(0..), nl()).map(|_| ())
            }
            pub fn space<S: TextSource>() -> impl Parser<S, Token = ()> {
                separated(sp(0..), continuation(), 0..).map(|_| ())
            }
            pub fn spaced<S: TextSource, T: Parser<S>>(
                parser: T,
            ) -> impl Parser<S, Token = T::Token> {
                (space(), parser, space()).map(|(_, token, _)| token)
            }
            pub struct EmptyLines {
                pub count: usize,
            }
            #[automatically_derived]
            impl ::core::fmt::Debug for EmptyLines {
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::debug_struct_field1_finish(
                        f,
                        "EmptyLines",
                        "count",
                        &&self.count,
                    )
                }
            }
            #[automatically_derived]
            impl ::core::clone::Clone for EmptyLines {
                #[inline]
                fn clone(&self) -> EmptyLines {
                    EmptyLines {
                        count: ::core::clone::Clone::clone(&self.count),
                    }
                }
            }
            pub fn empty_lines<S: TextSource>() -> impl Parser<
                S,
                Token = EmptyLines,
            > {
                fold_many(
                        (many(' ', 0..), nl()),
                        || 0,
                        |count, _| (count + 1, true),
                        1..,
                    )
                    .map(|count| EmptyLines { count })
            }
        }
        pub use space::*;
        mod operator {
            use riddle::{
                tokenization::{TextSource, Parser},
                provided::text::ExactMatch,
            };
            pub const OPERATORS: [(&str, usize); 23] = [
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
            ];
            pub fn operator<S: TextSource>() -> impl Parser<
                S,
                Token = ExactMatch<S::Span>,
            > {
                riddle::provided::common::alt(|| {
                    let mut operators = OPERATORS.clone().map(|(op, _)| op);
                    operators.sort_by(|a, b| b.len().cmp(&a.len()));
                    operators
                })
            }
        }
        pub use operator::*;
        mod expression {
            use riddle::{prelude::*, provided::text::rusty::Identifier};
            use super::*;
            mod index_range {
                use super::*;
                pub struct IndexRange<Span> {
                    pub start: Option<Expression<Span>>,
                    pub end: Option<Expression<Span>>,
                }
                #[automatically_derived]
                impl<Span: ::core::fmt::Debug> ::core::fmt::Debug for IndexRange<Span> {
                    fn fmt(
                        &self,
                        f: &mut ::core::fmt::Formatter,
                    ) -> ::core::fmt::Result {
                        ::core::fmt::Formatter::debug_struct_field2_finish(
                            f,
                            "IndexRange",
                            "start",
                            &self.start,
                            "end",
                            &&self.end,
                        )
                    }
                }
                #[automatically_derived]
                impl<Span: ::core::clone::Clone> ::core::clone::Clone
                for IndexRange<Span> {
                    #[inline]
                    fn clone(&self) -> IndexRange<Span> {
                        IndexRange {
                            start: ::core::clone::Clone::clone(&self.start),
                            end: ::core::clone::Clone::clone(&self.end),
                        }
                    }
                }
                pub fn index_range<S: TextSource>() -> impl Parser<
                    S,
                    Token = IndexRange<S::Span>,
                > {
                    (
                        expression_non_range().optional(),
                        spaced(':'),
                        expression_non_range().optional(),
                    )
                        .map(|(start, _, end)| IndexRange { start, end })
                }
            }
            pub use index_range::*;
            mod operation {
                use riddle::provided::common::chained;
                use super::*;
                pub fn operation<S: TextSource>() -> impl Parser<
                    S,
                    Token = Expression<S::Span>,
                > {
                    chained(spaced(expression_monome()), spaced(operator()))
                        .map(|
                            (
                                mut first,
                                mut tail,
                            ): (
                                Expression<S::Span>,
                                Vec<(ExactMatch<S::Span>, Expression<S::Span>)>,
                            )|
                        {
                            let operator_priority = |op: &ExactMatch<S::Span>| -> usize {
                                OPERATORS
                                    .iter()
                                    .cloned()
                                    .find(|(o, _)| *o == op.value)
                                    .map(|(_, p)| p)
                                    .unwrap()
                            };
                            fn pick_ith<'a, Span>(
                                first: &'a mut Expression<Span>,
                                tail: &'a mut Vec<(ExactMatch<Span>, Expression<Span>)>,
                                i: usize,
                            ) -> (
                                &'a mut Expression<Span>,
                                ExactMatch<Span>,
                                Expression<Span>,
                            ) {
                                let (op, expr) = tail.remove(i);
                                let l_expr = if i == 0 {
                                    first
                                } else {
                                    let (_, op) = tail.get_mut(i - 1).unwrap();
                                    op
                                };
                                (l_expr, op, expr)
                            }
                            while !tail.is_empty() {
                                let mut max_priority = 0;
                                let mut max_priority_i = 0;
                                for (i, (op, _)) in tail.iter().enumerate() {
                                    let priority = operator_priority(op);
                                    if priority > max_priority {
                                        max_priority = priority;
                                        max_priority_i = i;
                                    }
                                }
                                let (l_expr, op, r_expr) = pick_ith(
                                    &mut first,
                                    &mut tail,
                                    max_priority_i,
                                );
                                *l_expr = Expression::Operation {
                                    operator: op,
                                    left: Box::new(l_expr.clone()),
                                    right: Box::new(r_expr),
                                };
                            }
                            first
                        })
                }
            }
            pub use operation::*;
            mod monome {
                use riddle::prelude::*;
                use super::*;
                pub fn expression_monome<S: TextSource>() -> impl Parser<
                    S,
                    Token = Expression<S::Span>,
                > {
                    (move |source: S| {
                        if false {
                            ::core::panicking::panic(
                                "internal error: entered unreachable code",
                            )
                        } else if let ::riddle::tokenization::PResult(
                            Some(token),
                            source,
                        ) = (spaced(operator()), expression_monome())
                            .map(|(op, expr)| Expression::UnaryLeftOperation {
                                operator: op,
                                right: Box::new(expr),
                            })
                            .parse(source.clone())
                        {
                            ::riddle::tokenization::PResult(Some(token), source)
                        } else if let ::riddle::tokenization::PResult(
                            Some(token),
                            source,
                        ) = fortran_literal()
                            .map(Expression::Literal)
                            .parse(source.clone())
                        {
                            ::riddle::tokenization::PResult(Some(token), source)
                        } else if let ::riddle::tokenization::PResult(
                            Some(token),
                            source,
                        ) = identifier()
                            .map(Expression::Identifier)
                            .parse(source.clone())
                        {
                            ::riddle::tokenization::PResult(Some(token), source)
                        } else if let ::riddle::tokenization::PResult(
                            Some(token),
                            source,
                        ) = (spaced('('), spaced(expression()), spaced(')'))
                            .map(|(_, expr, _)| Expression::Parenthesis(Box::new(expr)))
                            .parse(source.clone())
                        {
                            ::riddle::tokenization::PResult(Some(token), source)
                        } else {
                            ::riddle::tokenization::PResult::unparsed(source)
                        }
                    })
                        .then(|expr| {
                            many(call_or_indexing(), 0..)
                                .map(move |calls| {
                                    let mut expr = expr.clone();
                                    for call in calls {
                                        expr = Expression::CallOrIndexing {
                                            function: Box::new(expr),
                                            arguments: call,
                                        };
                                    }
                                    expr
                                })
                        })
                }
                fn call_or_indexing<S: TextSource>() -> impl Parser<
                    S,
                    Token = Vec<Expression<S::Span>>,
                > {
                    (spaced('('), separated(spaced(expression()), ',', 0..), spaced(')'))
                        .map(|(_, args, _)| args)
                }
            }
            pub use monome::*;
            pub enum Expression<Span> {
                Literal(Literal<Span>),
                Identifier(Identifier<Span>),
                Parenthesis(Box<Expression<Span>>),
                CallOrIndexing {
                    function: Box<Expression<Span>>,
                    arguments: Vec<Expression<Span>>,
                },
                Operation {
                    left: Box<Expression<Span>>,
                    operator: ExactMatch<Span>,
                    right: Box<Expression<Span>>,
                },
                UnaryLeftOperation {
                    operator: ExactMatch<Span>,
                    right: Box<Expression<Span>>,
                },
                IndexRange(Box<IndexRange<Span>>),
            }
            #[automatically_derived]
            impl<Span: ::core::fmt::Debug> ::core::fmt::Debug for Expression<Span> {
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    match self {
                        Expression::Literal(__self_0) => {
                            ::core::fmt::Formatter::debug_tuple_field1_finish(
                                f,
                                "Literal",
                                &__self_0,
                            )
                        }
                        Expression::Identifier(__self_0) => {
                            ::core::fmt::Formatter::debug_tuple_field1_finish(
                                f,
                                "Identifier",
                                &__self_0,
                            )
                        }
                        Expression::Parenthesis(__self_0) => {
                            ::core::fmt::Formatter::debug_tuple_field1_finish(
                                f,
                                "Parenthesis",
                                &__self_0,
                            )
                        }
                        Expression::CallOrIndexing {
                            function: __self_0,
                            arguments: __self_1,
                        } => {
                            ::core::fmt::Formatter::debug_struct_field2_finish(
                                f,
                                "CallOrIndexing",
                                "function",
                                __self_0,
                                "arguments",
                                &__self_1,
                            )
                        }
                        Expression::Operation {
                            left: __self_0,
                            operator: __self_1,
                            right: __self_2,
                        } => {
                            ::core::fmt::Formatter::debug_struct_field3_finish(
                                f,
                                "Operation",
                                "left",
                                __self_0,
                                "operator",
                                __self_1,
                                "right",
                                &__self_2,
                            )
                        }
                        Expression::UnaryLeftOperation {
                            operator: __self_0,
                            right: __self_1,
                        } => {
                            ::core::fmt::Formatter::debug_struct_field2_finish(
                                f,
                                "UnaryLeftOperation",
                                "operator",
                                __self_0,
                                "right",
                                &__self_1,
                            )
                        }
                        Expression::IndexRange(__self_0) => {
                            ::core::fmt::Formatter::debug_tuple_field1_finish(
                                f,
                                "IndexRange",
                                &__self_0,
                            )
                        }
                    }
                }
            }
            #[automatically_derived]
            impl<Span: ::core::clone::Clone> ::core::clone::Clone for Expression<Span> {
                #[inline]
                fn clone(&self) -> Expression<Span> {
                    match self {
                        Expression::Literal(__self_0) => {
                            Expression::Literal(::core::clone::Clone::clone(__self_0))
                        }
                        Expression::Identifier(__self_0) => {
                            Expression::Identifier(::core::clone::Clone::clone(__self_0))
                        }
                        Expression::Parenthesis(__self_0) => {
                            Expression::Parenthesis(
                                ::core::clone::Clone::clone(__self_0),
                            )
                        }
                        Expression::CallOrIndexing {
                            function: __self_0,
                            arguments: __self_1,
                        } => {
                            Expression::CallOrIndexing {
                                function: ::core::clone::Clone::clone(__self_0),
                                arguments: ::core::clone::Clone::clone(__self_1),
                            }
                        }
                        Expression::Operation {
                            left: __self_0,
                            operator: __self_1,
                            right: __self_2,
                        } => {
                            Expression::Operation {
                                left: ::core::clone::Clone::clone(__self_0),
                                operator: ::core::clone::Clone::clone(__self_1),
                                right: ::core::clone::Clone::clone(__self_2),
                            }
                        }
                        Expression::UnaryLeftOperation {
                            operator: __self_0,
                            right: __self_1,
                        } => {
                            Expression::UnaryLeftOperation {
                                operator: ::core::clone::Clone::clone(__self_0),
                                right: ::core::clone::Clone::clone(__self_1),
                            }
                        }
                        Expression::IndexRange(__self_0) => {
                            Expression::IndexRange(::core::clone::Clone::clone(__self_0))
                        }
                    }
                }
            }
            pub fn expression_non_range<S: TextSource>() -> impl Parser<
                S,
                Token = Expression<S::Span>,
            > {
                move |source: S| {
                    if false {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = operation()
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = expression_monome()
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else {
                        ::riddle::tokenization::PResult::unparsed(source)
                    }
                }
            }
            pub fn expression<S: TextSource>() -> impl Parser<
                S,
                Token = Expression<S::Span>,
            > {
                move |source: S| {
                    if false {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = index_range()
                        .map(|r| Expression::IndexRange(Box::new(r)))
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = expression_non_range()
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else {
                        ::riddle::tokenization::PResult::unparsed(source)
                    }
                }
            }
        }
        pub use expression::*;
        mod keyword {
            use riddle::prelude::*;
            pub fn keyword<S: TextSource>(
                keyword: &'static str,
            ) -> impl Parser<S, Token = S::Span> {
                ExactMatch::exact(keyword, false).map(|m| m.span)
            }
        }
        pub use keyword::*;
        mod integer {
            use riddle::{prelude::*, provided::text::rusty::numeric_literal};
            pub fn integer_literal<S: TextSource>() -> impl Parser<
                S,
                Token = i128,
            > {
                numeric_literal().map(|lit| lit.value.parse::<i128>().unwrap())
            }
        }
        pub use integer::*;
        mod statement {
            use riddle::prelude::*;
            use crate::parse::Item;
            use super::*;
            mod use_statement {
                use crate::parse::{elements::spaced, eol_or_comment};
                use super::*;
                pub struct UseStatement {
                    pub module_name: String,
                    pub only: Vec<String>,
                }
                #[automatically_derived]
                impl ::core::fmt::Debug for UseStatement {
                    fn fmt(
                        &self,
                        f: &mut ::core::fmt::Formatter,
                    ) -> ::core::fmt::Result {
                        ::core::fmt::Formatter::debug_struct_field2_finish(
                            f,
                            "UseStatement",
                            "module_name",
                            &self.module_name,
                            "only",
                            &&self.only,
                        )
                    }
                }
                #[automatically_derived]
                impl ::core::clone::Clone for UseStatement {
                    #[inline]
                    fn clone(&self) -> UseStatement {
                        UseStatement {
                            module_name: ::core::clone::Clone::clone(&self.module_name),
                            only: ::core::clone::Clone::clone(&self.only),
                        }
                    }
                }
                pub fn use_statement<S: TextSource>() -> impl Parser<
                    S,
                    Token = UseStatement,
                > {
                    (
                        spaced(keyword("use")),
                        spaced(identifier()),
                        (
                            spaced(','),
                            spaced(keyword("only")),
                            spaced(':'),
                            separated(spaced(identifier()), ',', 0..),
                        )
                            .optional(),
                        eol_or_comment(),
                    )
                        .map(|(_, module, only, _)| {
                            let only = only
                                .map(|(_, _, _, ids)| ids)
                                .unwrap_or(::alloc::vec::Vec::new());
                            let only = only.into_iter().map(|id| id.value).collect();
                            UseStatement {
                                module_name: module.value,
                                only,
                            }
                        })
                }
            }
            pub use use_statement::*;
            mod print_statement {
                use riddle::provided::text::rusty::StringLiteral;
                use crate::parse::eol_or_comment;
                use super::*;
                pub enum PrintFormat<Span> {
                    Star,
                    FormatString(StringLiteral<Span>),
                }
                #[automatically_derived]
                impl<Span: ::core::fmt::Debug> ::core::fmt::Debug for PrintFormat<Span> {
                    fn fmt(
                        &self,
                        f: &mut ::core::fmt::Formatter,
                    ) -> ::core::fmt::Result {
                        match self {
                            PrintFormat::Star => {
                                ::core::fmt::Formatter::write_str(f, "Star")
                            }
                            PrintFormat::FormatString(__self_0) => {
                                ::core::fmt::Formatter::debug_tuple_field1_finish(
                                    f,
                                    "FormatString",
                                    &__self_0,
                                )
                            }
                        }
                    }
                }
                #[automatically_derived]
                impl<Span: ::core::clone::Clone> ::core::clone::Clone
                for PrintFormat<Span> {
                    #[inline]
                    fn clone(&self) -> PrintFormat<Span> {
                        match self {
                            PrintFormat::Star => PrintFormat::Star,
                            PrintFormat::FormatString(__self_0) => {
                                PrintFormat::FormatString(
                                    ::core::clone::Clone::clone(__self_0),
                                )
                            }
                        }
                    }
                }
                pub fn print_format<S: TextSource>() -> impl Parser<
                    S,
                    Token = PrintFormat<S::Span>,
                > {
                    move |source: S| {
                        if false {
                            ::core::panicking::panic(
                                "internal error: entered unreachable code",
                            )
                        } else if let ::riddle::tokenization::PResult(
                            Some(token),
                            source,
                        ) = spaced('*').map(|_| PrintFormat::Star).parse(source.clone())
                        {
                            ::riddle::tokenization::PResult(Some(token), source)
                        } else if let ::riddle::tokenization::PResult(
                            Some(token),
                            source,
                        ) = spaced(string_literal())
                            .map(PrintFormat::FormatString)
                            .parse(source.clone())
                        {
                            ::riddle::tokenization::PResult(Some(token), source)
                        } else {
                            ::riddle::tokenization::PResult::unparsed(source)
                        }
                    }
                }
                pub struct PrintStatement<Span> {
                    pub format: PrintFormat<Span>,
                    pub items: Vec<Expression<Span>>,
                }
                #[automatically_derived]
                impl<Span: ::core::fmt::Debug> ::core::fmt::Debug
                for PrintStatement<Span> {
                    fn fmt(
                        &self,
                        f: &mut ::core::fmt::Formatter,
                    ) -> ::core::fmt::Result {
                        ::core::fmt::Formatter::debug_struct_field2_finish(
                            f,
                            "PrintStatement",
                            "format",
                            &self.format,
                            "items",
                            &&self.items,
                        )
                    }
                }
                #[automatically_derived]
                impl<Span: ::core::clone::Clone> ::core::clone::Clone
                for PrintStatement<Span> {
                    #[inline]
                    fn clone(&self) -> PrintStatement<Span> {
                        PrintStatement {
                            format: ::core::clone::Clone::clone(&self.format),
                            items: ::core::clone::Clone::clone(&self.items),
                        }
                    }
                }
                pub fn print_statement<S: TextSource>() -> impl Parser<
                    S,
                    Token = PrintStatement<S::Span>,
                > {
                    (
                        spaced(keyword("print")),
                        print_format(),
                        spaced(','),
                        separated(expression(), spaced(','), 0..),
                        eol_or_comment(),
                    )
                        .map(|(_, format, _, items, _)| PrintStatement { format, items })
                }
            }
            pub use print_statement::*;
            mod variables_declaration {
                use super::*;
                pub struct VariablesDeclaration<Span> {
                    pub vars: Vec<(Type<Span>, String)>,
                }
                #[automatically_derived]
                impl<Span: ::core::fmt::Debug> ::core::fmt::Debug
                for VariablesDeclaration<Span> {
                    fn fmt(
                        &self,
                        f: &mut ::core::fmt::Formatter,
                    ) -> ::core::fmt::Result {
                        ::core::fmt::Formatter::debug_struct_field1_finish(
                            f,
                            "VariablesDeclaration",
                            "vars",
                            &&self.vars,
                        )
                    }
                }
                #[automatically_derived]
                impl<Span: ::core::clone::Clone> ::core::clone::Clone
                for VariablesDeclaration<Span> {
                    #[inline]
                    fn clone(&self) -> VariablesDeclaration<Span> {
                        VariablesDeclaration {
                            vars: ::core::clone::Clone::clone(&self.vars),
                        }
                    }
                }
                pub fn variable_declaree<S: TextSource>(
                    base_type: Type<S::Span>,
                ) -> impl Parser<S, Token = (Type<S::Span>, String)> {
                    (
                        spaced(identifier()),
                        (
                            spaced('('),
                            separated(expression(), spaced(','), 0..),
                            spaced(')'),
                        )
                            .map(|(_, ranges, _)| ranges)
                            .optional(),
                    )
                        .map(move |(name, array_ranges)| {
                            let ty = if let Some(ranges) = array_ranges {
                                Type::Array {
                                    ty: Box::new(base_type.clone()),
                                    ranges,
                                }
                            } else {
                                base_type.clone()
                            };
                            (ty, name.value)
                        })
                }
                pub fn variables_declaration<S: TextSource>() -> impl Parser<
                    S,
                    Token = VariablesDeclaration<S::Span>,
                > {
                    let d = spaced(type_())
                        .then(move |ty| {
                            variable_declaree(ty.clone())
                                .map(|var| <[_]>::into_vec(
                                    #[rustc_box]
                                    ::alloc::boxed::Box::new([var]),
                                ))
                                .or(
                                    (
                                        spaced(ExactMatch::exact("::", false)),
                                        separated(
                                            variable_declaree(ty.clone()),
                                            spaced(ExactMatch::exact(",", false)),
                                            1..,
                                        ),
                                    )
                                        .map(move |(_, vars)| vars),
                                )
                        });
                    (d, eol_or_comment()).map(|(vars, _)| VariablesDeclaration { vars })
                }
            }
            pub use variables_declaration::*;
            mod do_loop {
                use crate::parse::item;
                use super::*;
                pub fn do_loop<S: TextSource>() -> impl Parser<
                    S,
                    Token = Statement<S::Span>,
                > {
                    (
                        (
                            spaced(keyword("do")),
                            spaced(identifier()),
                            spaced('='),
                            spaced(expression()),
                            spaced(','),
                            spaced(expression()),
                            (spaced(','), spaced(expression()))
                                .optional()
                                .map(|opt| opt.map(|(_, step)| step)),
                            eol_or_comment(),
                        )
                            .map(|(_, var, _, from, _, to, step, _)| (
                                var,
                                from,
                                to,
                                step,
                            )),
                        many_until(
                                item(),
                                (
                                    spaced(keyword("end")),
                                    spaced(keyword("do").optional()),
                                    eol_or_comment(),
                                )
                                    .map(|_| ()),
                                0..,
                            )
                            .map(|(body, _)| body),
                    )
                        .map(|((var, from, to, step), body)| Statement::DoLoop {
                            variable: var.value,
                            start: from,
                            end: to,
                            step,
                            body,
                        })
                }
            }
            pub use do_loop::*;
            mod if_statement {
                use crate::parse::item;
                use super::*;
                pub fn if_<S: TextSource>() -> impl Parser<
                    S,
                    Token = Statement<S::Span>,
                > {
                    enum BodyTermination {
                        EndIf,
                        ElseIf,
                    }
                    #[automatically_derived]
                    impl ::core::fmt::Debug for BodyTermination {
                        fn fmt(
                            &self,
                            f: &mut ::core::fmt::Formatter,
                        ) -> ::core::fmt::Result {
                            ::core::fmt::Formatter::write_str(
                                f,
                                match self {
                                    BodyTermination::EndIf => "EndIf",
                                    BodyTermination::ElseIf => "ElseIf",
                                },
                            )
                        }
                    }
                    #[automatically_derived]
                    impl ::core::clone::Clone for BodyTermination {
                        #[inline]
                        fn clone(&self) -> BodyTermination {
                            *self
                        }
                    }
                    #[automatically_derived]
                    impl ::core::marker::Copy for BodyTermination {}
                    #[automatically_derived]
                    impl ::core::marker::StructuralPartialEq for BodyTermination {}
                    #[automatically_derived]
                    impl ::core::cmp::PartialEq for BodyTermination {
                        #[inline]
                        fn eq(&self, other: &BodyTermination) -> bool {
                            let __self_tag = ::core::intrinsics::discriminant_value(
                                self,
                            );
                            let __arg1_tag = ::core::intrinsics::discriminant_value(
                                other,
                            );
                            __self_tag == __arg1_tag
                        }
                    }
                    #[automatically_derived]
                    impl ::core::marker::StructuralEq for BodyTermination {}
                    #[automatically_derived]
                    impl ::core::cmp::Eq for BodyTermination {
                        #[inline]
                        #[doc(hidden)]
                        #[no_coverage]
                        fn assert_receiver_is_total_eq(&self) -> () {}
                    }
                    #[automatically_derived]
                    impl ::core::cmp::PartialOrd for BodyTermination {
                        #[inline]
                        fn partial_cmp(
                            &self,
                            other: &BodyTermination,
                        ) -> ::core::option::Option<::core::cmp::Ordering> {
                            let __self_tag = ::core::intrinsics::discriminant_value(
                                self,
                            );
                            let __arg1_tag = ::core::intrinsics::discriminant_value(
                                other,
                            );
                            ::core::cmp::PartialOrd::partial_cmp(
                                &__self_tag,
                                &__arg1_tag,
                            )
                        }
                    }
                    #[automatically_derived]
                    impl ::core::cmp::Ord for BodyTermination {
                        #[inline]
                        fn cmp(&self, other: &BodyTermination) -> ::core::cmp::Ordering {
                            let __self_tag = ::core::intrinsics::discriminant_value(
                                self,
                            );
                            let __arg1_tag = ::core::intrinsics::discriminant_value(
                                other,
                            );
                            ::core::cmp::Ord::cmp(&__self_tag, &__arg1_tag)
                        }
                    }
                    fn if_termination<S: TextSource>() -> impl Parser<
                        S,
                        Token = BodyTermination,
                    > {
                        (
                            spaced(keyword("end")),
                            spaced(keyword("if")).optional(),
                            eol_or_comment(),
                        )
                            .map(|_| BodyTermination::EndIf)
                    }
                    fn ok_body_termination<S: TextSource>() -> impl Parser<
                        S,
                        Token = BodyTermination,
                    > {
                        if_termination()
                            .or(
                                (
                                    spaced(keyword("else")),
                                    spaced(keyword("if").optional()),
                                    eol_or_comment(),
                                )
                                    .map(|_| BodyTermination::ElseIf),
                            )
                    }
                    (
                        (
                            spaced(keyword("if")),
                            spaced(expression()),
                            spaced(keyword("then")),
                            eol_or_comment(),
                        )
                            .map(|(_, cond, _, _)| cond),
                        many_until(item(), ok_body_termination(), 0..)
                            .then(|(body, termination)| {
                                many_until(item(), if_termination(), 0..)
                                    .map(|(else_body, _)| else_body)
                                    .where_(termination == Some(BodyTermination::ElseIf))
                                    .map(move |else_body| (body.clone(), else_body))
                            }),
                    )
                        .map(|(condition, (body, else_body))| Statement::If {
                            condition,
                            body,
                            else_body,
                        })
                }
            }
            pub use if_statement::*;
            mod call_statement {
                use super::*;
                pub fn call_statement<S: TextSource>() -> impl Parser<
                    S,
                    Token = Statement<S::Span>,
                > {
                    (spaced(keyword("call")), spaced(expression()), eol_or_comment())
                        .map(|(_, expr, _)| Statement::CallStatement(expr))
                }
            }
            pub use call_statement::*;
            pub enum Statement<Span> {
                UseStatement(UseStatement),
                VariablesDeclaration(VariablesDeclaration<Span>),
                Expression(Expression<Span>),
                DoLoop {
                    variable: String,
                    start: Expression<Span>,
                    end: Expression<Span>,
                    step: Option<Expression<Span>>,
                    body: Vec<Item<Span>>,
                },
                If {
                    condition: Expression<Span>,
                    body: Vec<Item<Span>>,
                    else_body: Option<Vec<Item<Span>>>,
                },
                CallStatement(Expression<Span>),
                PrintStatement(PrintStatement<Span>),
            }
            #[automatically_derived]
            impl<Span: ::core::fmt::Debug> ::core::fmt::Debug for Statement<Span> {
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    match self {
                        Statement::UseStatement(__self_0) => {
                            ::core::fmt::Formatter::debug_tuple_field1_finish(
                                f,
                                "UseStatement",
                                &__self_0,
                            )
                        }
                        Statement::VariablesDeclaration(__self_0) => {
                            ::core::fmt::Formatter::debug_tuple_field1_finish(
                                f,
                                "VariablesDeclaration",
                                &__self_0,
                            )
                        }
                        Statement::Expression(__self_0) => {
                            ::core::fmt::Formatter::debug_tuple_field1_finish(
                                f,
                                "Expression",
                                &__self_0,
                            )
                        }
                        Statement::DoLoop {
                            variable: __self_0,
                            start: __self_1,
                            end: __self_2,
                            step: __self_3,
                            body: __self_4,
                        } => {
                            ::core::fmt::Formatter::debug_struct_field5_finish(
                                f,
                                "DoLoop",
                                "variable",
                                __self_0,
                                "start",
                                __self_1,
                                "end",
                                __self_2,
                                "step",
                                __self_3,
                                "body",
                                &__self_4,
                            )
                        }
                        Statement::If {
                            condition: __self_0,
                            body: __self_1,
                            else_body: __self_2,
                        } => {
                            ::core::fmt::Formatter::debug_struct_field3_finish(
                                f,
                                "If",
                                "condition",
                                __self_0,
                                "body",
                                __self_1,
                                "else_body",
                                &__self_2,
                            )
                        }
                        Statement::CallStatement(__self_0) => {
                            ::core::fmt::Formatter::debug_tuple_field1_finish(
                                f,
                                "CallStatement",
                                &__self_0,
                            )
                        }
                        Statement::PrintStatement(__self_0) => {
                            ::core::fmt::Formatter::debug_tuple_field1_finish(
                                f,
                                "PrintStatement",
                                &__self_0,
                            )
                        }
                    }
                }
            }
            #[automatically_derived]
            impl<Span: ::core::clone::Clone> ::core::clone::Clone for Statement<Span> {
                #[inline]
                fn clone(&self) -> Statement<Span> {
                    match self {
                        Statement::UseStatement(__self_0) => {
                            Statement::UseStatement(
                                ::core::clone::Clone::clone(__self_0),
                            )
                        }
                        Statement::VariablesDeclaration(__self_0) => {
                            Statement::VariablesDeclaration(
                                ::core::clone::Clone::clone(__self_0),
                            )
                        }
                        Statement::Expression(__self_0) => {
                            Statement::Expression(::core::clone::Clone::clone(__self_0))
                        }
                        Statement::DoLoop {
                            variable: __self_0,
                            start: __self_1,
                            end: __self_2,
                            step: __self_3,
                            body: __self_4,
                        } => {
                            Statement::DoLoop {
                                variable: ::core::clone::Clone::clone(__self_0),
                                start: ::core::clone::Clone::clone(__self_1),
                                end: ::core::clone::Clone::clone(__self_2),
                                step: ::core::clone::Clone::clone(__self_3),
                                body: ::core::clone::Clone::clone(__self_4),
                            }
                        }
                        Statement::If {
                            condition: __self_0,
                            body: __self_1,
                            else_body: __self_2,
                        } => {
                            Statement::If {
                                condition: ::core::clone::Clone::clone(__self_0),
                                body: ::core::clone::Clone::clone(__self_1),
                                else_body: ::core::clone::Clone::clone(__self_2),
                            }
                        }
                        Statement::CallStatement(__self_0) => {
                            Statement::CallStatement(
                                ::core::clone::Clone::clone(__self_0),
                            )
                        }
                        Statement::PrintStatement(__self_0) => {
                            Statement::PrintStatement(
                                ::core::clone::Clone::clone(__self_0),
                            )
                        }
                    }
                }
            }
            pub fn statement<S: TextSource>() -> impl Parser<
                S,
                Token = Statement<S::Span>,
            > {
                move |source: S| {
                    if false {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = use_statement()
                        .map(Statement::UseStatement)
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = variables_declaration()
                        .map(Statement::VariablesDeclaration)
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = do_loop()
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = if_()
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = call_statement()
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = print_statement()
                        .map(Statement::PrintStatement)
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = (
                        expression(),
                        eol_or_comment(),
                    )
                        .map(|(e, _)| e)
                        .map(Statement::Expression)
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else {
                        ::riddle::tokenization::PResult::unparsed(source)
                    }
                }
            }
        }
        pub use statement::*;
        mod type_ {
            use super::*;
            use riddle::{prelude::*, provided::text::rusty::Identifier};
            pub enum BasicType {
                Integer,
                Integer2,
                Integer4,
                Integer8,
                Real,
                Real4,
                Real8,
                Real16,
                Complex,
                Complex8,
                Complex16,
                Complex32,
                DoubleComplex,
                Character,
                CharacterN(usize),
                Logical,
                Logical1,
                Logical2,
                Logical4,
                Logical8,
                DoublePrecision,
            }
            #[automatically_derived]
            impl ::core::fmt::Debug for BasicType {
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    match self {
                        BasicType::Integer => {
                            ::core::fmt::Formatter::write_str(f, "Integer")
                        }
                        BasicType::Integer2 => {
                            ::core::fmt::Formatter::write_str(f, "Integer2")
                        }
                        BasicType::Integer4 => {
                            ::core::fmt::Formatter::write_str(f, "Integer4")
                        }
                        BasicType::Integer8 => {
                            ::core::fmt::Formatter::write_str(f, "Integer8")
                        }
                        BasicType::Real => ::core::fmt::Formatter::write_str(f, "Real"),
                        BasicType::Real4 => ::core::fmt::Formatter::write_str(f, "Real4"),
                        BasicType::Real8 => ::core::fmt::Formatter::write_str(f, "Real8"),
                        BasicType::Real16 => {
                            ::core::fmt::Formatter::write_str(f, "Real16")
                        }
                        BasicType::Complex => {
                            ::core::fmt::Formatter::write_str(f, "Complex")
                        }
                        BasicType::Complex8 => {
                            ::core::fmt::Formatter::write_str(f, "Complex8")
                        }
                        BasicType::Complex16 => {
                            ::core::fmt::Formatter::write_str(f, "Complex16")
                        }
                        BasicType::Complex32 => {
                            ::core::fmt::Formatter::write_str(f, "Complex32")
                        }
                        BasicType::DoubleComplex => {
                            ::core::fmt::Formatter::write_str(f, "DoubleComplex")
                        }
                        BasicType::Character => {
                            ::core::fmt::Formatter::write_str(f, "Character")
                        }
                        BasicType::CharacterN(__self_0) => {
                            ::core::fmt::Formatter::debug_tuple_field1_finish(
                                f,
                                "CharacterN",
                                &__self_0,
                            )
                        }
                        BasicType::Logical => {
                            ::core::fmt::Formatter::write_str(f, "Logical")
                        }
                        BasicType::Logical1 => {
                            ::core::fmt::Formatter::write_str(f, "Logical1")
                        }
                        BasicType::Logical2 => {
                            ::core::fmt::Formatter::write_str(f, "Logical2")
                        }
                        BasicType::Logical4 => {
                            ::core::fmt::Formatter::write_str(f, "Logical4")
                        }
                        BasicType::Logical8 => {
                            ::core::fmt::Formatter::write_str(f, "Logical8")
                        }
                        BasicType::DoublePrecision => {
                            ::core::fmt::Formatter::write_str(f, "DoublePrecision")
                        }
                    }
                }
            }
            #[automatically_derived]
            impl ::core::clone::Clone for BasicType {
                #[inline]
                fn clone(&self) -> BasicType {
                    let _: ::core::clone::AssertParamIsClone<usize>;
                    *self
                }
            }
            #[automatically_derived]
            impl ::core::marker::Copy for BasicType {}
            #[automatically_derived]
            impl ::core::marker::StructuralPartialEq for BasicType {}
            #[automatically_derived]
            impl ::core::cmp::PartialEq for BasicType {
                #[inline]
                fn eq(&self, other: &BasicType) -> bool {
                    let __self_tag = ::core::intrinsics::discriminant_value(self);
                    let __arg1_tag = ::core::intrinsics::discriminant_value(other);
                    __self_tag == __arg1_tag
                        && match (self, other) {
                            (
                                BasicType::CharacterN(__self_0),
                                BasicType::CharacterN(__arg1_0),
                            ) => *__self_0 == *__arg1_0,
                            _ => true,
                        }
                }
            }
            #[automatically_derived]
            impl ::core::marker::StructuralEq for BasicType {}
            #[automatically_derived]
            impl ::core::cmp::Eq for BasicType {
                #[inline]
                #[doc(hidden)]
                #[no_coverage]
                fn assert_receiver_is_total_eq(&self) -> () {
                    let _: ::core::cmp::AssertParamIsEq<usize>;
                }
            }
            #[automatically_derived]
            impl ::core::cmp::PartialOrd for BasicType {
                #[inline]
                fn partial_cmp(
                    &self,
                    other: &BasicType,
                ) -> ::core::option::Option<::core::cmp::Ordering> {
                    let __self_tag = ::core::intrinsics::discriminant_value(self);
                    let __arg1_tag = ::core::intrinsics::discriminant_value(other);
                    match (self, other) {
                        (
                            BasicType::CharacterN(__self_0),
                            BasicType::CharacterN(__arg1_0),
                        ) => ::core::cmp::PartialOrd::partial_cmp(__self_0, __arg1_0),
                        _ => {
                            ::core::cmp::PartialOrd::partial_cmp(
                                &__self_tag,
                                &__arg1_tag,
                            )
                        }
                    }
                }
            }
            #[automatically_derived]
            impl ::core::cmp::Ord for BasicType {
                #[inline]
                fn cmp(&self, other: &BasicType) -> ::core::cmp::Ordering {
                    let __self_tag = ::core::intrinsics::discriminant_value(self);
                    let __arg1_tag = ::core::intrinsics::discriminant_value(other);
                    match ::core::cmp::Ord::cmp(&__self_tag, &__arg1_tag) {
                        ::core::cmp::Ordering::Equal => {
                            match (self, other) {
                                (
                                    BasicType::CharacterN(__self_0),
                                    BasicType::CharacterN(__arg1_0),
                                ) => ::core::cmp::Ord::cmp(__self_0, __arg1_0),
                                _ => ::core::cmp::Ordering::Equal,
                            }
                        }
                        cmp => cmp,
                    }
                }
            }
            pub fn basic_type<S: TextSource>() -> impl Parser<
                S,
                Token = BasicType,
            > {
                move |source: S| {
                    if false {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = (
                        keyword("integer"),
                        spaced('*'),
                        keyword("2"),
                    )
                        .map(|_| BasicType::Integer2)
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = (
                        keyword("integer"),
                        spaced('*'),
                        keyword("4"),
                    )
                        .map(|_| BasicType::Integer4)
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = (
                        keyword("integer"),
                        spaced('*'),
                        keyword("8"),
                    )
                        .map(|_| BasicType::Integer8)
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = (
                        keyword("integer"),
                        spaced('('),
                        keyword("2"),
                        spaced(')'),
                    )
                        .map(|_| BasicType::Integer2)
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = (
                        keyword("integer"),
                        spaced('('),
                        keyword("4"),
                        spaced(')'),
                    )
                        .map(|_| BasicType::Integer4)
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = (
                        keyword("integer"),
                        spaced('('),
                        keyword("8"),
                        spaced(')'),
                    )
                        .map(|_| BasicType::Integer8)
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = keyword(
                            "integer",
                        )
                        .map(|_| BasicType::Integer)
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = (
                        keyword("real"),
                        spaced('*'),
                        keyword("4"),
                    )
                        .map(|_| BasicType::Real4)
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = (
                        keyword("real"),
                        spaced('*'),
                        keyword("8"),
                    )
                        .map(|_| BasicType::Real8)
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = (
                        keyword("real"),
                        spaced('*'),
                        keyword("16"),
                    )
                        .map(|_| BasicType::Real16)
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = (
                        keyword("real"),
                        spaced('('),
                        keyword("4"),
                        spaced(')'),
                    )
                        .map(|_| BasicType::Real4)
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = (
                        keyword("real"),
                        spaced('('),
                        keyword("8"),
                        spaced(')'),
                    )
                        .map(|_| BasicType::Real8)
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = (
                        keyword("real"),
                        spaced('('),
                        keyword("16"),
                        spaced(')'),
                    )
                        .map(|_| BasicType::Real16)
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = keyword(
                            "real",
                        )
                        .map(|_| BasicType::Real)
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = (
                        keyword("complex"),
                        spaced('*'),
                        keyword("8"),
                    )
                        .map(|_| BasicType::Complex8)
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = (
                        keyword("complex"),
                        spaced('*'),
                        keyword("16"),
                    )
                        .map(|_| BasicType::Complex16)
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = (
                        keyword("complex"),
                        spaced('*'),
                        keyword("32"),
                    )
                        .map(|_| BasicType::Complex32)
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = (
                        keyword("complex"),
                        spaced('('),
                        keyword("8"),
                        spaced(')'),
                    )
                        .map(|_| BasicType::Complex8)
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = (
                        keyword("complex"),
                        spaced('('),
                        keyword("16"),
                        spaced(')'),
                    )
                        .map(|_| BasicType::Complex16)
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = (
                        keyword("complex"),
                        spaced('('),
                        keyword("32"),
                        spaced(')'),
                    )
                        .map(|_| BasicType::Complex32)
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = keyword(
                            "complex",
                        )
                        .map(|_| BasicType::Complex)
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = (
                        keyword("double"),
                        space(),
                        keyword("complex"),
                    )
                        .map(|_| BasicType::DoubleComplex)
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = (
                        keyword("logical"),
                        spaced('*'),
                        keyword("1"),
                    )
                        .map(|_| BasicType::Logical1)
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = (
                        keyword("logical"),
                        spaced('*'),
                        keyword("2"),
                    )
                        .map(|_| BasicType::Logical2)
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = (
                        keyword("logical"),
                        spaced('*'),
                        keyword("4"),
                    )
                        .map(|_| BasicType::Logical4)
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = (
                        keyword("logical"),
                        spaced('*'),
                        keyword("8"),
                    )
                        .map(|_| BasicType::Logical8)
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = (
                        keyword("logical"),
                        spaced('('),
                        keyword("1"),
                        spaced(')'),
                    )
                        .map(|_| BasicType::Logical1)
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = (
                        keyword("logical"),
                        spaced('('),
                        keyword("2"),
                        spaced(')'),
                    )
                        .map(|_| BasicType::Logical2)
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = (
                        keyword("logical"),
                        spaced('('),
                        keyword("4"),
                        spaced(')'),
                    )
                        .map(|_| BasicType::Logical4)
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = (
                        keyword("logical"),
                        spaced('('),
                        keyword("8"),
                        spaced(')'),
                    )
                        .map(|_| BasicType::Logical8)
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = keyword(
                            "logical",
                        )
                        .map(|_| BasicType::Logical)
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = (
                        keyword("character"),
                        spaced('*'),
                        integer_literal(),
                    )
                        .map(|(_, _, n)| BasicType::CharacterN(n as usize))
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = (
                        keyword("character"),
                        spaced('('),
                        integer_literal(),
                        spaced(')'),
                    )
                        .map(|(_, _, n, _)| BasicType::CharacterN(n as usize))
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = keyword(
                            "character",
                        )
                        .map(|_| BasicType::Character)
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = (
                        keyword("double"),
                        space(),
                        keyword("precision"),
                    )
                        .map(|_| BasicType::DoublePrecision)
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else {
                        ::riddle::tokenization::PResult::unparsed(source)
                    }
                }
            }
            pub enum Type<Span> {
                /// ex: `integer`
                Basic(BasicType),
                /// Ex: `integer(c_int)`
                BasicAlias(BasicType, Identifier<Span>),
                Type(Box<Type<Span>>),
                Array { ty: Box<Type<Span>>, ranges: Vec<Expression<Span>> },
            }
            #[automatically_derived]
            impl<Span: ::core::fmt::Debug> ::core::fmt::Debug for Type<Span> {
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    match self {
                        Type::Basic(__self_0) => {
                            ::core::fmt::Formatter::debug_tuple_field1_finish(
                                f,
                                "Basic",
                                &__self_0,
                            )
                        }
                        Type::BasicAlias(__self_0, __self_1) => {
                            ::core::fmt::Formatter::debug_tuple_field2_finish(
                                f,
                                "BasicAlias",
                                __self_0,
                                &__self_1,
                            )
                        }
                        Type::Type(__self_0) => {
                            ::core::fmt::Formatter::debug_tuple_field1_finish(
                                f,
                                "Type",
                                &__self_0,
                            )
                        }
                        Type::Array { ty: __self_0, ranges: __self_1 } => {
                            ::core::fmt::Formatter::debug_struct_field2_finish(
                                f,
                                "Array",
                                "ty",
                                __self_0,
                                "ranges",
                                &__self_1,
                            )
                        }
                    }
                }
            }
            #[automatically_derived]
            impl<Span: ::core::clone::Clone> ::core::clone::Clone for Type<Span> {
                #[inline]
                fn clone(&self) -> Type<Span> {
                    match self {
                        Type::Basic(__self_0) => {
                            Type::Basic(::core::clone::Clone::clone(__self_0))
                        }
                        Type::BasicAlias(__self_0, __self_1) => {
                            Type::BasicAlias(
                                ::core::clone::Clone::clone(__self_0),
                                ::core::clone::Clone::clone(__self_1),
                            )
                        }
                        Type::Type(__self_0) => {
                            Type::Type(::core::clone::Clone::clone(__self_0))
                        }
                        Type::Array { ty: __self_0, ranges: __self_1 } => {
                            Type::Array {
                                ty: ::core::clone::Clone::clone(__self_0),
                                ranges: ::core::clone::Clone::clone(__self_1),
                            }
                        }
                    }
                }
            }
            pub fn type_<S: TextSource>() -> impl Parser<S, Token = Type<S::Span>> {
                move |source: S| {
                    if false {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = (
                        basic_type(),
                        spaced('('),
                        identifier(),
                        spaced(')'),
                    )
                        .map(|(ty, _, alias, _)| Type::BasicAlias(ty, alias))
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = basic_type()
                        .map(Type::Basic)
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = (
                        keyword("type"),
                        spaced('('),
                        type_(),
                        spaced(')'),
                    )
                        .map(|(_, _, ty, _)| Type::Type(Box::new(ty)))
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else {
                        ::riddle::tokenization::PResult::unparsed(source)
                    }
                }
            }
        }
        pub use type_::*;
        mod eol {
            use riddle::prelude::*;
            use super::*;
            /// End of line
            pub fn eol<S: TextSource>() -> impl Parser<S, Token = ()> {
                ('\n', Char::exact('\r').optional()).map(|_| ()).or(eof())
            }
            pub fn eol_or_comment<S: TextSource>() -> impl Parser<
                S,
                Token = Option<LineComment<S::Span>>,
            > {
                move |source: S| {
                    if false {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = eol()
                        .map(|_| None)
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else if let ::riddle::tokenization::PResult(Some(token), source) = line_comment()
                        .map(|c| Some(c))
                        .parse(source.clone())
                    {
                        ::riddle::tokenization::PResult(Some(token), source)
                    } else {
                        ::riddle::tokenization::PResult::unparsed(source)
                    }
                }
            }
        }
        pub use eol::*;
    }
    pub enum Item<Span> {
        EmptyLines(EmptyLines),
        LineComment(LineComment<Span>),
        Program(Program<Span>),
        Statement(Statement<Span>),
        UnclassifiedLine(Span),
    }
    #[automatically_derived]
    impl<Span: ::core::fmt::Debug> ::core::fmt::Debug for Item<Span> {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                Item::EmptyLines(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "EmptyLines",
                        &__self_0,
                    )
                }
                Item::LineComment(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "LineComment",
                        &__self_0,
                    )
                }
                Item::Program(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Program",
                        &__self_0,
                    )
                }
                Item::Statement(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Statement",
                        &__self_0,
                    )
                }
                Item::UnclassifiedLine(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "UnclassifiedLine",
                        &__self_0,
                    )
                }
            }
        }
    }
    #[automatically_derived]
    impl<Span: ::core::clone::Clone> ::core::clone::Clone for Item<Span> {
        #[inline]
        fn clone(&self) -> Item<Span> {
            match self {
                Item::EmptyLines(__self_0) => {
                    Item::EmptyLines(::core::clone::Clone::clone(__self_0))
                }
                Item::LineComment(__self_0) => {
                    Item::LineComment(::core::clone::Clone::clone(__self_0))
                }
                Item::Program(__self_0) => {
                    Item::Program(::core::clone::Clone::clone(__self_0))
                }
                Item::Statement(__self_0) => {
                    Item::Statement(::core::clone::Clone::clone(__self_0))
                }
                Item::UnclassifiedLine(__self_0) => {
                    Item::UnclassifiedLine(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    pub fn item<S: TextSource>() -> impl Parser<S, Token = Item<S::Span>> {
        move |source: S| {
            if false {
                ::core::panicking::panic("internal error: entered unreachable code")
            } else if let ::riddle::tokenization::PResult(Some(token), source) = empty_lines()
                .map(Item::EmptyLines)
                .parse(source.clone())
            {
                ::riddle::tokenization::PResult(Some(token), source)
            } else if let ::riddle::tokenization::PResult(Some(token), source) = line_comment()
                .map(Item::LineComment)
                .parse(source.clone())
            {
                ::riddle::tokenization::PResult(Some(token), source)
            } else if let ::riddle::tokenization::PResult(Some(token), source) = program_definition()
                .map(Item::Program)
                .parse(source.clone())
            {
                ::riddle::tokenization::PResult(Some(token), source)
            } else if let ::riddle::tokenization::PResult(Some(token), source) = statement()
                .map(Item::Statement)
                .parse(source.clone())
            {
                ::riddle::tokenization::PResult(Some(token), source)
            } else if let ::riddle::tokenization::PResult(Some(token), source) = unclassified_line()
                .map(Item::UnclassifiedLine)
                .parse(source.clone())
            {
                ::riddle::tokenization::PResult(Some(token), source)
            } else {
                ::riddle::tokenization::PResult::unparsed(source)
            }
        }
    }
    pub fn items<S: TextSource>() -> impl Parser<S, Token = File<S::Span>> {
        many(item(), 0..).map(|items| File { items })
    }
    pub struct File<Span> {
        pub items: Vec<Item<Span>>,
    }
    #[automatically_derived]
    impl<Span: ::core::fmt::Debug> ::core::fmt::Debug for File<Span> {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field1_finish(
                f,
                "File",
                "items",
                &&self.items,
            )
        }
    }
    #[automatically_derived]
    impl<Span: ::core::clone::Clone> ::core::clone::Clone for File<Span> {
        #[inline]
        fn clone(&self) -> File<Span> {
            File {
                items: ::core::clone::Clone::clone(&self.items),
            }
        }
    }
    pub struct Program<Span> {
        pub name: String,
        pub items: Vec<Item<Span>>,
    }
    #[automatically_derived]
    impl<Span: ::core::fmt::Debug> ::core::fmt::Debug for Program<Span> {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "Program",
                "name",
                &self.name,
                "items",
                &&self.items,
            )
        }
    }
    #[automatically_derived]
    impl<Span: ::core::clone::Clone> ::core::clone::Clone for Program<Span> {
        #[inline]
        fn clone(&self) -> Program<Span> {
            Program {
                name: ::core::clone::Clone::clone(&self.name),
                items: ::core::clone::Clone::clone(&self.items),
            }
        }
    }
    pub fn program_declaration<S: TextSource>() -> impl Parser<S, Token = String> {
        (spaced(keyword("program")), spaced(identifier()), eol_or_comment())
            .map(|(_, name, _)| name.value)
    }
    pub fn program_end<S: TextSource>() -> impl Parser<S, Token = ()> {
        (
            spaced(keyword("end")),
            (spaced(keyword("program")), spaced(identifier()).optional()).optional(),
            eol_or_comment(),
        )
            .map(|_| ())
    }
    pub fn program_definition<S: TextSource>() -> impl Parser<
        S,
        Token = Program<S::Span>,
    > {
        program_declaration()
            .then(|name| {
                many_until(item(), program_end(), 0..)
                    .map(move |(items, _)| (name.clone(), items))
            })
            .map(|(name, items)| Program { name, items })
    }
    pub fn unclassified_line<S: TextSource>() -> impl Parser<S, Token = S::Span> {
        many_until(Char::<S::Span>::any(), eol_or_comment(), 0..)
            .map_if(|(chars, _newline)| {
                let mut span = chars.first().map(|c| c.span.clone());
                for c in chars.iter().skip(1) {
                    span = span.map(|s| S::joint_span(s, c.span.clone()));
                }
                span
            })
    }
}
fn main() {
    let src = load_file("in.f90");
    let start_time = std::time::Instant::now();
    let file = parse::items().parse(&src[..]);
    {
        ::std::io::_print(format_args!("time: {0:?}\n", start_time.elapsed()));
    };
    let out = std::fs::File::create("out.rs").unwrap();
    let mut out = std::io::BufWriter::new(out);
    let file = file.0.unwrap();
    out.write_fmt(format_args!("{0:#?}", file)).unwrap();
}
fn load_file(path: impl AsRef<Path>) -> String {
    let path = path.as_ref();
    let src = std::fs::read_to_string(path).unwrap().replace("\r", "");
    src
}

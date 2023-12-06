use riddle::{tokenization::{TextSource, TokenParser}, provided::{common::{many, many_until, separated, chained}, text::{spaced, space, ExactMatch, identifier, Char, eof, Identifier, Literal, numeric_literal, StringLiteral}}};

use crate::parse::tokenization::literals::{fortran_literal, string_literal};

use self::tokenization::{LineComment, empty_lines, EmptyLines, line_comment};

use riddle::alt;

pub mod tokenization;

#[derive(Debug, Clone)]
pub enum Item<Span> {
    EmptyLines(EmptyLines),
    LineComment(LineComment<Span>),
    Program(Program<Span>),
    Statement(Statement<Span>),
    UnclassifiedLine(Span),
}

pub fn item<S: TextSource>() -> impl TokenParser<S, Token = Item<S::Span>> {
    alt! {
        empty_lines().map(Item::EmptyLines),
        line_comment().map(Item::LineComment),
        program_definition().map(Item::Program),
        statement().map(Item::Statement),
        unclassified_line().map(Item::UnclassifiedLine),
    }
}

pub fn items<S: TextSource>() -> impl TokenParser<S, Token = File<S::Span>> {
    many(
        item(),
        0..,
    ).map(|items| File { items })
}

#[derive(Debug, Clone)]
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

pub fn statement<S: TextSource>() -> impl TokenParser<S, Token = Statement<S::Span>> {
    alt! {
        use_statement().map(Statement::UseStatement),
        variables_declaration().map(Statement::VariablesDeclaration),
        do_loop(),
        if_(),
        call_statement(),
        print_statement().map(Statement::PrintStatement),
        (expression(), eol_or_comment()).map(|(e, _)| e).map(Statement::Expression),
    }
}

#[derive(Debug, Clone)]
pub enum PrintFormat<Span> {
    Star,
    FormatString(StringLiteral<Span>),
}

pub fn print_format<S: TextSource>() -> impl TokenParser<S, Token = PrintFormat<S::Span>> {
    alt! {
        spaced('*').map(|_| PrintFormat::Star),
        spaced(string_literal()).map(PrintFormat::FormatString),
    }
}

#[derive(Debug, Clone)]
pub struct PrintStatement<Span> {
    pub format: PrintFormat<Span>,
    pub items: Vec<Expression<Span>>,
}

pub fn print_statement<S: TextSource>() -> impl TokenParser<S, Token = PrintStatement<S::Span>> {
    (
        spaced(keyword("print")),
        print_format(),
        spaced(','),
        separated(expression(), spaced(','), 0..),
        eol_or_comment(),
    ).map(|(_, format, _, items, _)| PrintStatement { format, items })
}

pub fn do_loop<S: TextSource>() -> impl TokenParser<S, Token = Statement<S::Span>> {
    (
        (
            spaced(keyword("do")),
            spaced(identifier()),
            spaced('='),
            spaced(expression()),
            spaced(','),
            spaced(expression()),
            (
                spaced(','),
                spaced(expression()),
            ).optional().map(|opt| opt.map(|(_, step)| step)),
            eol_or_comment(),
        ).map(|(_, var, _, from, _, to, step, _)| (var, from, to, step)),
        many_until(
            item(),
            (spaced(keyword("end")), spaced(keyword("do").optional()), eol_or_comment()).map(|_| ()),
            0..,
        ).map(|(body, _)| body),
    ).map(|((var, from, to, step), body)| Statement::DoLoop {
        variable: var.value,
        start: from,
        end: to,
        step,
        body,
    })
}

pub fn if_<S: TextSource>() -> impl TokenParser<S, Token = Statement<S::Span>> {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    enum BodyTermination {
        EndIf,
        ElseIf,
    }

    fn if_termination<S: TextSource>() -> impl TokenParser<S, Token = BodyTermination> {
        (
            spaced(keyword("end")),
            spaced(keyword("if")).optional(),
            eol_or_comment(),
        ).map(|_| BodyTermination::EndIf)
    }

    fn ok_body_termination<S: TextSource>() -> impl TokenParser<S, Token = BodyTermination> {
        if_termination()
            .or((
                spaced(keyword("else")),
                spaced(keyword("if").optional()),
                eol_or_comment(),
            ).map(|_| BodyTermination::ElseIf))
    }

    (
        (
            spaced(keyword("if")),
            spaced(expression()),
            spaced(keyword("then")),
            eol_or_comment(),
        ).map(|(_, cond, _, _)| cond),
        many_until(
            item(),
            ok_body_termination(),
            0..,
        ).then(|(body, termination)| {
            //(spaced(keyword("else")), eol_or_comment),
            many_until(
                item(),
                if_termination(),
                0..,
            )
                .map(|(else_body, _)| else_body)
                .where_(termination == Some(BodyTermination::ElseIf))
                .map(move |else_body| (body.clone(), else_body))
        })
    ).map(|(condition, (body, else_body))| Statement::If {
        condition,
        body,
        else_body,
    })
}

pub fn call_statement<S: TextSource>() -> impl TokenParser<S, Token = Statement<S::Span>> {
    (
        spaced(keyword("call")),
        spaced(expression()),
        eol_or_comment(),
    ).map(|(_, expr, _)| Statement::CallStatement(expr))
}

const OPERATORS: [(&str, usize); 23] = [
    // TODO check_priorities
    ("**", 60),
    ("*", 50),
    ("/", 50),
    ("+", 40),
    ("-", 40),
    ("//", 30),
    (".eq.", 20), ("==", 20),
    (".ne.", 20), ("!=", 20),
    (".lt.", 20), ("<", 20),
    (".le.", 20), ("<=", 20),
    (".gt.", 20), (">", 20),
    (".ge.", 20), (">=", 20),
    (".and.", 10),
    (".or.", 10),
    (".not.", 10),
    (".eqv.", 10),
    (".neqv.", 10),
];

pub fn operator<S: TextSource>() -> impl TokenParser<S, Token = ExactMatch<S::Span>> {
    //pub fn op<S: TextSource>(op: &'static str) -> impl TokenParser<S, Token = ExactMatch<S::Span>> {
    //    ExactMatch::exact(op, true)
    //}

    riddle::provided::common::alt(|| {
        let mut operators = OPERATORS.clone().map(|(op, _)| op);
        // longest first
        operators.sort_by(|a, b| b.len().cmp(&a.len()));
        operators
    })
}

#[derive(Debug, Clone)]
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

fn call_or_indexing<S: TextSource>() -> impl TokenParser<S, Token = Vec<Expression<S::Span>>> {
    (
        spaced('('),
        separated(
            spaced(expression()),
            ',',
            0..,
        ),
        spaced(')'),
    ).map(|(_, args, _)| args)
}

pub fn expression_monome<S: TextSource>() -> impl TokenParser<S, Token = Expression<S::Span>> {
    alt! {
        (spaced(operator()), expression_monome()).map(|(op, expr)| Expression::UnaryLeftOperation { operator: op, right: Box::new(expr) }),
        fortran_literal().map(Expression::Literal),
        identifier().map(Expression::Identifier),
        (
            spaced('('),
            spaced(expression()),
            spaced(')'),
        ).map(|(_, expr, _)| Expression::Parenthesis(Box::new(expr))),
    }
    .then(|expr| {
        many(
            call_or_indexing(),
            0..,
        ).map(move |calls| {
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

pub fn operation<S: TextSource>() -> impl TokenParser<S, Token = Expression<S::Span>> {
    chained(spaced(expression_monome()), spaced(operator()))
        .map(|
            (
                mut first,
                mut tail,
            ): (
                Expression<S::Span>,
                Vec<(ExactMatch<S::Span>, Expression<S::Span>)>
            )
        | {
            let operator_priority = |op: &ExactMatch<S::Span>| -> usize {
                OPERATORS
                    .iter()
                    .cloned()
                    .find(|(o, _)| *o == op.value)
                    .map(|(_, p)| p)
                    .unwrap()
            };

            fn pick_ith<'a, Span>(first: &'a mut Expression<Span>, tail: &'a mut Vec<(ExactMatch<Span>, Expression<Span>)>, i: usize) -> (&'a mut Expression<Span>, ExactMatch<Span>, Expression<Span>) {
                let (
                    op,
                    expr
                ) = tail.remove(i);
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

                let (
                    l_expr,
                    op,
                    r_expr
                ) = pick_ith(&mut first, &mut tail, max_priority_i);

                // https://stackoverflow.com/questions/67461269/replace-a-value-behind-a-mutable-reference-by-moving-and-mapping-the-original
                // XXX unsound, don't use
                //pub fn map_in_place<T>(place: &mut T, f: impl FnOnce(T) -> T) {
                //    let place = place as *mut T;
                //    unsafe {
                //        let val = std::ptr::read(place);
                //        let new_val = f(val);
                //        std::ptr::write(place, new_val);
                //    }
                //}

                *l_expr = Expression::Operation {
                    operator: op,
                    left: Box::new(l_expr.clone()),
                    right: Box::new(r_expr),
                };

                //map_in_place(l_expr, |l_expr| Expression::Operation {
                //    operator: op,
                //    left: Box::new(l_expr),
                //    right: Box::new(r_expr),
                //});
            }

            first
        })
}

pub fn expression_non_range<S: TextSource>() -> impl TokenParser<S, Token = Expression<S::Span>> {
    alt! {
        operation(),
        expression_monome(),
    }
}

pub fn expression<S: TextSource>() -> impl TokenParser<S, Token = Expression<S::Span>> {
    alt! {
        index_range().map(|r| Expression::IndexRange(Box::new(r))),
        expression_non_range(),
    }
}

#[derive(Debug, Clone)]
pub struct File<Span> {
    pub items: Vec<Item<Span>>,
}

/// End of line
pub fn eol<S: TextSource>() -> impl TokenParser<S, Token = ()> {
    ('\n', Char::exact('\r').optional()).map(|_| ()).or(eof())
}

pub fn eol_or_comment<S: TextSource>() -> impl TokenParser<S, Token = Option<LineComment<S::Span>>> {
    alt! {
        eol().map(|_| None),
        line_comment().map(|c| Some(c)),
    }
}

pub fn keyword<S: TextSource>(keyword: &'static str) -> impl TokenParser<S, Token = S::Span> {
    ExactMatch::exact(keyword, false).map(|m| m.span)
}

#[derive(Debug, Clone)]
pub struct UseStatement {
    pub module_name: String,
    pub only: Vec<String>,
}

pub fn punc<S: TextSource>(p: char) -> impl TokenParser<S, Token = S::Span> {
    p.map(|p| p.span)
}

pub fn use_statement<S: TextSource>() -> impl TokenParser<S, Token = UseStatement> {
    (
        spaced(keyword("use")),
        spaced(identifier()),
        (
            spaced(punc(',')),
            spaced(keyword("only")),
            spaced(':'),
            separated(spaced(identifier()), punc(','), 0..), // TODO better, until eol
        ).optional(),
        eol_or_comment(),
    ).map(|(_, module, only, _)| {
        let only = only.map(|(_, _, _, ids)| ids).unwrap_or(vec![]);
        let only = only.into_iter().map(|id| id.value).collect();
        UseStatement {
            module_name: module.value,
            only,
        }
    })
}

// https://docs.oracle.com/cd/E19957-01/805-4939/index.html
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
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

pub fn basic_type<S: TextSource>() -> impl TokenParser<S, Token = BasicType> {
    alt! {
        (keyword("integer"), spaced('*'), keyword("2")).map(|_| BasicType::Integer2),
        (keyword("integer"), spaced('*'), keyword("4")).map(|_| BasicType::Integer4),
        (keyword("integer"), spaced('*'), keyword("8")).map(|_| BasicType::Integer8),
        (keyword("integer"), spaced('('), keyword("2"), spaced(')')).map(|_| BasicType::Integer2),
        (keyword("integer"), spaced('('), keyword("4"), spaced(')')).map(|_| BasicType::Integer4),
        (keyword("integer"), spaced('('), keyword("8"), spaced(')')).map(|_| BasicType::Integer8),
        keyword("integer").map(|_| BasicType::Integer),
        (keyword("real"), spaced('*'), keyword("4")).map(|_| BasicType::Real4),
        (keyword("real"), spaced('*'), keyword("8")).map(|_| BasicType::Real8),
        (keyword("real"), spaced('*'), keyword("16")).map(|_| BasicType::Real16),
        (keyword("real"), spaced('('), keyword("4"), spaced(')')).map(|_| BasicType::Real4),
        (keyword("real"), spaced('('), keyword("8"), spaced(')')).map(|_| BasicType::Real8),
        (keyword("real"), spaced('('), keyword("16"), spaced(')')).map(|_| BasicType::Real16),
        keyword("real").map(|_| BasicType::Real),
        (keyword("complex"), spaced('*'), keyword("8")).map(|_| BasicType::Complex8),
        (keyword("complex"), spaced('*'), keyword("16")).map(|_| BasicType::Complex16),
        (keyword("complex"), spaced('*'), keyword("32")).map(|_| BasicType::Complex32),
        (keyword("complex"), spaced('('), keyword("8"), spaced(')')).map(|_| BasicType::Complex8),
        (keyword("complex"), spaced('('), keyword("16"), spaced(')')).map(|_| BasicType::Complex16),
        (keyword("complex"), spaced('('), keyword("32"), spaced(')')).map(|_| BasicType::Complex32),
        keyword("complex").map(|_| BasicType::Complex),
        (keyword("double"), space(1..), keyword("complex")).map(|_| BasicType::DoubleComplex),
        (keyword("logical"), spaced('*'), keyword("1")).map(|_| BasicType::Logical1),
        (keyword("logical"), spaced('*'), keyword("2")).map(|_| BasicType::Logical2),
        (keyword("logical"), spaced('*'), keyword("4")).map(|_| BasicType::Logical4),
        (keyword("logical"), spaced('*'), keyword("8")).map(|_| BasicType::Logical8),
        (keyword("logical"), spaced('('), keyword("1"), spaced(')')).map(|_| BasicType::Logical1),
        (keyword("logical"), spaced('('), keyword("2"), spaced(')')).map(|_| BasicType::Logical2),
        (keyword("logical"), spaced('('), keyword("4"), spaced(')')).map(|_| BasicType::Logical4),
        (keyword("logical"), spaced('('), keyword("8"), spaced(')')).map(|_| BasicType::Logical8),
        keyword("logical").map(|_| BasicType::Logical),
        (keyword("character"), spaced('*'), integer()).map(|(_, _, n)| BasicType::CharacterN(n as usize)),
        (keyword("character"), spaced('('), integer(), spaced(')')).map(|(_, _, n, _)| BasicType::CharacterN(n as usize)),
        keyword("character").map(|_| BasicType::Character),
        (keyword("double"), space(1..), keyword("precision")).map(|_| BasicType::DoublePrecision),
    }
}

fn integer<S: TextSource>() -> impl TokenParser<S, Token = i128> {
    numeric_literal().map(|lit| lit.value.parse::<i128>().unwrap())
}

#[derive(Debug, Clone)]
pub enum Type<Span> {
    /// ex: `integer`
    Basic(BasicType),

    /// Ex: `integer(c_int)`
    BasicAlias(BasicType, Identifier<Span>),

    Type(Box<Type<Span>>),

    Array {
        ty: Box<Type<Span>>,
        ranges: Vec<Expression<Span>>,
    },
}

pub fn type_<S: TextSource>() -> impl TokenParser<S, Token = Type<S::Span>> {
    alt!(
        (basic_type(), spaced('('), identifier(), spaced(')')).map(|(ty, _, alias, _)| Type::BasicAlias(ty, alias)),
        basic_type().map(Type::Basic),
        (keyword("type"), spaced('('), type_(), spaced(')')).map(|(_, _, ty, _)| Type::Type(Box::new(ty))),
    )
}

#[derive(Debug, Clone)]
pub struct IndexRange<Span> {
    pub start: Option<Expression<Span>>,
    pub end: Option<Expression<Span>>,
}

pub fn index_range<S: TextSource>() -> impl TokenParser<S, Token = IndexRange<S::Span>> {
    (
        expression_non_range().optional(),
        spaced(':'),
        expression_non_range().optional(),
    ).map(|(start, _, end)| IndexRange {
        start,
        end,
    })
}

#[derive(Debug, Clone)]
pub struct VariablesDeclaration<Span> {
    pub vars: Vec<(Type<Span>, String)>,
}

pub fn variable_declaree<S: TextSource>(base_type: Type<S::Span>) -> impl TokenParser<S, Token = (Type<S::Span>, String)> {
    // TODO arrays
    (
        spaced(identifier()),
        (
            spaced('('),
            separated(expression(), spaced(','), 0..),
            spaced(')'),
        ).map(|(_, ranges, _)| ranges).optional(),
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

pub fn variables_declaration<S: TextSource>() -> impl TokenParser<S, Token = VariablesDeclaration<S::Span>> {
    let d = spaced(type_())
        .then(move |ty| {
            variable_declaree(ty.clone()).map(|var| vec![var])
            .or(
                (
                    spaced(ExactMatch::exact("::", false)),
                    separated(
                        variable_declaree(ty.clone()), // TODO not identifier
                        spaced(ExactMatch::exact(",", false)),
                        1..,
                    ),
                ).map(move |(_, vars)| vars)
            )
        });
    (d, eol_or_comment()).map(|(vars, _)| VariablesDeclaration { vars })
}

#[derive(Debug, Clone)]
pub struct Program<Span> {
    pub name: String,
    pub items: Vec<Item<Span>>,
}

pub fn program_declaration<S: TextSource>() -> impl TokenParser<S, Token = String> {
    (
        spaced(keyword("program")),
        spaced(identifier()),
        eol_or_comment(),
    ).map(|(_, name, _)| name.value)
}

pub fn program_end<S: TextSource>() -> impl TokenParser<S, Token = ()> {
    (
        spaced(keyword("end")),
        (
            spaced(keyword("program")),
            spaced(identifier()).optional(),
        ).optional(),
        eol_or_comment(),
    ).map(|_| ())
}

pub fn program_definition<S: TextSource>() -> impl TokenParser<S, Token = Program<S::Span>> {
    program_declaration()
        .then(|name|
            many_until(
                item(),
                program_end(),
                0..,
            ).map(move |(items, _)| (name.clone(), items))
        )
        .map(|(name, items)| Program { name, items })
}

pub fn unclassified_line<S: TextSource>() -> impl TokenParser<S, Token = S::Span> {
    many_until(
        Char::<S::Span>::any(),
        eol_or_comment(),
        0..,
    ).map_if(|(chars, _newline)| {
        let mut span = chars.first().map(|c| c.span.clone());
        for c in chars.iter().skip(1) {
            span = span.map(|s| S::joint_span(s, c.span.clone()));
        }
        span
    })
}
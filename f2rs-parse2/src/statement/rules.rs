use f2rs_parser_combinator::prelude::*;
use enum_as_inner::EnumAsInner;

macro_rules! kw {
    ($($k:ident)*) => {
        keyword(&[$(stringify!($k)),*])
    };
}
pub(crate) use kw;

mod attributes; pub use attributes::*;
mod types; pub use types::*;
mod expression; pub use expression::*;
mod literals; pub use literals::*;
mod constants; pub use constants::*;
mod procedures; pub use procedures::*;
mod execution_control; pub use execution_control::*;
mod concepts; pub use concepts::*;
mod input_output_editing; pub use input_output_editing::*;
mod statements; pub use statements::*;
use crate::{s_rule, tokens::rules::{AddOp, AndOp, Arrow, CharLiteralConstant, ConcatOp, DefinedOperator, DefinedUnaryOrBinaryOp, Dot, DotDot, Equals, EquivOp, IntLiteralConstant, IntrinsicOperator, Label, LexicalToken, MultOp, NonComplexLiteralConstant, NotOp, OrOp, Percent, PowerOp, RelOp, SpecialCharacter, SpecialCharacterMatch}};
use std::ops::RangeBounds;

use crate::tokens::rules::{Colon, Comma, DoubleColon, Name};

use super::*;

type LexTk = LexicalToken<MultilineSpan>;
pub trait Lexed: Source<Element = LexTk> {}
impl<S> Lexed for S where S: Source<Element = LexTk> {}

// TODO test
pub fn list<'a, S: Source<Element = LexicalToken<MultilineSpan>> + 'a, P: Parser<S, Token = T> + 'a, T: 'a>(
    element: P,
    range: impl RangeBounds<usize> + Clone + 'a,
) -> impl Parser<S, Token = Vec<P::Token>> {
    separated(
        element,
        comma(),
        range
    )
}

#[derive(Debug, Clone)]
pub struct Keyword<Span> {
    span: Span,
    matches: Vec<Name<Span>>,
}

impl<Span> Spanned<Span> for Keyword<Span> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl<Span> MapSpan<Span> for Keyword<Span> {
    type Spanned<T> = Keyword<T>;
    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        Keyword {
            span: f(self.span),
            matches: self.matches.into_iter().map(|m| m.map_span(f)).collect(),
        }
    }
}

pub fn non_complex_literal_constant<S: Source<Element = LexicalToken<MultilineSpan>>>() -> impl Parser<S, Token = NonComplexLiteralConstant<MultilineSpan>> {
    token().map_if(|t| match t {
        LexicalToken::LiteralConstant(lc) => Some(lc),
        _ => None,
    })
}

pub fn int_literal_constant<S: Source<Element = LexicalToken<MultilineSpan>>>() -> impl Parser<S, Token = IntLiteralConstant<MultilineSpan>> {
    token().map_if(|t| match t {
        LexicalToken::LiteralConstant(NonComplexLiteralConstant::Int(lc)) => Some(lc),
        _ => None,
    })
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Sign<Span> { // TODO span
    Plus(Span),
    Minus(Span),
}

impl<Span> Sign<Span> {
    pub fn span(&self) -> &Span {
        match self {
            Self::Plus(span) => span,
            Self::Minus(span) => span,
        }
    }
}

impl<Span> MapSpan<Span> for Sign<Span> {
    type Spanned<T> = Sign<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        match self {
            Sign::Plus(span) => Sign::Plus(f(span)),
            Sign::Minus(span) => Sign::Minus(f(span)),
        }
    }
}

#[doc = s_rule!(
    F18V007r1 rule "sign" #712 :
    "is +"
    "or -",
)]
pub fn sign<S: Lexed>(source: S) -> PResult<Sign<MultilineSpan>, S> {
    alt! {
        for S =>
        op("+").map(|op| Sign::Plus(op.span().clone())),
        op("-").map(|op| Sign::Minus(op.span().clone())),
    }.parse(source)
}

#[derive(Debug, Clone)]
pub struct SignedIntLiteralConstant<Span> {
    // TODO span
    pub sign: Option<Sign<Span>>,
    pub int_literal_constant: IntLiteralConstant<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "signed-int-literal-constant" #707 : "is [ sign ] int-literal-constant",
)]
pub fn signed_int_literal_constant<S: Lexed>(source: S) -> PResult<SignedIntLiteralConstant<MultilineSpan>, S> {
    (
        sign.optional(),
        int_literal_constant(),
    )
        .map(|(sign, int_literal_constant)| SignedIntLiteralConstant {
            sign,
            int_literal_constant,
        })
        .parse(source)
}

pub fn char_literal_constant<S: Source<Element = LexicalToken<MultilineSpan>>>() -> impl Parser<S, Token = CharLiteralConstant<MultilineSpan>> {
    token().map_if(|t| match t {
        LexicalToken::LiteralConstant(NonComplexLiteralConstant::Char(lc)) => Some(lc),
        _ => None,
    })
}

pub fn dot<S: Source<Element = LexicalToken<MultilineSpan>>>() -> impl Parser<S, Token = Dot<MultilineSpan>> {
    token().map_if(|t| match t {
        LexicalToken::Dot(t) => Some(t),
        _ => None,
    })
}

pub fn name_as_keyword<S: Source<Element = LexicalToken<MultilineSpan>>>() -> impl Parser<S, Token = Keyword<MultilineSpan>> {
    name().map(|n| Keyword { span: n.span().clone(), matches: vec![n] })
}

pub fn keyword<S: Source<Element = LexicalToken<MultilineSpan>>>(parts: &'static [&'static str]) -> impl Parser<S, Token = Keyword<MultilineSpan>> {
    // examples:
    // ["end", "do"] should match "end do" and "enddo"

    move |mut source: S| {
        assert!(parts.len() > 0);

        if source.empty() {
            return None;
        }

        let mut matches = Vec::new();
        let mut idx = source.start();
        let mut i = 0;
        'a: while i < parts.len() {
            let token = source.get_at(&idx)?;
            let LexicalToken::Name(name) = token else { return None; };
            let name_str = name.0.value().to_lowercase();

            // now we try to match the name with one or more parts
            for j in (i + 1)..=parts.len() {
                let target = parts[i..j].iter().map(|s| s.to_lowercase()).collect::<String>();
                if name_str == target {
                    matches.push(name);
                    i = j;
                    idx = source.next(idx, 1);
                    source = source.tail(idx);
                    idx = source.start();
                    continue 'a;
                }
            }

            return None;
        }

        let mut span = matches[0].span().clone();
        for name in &matches[1..] {
            span = MultilineSpan::merge(span, name.span().clone());
        }

        Some((Keyword { span, matches }, source))
    }
}

pub fn token<S: Source<Element = LexicalToken<MultilineSpan>>>() -> impl Parser<S, Token = LexicalToken<MultilineSpan>> {
    move |source: S| {
        if source.empty() {
            return None;
        }

        let token = source.get_at(&source.start())?;
        let idx = source.next(source.start(), 1);
        Some((token, source.tail(idx)))
    }
}

pub fn name<S: Source<Element = LexicalToken<MultilineSpan>>>() -> impl Parser<S, Token = Name<MultilineSpan>> {
    token().map_if(|t| match t {
        LexicalToken::Name(t) => Some(t),
        _ => None,
    })
}

pub fn comma<S: Source<Element = LexicalToken<MultilineSpan>>>() -> impl Parser<S, Token = Comma<MultilineSpan>> {
    token().map_if(|t| match t {
        LexicalToken::Comma(t) => Some(t),
        _ => None,
    })
}

pub fn colon<S: Source<Element = LexicalToken<MultilineSpan>>>() -> impl Parser<S, Token = Colon<MultilineSpan>> {
    token().map_if(|t| match t {
        LexicalToken::Colon(t) => Some(t),
        _ => None,
    })
}

pub fn double_colon<S: Source<Element = LexicalToken<MultilineSpan>>>() -> impl Parser<S, Token = DoubleColon<MultilineSpan>> {
    token().map_if(|t| match t {
        LexicalToken::DoubleColon(t) => Some(t),
        _ => None,
    })
}

pub fn dot_dot<S: Source<Element = LexicalToken<MultilineSpan>>>() -> impl Parser<S, Token = DotDot<MultilineSpan>> {
    token().map_if(|t| match t {
        LexicalToken::DotDot(t) => Some(t),
        _ => None,
    })
}

pub fn delim<S: Lexed>(delim: impl Into<SpecialCharacter>) -> impl Parser<S, Token = SpecialCharacterMatch<MultilineSpan>> {
    let delim = delim.into();
    token().map_if(move |t| match t {
        LexicalToken::Delimiter(t) if t.character == delim => Some(t),
        _ => None,
    })
}

pub fn equals<S: Lexed>() -> impl Parser<S, Token = Equals<MultilineSpan>> {
    token().map_if(|t| match t {
        LexicalToken::Equals(t) => Some(t),
        _ => None,
    })
}

pub fn arrow<S: Lexed>() -> impl Parser<S, Token = Arrow<MultilineSpan>> {
    token().map_if(|t| match t {
        LexicalToken::Arrow(t) => Some(t),
        _ => None,
    })
}

pub fn defined_operator<S: Lexed>() -> impl Parser<S, Token = DefinedOperator<MultilineSpan>> {
    token().map_if(|t| match t {
        LexicalToken::Operator(op) => Some(op),
        _ => None,
    })
}

pub fn op<S: Lexed>(ty: &'static str) -> impl Parser<S, Token = DefinedOperator<MultilineSpan>> {
    token().map_if(move |t| match t {
        LexicalToken::Operator(op) => if op.to_string().to_lowercase() == ty.to_lowercase() { Some(op) } else { None },
        _ => None,
    })
}

pub fn defined_unary_or_binary_op<S: Lexed>() -> impl Parser<S, Token = DefinedUnaryOrBinaryOp<MultilineSpan>> {
    token().map_if(|t| match t {
        LexicalToken::Operator(DefinedOperator::DefinedUnaryOrBinary(op)) => Some(op),
        _ => None,
    })
}

pub fn power_op<S: Lexed>() -> impl Parser<S, Token = PowerOp<MultilineSpan>> {
    token().map_if(|t| match t {
        LexicalToken::Operator(DefinedOperator::IntrinsicEx(IntrinsicOperator::PowerOp(op))) => Some(op),
        _ => None,
    })
}

pub fn mult_op<S: Lexed>() -> impl Parser<S, Token = MultOp<MultilineSpan>> {
    token().map_if(|t| match t {
        LexicalToken::Operator(DefinedOperator::IntrinsicEx(IntrinsicOperator::MultOp(op))) => Some(op),
        _ => None,
    })
}

pub fn asterisk<S: Lexed>() -> impl Parser<S, Token = StringMatch<MultilineSpan>> { // TODO return SpecialCharacterMatch
    token().map_if(|t| match t {
        LexicalToken::Operator(DefinedOperator::IntrinsicEx(IntrinsicOperator::MultOp(op))) => {
            if op.0.value() == "*" {
                Some(op.0)
            } else {
                None
            }
        },
        _ => None,
    })
}

pub fn add_op<S: Lexed>() -> impl Parser<S, Token = AddOp<MultilineSpan>> {
    token().map_if(|t| match t {
        LexicalToken::Operator(DefinedOperator::IntrinsicEx(IntrinsicOperator::AddOp(op))) => Some(op),
        _ => None,
    })
}

pub fn concat_op<S: Lexed>() -> impl Parser<S, Token = ConcatOp<MultilineSpan>> {
    token().map_if(|t| match t {
        LexicalToken::Operator(DefinedOperator::IntrinsicEx(IntrinsicOperator::ConcatOp(op))) => Some(op),
        _ => None,
    })
}

pub fn rel_op<S: Lexed>() -> impl Parser<S, Token = RelOp<MultilineSpan>> {
    token().map_if(|t| match t {
        LexicalToken::Operator(DefinedOperator::IntrinsicEx(IntrinsicOperator::RelOp(op))) => Some(op),
        _ => None,
    })
}

pub fn not_op<S: Lexed>() -> impl Parser<S, Token = NotOp<MultilineSpan>> {
    token().map_if(|t| match t {
        LexicalToken::Operator(DefinedOperator::IntrinsicEx(IntrinsicOperator::NotOp(op))) => Some(op),
        _ => None,
    })
}

pub fn and_op<S: Lexed>() -> impl Parser<S, Token = AndOp<MultilineSpan>> {
    token().map_if(|t| match t {
        LexicalToken::Operator(DefinedOperator::IntrinsicEx(IntrinsicOperator::AndOp(op))) => Some(op),
        _ => None,
    })
}

pub fn or_op<S: Lexed>() -> impl Parser<S, Token = OrOp<MultilineSpan>> {
    token().map_if(|t| match t {
        LexicalToken::Operator(DefinedOperator::IntrinsicEx(IntrinsicOperator::OrOp(op))) => Some(op),
        _ => None,
    })
}

pub fn equiv_op<S: Lexed>() -> impl Parser<S, Token = EquivOp<MultilineSpan>> {
    token().map_if(|t| match t {
        LexicalToken::Operator(DefinedOperator::IntrinsicEx(IntrinsicOperator::EquivOp(op))) => Some(op),
        _ => None,
    })
}

pub fn percent<S: Lexed>() -> impl Parser<S, Token = Percent<MultilineSpan>> {
    token().map_if(|t| match t {
        LexicalToken::Percent(t) => Some(t),
        _ => None,
    })
}

pub fn label<S: Lexed>() -> impl Parser<S, Token = Label<MultilineSpan>> {
    token().map_if(|t| match t {
        LexicalToken::LiteralConstant(NonComplexLiteralConstant::Int(lc)) => Some(Label(lc)), // TODO checks
        _ => None,
    })
}

pub fn letter<S: Lexed>() -> impl Parser<S, Token = Char<MultilineSpan>> {
    name()
        .condition(|n, _| n.0.value().len() == 1 && n.0.value().chars().all(|c| c.is_alphabetic()))
        .map(|n| Char {
            value: n.0.value().chars().next().unwrap(),
            span: n.0.span().clone(),
        })
}

#[cfg(test)]
fn tokenize(source: &str) -> Vec<LexicalToken<MultilineSpan>> {
    use crate::tokens::rules::{lexical_token, space};

    let chars = source.chars().collect::<Vec<_>>();

    // TODO CFG????
    let (r, _s) = many(
        (space(0), lexical_token, space(0)).map(|(_, t, _)| t),
        0..,
    )
        .map(|t| t.map_span(&|s| MultilineSpan::from_line_span(0, s)))
        .parse(Chars::new(&chars, 0))
        .unwrap();

    r
}
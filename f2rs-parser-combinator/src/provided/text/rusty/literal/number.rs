use std::str::FromStr;

use crate::{tokenization::{TextSource, Parser}, provided::{text::{Char, identifier, Identifier}, common::fold_many}};


#[derive(Debug, Clone)]
pub struct NumericLiteral<Span> {
    pub span: Span,
    pub value: String,
    pub suffix: Option<Identifier<Span>>,
}

impl<Span> NumericLiteral<Span> {
    pub fn parse<N: FromStr>(&self) -> Option<N> {
        self.value.parse().ok()
    }
}

pub fn numeric_literal<S: TextSource>() -> impl Parser<S, Token = NumericLiteral<S::Span>> {
    //let digit = || Char::digit::<S>().or('.').or('_');
    //.or(Char::exact('\''))
    fn digits<S: TextSource>(min: usize) -> impl Parser<S, Token = (S::Span, String)> {
        let digit = || Char::digit::<S>().or('_');
        fold_many(
            digit(),
            || (None, String::new()),
            |(span, mut s), d| {
                let span = span.map_or(d.span.clone(), |span| S::merge_span(span, d.span));
                s.push(d.value);
                ((Some(span), s), true)
            },
            min..,
        ).map(|(span, s)| (span.unwrap_or(S::null_span()), s))
    }

    fn exponent<S: TextSource>() -> impl Parser<S, Token = (S::Span, String)> {
        (
            Char::exact('e').or('E').or('d').or('D'),
            Char::exact('+').or('-').optional(),
            digits::<S>(1),
        ).map(|(e, s, d)| -> (S::Span, String) {
            let mut span = e.span;
            let mut value = e.value.to_string();
            if let Some(s) = s {
                span = S::merge_span(span, s.span);
                value.push(s.value);
            }
            span = S::merge_span(span, d.0);
            value.push_str(&d.1);
            (span, value)
        })
        //.map(|_| ())
    }

    fn mantissa<S: TextSource>() -> impl Parser<S, Token = (S::Span, String)> {
        (
            Char::exact('.'),
            digits(1),
        ).map(|(dot, d)| {
            let span = S::merge_span(dot.span, d.0);
            (span, d.1)
        })
    }

    (
        digits::<S>(0),
        mantissa().optional(),
        (Char::exact('.').optional(), exponent()).optional(),
        identifier::<S>().optional(),
    )
        .condition(|(
            coeff,
            mantissa,
            _,
            _
        ), _| !coeff.1.is_empty() || mantissa.is_some())
        .map(|(
            coeff,
            mantissa,
            exp,
            suffix
        )| {
            let mut span = coeff.0;
            let mut value = coeff.1;
            if let Some((mantissa_span, mantissa)) = mantissa {
                span = S::merge_span(span, mantissa_span);
                value.push('.');
                value.push_str(&mantissa);
            }
            if let Some((dot, (exp_span, exp_value))) = exp {
                if let Some(dot) = dot {
                    span = S::merge_span(span, dot.span);
                    value.push(dot.value);
                }
                span = S::merge_span(span, exp_span);
                value.push_str(&exp_value);
            }
            if let Some(suffix) = &suffix {
                span = S::merge_span(span, suffix.span.clone());
            }
            NumericLiteral {
                span,
                value,
                suffix,
            }
        })
}
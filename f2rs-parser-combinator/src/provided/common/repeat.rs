use crate::tokenization::{Parser, Source, PResult, ParserCore};

#[derive(Debug, Clone, Copy)]
pub struct Named<P> {
    _name: &'static str,
    p: P,
}

impl<P> Named<P> {
    pub fn new(name: &'static str, p: P) -> Self {
        Self { _name: name, p }
    }
}

impl<S: Source, P> ParserCore<S> for Named<P>
where
    Self: Clone,
    P: ParserCore<S>,
{
    type Token = P::Token;
    fn parse(&self, source: S) -> PResult<Self::Token, S> {
        #[cfg(debug_assertions)]
        log::trace!("<{}>", self._name);
        let r = self.p.parse(source);
        #[cfg(debug_assertions)]
        log::trace!("</{}>", self._name);
        r
    }
}

#[derive(Debug, Clone, Copy)]
pub struct If_<P> {
    condition: bool,
    p: P,
}

impl<P> If_<P> {
    pub fn new(condition: bool, p: P) -> Self {
        Self { condition, p }
    }
}

impl<S: Source, P> ParserCore<S> for If_<P>
where
    Self: Clone,
    P: Parser<S>,
{
    type Token = P::Token;
    fn parse(&self, source: S) -> PResult<Self::Token, S> {
        let If_ { condition, p } = self;
        if *condition {
            p.parse(source)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Then<P1, FP2> {
    p1: P1,
    fp2: FP2,
}

impl<P1, FP2> Then<P1, FP2> {
    pub fn new(p1: P1, fp2: FP2) -> Self {
        Self { p1, fp2 }
    }
}

impl<S: Source, P1, P2, FP2> ParserCore<S> for Then<P1, FP2>
where
    Self: Clone,
    P1: Parser<S>,
    P2: Parser<S>,
    FP2: Fn(P1::Token) -> P2,
{
    type Token = P2::Token;
    fn parse(&self, source: S) -> PResult<Self::Token, S> {
        let Then { p1, fp2 } = self;
        let (token, new_source) = p1.parse(source)?;
        let p2 = fp2(token);
        p2.parse(new_source)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Where_<P1> {
    condition: bool,
    p1: P1,
}

impl<P1> Where_<P1> {
    pub fn new(condition: bool, p1: P1) -> Self {
        Self { condition, p1 }
    }
}

impl<S: Source, P1> ParserCore<S> for Where_<P1>
where
    Self: Clone,
    P1: Parser<S>,
{
    type Token = Option<P1::Token>;
    fn parse(&self, source: S) -> PResult<Self::Token, S> {
        let Where_ { condition, p1 } = self;
        if *condition {
            p1.clone().map(|r| Some(r)).parse(source)
        } else {
            let s = source.start();
            source.parsed_result(s, |_| None)
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum OrResult<A, B> {
    First(A),
    Second(B),
}

impl<A, B> OrResult<A, B> {
    fn new_first(a: A) -> Self {
        Self::First(a)
    }

    fn new_second(b: B) -> Self {
        Self::Second(b)
    }

    pub fn as_first(&self) -> Option<&A> {
        match self {
            OrResult::First(a) => Some(a),
            OrResult::Second(_) => None,
        }
    }

    pub fn as_second(&self) -> Option<&B> {
        match self {
            OrResult::First(_) => None,
            OrResult::Second(b) => Some(b),
        }
    }

    pub fn into_first(self) -> Option<A> {
        match self {
            OrResult::First(a) => Some(a),
            OrResult::Second(_) => None,
        }
    }

    pub fn into_second(self) -> Option<B> {
        match self {
            OrResult::First(_) => None,
            OrResult::Second(b) => Some(b),
        }
    }

    pub fn is_first(&self) -> bool {
        match self {
            OrResult::First(_) => true,
            OrResult::Second(_) => false,
        }
    }

    pub fn is_second(&self) -> bool {
        match self {
            OrResult::First(_) => false,
            OrResult::Second(_) => true,
        }
    }

    pub fn map_first<C>(self, f: impl FnOnce(A) -> C) -> OrResult<C, B> {
        match self {
            OrResult::First(a) => OrResult::First(f(a)),
            OrResult::Second(b) => OrResult::Second(b),
        }
    }

    pub fn map_second<C>(self, f: impl FnOnce(B) -> C) -> OrResult<A, C> {
        match self {
            OrResult::First(a) => OrResult::First(a),
            OrResult::Second(b) => OrResult::Second(f(b)),
        }
    }
}

impl<A> OrResult<A, A> {
    pub fn inner(self) -> A {
        match self {
            OrResult::First(a) => a,
            OrResult::Second(a) => a,
        }
    }

    pub fn as_inner(&self) -> &A {
        match self {
            OrResult::First(a) => a,
            OrResult::Second(a) => a,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Or<P1, P2> {
    p1: P1,
    p2: P2,
}

impl<P1, P2> Or<P1, P2> {
    pub fn new(p1: P1, p2: P2) -> Self {
        Self { p1, p2 }
    }
}

impl<S: Source, P1, P2> ParserCore<S> for Or<P1, P2>
where
    Self: Clone,
    P1: Parser<S>,
    P2: Parser<S>,
{
    type Token = OrResult<P1::Token, P2::Token>;
    fn parse(&self, source: S) -> PResult<Self::Token, S> {
        use OrResult::*;

        let Or { p1, p2 } = self;
        let r1 = p1.parse(source.clone());
        match r1 {
            Some((token, new_source)) => Some((First(token), new_source)),
            None => {
                p2
                    .parse(source)
                    .map(|(token, new_source)| (Second(token), new_source))
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Optional<P1> {
    p1: P1,
}

impl<P1> Optional<P1> {
    pub fn new(p1: P1) -> Self {
        Self { p1 }
    }
}

impl<S: Source, P1> ParserCore<S> for Optional<P1>
where
    P1: Parser<S>,
{
    type Token = Option<P1::Token>;
    fn parse(&self, source: S) -> PResult<Self::Token, S> {
        let Optional { p1 } = self;
        Some(
            p1
                .parse(source.clone())
                .map_or((None, source), |(token, new_source)| (Some(token), new_source))
        )
    }
}

#[derive(Debug, Clone, Copy)]
pub struct DoNotConsume<P1> {
    p1: P1,
}

impl<P1> DoNotConsume<P1> {
    pub fn new(p1: P1) -> Self {
        Self { p1 }
    }
}

impl<S: Source, P1> ParserCore<S> for DoNotConsume<P1>
where
    P1: Parser<S>,
{
    type Token = P1::Token;
    fn parse(&self, source: S) -> PResult<Self::Token, S> {
        let DoNotConsume { p1 } = self;
        let (token, _consumed) = p1.parse(source.clone())?;
        Some((token, source))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Condition<P, F> {
    p: P,
    f: F,
}

impl<P, F> Condition<P, F> {
    pub fn new(p: P, f: F) -> Self {
        Self { p, f }
    }
}

impl<S: Source, P, F> ParserCore<S> for Condition<P, F>
where
    Self: Clone,
    P: Parser<S>,
    F: Fn(&P::Token, &S) -> bool,
{
    type Token = P::Token;
    fn parse(&self, source: S) -> PResult<Self::Token, S> {
        let Condition { p, f } = self;
        let r = p.parse(source.clone());
        if let Some((token, new_source)) = r {
            if f(&token, &new_source) {
                return Some((token, new_source));
            }
        }
        None
    }
}

pub fn fold_many_until<S: Source, P: Parser<S>, U: Parser<S>, R>(
    parser: P,
    until: U,
    init: impl Fn() -> R + Clone,
    fold: impl Fn(R, P::Token) -> (R, bool) + Clone,
    range: impl std::ops::RangeBounds<usize> + Clone,
) -> impl Parser<S, Token = (R, Option<U::Token>)> {
    move |source: S| {
        let mut result = init();
        let mut source_tail = source.clone();
        let mut count = 0;
        let mut until_result = None;
        loop {
            if let Some((token, new_source)) = until.parse(source_tail.clone()) {
                source_tail = new_source;
                until_result = Some(token);
                break;
            }
            if let Some((token, new_source)) = parser.parse(source_tail.clone()) {
                source_tail = new_source;
                let r = fold(result, token);
                result = r.0;
                count += 1;
                if !r.1 {
                    break;
                }
            } else {
                break;
            }
        }

        if range.contains(&count) {
            Some(((result, until_result), source_tail))
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Never;

impl<S: Source> ParserCore<S> for Never {
    type Token = ();
    fn parse(&self, _source: S) -> PResult<Self::Token, S> {
        None
    }
}

#[macro_export]
macro_rules! todo_parser {
    () => {
        |source: S| todo!("TODO: parser not implemented yet")
    };
}

pub fn fold_many<S: Source, P: Parser<S>, R>(
    parser: P,
    init: impl Fn() -> R + Clone,
    fold: impl Fn(R, P::Token) -> (R, bool) + Clone,
    range: impl std::ops::RangeBounds<usize> + Clone,
) -> impl Parser<S, Token = R> {
    fold_many_until(
        parser,
        Never,
        init,
        fold,
        range,
    ).map(|(r, _)| r)
}

pub fn many<S: Source, P: Parser<S>>(
    parser: P,
    range: impl std::ops::RangeBounds<usize> + Clone,
) -> impl Parser<S, Token = Vec<P::Token>> {
    fold_many(
        parser,
        || Vec::new(),
        |mut vec, token| {
            vec.push(token);
            (vec, true)
        },
        range,
    )
}

pub fn many_until<S: Source, P: Parser<S>, U: Parser<S>>(
    parser: P,
    until: U,
    range: impl std::ops::RangeBounds<usize> + Clone,
) -> impl Parser<S, Token = (Vec<P::Token>, Option<U::Token>)> {
    fold_many_until(
        parser,
        until,
        || Vec::new(),
        |mut vec, token| {
            vec.push(token);
            (vec, true)
        },
        range,
    )
}

pub fn separated<S: Source, P1: Parser<S>>(
    parser: P1,
    separator: impl Parser<S>,
    range: impl std::ops::RangeBounds<usize> + Clone,
) -> impl Parser<S, Token = Vec<P1::Token>> {
    fold_many(
        (parser, separator.optional()),
        || Vec::new(),
        |mut vec, (token, separator)| {
            vec.push(token);
            (vec, separator.is_some())
        },
        range,
    )
}

pub fn chained<S: Source, P1: Parser<S>, P2: Parser<S>>(
    parser: P1,
    separator: P2,
) -> impl Parser<S, Token = (P1::Token, Vec<(P2::Token, P1::Token)>)> {
    (
        parser.clone(),
        many((separator, parser), 0..),
    )
}

/// **End Of Stream**
pub fn eos<S: Source>() -> impl Parser<S, Token = ()> {
    move |source: S| {
        if source.empty() {
            Some(((), source))
        } else {
            None
        }
    }
}

pub fn not<S: Source, P: Parser<S>>(
    parser: P,
) -> impl Parser<S, Token = ()> {
    move |source: S| {
        if parser.parse(source.clone()).is_none() {
            Some(((), source))
        } else {
            None
        }
    }
}

macro_rules! impl_parser_for_tuple {
    () => {
        impl<S> ParserCore<S> for ()
        where
            S: Source,
        {
            type Token = ();
            fn parse(&self, source: S) -> PResult<Self::Token, S> {
                Some(((), source))
            }
        }
    };
    ($($P:ident),*) => {
        #[allow(non_snake_case)]
        impl<S, $($P),*> ParserCore<S> for ($($P),*,)
        where
            S: Source,
            $($P: Parser<S>),*
        {
            type Token = ($($P::Token),*,);
            fn parse(&self, source: S) -> PResult<Self::Token, S> {
                let ($($P),*,) = self;
                let mut processing_source = source.clone();
                $(
                    let $P = if let Some((token, new_source)) = $P.parse(processing_source){
                        processing_source = new_source;
                        token
                    } else {
                        return None;
                    };
                )*
                Some((($($P),*,), processing_source))
            }
        }
    };
}

impl_parser_for_tuple!();
impl_parser_for_tuple!(P0);
impl_parser_for_tuple!(P0, P1);
impl_parser_for_tuple!(P0, P1, P2);
impl_parser_for_tuple!(P0, P1, P2, P3);
impl_parser_for_tuple!(P0, P1, P2, P3, P4);
impl_parser_for_tuple!(P0, P1, P2, P3, P4, P5);
impl_parser_for_tuple!(P0, P1, P2, P3, P4, P5, P6);
impl_parser_for_tuple!(P0, P1, P2, P3, P4, P5, P6, P7);
impl_parser_for_tuple!(P0, P1, P2, P3, P4, P5, P6, P7, P8);
impl_parser_for_tuple!(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9);
impl_parser_for_tuple!(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10);
impl_parser_for_tuple!(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
impl_parser_for_tuple!(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12);
use crate::tokenization::{Parser, Source, PResult, ParserCore, unpack_presult, parsed};

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
        println!("<{}>", self._name);
        let r = self.p.parse(source);
        println!("</{}>", self._name);
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
            source.unparsed_result()
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
        let (token, new_source) = unpack_presult(p1.parse(source.clone()));
        let token = match token {
            Some(token) => token,
            None => return source.unparsed_result(),
        };
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
            parsed(None, source)
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
    P2: Parser<S, Token = P1::Token>,
{
    type Token = P1::Token;
    fn parse(&self, source: S) -> PResult<Self::Token, S> {
        let Or { p1, p2 } = self;
        let r1 = p1.parse(source.clone());
        if r1.is_ok() {
            return r1;
        } else {
            p2.parse(source)
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct OrError<P1, E> {
    p1: P1,
    e: E,
}

impl<P1, E> OrError<P1, E> {
    pub fn new(p1: P1, e: E) -> Self {
        Self { p1, e }
    }
}

impl<S: Source, P1, E> ParserCore<S> for OrError<P1, E>
where
    Self: Clone,
    P1: Parser<S>,
    E: Clone,
{
    type Token = Result<P1::Token, E>;
    fn parse(&self, source: S) -> PResult<Self::Token, S> {
        let OrError { p1, e } = self;
        let r1 = p1.parse(source.clone());
        let r1 = unpack_presult(r1);
        if let Some(token) = r1.0 {
            parsed(Ok(token), r1.1)
        } else {
            parsed(Err(e.clone()), source)
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
        let (token, new_source) = unpack_presult(p1.parse(source));
        parsed(token, new_source)
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
        let (token, _) = unpack_presult(p1.parse(source.clone()));
        if let Some(token) = token {
            parsed(token, source)
        } else {
            source.unparsed_result()
        }
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
        let (token, new_source) = unpack_presult(p.parse(source.clone()));
        if let Some(token) = token {
            if f(&token, &new_source) {
                return parsed(token, new_source);
            }
        }
        source.unparsed_result()
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
            let (token, new_source) = unpack_presult(until.parse(source_tail));
            source_tail = new_source;
            if let Some(token) = token {
                until_result = Some(token);
                break;
            }
            let (token, new_source) = unpack_presult(parser.parse(source_tail));
            source_tail = new_source;
            if let Some(token) = token {
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
            parsed((result, until_result), source_tail)
        } else {
            source.unparsed_result()
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Never;

impl<S: Source> ParserCore<S> for Never {
    type Token = ();
    fn parse(&self, source: S) -> PResult<Self::Token, S> {
        source.unparsed_result()
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

macro_rules! impl_parser_for_tuple {
    () => {
        impl<S> ParserCore<S> for ()
        where
            S: Source,
        {
            type Token = ();
            fn parse(&self, source: S) -> PResult<Self::Token, S> {
                parsed((), source)
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
                    let (token, new_source) = unpack_presult($P.parse(processing_source));
                    processing_source = new_source;
                    let $P = token;
                    let $P = match $P {
                        Some($P) => $P,
                        None => return source.unparsed_result(),
                    };
                )*
                parsed(($($P),*,), processing_source)
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
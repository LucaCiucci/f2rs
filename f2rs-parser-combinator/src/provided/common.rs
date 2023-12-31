use std::marker::PhantomData;

use crate::tokenization::{ParserCore, Parser, Source, PResult, unpack_presult, unparsed};

mod repeat; pub use repeat::*;

#[derive(Debug, Clone, Copy)]
pub struct Mapped<S, P, F> {
    parser: P,
    map: F,
    _phantom: PhantomData<S>
}

impl<R, S: Source, P: ParserCore<S>, F: Fn(P::Token) -> R> Mapped<S, P, F> {
    pub fn new(parser: P, map: F) -> Self {
        Self {
            parser,
            map,
            _phantom: PhantomData
        }
    }
}

impl<R, S: Source, P: ParserCore<S>, F: Fn(P::Token) -> R> ParserCore<S> for Mapped<S, P, F>
where
    Self: Clone,
{
    type Token = F::Output;
    fn parse(&self, source: S) -> PResult<Self::Token, S> {
        let (token, source) = unpack_presult(self.parser.parse(source));
        //(token.map(|t| (self.map)(t)), source)
        if let Some(token) = token {
            Ok(((self.map)(token), source))
        } else {
            Err(source)
        }
    }
}

pub fn map<R, S: Source, P: ParserCore<S>, F: Fn(P::Token) -> R>(parser: P, map: F) -> Mapped<S, P, F> {
    Mapped::new(parser, map)
}

#[derive(Debug, Clone, Copy)]
pub struct MappedIf<S, P, F> {
    parser: P,
    map: F,
    _phantom: PhantomData<S>
}

impl<R, S: Source, P: ParserCore<S>, F: Fn(P::Token) -> R> MappedIf<S, P, F> {
    pub fn new(parser: P, map: F) -> Self {
        Self {
            parser,
            map,
            _phantom: PhantomData
        }
    }
}

impl<R, S: Source, P: ParserCore<S>, F: Fn(P::Token) -> Option<R>> ParserCore<S> for MappedIf<S, P, F>
where
    Self: Clone,
{
    type Token = R;
    fn parse(&self, source: S) -> PResult<Self::Token, S> {
        let (token, source) = unpack_presult(self.parser.parse(source));
        // (token.and_then(|t| (self.map)(t)), source)
        if let Some(token) = token {
            if let Some(token) = (self.map)(token) {
                Ok((token, source))
            } else {
                Err(source)
            }
        } else {
            Err(source)
        }
    }
}

pub fn map_if<R, S: Source, P: ParserCore<S>, F: Fn(P::Token) -> Option<R>>(parser: P, map: F) -> MappedIf<S, P, F> {
    MappedIf::new(parser, map)
}

#[derive(Debug, Clone, Copy)]
pub struct Match<Span, E> {
    pub span: Span,
    pub element: E,
}

pub fn matches<S: Source>(test: impl Fn(&S::Element) -> bool + Clone) -> impl Parser<S, Token = Match<S::Span, S::Element>> {
    move |source: S| {
        if let Some(element) = source.get_at(&source.start()) {
            if test(&element) {
                let span = source.make_span(source.start(), source.next(source.start(), 1));
                let next = source.next(source.start(), 1);
                return Ok((
                    Match {
                        span,
                        element,
                    },
                    source.tail(next),
                ))
            }
        }

        source.unparsed_result()
    }
}

#[macro_export]
macro_rules! match_variant {
    ($($variant:tt)+) => {
        $crate::provided::common::matches(|element| if let $($variant)+(_) = element { true } else { false })
            .map(|m| if let $($variant)+(element) = m.element {
                $crate::provided::common::Match {
                    element,
                    span: m.span,
                }
            } else {
                unreachable!()
            })
    };

}

#[macro_export]
macro_rules! alt {
    ($($a:expr),*,) => {
        move |source: S| {
            if false { unreachable!() }
            $(
                else if let Ok((token, source)) = $a.parse(source.clone()) {
                    Ok((
                        token,
                        source,
                    ))
                }
            )*
            else {
                source.unparsed_result()
            }
        }
    };
    //() => { () };
    //($a:expr, $($b:expr),*,) => {
    //    $a
    //    $(
    //        .or($b)
    //    )*
    //};
}

pub fn alt<S: Source, P: ParserCore<S>, I>(parsers: impl Fn() -> I + Clone) -> impl Parser<S, Token = P::Token>
where
    I: IntoIterator<Item = P>,
{
    move |source: S| {
        for parser in parsers() {
            if let Ok((token, source)) = parser.parse(source.clone()) {
                return Ok((
                    token,
                    source,
                ))
            }
        }
        unparsed(source)
    }
}

//#[derive(Clone)]
//pub struct Opaque<'a, S: Source, T> {
//    parser: Rc<dyn ParserCore<S, Token = T> + 'a>,
//    _phantom: PhantomData<S>,
//}
//
//impl<'a, S, T> Opaque<'a, S, T>
//where
//    S: Source,
//{
//    pub fn new(parser: impl ParserCore<S, Token = T> + 'a) -> Self {
//        Self {
//            parser: Rc::new(parser),
//            _phantom: PhantomData,
//        }
//    }
//}
//
//impl<'a, S: Source, T> ParserCore<S> for Opaque<'a, S, T> {
//    type Token = T;
//    fn parse(&self, source: S) -> PResult<Self::Token, S> {
//        let Opaque { parser, .. } = self;
//        parser.parse(source)
//    }
//}
//
//pub fn opaque<'a, S: Source, T>(parser: impl ParserCore<S, Token = T> + 'a) -> Opaque<'a, S, T> {
//    Opaque::new(parser)
//}
//
//pub fn opaque2<S: Source, T>(parser: impl ParserCore<S, Token = T> + Clone) -> impl Parser<S, Token = T> {
//    move |source: S| {
//        parser.parse(source)
//    }
//}
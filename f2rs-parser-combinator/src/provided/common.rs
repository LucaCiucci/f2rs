use std::marker::PhantomData;

use crate::tokenization::{ParserCore, Parser, Source, PResult};

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
        self.parser.parse(source).map(|(token, source)| ((self.map)(token), source))
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
        self.parser
            .parse(source)
            .map(|(token, source)| if let Some(token) = (self.map)(token) {
                Some((token, source))
            } else {
                None
            })
            .flatten()
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
                return Some((
                    Match {
                        span,
                        element,
                    },
                    source.tail(next),
                ))
            }
        }

        None
    }
}

pub fn matches_map<S: Source, R>(test: impl Fn(S::Element) -> Option<R> + Clone) -> impl Parser<S, Token = R> {
    move |source: S| {
        if let Some(element) = source.get_at(&source.start()) {
            if let Some(r) = test(element) {
                let next = source.next(source.start(), 1);
                return Some((
                    r,
                    source.tail(next),
                ))
            }
        }

        None
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
    (for $S:ty => $($a:expr),*,) => {
        move |source: $S| {
            if false { unreachable!() }
            $(
                else if let Some((token, source)) = {
                    //eprintln!("{}", stringify!($a));
                    $a.parse(source.clone())
                } {
                    Some((
                        token,
                        source,
                    ))
                }
            )*
            else {
                None
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
            let r = parser.parse(source.clone());
            if r.is_some() {
                return r
            }
        }
        None
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
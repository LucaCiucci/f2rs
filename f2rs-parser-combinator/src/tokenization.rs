
use std::ops::{Range, RangeBounds, RangeFull};

use crate::{provided::common::{Mapped, Then, Or, Optional, MappedIf, Where_}, prelude::{DoNotConsume, Named, If_, Condition}};

pub trait Spanned<Span> {
    fn span(&self) -> &Span;
}

pub trait MapSpan<Span> {
    type Spanned<T>: MapSpan<T>;
    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S>;
}

impl<Span, Tk: MapSpan<Span>> MapSpan<Span> for Vec<Tk> {
    type Spanned<T> = Vec<Tk::Spanned<T>>;
    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        self.into_iter().map(|tk| tk.map_span(f)).collect()
    }
}

pub trait TokenTree<Span>: Spanned<Span> {
    fn children<'s>(&'s self) -> Box<dyn Iterator<Item = &dyn TokenTree<Span>> + 's> {
        Box::new(std::iter::empty())
    }

    //fn depth(&self) -> usize {
    //    #[cfg(debug_assertions)]
    //    log::warn!("depth not implemented for {:?}, using default implementation", std::any::type_name::<Self>());
//
    //    // use a stack to avoid recursion,
    //    let mut stack = vec![(self.children(), 0)];
    //    let mut max_depth = 0;
    //    while let Some((children, depth)) = stack.pop() {
    //        max_depth = max_depth.max(depth);
    //        for child in children {
    //            stack.push((child.children(), depth + 1));
    //        }
    //    }
//
    //    max_depth
    //}
}

pub type PResult<T, S> = Option<(T, S)>; // TODO parse error

#[deprecated]
pub fn unparsed<T, S>() -> PResult<T, S> {
    None
}
#[deprecated]
pub fn parsed<T, S>(value: T, tail: S) -> PResult<T, S> {
    Some((value, tail))
}

pub trait ParserCore<S: Source> {
    //type Token: TokenTree<S::Span>;
    type Token;
    fn parse(&self, source: S) -> PResult<Self::Token, S>;
}

impl<T, S: Source> Parser<S> for T
where
    T: ParserCore<S> + Clone,
{
}

// TODO rename to Parser
pub trait Parser<S: Source>: ParserCore<S> + Clone {
    fn parses(&self, source: S) -> bool {
        self.parse(source).is_some()
    }

    fn named(self, name: &'static str) -> Named<Self>
    where
        Self: Sized,
    {
        Named::new(name, self)
    }

    fn if_(self, condition: bool) -> If_<Self>
    where
        Self: Sized,
    {
        If_::new(condition, self)
    }

    fn map<R, F: Fn(Self::Token) -> R>(self, map: F) -> Mapped<S, Self, F>
    where
        Self: Sized,
    {
        Mapped::new(self, map)
    }

    fn map_if<R, F: Fn(Self::Token) -> Option<R>>(self, map: F) -> MappedIf<S, Self, F>
    where
        Self: Sized,
    {
        MappedIf::new(self, map)
    }

    fn then<P2, FP2>(
        self,
        fp2: FP2,
    ) -> Then<Self, FP2>
    where
        Self: Sized,
        P2: ParserCore<S>,
        FP2: Fn(Self::Token) -> P2,
    {
        Then::new(self, fp2)
    }

    fn where_(
        self,
        condition: bool,
    ) -> Where_<Self>
    where
        Self: Sized,
    {
        Where_::new(condition, self)
    }

    fn or<P2>(self, p2: P2) -> Or<Self, P2>
    where
        Self: Sized,
        P2: ParserCore<S, Token = Self::Token>,
    {
        Or::new(self, p2)
    }

    fn optional(self) -> Optional<Self>
    where
        Self: Sized,
    {
        Optional::new(self)
    }

    fn do_not_consume(self) -> DoNotConsume<Self>
    where
        Self: Sized,
    {
        DoNotConsume::new(self)
    }

    fn condition<F>(self, condition: F) -> Condition<Self, F>
    where
        Self: Sized,
        F: Fn(&Self::Token, &S) -> bool,
    {
        Condition::new(self, condition)
    }
}

impl<F, T, S: Source> ParserCore<S> for F
where
    F: Clone,
    F: Fn(S) -> PResult<T, S>,
    //T: TokenTree<S::Span>,
{
    type Token = T;
    fn parse(&self, source: S) -> PResult<Self::Token, S> {
        self(source)
    }
}

pub trait SourceSpan: Clone {
    fn new_null() -> Self;
    fn is_null(&self) -> bool;
    fn merge(a: Self, b: Self) -> Self; // Note: could be null
}

impl SourceSpan for () {
    fn new_null() -> Self {
        ()
    }
    fn is_null(&self) -> bool {
        true
    }
    fn merge(_a: Self, _b: Self) -> Self {
        ()
    }
}

impl<Idx: PartialOrd + Default + Clone> SourceSpan for Range<Idx> {
    fn new_null() -> Self {
        Default::default()..Default::default()
    }
    fn is_null(&self) -> bool {
        self.is_empty()
    }
    fn merge(a: Self, b: Self) -> Self {
        if a.is_empty() {
            b
        } else if b.is_empty() {
            a
        } else {
            let start = if a.start < b.start { a.start } else { b.start };
            let end = if a.end > b.end { a.end } else { b.end };
            start..end
        }
    }
}

pub trait Source: Clone {
    type Element;
    type Index: Clone;
    type Span: SourceSpan;

    fn get_at<'s>(&'s self, index: &Self::Index) -> Option<Self::Element>; // TODO make it possible to return a reference
    fn start(&self) -> Self::Index;
    fn next(&self, index: Self::Index, count: usize) -> Self::Index;
    fn empty(&self) -> bool {
        self.get_at(&self.start()).is_none()
    }
    fn make_span(&self, start: Self::Index, end: Self::Index) -> Self::Span;
    fn tail(self, end: Self::Index) -> Self;
    fn full_span(&self) -> Self::Span;
    fn take(self, end: Self::Index) -> (Self::Span, Self) {
        let span = self.make_span(self.start(), end.clone());
        (span, self.tail(end))
    }
    fn parsed_result<T>(self, end: Self::Index, token: impl FnOnce(Self::Span) -> T) -> PResult<T, Self> {
        let (span, tail) = self.take(end);
        Some((token(span), tail))
    }
    fn parse<Token>(&mut self, parser: &impl Parser<Self, Token = Token>) -> Option<Token> {
        if let Some((token, tail)) = parser.parse(self.clone()) {
            *self = tail;
            Some(token)
        } else {
            None
        }
    }
}

pub trait TextSource: Source<Element = char> {
    // TODO ...
}

impl<T: Clone> Source for &[T] {
    type Element = T;
    type Index = usize;
    type Span = ();

    fn get_at<'s>(&'s self, index: &Self::Index) -> Option<Self::Element> {
        self.get(*index).cloned()
    }

    fn start(&self) -> Self::Index {
        0
    }

    fn next(&self, index: Self::Index, count: usize) -> Self::Index {
        index + count
    }

    fn make_span(&self, _start: Self::Index, _end: Self::Index) -> Self::Span {
        ()
    }

    fn tail(self, end: Self::Index) -> Self {
        &self[end..]
    }

    fn full_span(&self) -> Self::Span {
        ()
    }
}

pub trait AppendType<T> {
    type Result;
    fn append(self, t: T) -> Self::Result;
}

//impl<T> AppendType<T> for () {
//    type Result = (T,);
//    fn append(self, t: T) -> Self::Result {
//        (t,)
//    }
//}
//
//impl<T1, T> AppendType<T> for (T1,)
//where
//    T1: Clone,
//{
//    type Result = (T1, T);
//    fn append(self, t: T) -> Self::Result {
//        (self.0, t)
//    }
//}

macro_rules! impl_append {
    () => {
        impl<T> AppendType<T> for () {
            type Result = (T,);
            fn append(self, t: T) -> Self::Result {
                (t,)
            }
        }
    };
    ($($t:ident)*) => {
        impl<$($t,)* T> AppendType<T> for ($($t,)*)
        where
            $($t: Clone,)*
        {
            type Result = ($($t,)* T);
            fn append(
                self,
                t: T,
            ) -> Self::Result {
                #[allow(non_snake_case)]
                let ($($t,)*) = self;
                ($($t,)* t)
            }
        }
    };
}

impl_append!();
impl_append!(T1);
impl_append!(T1 T2);
impl_append!(T1 T2 T3);
impl_append!(T1 T2 T3 T4);
impl_append!(T1 T2 T3 T4 T5);
impl_append!(T1 T2 T3 T4 T5 T6);
impl_append!(T1 T2 T3 T4 T5 T6 T7);
impl_append!(T1 T2 T3 T4 T5 T6 T7 T8);
impl_append!(T1 T2 T3 T4 T5 T6 T7 T8 T9);
impl_append!(T1 T2 T3 T4 T5 T6 T7 T8 T9 T10);
impl_append!(T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11);
impl_append!(T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12);
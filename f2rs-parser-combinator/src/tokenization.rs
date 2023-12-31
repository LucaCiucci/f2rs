
use crate::{provided::common::{Mapped, Then, Or, Optional, MappedIf, Where_, OrError}, prelude::{DoNotConsume, Named, If_, Condition}};

pub trait TokenTree<Span> {
    fn span<'s>(&'s self) -> &'s Span;

    //fn name<'s>(&'s self) -> Cow<'s, str>;

    fn children<'s>(&'s self) -> Box<dyn Iterator<Item = &dyn TokenTree<Span>> + 's> {
        Box::new(std::iter::empty())
    }
}

pub type PResult<T, S> = Result<(T, S), S>; // TODO parse error

#[deprecated]
pub fn unparsed<T, S>(source: S) -> PResult<T, S> {
    Err(source)
}
#[deprecated]
pub fn parsed<T, S>(value: T, tail: S) -> PResult<T, S> {
    Ok((value, tail))
}

#[deprecated]
pub fn unpack_presult<T, S>(result: PResult<T, S>) -> (Option<T>, S) {
    match result {
        Ok((token, tail)) => (Some(token), tail),
        Err(tail) => (None, tail),
    }
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
        self.parse(source).is_ok()
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

    fn or_error<E: Clone>(self, e: E) -> OrError<Self, E> {
        OrError::new(self, e)
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

pub trait Source: Clone {
    type Element;
    type Index: Clone;
    type Span: Clone;

    fn get_at<'s>(&'s self, index: &Self::Index) -> Option<Self::Element>; // TODO make it possible to return a reference
    fn start(&self) -> Self::Index;
    fn next(&self, index: Self::Index, count: usize) -> Self::Index;
    fn empty(&self) -> bool {
        self.get_at(&self.start()).is_none()
    }
    fn make_span(&self, start: Self::Index, end: Self::Index) -> Self::Span;
    fn merge_span(a: Self::Span, b: Self::Span) -> Self::Span; // TODO maybe take &self ???
    fn tail(self, end: Self::Index) -> Self;
    fn take(self, end: Self::Index) -> (Self::Span, Self) {
        let span = self.make_span(self.start(), end.clone());
        (span, self.tail(end))
    }
    fn unparsed_result<T>(self) -> PResult<T, Self> {
        unparsed(self)
    }
    fn parsed_result<T>(self, end: Self::Index, token: impl FnOnce(Self::Span) -> T) -> PResult<T, Self> {
        let (span, tail) = self.take(end);
        parsed(token(span), tail)
    }
    fn null_span() -> Self::Span;
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

    fn merge_span(_a: Self::Span, _b: Self::Span) -> Self::Span {
        ()
    }

    fn tail(self, end: Self::Index) -> Self {
        &self[end..]
    }

    fn null_span() -> Self::Span {
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
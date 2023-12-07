
use super::*;

#[derive(Debug, Clone)]
pub enum Implicit<Span> {
    None,
    _Phantom(std::marker::PhantomData<Span>),
}


pub fn implicit<S: TextSource>() -> impl Parser<S, Token = Implicit<S::Span>> {
    (
        spaced(keyword("implicit")),
        spaced(keyword("none")),
        eol_or_comment(),
    ).map(|_| Implicit::None)
}
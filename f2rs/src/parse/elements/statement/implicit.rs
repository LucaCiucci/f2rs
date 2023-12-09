use super::*;

use enum_as_inner::EnumAsInner;

#[derive(Debug, Clone, EnumAsInner)]
pub enum Implicit<Span> {
    ImplicitNone,
    _Phantom(std::marker::PhantomData<Span>),
}

pub fn implicit<S: TextSource>() -> impl Parser<S, Token = Implicit<S::Span>> {
    (
        spaced(keyword("implicit")),
        spaced(keyword("none")),
        eol_or_comment(),
    )
        .map(|_| Implicit::ImplicitNone)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_implicit() {
        let src = "implicit none";
        let r = implicit().parse(src).0.unwrap();

        assert!(r.is_implicit_none());
    }
}

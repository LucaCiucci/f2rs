use super::*;

// TODO use cfg
pub fn delimiter<S: TextSource>(source: S) -> PResult<SpecialCharacterMatch<S::Span>, S> {
    alt!(
        for S =>
        SpecialCharacter::LeftParenthesis,
        SpecialCharacter::RightParenthesis,
        SpecialCharacter::LeftSquareBracket,
        SpecialCharacter::RightSquareBracket,
        SpecialCharacter::LeftCurlyBracket,
        SpecialCharacter::RightCurlyBracket,
    )
    .parse(source)
}

macro_rules! just_spanned {
    ($name:ident) => {
        #[derive(Debug, Clone)]
        pub struct $name<Span> {
            span: Span,
        }

        impl<Span> $name<Span> {
            pub fn new_spanned(span: Span) -> Self {
                $name { span }
            }
        }

        impl<Span> Spanned<Span> for $name<Span> {
            fn span(&self) -> &Span {
                &self.span
            }
        }

        impl<Span> MapSpan<Span> for $name<Span> {
            type Spanned<T> = $name<T>;

            fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
                $name {
                    span: f(self.span),
                }
            }
        }

        impl<Span> TokenTree<Span> for $name<Span> {
        }
    };
}

just_spanned!(Comma);
just_spanned!(Equals);
just_spanned!(Arrow);
just_spanned!(Colon);
just_spanned!(DoubleColon);
just_spanned!(Semicolon);
just_spanned!(DotDot);
just_spanned!(Percent);
just_spanned!(Dot);
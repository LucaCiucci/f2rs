pub use super::*;

#[derive(Debug, Clone)]
pub struct Label<Span>(pub IntLiteralConstant<Span>);

impl<Span> Spanned<Span> for Label<Span> {
    fn span(&self) -> &Span {
        &self.0.span
    }
}

impl<Span> MapSpan<Span> for Label<Span> {
    type Spanned<T> = Label<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        Label(self.0.map_span(f))
    }
}

#[doc = s_rule!(
    F18V007r1 rule "label" #611 : "is digit [ digit [ digit [ digit [ digit ] ] ] ]",
)]
pub fn label<S: TextSource>(source: S) -> PResult<Label<S::Span>, S> {
    // TODO implement rule (max 5 digits and no kind) and clause somewhere else
    digit_string::<S>.map(|s| Label({
        let span = s.span.clone();
        IntLiteralConstant {
            span,
            digits: s,
            kind_param: None,
        }
    }))
    .parse(source)
}
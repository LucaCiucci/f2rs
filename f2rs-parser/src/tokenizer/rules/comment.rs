use super::*;

// TODO ???
#[derive(Debug, Clone)]
pub struct LineComment<Span> {
    pub text: String,
    pub span: Span,
}

impl<Span> Spanned<Span> for LineComment<Span> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl<Span> MapSpan<Span> for LineComment<Span> {
    type Spanned<T> = LineComment<T>;
    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        LineComment {
            text: self.text,
            span: f(self.span),
        }
    }
}

// TODO ???
pub fn comment_start<S: TextSource>(source: S) -> PResult<S::Span, S> {
    // we accept both ! and c as comment starters
    //ExactMatch::exact("!", false).or(ExactMatch::exact("c ", true))
    fold_many(
        StringMatch::exact("!", false),
        || S::Span::new_null(),
        |s, m| (S::Span::merge(s, m.span), true),
        1..,
    )
    .parse(source)
}

// TODO ???
pub fn line_comment<S: TextSource>(source: S) -> PResult<LineComment<S::Span>, S> {
    comment_start.then(|bang_span: S::Span| {
        many_until(Char::<S::Span>::any(), eol.do_not_consume(), 0..).map(move |(chars, _newline)| {
            let bang_span = bang_span.clone();
            let span = if let Some(last) = chars.last() {
                S::Span::merge(bang_span, last.span.clone())
            } else {
                bang_span
            };

            LineComment {
                text: chars.into_iter().map(|c| c.value).collect(),
                span,
            }
        })
    })
    .parse(source)
}
use super::*;

#[derive(Debug, Clone)]
#[derive(EnumAsInner)]
pub enum StringElement<Span> {
    Char(Char<Span>),
    EscapeSequence(StringMatch<Span>, &'static str),
}

impl<Span> StringElement<Span> {
    pub fn value(&self) -> String {
        match self {
            Self::Char(c) => c.value.to_string(),
            Self::EscapeSequence(s, _) => s.value.clone(),
        }
    }

    pub fn span(&self) -> &Span {
        match self {
            Self::Char(c) => &c.span,
            Self::EscapeSequence(s, _) => &s.span,
        }
    }
}

impl<Span> MapSpan<Span> for StringElement<Span> {
    type Spanned<T> = StringElement<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        match self {
            Self::Char(c) => StringElement::Char(c.map_span(f)),
            Self::EscapeSequence(s, into) => StringElement::EscapeSequence(s.map_span(f), into),
        }
    }
}

pub fn string_element<'a, S: TextSource + 'a>(termination: char, escape: &'static str, into: &'static str) -> impl Parser<S, Token = StringElement<S::Span>> {
    alt!(
        for S =>
        StringMatch::exact(escape, true).map(|s| StringElement::EscapeSequence(s, into)),
        Char::parse(|c| c != termination).map(|c| StringElement::Char(c)),
    )
}

#[derive(Debug, Clone)]
pub struct CharLiteralConstant<Span> {
    pub kind_param: Option<KindParam<Span>>,
    pub delimiter: char,
    pub open_quote: Char<Span>,
    pub content: Vec<StringElement<Span>>,
    pub close_quote: Option<Char<Span>>,
    pub span: Span,
}

impl<Span> CharLiteralConstant<Span> {
    pub fn value(&self) -> String {
        self.content.iter().map(|e| e.value()).collect()
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

impl<Span> MapSpan<Span> for CharLiteralConstant<Span> {
    type Spanned<T> = CharLiteralConstant<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        CharLiteralConstant {
            kind_param: self.kind_param.map(|k| k.map_span(f)),
            delimiter: self.delimiter,
            open_quote: self.open_quote.map_span(f),
            content: self.content.into_iter().map(|e| e.map_span(f)).collect(),
            close_quote: self.close_quote.map(|c| c.map_span(f)),
            span: f(self.span),
        }
    }
}

#[doc = s_rule!(
    F18V007r1 rule "char-literal-constant" #724 :
    "is [ kind-param _ ] ' [ rep-char ] ... '"
    "or [ kind-param _ ] \" [ rep-char ] ... \"",
)]
pub fn char_literal_constant<S: TextSource>(source: S) -> PResult<CharLiteralConstant<S::Span>, S> {
    alt! {
        for S =>
        (
            (kind_param(true), underscore).map(|(k, _)| k).opt(),
            Char::<S::Span>::exact('\''),
            fold_many(
                string_element('\'', "''", "'"),
                || Vec::new(),
                |mut content, element: StringElement<S::Span>| {
                    content.push(element);
                    (content, true)
                },
                0..,
            ),
            Char::<S::Span>::exact('\'').opt(),
        )
            .map(|(kind_param, open_quote, content, close_quote)| {
                let mut span = open_quote.span.clone();
                if let Some(close_quote) = &close_quote {
                    span = S::Span::merge(span, close_quote.span.clone());
                } else if let Some(last) = content.last() {
                    span = S::Span::merge(span, last.span().clone());
                }
                CharLiteralConstant {
                    kind_param,
                    delimiter: '\'',
                    open_quote,
                    content,
                    close_quote,
                    span,
                }
            }),
        (
            (kind_param(true), underscore).map(|(k, _)| k).opt(),
            Char::<S::Span>::exact('"'),
            fold_many(
                string_element('"', "\"\"", "\""),
                || Vec::new(),
                |mut content, element: StringElement<S::Span>| {
                    content.push(element);
                    (content, true)
                },
                0..,
            ),
            Char::<S::Span>::exact('"').opt(),
        )
            .map(|(kind_param, open_quote, content, close_quote)| {
                let mut span = open_quote.span.clone();
                if let Some(close_quote) = &close_quote {
                    span = S::Span::merge(span, close_quote.span.clone());
                } else if let Some(last) = content.last() {
                    span = S::Span::merge(span, last.span().clone());
                }
                CharLiteralConstant {
                    kind_param,
                    delimiter: '"',
                    open_quote,
                    content,
                    close_quote,
                    span, // TODO ...
                }
            }),
    }.parse(source)
}

#[cfg(test)]
mod test {

    use crate::rule_test;
    use super::super::examples;

    use super::*;

    rule_test! {
        char_literal_constant(F18V007r1 724) {
            assert_eq!(char_literal_constant.parses(""), false);
            assert_eq!(char_literal_constant.parses("\"abc\""), true);
            assert_eq!(char_literal_constant.parses("'abc'"), true);
            let m = char_literal_constant.parse("123_'abc''").unwrap();
            let m = m.0;
            assert!(m.kind_param.as_ref().unwrap().is_digit_string());
            assert_eq!(m.kind_param.as_ref().unwrap().as_digit_string().unwrap().value, "123");
            assert_eq!(m.delimiter, '\'');
            assert!(m.close_quote.is_none());
            assert_eq!(m.content.last().unwrap().as_escape_sequence().unwrap().0.value, "''");
            let m = char_literal_constant.parse("123_\"abc\"\"").unwrap();
            let m = m.0;
            assert!(m.kind_param.as_ref().unwrap().is_digit_string());
            assert_eq!(m.kind_param.as_ref().unwrap().as_digit_string().unwrap().value, "123");
            assert_eq!(m.delimiter, '"');
            assert!(m.close_quote.is_none());
            assert_eq!(m.content.last().unwrap().as_escape_sequence().unwrap().0.value, "\"\"");

            // Examples from the standard
            examples(char_literal_constant, [
                "\"DON'T\"",
                "'DON''T'",
                "''",
            ]);
            // TODO add example "Note 2" in F18V007r1 §7.4.4.3
        }
    }
}
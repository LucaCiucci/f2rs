use super::*;

#[derive(Debug, Clone)]
pub struct LogicalLiteralConstant<Span> {
    pub value_match: StringMatch<Span>,
    pub value: bool,
    pub kind: Option<KindParam<Span>>,
    span: Span,
}

impl<Span> LogicalLiteralConstant<Span> {
    pub fn span(&self) -> &Span {
        &self.span
    }
}

impl<Span> MapSpan<Span> for LogicalLiteralConstant<Span> {
    type Spanned<T> = LogicalLiteralConstant<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        LogicalLiteralConstant {
            value_match: self.value_match.map_span(f),
            value: self.value,
            kind: self.kind.map(|k| k.map_span(f)),
            span: f(self.span),
        }
    }
}

#[doc = s_rule!(
    F18V007r1 rule "logical-literal-constant" #725 :
    "is .TRUE. [ _ kind-param ]"
    "or .FALSE. [ _ kind-param ]",
)]
pub fn logical_literal_constant<S: TextSource>(source: S) -> PResult<LogicalLiteralConstant<S::Span>, S> {
    (
        alt!(
            for S =>
            StringMatch::exact(".TRUE.", false).map(|m| (m, true)),
            StringMatch::exact(".FALSE.", false).map(|m| (m, false)),
        ),
        (space(0), underscore, space(0), kind_param(true)).map(|(_, _, _, k)| k).opt(),
    ).map(|(value, kind): ((StringMatch<S::Span>, bool), Option<KindParam<S::Span>>)| {
        let mut span = value.0.span.clone();
        if let Some(kind) = &kind {
            span = S::Span::merge(span, kind.span().clone());
        }
        LogicalLiteralConstant {
            value_match: value.0,
            value: value.1,
            kind,
            span,
        }
    }).parse(source)
}

#[cfg(test)]
mod test {

    use crate::rule_test;
    //use super::super::examples;

    use super::*;

    rule_test! {
        logical_literal_constant(F18V007r1 725) {
            assert_eq!(logical_literal_constant.parses(""), false);
            assert_eq!(logical_literal_constant.parses(".true."), true);
            assert_eq!(logical_literal_constant.parses(".false."), true);
            assert_eq!(logical_literal_constant.parses(".TRUE._foo"), true);
            assert_eq!(logical_literal_constant.parses(".FALSE."), true);
            assert_eq!(logical_literal_constant.parses(".true"), false);
            assert_eq!(logical_literal_constant.parses(".false"), false);
            assert_eq!(logical_literal_constant.parses(".TRUE"), false);
            assert_eq!(logical_literal_constant.parses(".FALSE"), false);

            let m = logical_literal_constant.parse(".true._foo").unwrap();
            let m = m.0;
            assert_eq!(m.value, true);
            assert!(m.kind.is_some());
            assert_eq!(m.kind.unwrap().as_scalar_int_constant_name().unwrap().0.value, "foo");

            let m = logical_literal_constant.parse(".FALSE._42").unwrap();
            let m = m.0;
            assert_eq!(m.value, false);
            assert!(m.kind.is_some());
            assert_eq!(m.kind.unwrap().as_digit_string().unwrap().value, "42");
        }
    }
}
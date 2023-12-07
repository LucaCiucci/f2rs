
use super::*;

#[derive(Debug, Clone)]
pub struct VariablesDeclaration<Span> {
    pub vars: Vec<(Type<Span>, String)>,
}

pub fn variable_declaree<S: TextSource>(base_type: Type<S::Span>) -> impl Parser<S, Token = (Type<S::Span>, String)> {
    // TODO arrays
    (
        spaced(identifier()),
        (
            spaced('('),
            separated(expression(), spaced(','), 0..),
            spaced(')'),
        ).map(|(_, ranges, _)| ranges).optional(),
    )
        .map(move |(name, array_ranges)| {
            let ty = if let Some(ranges) = array_ranges {
                Type::Array {
                    ty: Box::new(base_type.clone()),
                    ranges,
                }
            } else {
                base_type.clone()
            };
            (ty, name.value)
        })
}

pub fn variables_declaration<S: TextSource>() -> impl Parser<S, Token = VariablesDeclaration<S::Span>> {
    let d = spaced(type_())
        .then(move |ty| {
            (
                spaced(ExactMatch::exact("::", false)).optional(),
                separated(
                    variable_declaree(ty.clone()), // TODO not identifier
                    spaced(ExactMatch::exact(",", false)),
                    1..,
                ),
            ).map(move |(_, vars)| vars)
        });
    (d, eol_or_comment()).map(|(vars, _)| VariablesDeclaration { vars })
}
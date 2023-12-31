use super::*;

#[derive(Debug, Clone)]
pub struct VariablesDeclaration<Span> {
    pub vars: Vec<(TypeWithModifiers<Span>, String, Option<Expression<Span>>)>,
    pub comment: Option<LineComment<Span>>,
}

pub fn variable_declaree<S: TextSource>(
    base_type: TypeWithModifiers<S::Span>,
) -> impl Parser<S, Token = (TypeWithModifiers<S::Span>, String, Option<Expression<S::Span>>)> {
    // TODO arrays
    (
        spaced(identifier()),
        (
            spaced('('),
            separated(expression(), spaced(','), 0..),
            spaced(')'),
        )
            .map(|(_, ranges, _)| ranges)
            .optional(),
        (
            spaced('='),
            spaced(expression()),
        ).map(|(_, expr)| expr).optional(),
    )
        .map(move |(name, array_ranges, initialization)| {
            let ty = if let Some(ranges) = array_ranges {
                TypeWithModifiers {
                    ty: Type::Array {
                        ty: Box::new(base_type.ty.clone()),
                        ranges,
                    },
                    modifiers: base_type.modifiers.clone(),
                }
            } else {
                base_type.clone()
            };
            (ty, name.value, initialization)
        })
}

pub fn variables_declaration<S: TextSource>(
) -> impl Parser<S, Token = VariablesDeclaration<S::Span>> {
    let d = spaced(type_with_modifiers()).then(move |ty| {
        (
            spaced(StringMatch::exact("::", false)).optional(),
            separated(
                variable_declaree(ty.clone()), // TODO not identifier
                spaced(StringMatch::exact(",", false)),
                1..,
            ),
        )
            .map(move |(_, vars)| vars)
    });
    (d, eol_or_comment()).map(|(vars, comment)| VariablesDeclaration { vars, comment })
}

#[cfg(test)]
mod tests {
    //use super::*;

    // TODO ...
    // integer i
    // logical :: flag1, flag2
    // double precision :: d
    // integer(c_int) :: c
    // integer*2 :: c2
    // integer(4) :: c3
    // double complex :: z
    // integer :: some_vec(:, 2, 3), some_vec2(2, 3)
    // type(integer) iii
    // character*4 :: str
    // integer dsnjf(1:10)
}

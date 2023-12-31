use super::*;

pub fn special_function_2_rs(
    special_function: &SpecialStatementFunction<Span>,
    ctx: &Ctx,
) -> String {
    let special = if let Some(specials) = &special_function.special_args {
        let s = specials
            .iter()
            .map(|se| star_or_expr_2_rs(se))
            .collect::<Vec<_>>()
            .join(", ");
        format!(" ({})", s)
    } else {
        String::new()
    };
    let items = special_function
        .items
        .iter()
        .map(|se| star_or_expr_2_rs(se))
        .collect::<Vec<_>>()
        .join(", ");
    format!(
        "fortran!({}{} {});\n",
        correct_identifier(&special_function.name.value), special, items
    )
}
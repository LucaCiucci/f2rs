use super::*;

pub fn do_loop_2_rs(
    do_loop: &DoLoop<Span>,
    ctx: &Ctx,
) -> String {
    let mut out = BufWriter::new(Vec::new());
    if let Some(step) = &do_loop.step {
        writeln!(
            &mut out,
            "for {} in ({}..={}).step_by({}) {{{}",
            do_loop.variable,
            expression_2_rs(&do_loop.start, false),
            expression_2_rs(&do_loop.end, false),
            expression_2_rs(&step, false),
            optional_comment(&do_loop.opening_comment),
        )
        .unwrap();
    } else {
        writeln!(
            &mut out,
            "for {} in {}..={} {{{}",
            do_loop.variable,
            expression_2_rs(&do_loop.start, false),
            expression_2_rs(&do_loop.end, false),
            optional_comment(&do_loop.opening_comment),
        )
        .unwrap();
    }
    for item in &do_loop.body {
        write!(&mut out, "{}", element_2_rs(item, ctx, &[])).unwrap();
    }
    writeln!(&mut out, "}}").unwrap();

    String::from_utf8(out.into_inner().unwrap()).unwrap()
}
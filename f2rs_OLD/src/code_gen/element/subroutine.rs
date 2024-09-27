use crate::{parse::Subroutine, code_gen::ctx::resolve_arguments};

use super::*;

pub fn subroutine_2_rs(
    subroutine: &Subroutine<Span>,
    ctx: &Ctx,
) -> String {
    let args = resolve_arguments(
        subroutine.args.iter().map(|a| a.value.to_string()),
        &subroutine.body,
    );

    let mut out = BufWriter::new(Vec::new());
    write_function(
        &mut out,
        &subroutine.name,
        &args,
        None,
        &subroutine.body,
        ctx
    );

    String::from_utf8(out.into_inner().unwrap()).unwrap()
}
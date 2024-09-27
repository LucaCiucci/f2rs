use crate::{parse::Subroutine, code_gen::ctx::{resolve_arguments, resolve_return_type}};

use super::*;

pub fn function_2_rs(
    function: &Function<Span>,
    ctx: &Ctx,
) -> String {
    let args = resolve_arguments(
        function.args.iter().map(|a| a.value.to_string()),
        &function.body,
    );

    let ret = resolve_return_type(function);

    let mut out = BufWriter::new(Vec::new());
    write_function(
        &mut out,
        &function.name.value,
        &args,
        None,
        &function.body,
        ctx
    );

    String::from_utf8(out.into_inner().unwrap()).unwrap()
}
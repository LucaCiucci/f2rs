use crate::parse::Module;

use super::*;

pub fn module_2_rs(
    module: &Module<Span>,
    ctx: &Ctx,
) -> String {
    let ctx = &ctx.in_non_code_body(&module.items);

    let mut out = BufWriter::new(Vec::new());
    writeln!(&mut out, "pub mod {} {{", module.name).unwrap();
    writeln!(&mut out, "use super::*;").unwrap();
    let mut contains_found = false;
    for item in &module.items {
        write!(&mut out, "{}", element_2_rs(item, ctx, &[])).unwrap();
        if item.is_contains() {
            contains_found = true;
            writeln!(&mut out, "mod contains {{").unwrap();
            writeln!(&mut out, "use super::*;").unwrap();
        }
    }
    if contains_found {
        writeln!(&mut out, "}}").unwrap();
    }
    writeln!(&mut out, "}}").unwrap();
    String::from_utf8(out.into_inner().unwrap()).unwrap()
}
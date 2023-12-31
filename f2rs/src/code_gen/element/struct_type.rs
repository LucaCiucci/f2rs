use super::*;

pub fn struct_type_2_rs(
    s: &StructType<Span>,
    ctx: &Ctx,
) -> String {
    let ctx = &ctx.in_type_definition(&s.body);

    let StructType { name, body } = s;
    let mut out = BufWriter::new(Vec::new());
    writeln!(&mut out, "pub struct {} {{", name).unwrap();
    for item in body {
        write!(
            &mut out,
            "{}",
            element_2_rs(item, ctx, &[])
        ).unwrap();
    }
    writeln!(&mut out, "}}").unwrap();
    String::from_utf8(out.into_inner().unwrap()).unwrap()
}
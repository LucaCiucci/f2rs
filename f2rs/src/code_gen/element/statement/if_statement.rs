use super::*;

pub fn if_2_rs(
    if_statement: &IfStatement<Span>,
    ctx: &Ctx,
) -> String {
    let mut out = BufWriter::new(Vec::new());

    match if_statement.deref() {
        IfStatement::Statement {
            ref condition,
            ref condition_comment,
            ref blocks,
        } => {
            writeln!(&mut out, "if {} {{{}", expression_2_rs(condition, false), optional_comment(condition_comment)).unwrap();
            let mut finished = false;
            for block in blocks {
                for item in &block.items {
                    write!(&mut out, "{}", element_2_rs(item, ctx, &[])).unwrap();
                }
                match block.termination {
                    None => {
                        writeln!(&mut out, "}}").unwrap();
                        finished = true;
                    }
                    Some((IfBodyTermination::ElseIf(ref expr), ref comment)) => {
                        writeln!(&mut out, "}} else if {} {{{}", expression_2_rs(expr, false), optional_comment(comment)).unwrap();
                    }
                    Some((IfBodyTermination::Else, ref comment)) => {
                        writeln!(&mut out, "}} else {{{}", optional_comment(comment)).unwrap();
                    }
                    Some((IfBodyTermination::End, ref comment)) => {
                        writeln!(&mut out, "}}{}", optional_comment(comment)).unwrap();
                        finished = true;
                    }
                }
            }
            if !finished {
                writeln!(&mut out, "}}").unwrap();
            }
        }
        IfStatement::Logical {
            ref condition,
            ref statement,
        } => {
            writeln!(&mut out, "if {} {{", expression_2_rs(condition, false)).unwrap();
            write!(&mut out, "{}", statement_2_rs(statement, ctx, &[])).unwrap();
            writeln!(&mut out, "}}").unwrap();
        }
    }

    String::from_utf8(out.into_inner().unwrap()).unwrap()
}
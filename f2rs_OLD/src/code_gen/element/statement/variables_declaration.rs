use std::io::{BufWriter, Write};

use crate::{code_gen::{optional_comment, Span, type_2_rs, ctx::{self, ContextType}}, parse::elements::{TypeWithModifiers, Modifiers, VariablesDeclaration}};

use super::expression::expression_2_rs;



pub fn variable_declaration_2_rs(
    variable_declaration: &VariablesDeclaration<Span>,
    ctx: &ctx::Ctx,
    exclude_variables: &[String],
) -> String {
    let mut out = BufWriter::new(Vec::new());

    if variable_declaration.comment.is_some() {
        writeln!(&mut out, "{}", optional_comment(&variable_declaration.comment)).unwrap();
    }
    for (type_with_modifiers, name, initialization) in &variable_declaration.vars {
        let TypeWithModifiers { ty, modifiers } = type_with_modifiers;
        let Modifiers { intent, parameter, optional, pointer, save, external } = modifiers;
        if *parameter {
            let initialization = initialization.as_ref().unwrap(); // TODO
            writeln!(&mut out, "#[allow(non_upper_case_globals)]").unwrap();
            writeln!(&mut out, "const {}: {} = {};", name, type_2_rs(ty, ctx).0, expression_2_rs(initialization, false)).unwrap();
        } else {
            match ctx.context_type() {
                ContextType::Code => {
                    if let Some(initialization) = initialization {
                        writeln!(&mut out, "static mut {}: {} = {};", name, type_2_rs(ty, ctx).0, expression_2_rs(initialization, false)).unwrap();
                    } else {
                        if *save {
                            writeln!(&mut out, "static mut {}: {} = {};", name, type_2_rs(ty, ctx).0, type_2_rs(ty, ctx).1).unwrap();
                        } else {
                            writeln!(&mut out, "let mut {}: {} = {};", name, type_2_rs(ty, ctx).0, type_2_rs(ty, ctx).1).unwrap();
                        }
                    }
                }
                ContextType::NonCode => {
                    let init = if let Some(initialization) = initialization {
                        expression_2_rs(initialization, false)
                    } else {
                        type_2_rs(ty, ctx).1
                    };
                    
                }
                ContextType::InsideTypeDefinition => {
                    if let Some(initialization) = initialization {
                        writeln!(&mut out, "pub {}: {} = {};", name, type_2_rs(ty, ctx).0, expression_2_rs(initialization, false)).unwrap();
                    } else {
                        writeln!(&mut out, "pub {}: {};", name, type_2_rs(ty, ctx).0).unwrap();
                    }
                }
            }
        }
    }

    String::from_utf8(out.into_inner().unwrap()).unwrap()
}
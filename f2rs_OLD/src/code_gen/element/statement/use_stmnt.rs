use crate::{parse::elements::UseStatement, code_gen::{Span, optional_comment}};



pub fn use_statement_2_rs(use_statement: &UseStatement<Span>) -> String {
    if use_statement.only.is_empty() {
        format!("use {}::*;{}", use_statement.module_name, optional_comment(&use_statement.comment))
    } else {
        let only = use_statement
            .only
            .iter()
            .map(|(only, alias)| if let Some(alias) = alias { format!("{} as {}", only, alias) } else { only.clone() })
            .collect::<Vec<_>>()
            .join(", ");
        format!("use {}::{{{}}};{}", use_statement.module_name, only, optional_comment(&use_statement.comment))
    }
}
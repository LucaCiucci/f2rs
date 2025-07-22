pub mod tokenizer;
pub mod parser;

#[macro_export] // TODO in f2rs-parser-common
macro_rules! s_rule {
    (
        $(
            $standard:ident rule $rule_name:literal $(# $rule_number:literal)? $(section $rule_section:literal)? $(: $($rule_text:literal)*)?,
        )*
    ) => {
        "**TODO**"
        //paste::paste!{
        //    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        //    #[allow(non_camel_case_types)]
        //    pub struct [<_$name>];
//
        //    $(#[$meta])*
        //    pub const $name: [<_$name>] = [<_$name>];
        //}
    };
}
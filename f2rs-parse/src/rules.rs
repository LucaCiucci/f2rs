mod utils; pub use utils::*;
mod tokens; pub use tokens::*;
mod literals; pub use literals::*;
mod types; pub use types::*;
mod constants; pub use constants::*;
mod operators; pub use operators::*;
mod expression; pub use expression::*;
mod attributes; pub use attributes::*;
mod statements; pub use statements::*;
mod procedures; pub use procedures::*;

use crate::{Cfg, Standard::*};
use enum_as_inner::EnumAsInner;
use f2rs_parser_combinator::prelude::*;
use f2rs_parse_derive::syntax_rule;

/// Contains some implementation reports for the standards.
pub mod implementation_status {
    #[allow(unused_imports)]
    use super::*;

    /// Implementation status for [`F18V007r1`].
    ///
    /// This is an automatically generated report.
    ///
    #[doc = include_str!("rules/report/rules-18-007r1.md")]
    #[allow(non_snake_case)]
    pub mod F18V007r1_status {}
}

#[allow(unused)]
fn test_configs() -> impl Iterator<Item = Cfg> {
    [
        Cfg::f2018(),
    ].into_iter()
}

#[syntax_rule(
    F18V007r1 rule "xyz-list" #401,
    F18V007r1 rule "xyz-name" #402,
    F18V007r1 rule "scalar-xyz" #403,
)]
pub fn ignored_example_rule<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ()> + 'a {
    |_| unimplemented!("not a real rule")
}
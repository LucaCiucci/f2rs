/*!
Tokens and tokenization utilities.

# References
- [J3/18-007r1 §6 "Lexical tokens and source form"](https://j3-fortran.org/doc/year/18/18-007r1.pdf#chapter.399)
*/

use enum_as_inner::EnumAsInner;
use f2rs_parser_combinator::prelude::*;

pub mod rules;
pub mod line;

pub use line::*;


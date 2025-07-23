use enum_as_inner::EnumAsInner;
use f2rs_parser_combinator::prelude::*;
use line::LexicalToken;

use crate::s_rule;
use super::*;

mod basic;
mod comment;
mod delim;
mod label;
mod literal;
mod op;

pub use basic::*;
pub use comment::*;
pub use delim::*;
pub use label::*;
pub use literal::*;
pub use op::*;

#[doc = s_rule!(
    F18V007r1 rule "lexical-token" section "6.2.1",
)]
pub fn lexical_token<S: TextSource>(source: S) -> PResult<LexicalToken<S::Span>, S> {
    alt!{
        for S =>
        non_complex_literal_constant.map(LexicalToken::LiteralConstant),
        name(false).map(LexicalToken::Name),
        delimiter.map(LexicalToken::Delimiter),
        defined_operator.map(LexicalToken::Operator),
        seq!((c1: SpecialCharacter::Equals, c2: SpecialCharacter::GreaterThan) => LexicalToken::Arrow(Arrow::new_spanned(S::Span::merge(c1.span, c2.span)))),
        seq!((c1: SpecialCharacter::DecimalPointOrPeriod, c2: SpecialCharacter::DecimalPointOrPeriod) => LexicalToken::DotDot(DotDot::new_spanned(S::Span::merge(c1.span, c2.span)))),
        SpecialCharacter::Comma.map(|c| LexicalToken::Comma(Comma::new_spanned(c.span))),
        SpecialCharacter::Equals.map(|c| LexicalToken::Equals(Equals::new_spanned(c.span))),
        SpecialCharacter::Colon.map(|c| LexicalToken::Colon(Colon::new_spanned(c.span))),
        SpecialCharacter::SemiColon.map(|c| LexicalToken::Semicolon(Semicolon::new_spanned(c.span))),
        SpecialCharacter::Percent.map(|c| LexicalToken::Percent(Percent::new_spanned(c.span))),
        SpecialCharacter::DecimalPointOrPeriod.map(|c| LexicalToken::Dot(Dot::new_spanned(c.span))),
        Char::any().map(LexicalToken::Error),
    }.parse(source)
}

// TODO ???
pub fn nl<S: TextSource>(source: S) -> PResult<(), S> {
    alt! {
        for S =>
        "\n\r".map(|_| ()),
        "\n".map(|_| ()),
        "\r".map(|_| ()),
    }
        .map(|_| ())
        .parse(source)
}

// TODO ???
/// End of line
pub fn eol<S: TextSource>(source: S) -> PResult<(), S> {
    alt! {
        for S =>
        StringMatch::exact("\r\n", true),
        StringMatch::exact("\n", true),
    }
    .map(|_| ())
    .or(eof())
    .map(|o| o.inner())
    .parse(source)
}

#[cfg(test)]
mod test {

    use crate::rule_test;
    use super::super::examples;

    use super::*;

    rule_test! {
        lexical_token() {
            assert!(lexical_token.parse(".").unwrap().0.is_dot());
            assert!(lexical_token.parse("ciao").unwrap().0.is_name());
            assert!(lexical_token.parse("42").unwrap().0.is_literal_constant());
            assert!(lexical_token.parse("'42'").unwrap().0.as_literal_constant().unwrap().is_char());
            assert!(lexical_token.parse(".true.").unwrap().0.as_literal_constant().unwrap().is_logical());
            assert_eq!(
                lexical_token
                    .parse(">=").unwrap()
                    .0.as_operator().unwrap()
                    .as_intrinsic_ex().unwrap()
                    .as_rel_op().unwrap()
                    .0.value,
                ">=",
            );
            assert_eq!(
                lexical_token
                    .parse("==").unwrap()
                    .0.as_operator().unwrap()
                    .as_intrinsic_ex().unwrap()
                    .as_rel_op().unwrap()
                    .0.value,
                "==",
            );
            
            assert!(lexical_token.parse(",").unwrap().0.is_comma());
            assert!(lexical_token.parse("=").unwrap().0.is_equals());
            assert!(lexical_token.parse("=>").unwrap().0.is_arrow());
            assert!(lexical_token.parse(":").unwrap().0.is_colon());
            assert!(lexical_token.parse(";").unwrap().0.is_semicolon());
            assert!(lexical_token.parse("..").unwrap().0.is_dot_dot());
            assert!(lexical_token.parse("%").unwrap().0.is_percent());

            examples(lexical_token, [
                "hello",
                "74e1_foo",
                "'HELLO'",
                // WARNING: standard also says "complex literal constant",
                // but this is a pretty bad mix of concepts from different levels
                // (here we are lexing/tokenizing)
                "+",
                "42",
                "(",
                ",",
                "=",
                "=>",
                ":",
                //"::",
                ";",
                "..",
                "%"
            ]);
        }
    }
}
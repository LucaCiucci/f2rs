use super::*;
use enum_as_inner::EnumAsInner;
use f2rs_parser_combinator::prelude::*;

#[derive(Debug, Clone)]
pub struct PowerOp<Span>(pub StringMatch<Span>);

#[syntax_rule(
    F18V007r1 rule "power-op" #1007,
)]
pub fn power_op<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = PowerOp<S::Span>> + 'a {
    StringMatch::exact("**", true).map(PowerOp)
}

#[derive(Debug, Clone)]
pub struct MultOp<Span>(pub StringMatch<Span>);

#[syntax_rule(
    F18V007r1 rule "mult-op" #1008,
)]
pub fn mult_op<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = MultOp<S::Span>> + 'a {
    alt!(
        StringMatch::exact("*", true),
        StringMatch::exact("/", true),
    ).map(MultOp)
}

#[derive(Debug, Clone)]
pub struct AddOp<Span>(pub StringMatch<Span>);

#[syntax_rule(
    F18V007r1 rule "add-op" #1009,
)]
pub fn add_op<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = AddOp<S::Span>> + 'a {
    alt!(
        StringMatch::exact("+", true),
        StringMatch::exact("-", true),
    ).map(AddOp)
}

#[derive(Debug, Clone)]
pub struct ConcatOp<Span>(pub StringMatch<Span>);

#[syntax_rule(
    F18V007r1 rule "concat-op" #1011,
)]
pub fn concat_op<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ConcatOp<S::Span>> + 'a {
    StringMatch::exact("//", true).map(ConcatOp)
}

#[derive(Debug, Clone)]
pub struct RelOp<Span>(pub StringMatch<Span>);

#[syntax_rule(
    F18V007r1 rule "rel-op" #1013,
)]
pub fn rel_op<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = RelOp<S::Span>> + 'a {
    // NOTE: The order of the alternatives is important, is different from the standard
    alt!(
        StringMatch::exact(".eq.", false),
        StringMatch::exact(".ne.", false),
        StringMatch::exact(".lt.", false),
        StringMatch::exact(".le.", false),
        StringMatch::exact(".gt.", false),
        StringMatch::exact(".ge.", false),
        StringMatch::exact("==", false),
        StringMatch::exact("/=", false),
        StringMatch::exact("<=", false),
        StringMatch::exact(">=", false),
        StringMatch::exact("<", false),
        StringMatch::exact(">", false),
    ).map(RelOp)
}

#[derive(Debug, Clone)]
pub struct NotOp<Span>(pub StringMatch<Span>);

#[syntax_rule(
    F18V007r1 rule "not-op" #1018,
)]
pub fn not_op<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = NotOp<S::Span>> + 'a {
    StringMatch::exact(".not.", false).map(NotOp)
}

#[derive(Debug, Clone)]
pub struct AndOp<Span>(pub StringMatch<Span>);

#[syntax_rule(
    F18V007r1 rule "and-op" #1019,
)]
pub fn and_op<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = AndOp<S::Span>> + 'a {
    StringMatch::exact(".and.", false).map(AndOp)
}

#[derive(Debug, Clone)]
pub struct OrOp<Span>(pub StringMatch<Span>);

#[syntax_rule(
    F18V007r1 rule "or-op" #1020,
)]
pub fn or_op<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = OrOp<S::Span>> + 'a {
    StringMatch::exact(".or.", false).map(OrOp)
}

#[derive(Debug, Clone)]
pub struct EquivOp<Span>(pub StringMatch<Span>);

#[syntax_rule(
    F18V007r1 rule "equiv-op" #1021,
)]
pub fn equiv_op<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = EquivOp<S::Span>> + 'a {
    alt!(
        StringMatch::exact(".eqv.", false),
        StringMatch::exact(".neqv.", false),
    ).map(EquivOp)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum IntrinsicOperator<Span> {
    PowerOp(PowerOp<Span>),
    MultOp(MultOp<Span>),
    AddOp(AddOp<Span>),
    ConcatOp(ConcatOp<Span>),
    RelOp(RelOp<Span>),
    NotOp(NotOp<Span>),
    AndOp(AndOp<Span>),
    OrOp(OrOp<Span>),
    EquivOp(EquivOp<Span>),
}

#[syntax_rule(
    F18V007r1 rule "intrinsic-operator" #608,
)]
pub fn intrinsic_operator<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = IntrinsicOperator<S::Span>> + 'a {
    // NOTE: The order of the alternatives is important, is different from the standard
    alt!(
        power_op(cfg).map(IntrinsicOperator::PowerOp),
        rel_op(cfg).map(IntrinsicOperator::RelOp),
        concat_op(cfg).map(IntrinsicOperator::ConcatOp),
        mult_op(cfg).map(IntrinsicOperator::MultOp),
        add_op(cfg).map(IntrinsicOperator::AddOp),
        not_op(cfg).map(IntrinsicOperator::NotOp),
        and_op(cfg).map(IntrinsicOperator::AndOp),
        or_op(cfg).map(IntrinsicOperator::OrOp),
        equiv_op(cfg).map(IntrinsicOperator::EquivOp),
    )
}

// TODO test?
#[syntax_rule(
    F18V007r1 rule "extended-intrinsic-op" #610,
)]
pub fn extended_intrinsic_op<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = IntrinsicOperator<S::Span>> + 'a {
    intrinsic_operator(cfg)
}

#[derive(Debug, Clone)]
pub struct DefinedUnaryOrBinaryOp<Span> {
    pub m: StringMatch<Span>,
}

#[syntax_rule(
    F18V007r1 rule "defined-unary-op" #1003,
)]
pub fn defined_unary_op<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DefinedUnaryOrBinaryOp<S::Span>> + 'a {
    (
        '.',
        fold_many(
            letter(cfg),
            || StringMatch::empty::<S>(),
            |mut m, l| {
                m.push_char::<S>(l);
                (m, true)
            },
            1..,
        ),
        '.',
    ).map(|(o, mut m, c)| {
        m.push_front_char::<S>(o);
        m.push_char::<S>(c);
        m
    })
    .map(|m| DefinedUnaryOrBinaryOp { m })
}

#[syntax_rule(
    F18V007r1 rule "defined-binary-op" #1023,
)]
pub fn defined_binary_op<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DefinedUnaryOrBinaryOp<S::Span>> + 'a {
    (
        '.',
        fold_many(
            letter(cfg),
            || StringMatch::empty::<S>(),
            |mut m, l| {
                m.push_char::<S>(l);
                (m, true)
            },
            1..,
        ),
        '.',
    ).map(|(o, mut m, c)| {
        m.push_front_char::<S>(o);
        m.push_char::<S>(c);
        m
    })
    .map(|m| DefinedUnaryOrBinaryOp { m })
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum DefinedOperator<Span> {
    DefinedUnaryOrBinary(DefinedUnaryOrBinaryOp<Span>),
    IntrinsicEx(IntrinsicOperator<Span>),
}

#[syntax_rule(
    F18V007r1 rule "defined-operator" #609,
)]
pub fn defined_operator<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DefinedOperator<S::Span>> + 'a {
    alt!(
        extended_intrinsic_op(cfg).map(DefinedOperator::IntrinsicEx),
        defined_unary_op(cfg).map(DefinedOperator::DefinedUnaryOrBinary),
        defined_binary_op(cfg).map(DefinedOperator::DefinedUnaryOrBinary),
    )
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_power_op() {
        for cfg in test_configs() {
            let parser = power_op(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parses("*"), false);
            assert_eq!(parser.parses("**"), true);
        }
    }

    #[test]
    fn test_mult_op() {
        for cfg in test_configs() {
            let parser = mult_op(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parses("*"), true);
            assert_eq!(parser.parses("/"), true);
            assert_eq!(parser.parses("**"), true); // TODO maybe false??
            assert_eq!(parser.parses("//"), true); // TODO maybe false??
        }
    }

    #[test]
    fn test_add_op() {
        for cfg in test_configs() {
            let parser = add_op(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parses("+"), true);
            assert_eq!(parser.parses("-"), true);
            assert_eq!(parser.parses("**"), false);
        }
    }

    #[test]
    fn test_concat_op() {
        for cfg in test_configs() {
            let parser = concat_op(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parses("//"), true);
            assert_eq!(parser.parses("**"), false);
        }
    }

    #[test]
    fn test_rel_op() {
        for cfg in test_configs() {
            let parser = rel_op(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parse(".eq. ").unwrap().0.0.value(), ".eq.");
            assert_eq!(parser.parse(".ne. ").unwrap().0.0.value(), ".ne.");
            assert_eq!(parser.parse(".lt. ").unwrap().0.0.value(), ".lt.");
            assert_eq!(parser.parse(".le. ").unwrap().0.0.value(), ".le.");
            assert_eq!(parser.parse(".gt. ").unwrap().0.0.value(), ".gt.");
            assert_eq!(parser.parse(".ge. ").unwrap().0.0.value(), ".ge.");
            assert_eq!(parser.parse("== ").unwrap().0.0.value(), "==");
            assert_eq!(parser.parse("/= ").unwrap().0.0.value(), "/=");
            assert_eq!(parser.parse("<= ").unwrap().0.0.value(), "<=");
            assert_eq!(parser.parse(">= ").unwrap().0.0.value(), ">=");
            assert_eq!(parser.parse("< ").unwrap().0.0.value(), "<");
            assert_eq!(parser.parse("> ").unwrap().0.0.value(), ">");
            assert_eq!(parser.parses("**"), false);
        }
    }

    #[test]
    fn test_not_op() {
        for cfg in test_configs() {
            let parser = not_op(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parse(".not. ").unwrap().0.0.value(), ".not.");
            assert_eq!(parser.parses("**"), false);
        }
    } 

    #[test]
    fn test_and_op() {
        for cfg in test_configs() {
            let parser = and_op(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parse(".and. ").unwrap().0.0.value(), ".and.");
            assert_eq!(parser.parses("**"), false);
        }
    }

    #[test]
    fn test_or_op() {
        for cfg in test_configs() {
            let parser = or_op(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parse(".or. ").unwrap().0.0.value(), ".or.");
            assert_eq!(parser.parses("**"), false);
        }
    }

    #[test]
    fn test_equiv_op() {
        for cfg in test_configs() {
            let parser = equiv_op(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parse(".eqv. ").unwrap().0.0.value(), ".eqv.");
            assert_eq!(parser.parse(".neqv. ").unwrap().0.0.value(), ".neqv.");
            assert_eq!(parser.parses("**"), false);
        }
    }

    #[test]
    fn test_intrinsic_operator() {
        for cfg in test_configs() {
            let parser = intrinsic_operator(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parse(".eqv. ").unwrap().0.is_equiv_op(), true);
            assert_eq!(parser.parse(".neqv. ").unwrap().0.is_equiv_op(), true);
            assert_eq!(parser.parse(".eq. ").unwrap().0.is_rel_op(), true);
            assert_eq!(parser.parse(".ne. ").unwrap().0.is_rel_op(), true);
            assert_eq!(parser.parse(".lt. ").unwrap().0.is_rel_op(), true);
            assert_eq!(parser.parse(".le. ").unwrap().0.is_rel_op(), true);
            assert_eq!(parser.parse(".gt. ").unwrap().0.is_rel_op(), true);
            assert_eq!(parser.parse(".ge. ").unwrap().0.is_rel_op(), true);
            assert_eq!(parser.parse("== ").unwrap().0.is_rel_op(), true);
            assert_eq!(parser.parse("/= ").unwrap().0.is_rel_op(), true);
            assert_eq!(parser.parse("<= ").unwrap().0.is_rel_op(), true);
            assert_eq!(parser.parse(">= ").unwrap().0.is_rel_op(), true);
            assert_eq!(parser.parse("< ").unwrap().0.is_rel_op(), true);
            assert_eq!(parser.parse("> ").unwrap().0.is_rel_op(), true);
            assert_eq!(parser.parse(".not. ").unwrap().0.is_not_op(), true);
            assert_eq!(parser.parse(".and. ").unwrap().0.is_and_op(), true);
            assert_eq!(parser.parse(".or. ").unwrap().0.is_or_op(), true);
            assert_eq!(parser.parse("**").unwrap().0.is_power_op(), true);
            assert_eq!(parser.parse("*").unwrap().0.is_mult_op(), true);
            assert_eq!(parser.parse("/").unwrap().0.is_mult_op(), true);
            assert_eq!(parser.parse("+").unwrap().0.is_add_op(), true);
            assert_eq!(parser.parse("-").unwrap().0.is_add_op(), true);
            assert_eq!(parser.parse("//").unwrap().0.is_concat_op(), true);
        }
    }

    #[test]
    fn test_defined_unary_op() {
        for cfg in test_configs() {
            let parser = defined_unary_op(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parse(".foo.").unwrap().0.m.value(), ".foo.");
            assert_eq!(parser.parses(".foo"), false);
            assert_eq!(parser.parse(".foo.bar.").unwrap().0.m.value(), ".foo.");
        }
    }

    #[test]
    fn test_defined_binary_op() {
        for cfg in test_configs() {
            let parser = defined_binary_op(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parse(".foo.").unwrap().0.m.value(), ".foo.");
            assert_eq!(parser.parses(".foo"), false);
            assert_eq!(parser.parse(".foo.bar.").unwrap().0.m.value(), ".foo.");
        }
    }

    #[test]
    fn test_defined_operator() {
        for cfg in test_configs() {
            let parser = defined_operator(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parse(".foo.").unwrap().0.is_defined_unary_or_binary(), true);
            assert_eq!(parser.parse(".eqv. ").unwrap().0.is_intrinsic_ex(), true);
            assert_eq!(parser.parse("+ ").unwrap().0.is_intrinsic_ex(), true);
        }
    }
}
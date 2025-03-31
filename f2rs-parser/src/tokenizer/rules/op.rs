use super::*;

#[derive(Debug, Clone)]
pub struct PowerOp<Span>(pub StringMatch<Span>);

impl<Span> ToString for PowerOp<Span> {
    fn to_string(&self) -> String {
        self.0.value.clone()
    }
}

impl<Span> MapSpan<Span> for PowerOp<Span> {
    type Spanned<T> = PowerOp<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        PowerOp(self.0.map_span(f))
    }
}

#[doc = s_rule!(
    F18V007r1 rule "power-op" #1007 : "is **",
)]
pub fn power_op<S: TextSource>(source: S) -> PResult<PowerOp<S::Span>, S> {
    StringMatch::exact("**", true).map(PowerOp).parse(source)
}

#[derive(Debug, Clone)]
pub struct MultOp<Span>(pub StringMatch<Span>);

impl<Span> ToString for MultOp<Span> {
    fn to_string(&self) -> String {
        self.0.value.clone()
    }
}

impl<Span> MapSpan<Span> for MultOp<Span> {
    type Spanned<T> = MultOp<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        MultOp(self.0.map_span(f))
    }
}

#[doc = s_rule!(
    F18V007r1 rule "mult-op" #1008 :
    "is *"
    "or /",
)]
pub fn mult_op<S: TextSource>(source: S) -> PResult<MultOp<S::Span>, S> {
    alt!(
        for S =>
        StringMatch::exact("*", true),
        StringMatch::exact("/", true),
    ).map(MultOp).parse(source)
}

#[derive(Debug, Clone)]
pub struct AddOp<Span>(pub StringMatch<Span>);

impl<Span> ToString for AddOp<Span> {
    fn to_string(&self) -> String {
        self.0.value.clone()
    }
}

impl<Span> MapSpan<Span> for AddOp<Span> {
    type Spanned<T> = AddOp<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        AddOp(self.0.map_span(f))
    }
}

#[doc = s_rule!(
    F18V007r1 rule "add-op" #1009 :
    "is +"
    "or -",
)]
pub fn add_op<S: TextSource>(source: S) -> PResult<AddOp<S::Span>, S> {
    alt!(
        for S =>
        StringMatch::exact("+", true),
        StringMatch::exact("-", true),
    ).map(AddOp)
    .parse(source)
}

#[derive(Debug, Clone)]
pub struct ConcatOp<Span>(pub StringMatch<Span>);

impl<Span> ToString for ConcatOp<Span> {
    fn to_string(&self) -> String {
        self.0.value.clone()
    }
}

impl<Span> MapSpan<Span> for ConcatOp<Span> {
    type Spanned<T> = ConcatOp<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        ConcatOp(self.0.map_span(f))
    }
}

#[doc = s_rule!(
    F18V007r1 rule "concat-op" #1011 : "is //",
)]
pub fn concat_op<S: TextSource>(source: S) -> PResult<ConcatOp<S::Span>, S> {
    StringMatch::exact("//", true).map(ConcatOp).parse(source)
}

#[derive(Debug, Clone)]
pub struct RelOp<Span>(pub StringMatch<Span>);

impl<Span> ToString for RelOp<Span> {
    fn to_string(&self) -> String {
        self.0.value.clone()
    }
}

impl<Span> MapSpan<Span> for RelOp<Span> {
    type Spanned<T> = RelOp<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        RelOp(self.0.map_span(f))
    }
}

#[doc = s_rule!(
    F18V007r1 rule "rel-op" #1013 :
    "is .EQ."
    "or .NE."
    "or .LT."
    "or .LE."
    "or .GT."
    "or .GE."
    "or =="
    "or /="
    "or <"
    "or <="
    "or >"
    "or >=",
)]
pub fn rel_op<S: TextSource>(source: S) -> PResult<RelOp<S::Span>, S> {
    // NOTE: The order of the alternatives is important, is different from the standard
    alt!(
        for S =>
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
    .parse(source)
}

#[derive(Debug, Clone)]
pub struct NotOp<Span>(pub StringMatch<Span>);

impl<Span> ToString for NotOp<Span> {
    fn to_string(&self) -> String {
        self.0.value.clone()
    }
}

impl<Span> MapSpan<Span> for NotOp<Span> {
    type Spanned<T> = NotOp<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        NotOp(self.0.map_span(f))
    }
}

#[doc = s_rule!(
    F18V007r1 rule "not-op" #1018 : "is .NOT.",
)]
pub fn not_op<S: TextSource>(source: S) -> PResult<NotOp<S::Span>, S> {
    StringMatch::exact(".not.", false).map(NotOp).parse(source)
}

#[derive(Debug, Clone)]
pub struct AndOp<Span>(pub StringMatch<Span>);

impl<Span> ToString for AndOp<Span> {
    fn to_string(&self) -> String {
        self.0.value.clone()
    }
}

impl<Span> MapSpan<Span> for AndOp<Span> {
    type Spanned<T> = AndOp<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        AndOp(self.0.map_span(f))
    }
}

#[doc = s_rule!(
    F18V007r1 rule "and-op" #1019 : "is .AND.",
)]
pub fn and_op<S: TextSource>(source: S) -> PResult<AndOp<S::Span>, S> {
    StringMatch::exact(".and.", false).map(AndOp).parse(source)
}

#[derive(Debug, Clone)]
pub struct OrOp<Span>(pub StringMatch<Span>);

impl<Span> ToString for OrOp<Span> {
    fn to_string(&self) -> String {
        self.0.value.clone()
    }
}

impl<Span> MapSpan<Span> for OrOp<Span> {
    type Spanned<T> = OrOp<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        OrOp(self.0.map_span(f))
    }
}

#[doc = s_rule!(
    F18V007r1 rule "or-op" #1020 : "is .OR.",
)]
pub fn or_op<S: TextSource>(source: S) -> PResult<OrOp<S::Span>, S> {
    StringMatch::exact(".or.", false).map(OrOp).parse(source)
}

#[derive(Debug, Clone)]
pub struct EquivOp<Span>(pub StringMatch<Span>);

impl<Span> ToString for EquivOp<Span> {
    fn to_string(&self) -> String {
        self.0.value.clone()
    }
}

impl<Span> MapSpan<Span> for EquivOp<Span> {
    type Spanned<T> = EquivOp<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        EquivOp(self.0.map_span(f))
    }
}

#[doc = s_rule!(
    F18V007r1 rule "equiv-op" #1021 :
    "is .EQV."
    "or .NEQV.",
)]
pub fn equiv_op<S: TextSource>(source: S) -> PResult<EquivOp<S::Span>, S> {
    alt!(
        for S =>
        StringMatch::exact(".eqv.", false),
        StringMatch::exact(".neqv.", false),
    ).map(EquivOp)
    .parse(source)
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

impl<Span> IntrinsicOperator<Span> {
    pub fn span(&self) -> &Span {
        match self {
            Self::PowerOp(op) => &op.0.span,
            Self::MultOp(op) => &op.0.span,
            Self::AddOp(op) => &op.0.span,
            Self::ConcatOp(op) => &op.0.span,
            Self::RelOp(op) => &op.0.span,
            Self::NotOp(op) => &op.0.span,
            Self::AndOp(op) => &op.0.span,
            Self::OrOp(op) => &op.0.span,
            Self::EquivOp(op) => &op.0.span,
        }
    }
}

impl<Span> ToString for IntrinsicOperator<Span> {
    fn to_string(&self) -> String {
        match self {
            IntrinsicOperator::PowerOp(op) => op.to_string(),
            IntrinsicOperator::MultOp(op) => op.to_string(),
            IntrinsicOperator::AddOp(op) => op.to_string(),
            IntrinsicOperator::ConcatOp(op) => op.to_string(),
            IntrinsicOperator::RelOp(op) => op.to_string(),
            IntrinsicOperator::NotOp(op) => op.to_string(),
            IntrinsicOperator::AndOp(op) => op.to_string(),
            IntrinsicOperator::OrOp(op) => op.to_string(),
            IntrinsicOperator::EquivOp(op) => op.to_string(),
        }
    }
}

impl<Span> MapSpan<Span> for IntrinsicOperator<Span> {
    type Spanned<T> = IntrinsicOperator<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        match self {
            IntrinsicOperator::PowerOp(op) => IntrinsicOperator::PowerOp(op.map_span(f)),
            IntrinsicOperator::MultOp(op) => IntrinsicOperator::MultOp(op.map_span(f)),
            IntrinsicOperator::AddOp(op) => IntrinsicOperator::AddOp(op.map_span(f)),
            IntrinsicOperator::ConcatOp(op) => IntrinsicOperator::ConcatOp(op.map_span(f)),
            IntrinsicOperator::RelOp(op) => IntrinsicOperator::RelOp(op.map_span(f)),
            IntrinsicOperator::NotOp(op) => IntrinsicOperator::NotOp(op.map_span(f)),
            IntrinsicOperator::AndOp(op) => IntrinsicOperator::AndOp(op.map_span(f)),
            IntrinsicOperator::OrOp(op) => IntrinsicOperator::OrOp(op.map_span(f)),
            IntrinsicOperator::EquivOp(op) => IntrinsicOperator::EquivOp(op.map_span(f)),
        }
    }
}

#[doc = s_rule!(
    F18V007r1 rule "intrinsic-operator" #608 :
    "is power-op"
    "or mult-op"
    "or add-op"
    "or concat-op"
    "or rel-op"
    "or not-op"
    "or and-op"
    "or or-op"
    "or equiv-op",
)]
pub fn intrinsic_operator<S: TextSource>(source: S) -> PResult<IntrinsicOperator<S::Span>, S> {
    // NOTE: The order of the alternatives is important, is different from the standard
    alt!(
        for S =>
        power_op.map(IntrinsicOperator::PowerOp),
        rel_op.map(IntrinsicOperator::RelOp),
        concat_op.map(IntrinsicOperator::ConcatOp),
        mult_op.map(IntrinsicOperator::MultOp),
        add_op.map(IntrinsicOperator::AddOp),
        not_op.map(IntrinsicOperator::NotOp),
        and_op.map(IntrinsicOperator::AndOp),
        or_op.map(IntrinsicOperator::OrOp),
        equiv_op.map(IntrinsicOperator::EquivOp),
    ).parse(source)
}

// TODO test?
#[doc = s_rule!(
    F18V007r1 rule "extended-intrinsic-op" #610 : "is intrinsic-operator",
)]
pub fn extended_intrinsic_op<S: TextSource>(source: S) -> PResult<IntrinsicOperator<S::Span>, S> {
    intrinsic_operator.parse(source)
}

//#[derive(Debug, Clone)]
//pub struct Label<Span> {
//    pub digits: StringMatch<Span>,
//    pub value: u32,
//}
//
//impl<Span> Spanned<Span> for Label<Span> {
//    fn span(&self) -> &Span {
//        &self.digits.span
//    }
//}
//
//impl<Span> Spannable<Span> for Label<Span> {
//    type Spanned<T> = Label<T>;
//
//    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
//        Label {
//            digits: self.digits.map_span(f),
//            value: self.value,
//        }
//    }
//}
//
//#[doc = s_rule!(
//    F18V007r1 rule "label" #611 : "is digit [ digit [ digit [ digit [ digit ] ] ] ]",
//)]
//pub fn label<S: TextSource>(source: S) -> PResult<Label<S::Span>, S> {
//    digit
//        .then(|first| fold_many(
//            digit,
//            move || StringMatch::from_char(first.clone()),
//            |mut string, digit| {
//                string.push_char(digit);
//                (string, true)
//            },
//            0..=4,
//        ))
//        .map(|digits| {
//            let value = digits.value.parse::<u32>().unwrap();
//            Label { digits, value }
//        })
//}

#[derive(Debug, Clone)]
pub struct DefinedUnaryOrBinaryOp<Span> {
    pub m: StringMatch<Span>,
}

impl<Span> MapSpan<Span> for DefinedUnaryOrBinaryOp<Span> {
    type Spanned<T> = DefinedUnaryOrBinaryOp<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        DefinedUnaryOrBinaryOp {
            m: self.m.map_span(f),
        }
    }
}

#[doc = s_rule!(
    F18V007r1 rule "defined-unary-op" #1003 : "is . letter [ letter ] ... .",
)]
pub fn defined_unary_op<S: TextSource>(source: S) -> PResult<DefinedUnaryOrBinaryOp<S::Span>, S> {
    (
        '.',
        fold_many(
            letter,
            || StringMatch::empty(),
            |mut m, l| {
                m.push_char(l);
                (m, true)
            },
            1..,
        ),
        '.',
    ).map(|(o, mut m, c)| {
        m.push_front_char(o);
        m.push_char(c);
        m
    })
    .map(|m| DefinedUnaryOrBinaryOp { m })
    .parse(source)
}

#[doc = s_rule!(
    F18V007r1 rule "defined-binary-op" #1023 : "is . letter [ letter ] ... .",
)]
pub fn defined_binary_op<S: TextSource>(source: S) -> PResult<DefinedUnaryOrBinaryOp<S::Span>, S> {
    (
        '.',
        fold_many(
            letter,
            || StringMatch::empty(),
            |mut m, l| {
                m.push_char(l);
                (m, true)
            },
            1..,
        ),
        '.',
    ).map(|(o, mut m, c)| {
        m.push_front_char(o);
        m.push_char(c);
        m
    })
    .map(|m| DefinedUnaryOrBinaryOp { m })
    .parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum DefinedOperator<Span> {
    DefinedUnaryOrBinary(DefinedUnaryOrBinaryOp<Span>),
    IntrinsicEx(IntrinsicOperator<Span>),
}

impl<Span> DefinedOperator<Span> {
    pub fn span(&self) -> &Span {
        match self {
            Self::DefinedUnaryOrBinary(op) => &op.m.span,
            Self::IntrinsicEx(op) => op.span(),
        }
    }
}

impl<Span> ToString for DefinedOperator<Span> {
    fn to_string(&self) -> String {
        match self {
            DefinedOperator::DefinedUnaryOrBinary(op) => format!(".{}.", op.m.value),
            DefinedOperator::IntrinsicEx(op) => op.to_string(),
        }
    }
}

impl<Span> MapSpan<Span> for DefinedOperator<Span> {
    type Spanned<T> = DefinedOperator<T>;

    fn map_span<S>(self, f: &impl Fn(Span) -> S) -> Self::Spanned<S> {
        match self {
            DefinedOperator::DefinedUnaryOrBinary(op) => DefinedOperator::DefinedUnaryOrBinary(op.map_span(f)),
            DefinedOperator::IntrinsicEx(op) => DefinedOperator::IntrinsicEx(op.map_span(f)),
        }
    }
}

#[doc = s_rule!(
    F18V007r1 rule "defined-operator" #609 :
    "is defined-unary-op"
    "or defined-binary-op"
    "or extended-intrinsic-op",
)]
pub fn defined_operator<S: TextSource>(source: S) -> PResult<DefinedOperator<S::Span>, S> {
    alt!(
        for S =>
        extended_intrinsic_op.map(DefinedOperator::IntrinsicEx),
        defined_unary_op.map(DefinedOperator::DefinedUnaryOrBinary),
        defined_binary_op.map(DefinedOperator::DefinedUnaryOrBinary),
    ).parse(source)
}

#[cfg(test)]
mod test {

    use crate::rule_test;
    //use super::super::examples;

    use super::*;

    rule_test! {
        power_op(F18V007r1 1007) {
            assert_eq!(power_op.parses(""), false);
            assert_eq!(power_op.parses("*"), false);
            assert_eq!(power_op.parses("**"), true);
        }
    }

    rule_test! {
        mult_op(F18V007r1 1008) {
            assert_eq!(mult_op.parses(""), false);
            assert_eq!(mult_op.parses("*"), true);
            assert_eq!(mult_op.parses("/"), true);
            assert_eq!(mult_op.parses("**"), true); // TODO maybe false??
            assert_eq!(mult_op.parses("//"), true); // TODO maybe false??
        }
    }

    rule_test! {
        add_op(F18V007r1 1009) {
            assert_eq!(add_op.parses(""), false);
            assert_eq!(add_op.parses("+"), true);
            assert_eq!(add_op.parses("-"), true);
            assert_eq!(add_op.parses("**"), false);
        }
    }

    rule_test! {
        concat_op(F18V007r1 1011) {
            assert_eq!(concat_op.parses(""), false);
            assert_eq!(concat_op.parses("//"), true);
            assert_eq!(concat_op.parses("**"), false);
        }
    }

    rule_test! {
        rel_op(F18V007r1 1013) {
            assert_eq!(rel_op.parses(""), false);
            assert_eq!(rel_op.parse(".eq. ").unwrap().0.0.value(), ".eq.");
            assert_eq!(rel_op.parse(".ne. ").unwrap().0.0.value(), ".ne.");
            assert_eq!(rel_op.parse(".lt. ").unwrap().0.0.value(), ".lt.");
            assert_eq!(rel_op.parse(".le. ").unwrap().0.0.value(), ".le.");
            assert_eq!(rel_op.parse(".gt. ").unwrap().0.0.value(), ".gt.");
            assert_eq!(rel_op.parse(".ge. ").unwrap().0.0.value(), ".ge.");
            assert_eq!(rel_op.parse("== ").unwrap().0.0.value(), "==");
            assert_eq!(rel_op.parse("/= ").unwrap().0.0.value(), "/=");
            assert_eq!(rel_op.parse("<= ").unwrap().0.0.value(), "<=");
            assert_eq!(rel_op.parse(">= ").unwrap().0.0.value(), ">=");
            assert_eq!(rel_op.parse("< ").unwrap().0.0.value(), "<");
            assert_eq!(rel_op.parse("> ").unwrap().0.0.value(), ">");
            assert_eq!(rel_op.parses("**"), false);
        }
    }

    rule_test! {
        not_op(F18V007r1 1018) {
            assert_eq!(not_op.parses(""), false);
            assert_eq!(not_op.parse(".not. ").unwrap().0.0.value(), ".not.");
            assert_eq!(not_op.parses("**"), false);
        }
    }

    rule_test! {
        and_op(F18V007r1 1019) {
            assert_eq!(and_op.parses(""), false);
            assert_eq!(and_op.parse(".and. ").unwrap().0.0.value(), ".and.");
            assert_eq!(and_op.parses("**"), false);
        }
    }

    rule_test! {
        or_op(F18V007r1 1020) {
            assert_eq!(or_op.parses(""), false);
            assert_eq!(or_op.parse(".or. ").unwrap().0.0.value(), ".or.");
            assert_eq!(or_op.parses("**"), false);
        }
    }

    rule_test! {
        equiv_op(F18V007r1 1021) {
            assert_eq!(equiv_op.parses(""), false);
            assert_eq!(equiv_op.parse(".eqv. ").unwrap().0.0.value(), ".eqv.");
            assert_eq!(equiv_op.parse(".neqv. ").unwrap().0.0.value(), ".neqv.");
            assert_eq!(equiv_op.parses("**"), false);
        }
    }

    rule_test! {
        intrinsic_operator(F18V007r1 608, F18V007r1 610) {
            assert_eq!(intrinsic_operator.parses(""), false);
            assert_eq!(intrinsic_operator.parse(".eqv. ").unwrap().0.is_equiv_op(), true);
            assert_eq!(intrinsic_operator.parse(".neqv. ").unwrap().0.is_equiv_op(), true);
            assert_eq!(intrinsic_operator.parse(".eq. ").unwrap().0.is_rel_op(), true);
            assert_eq!(intrinsic_operator.parse(".ne. ").unwrap().0.is_rel_op(), true);
            assert_eq!(intrinsic_operator.parse(".lt. ").unwrap().0.is_rel_op(), true);
            assert_eq!(intrinsic_operator.parse(".le. ").unwrap().0.is_rel_op(), true);
            assert_eq!(intrinsic_operator.parse(".gt. ").unwrap().0.is_rel_op(), true);
            assert_eq!(intrinsic_operator.parse(".ge. ").unwrap().0.is_rel_op(), true);
            assert_eq!(intrinsic_operator.parse("== ").unwrap().0.is_rel_op(), true);
            assert_eq!(intrinsic_operator.parse("/= ").unwrap().0.is_rel_op(), true);
            assert_eq!(intrinsic_operator.parse("<= ").unwrap().0.is_rel_op(), true);
            assert_eq!(intrinsic_operator.parse(">= ").unwrap().0.is_rel_op(), true);
            assert_eq!(intrinsic_operator.parse("< ").unwrap().0.is_rel_op(), true);
            assert_eq!(intrinsic_operator.parse("> ").unwrap().0.is_rel_op(), true);
            assert_eq!(intrinsic_operator.parse(".not. ").unwrap().0.is_not_op(), true);
            assert_eq!(intrinsic_operator.parse(".and. ").unwrap().0.is_and_op(), true);
            assert_eq!(intrinsic_operator.parse(".or. ").unwrap().0.is_or_op(), true);
            assert_eq!(intrinsic_operator.parse("**").unwrap().0.is_power_op(), true);
            assert_eq!(intrinsic_operator.parse("*").unwrap().0.is_mult_op(), true);
            assert_eq!(intrinsic_operator.parse("/").unwrap().0.is_mult_op(), true);
            assert_eq!(intrinsic_operator.parse("+").unwrap().0.is_add_op(), true);
            assert_eq!(intrinsic_operator.parse("-").unwrap().0.is_add_op(), true);
            assert_eq!(intrinsic_operator.parse("//").unwrap().0.is_concat_op(), true);
        }
    }

    rule_test! {
        defined_unary_op(F18V007r1 1003) {
            assert_eq!(defined_unary_op.parses(""), false);
            assert_eq!(defined_unary_op.parse(".foo.").unwrap().0.m.value(), ".foo.");
            assert_eq!(defined_unary_op.parses(".foo"), false);
            assert_eq!(defined_unary_op.parse(".foo.bar.").unwrap().0.m.value(), ".foo.");
        }
    }

    rule_test! {
        defined_binary_op(F18V007r1 1023) {
            assert_eq!(defined_binary_op.parses(""), false);
            assert_eq!(defined_binary_op.parse(".foo.").unwrap().0.m.value(), ".foo.");
            assert_eq!(defined_binary_op.parses(".foo"), false);
            assert_eq!(defined_binary_op.parse(".foo.bar.").unwrap().0.m.value(), ".foo.");
        }
    }

    rule_test! {
        defined_operator(F18V007r1 609) {
            assert_eq!(defined_operator.parses(""), false);
            assert_eq!(defined_operator.parse(".foo.").unwrap().0.is_defined_unary_or_binary(), true);
            assert_eq!(defined_operator.parse(".eqv. ").unwrap().0.is_intrinsic_ex(), true);
            assert_eq!(defined_operator.parse("+ ").unwrap().0.is_intrinsic_ex(), true);
        }
    }
}
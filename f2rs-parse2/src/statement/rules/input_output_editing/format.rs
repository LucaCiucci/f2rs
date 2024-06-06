
use super::*;

#[derive(Debug, Clone)]
pub struct FormatStmt<Span> {
    pub format_specification: FormatSpecification<Span>,
}

#[syntax_rule(
    F18V007r1 rule "format-stmt" #1301 : "is FORMAT format-specification",
)]
pub fn format_stmt_2<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = FormatStmt<MultilineSpan>> + 'a {
    (
        kw!(format),
        format_specification(cfg),
    ).map(|(_, format_specification)| FormatStmt {
        format_specification,
    })
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum FormatSpecification<Span> {
    FormatItems {
        format_items: FormatItems<Span>,
    },
    UnlimitedFormatItem {
        format_items: Option<FormatItems<Span>>,
        unlimited_format_item: UnlimitedFormatItem<Span>,
    },
}

#[syntax_rule(
    F18V007r1 rule "format-specification" #1302 :
    "is ( [ format-items ] )"
    "or ( [ format-items, ] unlimited-format-item )",
)]
pub fn format_specification<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = FormatSpecification<MultilineSpan>> + 'a {
    alt!(
        (
            delim('('),
            format_items(cfg).optional(),
            delim(')'),
        ).map(|(_, format_items, _)| FormatSpecification::FormatItems {
            format_items: format_items.unwrap_or_else(|| FormatItems(Vec::new())),
        }),
        (
            delim('('),
            (
                format_items(cfg),
                comma(),
            ).map(|(format_items, _)| format_items),
            unlimited_format_item(cfg),
            delim(')'),
        ).map(|(_, format_items, unlimited_format_item, _)| FormatSpecification::UnlimitedFormatItem {
            format_items: Some(format_items),
            unlimited_format_item,
        }),
    )
}

#[derive(Debug, Clone)]
pub struct FormatItems<Span>(pub Vec<FormatItem<Span>>);

#[syntax_rule(
    F18V007r1 rule "format-items" #1303 : "is format-item [ [ , ] format-item ] ...",
)]
pub fn format_items<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = FormatItems<MultilineSpan>> + 'a {
    list(format_item(cfg), 1..).map(FormatItems)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum FormatItem<Span> {
    DataEditDesc {
        r: Option<R<Span>>,
        data_edit_desc: DataEditDesc<Span>,
    },
    ControlEditDesc(ControlEditDesc<Span>),
    CharStringEditDesc(CharStringEditDesc<Span>),
    FormatItems {
        r: Option<R<Span>>,
        format_items: FormatItems<Span>,
    },
}

#[syntax_rule(
    F18V007r1 rule "format-item" #1304 :
    "is [ r ] data-edit-desc"
    "or control-edit-desc"
    "or char-string-edit-desc"
    "or [ r ] ( format-items )",
)]
pub fn format_item<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = FormatItem<MultilineSpan>> + 'a {
    alt!(
        (
            r(cfg).optional(),
            data_edit_desc(cfg),
        ).map(|(r, data_edit_desc)| FormatItem::DataEditDesc {
            r,
            data_edit_desc,
        }),
        control_edit_desc(cfg).map(FormatItem::ControlEditDesc),
        char_string_edit_desc(cfg).map(FormatItem::CharStringEditDesc),
        (
            r(cfg).optional(),
            delim('('),
            format_items(cfg),
            delim(')'),
        ).map(|(r, _, format_items, _)| FormatItem::FormatItems {
            r,
            format_items,
        }),
    )
}

#[derive(Debug, Clone)]
pub struct UnlimitedFormatItem<Span> {
    pub format_items: FormatItems<Span>,
}

#[syntax_rule(
    F18V007r1 rule "unlimited-format-item" #1305 : "is * ( format-items )",
)]
pub fn unlimited_format_item<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = UnlimitedFormatItem<MultilineSpan>> + 'a {
    (
        asterisk(),
        delim('('),
        format_items(cfg),
        delim(')'),
    ).map(|(_, _, format_items, _)| UnlimitedFormatItem {
        format_items,
    })
}

#[derive(Debug, Clone)]
pub struct R<Span>(pub IntLiteralConstant<Span>);

#[syntax_rule(
    F18V007r1 rule "r" #1306 : "is int-literal-constant",
)]
pub fn r<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = R<MultilineSpan>> + 'a {
    int_literal_constant().map(R)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum DataEditDesc<Span> {
    I {
        w: W<Span>,
        m: Option<M<Span>>,
    },
    B {
        w: W<Span>,
        m: Option<M<Span>>,
    },
    O {
        w: W<Span>,
        m: Option<M<Span>>,
    },
    Z {
        w: W<Span>,
        m: Option<M<Span>>,
    },
    F {
        w: W<Span>,
        d: D<Span>,
    },
    E {
        w: W<Span>,
        d: D<Span>,
        e: Option<E<Span>>,
    },
    EN {
        w: W<Span>,
        d: D<Span>,
        e: Option<E<Span>>,
    },
    ES {
        w: W<Span>,
        d: D<Span>,
        e: Option<E<Span>>,
    },
    EX {
        w: W<Span>,
        d: D<Span>,
        e: Option<E<Span>>,
    },
    G {
        w: W<Span>,
        d: Option<D<Span>>,
        e: Option<E<Span>>,
    },
    L {
        w: W<Span>,
    },
    A {
        w: Option<W<Span>>,
    },
    D {
        w: W<Span>,
        d: D<Span>,
    },
    DT {
        char_literal_constant: Option<CharLiteralConstant<Span>>,
        v_list: Option<Vec<V<Span>>>,
    },
}

#[syntax_rule(
    F18V007r1 rule "data-edit-desc" #1307 :
    "is I w [ . m ]"
    "or B w [ . m ]"
    "or O w [ . m ]"
    "or Z w [ . m ]"
    "or F w . d"
    "or E w . d [ E e ]"
    "or EN w . d [ E e ]"
    "or ES w . d [ E e ]"
    "or EX w . d [ E e ]"
    "or G w [ . d [ E e ] ]"
    "or L w"
    "or A [ w ]"
    "or D w . d"
    "or DT [ char-literal-constant ] [ ( v-list ) ]",
)]
pub fn data_edit_desc<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DataEditDesc<MultilineSpan>> + 'a {
    alt!(
        (
            kw!(I),
            w(cfg),
            (dot(), m(cfg)).map(|(_, m)| m).optional(),
        ).map(|(_, w, m)| DataEditDesc::I { w, m }),
        (
            kw!(b),
            w(cfg),
            (dot(), m(cfg)).map(|(_, m)| m).optional(),
        ).map(|(_, w, m)| DataEditDesc::B { w, m }),
        (
            kw!(o),
            w(cfg),
            (dot(), m(cfg)).map(|(_, m)| m).optional(),
        ).map(|(_, w, m)| DataEditDesc::O { w, m }),
        (
            kw!(z),
            w(cfg),
            (dot(), m(cfg)).map(|(_, m)| m).optional(),
        ).map(|(_, w, m)| DataEditDesc::Z { w, m }),
        (
            kw!(f),
            w(cfg),
            dot(),
            d(cfg),
        ).map(|(_, w, _, d)| DataEditDesc::F { w, d }),
        (
            kw!(e),
            w(cfg),
            dot(),
            d(cfg),
            (
                kw!(e),
                e(cfg),
            ).map(|(_, e)| e).optional(),
        ).map(|(_, w, _, d, e)| DataEditDesc::E { w, d, e }),
        (
            kw!(en),
            w(cfg),
            dot(),
            d(cfg),
            (
                kw!(e),
                e(cfg),
            ).map(|(_, e)| e).optional(),
        ).map(|(_, w, _, d, e)| DataEditDesc::EN { w, d, e }),
        (
            kw!(es),
            w(cfg),
            dot(),
            d(cfg),
            (
                kw!(e),
                e(cfg),
            ).map(|(_, e)| e).optional(),
        ).map(|(_, w, _, d, e)| DataEditDesc::ES { w, d, e }),
        (
            kw!(ex),
            w(cfg),
            dot(),
            d(cfg),
            (
                kw!(e),
                e(cfg),
            ).map(|(_, e)| e).optional(),
        ).map(|(_, w, _, d, e)| DataEditDesc::EX { w, d, e }),
        (
            kw!(g),
            w(cfg),
            (
                dot(),
                d(cfg),
                (
                    kw!(e),
                    e(cfg),
                ).map(|(_, e)| e).optional(),
            ).map(|(_, d, e)| (d, e)).optional(),
        ).map(|(_, w, d_e)| {
            let (d, e) = if let Some((d, e)) = d_e {
                (Some(d), e)
            } else {
                (None, None)
            };
            DataEditDesc::G { w, d, e }
        }),
        (
            kw!(l),
            w(cfg),
        ).map(|(_, w)| DataEditDesc::L { w }),
        (
            kw!(a),
            w(cfg).optional(),
        ).map(|(_, w)| DataEditDesc::A { w }),
        (
            kw!(d),
            w(cfg),
            dot(),
            d(cfg),
        ).map(|(_, w, _, d)| DataEditDesc::D { w, d }),
        (
            kw!(dt),
            char_literal_constant().optional(),
            (
                delim('('),
                list(v(cfg), 0..),
                delim(')'),
            ).map(|(_, v_list, _)| v_list).optional(),
        ).map(|(_, char_literal_constant, v_list)| DataEditDesc::DT {
            char_literal_constant,
            v_list,
        }),
    )
}

#[derive(Debug, Clone)]
pub struct W<Span>(pub IntLiteralConstant<Span>);

#[syntax_rule(
    F18V007r1 rule "w" #1308 : "is int-literal-constant",
)]
pub fn w<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = W<MultilineSpan>> + 'a {
    int_literal_constant().map(W)
}

#[derive(Debug, Clone)]
pub struct M<Span>(pub IntLiteralConstant<Span>);

#[syntax_rule(
    F18V007r1 rule "m" #1309 : "is int-literal-constant",
)]
pub fn m<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = M<MultilineSpan>> + 'a {
    int_literal_constant().map(M)
}

#[derive(Debug, Clone)]
pub struct D<Span>(pub IntLiteralConstant<Span>);

#[syntax_rule(
    F18V007r1 rule "d" #1310 : "is int-literal-constant",
)]
pub fn d<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = D<MultilineSpan>> + 'a {
    int_literal_constant().map(D)
}

#[derive(Debug, Clone)]
pub struct E<Span>(pub IntLiteralConstant<Span>);

#[syntax_rule(
    F18V007r1 rule "e" #1311 : "is int-literal-constant",
)]
pub fn e<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = E<MultilineSpan>> + 'a {
    int_literal_constant().map(E)
}

#[derive(Debug, Clone)]
pub struct V<Span>(pub SignedIntLiteralConstant<Span>);

#[syntax_rule(
    F18V007r1 rule "v" #1312 : "is signed-int-literal-constant",
)]
pub fn v<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = V<MultilineSpan>> + 'a {
    signed_int_literal_constant(cfg).map(V)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ControlEditDesc<Span> {
    PositionEditDesc(PositionEditDesc<Span>),
    Slash(Option<R<Span>>),
    Colon,
    SignEditDesc(SignEditDesc),
    P {
        k: K<Span>,
    },
    BlankInterpEditDesc(BlankInterpEditDesc),
    RoundEditDesc(RoundEditDesc),
    DecimalEditDesc(DecimalEditDesc),
}

#[syntax_rule(
    F18V007r1 rule "control-edit-desc" #1313 :
    "is position-edit-desc"
    "or [ r ] /"
    "or :"
    "or sign-edit-desc"
    "or k P"
    "or blank-interp-edit-desc"
    "or round-edit-desc"
    "or decimal-edit-desc",
)]
pub fn control_edit_desc<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ControlEditDesc<MultilineSpan>> + 'a {
    alt!(
        position_edit_desc(cfg).map(ControlEditDesc::PositionEditDesc),
        (
            r(cfg).optional(),
            op("/"),
        ).map(|(r, _)| ControlEditDesc::Slash(r)),
        colon().map(|_| ControlEditDesc::Colon),
        sign_edit_desc(cfg).map(ControlEditDesc::SignEditDesc),
        (
            k(cfg),
            kw!(p),
        ).map(|(k, _)| ControlEditDesc::P { k }),
        blank_interp_edit_desc(cfg).map(ControlEditDesc::BlankInterpEditDesc),
        round_edit_desc(cfg).map(ControlEditDesc::RoundEditDesc),
        decimal_edit_desc(cfg).map(ControlEditDesc::DecimalEditDesc),
    )
}

#[derive(Debug, Clone)]
pub struct K<Span>(pub SignedIntLiteralConstant<Span>);

#[syntax_rule(
    F18V007r1 rule "k" #1314 : "is signed-int-literal-constant",
)]
pub fn k<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = K<MultilineSpan>> + 'a {
    signed_int_literal_constant(cfg).map(K)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum PositionEditDesc<Span> {
    T {
        n: N<Span>,
    },
    TL {
        n: N<Span>,
    },
    TR {
        n: N<Span>,
    },
    X {
        n: N<Span>,
    },
}

#[syntax_rule(
    F18V007r1 rule "position-edit-desc" #1315 :
    "is T n"
    "or TL n"
    "or TR n"
    "or n X",
)]
pub fn position_edit_desc<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = PositionEditDesc<MultilineSpan>> + 'a {
    alt!(
        (
            kw!(t),
            n(cfg),
        ).map(|(_, n)| PositionEditDesc::T { n }),
        (
            kw!(tl),
            n(cfg),
        ).map(|(_, n)| PositionEditDesc::TL { n }),
        (
            kw!(tr),
            n(cfg),
        ).map(|(_, n)| PositionEditDesc::TR { n }),
        (
            n(cfg),
            kw!(x),
        ).map(|(n, _)| PositionEditDesc::X { n }),
    )
}

#[derive(Debug, Clone)]
pub struct N<Span>(pub IntLiteralConstant<Span>);

#[syntax_rule(
    F18V007r1 rule "n" #1316 : "is int-literal-constant",
)]
pub fn n<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = N<MultilineSpan>> + 'a {
    int_literal_constant().map(N)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum SignEditDesc {
    SS,
    SP,
    S,
}

#[syntax_rule(
    F18V007r1 rule "sign-edit-desc" #1317 :
    "is SS"
    "or SP"
    "or S",
)]
pub fn sign_edit_desc<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = SignEditDesc> + 'a {
    alt!(
        kw!(ss).map(|_| SignEditDesc::SS),
        kw!(sp).map(|_| SignEditDesc::SP),
        kw!(s).map(|_| SignEditDesc::S),
    )
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum BlankInterpEditDesc {
    BN,
    BZ,
}

#[syntax_rule(
    F18V007r1 rule "blank-interp-edit-desc" #1318 :
    "is BN"
    "or BZ",
)]
pub fn blank_interp_edit_desc<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = BlankInterpEditDesc> + 'a {
    alt!(
        kw!(bn).map(|_| BlankInterpEditDesc::BN),
        kw!(bz).map(|_| BlankInterpEditDesc::BZ),
    )
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum RoundEditDesc {
    RU,
    RD,
    RZ,
    RN,
    RC,
    RP,
}

#[syntax_rule(
    F18V007r1 rule "round-edit-desc" #1319 :
    "is RU"
    "or RD"
    "or RZ"
    "or RN"
    "or RC"
    "or RP",
)]
pub fn round_edit_desc<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = RoundEditDesc> + 'a {
    alt!(
        kw!(ru).map(|_| RoundEditDesc::RU),
        kw!(rd).map(|_| RoundEditDesc::RD),
        kw!(rz).map(|_| RoundEditDesc::RZ),
        kw!(rn).map(|_| RoundEditDesc::RN),
        kw!(rc).map(|_| RoundEditDesc::RC),
        kw!(rp).map(|_| RoundEditDesc::RP),
    )
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum DecimalEditDesc {
    DC,
    DP,
}

#[syntax_rule(
    F18V007r1 rule "decimal-edit-desc" #1320 :
    "is DC"
    "or DP",
)]
pub fn decimal_edit_desc<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DecimalEditDesc> + 'a {
    alt!(
        kw!(dc).map(|_| DecimalEditDesc::DC),
        kw!(dp).map(|_| DecimalEditDesc::DP),
    )
}

#[derive(Debug, Clone)]
pub struct CharStringEditDesc<Span>(pub CharLiteralConstant<Span>);

#[syntax_rule(
    F18V007r1 rule "char-string-edit-desc" #1321 : "is char-literal-constant",
)]
pub fn char_string_edit_desc<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = CharStringEditDesc<MultilineSpan>> + 'a {
    char_literal_constant().map(CharStringEditDesc)
}

#[derive(Debug, Clone)]
pub struct HexDigitString<Span>(pub Vec<Char<Span>>);

//#[syntax_rule(
//    F18V007r1 rule "hex-digit-string" #1322 : "is hex-digit [ hex-digit ] ...",
//)]
//pub fn hex_digit_string<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = HexDigitString<MultilineSpan>> + 'a {
//    many(hex_digit(cfg), 1..).map(HexDigitString)
//}
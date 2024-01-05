use super::*;

#[derive(Debug, Clone)]
pub struct FormatStmt<Span> {
    pub format_specification: FormatSpecification<Span>,
    pub comment: Option<LineComment<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "format-stmt" #1301 : "is FORMAT format-specification",
)]
pub fn format_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = FormatStmt<S::Span>> + 'a {
    (
        space(0),
        kw("format", cfg), space(0),
        format_specification(cfg),
        statement_termination(),
    ).map(|(_, _, _, format_specification, comment)| FormatStmt {
        format_specification,
        comment,
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
pub fn format_specification<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = FormatSpecification<S::Span>> + 'a {
    alt!(
        (
            (space(0), '(', space(0)),
            format_items(cfg).optional(),
            (space(0), ')'),
        ).map(|(_, format_items, _)| FormatSpecification::FormatItems {
            format_items: format_items.unwrap_or_else(|| FormatItems(Vec::new())),
        }),
        (
            (space(0), '(', space(0)),
            (
                format_items(cfg),
                (space(0), ',', space(0)),
            ).map(|(format_items, _)| format_items),
            unlimited_format_item(cfg),
            (space(0), ')'),
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
pub fn format_items<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = FormatItems<S::Span>> + 'a {
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
pub fn format_item<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = FormatItem<S::Span>> + 'a {
    alt!(
        (
            (r(cfg), space(0)).map(|(r, _)| r).optional(),
            data_edit_desc(cfg),
        ).map(|(r, data_edit_desc)| FormatItem::DataEditDesc {
            r,
            data_edit_desc,
        }),
        control_edit_desc(cfg).map(FormatItem::ControlEditDesc),
        char_string_edit_desc(cfg).map(FormatItem::CharStringEditDesc),
        (
            (r(cfg), space(0)).map(|(r, _)| r).optional(),
            (space(0), '(', space(0)),
            format_items(cfg),
            (space(0), ')'),
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
pub fn unlimited_format_item<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = UnlimitedFormatItem<S::Span>> + 'a {
    (
        SpecialCharacter::Asterisk,
        (space(0), '(', space(0)),
        format_items(cfg),
        (space(0), ')'),
    ).map(|(_, _, format_items, _)| UnlimitedFormatItem {
        format_items,
    })
}

#[derive(Debug, Clone)]
pub struct R<Span>(pub IntLiteralConstant<Span>);

#[syntax_rule(
    F18V007r1 rule "r" #1306 : "is int-literal-constant",
)]
pub fn r<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = R<S::Span>> + 'a {
    int_literal_constant(cfg).map(R)
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
pub fn data_edit_desc<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DataEditDesc<S::Span>> + 'a {
    alt!(
        (
            (kw("i", cfg), space(0)),
            w(cfg),
            (space(0), '.', space(0), m(cfg)).map(|(_, _, _, m)| m).optional(),
        ).map(|(_, w, m)| DataEditDesc::I { w, m }),
        (
            (kw("b", cfg), space(0)),
            w(cfg),
            (space(0), '.', space(0), m(cfg)).map(|(_, _, _, m)| m).optional(),
        ).map(|(_, w, m)| DataEditDesc::B { w, m }),
        (
            (kw("o", cfg), space(0)),
            w(cfg),
            (space(0), '.', space(0), m(cfg)).map(|(_, _, _, m)| m).optional(),
        ).map(|(_, w, m)| DataEditDesc::O { w, m }),
        (
            (kw("z", cfg), space(0)),
            w(cfg),
            (space(0), '.', space(0), m(cfg)).map(|(_, _, _, m)| m).optional(),
        ).map(|(_, w, m)| DataEditDesc::Z { w, m }),
        (
            (kw("f", cfg), space(0)),
            w(cfg),
            (space(0), '.', space(0)),
            d(cfg),
        ).map(|(_, w, _, d)| DataEditDesc::F { w, d }),
        (
            (kw("e", cfg), space(0)),
            w(cfg),
            (space(0), '.', space(0)),
            d(cfg),
            (
                (space(0), kw("e", cfg), space(0)),
                e(cfg),
            ).map(|(_, e)| e).optional(),
        ).map(|(_, w, _, d, e)| DataEditDesc::E { w, d, e }),
        (
            (kw("en", cfg), space(0)),
            w(cfg),
            (space(0), '.', space(0)),
            d(cfg),
            (
                (space(0), kw("e", cfg), space(0)),
                e(cfg),
            ).map(|(_, e)| e).optional(),
        ).map(|(_, w, _, d, e)| DataEditDesc::EN { w, d, e }),
        (
            (kw("es", cfg), space(0)),
            w(cfg),
            (space(0), '.', space(0)),
            d(cfg),
            (
                (space(0), kw("e", cfg), space(0)),
                e(cfg),
            ).map(|(_, e)| e).optional(),
        ).map(|(_, w, _, d, e)| DataEditDesc::ES { w, d, e }),
        (
            (kw("ex", cfg), space(0)),
            w(cfg),
            (space(0), '.', space(0)),
            d(cfg),
            (
                (space(0), kw("e", cfg), space(0)),
                e(cfg),
            ).map(|(_, e)| e).optional(),
        ).map(|(_, w, _, d, e)| DataEditDesc::EX { w, d, e }),
        (
            (kw("g", cfg), space(0)),
            w(cfg),
            (
                (space(0), '.', space(0)),
                d(cfg),
                (
                    (space(0), kw("e", cfg), space(0)),
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
            (kw("l", cfg), space(0)),
            w(cfg),
        ).map(|(_, w)| DataEditDesc::L { w }),
        (
            (kw("a", cfg), space(0)),
            (space(0), w(cfg)).map(|(_, w)| w).optional(),
        ).map(|(_, w)| DataEditDesc::A { w }),
        (
            (kw("d", cfg), space(0)),
            w(cfg),
            (space(0), '.', space(0)),
            d(cfg),
        ).map(|(_, w, _, d)| DataEditDesc::D { w, d }),
        (
            kw("dt", cfg),
            char_literal_constant(cfg).optional(),
            (
                (space(0), '(', space(0)),
                list(v(cfg), 0..),
                (space(0), ')'),
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
pub fn w<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = W<S::Span>> + 'a {
    int_literal_constant(cfg).map(W)
}

#[derive(Debug, Clone)]
pub struct M<Span>(pub IntLiteralConstant<Span>);

#[syntax_rule(
    F18V007r1 rule "m" #1309 : "is int-literal-constant",
)]
pub fn m<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = M<S::Span>> + 'a {
    int_literal_constant(cfg).map(M)
}

#[derive(Debug, Clone)]
pub struct D<Span>(pub IntLiteralConstant<Span>);

#[syntax_rule(
    F18V007r1 rule "d" #1310 : "is int-literal-constant",
)]
pub fn d<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = D<S::Span>> + 'a {
    int_literal_constant(cfg).map(D)
}

#[derive(Debug, Clone)]
pub struct E<Span>(pub IntLiteralConstant<Span>);

#[syntax_rule(
    F18V007r1 rule "e" #1311 : "is int-literal-constant",
)]
pub fn e<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = E<S::Span>> + 'a {
    int_literal_constant(cfg).map(E)
}

#[derive(Debug, Clone)]
pub struct V<Span>(pub SignedIntLiteralConstant<Span>);

#[syntax_rule(
    F18V007r1 rule "v" #1312 : "is signed-int-literal-constant",
)]
pub fn v<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = V<S::Span>> + 'a {
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
pub fn control_edit_desc<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ControlEditDesc<S::Span>> + 'a {
    alt!(
        position_edit_desc(cfg).map(ControlEditDesc::PositionEditDesc),
        (
            (r(cfg), space(0)).map(|(r, _)| r).optional(),
            SpecialCharacter::Slash,
        ).map(|(r, _)| ControlEditDesc::Slash(r)),
        SpecialCharacter::Colon.map(|_| ControlEditDesc::Colon),
        sign_edit_desc(cfg).map(ControlEditDesc::SignEditDesc),
        (
            k(cfg),
            (space(0), kw("p", cfg), space(0)),
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
pub fn k<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = K<S::Span>> + 'a {
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
pub fn position_edit_desc<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = PositionEditDesc<S::Span>> + 'a {
    alt!(
        (
            kw("t", cfg),
            (space(0), n(cfg)).map(|(_, n)| n),
        ).map(|(_, n)| PositionEditDesc::T { n }),
        (
            kw("tl", cfg),
            (space(0), n(cfg)).map(|(_, n)| n),
        ).map(|(_, n)| PositionEditDesc::TL { n }),
        (
            kw("tr", cfg),
            (space(0), n(cfg)).map(|(_, n)| n),
        ).map(|(_, n)| PositionEditDesc::TR { n }),
        (
            n(cfg),
            (space(0), kw("x", cfg), space(0)),
        ).map(|(n, _)| PositionEditDesc::X { n }),
    )
}

#[derive(Debug, Clone)]
pub struct N<Span>(pub IntLiteralConstant<Span>);

#[syntax_rule(
    F18V007r1 rule "n" #1316 : "is int-literal-constant",
)]
pub fn n<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = N<S::Span>> + 'a {
    int_literal_constant(cfg).map(N)
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
pub fn sign_edit_desc<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = SignEditDesc> + 'a {
    alt!(
        kw("ss", cfg).map(|_| SignEditDesc::SS),
        kw("sp", cfg).map(|_| SignEditDesc::SP),
        kw("s", cfg).map(|_| SignEditDesc::S),
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
pub fn blank_interp_edit_desc<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = BlankInterpEditDesc> + 'a {
    alt!(
        kw("bn", cfg).map(|_| BlankInterpEditDesc::BN),
        kw("bz", cfg).map(|_| BlankInterpEditDesc::BZ),
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
pub fn round_edit_desc<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = RoundEditDesc> + 'a {
    alt!(
        kw("ru", cfg).map(|_| RoundEditDesc::RU),
        kw("rd", cfg).map(|_| RoundEditDesc::RD),
        kw("rz", cfg).map(|_| RoundEditDesc::RZ),
        kw("rn", cfg).map(|_| RoundEditDesc::RN),
        kw("rc", cfg).map(|_| RoundEditDesc::RC),
        kw("rp", cfg).map(|_| RoundEditDesc::RP),
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
pub fn decimal_edit_desc<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DecimalEditDesc> + 'a {
    alt!(
        kw("dc", cfg).map(|_| DecimalEditDesc::DC),
        kw("dp", cfg).map(|_| DecimalEditDesc::DP),
    )
}

#[derive(Debug, Clone)]
pub struct CharStringEditDesc<Span>(pub CharLiteralConstant<Span>);

#[syntax_rule(
    F18V007r1 rule "char-string-edit-desc" #1321 : "is char-literal-constant",
)]
pub fn char_string_edit_desc<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = CharStringEditDesc<S::Span>> + 'a {
    char_literal_constant(cfg).map(CharStringEditDesc)
}

#[derive(Debug, Clone)]
pub struct HexDigitString<Span>(pub Vec<Char<Span>>);

#[syntax_rule(
    F18V007r1 rule "hex-digit-string" #1322 : "is hex-digit [ hex-digit ] ...",
)]
pub fn hex_digit_string<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = HexDigitString<S::Span>> + 'a {
    many(hex_digit(cfg), 1..).map(HexDigitString)
}
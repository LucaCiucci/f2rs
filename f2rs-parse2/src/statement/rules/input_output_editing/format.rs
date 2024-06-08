
use super::*;

#[derive(Debug, Clone)]
pub struct FormatStmt<Span> {
    pub format_specification: FormatSpecification<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "format-stmt" #1301 : "is FORMAT format-specification",
)]
pub fn format_stmt_2<S: Lexed>(source: S) -> PResult<FormatStmt<MultilineSpan>, S> {
    (
        kw!(format),
        format_specification,
    ).map(|(_, format_specification)| FormatStmt {
        format_specification,
    }).parse(source)
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

#[doc = s_rule!(
    F18V007r1 rule "format-specification" #1302 :
    "is ( [ format-items ] )"
    "or ( [ format-items, ] unlimited-format-item )",
)]
pub fn format_specification<S: Lexed>(source: S) -> PResult<FormatSpecification<MultilineSpan>, S> {
    alt!(
        for S =>
        (
            delim('('),
            format_items.optional(),
            delim(')'),
        ).map(|(_, format_items, _)| FormatSpecification::FormatItems {
            format_items: format_items.unwrap_or_else(|| FormatItems(Vec::new())),
        }),
        (
            delim('('),
            (
                format_items,
                comma(),
            ).map(|(format_items, _)| format_items),
            unlimited_format_item,
            delim(')'),
        ).map(|(_, format_items, unlimited_format_item, _)| FormatSpecification::UnlimitedFormatItem {
            format_items: Some(format_items),
            unlimited_format_item,
        }),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct FormatItems<Span>(pub Vec<FormatItem<Span>>);

#[doc = s_rule!(
    F18V007r1 rule "format-items" #1303 : "is format-item [ [ , ] format-item ] ...",
)]
pub fn format_items<S: Lexed>(source: S) -> PResult<FormatItems<MultilineSpan>, S> {
    list(format_item, 1..).map(FormatItems).parse(source)
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

#[doc = s_rule!(
    F18V007r1 rule "format-item" #1304 :
    "is [ r ] data-edit-desc"
    "or control-edit-desc"
    "or char-string-edit-desc"
    "or [ r ] ( format-items )",
)]
pub fn format_item<S: Lexed>(source: S) -> PResult<FormatItem<MultilineSpan>, S> {
    alt!(
        for S =>
        (
            r.optional(),
            data_edit_desc,
        ).map(|(r, data_edit_desc)| FormatItem::DataEditDesc {
            r,
            data_edit_desc,
        }),
        control_edit_desc.map(FormatItem::ControlEditDesc),
        char_string_edit_desc.map(FormatItem::CharStringEditDesc),
        (
            r.optional(),
            delim('('),
            format_items,
            delim(')'),
        ).map(|(r, _, format_items, _)| FormatItem::FormatItems {
            r,
            format_items,
        }),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct UnlimitedFormatItem<Span> {
    pub format_items: FormatItems<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "unlimited-format-item" #1305 : "is * ( format-items )",
)]
pub fn unlimited_format_item<S: Lexed>(source: S) -> PResult<UnlimitedFormatItem<MultilineSpan>, S> {
    (
        asterisk(),
        delim('('),
        format_items,
        delim(')'),
    ).map(|(_, _, format_items, _)| UnlimitedFormatItem {
        format_items,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct R<Span>(pub IntLiteralConstant<Span>);

#[doc = s_rule!(
    F18V007r1 rule "r" #1306 : "is int-literal-constant",
)]
pub fn r<S: Lexed>(source: S) -> PResult<R<MultilineSpan>, S> {
    int_literal_constant().map(R).parse(source)
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

#[doc = s_rule!(
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
pub fn data_edit_desc<S: Lexed>(source: S) -> PResult<DataEditDesc<MultilineSpan>, S> {
    alt!(
        for S =>
        (
            kw!(I),
            w,
            (dot(), m).map(|(_, m)| m).optional(),
        ).map(|(_, w, m)| DataEditDesc::I { w, m }),
        (
            kw!(b),
            w,
            (dot(), m).map(|(_, m)| m).optional(),
        ).map(|(_, w, m)| DataEditDesc::B { w, m }),
        (
            kw!(o),
            w,
            (dot(), m).map(|(_, m)| m).optional(),
        ).map(|(_, w, m)| DataEditDesc::O { w, m }),
        (
            kw!(z),
            w,
            (dot(), m).map(|(_, m)| m).optional(),
        ).map(|(_, w, m)| DataEditDesc::Z { w, m }),
        (
            kw!(f),
            w,
            dot(),
            d,
        ).map(|(_, w, _, d)| DataEditDesc::F { w, d }),
        (
            kw!(e),
            w,
            dot(),
            d,
            (
                kw!(e),
                e,
            ).map(|(_, e)| e).optional(),
        ).map(|(_, w, _, d, e)| DataEditDesc::E { w, d, e }),
        (
            kw!(en),
            w,
            dot(),
            d,
            (
                kw!(e),
                e,
            ).map(|(_, e)| e).optional(),
        ).map(|(_, w, _, d, e)| DataEditDesc::EN { w, d, e }),
        (
            kw!(es),
            w,
            dot(),
            d,
            (
                kw!(e),
                e,
            ).map(|(_, e)| e).optional(),
        ).map(|(_, w, _, d, e)| DataEditDesc::ES { w, d, e }),
        (
            kw!(ex),
            w,
            dot(),
            d,
            (
                kw!(e),
                e,
            ).map(|(_, e)| e).optional(),
        ).map(|(_, w, _, d, e)| DataEditDesc::EX { w, d, e }),
        (
            kw!(g),
            w,
            (
                dot(),
                d,
                (
                    kw!(e),
                    e,
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
            w,
        ).map(|(_, w)| DataEditDesc::L { w }),
        (
            kw!(a),
            w.optional(),
        ).map(|(_, w)| DataEditDesc::A { w }),
        (
            kw!(d),
            w,
            dot(),
            d,
        ).map(|(_, w, _, d)| DataEditDesc::D { w, d }),
        (
            kw!(dt),
            char_literal_constant().optional(),
            (
                delim('('),
                list(v, 0..),
                delim(')'),
            ).map(|(_, v_list, _)| v_list).optional(),
        ).map(|(_, char_literal_constant, v_list)| DataEditDesc::DT {
            char_literal_constant,
            v_list,
        }),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct W<Span>(pub IntLiteralConstant<Span>);

#[doc = s_rule!(
    F18V007r1 rule "w" #1308 : "is int-literal-constant",
)]
pub fn w<S: Lexed>(source: S) -> PResult<W<MultilineSpan>, S> {
    int_literal_constant().map(W).parse(source)
}

#[derive(Debug, Clone)]
pub struct M<Span>(pub IntLiteralConstant<Span>);

#[doc = s_rule!(
    F18V007r1 rule "m" #1309 : "is int-literal-constant",
)]
pub fn m<S: Lexed>(source: S) -> PResult<M<MultilineSpan>, S> {
    int_literal_constant().map(M).parse(source)
}

#[derive(Debug, Clone)]
pub struct D<Span>(pub IntLiteralConstant<Span>);

#[doc = s_rule!(
    F18V007r1 rule "d" #1310 : "is int-literal-constant",
)]
pub fn d<S: Lexed>(source: S) -> PResult<D<MultilineSpan>, S> {
    int_literal_constant().map(D).parse(source)
}

#[derive(Debug, Clone)]
pub struct E<Span>(pub IntLiteralConstant<Span>);

#[doc = s_rule!(
    F18V007r1 rule "e" #1311 : "is int-literal-constant",
)]
pub fn e<S: Lexed>(source: S) -> PResult<E<MultilineSpan>, S> {
    int_literal_constant().map(E).parse(source)
}

#[derive(Debug, Clone)]
pub struct V<Span>(pub SignedIntLiteralConstant<Span>);

#[doc = s_rule!(
    F18V007r1 rule "v" #1312 : "is signed-int-literal-constant",
)]
pub fn v<S: Lexed>(source: S) -> PResult<V<MultilineSpan>, S> {
    signed_int_literal_constant.map(V).parse(source)
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

#[doc = s_rule!(
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
pub fn control_edit_desc<S: Lexed>(source: S) -> PResult<ControlEditDesc<MultilineSpan>, S> {
    alt!(
        for S =>
        position_edit_desc.map(ControlEditDesc::PositionEditDesc),
        (
            r.optional(),
            op("/"),
        ).map(|(r, _)| ControlEditDesc::Slash(r)),
        colon().map(|_| ControlEditDesc::Colon),
        sign_edit_desc.map(ControlEditDesc::SignEditDesc),
        (
            k,
            kw!(p),
        ).map(|(k, _)| ControlEditDesc::P { k }),
        blank_interp_edit_desc.map(ControlEditDesc::BlankInterpEditDesc),
        round_edit_desc.map(ControlEditDesc::RoundEditDesc),
        decimal_edit_desc.map(ControlEditDesc::DecimalEditDesc),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct K<Span>(pub SignedIntLiteralConstant<Span>);

#[doc = s_rule!(
    F18V007r1 rule "k" #1314 : "is signed-int-literal-constant",
)]
pub fn k<S: Lexed>(source: S) -> PResult<K<MultilineSpan>, S> {
    signed_int_literal_constant.map(K).parse(source)
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

#[doc = s_rule!(
    F18V007r1 rule "position-edit-desc" #1315 :
    "is T n"
    "or TL n"
    "or TR n"
    "or n X",
)]
pub fn position_edit_desc<S: Lexed>(source: S) -> PResult<PositionEditDesc<MultilineSpan>, S> {
    alt!(
        for S =>
        (
            kw!(t),
            n,
        ).map(|(_, n)| PositionEditDesc::T { n }),
        (
            kw!(tl),
            n,
        ).map(|(_, n)| PositionEditDesc::TL { n }),
        (
            kw!(tr),
            n,
        ).map(|(_, n)| PositionEditDesc::TR { n }),
        (
            n,
            kw!(x),
        ).map(|(n, _)| PositionEditDesc::X { n }),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct N<Span>(pub IntLiteralConstant<Span>);

#[doc = s_rule!(
    F18V007r1 rule "n" #1316 : "is int-literal-constant",
)]
pub fn n<S: Lexed>(source: S) -> PResult<N<MultilineSpan>, S> {
    int_literal_constant().map(N).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum SignEditDesc {
    SS,
    SP,
    S,
}

#[doc = s_rule!(
    F18V007r1 rule "sign-edit-desc" #1317 :
    "is SS"
    "or SP"
    "or S",
)]
pub fn sign_edit_desc<S: Lexed>(source: S) -> PResult<SignEditDesc, S> {
    alt!(
        for S =>
        kw!(ss).map(|_| SignEditDesc::SS),
        kw!(sp).map(|_| SignEditDesc::SP),
        kw!(s).map(|_| SignEditDesc::S),
    ).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum BlankInterpEditDesc {
    BN,
    BZ,
}

#[doc = s_rule!(
    F18V007r1 rule "blank-interp-edit-desc" #1318 :
    "is BN"
    "or BZ",
)]
pub fn blank_interp_edit_desc<S: Lexed>(source: S) -> PResult<BlankInterpEditDesc, S> {
    alt!(
        for S =>
        kw!(bn).map(|_| BlankInterpEditDesc::BN),
        kw!(bz).map(|_| BlankInterpEditDesc::BZ),
    ).parse(source)
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

#[doc = s_rule!(
    F18V007r1 rule "round-edit-desc" #1319 :
    "is RU"
    "or RD"
    "or RZ"
    "or RN"
    "or RC"
    "or RP",
)]
pub fn round_edit_desc<S: Lexed>(source: S) -> PResult<RoundEditDesc, S> {
    alt!(
        for S =>
        kw!(ru).map(|_| RoundEditDesc::RU),
        kw!(rd).map(|_| RoundEditDesc::RD),
        kw!(rz).map(|_| RoundEditDesc::RZ),
        kw!(rn).map(|_| RoundEditDesc::RN),
        kw!(rc).map(|_| RoundEditDesc::RC),
        kw!(rp).map(|_| RoundEditDesc::RP),
    ).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum DecimalEditDesc {
    DC,
    DP,
}

#[doc = s_rule!(
    F18V007r1 rule "decimal-edit-desc" #1320 :
    "is DC"
    "or DP",
)]
pub fn decimal_edit_desc<S: Lexed>(source: S) -> PResult<DecimalEditDesc, S> {
    alt!(
        for S =>
        kw!(dc).map(|_| DecimalEditDesc::DC),
        kw!(dp).map(|_| DecimalEditDesc::DP),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct CharStringEditDesc<Span>(pub CharLiteralConstant<Span>);

#[doc = s_rule!(
    F18V007r1 rule "char-string-edit-desc" #1321 : "is char-literal-constant",
)]
pub fn char_string_edit_desc<S: Lexed>(source: S) -> PResult<CharStringEditDesc<MultilineSpan>, S> {
    char_literal_constant().map(CharStringEditDesc).parse(source)
}

#[derive(Debug, Clone)]
pub struct HexDigitString<Span>(pub Vec<Char<Span>>);

//#[doc = s_rule!(
//    F18V007r1 rule "hex-digit-string" #1322 : "is hex-digit [ hex-digit ] ...",
//)]
//pub fn hex_digit_string<S: Lexed>(source: S) -> PResult<HexDigitString<MultilineSpan>, S> {
//    many(hex_digit, 1..).map(HexDigitString)
//}
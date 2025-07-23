use f2rs_parser_combinator::seq;

use super::*;

#[derive(Debug, Clone)]
pub struct AssociateStmt<Span> {
    pub associate_construct_name: Option<Name<Span>>,
    pub association_list: Vec<Association<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "associate-stmt" #1103 : "is [ associate-construct-name : ] ASSOCIATE (association-list )",
)]
pub fn associate_stmt<S: Lexed>(source: S) -> PResult<AssociateStmt<MultilineSpan>, S> {
    seq!((
        associate_construct_name: (
            name(),
            colon(),
        ).map(|(name, _)| name).opt(),
        _: kw!(associate),
        _: delim('('),
        association_list: list(association, 0..),
        _: delim(')'),
    ) => AssociateStmt {
        associate_construct_name,
        association_list,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct Association<Span> {
    pub associate_name: Name<Span>,
    pub selector: Selector<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "association" #1104 : "is associate-name => selector",
)]
pub fn association<S: Lexed>(source: S) -> PResult<Association<MultilineSpan>, S> {
    seq!((
        associate_name: name(),
        _: arrow(),
        selector: selector,
    ) => Association {
        associate_name,
        selector,
    }).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum Selector<Span> {
    Expr(Expr<Span>),
    Variable(Variable<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "selector" #1105 :
    "is expr"
    "or variable",
)]
pub fn selector<S: Lexed>(source: S) -> PResult<Selector<MultilineSpan>, S> {
    alt!(
        for S =>
        expr.map(Selector::Expr),
        variable(false/*TODO ???*/).map(Selector::Variable),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct EndAssociateStmt<Span> {
    pub associate_construct_name: Option<Name<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "end-associate-stmt" #1106 : "is END ASSOCIATE [ associate-construct-name ]",
)]
pub fn end_associate_stmt<S: Lexed>(source: S) -> PResult<EndAssociateStmt<MultilineSpan>, S> {
    seq!((
        _: seq!((
            _: kw!(end),
            _: kw!(associate),
        ) => ()),
        associate_construct_name: name().opt(),
    ) => EndAssociateStmt {
        associate_construct_name,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct BlockStmt<Span> {
    pub block_construct_name: Option<Name<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "block-stmt" #1108 : "is [ block-construct-name : ] BLOCK",
)]
pub fn block_stmt<S: Lexed>(source: S) -> PResult<BlockStmt<MultilineSpan>, S> {
    seq!((
        block_construct_name: seq!((
            name: name(),
            _: colon(),
        ) => name).opt(),
        _: kw!(block),
    ) => BlockStmt {
        block_construct_name,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct EndBlockStmt<Span> {
    pub block_construct_name: Option<Name<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "end-block-stmt" #1110 : "is END BLOCK [ block-construct-name ]",
)]
pub fn end_block_stmt<S: Lexed>(source: S) -> PResult<EndBlockStmt<MultilineSpan>, S> {
    seq!((
        _: seq!((
            _: kw!(end),
            _: kw!(block),
        ) => ()),
        block_construct_name: name().opt(),
    ) => EndBlockStmt {
        block_construct_name,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct ChangeTeamStmt<Span> {
    pub team_construct_name: Option<Name<Span>>,
    pub team_value: TeamValue<Span>,
    pub coarray_association_list: Option<Vec<CoarrayAssociation<Span>>>,
    pub sync_stat_list: Option<Vec<SyncStat<Span>>>,
}

#[doc = s_rule!(
    F18V007r1 rule "change-team-stmt" #1112 :
    "is [ team-construct-name : ] CHANGE TEAM ( team-value"
    "    [ , coarray-association-list ] [ , sync-stat-list ] )",
)]
pub fn change_team_stmt<S: Lexed>(source: S) -> PResult<ChangeTeamStmt<MultilineSpan>, S> {
    seq!((
        team_construct_name: seq!((name: name(), _: colon()) => name).opt(),
        _: (kw!(change), kw!(team), delim('(')),
        team_value: team_value,
        coarray_association_list: seq!((
            _: comma(),
            coarray_association_list: list(coarray_association, 0..),
        ) => coarray_association_list).opt(),
        sync_stat_list: seq!((
            _: comma(),
            sync_stat_list: list(sync_stat, 0..),
        ) => sync_stat_list).opt(),
        _: delim(')')
    ) => ChangeTeamStmt {
        team_construct_name,
        team_value,
        coarray_association_list,
        sync_stat_list,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct CoarrayAssociation<Span> {
    pub codimension_decl: CodimensionDecl<Span>,
    pub selector: Selector<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "coarray-association" #1113 : "is codimension-decl => selector",
)]
pub fn coarray_association<S: Lexed>(source: S) -> PResult<CoarrayAssociation<MultilineSpan>, S> {
    seq!((
        codimension_decl: codimension_decl,
        _: arrow(),
        selector: selector,
    ) => CoarrayAssociation {
        codimension_decl,
        selector,
    }).parse(source)
}

pub struct EndChangeTeamStmt<Span> {
    pub sync_stat_list: Option<Vec<SyncStat<Span>>>,
    pub team_construct_name: Option<Name<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "end-change-team-stmt" #1114 : "is END TEAM [ ( [ sync-stat-list ] ) ] [ team-construct-name ]",
)]
pub fn end_change_team_stmt<S: Lexed>(source: S) -> PResult<EndChangeTeamStmt<MultilineSpan>, S> {
    seq!((
        _: (kw!(end), kw!(team)),
        sync_stat_list: seq!((
            _: delim('('),
            sync_stat_list: list(sync_stat, 0..),
            _: delim(')'),
        ) => sync_stat_list).opt(),
        team_construct_name: name().opt(),
    ) => EndChangeTeamStmt {
        sync_stat_list,
        team_construct_name,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct CriticalStmt<Span> {
    pub critical_construct_name: Option<Name<Span>>,
    pub sync_stat_list: Option<Vec<SyncStat<Span>>>,
}

#[doc = s_rule!(
    F18V007r1 rule "critical-stmt" #1117 : "is [ critical-construct-name : ] CRITICAL [ ( [ sync-stat-list ] ) ]",
)]
pub fn critical_stmt<S: Lexed>(source: S) -> PResult<CriticalStmt<MultilineSpan>, S> {
    seq!((
        critical_construct_name: seq!((name: name(), _: colon()) => name).opt(),
        _: kw!(critical),
        sync_stat_list: seq!((
            _: delim('('),
            sync_stat_list: list(sync_stat, 0..),
            _: delim(')'),
        ) => sync_stat_list).opt(),
    ) => CriticalStmt {
        critical_construct_name,
        sync_stat_list,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct EndCriticalStmt<Span> {
    pub critical_construct_name: Option<Name<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "end-critical-stmt" #1118 : "is END CRITICAL [ critical-construct-name ]",
)]
pub fn end_critical_stmt<S: Lexed>(source: S) -> PResult<EndCriticalStmt<MultilineSpan>, S> {
    seq!((
        _: seq!((_: kw!(end), _: kw!(critical)) => ()),
        critical_construct_name: name().opt(),
    ) => EndCriticalStmt {
        critical_construct_name,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub enum DoStmt<Span> {
    Nonlabel(NonlabelDoStmt<Span>),
    Label(LabelDoStmt<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "do-stmt" #1120 :
    "is nonlabel-do-stmt"
    "or label-do-stmt",
)]
pub fn do_stmt<S: Lexed>(source: S) -> PResult<DoStmt<MultilineSpan>, S> {
    alt!(
        for S =>
        nonlabel_do_stmt.map(DoStmt::Nonlabel),
        label_do_stmt.map(DoStmt::Label),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct LabelDoStmt<Span> {
    pub do_construct_name: Option<Name<Span>>,
    pub label: Label<Span>,
    pub loop_control: Option<LoopControl<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "label-do-stmt" #1121 : "is [ do-construct-name : ] DO label [ loop-control ]",
)]
pub fn label_do_stmt<S: Lexed>(source: S) -> PResult<LabelDoStmt<MultilineSpan>, S> {
    seq!((
        do_construct_name: seq!((name: name(), _: colon()) => name).opt(),
        _: kw!(do),
        label: label(),
        loop_control: loop_control.opt(),
    ) => LabelDoStmt {
        do_construct_name,
        label,
        loop_control,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct NonlabelDoStmt<Span> {
    pub do_construct_name: Option<Name<Span>>,
    pub loop_control: LoopControl<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "nonlabel-do-stmt" #1122 : "is [ do-construct-name : ] DO [ loop-control ]",
)]
pub fn nonlabel_do_stmt<S: Lexed>(source: S) -> PResult<NonlabelDoStmt<MultilineSpan>, S> {
    seq!((
        do_construct_name: seq!((name: name(), _: colon()) => name).opt(),
        _: kw!(do),
        loop_control: loop_control,
    ) => NonlabelDoStmt {
        do_construct_name,
        loop_control,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub enum LoopControl<Span> {
    DoVariable {
        do_variable: DoVariable<Span>,
        a: IntExpr<Span>,
        b: IntExpr<Span>,
        c: Option<IntExpr<Span>>,
    },
    While(Expr<Span>),
    Concurrent {
        concurrent_header: ConcurrentHeader<Span>,
        concurrent_locality: Option<ConcurrentLocality<Span>>,
    },
}

#[doc = s_rule!(
    F18V007r1 rule "loop-control" #1123 :
    "is [ , ] do-variable = scalar-int-expr, scalar-int-expr"
    "    [ , scalar-int-expr ]"
    "or [ , ] WHILE ( scalar-logical-expr )"
    "or [ , ] CONCURRENT concurrent-header concurrent-locality",
)]
pub fn loop_control<S: Lexed>(source: S) -> PResult<LoopControl<MultilineSpan>, S> {
    alt!(
        for S =>
        seq!((
            _: comma().opt(),
            do_variable: do_variable,
            _: equals(),
            a: int_expr,
            _: comma(),
            b: int_expr,
            c: seq!((_: comma(), c: int_expr) => c).opt(),
        ) => LoopControl::DoVariable { do_variable, a, b, c }),
        seq!((
            _: comma().opt(),
            _: kw!(while),
            _: delim('('),
            expr: logical_expr,
            _: delim(')'),
        ) => LoopControl::While(expr)),
        seq!((
            _: comma().opt(),
            concurrent_header: concurrent_header,
            concurrent_locality: concurrent_locality.opt(),
        ) => LoopControl::Concurrent {
            concurrent_header,
            concurrent_locality,
        }),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct DoVariable<Span>(pub Name<Span>);

#[doc = s_rule!(
    F18V007r1 rule "do-variable" #1124 : "is scalar-int-variable-name",
)]
pub fn do_variable<S: Lexed>(source: S) -> PResult<DoVariable<MultilineSpan>, S> {
    name().map(DoVariable).parse(source)
}

#[derive(Debug, Clone)]
pub struct ConcurrentHeader<Span> {
    pub integer_type_spec: Option<IntegerTypeSpec<Span>>,
    pub concurrent_control_list: Vec<ConcurrentControl<Span>>,
    pub scalar_mask_expr: Option<MaskExpr<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "concurrent-header" #1125 : "is ( [ integer-type-spec :: ] concurrent-control-list [ , scalar-mask-expr ] )",
)]
pub fn concurrent_header<S: Lexed>(source: S) -> PResult<ConcurrentHeader<MultilineSpan>, S> {
    seq!((
        _: delim('('),
        integer_type_spec: seq!((
            integer_type_spec: integer_type_spec,
            _: double_colon(),
        ) => integer_type_spec).opt(),
        concurrent_control_list: list(concurrent_control, 1..),
        scalar_mask_expr: seq!((_: comma(), scalar_mask_expr: mask_expr) => scalar_mask_expr).opt(),
        _: delim(')'),
    ) => ConcurrentHeader {
        integer_type_spec,
        concurrent_control_list,
        scalar_mask_expr,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct ConcurrentControl<Span> {
    pub index_name: Name<Span>,
    pub concurrent_limit: IntExpr<Span>,
    pub concurrent_limit_2: IntExpr<Span>,
    pub concurrent_step: Option<IntExpr<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "concurrent-control" #1126 : "is index-name = concurrent-limit : concurrent-limit [ : concurrent-step ]",
)]
pub fn concurrent_control<S: Lexed>(source: S) -> PResult<ConcurrentControl<MultilineSpan>, S> {
    seq!((
        index_name: name(),
        _: equals(),
        concurrent_limit: int_expr,
        _: colon(),
        concurrent_limit_2: int_expr,
        concurrent_step: seq!((_: colon(), s: int_expr) => s).opt(),
    ) => ConcurrentControl {
        index_name,
        concurrent_limit,
        concurrent_limit_2,
        concurrent_step,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct ConcurrentLimit<Span>(pub IntExpr<Span>);

#[doc = s_rule!(
    F18V007r1 rule "concurrent-limit" #1127 : "is scalar-int-expr",
)]
pub fn concurrent_limit<S: Lexed>(source: S) -> PResult<ConcurrentLimit<MultilineSpan>, S> {
    int_expr.map(ConcurrentLimit).parse(source)
}

#[derive(Debug, Clone)]
pub struct ConcurrentStep<Span>(pub IntExpr<Span>);

#[doc = s_rule!(
    F18V007r1 rule "concurrent-step" #1128 : "is scalar-int-expr",
)]
pub fn concurrent_step<S: Lexed>(source: S) -> PResult<ConcurrentStep<MultilineSpan>, S> {
    int_expr.map(ConcurrentStep).parse(source)
}

#[derive(Debug, Clone)]
pub struct ConcurrentLocality<Span>(pub Vec<LocalitySpec<Span>>);

#[doc = s_rule!(
    F18V007r1 rule "concurrent-locality" #1129 : "is [ locality-spec ]...",
)]
pub fn concurrent_locality<S: Lexed>(source: S) -> PResult<ConcurrentLocality<MultilineSpan>, S> {
    many(locality_spec, 0..).map(ConcurrentLocality).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum LocalitySpec<Span> {
    Local(Vec<Name<Span>>),
    LocalInit(Vec<Name<Span>>),
    Shared(Vec<Name<Span>>),
    DefaultNone,
}

#[doc = s_rule!(
    F18V007r1 rule "locality-spec" #1130 :
    "is LOCAL ( variable-name-list )"
    "or LOCAL_INIT ( variable-name-list )"
    "or SHARED ( variable-name-list )"
    "or DEFAULT ( NONE )",
)]
pub fn locality_spec<S: Lexed>(source: S) -> PResult<LocalitySpec<MultilineSpan>, S> {
    alt!(
        for S =>
        seq!((
            _: kw!(local),
            _: delim('('),
            names: list(name(), 0..),
            _: delim(')'),
        ) => LocalitySpec::Local(names)),
        seq!((
            _: kw!(local_init),
            _: delim('('),
            names: list(name(), 0..),
            _: delim(')'),
        ) => LocalitySpec::LocalInit(names)),
        seq!((
            _: kw!(shared),
            _: delim('('),
            names: list(name(), 0..),
            _: delim(')'),
        ) => LocalitySpec::Shared(names)),
        seq!((
            _: kw!(default),
            _: delim('('),
            _: kw!(none),
            _: delim(')'),
        ) => LocalitySpec::DefaultNone),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct EndDoStmt<Span> {
    pub do_construct_name: Option<Name<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "end-do-stmt" #1132 : "is END DO [ do-construct-name ]",
)]
pub fn end_do_stmt<S: Lexed>(source: S) -> PResult<EndDoStmt<MultilineSpan>, S> {
    seq!((
        _: (kw!(end), kw!(do)),
        do_construct_name: name().opt(),
    ) => EndDoStmt {
        do_construct_name,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct CycleStmt<Span> {
    pub do_construct_name: Option<Name<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "cycle-stmt" #1133 : "is CYCLE [ do-construct-name ]",
)]
pub fn cycle_stmt<S: Lexed>(source: S) -> PResult<CycleStmt<MultilineSpan>, S> {
    seq!((
        _: kw!(cycle),
        do_construct_name: name().opt(),
    ) => CycleStmt {
        do_construct_name,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct IfThenStmt<Span> {
    pub if_construct_name: Option<Name<Span>>,
    pub scalar_logical_expr: Expr<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "if-then-stmt" #1135 : "is [ if-construct-name : ] IF ( scalar-logical-expr ) THEN",
)]
pub fn if_then_stmt<S: Lexed>(source: S) -> PResult<IfThenStmt<MultilineSpan>, S> {
    seq!((
        if_construct_name: seq!((n: name(), _: colon()) => n).opt(),
        _: kw!(if),
        _: delim('('),
        scalar_logical_expr: logical_expr,
        _: delim(')'),
        _: kw!(then),
    ) => IfThenStmt {
        if_construct_name,
        scalar_logical_expr,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct ElseIfStmt<Span> {
    pub scalar_logical_expr: Expr<Span>,
    pub if_construct_name: Option<Name<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "else-if-stmt" #1136 : "is ELSE IF ( scalar-logical-expr ) THEN [ if-construct-name ]",
)]
pub fn else_if_stmt<S: Lexed>(source: S) -> PResult<ElseIfStmt<MultilineSpan>, S> {
    seq!((
        _: (kw!(else), kw!(if), delim('(')),
        scalar_logical_expr: logical_expr,
        _: delim(')'),
        _: kw!(then),
        if_construct_name: name().opt(),
    ) => ElseIfStmt {
        scalar_logical_expr,
        if_construct_name,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct ElseStmt<Span> {
    pub if_construct_name: Option<Name<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "else-stmt" #1137 : "is ELSE [ if-construct-name ]",
)]
pub fn else_stmt<S: Lexed>(source: S) -> PResult<ElseStmt<MultilineSpan>, S> {
    seq!((
        _: kw!(else),
        if_construct_name: name().opt(),
    ) => ElseStmt {
        if_construct_name,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct EndIfStmt<Span> {
    pub if_construct_name: Option<Name<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "end-if-stmt" #1138 : "is END IF [ if-construct-name ]",
)]
pub fn end_if_stmt<S: Lexed>(source: S) -> PResult<EndIfStmt<MultilineSpan>, S> {
    seq!((
        _: seq!((_: kw!(end), _: kw!(if)) => ()),
        if_construct_name: name().opt(),
    ) => EndIfStmt {
        if_construct_name,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct IfStmt<Span> {
    pub scalar_logical_expr: Expr<Span>,
    pub action_stmt: Vec<ActionStmt<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "if-stmt" #1139 : "is IF ( scalar-logical-expr ) action-stmt",
)]
pub fn if_stmt<S: Lexed>(source: S) -> PResult<IfStmt<MultilineSpan>, S> {
    let (scalar_logical_expr, source) = seq!((
        _: kw!(if),
        _: delim('('),
        scalar_logical_expr: logical_expr,
        _: delim(')'),
    ) => scalar_logical_expr).parse(source)?;

    let possible_actions = action_stmt(source.clone())
        .into_iter()
        .map(|(action, _)| action)
        .collect::<Vec<_>>();

    if possible_actions.is_empty() {
        return None;
    }

    Some((IfStmt {
        scalar_logical_expr,
        action_stmt: possible_actions,
    }, source))
}

#[derive(Debug, Clone)]
pub struct SelectCaseStmt<Span> {
    pub case_construct_name: Option<Name<Span>>,
    pub case_expr: Expr<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "select-case-stmt" #1141 : "is [ case-construct-name : ] SELECT CASE ( case-expr )",
)]
pub fn select_case_stmt<S: Lexed>(source: S) -> PResult<SelectCaseStmt<MultilineSpan>, S> {
    seq!((
        case_construct_name: seq!((n: name(), _: colon()) => n).opt(),
        _: (kw!(select), kw!(case), delim('(')),
        case_expr: expr,
        _: delim(')'),
    ) => SelectCaseStmt {
        case_construct_name,
        case_expr,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct CaseStmt<Span> {
    pub case_selector: CaseSelector<Span>,
    pub case_construct_name: Option<Name<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "case-stmt" #1142 : "is CASE case-selector [case-construct-name]",
)]
pub fn case_stmt<S: Lexed>(source: S) -> PResult<CaseStmt<MultilineSpan>, S> {
    seq!((
        _: kw!(case),
        case_selector: case_selector,
        case_construct_name: name().opt(),
    ) => CaseStmt {
        case_selector,
        case_construct_name,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct EndSelectStmt<Span> {
    pub case_construct_name: Option<Name<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "end-select-stmt" #1143 : "is END SELECT [ case-construct-name ]",
)]
pub fn end_select_stmt<S: Lexed>(source: S) -> PResult<EndSelectStmt<MultilineSpan>, S> {
    seq!((
        _: seq!((_: kw!(end), _: kw!(select)) => ()),
        case_construct_name: name().opt(),
    ) => EndSelectStmt {
        case_construct_name,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct CaseExpr<Span>(pub Expr<Span>);

#[doc = s_rule!(
    F18V007r1 rule "case-expr" #1144 : "is scalar-expr",
)]
pub fn case_expr<S: Lexed>(source: S) -> PResult<CaseExpr<MultilineSpan>, S> {
    expr.map(CaseExpr).parse(source)
}

#[derive(Debug, Clone)]
pub enum CaseSelector<Span> {
    ValueRangeList(Vec<CaseValueRange<Span>>),
    Default,
}

#[doc = s_rule!(
    F18V007r1 rule "case-selector" #1145 :
    "is ( case-value-range-list )"
    "or DEFAULT",
)]
pub fn case_selector<S: Lexed>(source: S) -> PResult<CaseSelector<MultilineSpan>, S> {
    alt!(
        for S =>
        seq!((
            _: delim('('),
            value_range_list: list(case_value_range, 1..),
            _: delim(')'),
        ) => CaseSelector::ValueRangeList(value_range_list)),
        kw!(default).map(|_| CaseSelector::Default),
    ).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum CaseValueRange<Span> {
    Singe(CaseValue<Span>),
    Lower(CaseValue<Span>),
    Upper(CaseValue<Span>),
    Range(CaseValue<Span>, CaseValue<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "case-value-range" #1146 :
    "is case-value"
    "or case-value :"
    "or : case-value"
    "or case-value : case-value",
)]
pub fn case_value_range<S: Lexed>(source: S) -> PResult<CaseValueRange<MultilineSpan>, S> {
    alt!(
        for S =>
        case_value.map(CaseValueRange::Singe),
        seq!((
            value: case_value,
            _: colon()
        ) => CaseValueRange::Lower(value)),
        seq!((
            _: colon(),
            value: case_value,
        ) => CaseValueRange::Upper(value)),
        seq!((
            value1: case_value,
            _: colon(),
            value2: case_value,
        ) => CaseValueRange::Range(value1, value2)),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct CaseValue<Span>(pub ConstantExpr<Span>);

#[doc = s_rule!(
    F18V007r1 rule "case-value" #1147 : "is scalar-constant-expr",
)]
pub fn case_value<S: Lexed>(source: S) -> PResult<CaseValue<MultilineSpan>, S> {
    constant_expr.map(CaseValue).parse(source)
}

pub struct SelectRankStmt<Span> {
    pub select_construct_name: Option<Name<Span>>,
    pub associate_name: Option<Name<Span>>,
    pub selector: Selector<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "select-rank-stmt" #1149 :
    "is [ select-construct-name : ] SELECT RANK"
    "    ( [ associate-name => ] selector )",
)]
pub fn select_rank_stmt<S: Lexed>(source: S) -> PResult<SelectRankStmt<MultilineSpan>, S> {
    seq!((
        select_construct_name: seq!((n: name(), _: colon()) => n).opt(),
        _: (kw!(select), kw!(rank), delim('(')),
        associate_name: seq!((n: name(), _: arrow()) => n).opt(),
        selector: selector,
        _: delim(')'),
    ) => SelectRankStmt {
        select_construct_name,
        associate_name,
        selector,
    }).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum SelectRankCaseStmt<Span> {
    Rank(IntConstantExpr<Span>, Option<Name<Span>>),
    RankStar(Option<Name<Span>>),
    RankDefault(Option<Name<Span>>),
}

#[doc = s_rule!(
    F18V007r1 rule "select-rank-case-stmt" #1150 :
    "is RANK ( scalar-int-constant-expr ) [ select-construct-name ]"
    "or RANK ( * ) [ select-construct-name ]"
    "or RANK DEFAULT [ select-construct-name ]",
)]
pub fn select_rank_case_stmt<S: Lexed>(source: S) -> PResult<SelectRankCaseStmt<MultilineSpan>, S> {
    alt!(
        for S =>
        seq!((
            _: kw!(rank),
            _: delim('('),
            rank: int_constant_expr,
            _: delim(')'),
            select_construct_name: name().opt(),
        ) => SelectRankCaseStmt::Rank(rank, select_construct_name)),
        seq!((
            _: kw!(rank),
            _: delim('('),
            _: asterisk(),
            _: delim(')'),
            select_construct_name: name().opt(),
        ) => SelectRankCaseStmt::RankStar(select_construct_name)),
        seq!((
            _: kw!(rank),
            _: kw!(default),
            _: delim(')'),
            select_construct_name: name().opt(),
        ) => SelectRankCaseStmt::RankDefault(select_construct_name)),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct EndSelectRankStmt<Span> {
    pub select_construct_name: Option<Name<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "end-select-rank-stmt" #1151 : "is END SELECT [ select-construct-name ]",
)]
pub fn end_select_rank_stmt<S: Lexed>(source: S) -> PResult<EndSelectRankStmt<MultilineSpan>, S> {
    seq!((
        _: seq!((_: kw!(end), _: kw!(select)) => ()),
        select_construct_name: name().opt(),
    ) => EndSelectRankStmt {
        select_construct_name,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct SelectTypeStmt<Span> {
    pub select_construct_name: Option<Name<Span>>,
    pub associate_name: Option<Name<Span>>,
    pub selector: Selector<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "select-type-stmt" #1153 :
    "is [ select-construct-name : ] SELECT TYPE"
    "    ( [ associate-name => ] selector )",
)]
pub fn select_type_stmt<S: Lexed>(source: S) -> PResult<SelectTypeStmt<MultilineSpan>, S> {
    seq!((
        select_construct_name: seq!((n: name(), _: colon()) => n).opt(),
        _: (kw!(select), kw!(type), delim('(')),
        associate_name: seq!((n: name(), _: arrow()) => n).opt(),
        selector: selector,
        _: delim(')'),
    ) => SelectTypeStmt {
        select_construct_name,
        associate_name,
        selector,
    }).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum TypeGuardStmt<Span> {
    TypeIs(TypeSpec<Span>, Option<Name<Span>>),
    ClassIs(DerivedTypeSpec<Span>, Option<Name<Span>>),
    ClassDefault(Option<Name<Span>>),
}

#[doc = s_rule!(
    F18V007r1 rule "type-guard-stmt" #1154 :
    "is TYPE IS ( type-spec ) [ select-construct-name ]"
    "or CLASS IS ( derived-type-spec ) [ select-construct-name ]"
    "or CLASS DEFAULT [ select-construct-name ]",
)]
pub fn type_guard_stmt<S: Lexed>(source: S) -> PResult<TypeGuardStmt<MultilineSpan>, S> {
    alt!(
        for S =>
        seq!((
            _: kw!(type), _: kw!(is),
            _: delim('('),
            type_spec: type_spec,
            _: delim(')'),
            select_construct_name: name().opt(),
        ) => TypeGuardStmt::TypeIs(type_spec, select_construct_name)),
        seq!((
            _: kw!(class), _: kw!(is),
            _: delim('('),
            derived_type_spec: derived_type_spec,
            _: delim(')'),
            select_construct_name: name().opt(),
        ) => TypeGuardStmt::ClassIs(derived_type_spec, select_construct_name)),
        seq!((
            _: kw!(class), _: kw!(default),
            select_construct_name: name().opt(),
        ) => TypeGuardStmt::ClassDefault(select_construct_name)),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct EndSelectTypeStmt<Span> {
    pub select_construct_name: Option<Name<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "end-select-type-stmt" #1155 : "is END SELECT [ select-construct-name ]",
)]
pub fn end_select_type_stmt<S: Lexed>(source: S) -> PResult<EndSelectTypeStmt<MultilineSpan>, S> {
    seq!((
        _: kw!(end), _: kw!(select),
        select_construct_name: name().opt(),
    ) => EndSelectTypeStmt {
        select_construct_name,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct ExitStmt<Span> {
    pub construct_name: Option<Name<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "exit-stmt" #1156 : "is EXIT [ construct-name ]",
)]
pub fn exit_stmt<S: Lexed>(source: S) -> PResult<ExitStmt<MultilineSpan>, S> {
    seq!((
        _: kw!(exit),
        construct_name: name().opt(),
    ) => ExitStmt {
        construct_name,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct GotoStmt<Span> {
    pub label: Label<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "goto-stmt" #1157 : "is GO TO label",
)]
pub fn goto_stmt<S: Lexed>(source: S) -> PResult<GotoStmt<MultilineSpan>, S> {
    seq!((
        _: kw!(go),
        _: kw!(to),
        label: label(),
    ) => GotoStmt {
        label,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct ComputedGotoStmt<Span> {
    pub label_list: Vec<Label<Span>>,
    pub scalar_int_expression: Expr<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "computed-goto-stmt" #1158 : "is GO TO ( label-list ) [ , ] scalar-int-expression",
)]
pub fn computed_goto_stmt<S: Lexed>(source: S) -> PResult<ComputedGotoStmt<MultilineSpan>, S> {
    seq!((
        _: kw!(go),
        _: kw!(to),
        _: delim('('),
        label_list: list(label(), 1..),
        _: delim(')'),
        _: comma().opt(),
        scalar_int_expression: expr,
    ) => ComputedGotoStmt {
        label_list,
        scalar_int_expression,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct ContinueStmt<Span> {
    pub label: Option<Label<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "continue-stmt" #1159 : "is CONTINUE",
)]
pub fn continue_stmt<S: Lexed>(source: S) -> PResult<ContinueStmt<MultilineSpan>, S> {
    seq!((
        _: kw!(continue),
        label: label().opt()
    ) => ContinueStmt {
        label
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct StopStmt<Span> {
    pub stop_code: Option<StopCode<Span>>,
    pub quiet: Option<Expr<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "stop-stmt" #1160 : "is STOP [ stop-code ] [ , QUIET = scalar-logical-expr]",
)]
pub fn stop_stmt<S: Lexed>(source: S) -> PResult<StopStmt<MultilineSpan>, S> {
    seq!((
        _: kw!(stop),
        stop_code: stop_code.opt(),
        quiet: seq!((
            _: comma(),
            _: kw!(quiet),
            _: equals(),
            quiet: logical_expr
        ) => quiet).opt(),
    ) => StopStmt {
        stop_code,
        quiet,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct ErrorStopStmt<Span> {
    pub stop_code: Option<StopCode<Span>>,
    pub quiet: Option<Expr<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "error-stop-stmt" #1161 : "is ERROR STOP [ stop-code ] [ , QUIET = scalar-logical-expr",
)]
pub fn error_stop_stmt<S: Lexed>(source: S) -> PResult<ErrorStopStmt<MultilineSpan>, S> {
    seq!((
        _: kw!(error),
        _: kw!(stop),
        stop_code: stop_code.opt(),
        _: comma(),
        _: kw!(quiet),
        _: equals(),
        quiet: logical_expr.opt(),
    ) => ErrorStopStmt {
        stop_code,
        quiet,
    }).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum StopCode<Span> {
    ScalarDefaultCharExpr(DefaultCharExpr<Span>),
    ScalarIntExpr(IntExpr<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "stop-code" #1162 :
    "is scalar-default-char-expr"
    "or scalar-int-expr",
)]
pub fn stop_code<S: Lexed>(source: S) -> PResult<StopCode<MultilineSpan>, S> {
    alt!(
        for S =>
        default_char_expr.map(StopCode::ScalarDefaultCharExpr),
        int_expr.map(StopCode::ScalarIntExpr),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct FailImageStmt<Span> {
    _p: std::marker::PhantomData<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "fail-image-stmt" #1163 : "is FAIL IMAGE",
)]
pub fn fail_image_stmt<S: Lexed>(source: S) -> PResult<FailImageStmt<MultilineSpan>, S> {
    seq!((
        _: kw!(fail),
        _: kw!(image),
    ) => FailImageStmt {
        _p: std::marker::PhantomData,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct SyncAllStmt<Span> {
    pub sync_stat_list: Option<Vec<SyncStat<Span>>>,
}

#[doc = s_rule!(
    F18V007r1 rule "sync-all-stmt" #1164 : "is SYNC ALL [ ( [ sync-stat-list ] ) ]",
)]
pub fn sync_all_stmt<S: Lexed>(source: S) -> PResult<SyncAllStmt<MultilineSpan>, S> {
    seq!((
        _: kw!(sync),
        _: kw!(all),
        sync_stat_list: seq!((
            _: delim('('),
            sync_stat_list: list(sync_stat, 0..),
            _: delim(')'),
        ) => sync_stat_list).opt(),
    ) => SyncAllStmt {
        sync_stat_list,
    }).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum SyncStat<Span> {
    Stat(StatVariable<Span>),
    ErrMsg(ErrmsgVariable<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "sync-stat" #1165 :
    "is STAT = stat-variable"
    "or ERRMSG = errmsg-variable",
)]
pub fn sync_stat<S: Lexed>(source: S) -> PResult<SyncStat<MultilineSpan>, S> {
    alt!(
        for S =>
        seq!((
            _: kw!(stat),
            _: equals(),
            stat_variable: stat_variable,
        ) => SyncStat::Stat(stat_variable)),
        seq!((
            _: kw!(errmsg),
            _: equals(),
            errmsg_variable: errmsg_variable,
        ) => SyncStat::ErrMsg(errmsg_variable)),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct SyncImagesStmt<Span> {
    pub image_set: ImageSet<Span>,
    pub sync_stat_list: Option<Vec<SyncStat<Span>>>,
}

#[doc = s_rule!(
    F18V007r1 rule "sync-images-stmt" #1166 : "is SYNC IMAGES ( image-set [ , sync-stat-list ] )",
)]
pub fn sync_images_stmt<S: Lexed>(source: S) -> PResult<SyncImagesStmt<MultilineSpan>, S> {
    seq!((
        _: kw!(sync),
        _: kw!(images),
        _: delim('('),
        image_set: image_set,
        sync_stat_list: seq!((
            _: comma(),
            sync_stat_list: list(sync_stat, 0..),
        ) => sync_stat_list).opt(),
        _: delim(')'),
    ) => SyncImagesStmt {
        image_set,
        sync_stat_list,
    }).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ImageSet<Span> {
    IntExpr(IntExpr<Span>),
    Star,
}

#[doc = s_rule!(
    F18V007r1 rule "image-set" #1167 :
    "is int-expr"
    "or *",
)]
pub fn image_set<S: Lexed>(source: S) -> PResult<ImageSet<MultilineSpan>, S> {
    alt!(
        for S =>
        int_expr.map(ImageSet::IntExpr),
        asterisk().map(|_| ImageSet::Star),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct SyncMemoryStmt<Span> {
    pub sync_stat_list: Option<Vec<SyncStat<Span>>>,
}

#[doc = s_rule!(
    F18V007r1 rule "sync-memory-stmt" #1168 : "is SYNC MEMORY [ ( [ sync-stat-list ] ) ]",
)]
pub fn sync_memory_stmt<S: Lexed>(source: S) -> PResult<SyncMemoryStmt<MultilineSpan>, S> {
    seq!((
        _: kw!(sync),
        _: kw!(memory),
        sync_stat_list: seq!((
            _: delim('('),
            sync_stat_list: list(sync_stat, 0..),
            _: delim(')'),
        ) => sync_stat_list).opt(),
    ) => SyncMemoryStmt {
        sync_stat_list,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct SyncTeamStmt<Span> {
    pub team_value: TeamValue<Span>,
    pub sync_stat_list: Option<Vec<SyncStat<Span>>>,
}

#[doc = s_rule!(
    F18V007r1 rule "sync-team-stmt" #1169 : "is SYNC TEAM ( team-value [ , sync-stat-list ] )",
)]
pub fn sync_team_stmt<S: Lexed>(source: S) -> PResult<SyncTeamStmt<MultilineSpan>, S> {
    seq!((
        _: kw!(sync),
        _: kw!(team),
        _: delim('('),
        team_value: team_value,
        sync_stat_list: seq!((
            _: comma(),
            sync_stat_list: list(sync_stat, 0..),
        ) => sync_stat_list).opt(),
        _: delim(')'),
    ) => SyncTeamStmt {
        team_value,
        sync_stat_list,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct EventPostStmt<Span> {
    pub event_variable: EventVariable<Span>,
    pub sync_stat_list: Option<Vec<SyncStat<Span>>>,
}

#[doc = s_rule!(
    F18V007r1 rule "event-post-stmt" #1170 : "is EVENT POST ( event-variable [ , sync-stat-list ] )",
)]
pub fn event_post_stmt<S: Lexed>(source: S) -> PResult<EventPostStmt<MultilineSpan>, S> {
    seq!((
        _: kw!(event),
        _: kw!(post),
        _: delim('('),
        event_variable: event_variable,
        sync_stat_list: seq!((
            _: comma(),
            sync_stat_list: list(sync_stat, 0..),
        ) => sync_stat_list).opt(),
        _: delim(')'),
    ) => EventPostStmt {
        event_variable,
        sync_stat_list,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct EventVariable<Span>(pub Variable<Span>);

#[doc = s_rule!(
    F18V007r1 rule "event-variable" #1171 : "is scalar-variable",
)]
pub fn event_variable<S: Lexed>(source: S) -> PResult<EventVariable<MultilineSpan>, S> {
    variable(false).map(EventVariable).parse(source)
}

#[derive(Debug, Clone)]
pub struct EventWaitStmt<Span> {
    pub event_variable: EventVariable<Span>,
    pub event_wait_spec_list: Vec<EventWaitSpec<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "event-wait-stmt" #1172 : "is EVENT WAIT ( event-variable [ , event-wait-spec-list ] )",
)]
pub fn event_wait_stmt<S: Lexed>(source: S) -> PResult<EventWaitStmt<MultilineSpan>, S> {
    seq!((
        _: kw!(event),
        _: kw!(wait),
        _: delim('('),
        event_variable: event_variable,
        event_wait_spec_list: seq!((
            _: comma(),
            event_wait_spec_list: list(event_wait_spec, 1..),
        ) => event_wait_spec_list),
        _: delim(')'),
    ) => EventWaitStmt {
        event_variable,
        event_wait_spec_list,
    }).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum EventWaitSpec<Span> {
    UntilSpec(UntilSpec<Span>),
    SyncStat(SyncStat<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "event-wait-spec" #1173 :
    "is until-spec"
    "or sync-stat",
)]
pub fn event_wait_spec<S: Lexed>(source: S) -> PResult<EventWaitSpec<MultilineSpan>, S> {
    alt!(
        for S =>
        until_spec.map(EventWaitSpec::UntilSpec),
        sync_stat.map(EventWaitSpec::SyncStat),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct UntilSpec<Span> {
    pub until_count: IntExpr<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "until-spec" #1174 : "is UNTIL_COUNT = scalar-int-expr",
)]
pub fn until_spec<S: Lexed>(source: S) -> PResult<UntilSpec<MultilineSpan>, S> {
    seq!((
        _: kw!(until),
        _: kw!(count),
        _: equals(),
        until_count: int_expr,
    ) => UntilSpec {
        until_count,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct FormTeamStmt<Span> {
    pub team_number: IntExpr<Span>,
    pub team_variable: Variable<Span>,
    pub form_team_spec_list: Option<Vec<FormTeamSpec<Span>>>,
}

#[doc = s_rule!(
    F18V007r1 rule "form-team-stmt" #1175 :
    "is FORM TEAM ( team-number, team-variable"
    "    [ , form-team-spec-list ] )",
)]
pub fn form_team_stmt<S: Lexed>(source: S) -> PResult<FormTeamStmt<MultilineSpan>, S> {
    seq!((
        _: kw!(form),
        _: kw!(team),
        _: delim('('),
        team_number: int_expr,
        _: comma(),
        team_variable: variable(false),
        form_team_spec_list: seq!((
            _: comma(),
            form_team_spec_list: list(form_team_spec, 1..),
        ) => form_team_spec_list).opt(),
        _: delim(')'),
    ) => FormTeamStmt {
        team_number,
        team_variable,
        form_team_spec_list,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct TeamNumber<Span>(pub IntExpr<Span>);

#[doc = s_rule!(
    F18V007r1 rule "team-number" #1176 : "is scalar-int-expr",
)]
pub fn team_number<S: Lexed>(source: S) -> PResult<TeamNumber<MultilineSpan>, S> {
    int_expr.map(TeamNumber).parse(source)
}

#[derive(Debug, Clone)]
pub struct TeamVariable<Span>(pub Variable<Span>);

#[doc = s_rule!(
    F18V007r1 rule "team-variable" #1177 : "is scalar-variable",
)]
pub fn team_variable<S: Lexed>(source: S) -> PResult<TeamVariable<MultilineSpan>, S> {
    variable(false).map(TeamVariable).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum FormTeamSpec<Span> {
    NewIndex(IntExpr<Span>),
    SyncStat(SyncStat<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "form-team-spec" #1178 :
    "is NEW_INDEX = scalar-int-expr"
    "or sync-stat",
)]
pub fn form_team_spec<S: Lexed>(source: S) -> PResult<FormTeamSpec<MultilineSpan>, S> {
    alt!(
        for S =>
        seq!((
            _: kw!(new),
            _: kw!(index),
            _: equals(),
            new_index: int_expr,
        ) => FormTeamSpec::NewIndex(new_index)),
        sync_stat.map(FormTeamSpec::SyncStat),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct LockStmt<Span> {
    pub lock_variable: Variable<Span>,
    pub lock_stat_list: Option<Vec<LockStat<Span>>>,
}

#[doc = s_rule!(
    F18V007r1 rule "lock-stmt" #1179 : "is LOCK ( lock-variable [ , lock-stat-list ] )",
)]
pub fn lock_stmt<S: Lexed>(source: S) -> PResult<LockStmt<MultilineSpan>, S> {
    seq!((
        _: kw!(lock),
        _: delim('('),
        lock_variable: variable(false),
        lock_stat_list: seq!((
            _: comma(),
            lock_stat_list: list(lock_stat, 1..),
        ) => lock_stat_list).opt(),
        _: delim(')'),
    ) => LockStmt {
        lock_variable,
        lock_stat_list,
    }).parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum LockStat<Span> {
    AcquiredLock(LogicalVariable<Span>),
    SyncStat(SyncStat<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "lock-stat" #1180 :
    "is ACQUIRED_LOCK = scalar-logical-variable"
    "or sync-stat",
)]
pub fn lock_stat<S: Lexed>(source: S) -> PResult<LockStat<MultilineSpan>, S> {
    alt!(
        for S =>
        seq!((
            _: kw!(acquired),
            _: kw!(lock),
            _: equals(),
            acquired_lock: logical_variable(false),
        ) => LockStat::AcquiredLock(acquired_lock)),
        sync_stat.map(LockStat::SyncStat),
    ).parse(source)
}

#[derive(Debug, Clone)]
pub struct UnlockStmt<Span> {
    pub lock_variable: LockVariable<Span>,
    pub sync_stat_list: Option<Vec<SyncStat<Span>>>,
}

#[doc = s_rule!(
    F18V007r1 rule "unlock-stmt" #1181 : "is UNLOCK ( lock-variable [ , sync-stat-list ] )",
)]
pub fn unlock_stmt<S: Lexed>(source: S) -> PResult<UnlockStmt<MultilineSpan>, S> {
    seq!((
        _: kw!(UNLOCK),
        _: delim('('),
        lock_variable: lock_variable,
        sync_stat_list: seq!((
            _: comma(),
            sync_stat_list: list(sync_stat, 0..),
        ) => sync_stat_list).opt(),
        _: delim(')'),
    ) => UnlockStmt {
        lock_variable,
        sync_stat_list,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct LockVariable<Span>(pub Variable<Span>);

#[doc = s_rule!(
    F18V007r1 rule "lock-variable" #1182 : "is scalar-variable",
)]
pub fn lock_variable<S: Lexed>(source: S) -> PResult<LockVariable<MultilineSpan>, S> {
    variable(false).map(LockVariable).parse(source)
}
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
    (
        (
            name(),
            colon(),
        ).map(|(name, _)| name).optional(),
        kw!(associate),
        delim('('),
        list(association, 0..),
        delim(')'),
    ).map(|(associate_construct_name, _, _, association_list, _)| AssociateStmt {
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
    (
        name(),
        arrow(),
        selector,
    ).map(|(associate_name, _, selector)| Association {
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
    (
        (
            kw!(end),
            kw!(associate),
        ),
        name().optional(),
    ).map(|(_, associate_construct_name)| EndAssociateStmt {
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
    (
        (
            name(),
            colon(),
        ).map(|(name, _)| name).optional(),
        kw!(block),
    ).map(|(block_construct_name, _)| BlockStmt {
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
    (
        (
            kw!(end),
            kw!(block),
        ),
        name().optional(),
    ).map(|(_, block_construct_name)| EndBlockStmt {
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
    (
        (name(), colon()).map(|(name, _)| name).optional(),
        (kw!(change), kw!(team), delim('(')),
        team_value,
        (
            comma(),
            list(coarray_association, 0..),
        ).map(|(_, coarray_association_list)| coarray_association_list).optional(),
        (
            comma(),
            list(sync_stat, 0..),
        ).map(|(_, sync_stat_list)| sync_stat_list).optional(),
        delim(')')
    ).map(|(team_construct_name, _, team_value, coarray_association_list, sync_stat_list, _)| ChangeTeamStmt {
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
    (
        codimension_decl,
        arrow(),
        selector,
    ).map(|(codimension_decl, _, selector)| CoarrayAssociation {
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
    (
        (kw!(end), kw!(team)),
        (
            delim('('),
            list(sync_stat, 0..),
            delim(')'),
        ).map(|(_, sync_stat_list, _)| sync_stat_list).optional(),
        name().optional(),
    ).map(|(_, sync_stat_list, team_construct_name)| EndChangeTeamStmt {
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
    (
        (name(), colon()).map(|(name, _)| name).optional(),
        kw!(critical),
        (
            delim('('),
            list(sync_stat, 0..),
            delim(')'),
        ).map(|(_, sync_stat_list, _)| sync_stat_list).optional(),
    ).map(|(critical_construct_name, _, sync_stat_list)| CriticalStmt {
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
    (
        (kw!(end), kw!(critical)),
        name().optional(),
    ).map(|(_, critical_construct_name)| EndCriticalStmt {
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
    (
        (name(), colon()).map(|(name, _)| name).optional(),
        kw!(do),
        label(),
        loop_control.optional(),
    ).map(|(do_construct_name, _, label, loop_control)| LabelDoStmt {
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
    (
        (name(), colon()).map(|(name, _)| name).optional(),
        kw!(do),
        loop_control,
    ).map(|(do_construct_name, _, loop_control)| NonlabelDoStmt {
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
        (
            comma().optional(),
            do_variable, equals(),
            int_expr, comma(),
            int_expr,
            (comma(), int_expr).map(|(_,c)| c).optional(),
        ).map(|(_, do_variable, _, a, _, b, c)| LoopControl::DoVariable {
            do_variable,
            a,
            b,
            c,
        }),
        (
            comma().optional(),
            kw!(while), delim('('),
            logical_expr,
            delim(')'),
        ).map(|(_, _, _, expr, _)| LoopControl::While(expr)),
        (
            comma().optional(),
            concurrent_header,
            concurrent_locality.optional(),
        ).map(|(_, concurrent_header, concurrent_locality)| LoopControl::Concurrent {
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
    (
        delim('('),
        (
            integer_type_spec,
            double_colon(),
        ).map(|(integer_type_spec, _)| integer_type_spec).optional(),
        list(concurrent_control, 1..),
        (comma(), mask_expr).map(|(_, scalar_mask_expr)| scalar_mask_expr).optional(),
        delim(')'),
    ).map(|(_, integer_type_spec, concurrent_control_list, scalar_mask_expr, _)| ConcurrentHeader {
        integer_type_spec,
        concurrent_control_list,
        scalar_mask_expr,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct ConcurrentControl<Span> {
    pub index_name: Name<Span>,
    pub concurrent_limit: IntExpr<Span>,
    pub concurrent_step: Option<IntExpr<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "concurrent-control" #1126 : "is index-name = concurrent-limit : concurrent-limit [ : concurrent-step ]",
)]
pub fn concurrent_control<S: Lexed>(source: S) -> PResult<ConcurrentControl<MultilineSpan>, S> {
    (
        name(),
        equals(),
        int_expr,
        colon(),
        int_expr,
        (colon(), int_expr).map(|(_, concurrent_step)| concurrent_step).optional(),
    ).map(|(index_name, _, concurrent_limit, _, _, concurrent_step)| ConcurrentControl {
        index_name,
        concurrent_limit,
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
        (
            kw!(local), delim('('),
            list(name(), 0..),
            delim(')'),
        ).map(|(_, _, names, _)| LocalitySpec::Local(names)),
        (
            kw!(local_init), delim('('),
            list(name(), 0..),
            delim(')'),
        ).map(|(_, _, names, _)| LocalitySpec::LocalInit(names)),
        (
            kw!(shared), delim('('),
            list(name(), 0..),
            delim(')'),
        ).map(|(_, _, names, _)| LocalitySpec::Shared(names)),
        (
            kw!(default), delim('('),
            kw!(none),
            delim(')'),
        ).map(|(_, _, _, _)| LocalitySpec::DefaultNone),
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
    (
        (kw!(end), kw!(do)),
        name().optional(),
    ).map(|(_, do_construct_name)| EndDoStmt {
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
    (
        kw!(cycle),
        name().optional(),
    ).map(|(_, do_construct_name)| CycleStmt {
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
    (
        (name(), colon()).map(|(name, _)| name).optional(),
        kw!(if), delim('('),
        logical_expr,
        delim(')'),
        kw!(then),
    ).map(|(if_construct_name, _, _, scalar_logical_expr, _, _)| IfThenStmt {
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
    (
        (kw!(else), kw!(if), delim('(')),
        logical_expr,
        delim(')'),
        kw!(then),
        name().optional(),
    ).map(|(_, scalar_logical_expr, _, _, if_construct_name)| ElseIfStmt {
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
    (
        kw!(else),
        name().optional(),
    ).map(|(_, if_construct_name)| ElseStmt {
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
    (
        (kw!(end), kw!(if)),
        name().optional(),
    ).map(|(_, if_construct_name)| EndIfStmt {
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
    let ((_, _, scalar_logical_expr, _), source) = (
        kw!(if), delim('('),
        logical_expr,
        delim(')'),
    ).parse(source)?;

    let possible_actions = action_stmt(source.clone()).into_iter().map(|(action, _)| action).collect::<Vec<_>>();

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
    (
        (name(), colon()).map(|(name, _)| name).optional(),
        (kw!(select), kw!(case), delim('(')),
        expr,
        delim(')'),
    ).map(|(case_construct_name, _, case_expr, _)| SelectCaseStmt {
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
    (
        kw!(case),
        case_selector,
        name().optional(),
    ).map(|(_, case_selector, case_construct_name)| CaseStmt {
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
    (
        (kw!(end), kw!(select)),
        name().optional(),
    ).map(|(_, case_construct_name)| EndSelectStmt {
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
        (
            delim('('),
            list(case_value_range, 1..),
            delim(')'),
        ).map(|(_, value_range_list, _)| CaseSelector::ValueRangeList(value_range_list)),
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
        (
            case_value, colon()
        ).map(|(value, _)| CaseValueRange::Lower(value)),
        (
            colon(), case_value,
        ).map(|(_, value)| CaseValueRange::Upper(value)),
        (
            case_value, colon(), case_value,
        ).map(|(value1, _, value2)| CaseValueRange::Range(value1, value2)),
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
    (
        (name(), colon()).map(|(name, _)| name).optional(),
        (kw!(select), kw!(rank), delim('(')),
        (name(), arrow()).map(|(name, _)| name).optional(),
        selector,
        delim(')'),
    ).map(|(select_construct_name, _, associate_name, selector, _)| SelectRankStmt {
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
        (
            kw!(rank), delim('('),
            int_constant_expr,
            delim(')'),
            name().optional(),
        ).map(|(_, _, rank, _, select_construct_name)| SelectRankCaseStmt::Rank(rank, select_construct_name)),
        (
            kw!(rank), delim('('),
            asterisk(),
            delim(')'),
            name().optional(),
        ).map(|(_, _, _, _, select_construct_name)| SelectRankCaseStmt::RankStar(select_construct_name)),
        (
            kw!(rank), kw!(default),
            delim(')'),
            name().optional(),
        ).map(|(_, _, _, select_construct_name)| SelectRankCaseStmt::RankDefault(select_construct_name)),
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
    (
        (kw!(end), kw!(select)),
        name().optional(),
    ).map(|(_, select_construct_name)| EndSelectRankStmt {
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
    (
        (name(), colon()).map(|(name, _)| name).optional(),
        (kw!(select), kw!(type), delim('(')),
        (name(), arrow()).map(|(name, _)| name).optional(),
        selector,
        delim(')'),
    ).map(|(select_construct_name, _, associate_name, selector, _)| SelectTypeStmt {
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
        (
            (kw!(type), kw!(is), delim('(')),
            type_spec,
            delim(')'),
            name().optional(),
        ).map(|(_, type_spec, _, select_construct_name)| TypeGuardStmt::TypeIs(type_spec, select_construct_name)),
        (
            (kw!(class), kw!(is), delim('(')),
            derived_type_spec,
            delim(')'),
            name().optional(),
        ).map(|(_, derived_type_spec, _, select_construct_name)| TypeGuardStmt::ClassIs(derived_type_spec, select_construct_name)),
        (
            (kw!(class), kw!(default)),
            name().optional(),
        ).map(|(_, select_construct_name)| TypeGuardStmt::ClassDefault(select_construct_name)),
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
    (
        (kw!(end), kw!(select)),
        name().optional(),
    ).map(|(_, select_construct_name)| EndSelectTypeStmt {
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
    (
        kw!(exit),
        name().optional(),
    ).map(|(_, construct_name)| ExitStmt {
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
    (
        kw!(go),
        kw!(to),
        label(),
    ).map(|(_, _, label)| GotoStmt {
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
    (
        kw!(go),
        kw!(to),
        delim('('),
        list(label(), 1..),
        delim(')'),
        comma().optional(),
        expr,
    ).map(|(_, _, _, label_list, _, _, scalar_int_expression)| ComputedGotoStmt {
        label_list,
        scalar_int_expression,
    }).parse(source)
}

#[derive(Debug, Clone)]
pub struct ContinueStmt<Span> {
    _p: std::marker::PhantomData<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "continue-stmt" #1159 : "is CONTINUE",
)]
pub fn continue_stmt<S: Lexed>(source: S) -> PResult<ContinueStmt<MultilineSpan>, S> {
    (
        kw!(continue),
    ).map(|_| ContinueStmt {
        _p: std::marker::PhantomData,
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
    (
        kw!(stop),
        stop_code.optional(),
        (
            (comma(), kw!(quiet), equals()),
            logical_expr
        ).map(|(_, quiet)| quiet).optional(),
    ).map(|(_, stop_code, quiet)| StopStmt {
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
    (
        kw!(error),
        kw!(stop),
        stop_code.optional(),
        (comma(), kw!(quiet), equals()),
        logical_expr.optional(),
    ).map(|(_, _, stop_code, _, quiet)| ErrorStopStmt {
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
    (
        kw!(fail),
        kw!(image),
    ).map(|_| FailImageStmt {
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
    (
        kw!(sync),
        kw!(all),
        (
            delim('('),
            list(sync_stat, 0..),
            delim(')'),
        ).map(|(_, sync_stat_list, _)| sync_stat_list).optional(),
    ).map(|(_, _, sync_stat_list)| SyncAllStmt {
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
        (
            kw!(stat), equals(),
            stat_variable,
        ).map(|(_, _, stat_variable)| SyncStat::Stat(stat_variable)),
        (
            kw!(errmsg), equals(),
            errmsg_variable,
        ).map(|(_, _, errmsg_variable)| SyncStat::ErrMsg(errmsg_variable)),
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
    (
        kw!(sync),
        kw!(images),
        delim('('),
        image_set,
        (
            comma(),
            list(sync_stat, 0..),
        ).map(|(_, sync_stat_list)| sync_stat_list).optional(),
        delim(')'),
    ).map(|(_, _, _, image_set, sync_stat_list, _)| SyncImagesStmt {
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
    (
        kw!(sync),
        kw!(memory),
        (
            delim('('),
            list(sync_stat, 0..),
            delim(')'),
        ).map(|(_, sync_stat_list, _)| sync_stat_list).optional(),
    ).map(|(_, _, sync_stat_list)| SyncMemoryStmt {
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
    (
        kw!(sync),
        kw!(team),
        delim('('),
        team_value,
        (
            comma(),
            list(sync_stat, 0..),
        ).map(|(_, sync_stat_list)| sync_stat_list).optional(),
        delim(')'),
    ).map(|(_, _, _, team_value, sync_stat_list, _)| SyncTeamStmt {
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
    (
        kw!(event),
        kw!(post),
        delim('('),
        event_variable,
        (
            comma(),
            list(sync_stat, 0..),
        ).map(|(_, sync_stat_list)| sync_stat_list).optional(),
        delim(')'),
    ).map(|(_, _, _, event_variable, sync_stat_list, _)| EventPostStmt {
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
    (
        kw!(event),
        kw!(wait),
        delim('('),
        event_variable,
        (
            comma(),
            list(event_wait_spec, 1..),
        ).map(|(_, event_wait_spec_list)| event_wait_spec_list),
        delim(')'),
    ).map(|(_, _, _, event_variable, event_wait_spec_list, _)| EventWaitStmt {
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
    (
        kw!(until),
        kw!(count),
        equals(),
        int_expr,
    ).map(|(_, _, _, until_count)| UntilSpec {
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
    (
        kw!(form),
        kw!(team),
        delim('('),
        int_expr, comma(),
        variable(false),
        (
            comma(),
            list(form_team_spec, 1..),
        ).map(|(_, form_team_spec_list)| form_team_spec_list).optional(),
        delim(')'),
    ).map(|(_, _, _, team_number, _, team_variable, form_team_spec_list, _)| FormTeamStmt {
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
        (
            kw!(new),
            kw!(index),
            equals(),
            int_expr,
        ).map(|(_, _, _, new_index)| FormTeamSpec::NewIndex(new_index)),
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
    (
        kw!(lock),
        delim('('),
        variable(false),
        (
            comma(),
            list(lock_stat, 1..),
        ).map(|(_, lock_stat_list)| lock_stat_list).optional(),
        delim(')'),
    ).map(|(_, _, lock_variable, lock_stat_list, _)| LockStmt {
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
        (
            kw!(acquired),
            kw!(lock),
            equals(),
            logical_variable(false),
        ).map(|(_, _, _, acquired_lock)| LockStat::AcquiredLock(acquired_lock)),
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
    (
        kw!(UNLOCK),
        delim('('),
        lock_variable,
        (
            comma(),
            list(sync_stat, 0..),
        ).map(|(_, sync_stat_list)| sync_stat_list).optional(),
        delim(')'),
    ).map(|(_, _, lock_variable, sync_stat_list, _)| {
        UnlockStmt {
            lock_variable,
            sync_stat_list,
        }
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
use super::*;

#[derive(Debug, Clone)]
pub struct Block<Span> {
    pub execution_part_constructs: Vec<MaybeStatement<ExecutionPartConstruct<Span>, Span>>,
}

#[syntax_rule(
    F18V007r1 rule "block" #1101 : "is [ execution-part-construct ] ...",
)]
pub fn block<'a, S: TextSource + 'a, U: 'a>(
    cfg: &'a Cfg,
    until: impl Parser<S, Token = U> + 'a,
) -> impl Parser<S, Token = (Block<S::Span>, Option<U>)> + 'a {
    many_until(
        maybe_statement(execution_part_construct(cfg)),
        until,
        0..,
    ).to_owned().map(|(execution_part_constructs, until)| (Block {
        execution_part_constructs,
    }, until))
}

#[derive(Debug, Clone)]
pub struct AssociateConstruct<Span> {
    pub associate_stmt: AssociateStmt<Span>,
    pub block: Block<Span>,
    pub end_associate_stmt: Option<EndAssociateStmt<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "associate-construct" #1102 :
    "is associate-stmt"
    "    block"
    "    end-associate-stmt",
)]
pub fn associate_construct<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = AssociateConstruct<S::Span>> + 'a {
    |source: S| {
        let (associate_stmt, source) = associate_stmt(cfg).parse(source)?;
        let ((block, end_associate_stmt), source) = block(cfg, end_associate_stmt(cfg)).parse(source)?;
        Some((AssociateConstruct {
            associate_stmt,
            block,
            end_associate_stmt,
        }, source))
    }
}

#[derive(Debug, Clone)]
pub struct AssociateStmt<Span> {
    pub associate_construct_name: Option<Name<Span>>,
    pub association_list: Vec<Association<Span>>,
    pub comment: Option<LineComment<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "associate-stmt" #1103 : "is [ associate-construct-name : ] ASSOCIATE (association-list )",
)]
pub fn associate_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = AssociateStmt<S::Span>> + 'a {
    (
        space(0),
        (
            name(cfg, false),
            (space(0), ':', space(0)),
        ).map(|(name, _)| name).optional(),
        kw("associate", cfg),
        (space(0), '(', space(0)),
        list(association(cfg), 0..),
        (space(0), ')', space(0)),
        statement_termination(), 
    ).map(|(_, associate_construct_name, _, _, association_list, _, comment)| AssociateStmt {
        associate_construct_name,
        association_list,
        comment,
    })
}

#[derive(Debug, Clone)]
pub struct Association<Span> {
    pub associate_name: Name<Span>,
    pub selector: Selector<Span>,
}

#[syntax_rule(
    F18V007r1 rule "association" #1104 : "is associate-name => selector",
)]
pub fn association<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = Association<S::Span>> + 'a {
    (
        name(cfg, false),
        (space(0), "=>", space(0)),
        selector(cfg),
    ).map(|(associate_name, _, selector)| Association {
        associate_name,
        selector,
    })
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum Selector<Span> {
    Expr(Expr<Span>),
    Variable(Variable<Span>),
}

#[syntax_rule(
    F18V007r1 rule "selector" #1105 :
    "is expr"
    "or variable",
)]
pub fn selector<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = Selector<S::Span>> + 'a {
    alt!(
        expr(cfg).map(Selector::Expr),
        variable(cfg, false/*TODO ???*/).map(Selector::Variable),
    )
}

#[derive(Debug, Clone)]
pub struct EndAssociateStmt<Span> {
    pub associate_construct_name: Option<Name<Span>>,
    pub comment: Option<LineComment<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "end-associate-stmt" #1106 : "is END ASSOCIATE [ associate-construct-name ]",
)]
pub fn end_associate_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = EndAssociateStmt<S::Span>> + 'a {
    (
        space(0),
        (
            kw("end", cfg),
            space(0),
            kw("associate", cfg),
        ),
        space(0),
        name(cfg, false).optional(),
        statement_termination(),
    ).map(|(_, _, _, associate_construct_name, comment)| EndAssociateStmt {
        associate_construct_name,
        comment,
    })
}

#[derive(Debug, Clone)]
pub struct BlockConstruct<Span> {
    pub block_stmt: BlockStmt<Span>,
    pub block_specification_part: Option<BlockSpecificationPart<Span>>,
    pub block: Option<Block<Span>>,
    pub end_block_stmt: Option<EndBlockStmt<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "block-construct" #1107 :
    "is block-stmt"
    "    [ block-specification-part ]"
    "    block"
    "    end-block-stmt",
)]
pub fn block_construct<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = BlockConstruct<S::Span>> + 'a {
    |source: S| {
        let (block_stmt, source) = block_stmt(cfg).parse(source)?;
        let ((block_specification_part, end), source) = block_specification_part(cfg, end_block_stmt(cfg)).parse(source)?;
        if end.is_some() {
            return Some((BlockConstruct {
                block_stmt,
                block_specification_part,
                block: None,
                end_block_stmt: end,
            }, source));
        }
        let ((block, end), source) = block(cfg, end_block_stmt(cfg)).parse(source)?;
        Some((BlockConstruct {
            block_stmt,
            block_specification_part,
            block: Some(block),
            end_block_stmt: end,
        }, source))
    }
}

#[derive(Debug, Clone)]
pub struct BlockStmt<Span> {
    pub block_construct_name: Option<Name<Span>>,
    pub comment: Option<LineComment<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "block-stmt" #1108 : "is [ block-construct-name : ] BLOCK",
)]
pub fn block_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = BlockStmt<S::Span>> + 'a {
    (
        space(0),
        (
            name(cfg, false),
            (space(0), ':', space(0)),
        ).map(|(name, _)| name).optional(),
        kw("block", cfg),
        statement_termination(),
    ).map(|(_, block_construct_name, _, comment)| BlockStmt {
        block_construct_name,
        comment,
    })
}

#[derive(Debug, Clone)]
pub struct BlockSpecificationPart<Span> {
    _p: std::marker::PhantomData<Span>,
}

#[syntax_rule(
    F18V007r1 rule "block-specification-part" #1109 :
    "is [ use-stmt ]..."
    "    [ import-stmt ] ..."
    "    [ [ declaration-construct ] ..."
    "    specification-construct ]",
)]
pub fn block_specification_part<'a, S: TextSource + 'a, U: 'a>(
    cfg: &'a Cfg,
    until: impl Parser<S, Token = U> + 'a,
) -> impl Parser<S, Token = (Option<BlockSpecificationPart<S::Span>>, Option<U>)> + 'a {
    |_| todo!("TODO: \"block_specification_part\" parser not implemented yet")
}

#[derive(Debug, Clone)]
pub struct EndBlockStmt<Span> {
    pub block_construct_name: Option<Name<Span>>,
    pub comment: Option<LineComment<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "end-block-stmt" #1110 : "is END BLOCK [ block-construct-name ]",
)]
pub fn end_block_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = EndBlockStmt<S::Span>> + 'a {
    (
        space(0),
        (
            kw("end", cfg),
            space(0),
            kw("block", cfg),
        ),
        space(0),
        name(cfg, false).optional(),
        statement_termination(),
    ).map(|(_, _, _, block_construct_name, comment)| EndBlockStmt {
        block_construct_name,
        comment,
    })
}
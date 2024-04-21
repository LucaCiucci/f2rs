use super::*;

#[derive(Debug, Clone, EnumAsInner)]
pub enum UseStmt<Span> {
    Use {
        module_nature: Option<ModuleNature>,
        module_name: Name<Span>,
        rename_list: Option<Vec<Rename<Span>>>,
        comment: Option<LineComment<Span>>,
    },
    UseOnly {
        module_nature: Option<ModuleNature>,
        module_name: Name<Span>,
        only_list: Vec<Only<Span>>,
        comment: Option<LineComment<Span>>,
    },
}

#[syntax_rule(
    F18V007r1 rule "use-stmt" #1409 :
    "is USE [ [ , module-nature ] :: ] module-name [ , rename-list ]"
    "or USE [ [ , module-nature ] :: ] module-name , ONLY : [ only-list ]",
)]
pub fn use_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = UseStmt<S::Span>> + 'a {
    alt!(
        (
            space(0),
            kw("use", cfg), space(0),
            (
                (
                    (space(0), ',', space(0)),
                    module_nature(cfg),
                ).map(|(_, module_nature)| module_nature).optional(),
                (space(0), "::", space(0)),
            ).map(|(module_nature, _)| module_nature).optional(),
            name(cfg, false),
            (
                (space(0), ',', space(0)),
                list(rename(cfg), 0..),
            ).map(|(_, rename_list)| rename_list).optional(),
            statement_termination(),
        ).map(|(_, _, _, module_nature, module_name, rename_list, comment)| UseStmt::Use {
            module_nature: module_nature.flatten(),
            module_name,
            rename_list,
            comment,
        }),
        (
            space(0),
            kw("use", cfg), space(0),
            (
                (
                    (space(0), ',', space(0)),
                    module_nature(cfg),
                ).map(|(_, module_nature)| module_nature).optional(),
                (space(0), "::", space(0)),
            ).map(|(module_nature, _)| module_nature).optional(),
            name(cfg, false),
            (space(0), ',', space(0), kw("only", cfg), space(0), ':', space(0)),
            list(only(cfg), 0..),
            statement_termination(),
        ).map(|(_, _, _, module_nature, module_name, _, only_list, comment)| UseStmt::UseOnly {
            module_nature: module_nature.flatten(),
            module_name,
            only_list,
            comment,
        }),
    )
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ModuleNature {
    Intrinsic,
    NonIntrinsic,
}

#[syntax_rule(
    F18V007r1 rule "module-nature" #1410 :
    "is INTRINSIC"
    "or NON_INTRINSIC",
)]
pub fn module_nature<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ModuleNature> + 'a {
    alt!(
        StringMatch::exact("intrinsic", false).map(|_| ModuleNature::Intrinsic),
        StringMatch::exact("non_intrinsic", false).map(|_| ModuleNature::NonIntrinsic),
    )
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum Rename<Span> {
    Name {
        local_name: Name<Span>,
        use_name: Name<Span>,
    },
    Operator {
        local_defined_operator: LocalDefinedOperator<Span>,
        use_defined_operator: UseDefinedOperator<Span>,
    },
}

#[syntax_rule(
    F18V007r1 rule "rename" #1411 :
    "is local-name => use-name"
    "or OPERATOR (local-defined-operator) => OPERATOR (use-defined-operator)",
)]
pub fn rename<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = Rename<S::Span>> + 'a {
    alt!(
        (
            name(cfg, false),
            (space(0), "=>", space(0)),
            name(cfg, false),
        ).map(|(local_name, _, use_name)| Rename::Name { local_name, use_name }),
        (
            kw("operator", cfg),
            space(0),
            local_defined_operator(cfg),
            (space(0), "=>", space(0)),
            kw("operator", cfg),
            space(0),
            use_defined_operator(cfg),
        ).map(|(_, _, local_defined_operator, _, _, _, use_defined_operator)| Rename::Operator { local_defined_operator, use_defined_operator }),
    )
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum Only<Span> {
    GenericSpec(GenericSpec<Span>),
    OnlyUseName(OnlyUseName<Span>),
    Rename(Rename<Span>),
}

#[syntax_rule(
    F18V007r1 rule "only" #1412 :
    "is generic-spec"
    "or only-use-name"
    "or rename",
)]
pub fn only<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = Only<S::Span>> + 'a {
    alt!(
        generic_spec(cfg).map(Only::GenericSpec),
        only_use_name(cfg).map(Only::OnlyUseName),
        rename(cfg).map(Only::Rename),
    )
}

#[derive(Debug, Clone)]
pub struct OnlyUseName<Span>(pub Name<Span>);

#[syntax_rule(
    F18V007r1 rule "only-use-name" #1413 : "is use-name",
)]
pub fn only_use_name<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = OnlyUseName<S::Span>> + 'a {
    name(cfg, false).map(OnlyUseName)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum LocalDefinedOperator<Span> {
    Unary(DefinedUnaryOrBinaryOp<Span>),
    Binary(DefinedUnaryOrBinaryOp<Span>),
}

#[syntax_rule(
    F18V007r1 rule "local-defined-operator" #1414 :
    "is defined-unary-op"
    "or defined-binary-op",
)]
pub fn local_defined_operator<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = LocalDefinedOperator<S::Span>> + 'a {
    // WARNING they are the same, will always match the first one
    alt!(
        defined_unary_op(cfg).map(LocalDefinedOperator::Unary),
        defined_binary_op(cfg).map(LocalDefinedOperator::Binary),
    )
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum UseDefinedOperator<Span> {
    Unary(DefinedUnaryOrBinaryOp<Span>),
    Binary(DefinedUnaryOrBinaryOp<Span>),
}

#[syntax_rule(
    F18V007r1 rule "use-defined-operator" #1415 :
    "is defined-unary-op"
    "or defined-binary-op",
)]
pub fn use_defined_operator<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = UseDefinedOperator<S::Span>> + 'a {
    // WARNING they are the same, will always match the first one
    alt!(
        defined_unary_op(cfg).map(UseDefinedOperator::Unary),
        defined_binary_op(cfg).map(UseDefinedOperator::Binary),
    )
}
use super::*;

#[derive(Debug, Clone, EnumAsInner)]
enum Stmt<Span> { // TODO maybe StatementInner
    OtherSpecification(OtherSpecificationStmt<Span>),
    ImplicitPart(ImplicitPartStmt<Span>),
    MpSubprogram(MpSubprogramStmt<Span>),
    Function(FunctionStmt<Span>),
    Procedure(ProcedureStmt<Span>),
    EndInterface(EndInterfaceStmt<Span>),
    Interface(InterfaceStmt<Span>),
}

impl<Span> Stmt<Span> {
    fn parser<'a, S: TextSource<Span = Span> + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = Self> + 'a {
        alt!(
            other_specification_stmt_2(cfg).map(Stmt::OtherSpecification),
            implicit_part_stmt_2(cfg).map(Stmt::ImplicitPart),
            mp_subprogram_stmt_2(cfg).map(Stmt::MpSubprogram),
            function_stmt_2(cfg).map(Stmt::Function),
            procedure_stmt_2(cfg).map(Stmt::Procedure),
            end_interface_stmt_2(cfg).map(Stmt::EndInterface),
            interface_stmt_2(cfg).map(Stmt::Interface),
        )
    }
}

struct Statement<Span> {
    label: Option<Label<Span>>,
    inner: Stmt<Span>,
    final_comment: Option<LineComment<Span>>,
}

impl<Span> Statement<Span> {
    fn parser<'a, S: TextSource<Span = Span> + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = Self> + 'a {
        (
            space(0),
            label(cfg).optional(),
            space(0),
            Stmt::parser(cfg),
            space(0),
            statement_termination(),
        ).map(|(_, label, _, inner, _, final_comment)| Statement {
            label,
            inner,
            final_comment,
        })
    }
}

#[derive(Debug, Clone)]
pub struct Label<Span> {
    pub digits: StringMatch<Span>,
    pub value: u32,
}

#[syntax_rule(
    F18V007r1 rule "label" #611 : "is digit [ digit [ digit [ digit [ digit ] ] ] ]",
)]
pub fn label<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = Label<S::Span>> + 'a {
    digit(cfg)
        .then(|first| fold_many(
            digit(cfg),
            move || StringMatch::from_char(first.clone()),
            |mut string, digit| {
                string.push_char::<S>(digit);
                (string, true)
            },
            0..=4,
        ))
        .map(|digits| {
            let value = digits.value.parse::<u32>().unwrap();
            Label { digits, value }
        })
}

#[derive(Debug, Clone)]
pub struct InvalidStatement<Span> {
    pub m: StringMatch<Span>,
}

// TODO test
pub fn invalid_statement<'a, S: TextSource + 'a>() -> impl Parser<S, Token = InvalidStatement<S::Span>> + 'a {
    fold_many_until(
        Char::any(),
        nl,
        || StringMatch::empty::<S>(),
        |mut m, c| {
            m.push_char::<S>(c);
            (m, true)
        },
        0..,
    ).map(|(m, _nl)| {
        InvalidStatement { m }
    })
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum MaybeStatement<Statement, Span> {
    EmptyLines(EmptyLines),
    Statement(Statement),
    Comment(LineComment<Span>),
    Invalid(InvalidStatement<Span>),
}

// TODO test
pub fn maybe_statement<'a, S: TextSource + 'a, Statement: 'a>(
    statement: impl Parser<S, Token = Statement> + 'a,
) -> impl Parser<S, Token = MaybeStatement<Statement, S::Span>> + 'a {
    alt!(
        empty_lines().map(MaybeStatement::EmptyLines),
        statement.clone().map(MaybeStatement::Statement),
        (space(0), line_comment()).map(|(_, comment)| MaybeStatement::Comment(comment)),
        invalid_statement().map(MaybeStatement::Invalid),
    )
}

#[derive(Debug, Clone)]
pub struct SequenceStmt<Span> {
    pub comment: Option<LineComment<Span>>,
}

// TODO test
#[syntax_rule(
    F18V007r1 rule "sequence-stmt" #731 : "is SEQUENCE",
)]
pub fn sequence_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = SequenceStmt<S::Span>> + 'a {
    (
        space(0),
        StringMatch::exact("sequence", false),
        statement_termination(),
    ).map(|(_, _, comment)| SequenceStmt {
        comment,
    })
}

#[derive(Debug, Clone)]
pub struct PrivateComponentsStmt<Span> {
    pub comment: Option<LineComment<Span>>,
}

// TODO test
#[syntax_rule(
    F18V007r1 rule "private-components-stmt" #745 : "is PRIVATE",
)]
pub fn private_components_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = PrivateComponentsStmt<S::Span>> + 'a {
    (
        space(0),
        StringMatch::exact("private", false),
        statement_termination(),
    ).map(|(_, _, comment)| PrivateComponentsStmt {
        comment,
    })
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum PrivateOrSequence<Span> {
    Private(PrivateComponentsStmt<Span>),
    Sequence(SequenceStmt<Span>),
}

// TODO test
#[syntax_rule(
    F18V007r1 rule "private-or-sequence" #729 :
    "is private-components-stmt"
    "or sequence-stmt",
)]
pub fn private_or_sequence<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = PrivateOrSequence<S::Span>> + 'a {
    alt!(
        private_components_stmt(cfg).map(PrivateOrSequence::Private),
        sequence_stmt(cfg).map(PrivateOrSequence::Sequence),
    )
}

#[derive(Debug, Clone)]
pub struct NullifyStmt<Span> {
    pub pointer_object_list: Vec<PointerObject<Span>>,
    pub comment: Option<LineComment<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "nullify-stmt" #938 : "is NULLIFY ( pointer-object-list )",
)]
pub fn nullify_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = NullifyStmt<S::Span>> + 'a {
    (
        space(0),
        kw("nullify", cfg),
        (space(0), '(', space(0)),
        list(pointer_object(cfg), 0..),
        (space(0), ')', space(0)),
        statement_termination(),
    ).map(|(_, _, _, pointer_object_list, _, comment)| NullifyStmt {
        pointer_object_list,
        comment,
    })
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum PointerObject<Span> {
    VariableName(VariableName<Span>),
    StructureComponent(StructureComponent<Span>),
    ProcPointerName(ProcPointerName<Span>),
}

#[syntax_rule(
    F18V007r1 rule "pointer-object" #939 :
    "is variable-name"
    "or structure-component"
    "or proc-pointer-name",
)]
pub fn pointer_object<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = PointerObject<S::Span>> + 'a {
    alt!(
        variable_name(cfg).map(PointerObject::VariableName),
        structure_component(cfg).map(PointerObject::StructureComponent),
        proc_pointer_name(cfg).map(PointerObject::ProcPointerName),
    )
}

#[derive(Debug, Clone)]
pub struct DeallocateStmt<Span> {
    pub allocate_object_list: Vec<AllocateObject<Span>>,
    pub dealloc_opt_list: Vec<DeallocOpt<Span>>,
    pub comment: Option<LineComment<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "deallocate-stmt" #940 : "is DEALLOCATE ( allocate-object-list [ , dealloc-opt-list ] )",
)]
pub fn deallocate_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DeallocateStmt<S::Span>> + 'a {
    (
        space(0), '(', space(0),
        list(allocate_object(cfg), 0..),
        (
            space(0), ',', space(0),
            list(dealloc_opt(cfg), 0..),
        ).map(|(_, _, _, dealloc_opt_list)| dealloc_opt_list).optional(),
        space(0), ')',
        statement_termination(),
    ).map(|(_, _, _, allocate_object_list, dealloc_opt_list, _, _, comment)| DeallocateStmt {
        allocate_object_list,
        dealloc_opt_list: dealloc_opt_list.unwrap_or_default(),
        comment,
    })
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum DeallocOpt<Span> {
    Stat(StatVariable<Span>),
    Errmsg(ErrmsgVariable<Span>),
}

#[syntax_rule(
    F18V007r1 rule "dealloc-opt" #941 :
    "is STAT = stat-variable"
    "or ERRMSG = errmsg-variable",
)]
pub fn dealloc_opt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DeallocOpt<S::Span>> + 'a {
    alt!(
        (kw("stat", cfg), space(0), '=', space(0), stat_variable(cfg)).map(|(_, _, _, _, stat)| DeallocOpt::Stat(stat)),
        (kw("errmsg", cfg), space(0), '=', space(0), errmsg_variable(cfg)).map(|(_, _, _, _, errmsg)| DeallocOpt::Errmsg(errmsg)),
    )
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_label() {
        for cfg in test_configs() {
            let parser = label(&cfg);
            assert!(parser.parse("1").unwrap().0.value == 1);
            assert!(parser.parse("10").unwrap().0.value == 10);
            assert!(parser.parse("100").unwrap().0.value == 100);
            assert!(parser.parse("1000").unwrap().0.value == 1000);
            assert!(parser.parse("10000").unwrap().0.value == 10000);
            assert!(parser.parse("100000").is_none());
        }
    }

    #[test]
    fn sequence_stmt_1() {
        for cfg in test_configs() {
            let parser = sequence_stmt(&cfg);
            assert_eq!(parser.parse("sequence\n ").unwrap().1, " ");
            assert_eq!(parser.parse("sequence; ! hello \n a").unwrap().1, " a");
        }
    }
}
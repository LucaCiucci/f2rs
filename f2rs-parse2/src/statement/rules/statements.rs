use std::marker::PhantomData;

use super::*;

#[derive(Debug, Clone)]
pub struct SequenceStmt<Span> {
    _p: PhantomData<Span>,
}

// TODO test
#[syntax_rule(
    F18V007r1 rule "sequence-stmt" #731 : "is SEQUENCE",
)]
pub fn sequence_stmt<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = SequenceStmt<MultilineSpan>> + 'a {
    kw!(SEQUENCE).map(|_| SequenceStmt {
        _p: PhantomData,
    })
}

#[derive(Debug, Clone)]
pub struct PrivateComponentsStmt<Span> {
    _p: PhantomData<Span>,
}

// TODO test
#[syntax_rule(
    F18V007r1 rule "private-components-stmt" #745 : "is PRIVATE",
)]
pub fn private_components_stmt<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = PrivateComponentsStmt<MultilineSpan>> + 'a {
    kw!(PRIVATE).map(|_| PrivateComponentsStmt {
        _p: PhantomData,
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
pub fn private_or_sequence<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = PrivateOrSequence<MultilineSpan>> + 'a {
    alt!(
        private_components_stmt(cfg).map(PrivateOrSequence::Private),
        sequence_stmt(cfg).map(PrivateOrSequence::Sequence),
    )
}

#[derive(Debug, Clone)]
pub struct NullifyStmt<Span> {
    pub pointer_object_list: Vec<PointerObject<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "nullify-stmt" #938 : "is NULLIFY ( pointer-object-list )",
)]
pub fn nullify_stmt<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = NullifyStmt<MultilineSpan>> + 'a {
    (
        kw!(NULLIFY),
        delim('('),
        list(pointer_object(cfg), 0..),
        delim(')'),
    ).map(|(_, _, pointer_object_list, _)| NullifyStmt {
        pointer_object_list,
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
pub fn pointer_object<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = PointerObject<MultilineSpan>> + 'a {
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
}

#[syntax_rule(
    F18V007r1 rule "deallocate-stmt" #940 : "is DEALLOCATE ( allocate-object-list [ , dealloc-opt-list ] )",
)]
pub fn deallocate_stmt<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DeallocateStmt<MultilineSpan>> + 'a {
    (
        delim('('),
        list(allocate_object(cfg), 0..),
        (
            comma(),
            list(dealloc_opt(cfg), 0..),
        ).map(|(_, dealloc_opt_list)| dealloc_opt_list).optional(),
        delim(')'),
    ).map(|(_, allocate_object_list, dealloc_opt_list, _)| DeallocateStmt {
        allocate_object_list,
        dealloc_opt_list: dealloc_opt_list.unwrap_or_default(),
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
pub fn dealloc_opt<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DeallocOpt<MultilineSpan>> + 'a {
    alt!(
        (kw!(STAT), equals(), stat_variable(cfg)).map(|(_, _, stat)| DeallocOpt::Stat(stat)),
        (kw!(ERRMSG), equals(), errmsg_variable(cfg)).map(|(_, _, errmsg)| DeallocOpt::Errmsg(errmsg)),
    )
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ForallAssignmentStmt<Span> {
    AssignmentStmt(AssignmentStmt<Span>),
    PointerAssignmentStmt(PointerAssignmentStmt<Span>),
}

#[syntax_rule(
    F18V007r1 rule "forall-assignment-stmt" #1055 :
    "is assignment-stmt"
    "or pointer-assignment-stmt",
)]
pub fn forall_assignment_stmt<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ForallAssignmentStmt<MultilineSpan>> + 'a {
    alt!(
        assignment_stmt_2(cfg).map(ForallAssignmentStmt::AssignmentStmt),
        pointer_assignment_stmt(cfg).map(ForallAssignmentStmt::PointerAssignmentStmt),
    )
}

#[derive(Debug, Clone)]
pub struct ForallStmt<Span> {
    pub concurrent_header: Option<ConcurrentHeader<Span>>,
    pub forall_assignment_stmt: ForallAssignmentStmt<Span>,
}

#[syntax_rule(
    F18V007r1 rule "forall-stmt" #1055 : "is FORALL concurrent-header forall-assignment-stmt"
)]
pub fn forall_stmt<'a, S: Lexed + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ForallStmt<MultilineSpan>> + 'a {
    (
        kw!(FORALL),
        concurrent_header(cfg).optional(),
        forall_assignment_stmt(cfg),
    ).map(|(_, concurrent_header, forall_assignment_stmt)| ForallStmt {
        concurrent_header,
        forall_assignment_stmt,
    })
}

#[cfg(test)]
mod test {
    //use super::*;

    //#[test]
    //fn test_label() {
    //    for cfg in test_configs() {
    //        let parser = label(&cfg);
    //        assert!(parser.parse("1").unwrap().0.value == 1);
    //        assert!(parser.parse("10").unwrap().0.value == 10);
    //        assert!(parser.parse("100").unwrap().0.value == 100);
    //        assert!(parser.parse("1000").unwrap().0.value == 1000);
    //        assert!(parser.parse("10000").unwrap().0.value == 10000);
    //        assert!(parser.parse("100000").is_none());
    //    }
    //}
//
    //#[test]
    //fn sequence_stmt_1() {
    //    for cfg in test_configs() {
    //        let parser = sequence_stmt(&cfg);
    //        assert_eq!(parser.parse("sequence\n ").unwrap().1, " ");
    //        assert_eq!(parser.parse("sequence; ! hello \n a").unwrap().1, " a");
    //    }
    //}
}
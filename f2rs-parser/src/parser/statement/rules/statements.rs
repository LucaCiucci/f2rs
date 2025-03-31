use std::marker::PhantomData;

use super::*;

#[derive(Debug, Clone)]
pub struct SequenceStmt<Span> {
    _p: PhantomData<Span>,
}

// TODO test
#[doc = s_rule!(
    F18V007r1 rule "sequence-stmt" #731 : "is SEQUENCE",
)]
pub fn sequence_stmt<S: Lexed>(source: S) -> PResult<SequenceStmt<MultilineSpan>, S> {
    kw!(SEQUENCE).map(|_| SequenceStmt {
        _p: PhantomData,
    })
    .parse(source)
}

#[derive(Debug, Clone)]
pub struct PrivateComponentsStmt<Span> {
    _p: PhantomData<Span>,
}

// TODO test
#[doc = s_rule!(
    F18V007r1 rule "private-components-stmt" #745 : "is PRIVATE",
)]
pub fn private_components_stmt<S: Lexed>(source: S) -> PResult<PrivateComponentsStmt<MultilineSpan>, S> {
    kw!(PRIVATE).map(|_| PrivateComponentsStmt {
        _p: PhantomData,
    })
    .parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum PrivateOrSequence<Span> {
    Private(PrivateComponentsStmt<Span>),
    Sequence(SequenceStmt<Span>),
}

// TODO test
#[doc = s_rule!(
    F18V007r1 rule "private-or-sequence" #729 :
    "is private-components-stmt"
    "or sequence-stmt",
)]
pub fn private_or_sequence<S: Lexed>(source: S) -> PResult<PrivateOrSequence<MultilineSpan>, S> {
    alt!(
        for S =>
        private_components_stmt.map(PrivateOrSequence::Private),
        sequence_stmt.map(PrivateOrSequence::Sequence),
    )
    .parse(source)
}

#[derive(Debug, Clone)]
pub struct NullifyStmt<Span> {
    pub pointer_object_list: Vec<PointerObject<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "nullify-stmt" #938 : "is NULLIFY ( pointer-object-list )",
)]
pub fn nullify_stmt<S: Lexed>(source: S) -> PResult<NullifyStmt<MultilineSpan>, S> {
    (
        kw!(NULLIFY),
        delim('('),
        list(pointer_object, 0..),
        delim(')'),
    ).map(|(_, _, pointer_object_list, _)| NullifyStmt {
        pointer_object_list,
    })
    .parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum PointerObject<Span> {
    VariableName(VariableName<Span>),
    StructureComponent(StructureComponent<Span>),
    ProcPointerName(ProcPointerName<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "pointer-object" #939 :
    "is variable-name"
    "or structure-component"
    "or proc-pointer-name",
)]
pub fn pointer_object<S: Lexed>(source: S) -> PResult<PointerObject<MultilineSpan>, S> {
    alt!(
        for S =>
        variable_name.map(PointerObject::VariableName),
        structure_component.map(PointerObject::StructureComponent),
        proc_pointer_name.map(PointerObject::ProcPointerName),
    )
    .parse(source)
}

#[derive(Debug, Clone)]
pub struct DeallocateStmt<Span> {
    pub allocate_object_list: Vec<AllocateObject<Span>>,
    pub dealloc_opt_list: Vec<DeallocOpt<Span>>,
}

#[doc = s_rule!(
    F18V007r1 rule "deallocate-stmt" #940 : "is DEALLOCATE ( allocate-object-list [ , dealloc-opt-list ] )",
)]
pub fn deallocate_stmt<S: Lexed>(source: S) -> PResult<DeallocateStmt<MultilineSpan>, S> {
    (
        delim('('),
        list(allocate_object, 0..),
        (
            comma(),
            list(dealloc_opt, 0..),
        ).map(|(_, dealloc_opt_list)| dealloc_opt_list).optional(),
        delim(')'),
    ).map(|(_, allocate_object_list, dealloc_opt_list, _)| DeallocateStmt {
        allocate_object_list,
        dealloc_opt_list: dealloc_opt_list.unwrap_or_default(),
    })
    .parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum DeallocOpt<Span> {
    Stat(StatVariable<Span>),
    Errmsg(ErrmsgVariable<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "dealloc-opt" #941 :
    "is STAT = stat-variable"
    "or ERRMSG = errmsg-variable",
)]
pub fn dealloc_opt<S: Lexed>(source: S) -> PResult<DeallocOpt<MultilineSpan>, S> {
    alt!(
        for S =>
        (kw!(STAT), equals(), stat_variable).map(|(_, _, stat)| DeallocOpt::Stat(stat)),
        (kw!(ERRMSG), equals(), errmsg_variable).map(|(_, _, errmsg)| DeallocOpt::Errmsg(errmsg)),
    )
    .parse(source)
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ForallAssignmentStmt<Span> {
    AssignmentStmt(AssignmentStmt<Span>),
    PointerAssignmentStmt(PointerAssignmentStmt<Span>),
}

#[doc = s_rule!(
    F18V007r1 rule "forall-assignment-stmt" #1055 :
    "is assignment-stmt"
    "or pointer-assignment-stmt",
)]
pub fn forall_assignment_stmt<S: Lexed>(source: S) -> PResult<ForallAssignmentStmt<MultilineSpan>, S> {
    alt!(
        for S =>
        assignment_stmt_2.map(ForallAssignmentStmt::AssignmentStmt),
        pointer_assignment_stmt.map(ForallAssignmentStmt::PointerAssignmentStmt),
    )
    .parse(source)
}

#[derive(Debug, Clone)]
pub struct ForallStmt<Span> {
    pub concurrent_header: Option<ConcurrentHeader<Span>>,
    pub forall_assignment_stmt: ForallAssignmentStmt<Span>,
}

#[doc = s_rule!(
    F18V007r1 rule "forall-stmt" #1055 : "is FORALL concurrent-header forall-assignment-stmt",
)]
pub fn forall_stmt<S: Lexed>(source: S) -> PResult<ForallStmt<MultilineSpan>, S> {
    (
        kw!(FORALL),
        concurrent_header.optional(),
        forall_assignment_stmt,
    ).map(|(_, concurrent_header, forall_assignment_stmt)| ForallStmt {
        concurrent_header,
        forall_assignment_stmt,
    })
    .parse(source)
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
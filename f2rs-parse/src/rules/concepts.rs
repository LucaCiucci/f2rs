use super::*;

#[derive(Debug, Clone)]
pub struct SpecificationPart<Span> {
    pub use_stmts: Vec<UseStmt<Span>>,
    pub import_stmts: Vec<ImportStmt<Span>>,
    pub implicit_part: Option<ImplicitPart<Span>>,
    pub declaration_constructs: Vec<DeclarationConstruct<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "specification-part" #504 :
    "is [ use-stmt ] ..."
    "    [ import-stmt ] ..."
    "    [ implicit-part ]"
    "    [ declaration-construct ] ...",
)]
pub fn specification_part<'a, S: TextSource + 'a, U: 'a>(
    cfg: &'a Cfg,
    until: impl Parser<S, Token = U> + 'a,
) -> impl Parser<S, Token = (SpecificationPart<S::Span>, Option<U>)> + 'a {
    move |source: S| {
        let mut result = SpecificationPart {
            use_stmts: Vec::new(),
            import_stmts: Vec::new(),
            implicit_part: None,
            declaration_constructs: Vec::new(),
        };

        let ((use_stmts, u), source) = many_until(use_stmt(cfg), until, 0..).parse(source)?;
        result.use_stmts = use_stmts;
        if u.is_some() {
            return Some(((result, u), source));
        }

        let ((import_stmts, u), source) = many_until(import_stmt(cfg), until, 0..).parse(source)?;
        result.import_stmts = import_stmts;
        if u.is_some() {
            return Some(((result, u), source));
        }

        let ((implicit_part, u), source) = implicit_part(cfg, until).optional().parse(source)?;
        result.implicit_part = implicit_part;
        if u.is_some() {
            return Some(((result, u), source));
        }

        let ((declaration_constructs, u), source) = many_until(declaration_construct(cfg), until, u..)?;
        result.declaration_constructs = declaration_constructs;
        if u.is_some() {
            return Some(((result, u), source));
        }

        Some(((result, None), source))
    }
}

#[derive(Debug, Clone)]
pub struct ImplicitPart<Span> {
    pub implicit_part_stmts: Vec<ImplicitPartStmt<Span>>,
    pub implicit_stmt: ImplicitStmt<Span>,
}

#[syntax_rule(
    F18V007r1 rule "implicit-part" #505 :
    "is [ implicit-part-stmt ] ..."
    "    implicit-stmt",
)]
pub fn implicit_part<'a, S: TextSource + 'a, U: 'a>(
    cfg: &'a Cfg,
    until: impl Parser<S, Token = U> + 'a,
) -> impl Parser<S, Token = (ImplicitPart<S::Span>, Option<U>)> + 'a {
    move |mut source: S| {
        let mut implicit_part_stmts = vec![];
        let mut u_result = None;
        loop {
            let r_u = until.parse(source);
            match r_u {
                Some((u, s)) => {
                    source = s;
                    u_result = Some(u);
                    break;
                },
                None => {}
            }

            let r = implicit_part_stmt(cfg).parse(source);
            match r {
                Some((implicit_part_stmt, s)) => {
                    source = s;
                    implicit_part_stmts.push(implicit_part_stmt);
                },
                None => break,
            }
        }

        let implicit_stmt = match implicit_part_stmts.pop()? {
            ImplicitPartStmt::ImplicitStmt(implicit_stmt) => implicit_stmt,
            _ => return None,
        };

        Some(((ImplicitPart {
            implicit_part_stmts,
            implicit_stmt,
        }, u_result), source))
    }
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ImplicitPartStmt<Span> {
    ImplicitStmt(ImplicitStmt<Span>),
    ParameterStmt(ParameterStmt<Span>),
    FormatStmt(FormatStmt<Span>),
    EntryStmt(EntryStmt<Span>),
}

#[syntax_rule(
    F18V007r1 rule "implicit-part-stmt" #506 :
    "is implicit-stmt"
    "or parameter-stmt"
    "or format-stmt"
    "or entry-stmt",
)]
pub fn implicit_part_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ImplicitPartStmt<S::Span>> + 'a {
    alt!(
        implicit_stmt(cfg).map(ImplicitPartStmt::ImplicitStmt),
        parameter_stmt(cfg).map(ImplicitPartStmt::ParameterStmt),
        format_stmt(cfg).map(ImplicitPartStmt::FormatStmt),
        entry_stmt(cfg).map(ImplicitPartStmt::EntryStmt),
    )
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum DeclarationConstruct<Span> {
    SpecificationConstruct(SpecificationConstruct<Span>),
    DataStmt(DataStmt<Span>),
    FormatStmt(FormatStmt<Span>),
    EntryStmt(EntryStmt<Span>),
    StmtFunctionStmt(StmtFunctionStmt<Span>),
}

#[syntax_rule(
    F18V007r1 rule "declaration-construct" #507 :
    "is specification-construct"
    "or data-stmt"
    "or format-stmt"
    "or entry-stmt"
    "or stmt-function-stmt",
)]
pub fn declaration_construct<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = DeclarationConstruct<S::Span>> + 'a {
    alt!(
        specification_construct(cfg).map(DeclarationConstruct::SpecificationConstruct),
        data_stmt(cfg).map(DeclarationConstruct::DataStmt),
        format_stmt(cfg).map(DeclarationConstruct::FormatStmt),
        entry_stmt(cfg).map(DeclarationConstruct::EntryStmt),
        stmt_function_stmt(cfg).map(DeclarationConstruct::StmtFunctionStmt),
    )
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum SpecificationConstruct<Span> {
    DerivedTypeDef(DerivedTypeDef<Span>),
    EnumDef(EnumDef<Span>),
    GenericStmt(GenericStmt<Span>),
    InterfaceBlock(InterfaceBlock<Span>),
    ParameterStmt(ParameterStmt<Span>),
    ProcedureDeclarationStmt(ProcedureDeclarationStmt<Span>),
    OtherSpecificationStmt(OtherSpecificationStmt<Span>),
    TypeDeclarationStmt(TypeDeclarationStmt<Span>),
}

#[syntax_rule(
    F18V007r1 rule "specification-construct" #508 :
    "is derived-type-def"
    "or enum-def"
    "or generic-stmt"
    "or interface-block"
    "or parameter-stmt"
    "or procedure-declaration-stmt"
    "or other-specification-stmt"
    "or type-declaration-stmt",
)]
pub fn specification_construct<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = SpecificationConstruct<S::Span>> + 'a {
    alt!(
        derived_type_def(cfg).map(SpecificationConstruct::DerivedTypeDef),
        enum_def(cfg).map(SpecificationConstruct::EnumDef),
        generic_stmt(cfg).map(SpecificationConstruct::GenericStmt),
        interface_block(cfg).map(SpecificationConstruct::InterfaceBlock),
        parameter_stmt(cfg).map(SpecificationConstruct::ParameterStmt),
        procedure_declaration_stmt(cfg).map(SpecificationConstruct::ProcedureDeclarationStmt),
        other_specification_stmt(cfg).map(SpecificationConstruct::OtherSpecificationStmt),
        type_declaration_stmt(cfg).map(SpecificationConstruct::TypeDeclarationStmt),
    )
}

#[derive(Debug, Clone)]
pub struct ExecutionPart<Span> {
    pub executable_construct: ExecutableConstruct<Span>,
    pub execution_part_constructs: Vec<ExecutionPartConstruct<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "execution-part" #509 :
    "is executable-construct"
    "    [ execution-part-construct ] ...",
)]
pub fn execution_part<'a, S: TextSource + 'a, U: 'a>(
    cfg: &'a Cfg,
    until: impl Parser<S, Token = U> + 'a,
) -> impl Parser<S, Token = (ExecutionPart<S::Span>, Option<U>)> + 'a {
    (
        executable_construct(cfg),
        many_until(execution_part_construct(cfg), until, 0..),
    ).map(|(executable_construct, (execution_part_constructs, u))| (ExecutionPart {
        executable_construct,
        execution_part_constructs,
    }, u))
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ExecutionPartConstruct<Span> {
    ExecutableConstruct(ExecutableConstruct<Span>),
    FormatStmt(FormatStmt<Span>),
    EntryStmt(EntryStmt<Span>),
    DataStmt(DataStmt<Span>),
}

#[syntax_rule(
    F18V007r1 rule "execution-part-construct" #510 :
    "is executable-construct"
    "or format-stmt"
    "or entry-stmt"
    "or data-stmt",
)]
pub fn execution_part_construct<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ExecutionPartConstruct<S::Span>> + 'a {
    alt!(
        executable_construct(cfg).map(ExecutionPartConstruct::ExecutableConstruct),
        format_stmt(cfg).map(ExecutionPartConstruct::FormatStmt),
        entry_stmt(cfg).map(ExecutionPartConstruct::EntryStmt),
        data_stmt(cfg).map(ExecutionPartConstruct::DataStmt),
    )
}

#[derive(Debug, Clone)]
pub struct InternalSubprogramPart<Span> {
    pub contains_stmt: ContainsStmt<Span>,
    pub internal_subprograms: Vec<InternalSubprogram<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "internal-subprogram-part" #511 :
    "is contains-stmt"
    "    [ internal-subprogram ] ...",
)]
pub fn internal_subprogram_part<'a, S: TextSource + 'a, U: 'a>(
    cfg: &'a Cfg,
    until: impl Parser<S, Token = U> + 'a,
) -> impl Parser<S, Token = (InternalSubprogramPart<S::Span>, Option<U>)> + 'a {
    (
        contains_stmt(cfg),
        many_until(internal_subprogram(cfg), until, 0..),
    ).map(|(contains_stmt, (internal_subprograms, u))| (InternalSubprogramPart {
        contains_stmt,
        internal_subprograms,
    }, u))
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum InternalSubprogram<Span> {
    FunctionSubprogram(FunctionSubprogram<Span>),
    SubroutineSubprogram(SubroutineSubprogram<Span>),
}

#[syntax_rule(
    F18V007r1 rule "internal-subprogram" #512 :
    "is function-subprogram"
    "or subroutine-subprogram",
)]
pub fn internal_subprogram<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = InternalSubprogram<S::Span>> + 'a {
    alt!(
        function_subprogram(cfg).map(InternalSubprogram::FunctionSubprogram),
        subroutine_subprogram(cfg).map(InternalSubprogram::SubroutineSubprogram),
    )
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum OtherSpecificationStmt<Span> {
    AccessStmt(AccessStmt<Span>),
    AllocatableStmt(AllocatableStmt<Span>),
    AsynchronousStmt(AsynchronousStmt<Span>),
    BindStmt(BindStmt<Span>),
    CodimensionStmt(CodimensionStmt<Span>),
    ContiguousStmt(ContiguousStmt<Span>),
    DimensionStmt(DimensionStmt<Span>),
    ExternalStmt(ExternalStmt<Span>),
    IntentStmt(IntentStmt<Span>),
    IntrinsicStmt(IntrinsicStmt<Span>),
    NamelistStmt(NamelistStmt<Span>),
    OptionalStmt(OptionalStmt<Span>),
    PointerStmt(PointerStmt<Span>),
    ProtectedStmt(ProtectedStmt<Span>),
    SaveStmt(SaveStmt<Span>),
    TargetStmt(TargetStmt<Span>),
    VolatileStmt(VolatileStmt<Span>),
    ValueStmt(ValueStmt<Span>),
    CommonStmt(CommonStmt<Span>),
    EquivalenceStmt(EquivalenceStmt<Span>),
}

#[syntax_rule(
    F18V007r1 rule "other-specification-stmt" #513 :
    "is access-stmt"
    "or allocatable-stmt"
    "or asynchronous-stmt"
    "or bind-stmt"
    "or codimension-stmt"
    "or contiguous-stmt"
    "or dimension-stmt"
    "or external-stmt"
    "or intent-stmt"
    "or intrinsic-stmt"
    "or namelist-stmt"
    "or optional-stmt"
    "or pointer-stmt"
    "or protected-stmt"
    "or save-stmt"
    "or target-stmt"
    "or volatile-stmt"
    "or value-stmt"
    "or common-stmt"
    "or equivalence-stmt",
)]
pub fn other_specification_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = OtherSpecificationStmt<S::Span>> + 'a {
    alt!(
        access_stmt(cfg).map(OtherSpecificationStmt::AccessStmt),
        allocatable_stmt(cfg).map(OtherSpecificationStmt::AllocatableStmt),
        asynchronous_stmt(cfg).map(OtherSpecificationStmt::AsynchronousStmt),
        bind_stmt(cfg).map(OtherSpecificationStmt::BindStmt),
        codimension_stmt(cfg).map(OtherSpecificationStmt::CodimensionStmt),
        contiguous_stmt(cfg).map(OtherSpecificationStmt::ContiguousStmt),
        dimension_stmt(cfg).map(OtherSpecificationStmt::DimensionStmt),
        external_stmt(cfg).map(OtherSpecificationStmt::ExternalStmt),
        intent_stmt(cfg).map(OtherSpecificationStmt::IntentStmt),
        intrinsic_stmt(cfg).map(OtherSpecificationStmt::IntrinsicStmt),
        namelist_stmt(cfg).map(OtherSpecificationStmt::NamelistStmt),
        optional_stmt(cfg).map(OtherSpecificationStmt::OptionalStmt),
        pointer_stmt(cfg).map(OtherSpecificationStmt::PointerStmt),
        protected_stmt(cfg).map(OtherSpecificationStmt::ProtectedStmt),
        save_stmt(cfg).map(OtherSpecificationStmt::SaveStmt),
        target_stmt(cfg).map(OtherSpecificationStmt::TargetStmt),
        volatile_stmt(cfg).map(OtherSpecificationStmt::VolatileStmt),
        value_stmt(cfg).map(OtherSpecificationStmt::ValueStmt),
        common_stmt(cfg).map(OtherSpecificationStmt::CommonStmt),
        equivalence_stmt(cfg).map(OtherSpecificationStmt::EquivalenceStmt),
    )
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ExecutableConstruct<Span> {
    ActionStmt(ActionStmt<Span>),
    AssociateConstruct(AssociateConstruct<Span>),
    BlockConstruct(BlockConstruct<Span>),
    CaseConstruct(CaseConstruct<Span>),
    ChangeTeamConstruct(ChangeTeamConstruct<Span>),
    CriticalConstruct(CriticalConstruct<Span>),
    DoConstruct(DoConstruct<Span>),
    IfConstruct(IfConstruct<Span>),
    SelectRankConstruct(SelectRankConstruct<Span>),
    SelectTypeConstruct(SelectTypeConstruct<Span>),
    WhereConstruct(WhereConstruct<Span>),
    ForallConstruct(ForallConstruct<Span>),
}

#[syntax_rule(
    F18V007r1 rule "executable-construct" #514 :
    "is action-stmt"
    "or associate-construct"
    "or block-construct"
    "or case-construct"
    "or change-team-construct"
    "or critical-construct"
    "or do-construct"
    "or if-construct"
    "or select-rank-construct"
    "or select-type-construct"
    "or where-construct"
    "or forall-construct",
)]
pub fn executable_construct<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ExecutableConstruct<S::Span>> + 'a {
    alt!(
        action_stmt(cfg).map(ExecutableConstruct::ActionStmt),
        associate_construct(cfg).map(ExecutableConstruct::AssociateConstruct),
        block_construct(cfg).map(ExecutableConstruct::BlockConstruct),
        case_construct(cfg).map(ExecutableConstruct::CaseConstruct),
        change_team_construct(cfg).map(ExecutableConstruct::ChangeTeamConstruct),
        critical_construct(cfg).map(ExecutableConstruct::CriticalConstruct),
        do_construct(cfg).map(ExecutableConstruct::DoConstruct),
        if_construct(cfg).map(ExecutableConstruct::IfConstruct),
        select_rank_construct(cfg).map(ExecutableConstruct::SelectRankConstruct),
        select_type_construct(cfg).map(ExecutableConstruct::SelectTypeConstruct),
        where_construct(cfg).map(ExecutableConstruct::WhereConstruct),
        forall_construct(cfg).map(ExecutableConstruct::ForallConstruct),
    )
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ActionStmt<Span> {
    AllocateStmt(AllocateStmt<Span>),
    AssignmentStmt(AssignmentStmt<Span>),
    BackspaceStmt(BackspaceStmt<Span>),
    CallStmt(CallStmt<Span>),
    CloseStmt(CloseStmt<Span>),
    ContinueStmt(ContinueStmt<Span>),
    CycleStmt(CycleStmt<Span>),
    DeallocateStmt(DeallocateStmt<Span>),
    EndfileStmt(EndfileStmt<Span>),
    ErrorStopStmt(ErrorStopStmt<Span>),
    EventPostStmt(EventPostStmt<Span>),
    EventWaitStmt(EventWaitStmt<Span>),
    ExitStmt(ExitStmt<Span>),
    FailImageStmt(FailImageStmt<Span>),
    FlushStmt(FlushStmt<Span>),
    FormTeamStmt(FormTeamStmt<Span>),
    GotoStmt(GotoStmt<Span>),
    IfStmt(IfStmt<Span>),
    InquireStmt(InquireStmt<Span>),
    LockStmt(LockStmt<Span>),
    NullifyStmt(NullifyStmt<Span>),
    OpenStmt(OpenStmt<Span>),
    PointerAssignmentStmt(PointerAssignmentStmt<Span>),
    PrintStmt(PrintStmt<Span>),
    ReadStmt(ReadStmt<Span>),
    ReturnStmt(ReturnStmt<Span>),
    RewindStmt(RewindStmt<Span>),
    StopStmt(StopStmt<Span>),
    SyncAllStmt(SyncAllStmt<Span>),
    SyncImagesStmt(SyncImagesStmt<Span>),
    SyncMemoryStmt(SyncMemoryStmt<Span>),
    SyncTeamStmt(SyncTeamStmt<Span>),
    UnlockStmt(UnlockStmt<Span>),
    WaitStmt(WaitStmt<Span>),
    WhereStmt(WhereStmt<Span>),
    WriteStmt(WriteStmt<Span>),
    ComputedGotoStmt(ComputedGotoStmt<Span>),
    ForallStmt(ForallStmt<Span>),
}

#[syntax_rule(
    F18V007r1 rule "action-stmt" #515 :
    "is allocate-stmt"
    "or assignment-stmt"
    "or backspace-stmt"
    "or call-stmt"
    "or close-stmt"
    "or continue-stmt"
    "or cycle-stmt"
    "or deallocate-stmt"
    "or endfile-stmt"
    "or error-stop-stmt"
    "or event-post-stmt"
    "or event-wait-stmt"
    "or exit-stmt"
    "or fail-image-stmt"
    "or flush-stmt"
    "or form-team-stmt"
    "or goto-stmt"
    "or if-stmt"
    "or inquire-stmt"
    "or lock-stmt"
    "or nullify-stmt"
    "or open-stmt"
    "or pointer-assignment-stmt"
    "or print-stmt"
    "or read-stmt"
    "or return-stmt"
    "or rewind-stmt"
    "or stop-stmt"
    "or sync-all-stmt"
    "or sync-images-stmt"
    "or sync-memory-stmt"
    "or sync-team-stmt"
    "or unlock-stmt"
    "or wait-stmt"
    "or where-stmt"
    "or write-stmt"
    "or computed-goto-stmt"
    "or forall-stmt",
)]
pub fn action_stmt<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ActionStmt<S::Span>> + 'a {
    alt!(
        allocate_stmt(cfg).map(ActionStmt::AllocateStmt),
        assignment_stmt(cfg).map(ActionStmt::AssignmentStmt),
        backspace_stmt(cfg).map(ActionStmt::BackspaceStmt),
        call_stmt(cfg).map(ActionStmt::CallStmt),
        close_stmt(cfg).map(ActionStmt::CloseStmt),
        continue_stmt(cfg).map(ActionStmt::ContinueStmt),
        cycle_stmt(cfg).map(ActionStmt::CycleStmt),
        deallocate_stmt(cfg).map(ActionStmt::DeallocateStmt),
        endfile_stmt(cfg).map(ActionStmt::EndfileStmt),
        error_stop_stmt(cfg).map(ActionStmt::ErrorStopStmt),
        event_post_stmt(cfg).map(ActionStmt::EventPostStmt),
        event_wait_stmt(cfg).map(ActionStmt::EventWaitStmt),
        exit_stmt(cfg).map(ActionStmt::ExitStmt),
        fail_image_stmt(cfg).map(ActionStmt::FailImageStmt),
        flush_stmt(cfg).map(ActionStmt::FlushStmt),
        form_team_stmt(cfg).map(ActionStmt::FormTeamStmt),
        goto_stmt(cfg).map(ActionStmt::GotoStmt),
        if_stmt(cfg).map(ActionStmt::IfStmt),
        inquire_stmt(cfg).map(ActionStmt::InquireStmt),
        lock_stmt(cfg).map(ActionStmt::LockStmt),
        nullify_stmt(cfg).map(ActionStmt::NullifyStmt),
        open_stmt(cfg).map(ActionStmt::OpenStmt),
        pointer_assignment_stmt(cfg).map(ActionStmt::PointerAssignmentStmt),
        print_stmt(cfg).map(ActionStmt::PrintStmt),
        read_stmt(cfg).map(ActionStmt::ReadStmt),
        return_stmt(cfg).map(ActionStmt::ReturnStmt),
        rewind_stmt(cfg).map(ActionStmt::RewindStmt),
        stop_stmt(cfg).map(ActionStmt::StopStmt),
        sync_all_stmt(cfg).map(ActionStmt::SyncAllStmt),
        sync_images_stmt(cfg).map(ActionStmt::SyncImagesStmt),
        sync_memory_stmt(cfg).map(ActionStmt::SyncMemoryStmt),
        sync_team_stmt(cfg).map(ActionStmt::SyncTeamStmt),
        unlock_stmt(cfg).map(ActionStmt::UnlockStmt),
        wait_stmt(cfg).map(ActionStmt::WaitStmt),
        where_stmt(cfg).map(ActionStmt::WhereStmt),
        write_stmt(cfg).map(ActionStmt::WriteStmt),
        computed_goto_stmt(cfg).map(ActionStmt::ComputedGotoStmt),
        forall_stmt(cfg).map(ActionStmt::ForallStmt),
    )
}
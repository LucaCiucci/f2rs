use super::*;

macro_rules! parse_options {
    (
        $source:expr;
        $(
            $parser:expr => $map:expr,
        )*
    ) => {
        {
            let mut options = Vec::new();

            $(
                if let Some((r, tail)) = $parser.map($map).parse($source.clone()) {
                    if tail.empty() {
                        options.push((r, None));
                    } else {
                        options.push((r, Some(tail.full_span())));
                    }
                }
            )*

            options
        }
    };
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ImplicitPartStmt<Span> {
    Implicit(ImplicitStmt<Span>),
    Parameter(ParameterStmt<Span>),
    Format(FormatStmt<Span>),
    Entry(EntryStmt<Span>),
}

impl<Span> ImplicitPartStmt<Span> {
    pub fn statement_kind_name(&self) -> &'static str {
        match self {
            ImplicitPartStmt::Implicit(_) => "IMPLICIT",
            ImplicitPartStmt::Parameter(_) => "PARAMETER",
            ImplicitPartStmt::Format(_) => "FORMAT",
            ImplicitPartStmt::Entry(_) => "ENTRY",
        }
    }
}

#[doc = s_rule!(
    F18V007r1 rule "implicit-part-stmt" #506 :
    "is implicit-stmt"
    "or parameter-stmt"
    "or format-stmt"
    "or entry-stmt",
)]
pub fn implicit_part_stmt_2<S: Lexed>(source: S) -> PResult<ImplicitPartStmt<MultilineSpan>, S> {
    //println!("PARSING IMPLICIT PART STMT");
    alt!(
        for S =>
        implicit_stmt_2.map(ImplicitPartStmt::Implicit),
        parameter_stmt_2.map(ImplicitPartStmt::Parameter),
        format_stmt_2.map(ImplicitPartStmt::Format),
        entry_stmt_2.map(ImplicitPartStmt::Entry),
    ).parse(source)
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

impl<Span> OtherSpecificationStmt<Span> {
    pub fn statement_kind_name(&self) -> &'static str {
        match self {
            OtherSpecificationStmt::AccessStmt(_) => "ACCESS",
            OtherSpecificationStmt::AllocatableStmt(_) => "ALLOCATABLE",
            OtherSpecificationStmt::AsynchronousStmt(_) => "ASYNCHRONOUS",
            OtherSpecificationStmt::BindStmt(_) => "BIND",
            OtherSpecificationStmt::CodimensionStmt(_) => "CODIMENSION",
            OtherSpecificationStmt::ContiguousStmt(_) => "CONTIGUOUS",
            OtherSpecificationStmt::DimensionStmt(_) => "DIMENSION",
            OtherSpecificationStmt::ExternalStmt(_) => "EXTERNAL",
            OtherSpecificationStmt::IntentStmt(_) => "INTENT",
            OtherSpecificationStmt::IntrinsicStmt(_) => "INTRINSIC",
            OtherSpecificationStmt::NamelistStmt(_) => "NAMELIST",
            OtherSpecificationStmt::OptionalStmt(_) => "OPTIONAL",
            OtherSpecificationStmt::PointerStmt(_) => "POINTER",
            OtherSpecificationStmt::ProtectedStmt(_) => "PROTECTED",
            OtherSpecificationStmt::SaveStmt(_) => "SAVE",
            OtherSpecificationStmt::TargetStmt(_) => "TARGET",
            OtherSpecificationStmt::VolatileStmt(_) => "VOLATILE",
            OtherSpecificationStmt::ValueStmt(_) => "VALUE",
            OtherSpecificationStmt::CommonStmt(_) => "COMMON",
            OtherSpecificationStmt::EquivalenceStmt(_) => "EQUIVALENCE",
        }
    }
}

#[doc = s_rule!(
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
pub fn other_specification_stmt_2<S: Lexed>(source: S) -> PResult<OtherSpecificationStmt<MultilineSpan>, S> {
    //println!("PARSING OTHER SPECIFICATION STMT");
    alt!(
        for S =>
        access_stmt_2.map(OtherSpecificationStmt::AccessStmt),
        allocatable_stmt_2.map(OtherSpecificationStmt::AllocatableStmt),
        asynchronous_stmt_2.map(OtherSpecificationStmt::AsynchronousStmt),
        bind_stmt_2.map(OtherSpecificationStmt::BindStmt),
        codimension_stmt_2.map(OtherSpecificationStmt::CodimensionStmt),
        contiguous_stmt_2.map(OtherSpecificationStmt::ContiguousStmt),
        dimension_stmt_2.map(OtherSpecificationStmt::DimensionStmt),
        external_stmt_2.map(OtherSpecificationStmt::ExternalStmt),
        intent_stmt_2.map(OtherSpecificationStmt::IntentStmt),
        intrinsic_stmt_2.map(OtherSpecificationStmt::IntrinsicStmt),
        namelist_stmt_2.map(OtherSpecificationStmt::NamelistStmt),
        optional_stmt_2.map(OtherSpecificationStmt::OptionalStmt),
        pointer_stmt_2.map(OtherSpecificationStmt::PointerStmt),
        protected_stmt_2.map(OtherSpecificationStmt::ProtectedStmt),
        save_stmt_2.map(OtherSpecificationStmt::SaveStmt),
        target_stmt_2.map(OtherSpecificationStmt::TargetStmt),
        volatile_stmt_2.map(OtherSpecificationStmt::VolatileStmt),
        value_stmt_2.map(OtherSpecificationStmt::ValueStmt),
        common_stmt_2.map(OtherSpecificationStmt::CommonStmt),
        equivalence_stmt_2.map(OtherSpecificationStmt::EquivalenceStmt),
    ).parse(source)
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

impl<Span> ActionStmt<Span> {
    pub fn statement_kind_name(&self) -> &'static str {
        match self {
            ActionStmt::AllocateStmt(_) => "ALLOCATE",
            ActionStmt::AssignmentStmt(_) => "ASSIGNMENT",
            ActionStmt::BackspaceStmt(_) => "BACKSPACE",
            ActionStmt::CallStmt(_) => "CALL",
            ActionStmt::CloseStmt(_) => "CLOSE",
            ActionStmt::ContinueStmt(_) => "CONTINUE",
            ActionStmt::CycleStmt(_) => "CYCLE",
            ActionStmt::DeallocateStmt(_) => "DEALLOCATE",
            ActionStmt::EndfileStmt(_) => "ENDFILE",
            ActionStmt::ErrorStopStmt(_) => "ERROR STOP",
            ActionStmt::EventPostStmt(_) => "EVENT POST",
            ActionStmt::EventWaitStmt(_) => "EVENT WAIT",
            ActionStmt::ExitStmt(_) => "EXIT",
            ActionStmt::FailImageStmt(_) => "FAIL IMAGE",
            ActionStmt::FlushStmt(_) => "FLUSH",
            ActionStmt::FormTeamStmt(_) => "FORM TEAM",
            ActionStmt::GotoStmt(_) => "GOTO",
            ActionStmt::IfStmt(_) => "IF",
            ActionStmt::InquireStmt(_) => "INQUIRE",
            ActionStmt::LockStmt(_) => "LOCK",
            ActionStmt::NullifyStmt(_) => "NULLIFY",
            ActionStmt::OpenStmt(_) => "OPEN",
            ActionStmt::PointerAssignmentStmt(_) => "POINTER ASSIGNMENT",
            ActionStmt::PrintStmt(_) => "PRINT",
            ActionStmt::ReadStmt(_) => "READ",
            ActionStmt::ReturnStmt(_) => "RETURN",
            ActionStmt::RewindStmt(_) => "REWIND",
            ActionStmt::StopStmt(_) => "STOP",
            ActionStmt::SyncAllStmt(_) => "SYNC ALL",
            ActionStmt::SyncImagesStmt(_) => "SYNC IMAGES",
            ActionStmt::SyncMemoryStmt(_) => "SYNC MEMORY",
            ActionStmt::SyncTeamStmt(_) => "SYNC TEAM",
            ActionStmt::UnlockStmt(_) => "UNLOCK",
            ActionStmt::WaitStmt(_) => "WAIT",
            ActionStmt::WhereStmt(_) => "WHERE",
            ActionStmt::WriteStmt(_) => "WRITE",
            ActionStmt::ComputedGotoStmt(_) => "COMPUTED GOTO",
            ActionStmt::ForallStmt(_) => "FORALL",
        }
    }
}

#[doc = s_rule!(
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
pub fn action_stmt<S: Lexed>(source: S) -> PResult<ActionStmt<MultilineSpan>, S> {
    //println!("PARSING ACTION STMT");
    alt!(
        for S =>
        allocate_stmt_2.map(ActionStmt::AllocateStmt),
        assignment_stmt_2.map(ActionStmt::AssignmentStmt),
        backspace_stmt_2.map(ActionStmt::BackspaceStmt),
        call_stmt.map(ActionStmt::CallStmt),
        close_stmt.map(ActionStmt::CloseStmt),
        continue_stmt.map(ActionStmt::ContinueStmt),
        cycle_stmt.map(ActionStmt::CycleStmt),
        deallocate_stmt.map(ActionStmt::DeallocateStmt),
        endfile_stmt.map(ActionStmt::EndfileStmt),
        error_stop_stmt.map(ActionStmt::ErrorStopStmt),
        event_post_stmt.map(ActionStmt::EventPostStmt),
        event_wait_stmt.map(ActionStmt::EventWaitStmt),
        exit_stmt.map(ActionStmt::ExitStmt),
        fail_image_stmt.map(ActionStmt::FailImageStmt),
        flush_stmt.map(ActionStmt::FlushStmt),
        form_team_stmt.map(ActionStmt::FormTeamStmt),
        goto_stmt.map(ActionStmt::GotoStmt),
        if_stmt.map(ActionStmt::IfStmt),
        inquire_stmt.map(ActionStmt::InquireStmt),
        lock_stmt.map(ActionStmt::LockStmt),
        nullify_stmt.map(ActionStmt::NullifyStmt),
        open_stmt.map(ActionStmt::OpenStmt),
        pointer_assignment_stmt.map(ActionStmt::PointerAssignmentStmt),
        print_stmt.map(ActionStmt::PrintStmt),
        read_stmt.map(ActionStmt::ReadStmt),
        return_stmt.map(ActionStmt::ReturnStmt),
        rewind_stmt.map(ActionStmt::RewindStmt),
        stop_stmt.map(ActionStmt::StopStmt),
        sync_all_stmt.map(ActionStmt::SyncAllStmt),
        sync_images_stmt.map(ActionStmt::SyncImagesStmt),
        sync_memory_stmt.map(ActionStmt::SyncMemoryStmt),
        sync_team_stmt.map(ActionStmt::SyncTeamStmt),
        unlock_stmt.map(ActionStmt::UnlockStmt),
        wait_stmt.map(ActionStmt::WaitStmt),
        where_stmt.map(ActionStmt::WhereStmt),
        write_stmt.map(ActionStmt::WriteStmt),
        computed_goto_stmt.map(ActionStmt::ComputedGotoStmt),
        forall_stmt.map(ActionStmt::ForallStmt),
    ).parse(source)
}
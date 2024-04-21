use super::*;

#[derive(Debug, Clone, EnumAsInner)]
pub enum ImplicitPartStmt<Span> {
    Implicit(ImplicitStmt<Span>),
    Parameter(ParameterStmt<Span>),
    Format(FormatStmt<Span>),
    Entry(EntryStmt<Span>),
}

#[syntax_rule(
    F18V007r1 rule "implicit-part-stmt" #506 :
    "is implicit-stmt"
    "or parameter-stmt"
    "or format-stmt"
    "or entry-stmt",
)]
pub fn implicit_part_stmt_2<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = ImplicitPartStmt<S::Span>> + 'a {
    alt!(
        implicit_stmt_2(cfg).map(ImplicitPartStmt::Implicit),
        parameter_stmt_2(cfg).map(ImplicitPartStmt::Parameter),
        format_stmt_2(cfg).map(ImplicitPartStmt::Format),
        entry_stmt_2(cfg).map(ImplicitPartStmt::Entry),
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
pub fn other_specification_stmt_2<'a, S: TextSource + 'a>(cfg: &'a Cfg) -> impl Parser<S, Token = OtherSpecificationStmt<S::Span>> + 'a {
    alt!(
        access_stmt_2(cfg).map(OtherSpecificationStmt::AccessStmt),
        allocatable_stmt_2(cfg).map(OtherSpecificationStmt::AllocatableStmt),
        asynchronous_stmt_2(cfg).map(OtherSpecificationStmt::AsynchronousStmt),
        bind_stmt_2(cfg).map(OtherSpecificationStmt::BindStmt),
        codimension_stmt_2(cfg).map(OtherSpecificationStmt::CodimensionStmt),
        contiguous_stmt_2(cfg).map(OtherSpecificationStmt::ContiguousStmt),
        dimension_stmt_2(cfg).map(OtherSpecificationStmt::DimensionStmt),
        external_stmt_2(cfg).map(OtherSpecificationStmt::ExternalStmt),
        intent_stmt_2(cfg).map(OtherSpecificationStmt::IntentStmt),
        intrinsic_stmt_2(cfg).map(OtherSpecificationStmt::IntrinsicStmt),
        namelist_stmt_2(cfg).map(OtherSpecificationStmt::NamelistStmt),
        optional_stmt_2(cfg).map(OtherSpecificationStmt::OptionalStmt),
        pointer_stmt_2(cfg).map(OtherSpecificationStmt::PointerStmt),
        protected_stmt_2(cfg).map(OtherSpecificationStmt::ProtectedStmt),
        save_stmt_2(cfg).map(OtherSpecificationStmt::SaveStmt),
        target_stmt_2(cfg).map(OtherSpecificationStmt::TargetStmt),
        volatile_stmt_2(cfg).map(OtherSpecificationStmt::VolatileStmt),
        value_stmt_2(cfg).map(OtherSpecificationStmt::ValueStmt),
        common_stmt_2(cfg).map(OtherSpecificationStmt::CommonStmt),
        equivalence_stmt_2(cfg).map(OtherSpecificationStmt::EquivalenceStmt),
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
        allocate_stmt_2(cfg).map(ActionStmt::AllocateStmt),
        assignment_stmt_2(cfg).map(ActionStmt::AssignmentStmt),
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
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
    pub fn statement_name(&self) -> &'static str {
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
pub fn implicit_part_stmt_2<S: Lexed>(source: S) -> Vec<(ImplicitPartStmt<MultilineSpan>, Option<S::Span>)> {
    parse_options!(
        source;
        implicit_stmt_2 => ImplicitPartStmt::Implicit,
        parameter_stmt_2 => ImplicitPartStmt::Parameter,
        format_stmt_2 => ImplicitPartStmt::Format,
        entry_stmt_2 => ImplicitPartStmt::Entry,
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
pub fn other_specification_stmt_2<S: Lexed>(source: S) -> Vec<(OtherSpecificationStmt<MultilineSpan>, Option<S::Span>)> {
    parse_options!(
        source;
        access_stmt_2 => OtherSpecificationStmt::AccessStmt,
        allocatable_stmt_2 => OtherSpecificationStmt::AllocatableStmt,
        asynchronous_stmt_2 => OtherSpecificationStmt::AsynchronousStmt,
        bind_stmt_2 => OtherSpecificationStmt::BindStmt,
        codimension_stmt_2 => OtherSpecificationStmt::CodimensionStmt,
        contiguous_stmt_2 => OtherSpecificationStmt::ContiguousStmt,
        dimension_stmt_2 => OtherSpecificationStmt::DimensionStmt,
        external_stmt_2 => OtherSpecificationStmt::ExternalStmt,
        intent_stmt_2 => OtherSpecificationStmt::IntentStmt,
        intrinsic_stmt_2 => OtherSpecificationStmt::IntrinsicStmt,
        namelist_stmt_2 => OtherSpecificationStmt::NamelistStmt,
        optional_stmt_2 => OtherSpecificationStmt::OptionalStmt,
        pointer_stmt_2 => OtherSpecificationStmt::PointerStmt,
        protected_stmt_2 => OtherSpecificationStmt::ProtectedStmt,
        save_stmt_2 => OtherSpecificationStmt::SaveStmt,
        target_stmt_2 => OtherSpecificationStmt::TargetStmt,
        volatile_stmt_2 => OtherSpecificationStmt::VolatileStmt,
        value_stmt_2 => OtherSpecificationStmt::ValueStmt,
        common_stmt_2 => OtherSpecificationStmt::CommonStmt,
        equivalence_stmt_2 => OtherSpecificationStmt::EquivalenceStmt,
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
pub fn action_stmt<S: Lexed>(source: S) -> Vec<(ActionStmt<MultilineSpan>, Option<S::Span>)> {
    parse_options!(
        source;
        allocate_stmt_2 => ActionStmt::AllocateStmt,
        assignment_stmt_2 => ActionStmt::AssignmentStmt,
        backspace_stmt_2 => ActionStmt::BackspaceStmt,
        call_stmt => ActionStmt::CallStmt,
        close_stmt => ActionStmt::CloseStmt,
        continue_stmt => ActionStmt::ContinueStmt,
        cycle_stmt => ActionStmt::CycleStmt,
        deallocate_stmt => ActionStmt::DeallocateStmt,
        endfile_stmt => ActionStmt::EndfileStmt,
        error_stop_stmt => ActionStmt::ErrorStopStmt,
        event_post_stmt => ActionStmt::EventPostStmt,
        event_wait_stmt => ActionStmt::EventWaitStmt,
        exit_stmt => ActionStmt::ExitStmt,
        fail_image_stmt => ActionStmt::FailImageStmt,
        flush_stmt => ActionStmt::FlushStmt,
        form_team_stmt => ActionStmt::FormTeamStmt,
        goto_stmt => ActionStmt::GotoStmt,
        if_stmt => ActionStmt::IfStmt,
        inquire_stmt => ActionStmt::InquireStmt,
        lock_stmt => ActionStmt::LockStmt,
        nullify_stmt => ActionStmt::NullifyStmt,
        open_stmt => ActionStmt::OpenStmt,
        pointer_assignment_stmt => ActionStmt::PointerAssignmentStmt,
        print_stmt => ActionStmt::PrintStmt,
        read_stmt => ActionStmt::ReadStmt,
        return_stmt => ActionStmt::ReturnStmt,
        rewind_stmt => ActionStmt::RewindStmt,
        stop_stmt => ActionStmt::StopStmt,
        sync_all_stmt => ActionStmt::SyncAllStmt,
        sync_images_stmt => ActionStmt::SyncImagesStmt,
        sync_memory_stmt => ActionStmt::SyncMemoryStmt,
        sync_team_stmt => ActionStmt::SyncTeamStmt,
        unlock_stmt => ActionStmt::UnlockStmt,
        wait_stmt => ActionStmt::WaitStmt,
        where_stmt => ActionStmt::WhereStmt,
        write_stmt => ActionStmt::WriteStmt,
        computed_goto_stmt => ActionStmt::ComputedGotoStmt,
        forall_stmt => ActionStmt::ForallStmt,
    )
}
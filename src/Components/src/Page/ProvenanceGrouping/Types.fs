module Swate.Components.Page.ProvenanceGrouping.Types

open Swate.Components.Page.ProvenanceGrouping.Identifiers
open Swate.Components.Page.ProvenanceGrouping.Values
open Swate.Components.Page.ProvenanceGrouping.Domain
open Swate.Components.Page.ProvenanceGrouping.MutationTypes
open Swate.Components.Page.ProvenanceGrouping.ProjectionTypes

/// The public error union for every editor command. Per D7 the cases are
/// declared in `MutationTypes.fs`, because `Commands.fs` needs them before this
/// file compiles; F# has no re-export, so an abbreviation gives consumers the
/// *name* only. Anything that constructs or matches a case must open
/// `MutationTypes` or qualify it.
type ProvenanceCommandError = MutationTypes.ProvenanceCommandError

/// What the host receives for one controlled change. `Mutations` is the delta
/// for this change alone and is empty for navigation-only changes such as undo
/// or a layer switch. It is not a writeback record: `Session.MutationJournal` is
/// the authoritative ordered log, and it survives undo for free because undo
/// restores a whole prior session snapshot.
type ProvenanceEditorChange = {
    Session: ProvenanceSession
    Mutations: ProvenanceMutation list
}

/// A sidebar value the user has authored but not yet assigned. Drafts are UI
/// state only (intent §3): a draft becomes a session value definition on its
/// first successful assignment. The owner kind is chosen at creation, which is
/// why it travels with the draft rather than being asked for after a drop.
type SidebarDraft = {
    Id: string
    OwnerKind: AnnotationOwnerKind
    Category: ProvenanceTerm
    Value: ProvenanceValue
    Unit: ProvenanceTerm option
}

/// Identifies a property for UI purposes that are per-header rather than
/// per-assignment: which header a side groups by, rail placement and ordering,
/// expansion, and manual colour. Node and process annotations of the same header
/// are always distinct here, matching that they never group together.
type AnnotationHeaderKey = {
    Kind: AnnotationOwnerKind
    Header: ProvenanceTerm
}

/// Design §3.5 names this type for the manual colour map. It is the same
/// `(owner kind, normalized header)` pair as every other per-header UI key, so
/// it is an abbreviation rather than a second record.
type PropertyColorKey = AnnotationHeaderKey

/// The grouping selection for one side: which header's values partition it.
type GroupingKey = AnnotationHeaderKey

[<RequireQualifiedAccess>]
type GroupingScope =
    | Input
    | Output
    | Both

type GroupingAssignment = {
    Key: GroupingKey
    Scope: GroupingScope
}

/// The single-side scope for a side-local grouping action. `Both` is only ever
/// chosen explicitly by the user, never inferred from a side.
let scopeForSide side =
    match side with
    | ProvenanceSide.Input -> GroupingScope.Input
    | ProvenanceSide.Output -> GroupingScope.Output

let scopeAppliesToSide side scope =
    match side, scope with
    | ProvenanceSide.Input, GroupingScope.Input
    | ProvenanceSide.Input, GroupingScope.Both
    | ProvenanceSide.Output, GroupingScope.Output
    | ProvenanceSide.Output, GroupingScope.Both -> true
    | _ -> false

type SideViewState = {
    GroupingAssignments: GroupingAssignment list
}

type ProvenanceDetail =
    | Group of side: ProvenanceSide * groupId: string
    | Connection of connectorId: string

/// Where a value assignment is going. Node and process targets are resolved
/// separately because the two command families take different owners: canonical
/// nodes, or the process links a drop resolved to.
type ValueAssignmentTarget =
    | NodeTargets of Set<CanonicalNodeId>
    | ProcessTargets of Set<ProcessLinkId>

/// A pending overwrite the user must confirm. It carries the assignments it
/// would replace, so confirmation replaces exactly those and an ambiguous
/// same-header conflict can be refused instead of guessed (intent §3).
type ValueAssignmentWarning = {
    Target: ValueAssignmentTarget
    ExistingAssignmentIds: Set<AnnotationAssignmentId>
    Header: ProvenanceTerm
    Value: ProvenanceValue
    Unit: ProvenanceTerm option
}

type ValueAssignmentRequest = {
    Target: ValueAssignmentTarget
    OwnerKind: AnnotationOwnerKind
    PropertyKind: AssignmentPropertyKind
    Category: ProvenanceTerm
    Value: ProvenanceValue
    Unit: ProvenanceTerm option
}

/// What the user dragged. `Key` is the kind-bearing property entry, so conflict
/// detection is scoped to `(owner kind, header)` rather than to the header alone
/// — the same header may exist under distinct concrete kinds. `PropertyKind`
/// carries the stored concrete kind forward, so a reused entry keeps it and the
/// target side never chooses one.
type ValueAssignmentSource = {
    Key: AnnotationHeaderKey
    PropertyKind: AssignmentPropertyKind
    Value: ProvenanceValue
    Unit: ProvenanceTerm option
    /// Set for a container-bound dependent (a Recipe Component), which may only
    /// exist on a link that also carries its Recipe reference.
    ContainerReferenceValueId: PropertyValueDefinitionId option
    /// Set for a reference value, which occupies at most one slot per link.
    ReferenceSlotId: ReferenceSlotId option
    CopiedFromAssignmentId: AnnotationAssignmentId option
}

/// The complete identity of a value rail entry. A value definition id alone is
/// not enough: one definition can be projected from several owner kinds and a
/// draft has no definition id until its first successful assignment.
type PropertyValueDrag = {
    DefinitionId: PropertyValueDefinitionId option
    DraftId: string option
    Source: ValueAssignmentSource
}

type ValueAssignmentPlan =
    | AddCurrent of ValueAssignmentRequest
    | ConfirmOverwrite of ValueAssignmentWarning

type PropertyAssignmentBatch = {
    Adds: ValueAssignmentRequest list
    Overwrites: ValueAssignmentWarning list
}

type PendingAssignmentBatch = {
    Batch: PropertyAssignmentBatch
    DraftId: string option
    Source: ValueAssignmentSource option
    AffectedSideCount: int
    AffectedValueCount: int
    AffectedGroupCount: int
    AffectedEntityCount: int
}

/// The value shape a draft edit is being typed as. Declared here (not in
/// `ControlsSupport.fs`) because `PendingAnnotationEdit` stores it in UI state.
type DraftValueKind =
    | DraftText
    | DraftInteger
    | DraftFloat
    | DraftTerm

/// A downstream annotation edit awaiting its new value (design §4/§7.2). The
/// receiver is where the user right-clicked; it is not necessarily the
/// annotation's owner, which is why the edit still has to resolve through
/// `Commands.editAvailableReferences` rather than assuming ownership.
/// The draft is kept as raw kind/text/term fields, not a typed value: deriving
/// the kind back from a half-typed value would flip it to `Text` on the first
/// keystroke and make switching to `Term` impossible.
type PendingAnnotationEdit = {
    ReceiverId: CanonicalNodeId
    VisibleLinkIds: Set<ProcessLinkId>
    /// Which surface opened the prompt. A connector must resolve to one backing
    /// link; a node card resolves across the links that entity carries.
    Scope: Commands.ProcessEditScope
    Annotations: ProjectedAnnotation list
    Header: AnnotationHeaderKey
    DraftKind: DraftValueKind
    DraftText: string
    DraftTerm: ProvenanceTerm option
    Unit: ProvenanceTerm option
}

/// The seed for a new layer: a name plus the canonical nodes selected on each
/// side. Mixed input/output selection is legitimate here — intent §3 keeps it
/// for layer seeding even though it is never a cross-side annotation target.
/// `Commands.addLayer` consumes it; an empty selection seeds from the active
/// layer's outputs.
type AddLayerRequest = {
    Name: string
    SelectedNodes: (ProvenanceSide * CanonicalNodeId) list
}

/// Provenance shown on a rail value chip. The old model read this from a stored
/// writeback anchor; canonically it is derived — the source name from the owning
/// layer, the process name from the owning structural process, and "current"
/// from the availability relation rather than a table comparison. Node
/// annotations have no process, so `ProcessName` stays `None` for them.
type PropertyValueSourceInfo = {
    SourceName: ProvenanceSourceName option
    ProcessName: ProvenanceProcessName option
    IsCurrent: bool
}

type PanelRatios = { Left: int; Middle: int; Right: int }

[<RequireQualifiedAccess>]
type ConnectionHandleKind =
    | GroupCard
    | GroupMember
    | GroupMemberPropertyAnchor
    | PropertyHeader
    | PropertyValue
    /// Measurement-only anchor on the property-facing edge of a group card.
    /// Property and value connectors attach here; it is never draggable or droppable.
    | GroupPropertyAnchor

type ConnectionHandleRef = {
    Kind: ConnectionHandleKind
    Side: ProvenanceSide
    Id: string
    ParentGroupId: string option
}

type ConnectionPoint = { X: float; Y: float }

type LiveConnectionDrag = {
    Source: ConnectionHandleRef
    Start: ConnectionPoint
    Current: ConnectionPoint
}

type PendingMemberResolution = {
    LayerId: ProvenanceLayerId
    InputGroupId: string
    OutputGroupId: string
    InputMemberCount: int
    OutputMemberCount: int
}

/// Colour state. Per design §3.5 there is no colour *context*: colour is not
/// keyed by component, layer, or viewing shelf. A manual override is keyed by
/// `(owner kind, normalized header)`; everything else resolves from the
/// projected item's backing origin sources.
type PropertyColorSettings = {
    ManualPropertyColors: Map<PropertyColorKey, ProvenanceColor>
    SourceColors: Map<ProvenanceSourceId, ProvenanceColor>
    SourceColorSetOrder: Map<ProvenanceSourceId, int>
    NextSourceColorSetOrder: int
}

[<RequireQualifiedAccess>]
type PropertySort =
    | ValueCountDesc
    | NameAsc
    | ConnectionCountDesc

[<RequireQualifiedAccess>]
type GroupSort =
    | NameAsc
    | MemberCountDesc
    | ConnectionCountDesc

[<RequireQualifiedAccess>]
type PropertyValueCountFilter =
    | Any
    | Singleton
    | Multiple
    | CoverageGap

[<RequireQualifiedAccess>]
type PropertyOriginFilter =
    | AnyOrigin
    | CurrentOnly
    | AnyUpstream
    | Source of ProvenanceSourceId

type FilterState = {
    SearchText: string
    PropertySort: PropertySort
    GroupSort: GroupSort
    ValueCountFilter: PropertyValueCountFilter
    OriginFilter: PropertyOriginFilter
}

type PropertyStats = {
    Property: AnnotationHeaderKey
    DistinctValueCount: int
    ItemsWithValueCount: int
    TotalItemCount: int
}

type PropertyCountBadge =
    | Hide
    | DistinctValues of int
    | Coverage of itemsWithValueCount: int * totalItemCount: int

type LayerSideId = ProvenanceLayerId * ProvenanceSide

type UiState = {
    SideStates: Map<LayerSideId, SideViewState>
    PropertyRailPlacements: Map<ProvenanceLayerId * GroupingKey, ProvenanceSide>
    PropertyRailOrders: Map<LayerSideId, GroupingKey list>
    ExpandedProperties: Set<ProvenanceLayerId * ProvenanceSide * GroupingKey>
    /// Unassigned sidebar drafts, per rail. These are the replacement for the
    /// old palette values and never become session state until assigned.
    Drafts: Map<LayerSideId, SidebarDraft list>
    PendingAssignmentBatch: PendingAssignmentBatch option
    PanelRatios: Map<ProvenanceLayerId, PanelRatios>
    PendingMemberResolution: PendingMemberResolution option
    PendingAnnotationEdit: PendingAnnotationEdit option
    SelectedInputs: Set<ProvenanceLayerId * string>
    SelectedOutputs: Set<ProvenanceLayerId * string>
    /// Explicitly expanded group cards. Manual toggling keeps at most one entry;
    /// manual connection resolution expands the two pending cards together.
    ExpandedGroups: Set<ProvenanceSide * string>
    Detail: ProvenanceDetail option
    Error: string option
    Hint: string option
    PropertyColors: PropertyColorSettings
    Filters: FilterState
}

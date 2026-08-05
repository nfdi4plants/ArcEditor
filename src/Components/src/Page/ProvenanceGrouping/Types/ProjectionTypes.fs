module Swate.Components.Page.ProvenanceGrouping.ProjectionTypes

open Swate.Components.Page.ProvenanceGrouping.Identifiers
open Swate.Components.Page.ProvenanceGrouping.Values
open Swate.Components.Page.ProvenanceGrouping.Domain
open Swate.Components.Page.ProvenanceGrouping.MutationTypes
open Swate.Components.Page.ProvenanceGrouping.AvailabilityTypes

type GroupingValueIdentity =
    | TextIdentity of string
    | IntegerIdentity of int
    | FloatIdentity of float
    | TermIdentity of ProvenanceTerm
    | ReferenceIdentity of scheme: string * id: string

type GroupingValueKey =
    | NodeValue of header: ProvenanceTerm * value: GroupingValueIdentity * unit: ProvenanceTerm option
    | ProcessValue of
        header: ProvenanceTerm *
        value: GroupingValueIdentity *
        unit: ProvenanceTerm option *
        originSource: ProvenanceSourceId

type AssignmentProjectionIdentity = {
    PropertyId: PropertyDefinitionId
    ValueId: PropertyValueDefinitionId
    AssignmentId: AnnotationAssignmentId
    PropertyKind: AssignmentPropertyKind
}

type AssignmentProjectionBacking =
    | NodeAssignmentBacking of
        identity: AssignmentProjectionIdentity *
        ownerId: CanonicalNodeId *
        targetSource: ProvenanceSourceRef option
    | ProcessAssignmentBacking of
        identity: AssignmentProjectionIdentity *
        ownerId: StructuralProcessId *
        linkIds: Set<ProcessLinkId> *
        containerReferenceValueId: PropertyValueDefinitionId option *
        referenceSlotId: ReferenceSlotId option

type AssignmentAvailabilityEvidence = {
    Relation: AvailabilityRelation
    OriginatingLinkIds: Set<ProcessLinkId>
    VisibleThroughLinkIds: Set<ProcessLinkId>
}

type ProjectedAnnotation = {
    Key: GroupingValueKey
    Backing: AssignmentProjectionBacking
    Availability: AssignmentAvailabilityEvidence
    /// The genuinely derived origin - a process assignment's owning layer
    /// source. A node assignment stores no origin (intent §2), so this is
    /// always `None` for one; its optional writeback target still lives on
    /// `NodeAssignmentBacking` and must be read from there, never from here.
    DerivedOriginSource: ProvenanceSourceRef option
}

type DisplayGroup = {
    Id: string
    Side: ProvenanceSide
    /// The normalized composite grouping key that formed this card: the members'
    /// values for the side's active grouping headers, deduplicated and ordered
    /// (intent §7). Empty for an item-specific fallback key, which is what every
    /// item gets while no header is active.
    GroupingValues: GroupingValueKey list
    CanonicalNodeIds: Set<CanonicalNodeId>
    EndpointKeys: Set<LayerEndpointKey>
    ProcessLinkIds: Set<ProcessLinkId>
    Annotations: ProjectedAnnotation list
    /// Exact availability projection for each member appearance. This cannot be
    /// reconstructed from assignment ownership because propagated annotations
    /// are owned by a different canonical node.
    AnnotationsByNodeId: Map<CanonicalNodeId, ProjectedAnnotation list>
}

type DisplayConnector = {
    Id: string
    InputGroupId: string
    OutputGroupId: string
    StructuralProcessIds: Set<StructuralProcessId>
    LinkIds: Set<ProcessLinkId>
    InputEndpointKeys: Set<LayerEndpointKey>
    OutputEndpointKeys: Set<LayerEndpointKey>
    Annotations: ProjectedAnnotation list
}

type ProcessOnlyEntry = {
    StructuralProcessId: StructuralProcessId
    LinkId: ProcessLinkId
    Annotations: ProjectedAnnotation list
}

type AssignmentBackedShelfPayload = {
    Backing: AssignmentProjectionBacking
    Availability: AssignmentAvailabilityEvidence
    CanonicalNodeIds: Set<CanonicalNodeId>
    EndpointKeys: Set<LayerEndpointKey>
}

type CatalogShelfPayload = { Entry: ReferenceCatalogEntry }

type PropertyShelfPayload =
    | AssignmentBacked of AssignmentBackedShelfPayload
    | CatalogBacked of CatalogShelfPayload

type PropertyShelfEntry = {
    Id: string
    Payload: PropertyShelfPayload
}

type CachedLayerProjection = {
    TopologyRevision: int
    ValueRevision: int
    Stale: bool
    Groups: DisplayGroup list
    Connectors: DisplayConnector list
    ProcessOnlyEntries: ProcessOnlyEntry list
    ShelfEntries: PropertyShelfEntry list
}

type ProvenanceSession = {
    Nodes: Map<CanonicalNodeId, CanonicalNode>
    Processes: Map<StructuralProcessId, StructuralProcess>
    Properties: Map<PropertyDefinitionId, PropertyDefinition>
    Values: Map<PropertyValueDefinitionId, PropertyValueDefinition>
    Layers: Map<ProvenanceLayerId, ProvenanceLayer>
    LayerOrder: ProvenanceLayerId list
    ActiveLayerId: ProvenanceLayerId
    AvailabilityTopologyRevision: int
    AnnotationValueRevision: int
    ReachabilityMemo: Map<CanonicalNodeId, ForwardAvailabilityMemo>
    LayerProjections: Map<ProvenanceLayerId, CachedLayerProjection>
    MutationJournal: ProvenanceMutation list
}

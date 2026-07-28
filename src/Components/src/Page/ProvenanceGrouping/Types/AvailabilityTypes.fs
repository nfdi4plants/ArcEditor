module Swate.Components.Page.ProvenanceGrouping.AvailabilityTypes

open Swate.Components.Page.ProvenanceGrouping.Identifiers

type AnnotationOwner =
    | NodeOwner of CanonicalNodeId
    | ProcessOwner of StructuralProcessId

type AvailabilityRelation =
    | OwnedNode
    | IncidentProcess of ProcessLinkId
    | ForwardPropagated of route: ProcessLinkId list
    | ReverseConnectionLocal of ProcessLinkId

type AvailableAnnotationRef = {
    AssignmentId: AnnotationAssignmentId
    ValueId: PropertyValueDefinitionId
    Owner: AnnotationOwner
    Relation: AvailabilityRelation
    OriginatingLinkIds: Set<ProcessLinkId>
    VisibleThroughLinkIds: Set<ProcessLinkId>
}

// Reachability evidence only. This deliberately does not carry ValueId:
// repointing an assignment advances only the value revision, so this memo may
// survive while projection resolves the assignment's current value.
type ReachabilityEvidence = {
    AssignmentId: AnnotationAssignmentId
    Owner: AnnotationOwner
    Relation: AvailabilityRelation
    OriginatingLinkIds: Set<ProcessLinkId>
    VisibleThroughLinkIds: Set<ProcessLinkId>
}

type ForwardAvailabilityMemo = {
    // Readable only while this equals AvailabilityTopologyRevision.
    TopologyRevision: int
    Evidence: ReachabilityEvidence list
}

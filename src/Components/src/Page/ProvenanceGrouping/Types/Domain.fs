module Swate.Components.Page.ProvenanceGrouping.Domain

open Swate.Components.Page.ProvenanceGrouping.Identifiers
open Swate.Components.Page.ProvenanceGrouping.Values

type NodeAssignment = {
    Id: AnnotationAssignmentId
    ValueId: PropertyValueDefinitionId
    PropertyKind: AssignmentPropertyKind
    // A canonical node is source-agnostic, so it stores no origin. Its layer
    // membership and color derive from its endpoint appearances.
    TargetSource: ProvenanceSourceRef option
    Lineage: AssignmentLineage
}

type ProcessAssignment = {
    Id: AnnotationAssignmentId
    ValueId: PropertyValueDefinitionId
    PropertyKind: AssignmentPropertyKind
    CoveredLinkIds: Set<ProcessLinkId>
    ContainerReferenceValueId: PropertyValueDefinitionId option
    ReferenceSlotId: ReferenceSlotId option
    // Origin derives from the owning structural process and its origin layer.
    Lineage: AssignmentLineage
}

type CanonicalNode = {
    Id: CanonicalNodeId
    Key: CanonicalNodeKey
    Kind: ProvenanceKind
    Name: string
    Assignments: Map<AnnotationAssignmentId, NodeAssignment>
}

type ProcessLink = {
    Id: ProcessLinkId
    Shape: ProcessLinkShape
}

type StructuralProcess = {
    Id: StructuralProcessId
    OriginLayerId: ProvenanceLayerId
    Name: ProvenanceProcessName option
    Links: Map<ProcessLinkId, ProcessLink>
    Assignments: Map<AnnotationAssignmentId, ProcessAssignment>
}

type LayerEndpointKey = {
    LayerId: ProvenanceLayerId
    Side: ProvenanceSide
    NodeId: CanonicalNodeId
}

type LayerEndpoint = {
    Key: LayerEndpointKey
    Header: ProvenanceIOHeader
    LayerOrderPosition: int
}

type ProvenanceLayer = {
    Id: ProvenanceLayerId
    Label: string
    Source: ProvenanceSourceRef
    InputEndpoints: Map<CanonicalNodeId, LayerEndpoint>
    OutputEndpoints: Map<CanonicalNodeId, LayerEndpoint>
    StructuralProcessIds: Set<StructuralProcessId>
}

type ReferenceDependentProcessValue = {
    Key: string
    Category: ProvenanceTerm
    Value: ProvenanceValue
    Unit: ProvenanceTerm option
    PropertyKind: AssignmentPropertyKind
}

type ReferenceCatalogEntry = {
    Category: ProvenanceTerm
    Reference: ReferenceValue
    Unit: ProvenanceTerm option
    AssignmentKind: AnnotationOwnerKind
    PropertyKind: AssignmentPropertyKind
    Cardinality: ReferenceCardinality
    DependentProcessValues: ReferenceDependentProcessValue list
}

type ReferenceCatalog = Map<string * string, ReferenceCatalogEntry>

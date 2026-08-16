module Swate.Components.Page.ProvenanceGrouping.MutationTypes

open Swate.Components.Page.ProvenanceGrouping.Identifiers
open Swate.Components.Page.ProvenanceGrouping.Values
open Swate.Components.Page.ProvenanceGrouping.Domain

type AssignmentOwnerRef =
    | NodeAssignmentOwner of CanonicalNodeId
    | ProcessAssignmentOwner of StructuralProcessId

type MutationSemanticScope =
    | GlobalDefinition
    | OwnerScoped of owners: Set<AssignmentOwnerRef>

type ResolvedMutationCoverage = {
    AssignmentIds: Set<AnnotationAssignmentId>
    LinkIds: Set<ProcessLinkId>
}

type MutationContext = {
    Scope: MutationSemanticScope
    Coverage: ResolvedMutationCoverage
}

type NodeAssignmentTombstone = {
    OwnerId: CanonicalNodeId
    Assignment: NodeAssignment
}

type ProcessAssignmentTombstone = {
    OwnerId: StructuralProcessId
    Assignment: ProcessAssignment
}

type AssignmentTombstone =
    | NodeTombstone of NodeAssignmentTombstone
    | ProcessTombstone of ProcessAssignmentTombstone

type ProvenanceMutation =
    | CanonicalNodeCreated of CanonicalNode
    | LayerEndpointAdded of LayerEndpoint
    | StructuralProcessCreated of StructuralProcess
    | StructuralProcessReshaped of before: StructuralProcess * after: StructuralProcess
    | ProcessLinkAdded of processId: StructuralProcessId * link: ProcessLink
    | ProcessLinkRemoved of processId: StructuralProcessId * link: ProcessLink * context: MutationContext
    | PropertyDefinitionCreated of PropertyDefinition
    | PropertyDefinitionUpdated of before: PropertyDefinition * after: PropertyDefinition * context: MutationContext
    | PropertyValueDefinitionCreated of PropertyValueDefinition
    | PropertyValueDefinitionUpdated of
        before: PropertyValueDefinition *
        after: PropertyValueDefinition *
        context: MutationContext
    | PropertyValueDefinitionDeleted of
        value: PropertyValueDefinition *
        assignmentTombstones: AssignmentTombstone list *
        context: MutationContext
    | PropertyDefinitionDeleted of
        property: PropertyDefinition *
        values: PropertyValueDefinition list *
        assignmentTombstones: AssignmentTombstone list *
        context: MutationContext
    | NodeAssignmentAdded of ownerId: CanonicalNodeId * assignment: NodeAssignment * context: MutationContext
    | NodeAssignmentValueChanged of
        ownerId: CanonicalNodeId *
        before: NodeAssignment *
        after: NodeAssignment *
        context: MutationContext
    | NodeAssignmentRemoved of tombstone: NodeAssignmentTombstone * context: MutationContext
    | ProcessAssignmentAdded of ownerId: StructuralProcessId * assignment: ProcessAssignment * context: MutationContext
    | ProcessAssignmentCoverageChanged of
        ownerId: StructuralProcessId *
        before: ProcessAssignment *
        after: ProcessAssignment *
        context: MutationContext
    | ProcessAssignmentValueChanged of
        ownerId: StructuralProcessId *
        before: ProcessAssignment *
        after: ProcessAssignment *
        context: MutationContext
    | ProcessAssignmentSplit of
        ownerId: StructuralProcessId *
        original: ProcessAssignment *
        retained: ProcessAssignment *
        split: ProcessAssignment *
        context: MutationContext
    | ProcessAssignmentRemoved of tombstone: ProcessAssignmentTombstone * context: MutationContext
    | AdapterResourceReferenceReplaced of
        ownerId: StructuralProcessId *
        before: ProcessAssignment *
        after: ProcessAssignment *
        removedDependents: ProcessAssignmentTombstone list *
        addedDependents: ProcessAssignment list *
        context: MutationContext

type ProvenanceCommandError =
    | NodeNotFound of CanonicalNodeId
    | LayerNotFound of ProvenanceLayerId
    | ProcessNotFound of StructuralProcessId
    | LinkNotFound of ProcessLinkId
    | AssignmentNotFound of owner: AssignmentOwnerRef option * assignmentId: AnnotationAssignmentId
    | PropertyNotFound of PropertyDefinitionId
    | ValueNotFound of PropertyValueDefinitionId
    | DuplicateEndpointAppearance of LayerEndpointKey
    | EmptyTarget
    | AmbiguousPooledEdit of linkIds: Set<ProcessLinkId> * assignmentIds: Set<AnnotationAssignmentId>
    | ReadOnlyReverseLocalEdit of assignmentId: AnnotationAssignmentId * throughLinkId: ProcessLinkId
    | ReadOnlyReverseLocalRemoval of assignmentId: AnnotationAssignmentId * throughLinkId: ProcessLinkId
    | PropagatedRemovalAtReceiver of assignmentId: AnnotationAssignmentId * receiverId: CanonicalNodeId
    | OverwriteConfirmationRequired of propertyId: PropertyDefinitionId * assignmentIds: Set<AnnotationAssignmentId>
    | MultiplePropertyValues of propertyId: PropertyDefinitionId * assignmentIds: Set<AnnotationAssignmentId>
    | MixedPropertyValueCounts of propertyId: PropertyDefinitionId * countsByLink: Map<ProcessLinkId, int>
    | MissingReferenceContainer of
        assignmentId: AnnotationAssignmentId *
        requiredValueId: PropertyValueDefinitionId *
        linkIds: Set<ProcessLinkId>
    | ReferenceSlotOccupied of
        slotId: ReferenceSlotId *
        linkIds: Set<ProcessLinkId> *
        assignmentIds: Set<AnnotationAssignmentId>
    | ReadOnlyAdapterResourceMutation
    | InconsistentCanonicalState of details: string
    | InconsistentLayerProjection of layerId: ProvenanceLayerId * details: string

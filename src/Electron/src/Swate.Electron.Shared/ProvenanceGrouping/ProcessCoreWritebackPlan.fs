module Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreWritebackPlan

open System
open System.Globalization
open ProcessCore
open Swate.Components.ProcessCore.Copy
open Swate.Components.Page.ProvenanceGrouping.Identifiers
open Swate.Components.Page.ProvenanceGrouping.Values
open Swate.Components.Page.ProvenanceGrouping.Domain
open Swate.Components.Page.ProvenanceGrouping.MutationTypes
open Swate.Components.Page.ProvenanceGrouping.ProjectionTypes
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreAdapterTypes
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreGraph

/// Complete, order-independent identity of one process-assignment occurrence
/// in a materialized ProcessCore process partition.
[<RequireQualifiedAccess>]
type ProcessCoreProcessAssignmentFingerprint =
    | AnnotationFingerprint of ProcessCoreCanonicalAnnotationFingerprint
    | RecipeReferenceFingerprint of scheme: string * resourceKey: RecipeResourceKey

type PlannedAnnotation = {
    AssignmentId: AnnotationAssignmentId
    Fingerprint: ProcessCoreCanonicalAnnotationFingerprint
    RegistryId: string
    SourceLocations: ProcessCoreCanonicalAnnotationLocation list
    ControlledByOperation: bool
    TargetSource: ProvenanceSourceRef option
    TargetDestination: ProcessCoreProcessGroupLocation option
}

type PlannedNode = {
    NodeId: CanonicalNodeId
    Key: CanonicalNodeKey
    Kind: ProvenanceKind
    ExistingLocations: ProcessCoreCanonicalNodeSourceLocation list
    IsNew: bool
    Annotations: PlannedAnnotation list
}

type PlannedProcessAssignment = {
    AssignmentId: AnnotationAssignmentId
    Fingerprint: ProcessCoreProcessAssignmentFingerprint
    Annotation: PlannedAnnotation option
}

type PlannedProcessPartition = {
    Id: string
    StructuralProcessId: StructuralProcessId
    Signature: Set<AnnotationAssignmentId * ProcessCoreProcessAssignmentFingerprint>
    Links: Set<ProcessLinkId>
    Assignments: PlannedProcessAssignment list
}

[<RequireQualifiedAccess>]
type PlannedProcessDisposition =
    | ReuseIndexed
    | CloneIndexed
    | NewProcess

[<RequireQualifiedAccess>]
type RecipeAssociationChange =
    | Keep
    | Set
    | Replace
    | Clear

type PlannedRecipeAssociation = {
    StructuralProcessId: StructuralProcessId
    LinkId: ProcessLinkId
    IndexedProcess: ProcessCoreProcessLocation option
    Change: RecipeAssociationChange
    PreviousResource: ProcessCoreRecipeResourceLocation option
    FinalResource: ProcessCoreRecipeResourceLocation option
}

type PlannedProcess = {
    StructuralProcessId: StructuralProcessId
    LinkId: ProcessLinkId
    Shape: ProcessLinkShape
    PartitionId: string
    ProcessName: string
    Destination: ProcessCoreProcessGroupLocation
    DestinationOrder: int
    Disposition: PlannedProcessDisposition
    IndexedProcess: ProcessCoreProcessLocation option
    ReusesIndexedProcess: bool
    RecipeAssociation: PlannedRecipeAssociation option
}

type PlannedAnnotationRemoval = {
    AssignmentId: AnnotationAssignmentId
    Locations: ProcessCoreCanonicalAnnotationLocation list
}

type PlannedProcessRemoval = {
    StructuralProcessId: StructuralProcessId
    Location: ProcessCoreProcessLocation
}

type PlannedAnnotationReminting = {
    AssignmentId: AnnotationAssignmentId
    OriginalRegistryId: string
    PlannedRegistryId: string
    OriginalFingerprint: ProcessCoreCanonicalAnnotationFingerprint
    PlannedFingerprint: ProcessCoreCanonicalAnnotationFingerprint
}

type ProcessCoreWritebackPlan = {
    Nodes: PlannedNode list
    Partitions: PlannedProcessPartition list
    Processes: PlannedProcess list
    ProcessRemovals: PlannedProcessRemoval list
    RecipeAssociations: PlannedRecipeAssociation list
    AnnotationRemovals: PlannedAnnotationRemoval list
    AnnotationRemintings: PlannedAnnotationReminting list
    Summary: ProcessCoreWritebackSummary
    /// Recipe resources are assign-only. This field is intentionally fixed at
    /// zero and makes the absence of a resource-creation path observable.
    RecipeResourcesAdded: int
}

type private AssignmentOwner =
    | NodeOwner of CanonicalNodeId * NodeAssignment
    | ProcessOwner of StructuralProcessId * ProcessAssignment

type private ResolvedRecipeAssignment = {
    Assignment: ProcessAssignment
    Value: PropertyValueDefinition
    Reference: ReferenceValue
    Resource: ProcessCoreRecipeResourceLocation
}

type private ProcessPlanningState = {
    StructuralProcess: StructuralProcess
    Destination: ProcessCoreProcessGroupLocation
    IndexedProcess: ProcessCoreProcessLocation option
    OrdinaryAssignments: PlannedProcessAssignment list
    RecipeAssignments: ResolvedRecipeAssignment list
    Partitions: PlannedProcessPartition list
}

let private error message =
    ProcessCoreCanonicalWritebackError.InvalidPreparedState message

let private addError (errors: ResizeArray<ProcessCoreCanonicalWritebackError>) value = errors.Add value

let private distinctErrors (errors: ResizeArray<ProcessCoreCanonicalWritebackError>) =
    errors |> Seq.distinct |> Seq.toList

let private tryStableRecipeResourceKeyValue key =
    try
        if obj.ReferenceEquals(box key, null) then
            None
        else
            match key with
            | RecipeResourceKey.ById id when isNull id -> None
            | RecipeResourceKey.ByMetadata(name, version, url) when
                name |> Option.exists isNull
                || version |> Option.exists isNull
                || url |> Option.exists isNull
                ->
                None
            | _ -> Some(RecipeResourceKey.toStableString key)
    with _ ->
        None

let private stableRecipeResourceKeyOrInvalid key =
    tryStableRecipeResourceKeyValue key
    |> Option.defaultValue "<invalid-recipe-resource-key>"

let private tryStableRecipeResourceKey (errors: ResizeArray<ProcessCoreCanonicalWritebackError>) context key =
    match tryStableRecipeResourceKeyValue key with
    | Some stable -> Some stable
    | None ->
        addError errors (error $"Recipe resource key for '{context}' is null or malformed.")

        None

let private tryStableRecipeKeyFromResource
    (errors: ResizeArray<ProcessCoreCanonicalWritebackError>)
    context
    (recipe: Recipe)
    =
    try
        if obj.ReferenceEquals(box recipe, null) then
            None
        else
            recipe |> RecipeResourceKey.ofRecipe |> tryStableRecipeResourceKeyValue
    with _ ->
        None
    |> function
        | Some stable -> Some stable
        | None ->
            addError errors (error $"Recipe payload for '{context}' has a null or malformed resource key.")

            None

let private isComponentKind =
    function
    | AssignmentPropertyKind.AdapterSpecific kind -> kind.Id = ProcessCoreCanonicalKinds.componentKind.Id
    | AssignmentPropertyKind.Generic -> false

let private isRecipeKind =
    function
    | AssignmentPropertyKind.AdapterSpecific kind -> kind.Id = ProcessCoreCanonicalKinds.processCoreRecipeKind.Id
    | AssignmentPropertyKind.Generic -> false

let private isRecipeReferenceValue =
    function
    | ProvenanceValue.Reference reference -> reference.Scheme = ProcessCoreCanonicalKinds.processCoreRecipeScheme
    | _ -> false

let private isRecipeValueId (session: ProvenanceSession) valueId =
    session.Values
    |> Map.tryFind valueId
    |> Option.exists (fun definition -> isRecipeReferenceValue definition.Value)

let private assignmentsById (errors: ResizeArray<ProcessCoreCanonicalWritebackError>) (session: ProvenanceSession) =
    let mutable owners: Map<AnnotationAssignmentId, AssignmentOwner> = Map.empty

    let add assignmentId owner =
        if owners.ContainsKey assignmentId then
            addError errors (error $"Annotation assignment identity '{assignmentId}' occurs on more than one owner.")
        else
            owners <- owners |> Map.add assignmentId owner

    for KeyValue(storedNodeId, node) in session.Nodes do
        if storedNodeId <> node.Id then
            addError
                errors
                (ProcessCoreCanonicalWritebackError.InconsistentCanonicalState
                    $"Node map key '{storedNodeId}' differs from embedded ID '{node.Id}'.")

        for KeyValue(storedAssignmentId, assignment) in node.Assignments do
            if storedAssignmentId <> assignment.Id then
                addError
                    errors
                    (ProcessCoreCanonicalWritebackError.InconsistentCanonicalState
                        $"Node assignment map key '{storedAssignmentId}' differs from embedded ID '{assignment.Id}'.")

            add assignment.Id (NodeOwner(node.Id, assignment))

    for KeyValue(storedProcessId, structuralProcess) in session.Processes do
        if storedProcessId <> structuralProcess.Id then
            addError
                errors
                (ProcessCoreCanonicalWritebackError.InconsistentCanonicalState
                    $"Process map key '{storedProcessId}' differs from embedded ID '{structuralProcess.Id}'.")

        for KeyValue(storedAssignmentId, assignment) in structuralProcess.Assignments do
            if storedAssignmentId <> assignment.Id then
                addError
                    errors
                    (ProcessCoreCanonicalWritebackError.InconsistentCanonicalState
                        $"Process assignment map key '{storedAssignmentId}' differs from embedded ID '{assignment.Id}'.")

            add assignment.Id (ProcessOwner(structuralProcess.Id, assignment))

    owners

let private assignmentLineage =
    function
    | NodeOwner(_, assignment) -> assignment.Lineage
    | ProcessOwner(_, assignment) -> assignment.Lineage

let private assignmentLineages (session: ProvenanceSession) (owners: Map<AnnotationAssignmentId, AssignmentOwner>) =
    let mutable lineages = owners |> Map.map (fun _ owner -> assignmentLineage owner)

    let add assignmentId lineage =
        if not (lineages.ContainsKey assignmentId) then
            lineages <- lineages |> Map.add assignmentId lineage

    let addNode (assignment: NodeAssignment) = add assignment.Id assignment.Lineage

    let addProcess (assignment: ProcessAssignment) = add assignment.Id assignment.Lineage

    let addTombstone =
        function
        | AssignmentTombstone.NodeTombstone tombstone -> addNode tombstone.Assignment
        | AssignmentTombstone.ProcessTombstone tombstone -> addProcess tombstone.Assignment

    for mutation in session.MutationJournal do
        match mutation with
        | ProvenanceMutation.NodeAssignmentAdded(_, assignment, _) -> addNode assignment
        | ProvenanceMutation.NodeAssignmentValueChanged(_, before, after, _) ->
            addNode before
            addNode after
        | ProvenanceMutation.NodeAssignmentRemoved(tombstone, _) -> addNode tombstone.Assignment
        | ProvenanceMutation.ProcessAssignmentAdded(_, assignment, _) -> addProcess assignment
        | ProvenanceMutation.ProcessAssignmentCoverageChanged(_, before, after, _)
        | ProvenanceMutation.ProcessAssignmentValueChanged(_, before, after, _) ->
            addProcess before
            addProcess after
        | ProvenanceMutation.ProcessAssignmentSplit(_, original, retained, split, _) ->
            addProcess original
            addProcess retained
            addProcess split
        | ProvenanceMutation.ProcessAssignmentRemoved(tombstone, _) -> addProcess tombstone.Assignment
        | ProvenanceMutation.PropertyValueDefinitionDeleted(_, tombstones, _)
        | ProvenanceMutation.PropertyDefinitionDeleted(_, _, tombstones, _) -> tombstones |> List.iter addTombstone
        | ProvenanceMutation.AdapterResourceReferenceReplaced(_, before, after, removed, added, _) ->
            addProcess before
            addProcess after
            removed |> List.iter (fun tombstone -> addProcess tombstone.Assignment)
            added |> List.iter addProcess
        | _ -> ()

    lineages

let private sourceFingerprintForAssignment
    (errors: ResizeArray<ProcessCoreCanonicalWritebackError>)
    (index: ProcessCoreCanonicalIndex)
    (lineages: Map<AnnotationAssignmentId, AssignmentLineage>)
    assignmentId
    =
    let rec resolve visited currentId =
        if visited |> Set.contains currentId then
            addError errors (error $"Assignment lineage for '{assignmentId}' contains a cycle at '{currentId}'.")

            None
        else
            let sourceFingerprints =
                index.AssignmentLocations
                |> Map.tryFind currentId
                |> Option.defaultValue []
                |> List.map _.Fingerprint
                |> List.distinct

            match sourceFingerprints with
            | [ fingerprint ] -> Some fingerprint
            | first :: second :: _ ->
                addError
                    errors
                    (ProcessCoreCanonicalWritebackError.ConflictingAnnotationIdentity(
                        currentId,
                        first.Payload,
                        second.Payload
                    ))

                Some first
            | [] ->
                match lineages |> Map.tryFind currentId with
                | Some(AssignmentLineage.DerivedFrom parentId) -> resolve (visited |> Set.add currentId) parentId
                | Some AssignmentLineage.Loaded ->
                    addError errors (error $"Loaded assignment '{currentId}' has no indexed annotation occurrence.")

                    None
                | Some AssignmentLineage.Created
                | Some(AssignmentLineage.DerivedFromCatalog _) -> None
                | None ->
                    addError
                        errors
                        (error $"Assignment '{assignmentId}' derives from unknown assignment '{currentId}'.")

                    None

    resolve Set.empty assignmentId

let private validateCanonicalState
    (errors: ResizeArray<ProcessCoreCanonicalWritebackError>)
    (session: ProvenanceSession)
    =
    for KeyValue(_, node) in session.Nodes do
        if node.Key.KindId <> node.Kind.Id || node.Key.Name <> node.Name then
            addError
                errors
                (ProcessCoreCanonicalWritebackError.InconsistentCanonicalState
                    $"Node '{node.Id}' has an identity key that differs from its kind or name.")

    session.Nodes
    |> Map.toList
    |> List.groupBy (snd >> _.Key)
    |> List.iter (fun (key, nodes) ->
        let nodeIds = nodes |> List.map fst

        if nodeIds.Length > 1 then
            addError
                errors
                (ProcessCoreCanonicalWritebackError.ConflictingNodeIdentity($"{key.KindId}:{key.Name}", nodeIds))
    )

    let mutable linkOwners: Map<ProcessLinkId, StructuralProcessId> = Map.empty

    for KeyValue(processId, structuralProcess) in session.Processes do
        if not (session.Layers.ContainsKey structuralProcess.OriginLayerId) then
            addError errors (ProcessCoreCanonicalWritebackError.LayerNotFound structuralProcess.OriginLayerId)

        for KeyValue(storedLinkId, link) in structuralProcess.Links do
            if storedLinkId <> link.Id then
                addError errors (ProcessCoreCanonicalWritebackError.InvalidProcessLink storedLinkId)

            match linkOwners |> Map.tryFind link.Id with
            | Some existingOwner when existingOwner <> processId ->
                addError
                    errors
                    (ProcessCoreCanonicalWritebackError.InconsistentCanonicalState
                        $"Link '{link.Id}' is owned by both '{existingOwner}' and '{processId}'.")
            | _ -> linkOwners <- linkOwners |> Map.add link.Id processId

            let nodeIds =
                match link.Shape with
                | ProcessLinkShape.Between(inputId, outputId) -> [ inputId; outputId ]
                | ProcessLinkShape.InputOnly inputId -> [ inputId ]
                | ProcessLinkShape.OutputOnly outputId -> [ outputId ]
                | ProcessLinkShape.Endpointless -> []

            for nodeId in nodeIds do
                if not (session.Nodes.ContainsKey nodeId) then
                    addError errors (ProcessCoreCanonicalWritebackError.NodeNotFound nodeId)

        for KeyValue(_, assignment) in structuralProcess.Assignments do
            if assignment.CoveredLinkIds.IsEmpty then
                addError
                    errors
                    (ProcessCoreCanonicalWritebackError.InconsistentCanonicalState
                        $"Process assignment '{assignment.Id}' has empty coverage.")

            for linkId in assignment.CoveredLinkIds do
                if not (structuralProcess.Links.ContainsKey linkId) then
                    addError errors (ProcessCoreCanonicalWritebackError.LinkNotFound linkId)

    for KeyValue(_, node) in session.Nodes do
        for KeyValue(_, assignment) in node.Assignments do
            if not (session.Values.ContainsKey assignment.ValueId) then
                addError errors (ProcessCoreCanonicalWritebackError.ValueNotFound assignment.ValueId)

    for KeyValue(_, structuralProcess) in session.Processes do
        for KeyValue(_, assignment) in structuralProcess.Assignments do
            if not (session.Values.ContainsKey assignment.ValueId) then
                addError errors (ProcessCoreCanonicalWritebackError.ValueNotFound assignment.ValueId)

    for KeyValue(valueId, definition) in session.Values do
        if valueId <> definition.Id then
            addError
                errors
                (ProcessCoreCanonicalWritebackError.InconsistentCanonicalState
                    $"Value map key '{valueId}' differs from embedded ID '{definition.Id}'.")

        if not (session.Properties.ContainsKey definition.PropertyId) then
            addError errors (ProcessCoreCanonicalWritebackError.ValueNotFound definition.Id)

let private validateResourceJournal
    (errors: ResizeArray<ProcessCoreCanonicalWritebackError>)
    (session: ProvenanceSession)
    (owners: Map<AnnotationAssignmentId, AssignmentOwner>)
    =
    let recipeAssignment (assignment: ProcessAssignment) =
        isRecipeKind assignment.PropertyKind
        || isRecipeValueId session assignment.ValueId

    let componentAssignment (assignment: ProcessAssignment) =
        assignment.ContainerReferenceValueId.IsSome
        || isComponentKind assignment.PropertyKind

    let assignmentIsRecipeOrComponent assignmentId =
        owners
        |> Map.tryFind assignmentId
        |> Option.exists (
            function
            | NodeOwner _ -> false
            | ProcessOwner(_, assignment) -> recipeAssignment assignment || componentAssignment assignment
        )

    let valueIsRecipe valueId = isRecipeValueId session valueId

    let definitionIsRecipe (definition: PropertyValueDefinition) = isRecipeReferenceValue definition.Value

    let valueIsComponent valueId =
        owners
        |> Map.exists (fun _ owner ->
            match owner with
            | ProcessOwner(_, assignment) -> assignment.ValueId = valueId && componentAssignment assignment
            | NodeOwner _ -> false
        )

    let propertyIsRecipe propertyId =
        session.Values
        |> Map.exists (fun _ definition ->
            definition.PropertyId = propertyId
            && (definitionIsRecipe definition || valueIsRecipe definition.Id)
        )

    let componentAssignmentForProperty propertyId =
        session.Values
        |> Map.toSeq
        |> Seq.filter (fun (_, definition) -> definition.PropertyId = propertyId)
        |> Seq.tryPick (fun (valueId, _) ->
            owners
            |> Map.tryPick (fun assignmentId owner ->
                match owner with
                | ProcessOwner(_, assignment) when assignment.ValueId = valueId && componentAssignment assignment ->
                    Some assignmentId
                | _ -> None
            )
        )

    let tombstoneIsRecipe =
        function
        | AssignmentTombstone.ProcessTombstone tombstone -> recipeAssignment tombstone.Assignment
        | AssignmentTombstone.NodeTombstone _ -> false

    let tombstoneIsComponent =
        function
        | AssignmentTombstone.ProcessTombstone tombstone -> componentAssignment tombstone.Assignment
        | AssignmentTombstone.NodeTombstone _ -> false

    let recipeAdds =
        session.MutationJournal
        |> List.choose (
            function
            | ProvenanceMutation.ProcessAssignmentAdded(ownerId, assignment, context) when recipeAssignment assignment ->
                Some(ownerId, assignment, context)
            | _ -> None
        )

    let recipeRemovals =
        session.MutationJournal
        |> List.choose (
            function
            | ProvenanceMutation.ProcessAssignmentRemoved(tombstone, context) when recipeAssignment tombstone.Assignment ->
                Some(tombstone.OwnerId, tombstone.Assignment, context)
            | _ -> None
        )

    let recipeCoverageChanges =
        session.MutationJournal
        |> List.choose (
            function
            | ProvenanceMutation.ProcessAssignmentCoverageChanged(ownerId, before, after, context) when
                recipeAssignment before || recipeAssignment after
                ->
                Some(ownerId, before, after, context)
            | _ -> None
        )

    let adapterReplacements =
        session.MutationJournal
        |> List.choose (
            function
            | ProvenanceMutation.AdapterResourceReferenceReplaced(ownerId, before, after, removed, added, context) ->
                Some(ownerId, before, after, removed, added, context)
            | _ -> None
        )

    let componentAddIsAtomic ownerId (assignment: ProcessAssignment) context =
        adapterReplacements
        |> List.exists (fun (replacementOwnerId, _, _, _, added, replacementContext) ->
            replacementOwnerId = ownerId
            && replacementContext = context
            && added |> List.exists (fun dependent -> dependent = assignment)
        )
        || recipeAdds
           |> List.exists (fun (recipeOwnerId, recipe, recipeContext) ->
               recipeOwnerId = ownerId
               && recipeContext = context
               && assignment.ContainerReferenceValueId = Some recipe.ValueId
               && assignment.CoveredLinkIds = recipe.CoveredLinkIds
           )

    let componentRemovalIsAtomic (tombstone: ProcessAssignmentTombstone) context =
        adapterReplacements
        |> List.exists (fun (ownerId, _, _, removed, _, replacementContext) ->
            ownerId = tombstone.OwnerId
            && replacementContext = context
            && removed |> List.exists ((=) tombstone)
        )
        || recipeRemovals
           |> List.exists (fun (ownerId, recipe, recipeContext) ->
               ownerId = tombstone.OwnerId
               && recipeContext = context
               && tombstone.Assignment.ContainerReferenceValueId = Some recipe.ValueId
               && tombstone.Assignment.CoveredLinkIds = recipe.CoveredLinkIds
           )

    let componentCoverageIsAtomic ownerId (before: ProcessAssignment) (after: ProcessAssignment) context =
        recipeCoverageChanges
        |> List.exists (fun (recipeOwnerId, recipeBefore, recipeAfter, recipeContext) ->
            recipeOwnerId = ownerId
            && recipeContext = context
            && before.ContainerReferenceValueId = Some recipeBefore.ValueId
            && after.ContainerReferenceValueId = Some recipeAfter.ValueId
            && before.CoveredLinkIds = recipeBefore.CoveredLinkIds
            && after.CoveredLinkIds = recipeAfter.CoveredLinkIds
        )

    for mutation in session.MutationJournal do
        match mutation with
        | ProvenanceMutation.PropertyDefinitionUpdated(before, after, _) when
            propertyIsRecipe before.Id || propertyIsRecipe after.Id
            ->
            addError errors ProcessCoreCanonicalWritebackError.ReadOnlyRecipeResourceMutation
        | ProvenanceMutation.PropertyDefinitionUpdated(before, after, _) when
            componentAssignmentForProperty before.Id |> Option.isSome
            || componentAssignmentForProperty after.Id |> Option.isSome
            ->
            let assignmentId =
                componentAssignmentForProperty after.Id
                |> Option.orElseWith (fun () -> componentAssignmentForProperty before.Id)

            addError errors (ProcessCoreCanonicalWritebackError.ReadOnlyRecipeComponentMutation assignmentId)
        | ProvenanceMutation.PropertyDefinitionDeleted(_, definitions, tombstones, _) when
            definitions |> List.exists definitionIsRecipe
            || tombstones |> List.exists tombstoneIsRecipe
            ->
            addError errors ProcessCoreCanonicalWritebackError.ReadOnlyRecipeResourceMutation
        | ProvenanceMutation.PropertyDefinitionDeleted(_, definitions, tombstones, _) when
            definitions |> List.exists (fun definition -> valueIsComponent definition.Id)
            || tombstones |> List.exists tombstoneIsComponent
            ->
            addError errors (ProcessCoreCanonicalWritebackError.ReadOnlyRecipeComponentMutation None)
        | ProvenanceMutation.PropertyValueDefinitionUpdated(before, after, _) when
            definitionIsRecipe before
            || definitionIsRecipe after
            || valueIsRecipe before.Id
            || valueIsRecipe after.Id
            ->
            addError errors ProcessCoreCanonicalWritebackError.ReadOnlyRecipeResourceMutation
        | ProvenanceMutation.PropertyValueDefinitionDeleted(definition, tombstones, _) when
            definitionIsRecipe definition
            || valueIsRecipe definition.Id
            || tombstones |> List.exists tombstoneIsRecipe
            ->
            addError errors ProcessCoreCanonicalWritebackError.ReadOnlyRecipeResourceMutation
        | ProvenanceMutation.PropertyValueDefinitionUpdated(before, after, _) when
            valueIsComponent before.Id || valueIsComponent after.Id
            ->
            let assignmentId =
                owners
                |> Map.tryPick (fun assignmentId owner ->
                    match owner with
                    | ProcessOwner(_, assignment) when assignment.ValueId = after.Id -> Some assignmentId
                    | _ -> None
                )

            addError errors (ProcessCoreCanonicalWritebackError.ReadOnlyRecipeComponentMutation assignmentId)
        | ProvenanceMutation.PropertyValueDefinitionDeleted(definition, tombstones, _) when
            valueIsComponent definition.Id || tombstones |> List.exists tombstoneIsComponent
            ->
            addError errors (ProcessCoreCanonicalWritebackError.ReadOnlyRecipeComponentMutation None)
        | ProvenanceMutation.ProcessAssignmentValueChanged(_, before, after, _) when
            recipeAssignment before || recipeAssignment after
            ->
            addError errors ProcessCoreCanonicalWritebackError.ReadOnlyRecipeResourceMutation
        | ProvenanceMutation.ProcessAssignmentValueChanged(_, before, after, _) when
            componentAssignment before || componentAssignment after
            ->
            addError errors (ProcessCoreCanonicalWritebackError.ReadOnlyRecipeComponentMutation(Some after.Id))
        | ProvenanceMutation.ProcessAssignmentAdded(ownerId, assignment, context) when
            componentAssignment assignment
            && not (componentAddIsAtomic ownerId assignment context)
            ->
            addError errors (ProcessCoreCanonicalWritebackError.ReadOnlyRecipeComponentMutation(Some assignment.Id))
        | ProvenanceMutation.ProcessAssignmentRemoved(tombstone, context) when
            componentAssignment tombstone.Assignment
            && not (componentRemovalIsAtomic tombstone context)
            ->
            addError
                errors
                (ProcessCoreCanonicalWritebackError.ReadOnlyRecipeComponentMutation(Some tombstone.Assignment.Id))
        | ProvenanceMutation.ProcessAssignmentCoverageChanged(ownerId, before, after, context) when
            componentAssignment before || componentAssignment after
            ->
            if not (componentCoverageIsAtomic ownerId before after context) then
                addError errors (ProcessCoreCanonicalWritebackError.ReadOnlyRecipeComponentMutation(Some after.Id))
        | ProvenanceMutation.ProcessAssignmentSplit(_, original, retained, split, _) when
            componentAssignment original
            || componentAssignment retained
            || componentAssignment split
            ->
            addError errors (ProcessCoreCanonicalWritebackError.ReadOnlyRecipeComponentMutation(Some split.Id))
        | ProvenanceMutation.NodeAssignmentAdded(_, assignment, _) when
            isComponentKind assignment.PropertyKind
            || isRecipeValueId session assignment.ValueId
            ->
            addError errors (error "Recipe resources and Components cannot be node assignments.")
        | ProvenanceMutation.NodeAssignmentRemoved(tombstone, _) when
            isComponentKind tombstone.Assignment.PropertyKind
            || isRecipeValueId session tombstone.Assignment.ValueId
            ->
            addError errors (error "Recipe resources and Components cannot be node assignments.")
        | ProvenanceMutation.NodeAssignmentValueChanged(_, before, after, _) when
            assignmentIsRecipeOrComponent before.Id
            || assignmentIsRecipeOrComponent after.Id
            ->
            addError errors (error "Recipe resources and Components cannot be node assignments.")
        | _ -> ()

let private sameNodeSourceOccurrence
    (left: ProcessCoreCanonicalNodeSourceLocation)
    (right: ProcessCoreCanonicalNodeSourceLocation)
    =
    left.ProcessGroup = right.ProcessGroup
    && left.Process = right.Process
    && left.Side = right.Side
    && left.Node = right.Node

let private sourceNodeId (index: ProcessCoreCanonicalIndex) (sourceLocation: ProcessCoreCanonicalNodeSourceLocation) =
    index.NodeLocations
    |> Map.tryPick (fun nodeId locations ->
        if locations |> List.exists (sameNodeSourceOccurrence sourceLocation) then
            Some nodeId
        else
            None
    )

let private indexedProcessLinkIds (index: ProcessCoreCanonicalIndex) processId =
    index.ProcessLocations
    |> Map.tryFind processId
    |> Option.map (fun processLocation ->
        index.LinkLocations
        |> Map.toSeq
        |> Seq.choose (fun (linkId, location) ->
            if location.Process = processLocation then
                Some linkId
            else
                None
        )
        |> Set.ofSeq
    )

let private indexedLink (index: ProcessCoreCanonicalIndex) processId linkId =
    match index.ProcessLocations |> Map.tryFind processId, index.LinkLocations |> Map.tryFind linkId with
    | Some processLocation, Some location when location.Process = processLocation ->
        let resolveEndpoint =
            function
            | None -> Some None
            | Some sourceLocation -> sourceNodeId index sourceLocation |> Option.map Some

        match resolveEndpoint location.Input, resolveEndpoint location.Output with
        | Some(Some inputId), Some(Some outputId) ->
            Some {
                Id = linkId
                Shape = ProcessLinkShape.Between(inputId, outputId)
            }
        | Some(Some inputId), Some None ->
            Some {
                Id = linkId
                Shape = ProcessLinkShape.InputOnly inputId
            }
        | Some None, Some(Some outputId) ->
            Some {
                Id = linkId
                Shape = ProcessLinkShape.OutputOnly outputId
            }
        | Some None, Some None ->
            Some {
                Id = linkId
                Shape = ProcessLinkShape.Endpointless
            }
        | _ -> None
    | _ -> None

let private indexedProcessLinks index processId =
    indexedProcessLinkIds index processId
    |> Option.bind (fun linkIds ->
        let links =
            linkIds
            |> Set.toList
            |> List.choose (fun linkId -> indexedLink index processId linkId |> Option.map (fun link -> linkId, link))

        if links.Length = linkIds.Count then
            links |> Map.ofList |> Some
        else
            None
    )

let private canonicalNodePropertyKind (mappings: ProcessCoreGenericPropertyMappings) (annotation: Annotation) =
    match annotation.AdditionalType with
    | Some additionalType when additionalType = mappings.Node.AdditionalType -> AssignmentPropertyKind.Generic
    | Some "CharacteristicValue" -> AssignmentPropertyKind.AdapterSpecific ProcessCoreCanonicalKinds.characteristic
    | Some "FactorValue" -> AssignmentPropertyKind.AdapterSpecific ProcessCoreCanonicalKinds.factor
    | Some "ParameterValue" -> AssignmentPropertyKind.AdapterSpecific ProcessCoreCanonicalKinds.parameter
    | Some "Component" -> AssignmentPropertyKind.AdapterSpecific ProcessCoreCanonicalKinds.componentKind
    | _ -> AssignmentPropertyKind.AdapterSpecific ProcessCoreCanonicalKinds.additionalProperty

let private canonicalProcessParameterKind (mappings: ProcessCoreGenericPropertyMappings) (annotation: Annotation) =
    match annotation.AdditionalType with
    | Some additionalType when additionalType = mappings.Process.AdditionalType -> AssignmentPropertyKind.Generic
    | _ -> AssignmentPropertyKind.AdapterSpecific ProcessCoreCanonicalKinds.parameter

let private tryAnnotationFingerprint (fingerprint: ProcessCoreCanonicalAnnotationFingerprint) =
    try
        if isNull fingerprint.Payload then
            None
        else
            Some(ProcessCore.Yaml.Annotation.fromYamlString false fingerprint.Payload)
    with _ ->
        None

let private validateJournalJustification
    (errors: ResizeArray<ProcessCoreCanonicalWritebackError>)
    (index: ProcessCoreCanonicalIndex)
    (session: ProvenanceSession)
    (owners: Map<AnnotationAssignmentId, AssignmentOwner>)
    =
    let removalTombstones =
        session.MutationJournal
        |> List.collect (
            function
            | ProvenanceMutation.NodeAssignmentRemoved(tombstone, context) -> [
                AssignmentTombstone.NodeTombstone tombstone, context
              ]
            | ProvenanceMutation.ProcessAssignmentRemoved(tombstone, context) -> [
                AssignmentTombstone.ProcessTombstone tombstone, context
              ]
            | ProvenanceMutation.PropertyValueDefinitionDeleted(_, tombstones, context)
            | ProvenanceMutation.PropertyDefinitionDeleted(_, _, tombstones, context) ->
                tombstones |> List.map (fun tombstone -> tombstone, context)
            | ProvenanceMutation.AdapterResourceReferenceReplaced(_, _, _, removed, _, context) ->
                removed
                |> List.map (fun tombstone -> AssignmentTombstone.ProcessTombstone tombstone, context)
            | _ -> []
        )

    let previousRecipeAssignments =
        session.MutationJournal
        |> List.collect (
            function
            | ProvenanceMutation.ProcessAssignmentRemoved(tombstone, _) when
                isRecipeKind tombstone.Assignment.PropertyKind
                  ->
                  [ tombstone.OwnerId, tombstone.Assignment ]
            | ProvenanceMutation.AdapterResourceReferenceReplaced(ownerId, before, _, _, _, _) -> [ ownerId, before ]
            | _ -> []
        )

    let finalAssignmentIds = owners |> Map.keys |> Set.ofSeq

    let processIdForLocation processLocation =
        index.ProcessLocations
        |> Map.tryPick (fun processId location -> if location = processLocation then Some processId else None)

    let nodeIdForLocation nodeLocation =
        index.NodeLocations
        |> Map.tryPick (fun nodeId locations ->
            if locations |> List.exists (fun location -> location.Node = nodeLocation) then
                Some nodeId
            else
                None
        )

    let contextCovers assignmentId linkIds context =
        context.Coverage.AssignmentIds.Contains assignmentId
        && Set.isSubset linkIds context.Coverage.LinkIds

    let exactNodeRemoval
        assignmentId
        (locations: ProcessCoreCanonicalAnnotationLocation list)
        (tombstone: NodeAssignmentTombstone)
        context
        =
        let exactLocations =
            locations
            |> List.forall (fun location ->
                match location.Owner, tryAnnotationFingerprint location.Fingerprint with
                | ProcessCoreCanonicalAnnotationOwner.NodeAdditionalProperty nodeLocation, Some annotation ->
                    nodeIdForLocation nodeLocation = Some tombstone.OwnerId
                    && tombstone.Assignment.PropertyKind = canonicalNodePropertyKind
                        index.GenericPropertyMappings
                        annotation
                    && Map.tryFind assignmentId index.AssignmentValueIds = Some tombstone.Assignment.ValueId
                | _ -> false
            )

        tombstone.Assignment.Id = assignmentId
        && tombstone.Assignment.Lineage = AssignmentLineage.Loaded
        && tombstone.Assignment.TargetSource.IsNone
        && exactLocations
        && contextCovers assignmentId Set.empty context

    let exactProcessParameterRemoval
        assignmentId
        (locations: ProcessCoreCanonicalAnnotationLocation list)
        (tombstone: ProcessAssignmentTombstone)
        context
        =
        let expectedLinkIds =
            indexedProcessLinkIds index tombstone.OwnerId |> Option.defaultValue Set.empty

        let exactLocations =
            locations
            |> List.forall (fun location ->
                match location.Owner, tryAnnotationFingerprint location.Fingerprint with
                | ProcessCoreCanonicalAnnotationOwner.ProcessParameterValue processLocation, Some annotation ->
                    processIdForLocation processLocation = Some tombstone.OwnerId
                    && assignmentId = $"{tombstone.OwnerId}::parameter:{location.Position}"
                    && tombstone.Assignment.PropertyKind = canonicalProcessParameterKind
                        index.GenericPropertyMappings
                        annotation
                    && Map.tryFind assignmentId index.AssignmentValueIds = Some tombstone.Assignment.ValueId
                | _ -> false
            )

        tombstone.Assignment.Id = assignmentId
        && tombstone.Assignment.Lineage = AssignmentLineage.Loaded
        && tombstone.Assignment.CoveredLinkIds = expectedLinkIds
        && tombstone.Assignment.ContainerReferenceValueId.IsNone
        && tombstone.Assignment.ReferenceSlotId.IsNone
        && exactLocations
        && contextCovers assignmentId expectedLinkIds context

    let exactRecipeComponentRemoval
        assignmentId
        (locations: ProcessCoreCanonicalAnnotationLocation list)
        (tombstone: ProcessAssignmentTombstone)
        context
        =
        let resourceAndLocation =
            match locations with
            | [ location ] ->
                match location.Owner with
                | ProcessCoreCanonicalAnnotationOwner.RecipeComponent(scheme, resourceId) ->
                    index.RecipeResources
                    |> Map.tryFind (scheme, resourceId)
                    |> Option.map (fun resource -> resourceId, resource, location)
                | _ -> None
            | _ -> None

        match resourceAndLocation with
        | None -> false
        | Some(resourceId, resource, location) ->
            let expectedOwnerId =
                resource.ReferencingProcesses
                |> List.choose processIdForLocation
                |> List.tryFind (fun processId -> assignmentId = $"{processId}::recipe-component:{location.Position}")

            let expectedLinkIds =
                expectedOwnerId
                |> Option.bind (indexedProcessLinkIds index)
                |> Option.defaultValue Set.empty

            let expectedComponent =
                resource.Components
                |> List.tryFind (fun componentLocation -> componentLocation.Position = location.Position)

            let exactValue =
                match expectedComponent, tryAnnotationFingerprint location.Fingerprint with
                | Some componentLocation, Some annotation ->
                    componentLocation.Fingerprint = location.Fingerprint
                    && Map.tryFind assignmentId index.AssignmentValueIds = Some tombstone.Assignment.ValueId
                | _ -> false

            let exactContainer =
                previousRecipeAssignments
                |> List.exists (fun (ownerId, recipe) ->
                    Some ownerId = expectedOwnerId
                    && recipe.Id = $"{ownerId}::recipe"
                    && recipe.Lineage = AssignmentLineage.Loaded
                    && isRecipeKind recipe.PropertyKind
                    && recipe.ReferenceSlotId = Some ProcessCoreCanonicalKinds.processCoreExecutesRecipeSlot
                    && recipe.ContainerReferenceValueId.IsNone
                    && Map.tryFind $"{ownerId}::recipe" index.AssignmentValueIds = Some recipe.ValueId
                    && recipe.ValueId = (tombstone.Assignment.ContainerReferenceValueId |> Option.defaultValue "")
                    && recipe.CoveredLinkIds = expectedLinkIds
                )

            Some tombstone.OwnerId = expectedOwnerId
            && tombstone.Assignment.Id = assignmentId
            && tombstone.Assignment.PropertyKind = AssignmentPropertyKind.AdapterSpecific
                ProcessCoreCanonicalKinds.componentKind
            && tombstone.Assignment.CoveredLinkIds = expectedLinkIds
            && tombstone.Assignment.ReferenceSlotId.IsNone
            && tombstone.Assignment.Lineage = AssignmentLineage.DerivedFromCatalog(
                resource.Scheme,
                resourceId,
                $"{resourceId}/component/{location.Position}"
            )
            && exactValue
            && exactContainer
            && contextCovers assignmentId expectedLinkIds context

    let exactIndexedRemovalIsJournalled assignmentId locations =
        removalTombstones
        |> List.exists (fun (tombstone, context) ->
            match tombstone with
            | AssignmentTombstone.NodeTombstone removed -> exactNodeRemoval assignmentId locations removed context
            | AssignmentTombstone.ProcessTombstone removed ->
                if
                    locations
                    |> List.exists (fun location ->
                        match location.Owner with
                        | ProcessCoreCanonicalAnnotationOwner.RecipeComponent _ -> true
                        | _ -> false
                    )
                then
                    exactRecipeComponentRemoval assignmentId locations removed context
                else
                    exactProcessParameterRemoval assignmentId locations removed context
        )

    for KeyValue(assignmentId, locations) in index.AssignmentLocations do
        if not (finalAssignmentIds.Contains assignmentId) then
            if not (exactIndexedRemovalIsJournalled assignmentId locations) then
                addError
                    errors
                    (error
                        $"Indexed assignment '{assignmentId}' is absent without its exact loaded owner, assignment record, and removal context.")

        if locations.IsEmpty then
            addError errors (error $"Indexed assignment '{assignmentId}' has no source occurrence.")

    let assignmentCreationIsJournalled assignmentId owner =
        session.MutationJournal
        |> List.exists (
            function
            | ProvenanceMutation.NodeAssignmentAdded(ownerId, assignment, _) ->
                match owner with
                | NodeOwner(expectedOwnerId, finalAssignment) ->
                    ownerId = expectedOwnerId
                    && assignment.Id = assignmentId
                    && assignment = finalAssignment
                | ProcessOwner _ -> false
            | ProvenanceMutation.ProcessAssignmentAdded(ownerId, assignment, _) ->
                match owner with
                | ProcessOwner(expectedOwnerId, finalAssignment) ->
                    ownerId = expectedOwnerId
                    && assignment.Id = assignmentId
                    && assignment = finalAssignment
                | NodeOwner _ -> false
            | ProvenanceMutation.ProcessAssignmentSplit(ownerId, _, _, split, _) ->
                match owner with
                | ProcessOwner(expectedOwnerId, finalAssignment) ->
                    ownerId = expectedOwnerId && split.Id = assignmentId && split = finalAssignment
                | NodeOwner _ -> false
            | ProvenanceMutation.AdapterResourceReferenceReplaced(ownerId, _, after, _, added, _) ->
                match owner with
                | ProcessOwner(expectedOwnerId, finalAssignment) ->
                    ownerId = expectedOwnerId
                    && (after = finalAssignment || added |> List.exists ((=) finalAssignment))
                | NodeOwner _ -> false
            | _ -> false
        )

    /// A loaded assignment stays on the ProcessCore object it was read from. The indexed
    /// occurrence, not final state, decides which owner that is, so an assignment whose final
    /// owner no longer matches its source occurrence has moved without a witness.
    let indexedOwnershipIsIntact assignmentId owner =
        index.AssignmentLocations
        |> Map.tryFind assignmentId
        |> Option.defaultValue []
        |> List.forall (fun location ->
            match location.Owner, owner with
            | ProcessCoreCanonicalAnnotationOwner.NodeAdditionalProperty nodeLocation, NodeOwner(nodeId, _) ->
                index.NodeLocations
                |> Map.tryFind nodeId
                |> Option.defaultValue []
                |> List.exists (fun indexedNode -> indexedNode.Node = nodeLocation)
            | ProcessCoreCanonicalAnnotationOwner.ProcessParameterValue processLocation, ProcessOwner(processId, _) ->
                index.ProcessLocations |> Map.tryFind processId = Some processLocation
            // Components are process-owned and separately guarded as read-only.
            | ProcessCoreCanonicalAnnotationOwner.RecipeComponent _, ProcessOwner _ -> true
            | _ -> false
        )

    for KeyValue(assignmentId, owner) in owners do
        let hasIndexedOccurrence =
            index.AssignmentLocations
            |> Map.tryFind assignmentId
            |> Option.exists (not << List.isEmpty)

        if not hasIndexedOccurrence then
            let loadedRecipe =
                match owner with
                | ProcessOwner(_, assignment) ->
                    assignment.Lineage = AssignmentLineage.Loaded
                    && isRecipeKind assignment.PropertyKind
                | NodeOwner _ -> false

            if not loadedRecipe && not (assignmentCreationIsJournalled assignmentId owner) then
                addError
                    errors
                    (error $"Unindexed assignment '{assignmentId}' has no semantic creation or split mutation.")

        if not (indexedOwnershipIsIntact assignmentId owner) then
            addError
                errors
                (error $"Indexed assignment '{assignmentId}' changed owner without an exact semantic transition.")

    let coverageChangeIsJournalled processId originalLinkIds assignment =
        let expectedBefore = {
            assignment with
                CoveredLinkIds = originalLinkIds
        }

        let changedLinkIds =
            (originalLinkIds - assignment.CoveredLinkIds)
            + (assignment.CoveredLinkIds - originalLinkIds)

        let contextCoversChange context =
            context.Coverage.AssignmentIds.Contains assignment.Id
            && Set.isSubset changedLinkIds context.Coverage.LinkIds

        session.MutationJournal
        |> List.exists (
            function
            | ProvenanceMutation.ProcessAssignmentCoverageChanged(ownerId, before, after, context) ->
                ownerId = processId
                && before = expectedBefore
                && after = assignment
                && contextCoversChange context
            | ProvenanceMutation.ProcessAssignmentSplit(ownerId, original, retained, _, context) ->
                ownerId = processId
                && original = expectedBefore
                && retained = assignment
                && contextCoversChange context
            | ProvenanceMutation.AdapterResourceReferenceReplaced(ownerId, before, after, _, _, context) ->
                ownerId = processId
                && before.Id = assignment.Id
                && before.CoveredLinkIds = originalLinkIds
                && after = assignment
                && contextCoversChange context
            | _ -> false
        )

    for KeyValue(assignmentId, owner) in owners do
        match owner with
        | NodeOwner _ -> ()
        | ProcessOwner(processId, assignment) ->
            let hasIndexedOccurrence =
                index.AssignmentLocations
                |> Map.tryFind assignmentId
                |> Option.exists (not << List.isEmpty)

            let isLoadedRecipe =
                assignment.Lineage = AssignmentLineage.Loaded
                && isRecipeKind assignment.PropertyKind

            match indexedProcessLinkIds index processId with
            | Some originalLinkIds when
                (hasIndexedOccurrence || isLoadedRecipe)
                && assignment.CoveredLinkIds <> originalLinkIds
                && not (coverageChangeIsJournalled processId originalLinkIds assignment)
                ->
                if isComponentKind assignment.PropertyKind then
                    addError
                        errors
                        (ProcessCoreCanonicalWritebackError.ReadOnlyRecipeComponentMutation(Some assignment.Id))
                else
                    addError
                        errors
                        (error
                            $"Indexed assignment '{assignmentId}' changed link coverage without an exact semantic coverage mutation.")
            | _ -> ()

    let createdProcesses =
        session.MutationJournal
        |> List.choose (
            function
            | ProvenanceMutation.StructuralProcessCreated structuralProcess ->
                Some(structuralProcess.Id, structuralProcess)
            | _ -> None
        )
        |> Map.ofList

    let validateProcessJournal
        processId
        (initialProcess: StructuralProcess)
        (initialLinks: Map<ProcessLinkId, ProcessLink>)
        (finalProcess: StructuralProcess)
        =
        let mutable replayedLinks = initialLinks
        let mutable exact = true

        let identityMatches (candidate: StructuralProcess) =
            candidate.Id = processId
            && candidate.OriginLayerId = initialProcess.OriginLayerId
            && candidate.Name = initialProcess.Name

        for mutation in session.MutationJournal do
            match mutation with
            | ProvenanceMutation.ProcessLinkAdded(ownerId, added) when ownerId = processId ->
                match replayedLinks |> Map.tryFind added.Id with
                | None -> replayedLinks <- replayedLinks |> Map.add added.Id added
                | Some existing when existing = added -> ()
                | Some _ -> exact <- false
            | ProvenanceMutation.ProcessLinkRemoved(ownerId, removed, context) when ownerId = processId ->
                if
                    replayedLinks |> Map.tryFind removed.Id = Some removed
                    && context.Coverage.LinkIds.Contains removed.Id
                then
                    replayedLinks <- replayedLinks |> Map.remove removed.Id
                else
                    exact <- false
            | ProvenanceMutation.StructuralProcessReshaped(before, after) when
                before.Id = processId || after.Id = processId
                ->
                if identityMatches before && identityMatches after && before.Links = replayedLinks then
                    replayedLinks <- after.Links
                else
                    exact <- false
            | _ -> ()

        if not exact || replayedLinks <> finalProcess.Links then
            addError
                errors
                (error
                    $"Final links for structural process '{processId}' do not replay exactly from their indexed or created snapshot.")

    /// A loaded process belongs to the layer whose source owns its indexed process group.
    /// No journal mutation renames or relocates a loaded process, so this is checked against
    /// the indexed snapshot rather than against anything derived from final state.
    let indexedOriginLayerIsIntact
        processId
        (processLocation: ProcessCoreProcessLocation)
        (finalProcess: StructuralProcess)
        =
        match session.Layers |> Map.tryFind finalProcess.OriginLayerId with
        | None -> false
        | Some layer ->
            layer.StructuralProcessIds.Contains processId
            && (index.SourceLocations
                |> Map.tryFind layer.Source.Id
                |> Option.exists (fun groupLocation ->
                    groupLocation.DatasetPath = processLocation.DatasetPath
                    && groupLocation.ProcessGroupName = processLocation.ExpectedName
                ))

    for KeyValue(processId, processLocation) in index.ProcessLocations do
        match session.Processes |> Map.tryFind processId with
        | None -> ()
        | Some finalProcess ->
            if finalProcess.Name <> Some processLocation.ExpectedName then
                addError
                    errors
                    (error
                        $"Indexed structural process '{processId}' changed its name from its ProcessCore source snapshot.")

            if not (indexedOriginLayerIsIntact processId processLocation finalProcess) then
                addError
                    errors
                    (error $"Indexed structural process '{processId}' changed its origin layer without evidence.")

        match indexedProcessLinks index processId with
        | None ->
            addError errors (error $"Indexed structural process '{processId}' has no reconstructable link snapshot.")
        | Some initialLinks ->
            let finalProcess =
                session.Processes
                |> Map.tryFind processId
                |> Option.defaultValue {
                    Id = processId
                    OriginLayerId = ""
                    Name = Some processLocation.ExpectedName
                    Links = Map.empty
                    Assignments = Map.empty
                }

            // The replay baseline is the indexed snapshot. Deriving the initial name from
            // finalProcess would make an unjournalled rename structurally undetectable.
            let initialProcess = {
                finalProcess with
                    Name = Some processLocation.ExpectedName
                    Links = initialLinks
            }

            validateProcessJournal processId initialProcess initialLinks finalProcess

    for KeyValue(processId, structuralProcess) in session.Processes do
        if not (index.ProcessLocations.ContainsKey processId) then
            match createdProcesses |> Map.tryFind processId with
            | None ->
                addError
                    errors
                    (error $"Unindexed structural process '{processId}' has no StructuralProcessCreated mutation.")
            | Some created ->
                if
                    created.OriginLayerId <> structuralProcess.OriginLayerId
                    || created.Name <> structuralProcess.Name
                then
                    addError
                        errors
                        (error $"Created structural process '{processId}' differs from its journalled origin or name.")

                validateProcessJournal processId created created.Links structuralProcess

/// An assignment is controlled by this operation only when the journal witnesses a transition
/// attributed to its exact final owner and ending at its exact final record. A mutation that
/// merely mentions an assignment or value ID authorizes nothing: a forged or unrelated entry
/// would otherwise license arbitrary divergence from the indexed annotation.
let private transitionControlsAssignment assignmentId owner mutation =
    match owner, mutation with
    | NodeOwner(ownerId, finalAssignment), ProvenanceMutation.NodeAssignmentAdded(mutationOwnerId, assignment, _) ->
        mutationOwnerId = ownerId
        && assignment.Id = assignmentId
        && assignment = finalAssignment
    | NodeOwner(ownerId, finalAssignment),
      ProvenanceMutation.NodeAssignmentValueChanged(mutationOwnerId, before, after, _) ->
        mutationOwnerId = ownerId && before.Id = assignmentId && after = finalAssignment
    | ProcessOwner(ownerId, finalAssignment), ProvenanceMutation.ProcessAssignmentAdded(mutationOwnerId, assignment, _) ->
        mutationOwnerId = ownerId
        && assignment.Id = assignmentId
        && assignment = finalAssignment
    | ProcessOwner(ownerId, finalAssignment),
      ProvenanceMutation.ProcessAssignmentValueChanged(mutationOwnerId, before, after, _)
    | ProcessOwner(ownerId, finalAssignment),
      ProvenanceMutation.ProcessAssignmentCoverageChanged(mutationOwnerId, before, after, _) ->
        mutationOwnerId = ownerId && before.Id = assignmentId && after = finalAssignment
    | ProcessOwner(ownerId, finalAssignment),
      ProvenanceMutation.ProcessAssignmentSplit(mutationOwnerId, _, retained, split, _) ->
        mutationOwnerId = ownerId
        && ((retained.Id = assignmentId && retained = finalAssignment)
            || (split.Id = assignmentId && split = finalAssignment))
    | ProcessOwner(ownerId, finalAssignment),
      ProvenanceMutation.AdapterResourceReferenceReplaced(mutationOwnerId, _, after, _, added, _) ->
        mutationOwnerId = ownerId
        && ((after.Id = assignmentId && after = finalAssignment)
            || (added
                |> List.exists (fun dependent -> dependent.Id = assignmentId && dependent = finalAssignment)))
    | _ -> false

let private controlledAssignments (session: ProvenanceSession) (owners: Map<AnnotationAssignmentId, AssignmentOwner>) =
    owners
    |> Map.fold
        (fun state assignmentId owner ->
            let created =
                match owner with
                | NodeOwner(_, assignment) -> assignment.Lineage = AssignmentLineage.Created
                | ProcessOwner(_, assignment) -> assignment.Lineage = AssignmentLineage.Created

            if
                created
                || session.MutationJournal
                   |> List.exists (transitionControlsAssignment assignmentId owner)
            then
                state |> Set.add assignmentId
            else
                state
        )
        Set.empty

let private setAnnotationValue (value: ProvenanceValue) (unitValue: ProvenanceTerm option) (annotation: Annotation) =
    match value with
    | ProvenanceValue.Text text ->
        annotation.Value <- Some text
        annotation.ValueTAN <- None
    | ProvenanceValue.Integer integer ->
        annotation.Value <- Some(integer.ToString(CultureInfo.InvariantCulture))
        annotation.ValueTAN <- None
    | ProvenanceValue.Float floating ->
        annotation.Value <- Some(floating.ToString("R", CultureInfo.InvariantCulture))
        annotation.ValueTAN <- None
    | ProvenanceValue.Term term ->
        annotation.Value <- Some term.Name
        annotation.ValueTAN <- term.TermAccession
    | ProvenanceValue.Reference _ ->
        invalidArg (nameof value) "Reference values do not materialize as ordinary ProcessCore annotations."

    match unitValue with
    | Some unitTerm ->
        annotation.Unit <- Some unitTerm.Name
        annotation.UnitTAN <- unitTerm.TermAccession
    | None ->
        annotation.Unit <- None
        annotation.UnitTAN <- None

let private additionalTypeForNode mappings propertyKind =
    match propertyKind with
    | AssignmentPropertyKind.Generic -> Ok(Some mappings.Node.AdditionalType)
    | AssignmentPropertyKind.AdapterSpecific kind when kind.Id = ProcessCoreCanonicalKinds.characteristic.Id ->
        Ok(Some "CharacteristicValue")
    | AssignmentPropertyKind.AdapterSpecific kind when kind.Id = ProcessCoreCanonicalKinds.factor.Id ->
        Ok(Some "FactorValue")
    | AssignmentPropertyKind.AdapterSpecific kind when kind.Id = ProcessCoreCanonicalKinds.parameter.Id ->
        Ok(Some "ParameterValue")
    | AssignmentPropertyKind.AdapterSpecific kind when kind.Id = ProcessCoreCanonicalKinds.componentKind.Id ->
        Ok(Some "Component")
    | AssignmentPropertyKind.AdapterSpecific kind when kind.Id = ProcessCoreCanonicalKinds.additionalProperty.Id ->
        Ok None
    | AssignmentPropertyKind.AdapterSpecific kind ->
        Error(ProcessCoreCanonicalWritebackError.UnsupportedPropertyKind kind.Id)

let private additionalTypeForProcess mappings propertyKind =
    match propertyKind with
    | AssignmentPropertyKind.Generic -> Ok(Some mappings.Process.AdditionalType)
    | AssignmentPropertyKind.AdapterSpecific kind when kind.Id = ProcessCoreCanonicalKinds.parameter.Id ->
        Ok(Some "ParameterValue")
    | AssignmentPropertyKind.AdapterSpecific kind ->
        Error(ProcessCoreCanonicalWritebackError.UnsupportedPropertyKind kind.Id)

let private annotationFromDefinition
    (additionalType: string option)
    (property: PropertyDefinition)
    (definition: PropertyValueDefinition)
    =
    let annotation =
        Annotation(property.Category.Name, ?nameTAN = property.Category.TermAccession, ?additionalType = additionalType)

    setAnnotationValue definition.Value definition.Unit annotation
    annotation

let private primaryAnnotationFingerprint (annotation: Annotation) =
    annotation.Name,
    annotation.Value,
    annotation.Unit,
    annotation.NameTAN,
    annotation.ValueTAN,
    annotation.UnitTAN,
    annotation.AdditionalType

let private applyCanonicalAnnotationFields (source: Annotation) (requested: Annotation) =
    source.Name <- requested.Name
    source.Value <- requested.Value
    source.Unit <- requested.Unit
    source.NameTAN <- requested.NameTAN
    source.ValueTAN <- requested.ValueTAN
    source.UnitTAN <- requested.UnitTAN
    source.AdditionalType <- requested.AdditionalType
    source

let private tryAnnotationFromFingerprint
    (errors: ResizeArray<ProcessCoreCanonicalWritebackError>)
    context
    (fingerprint: ProcessCoreCanonicalAnnotationFingerprint)
    =
    try
        if isNull fingerprint.Payload then
            invalidArg (nameof fingerprint) "The annotation fingerprint payload is null."

        Some(ProcessCore.Yaml.Annotation.fromYamlString false fingerprint.Payload)
    with _ ->
        addError errors (error $"Indexed annotation fingerprint for '{context}' contains an invalid payload.")

        None

let private plannedAnnotation
    (errors: ResizeArray<ProcessCoreCanonicalWritebackError>)
    mappings
    ownerKind
    controlled
    (targetSource: ProvenanceSourceRef option)
    assignmentId
    valueId
    propertyKind
    lineages
    (session: ProvenanceSession)
    (index: ProcessCoreCanonicalIndex)
    =
    match session.Values |> Map.tryFind valueId with
    | None ->
        addError errors (ProcessCoreCanonicalWritebackError.ValueNotFound valueId)
        None
    | Some definition ->
        match session.Properties |> Map.tryFind definition.PropertyId with
        | None ->
            addError errors (ProcessCoreCanonicalWritebackError.ValueNotFound valueId)
            None
        | Some property ->
            match definition.Value with
            | ProvenanceValue.Reference _ ->
                addError errors (ProcessCoreCanonicalWritebackError.UnsupportedPropertyKind "reference-as-annotation")

                None
            | _ ->
                let additionalType =
                    match ownerKind with
                    | AnnotationOwnerKind.Node -> additionalTypeForNode mappings propertyKind
                    | AnnotationOwnerKind.Process -> additionalTypeForProcess mappings propertyKind

                match additionalType with
                | Error planningError ->
                    addError errors planningError
                    None
                | Ok additionalType ->
                    let requested = annotationFromDefinition additionalType property definition
                    let requestedFingerprint = canonicalAnnotationFingerprint requested

                    let sourceLocations =
                        index.AssignmentLocations |> Map.tryFind assignmentId |> Option.defaultValue []

                    let sourceFingerprint =
                        sourceFingerprintForAssignment errors index lineages assignmentId

                    /// The bound definition may also be edited in place, which keeps the value
                    /// ID stable. That is authorized only by an update whose recorded `before`
                    /// reconstructs to the exact indexed annotation and whose `after` is the
                    /// definition the session actually ended up with.
                    let definitionUpdateIsWitnessed (sourceAnnotation: Annotation) =
                        session.MutationJournal
                        |> List.exists (
                            function
                            | ProvenanceMutation.PropertyValueDefinitionUpdated(before, after, _) ->
                                before.Id = valueId
                                && after.Id = valueId
                                && session.Values |> Map.tryFind valueId = Some after
                                && (
                                    match session.Properties |> Map.tryFind before.PropertyId with
                                    | None -> false
                                    | Some beforeProperty ->
                                        primaryAnnotationFingerprint (
                                            annotationFromDefinition additionalType beforeProperty before
                                        ) = primaryAnnotationFingerprint sourceAnnotation
                                )
                            | _ -> false
                        )

                    let fingerprint =
                        match sourceFingerprint with
                        | None -> requestedFingerprint
                        | Some fingerprint ->
                            match tryAnnotationFromFingerprint errors assignmentId fingerprint with
                            | None -> if controlled then requestedFingerprint else fingerprint
                            | Some sourceAnnotation ->
                                if controlled || definitionUpdateIsWitnessed sourceAnnotation then
                                    applyCanonicalAnnotationFields sourceAnnotation requested
                                    |> canonicalAnnotationFingerprint
                                else
                                    if
                                        primaryAnnotationFingerprint sourceAnnotation
                                        <> primaryAnnotationFingerprint requested
                                    then
                                        addError
                                            errors
                                            (error
                                                $"Assignment '{assignmentId}' diverges from its indexed annotation without a semantic journal mutation.")

                                    fingerprint

                    match tryAnnotationFromFingerprint errors assignmentId fingerprint with
                    | None -> None
                    | Some annotation ->
                        let targetDestination =
                            match targetSource with
                            | None -> None
                            | Some source ->
                                match index.SourceLocations |> Map.tryFind source.Id with
                                | Some destination -> Some destination
                                | None ->
                                    addError
                                        errors
                                        (ProcessCoreCanonicalWritebackError.SourceLocationNotFound source.Id)

                                    None

                        Some {
                            AssignmentId = assignmentId
                            Fingerprint = fingerprint
                            RegistryId = ProcessCore.Yaml.Annotation.genID annotation
                            SourceLocations = sourceLocations
                            ControlledByOperation = controlled
                            TargetSource = targetSource
                            TargetDestination = targetDestination
                        }

let private resolveRecipeResource
    (errors: ResizeArray<ProcessCoreCanonicalWritebackError>)
    (index: ProcessCoreCanonicalIndex)
    (reference: ReferenceValue)
    =
    let candidates =
        index.RecipeResources
        |> Map.toList
        |> List.map snd
        |> List.filter (fun resource ->
            let resourceKey =
                tryStableRecipeResourceKey errors $"reference '{reference.Scheme}:{reference.Id}'" resource.ResourceKey

            resource.Scheme = reference.Scheme && resourceKey = Some reference.Id
        )

    match candidates with
    | [] ->
        let key = RecipeResourceKey.ById reference.Id

        addError errors (ProcessCoreCanonicalWritebackError.RecipeResourceNotFound(reference.Scheme, key))

        None
    | _ :: _ :: _ ->
        addError
            errors
            (ProcessCoreCanonicalWritebackError.AmbiguousRecipeResourceKey(
                reference.Scheme,
                candidates.Head.ResourceKey
            ))

        None
    | [ resource ] ->
        if recipePayloadFingerprint resource.Resource <> resource.LoadFingerprint then
            let stableKey =
                tryStableRecipeResourceKey errors $"stale resource '{resource.Scheme}'" resource.ResourceKey
                |> Option.defaultValue reference.Id

            addError errors (ProcessCoreCanonicalWritebackError.StaleRecipeResource(resource.Scheme, stableKey))

        Some resource

/// A stored resource whose Recipe payload or Component collection is absent cannot be
/// validated or planned against. Planning must report it rather than dereference it.
let private recipeResourceIsMalformed (resource: ProcessCoreRecipeResourceLocation) =
    obj.ReferenceEquals(resource.Resource, null)
    || obj.ReferenceEquals(resource.Resource.Components, null)

let private validateRecipeIndex
    (errors: ResizeArray<ProcessCoreCanonicalWritebackError>)
    (index: ProcessCoreCanonicalIndex)
    =
    for KeyValue((storedScheme, storedKey), resource) in index.RecipeResources do
        let embeddedKey =
            tryStableRecipeResourceKey errors $"index entry '{storedScheme}:{storedKey}'" resource.ResourceKey

        let keyForError = embeddedKey |> Option.defaultValue storedKey

        // A malformed stored resource must fail closed here. Every later planning phase
        // dereferences the Recipe payload directly and would throw instead of returning Error.
        if recipeResourceIsMalformed resource then
            addError errors (ProcessCoreCanonicalWritebackError.StaleRecipeResource(resource.Scheme, keyForError))
        else

            if
                storedScheme <> resource.Scheme
                || (embeddedKey |> Option.exists ((<>) storedKey))
            then
                addError
                    errors
                    (error
                        $"Recipe index key '{storedScheme}:{storedKey}' differs from embedded key '{resource.Scheme}:{keyForError}'.")

            let currentKey =
                tryStableRecipeKeyFromResource errors $"index entry '{storedScheme}:{storedKey}'" resource.Resource

            if
                match embeddedKey, currentKey with
                | Some embedded, Some current -> current <> embedded
                | _ -> true
            then
                addError errors (ProcessCoreCanonicalWritebackError.StaleRecipeResource(resource.Scheme, keyForError))

            if recipePayloadFingerprint resource.Resource <> resource.LoadFingerprint then
                addError errors (ProcessCoreCanonicalWritebackError.StaleRecipeResource(resource.Scheme, keyForError))

            let componentPositions = resource.Components |> List.map _.Position

            if
                componentPositions |> Set.ofList |> Set.count <> componentPositions.Length
                || resource.Components.Length <> resource.Resource.Components.Count
            then
                addError errors (ProcessCoreCanonicalWritebackError.StaleRecipeResource(resource.Scheme, keyForError))

            for componentLocation in resource.Components do
                if
                    componentLocation.Position < 0
                    || componentLocation.Position >= resource.Resource.Components.Count
                then
                    addError
                        errors
                        (ProcessCoreCanonicalWritebackError.StaleRecipeResource(resource.Scheme, keyForError))
                else
                    let expectedKey = $"{keyForError}/component/{componentLocation.Position}"
                    let recipeComponent = resource.Resource.Components[componentLocation.Position]

                    if
                        componentLocation.ComponentKey <> expectedKey
                        || componentLocation.Fingerprint <> canonicalAnnotationFingerprint recipeComponent
                    then
                        addError
                            errors
                            (ProcessCoreCanonicalWritebackError.StaleRecipeResource(resource.Scheme, keyForError))

    index.RecipeResources
    |> Map.toList
    |> List.map snd
    |> List.choose (fun resource ->
        tryStableRecipeResourceKey errors $"duplicate-key validation for '{resource.Scheme}'" resource.ResourceKey
        |> Option.map (fun stableKey -> (resource.Scheme, stableKey), resource)
    )
    |> List.groupBy fst
    |> List.map (fun (key, entries) -> key, entries |> List.map snd)
    |> List.iter (fun ((scheme, _), resources) ->
        if resources.Length > 1 then
            addError
                errors
                (ProcessCoreCanonicalWritebackError.AmbiguousRecipeResourceKey(scheme, resources.Head.ResourceKey))
    )

let private basicComponentMatches
    (session: ProvenanceSession)
    (assignment: ProcessAssignment)
    (componentAnnotation: Annotation)
    =
    match
        session.Values |> Map.tryFind assignment.ValueId,
        session.Values
        |> Map.tryFind assignment.ValueId
        |> Option.bind (fun value -> session.Properties |> Map.tryFind value.PropertyId)
    with
    | Some definition, Some property ->
        let categoryMatches =
            property.Category.Name = componentAnnotation.Name
            && property.Category.TermSource.IsNone
            && property.Category.TermAccession = componentAnnotation.NameTAN

        let valueMatches =
            match definition.Value with
            | ProvenanceValue.Text text -> componentAnnotation.Value = Some text && componentAnnotation.ValueTAN.IsNone
            | ProvenanceValue.Integer integer ->
                componentAnnotation.Value = Some(integer.ToString(CultureInfo.InvariantCulture))
                && componentAnnotation.ValueTAN.IsNone
            | ProvenanceValue.Float floating ->
                componentAnnotation.Value = Some(floating.ToString("R", CultureInfo.InvariantCulture))
                && componentAnnotation.ValueTAN.IsNone
            | ProvenanceValue.Term term ->
                componentAnnotation.Value = Some term.Name
                && componentAnnotation.ValueTAN = term.TermAccession
            | ProvenanceValue.Reference _ -> false

        let unitMatches =
            match definition.Unit with
            | None -> componentAnnotation.Unit.IsNone && componentAnnotation.UnitTAN.IsNone
            | Some unitTerm ->
                componentAnnotation.Unit = Some unitTerm.Name
                && unitTerm.TermSource.IsNone
                && componentAnnotation.UnitTAN = unitTerm.TermAccession

        categoryMatches
        && valueMatches
        && unitMatches
        && isComponentKind assignment.PropertyKind
    | _ -> false

let private validateRecipeComponents
    (errors: ResizeArray<ProcessCoreCanonicalWritebackError>)
    (index: ProcessCoreCanonicalIndex)
    (session: ProvenanceSession)
    (structuralProcess: StructuralProcess)
    (linkId: ProcessLinkId)
    (recipeAssignment: ResolvedRecipeAssignment option)
    =
    let bound =
        structuralProcess.Assignments
        |> Map.toList
        |> List.map snd
        |> List.filter (fun assignment ->
            assignment.ContainerReferenceValueId.IsSome
            && assignment.CoveredLinkIds.Contains linkId
        )

    match recipeAssignment with
    | None when not bound.IsEmpty ->
        addError
            errors
            (ProcessCoreCanonicalWritebackError.ReadOnlyRecipeComponentMutation(
                bound |> List.tryHead |> Option.map _.Id
            ))
    | None -> ()
    | Some recipeAssignment ->
        let expected =
            recipeAssignment.Resource.Components
            |> List.map (fun location -> location.ComponentKey, location)
            |> Map.ofList

        let actual =
            bound
            |> List.choose (fun assignment ->
                if assignment.ContainerReferenceValueId <> Some recipeAssignment.Value.Id then
                    addError
                        errors
                        (ProcessCoreCanonicalWritebackError.ReadOnlyRecipeComponentMutation(Some assignment.Id))

                    None
                else
                    match assignment.Lineage with
                    | AssignmentLineage.DerivedFromCatalog(scheme, resourceId, key) when
                        scheme = recipeAssignment.Reference.Scheme
                        && resourceId = recipeAssignment.Reference.Id
                        ->
                        Some(key, assignment)
                    | _ ->
                        addError
                            errors
                            (ProcessCoreCanonicalWritebackError.ReadOnlyRecipeComponentMutation(Some assignment.Id))

                        None
            )
            |> List.groupBy fst
            |> List.map (fun (key, entries) -> key, entries |> List.map snd)
            |> Map.ofList

        if expected.Count <> actual.Count then
            addError errors (ProcessCoreCanonicalWritebackError.ReadOnlyRecipeComponentMutation None)

        for KeyValue(key, componentLocation) in expected do
            match actual |> Map.tryFind key with
            | Some [ assignment ] ->
                let componentIsJournalCreated =
                    session.MutationJournal
                    |> List.exists (
                        function
                        | ProvenanceMutation.ProcessAssignmentAdded(ownerId, added, _) ->
                            ownerId = structuralProcess.Id && added.Id = assignment.Id
                        | _ -> false
                    )

                let indexedLocations =
                    index.AssignmentLocations |> Map.tryFind assignment.Id |> Option.defaultValue []

                let exactIndexedOccurrence =
                    match indexedLocations with
                    | [] -> componentIsJournalCreated
                    | [ location ] ->
                        location.Position = componentLocation.Position
                        && location.Fingerprint = componentLocation.Fingerprint
                        && (
                            match location.Owner with
                            | ProcessCoreCanonicalAnnotationOwner.RecipeComponent(scheme, resourceId) ->
                                scheme = recipeAssignment.Reference.Scheme
                                && resourceId = recipeAssignment.Reference.Id
                            | _ -> false
                        )
                    | _ -> false

                let validPosition =
                    componentLocation.Position >= 0
                    && componentLocation.Position < recipeAssignment.Resource.Resource.Components.Count

                if not validPosition then
                    addError
                        errors
                        (ProcessCoreCanonicalWritebackError.ReadOnlyRecipeComponentMutation(Some assignment.Id))
                else
                    let componentAnnotation =
                        recipeAssignment.Resource.Resource.Components[componentLocation.Position]

                    if
                        not exactIndexedOccurrence
                        || canonicalAnnotationFingerprint componentAnnotation
                           <> componentLocation.Fingerprint
                        || not (basicComponentMatches session assignment componentAnnotation)
                    then
                        addError
                            errors
                            (ProcessCoreCanonicalWritebackError.ReadOnlyRecipeComponentMutation(Some assignment.Id))
            | Some assignments ->
                addError
                    errors
                    (ProcessCoreCanonicalWritebackError.ReadOnlyRecipeComponentMutation(
                        assignments |> List.tryHead |> Option.map _.Id
                    ))
            | None -> addError errors (ProcessCoreCanonicalWritebackError.ReadOnlyRecipeComponentMutation None)

let private destinationForProcess
    (errors: ResizeArray<ProcessCoreCanonicalWritebackError>)
    (index: ProcessCoreCanonicalIndex)
    (session: ProvenanceSession)
    (structuralProcess: StructuralProcess)
    =
    match session.Layers |> Map.tryFind structuralProcess.OriginLayerId with
    | None ->
        addError errors (ProcessCoreCanonicalWritebackError.LayerNotFound structuralProcess.OriginLayerId)
        None
    | Some layer ->
        match index.SourceLocations |> Map.tryFind layer.Source.Id with
        | Some location -> Some location
        | None ->
            addError errors (ProcessCoreCanonicalWritebackError.SourceLocationNotFound layer.Source.Id)

            None

let private currentRecipeForProcess
    (errors: ResizeArray<ProcessCoreCanonicalWritebackError>)
    (index: ProcessCoreCanonicalIndex)
    processLocation
    =
    let resources =
        index.RecipeResources
        |> Map.toList
        |> List.map snd
        |> List.filter (fun resource -> resource.ReferencingProcesses |> List.contains processLocation)

    match resources with
    | [] -> None
    | [ resource ] -> Some resource
    | _ ->
        addError
            errors
            (error
                $"Indexed Process '{processLocation.DatasetPath}/{processLocation.ProcessIndex}' references more than one Recipe.")

        None

let private chooseReuseLink (index: ProcessCoreCanonicalIndex) (structuralProcess: StructuralProcess) sourceProcess =
    let original =
        index.LinkLocations
        |> Map.toList
        |> List.tryFind (fun (_, location) -> location.Process = sourceProcess)

    let links = structuralProcess.Links |> Map.toList |> List.map snd

    match original with
    | None -> links |> List.tryHead |> Option.map _.Id
    | Some(originalLinkId, location) ->
        let originalInput = location.Input |> Option.bind (sourceNodeId index)
        let originalOutput = location.Output |> Option.bind (sourceNodeId index)

        let byShape predicate =
            links
            |> List.filter predicate
            |> List.sortBy _.Id
            |> List.tryHead
            |> Option.map _.Id

        match originalInput, originalOutput with
        | Some _, Some outputId ->
            // Disconnection is the one asymmetric split: output continuity,
            // not link/map/creation order, owns the indexed Process.
            byShape (fun link ->
                match link.Shape with
                | ProcessLinkShape.OutputOnly candidate -> candidate = outputId
                | _ -> false
            )
            |> Option.orElseWith (fun () ->
                links |> List.tryFind (fun link -> link.Id = originalLinkId) |> Option.map _.Id
            )
            |> Option.orElseWith (fun () ->
                byShape (fun link ->
                    match link.Shape with
                    | ProcessLinkShape.Between(inputId, candidateOutput) ->
                        originalInput = Some inputId && candidateOutput = outputId
                    | _ -> false
                )
            )
        | Some inputId, None ->
            links
            |> List.tryFind (fun link -> link.Id = originalLinkId)
            |> Option.map _.Id
            |> Option.orElseWith (fun () ->
                byShape (fun link ->
                    match link.Shape with
                    | ProcessLinkShape.Between(candidateInput, _) -> candidateInput = inputId
                    | ProcessLinkShape.InputOnly candidateInput -> candidateInput = inputId
                    | _ -> false
                )
            )
        | None, Some outputId ->
            links
            |> List.tryFind (fun link -> link.Id = originalLinkId)
            |> Option.map _.Id
            |> Option.orElseWith (fun () ->
                byShape (fun link ->
                    match link.Shape with
                    | ProcessLinkShape.Between(_, candidateOutput) -> candidateOutput = outputId
                    | ProcessLinkShape.OutputOnly candidateOutput -> candidateOutput = outputId
                    | _ -> false
                )
            )
        | None, None ->
            links
            |> List.tryFind (fun link -> link.Id = originalLinkId)
            |> Option.map _.Id
            |> Option.orElseWith (fun () -> byShape (fun link -> link.Shape = ProcessLinkShape.Endpointless))

let private recipeForLink (state: ProcessPlanningState) linkId =
    state.RecipeAssignments
    |> List.filter (fun recipe -> recipe.Assignment.CoveredLinkIds.Contains linkId)
    |> function
        | [] -> None
        | [ recipe ] -> Some recipe.Resource
        // The validation pass already records InvalidProcessLink. Continue
        // deterministically so tryCreate can return the complete error list.
        | recipe :: _ -> Some recipe.Resource

let private recipeChange previousResource finalResource =
    match previousResource, finalResource with
    | None, None -> None
    | Some previous, Some final when previous.Scheme = final.Scheme && previous.ResourceKey = final.ResourceKey ->
        Some RecipeAssociationChange.Keep
    | None, Some _ -> Some RecipeAssociationChange.Set
    | Some _, None -> Some RecipeAssociationChange.Clear
    | Some _, Some _ -> Some RecipeAssociationChange.Replace

let private validateRecipeAssociationJournal
    (errors: ResizeArray<ProcessCoreCanonicalWritebackError>)
    (index: ProcessCoreCanonicalIndex)
    (session: ProvenanceSession)
    (associations: PlannedRecipeAssociation list)
    =
    let recipeDefinitionMatchesResource
        (resource: ProcessCoreRecipeResourceLocation)
        (definition: PropertyValueDefinition)
        =
        let stableKey =
            tryStableRecipeResourceKey errors $"Recipe association '{resource.Scheme}'" resource.ResourceKey

        match definition.Value, stableKey with
        | ProvenanceValue.Reference reference, Some expectedId ->
            reference.Scheme = resource.Scheme && reference.Id = expectedId
        | _ -> false

    let expectedCreatedRecipeValueId (resource: ProcessCoreRecipeResourceLocation) =
        tryStableRecipeResourceKey errors $"created Recipe association '{resource.Scheme}'" resource.ResourceKey
        |> Option.map (fun resourceId ->
            let reference: ReferenceValue = {
                Scheme = resource.Scheme
                Id = resourceId
                Label = ""
            }

            let category: ProvenanceTerm = {
                Name = "Recipe"
                TermSource = None
                TermAccession = None
            }

            let isolated = {
                session with
                    Properties = Map.empty
                    Values = Map.empty
            }

            Swate.Components.Page.ProvenanceGrouping.Model.ensureValueDefinition
                category
                (ProvenanceValue.Reference reference)
                None
                isolated
            |> _.ValueDefinition.Id
        )

    let createdRecipeValueIdMatchesResource (resource: ProcessCoreRecipeResourceLocation) valueId =
        let finalDefinitionMatches =
            session.Values
            |> Map.tryFind valueId
            |> Option.exists (recipeDefinitionMatchesResource resource)

        let identityMatches = expectedCreatedRecipeValueId resource = Some valueId

        finalDefinitionMatches && identityMatches

    let finalRecipeAssignment ownerId linkId =
        session.Processes
        |> Map.tryFind ownerId
        |> Option.bind (fun structuralProcess ->
            structuralProcess.Assignments
            |> Map.toList
            |> List.map snd
            |> List.filter (fun assignment ->
                isRecipeKind assignment.PropertyKind
                && isRecipeValueId session assignment.ValueId
                && assignment.CoveredLinkIds.Contains linkId
            )
            |> function
                | [ assignment ] -> Some assignment
                | _ -> None
        )

    let finalAssignmentById ownerId assignmentId =
        session.Processes
        |> Map.tryFind ownerId
        |> Option.bind (fun structuralProcess -> structuralProcess.Assignments |> Map.tryFind assignmentId)

    let contextCovers assignmentId linkId context =
        context.Coverage.AssignmentIds.Contains assignmentId
        && context.Coverage.LinkIds.Contains linkId

    let exactLoadedRecipeIdentity
        ownerId
        (resource: ProcessCoreRecipeResourceLocation)
        (assignment: ProcessAssignment)
        =
        match index.ProcessLocations |> Map.tryFind ownerId with
        | Some processLocation ->
            resource.ReferencingProcesses |> List.contains processLocation
            && assignment.Id = $"{ownerId}::recipe"
            && assignment.Lineage = AssignmentLineage.Loaded
            && isRecipeKind assignment.PropertyKind
            && assignment.ReferenceSlotId = Some ProcessCoreCanonicalKinds.processCoreExecutesRecipeSlot
            && assignment.ContainerReferenceValueId.IsNone
            && Map.tryFind assignment.Id index.AssignmentValueIds = Some assignment.ValueId
        | _ -> false

    let exactLoadedRecipeAssignment
        ownerId
        (resource: ProcessCoreRecipeResourceLocation)
        (assignment: ProcessAssignment)
        =
        exactLoadedRecipeIdentity ownerId resource assignment
        && (indexedProcessLinkIds index ownerId
            |> Option.exists ((=) assignment.CoveredLinkIds))

    let recipeAdded ownerId linkId resource =
        let finalAssignment = finalRecipeAssignment ownerId linkId

        session.MutationJournal
        |> List.exists (
            function
            | ProvenanceMutation.ProcessAssignmentAdded(mutationOwnerId, assignment, context) ->
                mutationOwnerId = ownerId
                && Some assignment = finalAssignment
                && isRecipeKind assignment.PropertyKind
                && assignment.Lineage = AssignmentLineage.Created
                && createdRecipeValueIdMatchesResource resource assignment.ValueId
                && assignment.CoveredLinkIds.Contains linkId
                && contextCovers assignment.Id linkId context
            | ProvenanceMutation.ProcessAssignmentCoverageChanged(mutationOwnerId, before, after, context) ->
                mutationOwnerId = ownerId
                && Some after = finalAssignment
                && isRecipeKind after.PropertyKind
                && createdRecipeValueIdMatchesResource resource after.ValueId
                && not (before.CoveredLinkIds.Contains linkId)
                && after.CoveredLinkIds.Contains linkId
                && contextCovers after.Id linkId context
            | _ -> false
        )

    let recipeDetached ownerId linkId previousResource =
        session.MutationJournal
        |> List.exists (
            function
            | ProvenanceMutation.ProcessAssignmentRemoved(tombstone, context) ->
                tombstone.OwnerId = ownerId
                && exactLoadedRecipeAssignment ownerId previousResource tombstone.Assignment
                && tombstone.Assignment.CoveredLinkIds.Contains linkId
                && contextCovers tombstone.Assignment.Id linkId context
            | ProvenanceMutation.ProcessAssignmentCoverageChanged(mutationOwnerId, before, after, context) ->
                mutationOwnerId = ownerId
                && exactLoadedRecipeAssignment ownerId previousResource before
                && before.CoveredLinkIds.Contains linkId
                && not (after.CoveredLinkIds.Contains linkId)
                && finalAssignmentById ownerId after.Id = Some after
                && contextCovers before.Id linkId context
            | _ -> false
        )

    let recipeReplaced ownerId linkId previousResource finalResource =
        let finalAssignment = finalRecipeAssignment ownerId linkId

        session.MutationJournal
        |> List.exists (
            function
            | ProvenanceMutation.AdapterResourceReferenceReplaced(mutationOwnerId, before, after, _, _, context) ->
                mutationOwnerId = ownerId
                && exactLoadedRecipeAssignment ownerId previousResource before
                && after.Lineage = AssignmentLineage.Created
                && createdRecipeValueIdMatchesResource finalResource after.ValueId
                && Some after = finalAssignment
                && before.CoveredLinkIds.Contains linkId
                && after.CoveredLinkIds.Contains linkId
                && contextCovers after.Id linkId context
            | _ -> false
        )

    for association in associations do
        if index.LinkLocations.ContainsKey association.LinkId then
            let justified =
                match association.Change, association.PreviousResource, association.FinalResource with
                | RecipeAssociationChange.Keep, Some previousResource, Some finalResource when
                    previousResource.Scheme = finalResource.Scheme
                    && previousResource.ResourceKey = finalResource.ResourceKey
                    ->
                    finalRecipeAssignment association.StructuralProcessId association.LinkId
                    |> Option.exists (fun assignment ->
                        exactLoadedRecipeIdentity association.StructuralProcessId previousResource assignment
                        || (assignment.Lineage = AssignmentLineage.Created
                            && createdRecipeValueIdMatchesResource finalResource assignment.ValueId)
                    )
                | RecipeAssociationChange.Set, None, Some finalResource ->
                    recipeAdded association.StructuralProcessId association.LinkId finalResource
                | RecipeAssociationChange.Clear, Some previousResource, None ->
                    recipeDetached association.StructuralProcessId association.LinkId previousResource
                | RecipeAssociationChange.Replace, Some previousResource, Some finalResource ->
                    recipeReplaced association.StructuralProcessId association.LinkId previousResource finalResource
                | _ -> false

            if not justified then
                addError
                    errors
                    (error
                        $"Recipe association change '{association.Change}' for loaded link '{association.LinkId}' has no matching semantic mutation.")

let private remintAnnotations
    (errors: ResizeArray<ProcessCoreCanonicalWritebackError>)
    (index: ProcessCoreCanonicalIndex)
    (nodes: PlannedNode list)
    (partitions: PlannedProcessPartition list)
    =
    let plannedAnnotations: PlannedAnnotation list = [
        yield! nodes |> List.collect _.Annotations

        yield! partitions |> List.collect _.Assignments |> List.choose _.Annotation
    ]

    let storedResourceAnnotations: PlannedAnnotation list =
        index.RecipeResources
        |> Map.toList
        |> List.collect (fun ((scheme, resourceKey), resource) ->
            [
                yield!
                    resource.Resource.Components
                    |> Seq.mapi (fun position annotation -> "component", position, annotation)

                yield!
                    resource.Resource.AdditionalProperty
                    |> Seq.mapi (fun position annotation -> "additional-property", position, annotation)
            ]
            |> Seq.map (fun (kind, position, annotation) -> {
                AssignmentId = $"read-only-recipe-{kind}:{scheme}:{resourceKey}:{position}"
                Fingerprint = canonicalAnnotationFingerprint annotation
                RegistryId = ProcessCore.Yaml.Annotation.genID annotation
                SourceLocations = []
                ControlledByOperation = false
                TargetSource = None
                TargetDestination = None
            })
            |> Seq.toList
        )

    let explicitDefinedTermId (term: DefinedTerm) =
        match term.TryGetPropertyValue("@id") with
        | Some(:? string as id) -> Some id
        | _ -> None

    let explicitFormalParameterIds (parameter: FormalParameter) = [
        match parameter.TryGetPropertyValue("@id") with
        | Some(:? string as id) -> yield id
        | _ -> ()

        yield! parameter.DefaultValue |> Option.bind explicitDefinedTermId |> Option.toList
    ]

    let nestedAnnotationIds (annotation: Annotation) =
        annotation.InstanceOf
        |> Option.toList
        |> List.collect explicitFormalParameterIds

    let reservedRecipeOwnedRegistryIds =
        index.RecipeResources
        |> Map.toList
        |> List.collect (fun (_, resource) -> [
            yield!
                resource.Resource.IntendedUse
                |> Option.bind explicitDefinedTermId
                |> Option.toList

            yield! resource.Resource.Parameters |> Seq.collect explicitFormalParameterIds

            yield!
                Seq.append resource.Resource.Components resource.Resource.AdditionalProperty
                |> Seq.collect nestedAnnotationIds
        ])
        |> Set.ofList

    let annotations =
        plannedAnnotations @ storedResourceAnnotations
        |> List.distinctBy (fun annotation -> annotation.AssignmentId, annotation.Fingerprint)

    let mutable usedRegistryIds =
        Set.union (annotations |> List.map _.RegistryId |> Set.ofList) reservedRecipeOwnedRegistryIds

    let mutable remintByAssignment: Map<AnnotationAssignmentId, PlannedAnnotationReminting> =
        Map.empty

    for registryId, collisionGroup in annotations |> List.groupBy _.RegistryId do
        let divergent =
            collisionGroup
            |> List.groupBy _.Fingerprint
            |> List.map (fun (_, entries) -> entries |> List.sortBy _.AssignmentId)

        if divergent.Length > 1 then
            let uncontrolled =
                divergent
                |> List.collect id
                |> List.filter (fun annotation -> not annotation.ControlledByOperation)

            let uncontrolledFingerprints = uncontrolled |> List.map _.Fingerprint |> Set.ofList

            if uncontrolledFingerprints.Count > 1 then
                let first = uncontrolled.Head

                let conflicting =
                    uncontrolled |> List.find (fun item -> item.Fingerprint <> first.Fingerprint)

                addError
                    errors
                    (ProcessCoreCanonicalWritebackError.ConflictingAnnotationIdentity(
                        registryId,
                        first.Fingerprint.Payload,
                        conflicting.Fingerprint.Payload
                    ))
            else
                let survivorFingerprint =
                    uncontrolled
                    |> List.tryHead
                    |> Option.map _.Fingerprint
                    |> Option.defaultWith (fun () ->
                        divergent
                        |> List.collect id
                        |> List.sortBy _.AssignmentId
                        |> List.head
                        |> _.Fingerprint
                    )

                let remintCandidates: PlannedAnnotation list =
                    divergent
                    |> List.collect id
                    |> List.filter (fun annotation ->
                        annotation.ControlledByOperation
                        && annotation.Fingerprint <> survivorFingerprint
                    )
                    |> List.groupBy _.AssignmentId
                    |> List.map (snd >> List.head)
                    |> List.sortBy _.AssignmentId

                for candidate in remintCandidates do
                    let encodedAssignment =
                        candidate.AssignmentId
                        |> Seq.map (fun character -> ((int character).ToString("X4", CultureInfo.InvariantCulture)))
                        |> String.concat ""

                    let baseId = $"{registryId}__arc_{encodedAssignment}"

                    let plannedId =
                        Seq.initInfinite (fun index -> if index = 0 then baseId else $"{baseId}_{index + 1}")
                        |> Seq.find (fun candidateId -> not (usedRegistryIds.Contains candidateId))

                    usedRegistryIds <- usedRegistryIds |> Set.add plannedId

                    match tryAnnotationFromFingerprint errors candidate.AssignmentId candidate.Fingerprint with
                    | None -> ()
                    | Some annotation ->
                        annotation.SetProperty("@id", plannedId)
                        let plannedFingerprint = canonicalAnnotationFingerprint annotation

                        remintByAssignment <-
                            remintByAssignment
                            |> Map.add candidate.AssignmentId {
                                AssignmentId = candidate.AssignmentId
                                OriginalRegistryId = candidate.RegistryId
                                PlannedRegistryId = plannedId
                                OriginalFingerprint = candidate.Fingerprint
                                PlannedFingerprint = plannedFingerprint
                            }

    let applyRemint (annotation: PlannedAnnotation) : PlannedAnnotation =
        match remintByAssignment |> Map.tryFind annotation.AssignmentId with
        | None -> annotation
        | Some reminting -> {
            annotation with
                Fingerprint = reminting.PlannedFingerprint
                RegistryId = reminting.PlannedRegistryId
          }

    let remintedNodes =
        nodes
        |> List.map (fun node -> {
            node with
                Annotations = node.Annotations |> List.map applyRemint
        })

    let remintedPartitions =
        partitions
        |> List.map (fun partition ->
            let assignments =
                partition.Assignments
                |> List.map (fun assignment ->
                    match assignment.Annotation with
                    | None -> assignment
                    | Some annotation ->
                        let reminted = applyRemint annotation

                        {
                            assignment with
                                Annotation = Some reminted
                                Fingerprint =
                                    ProcessCoreProcessAssignmentFingerprint.AnnotationFingerprint reminted.Fingerprint
                        }
                )

            {
                partition with
                    Assignments = assignments
                    Signature =
                        assignments
                        |> List.map (fun assignment -> assignment.AssignmentId, assignment.Fingerprint)
                        |> Set.ofList
            }
        )

    remintedNodes, remintedPartitions, (remintByAssignment |> Map.toList |> List.map snd |> List.sortBy _.AssignmentId)

let private stableShape =
    function
    | ProcessLinkShape.Between(inputId, outputId) -> $"B:{inputId}:{outputId}"
    | ProcessLinkShape.InputOnly inputId -> $"I:{inputId}"
    | ProcessLinkShape.OutputOnly outputId -> $"O:{outputId}"
    | ProcessLinkShape.Endpointless -> "E"

/// Builds a complete, non-mutating ProcessCore writeback plan from the final
/// canonical state, its semantic journal, and the single canonical index.
let private tryCreatePlan
    (index: ProcessCoreCanonicalIndex)
    (session: ProvenanceSession)
    : Result<ProcessCoreWritebackPlan, ProcessCoreCanonicalWritebackError list> =
    let errors = ResizeArray<ProcessCoreCanonicalWritebackError>()
    validateCanonicalState errors session
    let owners = assignmentsById errors session
    validateResourceJournal errors session owners
    validateRecipeIndex errors index
    validateJournalJustification errors index session owners
    let lineages = assignmentLineages session owners
    let controlled = controlledAssignments session owners

    let mutable nodes: PlannedNode list = []

    for KeyValue(nodeId, node) in session.Nodes do
        let existingLocations =
            index.NodeLocations |> Map.tryFind nodeId |> Option.defaultValue []

        match nodeFromCanonicalNode node with
        | Error planningError -> addError errors planningError
        | Ok materialized ->
            if existingLocations.IsEmpty then
                let creationWitnesses =
                    session.MutationJournal
                    |> List.choose (
                        function
                        | ProvenanceMutation.CanonicalNodeCreated created when created.Id = nodeId -> Some created
                        | _ -> None
                    )

                match creationWitnesses with
                | [ created ] when
                    created.Id = node.Id
                    && created.Key = node.Key
                    && created.Kind = node.Kind
                    && created.Name = node.Name
                    && created.Assignments.IsEmpty
                    ->
                    ()
                | _ ->
                    addError
                        errors
                        (error
                            $"New canonical node '{nodeId}' has no unique CanonicalNodeCreated witness for its exact identity.")
            else
                let expectedLocation = nodeLocation materialized

                if
                    existingLocations
                    |> List.exists (fun location -> location.Node <> expectedLocation)
                then
                    addError
                        errors
                        (error
                            $"Indexed canonical node '{nodeId}' differs in kind or key from its ProcessCore source location.")

        let annotations =
            node.Assignments
            |> Map.toList
            |> List.choose (fun (_, assignment) ->
                plannedAnnotation
                    errors
                    index.GenericPropertyMappings
                    AnnotationOwnerKind.Node
                    (controlled.Contains assignment.Id)
                    assignment.TargetSource
                    assignment.Id
                    assignment.ValueId
                    assignment.PropertyKind
                    lineages
                    session
                    index
            )

        nodes <-
            {
                NodeId = nodeId
                Key = node.Key
                Kind = node.Kind
                ExistingLocations = existingLocations
                IsNew = existingLocations.IsEmpty
                Annotations = annotations
            }
            :: nodes

    nodes <- nodes |> List.rev

    let mutable processStates: ProcessPlanningState list = []

    for KeyValue(_, structuralProcess) in session.Processes do
        match destinationForProcess errors index session structuralProcess with
        | None -> ()
        | Some destination ->
            let ordinaryAssignments =
                structuralProcess.Assignments
                |> Map.toList
                |> List.choose (fun (_, assignment) ->
                    match session.Values |> Map.tryFind assignment.ValueId with
                    | None -> None
                    | Some definition when
                        isRecipeReferenceValue definition.Value
                        || assignment.ContainerReferenceValueId.IsSome
                        || isRecipeKind assignment.PropertyKind
                        || isComponentKind assignment.PropertyKind
                        ->
                        None
                    | Some _ ->
                        plannedAnnotation
                            errors
                            index.GenericPropertyMappings
                            AnnotationOwnerKind.Process
                            (controlled.Contains assignment.Id)
                            None
                            assignment.Id
                            assignment.ValueId
                            assignment.PropertyKind
                            lineages
                            session
                            index
                        |> Option.map (fun annotation -> {
                            AssignmentId = assignment.Id
                            Fingerprint =
                                ProcessCoreProcessAssignmentFingerprint.AnnotationFingerprint annotation.Fingerprint
                            Annotation = Some annotation
                        })
                )

            let recipeAssignments =
                structuralProcess.Assignments
                |> Map.toList
                |> List.choose (fun (_, assignment) ->
                    match session.Values |> Map.tryFind assignment.ValueId with
                    | Some({
                               Value = ProvenanceValue.Reference reference
                           } as definition) when reference.Scheme = ProcessCoreCanonicalKinds.processCoreRecipeScheme ->
                        if
                            not (isRecipeKind assignment.PropertyKind)
                            || assignment.ReferenceSlotId
                               <> Some ProcessCoreCanonicalKinds.processCoreExecutesRecipeSlot
                            || assignment.ContainerReferenceValueId.IsSome
                        then
                            addError
                                errors
                                (error
                                    $"Recipe assignment '{assignment.Id}' has invalid kind, slot, or container metadata.")

                        resolveRecipeResource errors index reference
                        |> Option.map (fun resource -> {
                            Assignment = assignment
                            Value = definition
                            Reference = reference
                            Resource = resource
                        })
                    | Some {
                               Value = ProvenanceValue.Reference reference
                           } ->
                        addError
                            errors
                            (ProcessCoreCanonicalWritebackError.UnsupportedPropertyKind $"reference:{reference.Scheme}")

                        None
                    | Some _ when isRecipeKind assignment.PropertyKind ->
                        addError
                            errors
                            (error $"Recipe assignment '{assignment.Id}' does not contain a reference value.")

                        None
                    | _ -> None
                )

            for KeyValue(linkId, _) in structuralProcess.Links do
                let recipesForLink =
                    recipeAssignments
                    |> List.filter (fun recipe -> recipe.Assignment.CoveredLinkIds.Contains linkId)

                if recipesForLink.Length > 1 then
                    addError errors (ProcessCoreCanonicalWritebackError.InvalidProcessLink linkId)

                validateRecipeComponents errors index session structuralProcess linkId (recipesForLink |> List.tryHead)

            let assignmentForSignature linkId =
                [
                    yield!
                        ordinaryAssignments
                        |> List.choose (fun planned ->
                            let assignment = structuralProcess.Assignments[planned.AssignmentId]

                            if assignment.CoveredLinkIds.Contains linkId then
                                Some(planned.AssignmentId, planned.Fingerprint)
                            else
                                None
                        )

                    yield!
                        recipeAssignments
                        |> List.choose (fun recipe ->
                            if recipe.Assignment.CoveredLinkIds.Contains linkId then
                                Some(
                                    recipe.Assignment.Id,
                                    ProcessCoreProcessAssignmentFingerprint.RecipeReferenceFingerprint(
                                        recipe.Reference.Scheme,
                                        recipe.Resource.ResourceKey
                                    )
                                )
                            else
                                None
                        )
                ]
                |> Set.ofList

            let partitionGroups =
                structuralProcess.Links
                |> Map.toList
                |> List.map (fun (linkId, _) -> assignmentForSignature linkId, linkId)
                |> List.groupBy fst
                |> List.map (fun (signature, entries) -> signature, entries |> List.map snd |> Set.ofList)
                |> List.sortBy (snd >> Set.minElement)

            let partitions =
                partitionGroups
                |> List.mapi (fun ordinal (signature, links) ->
                    let assignments =
                        signature
                        |> Set.toList
                        |> List.map (fun (assignmentId, fingerprint) ->
                            match fingerprint with
                            | ProcessCoreProcessAssignmentFingerprint.AnnotationFingerprint _ ->
                                ordinaryAssignments
                                |> List.find (fun assignment -> assignment.AssignmentId = assignmentId)
                            | ProcessCoreProcessAssignmentFingerprint.RecipeReferenceFingerprint _ -> {
                                AssignmentId = assignmentId
                                Fingerprint = fingerprint
                                Annotation = None
                              }
                        )

                    {
                        Id = $"{structuralProcess.Id}::partition:{ordinal + 1}"
                        StructuralProcessId = structuralProcess.Id
                        Signature = signature
                        Links = links
                        Assignments = assignments
                    }
                )

            processStates <-
                {
                    StructuralProcess = structuralProcess
                    Destination = destination
                    IndexedProcess = index.ProcessLocations |> Map.tryFind structuralProcess.Id
                    OrdinaryAssignments = ordinaryAssignments
                    RecipeAssignments = recipeAssignments
                    Partitions = partitions
                }
                :: processStates

    processStates <- processStates |> List.rev
    let allPartitions = processStates |> List.collect _.Partitions

    let remintedNodes, remintedPartitions, remintings =
        remintAnnotations errors index nodes allPartitions

    let remintedPartitionById =
        remintedPartitions |> List.map (fun item -> item.Id, item) |> Map.ofList

    processStates <-
        processStates
        |> List.map (fun state -> {
            state with
                Partitions =
                    state.Partitions
                    |> List.map (fun partition -> remintedPartitionById[partition.Id])
        })

    let mutable unsortedProcesses: PlannedProcess list = []
    let mutable recipeAssociations: PlannedRecipeAssociation list = []

    for state in processStates do
        let reuseLink =
            state.IndexedProcess
            |> Option.bind (chooseReuseLink index state.StructuralProcess)

        let previousResource =
            state.IndexedProcess |> Option.bind (currentRecipeForProcess errors index)

        for KeyValue(linkId, link) in state.StructuralProcess.Links do
            let disposition =
                match state.IndexedProcess, reuseLink with
                | Some _, Some reusable when reusable = linkId -> PlannedProcessDisposition.ReuseIndexed
                | Some _, _ -> PlannedProcessDisposition.CloneIndexed
                | None, _ -> PlannedProcessDisposition.NewProcess

            let finalResource = recipeForLink state linkId

            let association =
                recipeChange previousResource finalResource
                |> Option.map (fun change -> {
                    StructuralProcessId = state.StructuralProcess.Id
                    LinkId = linkId
                    IndexedProcess = state.IndexedProcess
                    Change = change
                    PreviousResource = previousResource
                    FinalResource = finalResource
                })

            association
            |> Option.iter (fun planned -> recipeAssociations <- planned :: recipeAssociations)

            let partition =
                state.Partitions |> List.find (fun partition -> partition.Links.Contains linkId)

            unsortedProcesses <-
                {
                    StructuralProcessId = state.StructuralProcess.Id
                    LinkId = linkId
                    Shape = link.Shape
                    PartitionId = partition.Id
                    ProcessName =
                        state.StructuralProcess.Name
                        |> Option.defaultValue state.Destination.ProcessGroupName
                    Destination = state.Destination
                    DestinationOrder = -1
                    Disposition = disposition
                    IndexedProcess = state.IndexedProcess
                    ReusesIndexedProcess = disposition = PlannedProcessDisposition.ReuseIndexed
                    RecipeAssociation = association
                }
                :: unsortedProcesses

    let dispositionRank =
        function
        | PlannedProcessDisposition.ReuseIndexed -> 0
        | PlannedProcessDisposition.CloneIndexed -> 1
        | PlannedProcessDisposition.NewProcess -> 2

    let processes =
        unsortedProcesses
        |> List.groupBy _.Destination
        |> List.sortBy fst
        |> List.collect (fun (_, destinationProcesses) ->
            destinationProcesses
            |> List.sortBy (fun plannedProcess ->
                plannedProcess.IndexedProcess
                |> Option.map _.ProcessIndex
                |> Option.defaultValue Int32.MaxValue,
                dispositionRank plannedProcess.Disposition,
                plannedProcess.StructuralProcessId,
                plannedProcess.PartitionId,
                plannedProcess.LinkId
            )
            |> List.mapi (fun order plannedProcess -> {
                plannedProcess with
                    DestinationOrder = order
            })
        )

    recipeAssociations <- recipeAssociations |> List.rev
    validateRecipeAssociationJournal errors index session recipeAssociations

    let finalAssignmentIds = owners |> Map.keys |> Set.ofSeq

    let annotationRemovals =
        index.AssignmentLocations
        |> Map.toList
        |> List.choose (fun (assignmentId, locations) ->
            if finalAssignmentIds.Contains assignmentId then
                None
            else
                let writable =
                    locations
                    |> List.filter (fun location ->
                        match location.Owner with
                        | ProcessCoreCanonicalAnnotationOwner.RecipeComponent _ -> false
                        | _ -> true
                    )

                if writable.IsEmpty then
                    None
                else
                    Some {
                        AssignmentId = assignmentId
                        Locations = writable
                    }
        )

    let reusedLocations =
        processes
        |> List.choose (fun plannedProcess ->
            if plannedProcess.ReusesIndexedProcess then
                plannedProcess.IndexedProcess
            else
                None
        )
        |> Set.ofList

    let processRemovals =
        index.ProcessLocations
        |> Map.toList
        |> List.choose (fun (structuralProcessId, location) ->
            if reusedLocations.Contains location then
                None
            else
                Some {
                    StructuralProcessId = structuralProcessId
                    Location = location
                }
        )
        |> List.sortBy (fun removal -> removal.StructuralProcessId, removal.Location)

    for removal in processRemovals do
        let justified =
            session.MutationJournal
            |> List.exists (
                function
                | ProvenanceMutation.ProcessLinkRemoved(ownerId, _, _) -> ownerId = removal.StructuralProcessId
                | ProvenanceMutation.StructuralProcessReshaped(before, after) ->
                    before.Id = removal.StructuralProcessId
                    && after.Id = removal.StructuralProcessId
                    && before.Links <> after.Links
                | _ -> false
            )

        if not justified then
            addError
                errors
                (error
                    $"Indexed Process '{removal.StructuralProcessId}' is obsolete without a link-removal or reshape mutation.")

    let addedProcessCount =
        processes
        |> List.filter (fun plannedProcess -> plannedProcess.Disposition <> PlannedProcessDisposition.ReuseIndexed)
        |> List.length

    let addedAnnotations =
        [
            yield!
                remintedNodes
                |> List.collect _.Annotations
                |> List.filter (fun annotation -> annotation.SourceLocations.IsEmpty)

            yield!
                remintedPartitions
                |> List.collect _.Assignments
                |> List.choose _.Annotation
                |> List.filter (fun annotation -> annotation.SourceLocations.IsEmpty)
        ]
        |> List.distinctBy _.AssignmentId
        |> List.length

    let updatedAnnotations =
        [
            yield! remintedNodes |> List.collect _.Annotations
            yield! remintedPartitions |> List.collect _.Assignments |> List.choose _.Annotation
        ]
        |> List.distinctBy _.AssignmentId
        |> List.sumBy (fun annotation ->
            annotation.SourceLocations
            |> List.filter (fun location -> location.Fingerprint <> annotation.Fingerprint)
            |> List.length
        )

    if errors.Count > 0 then
        Error(distinctErrors errors)
    else
        Ok {
            Nodes = remintedNodes
            Partitions = remintedPartitions
            Processes = processes
            ProcessRemovals = processRemovals
            RecipeAssociations = recipeAssociations
            AnnotationRemovals = annotationRemovals
            AnnotationRemintings = remintings
            Summary = {
                UpdatedAnnotations = updatedAnnotations
                AddedAnnotations = addedAnnotations
                AddedNodes = remintedNodes |> List.filter _.IsNew |> List.length
                AddedProcesses = addedProcessCount
                RemovedProcesses = processRemovals.Length
            }
            RecipeResourcesAdded = 0
        }

let tryCreate
    (index: ProcessCoreCanonicalIndex)
    (session: ProvenanceSession)
    : Result<ProcessCoreWritebackPlan, ProcessCoreCanonicalWritebackError list> =
    // Planning dereferences stored Recipe payloads in several later phases, so a malformed
    // resource is rejected up front instead of throwing part-way through.
    if
        index.RecipeResources
        |> Map.exists (fun _ resource -> recipeResourceIsMalformed resource)
    then
        let errors = ResizeArray<ProcessCoreCanonicalWritebackError>()
        validateRecipeIndex errors index
        Error(distinctErrors errors)
    else
        tryCreatePlan index session

module Swate.Components.Page.ProvenanceGrouping.Session

open Swate.Components.Page.ProvenanceGrouping.Commands
open Swate.Components.Page.ProvenanceGrouping.Identifiers
open Swate.Components.Page.ProvenanceGrouping.Values
open Swate.Components.Page.ProvenanceGrouping.Domain
open Swate.Components.Page.ProvenanceGrouping.MutationTypes
open Swate.Components.Page.ProvenanceGrouping.ProjectionTypes
open Swate.Components.Page.ProvenanceGrouping.Model
open Swate.Components.Page.ProvenanceGrouping.Availability
open Swate.Components.Page.ProvenanceGrouping.Projection

let private mutationContext =
    function
    | NodeAssignmentAdded(_, _, context)
    | NodeAssignmentValueChanged(_, _, _, context)
    | NodeAssignmentRemoved(_, context)
    | ProcessLinkRemoved(_, _, context)
    | PropertyDefinitionUpdated(_, _, context)
    | PropertyValueDefinitionUpdated(_, _, context)
    | PropertyValueDefinitionDeleted(_, _, context)
    | PropertyDefinitionDeleted(_, _, _, context)
    | ProcessAssignmentAdded(_, _, context)
    | ProcessAssignmentCoverageChanged(_, _, _, context)
    | ProcessAssignmentValueChanged(_, _, _, context)
    | ProcessAssignmentSplit(_, _, _, _, context)
    | ProcessAssignmentRemoved(_, context)
    | AdapterResourceReferenceReplaced(_, _, _, _, _, context) -> Some context
    | _ -> None

let private assignmentTombstone =
    function
    | NodeAssignmentRemoved(tombstone, _) -> Some(tombstone.Assignment.ValueId, NodeTombstone tombstone)
    | ProcessAssignmentRemoved(tombstone, _) -> Some(tombstone.Assignment.ValueId, ProcessTombstone tombstone)
    | _ -> None

let private appendImplicitGlobalValueDeletions (session: ProvenanceSession) (content: CanonicalContentView) mutations =
    let explicitlyDeletedValueIds =
        mutations
        |> List.collect (
            function
            | PropertyValueDefinitionDeleted(value, _, _) -> [ value.Id ]
            | PropertyDefinitionDeleted(_, values, _, _) -> values |> List.map _.Id
            | _ -> []
        )
        |> Set.ofList

    let globalContexts =
        mutations
        |> List.choose mutationContext
        |> List.filter (fun context -> context.Scope = GlobalDefinition)

    if globalContexts.IsEmpty then
        mutations
    else
        let tombstonesByValueId =
            mutations
            |> List.choose assignmentTombstone
            |> List.groupBy fst
            |> List.map (fun (valueId, entries) -> valueId, entries |> List.map snd)
            |> Map.ofList

        let implicitDeletions =
            session.Values
            |> Map.toList
            |> List.choose (fun (valueId, value) ->
                if
                    content.Values |> Map.containsKey valueId
                    || explicitlyDeletedValueIds |> Set.contains valueId
                then
                    None
                else
                    let tombstones =
                        tombstonesByValueId |> Map.tryFind valueId |> Option.defaultValue []

                    let removedAssignmentIds =
                        tombstones
                        |> List.map (
                            function
                            | NodeTombstone tombstone -> tombstone.Assignment.Id
                            | ProcessTombstone tombstone -> tombstone.Assignment.Id
                        )
                        |> Set.ofList

                    let contexts =
                        globalContexts
                        |> List.filter (fun context ->
                            removedAssignmentIds.IsEmpty
                            || not (Set.intersect removedAssignmentIds context.Coverage.AssignmentIds).IsEmpty
                        )

                    if contexts.IsEmpty then
                        None
                    else
                        let context = {
                            Scope = GlobalDefinition
                            Coverage = {
                                AssignmentIds =
                                    contexts
                                    |> List.collect (fun item -> item.Coverage.AssignmentIds |> Set.toList)
                                    |> Set.ofList
                                LinkIds =
                                    contexts
                                    |> List.collect (fun item -> item.Coverage.LinkIds |> Set.toList)
                                    |> Set.ofList
                            }
                        }

                        Some(PropertyValueDefinitionDeleted(value, tombstones, context))
            )

        mutations @ implicitDeletions

let private cachedReferenceCatalog layerId (session: ProvenanceSession) =
    session.LayerProjections
    |> Map.tryFind layerId
    |> Option.map (fun projection ->
        projection.ShelfEntries
        |> List.choose (fun shelfEntry ->
            match shelfEntry.Payload with
            | CatalogBacked payload ->
                Some((payload.Entry.Reference.Scheme, payload.Entry.Reference.Id), payload.Entry)
            | AssignmentBacked _ -> None
        )
        |> Map.ofList
    )
    |> Option.defaultValue Map.empty

/// Refresh against a host-supplied catalog. The catalog is load-boundary data
/// the host owns, not canonical state, so a layer that has never been projected
/// - one `addLayer` has just created - has no cached shelf entries to recover it
/// from and would otherwise lose the catalog folder entirely. The controlled
/// entries win over recovered ones, because the host's copy is the current one.
let refreshLayerWithCatalog (catalog: ReferenceCatalog) layerId (session: ProvenanceSession) =
    let effectiveCatalog =
        catalog
        |> Map.fold (fun merged key entry -> merged |> Map.add key entry) (cachedReferenceCatalog layerId session)

    projectLayerWithMemo layerId effectiveCatalog session
    |> Result.map (fun (projection, session) -> {
        session with
            LayerProjections = session.LayerProjections |> Map.add layerId projection
    })

let refreshLayer layerId (session: ProvenanceSession) =
    refreshLayerWithCatalog Map.empty layerId session

let activateLayerWithCatalog (catalog: ReferenceCatalog) layerId (session: ProvenanceSession) =
    let activated = { session with ActiveLayerId = layerId }
    refreshLayerWithCatalog catalog layerId activated

let activateLayer layerId (session: ProvenanceSession) =
    activateLayerWithCatalog Map.empty layerId session

let commit (effect: CommandEffect) (session: ProvenanceSession) : ProvenanceSession =
    match view effect with
    | CommandEffectView.NoChange -> session
    | CommandEffectView.Changed(classification, content, mutations) ->
        let mutations = appendImplicitGlobalValueDeletions session content mutations

        let topologyRevision =
            match classification with
            | CommandChangeClassification.Topology
            | CommandChangeClassification.Both -> session.AvailabilityTopologyRevision + 1
            | CommandChangeClassification.Value -> session.AvailabilityTopologyRevision

        let valueRevision =
            match classification with
            | CommandChangeClassification.Value
            | CommandChangeClassification.Both -> session.AnnotationValueRevision + 1
            | CommandChangeClassification.Topology -> session.AnnotationValueRevision

        let staleSession = {
            session with
                Nodes = content.Nodes
                Processes = content.Processes
                Properties = content.Properties
                Values = content.Values
                Layers = content.Layers
                LayerOrder = content.LayerOrder
                ActiveLayerId = content.ActiveLayerId
                AvailabilityTopologyRevision = topologyRevision
                AnnotationValueRevision = valueRevision
                MutationJournal = session.MutationJournal @ mutations
                LayerProjections =
                    session.LayerProjections
                    |> Map.map (fun _ projection -> { projection with Stale = true })
        }

        if staleSession.Layers.ContainsKey staleSession.ActiveLayerId then
            refreshLayer staleSession.ActiveLayerId staleSession
            |> Result.defaultWith (fun error ->
                invalidOp $"The active canonical layer could not be projected after a valid command: {error}"
            )
        else
            staleSession

let addEndpoint layerId side kind header name layerOrderPosition session =
    Commands.addEndpoint layerId side kind header name layerOrderPosition session
    |> Result.map (fun effect -> commit effect session)

let connectNodes layerId pairs session =
    Commands.connectNodes layerId pairs session
    |> Result.map (fun effect -> commit effect session)

let addLayer name selected session =
    Commands.addLayer name selected session
    |> Result.map (fun effect -> commit effect session)

let disconnectLinks linkIds session =
    Commands.disconnectLinks linkIds session
    |> Result.map (fun effect -> commit effect session)

let editValueGlobally valueId content session =
    Commands.editValueGlobally valueId content session
    |> Result.map (fun effect -> commit effect session)

let removeValuesGlobally valueIds session =
    Commands.removeValuesGlobally valueIds session
    |> Result.map (fun effect -> commit effect session)

let removePropertyGlobally propertyId session =
    Commands.removePropertyGlobally propertyId session
    |> Result.map (fun effect -> commit effect session)

let removePropertiesGlobally propertyIds session =
    Commands.removePropertiesGlobally propertyIds session
    |> Result.map (fun effect -> commit effect session)

let resolveNodeAvailability nodeId session =
    Availability.resolveNodeAvailabilityWithMemo nodeId session

let resolveLayerAvailability layerId session =
    Availability.resolveLayerAvailabilityWithMemo layerId session

type private CleanupResult = {
    Session: ProvenanceSession
    Mutations: ProvenanceMutation list
    TopologyChanged: bool
    ValueChanged: bool
}

let private cleanupContext
    (nodeTombstones: NodeAssignmentTombstone list)
    (processTombstones: ProcessAssignmentTombstone list)
    =
    let owners =
        [
            yield!
                nodeTombstones
                |> List.map (fun tombstone -> NodeAssignmentOwner tombstone.OwnerId)

            yield!
                processTombstones
                |> List.map (fun tombstone -> ProcessAssignmentOwner tombstone.OwnerId)
        ]
        |> Set.ofList

    {
        Scope = OwnerScoped owners
        Coverage = {
            AssignmentIds =
                [
                    yield! nodeTombstones |> List.map (fun tombstone -> tombstone.Assignment.Id)
                    yield! processTombstones |> List.map (fun tombstone -> tombstone.Assignment.Id)
                ]
                |> Set.ofList
            LinkIds =
                processTombstones
                |> List.collect (fun tombstone -> tombstone.Assignment.CoveredLinkIds |> Set.toList)
                |> Set.ofList
        }
    }

let private storageOrphanCleanup (session: ProvenanceSession) =
    let assignmentIds = [
        yield!
            session.Nodes
            |> Map.toList
            |> List.collect (fun (_, node) -> node.Assignments |> Map.toList |> List.map (snd >> _.Id))

        yield!
            session.Processes
            |> Map.toList
            |> List.collect (fun (_, structuralProcess) ->
                structuralProcess.Assignments |> Map.toList |> List.map (snd >> _.Id)
            )
    ]

    let duplicateIds =
        assignmentIds
        |> List.countBy id
        |> List.choose (fun (assignmentId, count) -> if count > 1 then Some assignmentId else None)
        |> Set.ofList

    let nodeTombstones: NodeAssignmentTombstone list =
        session.Nodes
        |> Map.toList
        |> List.collect (fun (_, node) ->
            node.Assignments
            |> Map.toList
            |> List.choose (fun (storedKey, assignment) ->
                if
                    storedKey <> assignment.Id
                    || duplicateIds.Contains assignment.Id
                    || not (session.Values.ContainsKey assignment.ValueId)
                then
                    Some {
                        OwnerId = node.Id
                        Assignment = assignment
                    }
                else
                    None
            )
        )

    let processTombstones: ProcessAssignmentTombstone list =
        session.Processes
        |> Map.toList
        |> List.collect (fun (_, structuralProcess) ->
            structuralProcess.Assignments
            |> Map.toList
            |> List.choose (fun (storedKey, assignment) ->
                let ownsCoverage =
                    assignment.CoveredLinkIds |> Set.forall structuralProcess.Links.ContainsKey

                if
                    storedKey <> assignment.Id
                    || duplicateIds.Contains assignment.Id
                    || not (session.Values.ContainsKey assignment.ValueId)
                    || assignment.CoveredLinkIds.IsEmpty
                    || not ownsCoverage
                then
                    Some {
                        OwnerId = structuralProcess.Id
                        Assignment = assignment
                    }
                else
                    None
            )
        )

    let removedByOwner =
        [
            yield!
                nodeTombstones
                |> List.map (fun tombstone -> NodeAssignmentOwner tombstone.OwnerId, tombstone.Assignment.Id)

            yield!
                processTombstones
                |> List.map (fun tombstone -> ProcessAssignmentOwner tombstone.OwnerId, tombstone.Assignment.Id)
        ]
        |> Set.ofList

    let withoutAssignments = {
        session with
            Nodes =
                session.Nodes
                |> Map.map (fun _ node -> {
                    node with
                        Assignments =
                            node.Assignments
                            |> Map.filter (fun storedKey assignment ->
                                not (removedByOwner.Contains(NodeAssignmentOwner node.Id, storedKey))
                                && not (removedByOwner.Contains(NodeAssignmentOwner node.Id, assignment.Id))
                            )
                })
            Processes =
                session.Processes
                |> Map.map (fun _ structuralProcess -> {
                    structuralProcess with
                        Assignments =
                            structuralProcess.Assignments
                            |> Map.filter (fun storedKey assignment ->
                                not (removedByOwner.Contains(ProcessAssignmentOwner structuralProcess.Id, storedKey))
                                && not (
                                    removedByOwner.Contains(
                                        ProcessAssignmentOwner structuralProcess.Id,
                                        assignment.Id
                                    )
                                )
                            )
                })
    }

    let orphanValueIds = orphanValueDefinitionIds withoutAssignments

    let removedValues =
        orphanValueIds
        |> Set.toList
        |> List.choose (fun valueId -> withoutAssignments.Values |> Map.tryFind valueId)

    let cleaned = {
        withoutAssignments with
            Values =
                withoutAssignments.Values
                |> Map.filter (fun valueId _ -> not (orphanValueIds.Contains valueId))
    }

    let context = cleanupContext nodeTombstones processTombstones

    let tombstonesForValue valueId = [
        yield!
            nodeTombstones
            |> List.choose (fun tombstone ->
                if tombstone.Assignment.ValueId = valueId then
                    Some(NodeTombstone tombstone)
                else
                    None
            )

        yield!
            processTombstones
            |> List.choose (fun tombstone ->
                if tombstone.Assignment.ValueId = valueId then
                    Some(ProcessTombstone tombstone)
                else
                    None
            )
    ]

    {
        Session = cleaned
        Mutations = [
            yield!
                nodeTombstones
                |> List.map (fun tombstone -> NodeAssignmentRemoved(tombstone, context))

            yield!
                processTombstones
                |> List.map (fun tombstone -> ProcessAssignmentRemoved(tombstone, context))

            yield!
                removedValues
                |> List.map (fun value -> PropertyValueDefinitionDeleted(value, tombstonesForValue value.Id, context))
        ]
        TopologyChanged = not nodeTombstones.IsEmpty || not processTombstones.IsEmpty
        ValueChanged = not removedValues.IsEmpty
    }

let private applyCleanupRevisions cleanup =
    if not cleanup.TopologyChanged && not cleanup.ValueChanged then
        cleanup.Session
    else
        {
            cleanup.Session with
                AvailabilityTopologyRevision =
                    cleanup.Session.AvailabilityTopologyRevision
                    + (if cleanup.TopologyChanged then 1 else 0)
                AnnotationValueRevision =
                    cleanup.Session.AnnotationValueRevision
                    + (if cleanup.ValueChanged then 1 else 0)
                LayerProjections =
                    cleanup.Session.LayerProjections
                    |> Map.map (fun _ projection -> { projection with Stale = true })
                MutationJournal = cleanup.Session.MutationJournal @ cleanup.Mutations
        }

let private orderedLayerIds (session: ProvenanceSession) =
    [
        yield! session.LayerOrder

        yield!
            session.Layers
            |> Map.toList
            |> List.map fst
            |> List.filter (fun layerId -> session.LayerOrder |> List.contains layerId |> not)
    ]
    |> List.distinct

let private refreshPendingLayers (session: ProvenanceSession) =
    let refreshIfNeeded state layerId =
        state
        |> Result.bind (fun current ->
            let needsRefresh =
                current.LayerProjections
                |> Map.tryFind layerId
                |> Option.map (fun projection ->
                    projection.Stale
                    || projection.TopologyRevision <> current.AvailabilityTopologyRevision
                    || projection.ValueRevision <> current.AnnotationValueRevision
                )
                |> Option.defaultValue true

            if needsRefresh then
                refreshLayer layerId current
            else
                Ok current
        )

    orderedLayerIds session |> List.fold refreshIfNeeded (Ok session)

let private firstError checks =
    checks |> List.tryPick id |> Option.map Error |> Option.defaultValue (Ok())

let private validateCanonicalReferences (session: ProvenanceSession) =
    let layerErrors = [
        for storedLayerId, layer in session.Layers |> Map.toList do
            if storedLayerId <> layer.Id then
                Some(InconsistentCanonicalState $"Layer map key '{storedLayerId}' differs from '{layer.Id}'.")

            for storedNodeId, endpoint in layer.InputEndpoints |> Map.toList do
                if
                    storedNodeId <> endpoint.Key.NodeId
                    || endpoint.Key.LayerId <> layer.Id
                    || endpoint.Key.Side <> ProvenanceSide.Input
                then
                    Some(InconsistentCanonicalState $"Input endpoint '{storedNodeId}' has an inconsistent key.")
                elif not (session.Nodes.ContainsKey storedNodeId) then
                    Some(NodeNotFound storedNodeId)

            for storedNodeId, endpoint in layer.OutputEndpoints |> Map.toList do
                if
                    storedNodeId <> endpoint.Key.NodeId
                    || endpoint.Key.LayerId <> layer.Id
                    || endpoint.Key.Side <> ProvenanceSide.Output
                then
                    Some(InconsistentCanonicalState $"Output endpoint '{storedNodeId}' has an inconsistent key.")
                elif not (session.Nodes.ContainsKey storedNodeId) then
                    Some(NodeNotFound storedNodeId)

            for processId in layer.StructuralProcessIds do
                if not (session.Processes.ContainsKey processId) then
                    Some(ProcessNotFound processId)
    ]

    let valueErrors = [
        for valueId, value in session.Values |> Map.toList do
            if valueId <> value.Id then
                Some(InconsistentCanonicalState $"Value map key '{valueId}' differs from '{value.Id}'.")
            elif not (session.Properties.ContainsKey value.PropertyId) then
                Some(PropertyNotFound value.PropertyId)
    ]

    let nodeErrors = [
        for storedNodeId, node in session.Nodes |> Map.toList do
            if storedNodeId <> node.Id then
                Some(InconsistentCanonicalState $"Node map key '{storedNodeId}' differs from '{node.Id}'.")

            for storedAssignmentId, assignment in node.Assignments |> Map.toList do
                if storedAssignmentId <> assignment.Id then
                    Some(
                        InconsistentCanonicalState
                            $"Node assignment map key '{storedAssignmentId}' differs from '{assignment.Id}'."
                    )
                elif not (session.Values.ContainsKey assignment.ValueId) then
                    Some(ValueNotFound assignment.ValueId)
    ]

    let processErrors = [
        for storedProcessId, structuralProcess in session.Processes |> Map.toList do
            if storedProcessId <> structuralProcess.Id then
                Some(
                    InconsistentCanonicalState
                        $"Process map key '{storedProcessId}' differs from '{structuralProcess.Id}'."
                )
            elif not (session.Layers.ContainsKey structuralProcess.OriginLayerId) then
                Some(LayerNotFound structuralProcess.OriginLayerId)

            for storedLinkId, processLink in structuralProcess.Links |> Map.toList do
                if storedLinkId <> processLink.Id then
                    Some(InconsistentCanonicalState $"Link map key '{storedLinkId}' differs from '{processLink.Id}'.")

                let referencedNodeIds =
                    match processLink.Shape with
                    | ProcessLinkShape.Between(inputId, outputId) -> [ inputId; outputId ]
                    | ProcessLinkShape.InputOnly inputId -> [ inputId ]
                    | ProcessLinkShape.OutputOnly outputId -> [ outputId ]
                    | ProcessLinkShape.Endpointless -> []

                for nodeId in referencedNodeIds do
                    if not (session.Nodes.ContainsKey nodeId) then
                        Some(NodeNotFound nodeId)

            for storedAssignmentId, assignment in structuralProcess.Assignments |> Map.toList do
                if storedAssignmentId <> assignment.Id then
                    Some(
                        InconsistentCanonicalState
                            $"Process assignment map key '{storedAssignmentId}' differs from '{assignment.Id}'."
                    )
                elif not (session.Values.ContainsKey assignment.ValueId) then
                    Some(ValueNotFound assignment.ValueId)
                elif assignment.CoveredLinkIds.IsEmpty then
                    Some(InconsistentCanonicalState $"Process assignment '{assignment.Id}' has empty coverage.")
                else
                    for linkId in assignment.CoveredLinkIds do
                        if not (structuralProcess.Links.ContainsKey linkId) then
                            Some(LinkNotFound linkId)
    ]

    firstError (layerErrors @ valueErrors @ nodeErrors @ processErrors)

let private validateContainerBindings (session: ProvenanceSession) =
    let errors = [
        for _, structuralProcess in session.Processes |> Map.toList do
            for _, assignment in structuralProcess.Assignments |> Map.toList do
                match assignment.ContainerReferenceValueId with
                | None -> ()
                | Some requiredValueId ->
                    let requiredIsReference =
                        match session.Values |> Map.tryFind requiredValueId with
                        | Some { Value = ProvenanceValue.Reference _ } -> true
                        | _ -> false

                    let missingLinks =
                        assignment.CoveredLinkIds
                        |> Set.filter (fun linkId ->
                            let matchingContainers =
                                structuralProcess.Assignments
                                |> Map.toList
                                |> List.map snd
                                |> List.filter (fun candidate ->
                                    candidate.ValueId = requiredValueId && candidate.CoveredLinkIds.Contains linkId
                                )

                            not requiredIsReference || matchingContainers.Length <> 1
                        )

                    if not missingLinks.IsEmpty then
                        Some(MissingReferenceContainer(assignment.Id, requiredValueId, missingLinks))
    ]

    firstError errors

let private validateReferenceSlots (session: ProvenanceSession) =
    let occupied =
        session.Processes
        |> Map.toList
        |> List.collect (fun (_, structuralProcess) ->
            structuralProcess.Assignments
            |> Map.toList
            |> List.collect (fun (_, assignment) ->
                match assignment.ReferenceSlotId with
                | None -> []
                | Some slotId ->
                    assignment.CoveredLinkIds
                    |> Set.toList
                    |> List.map (fun linkId -> slotId, linkId, assignment.Id, assignment.ValueId)
            )
        )
        |> List.groupBy (fun (slotId, linkId, _, _) -> slotId, linkId)
        |> List.tryPick (fun ((slotId, linkId), entries) ->
            let assignmentIds =
                entries |> List.map (fun (_, _, assignmentId, _) -> assignmentId) |> Set.ofList

            let allReferenceValues =
                entries
                |> List.forall (fun (_, _, _, valueId) ->
                    match session.Values |> Map.tryFind valueId with
                    | Some { Value = ProvenanceValue.Reference _ } -> true
                    | _ -> false
                )

            if not allReferenceValues then
                Some(
                    InconsistentCanonicalState
                        $"Reference slot '{slotId}' on link '{linkId}' is attached to a non-reference value."
                )
            elif assignmentIds.Count > 1 then
                Some(ReferenceSlotOccupied(slotId, Set.singleton linkId, assignmentIds))
            else
                None
        )

    occupied |> Option.map Error |> Option.defaultValue (Ok())

let private validateProjectionBacking layerId (session: ProvenanceSession) backing =
    match backing with
    | NodeAssignmentBacking(identity, ownerId, _) ->
        match session.Nodes |> Map.tryFind ownerId with
        | None -> Error(InconsistentLayerProjection(layerId, $"Node owner '{ownerId}' is missing."))
        | Some node ->
            match node.Assignments |> Map.tryFind identity.AssignmentId with
            | None ->
                Error(
                    InconsistentLayerProjection(
                        layerId,
                        $"Node assignment '{identity.AssignmentId}' is missing from '{ownerId}'."
                    )
                )
            | Some assignment ->
                let propertyId =
                    session.Values |> Map.tryFind assignment.ValueId |> Option.map _.PropertyId

                if
                    identity.ValueId <> assignment.ValueId
                    || identity.PropertyKind <> assignment.PropertyKind
                    || propertyId <> Some identity.PropertyId
                then
                    Error(
                        InconsistentLayerProjection(
                            layerId,
                            $"Node assignment '{identity.AssignmentId}' has conflicting projected identity."
                        )
                    )
                else
                    Ok()
    | ProcessAssignmentBacking(identity, ownerId, linkIds, containerValueId, slotId) ->
        match session.Processes |> Map.tryFind ownerId with
        | None -> Error(InconsistentLayerProjection(layerId, $"Process owner '{ownerId}' is missing."))
        | Some structuralProcess ->
            match structuralProcess.Assignments |> Map.tryFind identity.AssignmentId with
            | None ->
                Error(
                    InconsistentLayerProjection(
                        layerId,
                        $"Process assignment '{identity.AssignmentId}' is missing from '{ownerId}'."
                    )
                )
            | Some assignment ->
                let propertyId =
                    session.Values |> Map.tryFind assignment.ValueId |> Option.map _.PropertyId

                if
                    identity.ValueId <> assignment.ValueId
                    || identity.PropertyKind <> assignment.PropertyKind
                    || propertyId <> Some identity.PropertyId
                    || linkIds <> assignment.CoveredLinkIds
                    || containerValueId <> assignment.ContainerReferenceValueId
                    || slotId <> assignment.ReferenceSlotId
                then
                    Error(
                        InconsistentLayerProjection(
                            layerId,
                            $"Process assignment '{identity.AssignmentId}' has conflicting projected identity or coverage."
                        )
                    )
                else
                    Ok()

let private validateLayerProjections (session: ProvenanceSession) =
    let validateProjection state (layerId, projection) =
        state
        |> Result.bind (fun () ->
            if
                projection.Stale
                || projection.TopologyRevision <> session.AvailabilityTopologyRevision
                || projection.ValueRevision <> session.AnnotationValueRevision
            then
                Error(InconsistentLayerProjection(layerId, "The cached projection is not current."))
            else
                let backings = [
                    yield!
                        projection.Groups
                        |> List.collect (fun group -> group.Annotations |> List.map _.Backing)

                    yield!
                        projection.Connectors
                        |> List.collect (fun connector -> connector.Annotations |> List.map _.Backing)

                    yield!
                        projection.ProcessOnlyEntries
                        |> List.collect (fun entry -> entry.Annotations |> List.map _.Backing)

                    yield!
                        projection.ShelfEntries
                        |> List.choose (fun entry ->
                            match entry.Payload with
                            | AssignmentBacked payload -> Some payload.Backing
                            | CatalogBacked _ -> None
                        )
                ]

                backings
                |> List.fold
                    (fun backingState backing ->
                        backingState
                        |> Result.bind (fun () -> validateProjectionBacking layerId session backing)
                    )
                    (Ok())
        )

    session.LayerProjections |> Map.toList |> List.fold validateProjection (Ok())

let prepareForWriteback (session: ProvenanceSession) =
    let cleanup = storageOrphanCleanup session
    let cleaned = applyCleanupRevisions cleanup

    refreshPendingLayers cleaned
    |> Result.bind (fun refreshed ->
        validateCanonicalReferences refreshed
        |> Result.bind (fun () -> validateContainerBindings refreshed)
        |> Result.bind (fun () -> validateReferenceSlots refreshed)
        |> Result.bind (fun () -> validateLayerProjections refreshed)
        |> Result.map (fun () -> refreshed)
    )

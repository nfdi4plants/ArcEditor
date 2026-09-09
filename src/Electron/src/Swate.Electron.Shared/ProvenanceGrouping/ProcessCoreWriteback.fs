module Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreWriteback

open ProcessCore
open System.Globalization
open System.Collections.Generic
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreAdapterTypes
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreGraph

module CanonicalIdentifiers = Swate.Components.Page.ProvenanceGrouping.Identifiers
module CanonicalPlan = Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreWritebackPlan
module CanonicalProjectionTypes = Swate.Components.Page.ProvenanceGrouping.ProjectionTypes
module CanonicalValues = Swate.Components.Page.ProvenanceGrouping.Values

/// One indexed annotation occurrence resolved to the live ProcessCore object
/// and the collection holding it, captured before any mutation so later
/// additions and removals cannot shift the positions it was resolved from.
type private ResolvedCanonicalAnnotation = {
    Owner: ProcessCoreCanonicalAnnotationOwner
    Annotation: Annotation
    Collection: ResizeArray<Annotation>
    Position: int
}

/// One canonical node's live ProcessCore representation. `IsNewObject` is true
/// only when the node exists neither in the loaded index nor anywhere in the
/// ARC, so an equal-key node outside the loaded selection is attached to
/// instead of duplicated.
type private ResolvedCanonicalNode = {
    Node: IONode
    IsNewObject: bool
    /// Every physical occurrence this canonical node is known to have at
    /// load time. A merged multi-source node (H.2) can have more than one;
    /// reconciliation below must touch every one of an assignment's own
    /// recorded occurrences, not only whichever object `Node` happens to be.
    ExistingLocations: ProcessCoreCanonicalNodeSourceLocation list
    Annotations: CanonicalPlan.PlannedAnnotation list
}

[<RequireQualifiedAccess>]
type private CanonicalProcessTarget =
    /// The exact indexed ProcessCore process, updated in place.
    | Indexed of Process
    /// A shell cloned from the indexed process. Annotations are written from
    /// the plan rather than copied, so two partitions of one original process
    /// can never share an annotation object.
    | Clone of source: Process * destination: Dataset
    | Created of destination: Dataset

type private ResolvedCanonicalProcess = {
    Planned: CanonicalPlan.PlannedProcess
    Target: CanonicalProcessTarget
    Inputs: IONode list
    Outputs: IONode list
    Annotations: CanonicalPlan.PlannedAnnotation list
}

type private ResolvedCanonicalPlan = {
    Plan: CanonicalPlan.ProcessCoreWritebackPlan
    Nodes: (CanonicalIdentifiers.CanonicalNodeId * ResolvedCanonicalNode) list
    Processes: ResolvedCanonicalProcess list
    Removals: (Dataset * Process) list
    Occurrences: Map<CanonicalIdentifiers.AnnotationAssignmentId, ResolvedCanonicalAnnotation list>
    Remintings: Map<CanonicalIdentifiers.AnnotationAssignmentId, CanonicalPlan.PlannedAnnotationReminting>
    DetachedAnnotations: (ResizeArray<Annotation> * int * Annotation) list
}

type private PhysicalAnnotationSlot = {
    Collection: ResizeArray<Annotation>
    Position: int
    Annotation: Annotation
}

let private canonicalInvalidState message =
    ProcessCoreWritebackError.InvalidPreparedState message

/// Both public entry points refuse a session whose layer projections were never
/// resolved, so no caller can bypass `Session.prepareForWriteback`.
let private validateCanonicalProjections (session: CanonicalProjectionTypes.ProvenanceSession) = [
    for KeyValue(layerId, _) in session.Layers do
        match session.LayerProjections |> Map.tryFind layerId with
        | None ->
            yield
                canonicalInvalidState
                    $"Layer '{layerId}' has no resolved projection; the session was not prepared for writeback."
        | Some projection ->
            if
                projection.Stale
                || projection.TopologyRevision <> session.AvailabilityTopologyRevision
                || projection.ValueRevision <> session.AnnotationValueRevision
            then
                yield
                    canonicalInvalidState
                        $"Layer '{layerId}' has an unresolved projection invalidation; the session was not prepared for writeback."
]

let private validateCanonicalGraph (index: ProcessCoreWritebackIndex) (arc: ARC) =
    if graphFingerprint arc <> index.ArcFingerprint then
        [ ProcessCoreWritebackError.StaleArc ]
    else
        []

let private validateCanonicalSources
    (index: ProcessCoreWritebackIndex)
    (session: CanonicalProjectionTypes.ProvenanceSession)
    =
    [
        if index.LoadedProcessGroups.IsEmpty then
            yield canonicalInvalidState "The canonical index carries no loaded process group."

        let layerIds = session.Layers |> Map.keys |> Set.ofSeq
        let orderIds = session.LayerOrder |> Set.ofList

        if layerIds <> orderIds || session.LayerOrder.Length <> orderIds.Count then
            yield ProcessCoreWritebackError.InvalidLayerOrder session.LayerOrder

        match CanonicalPlan.tryResolveLayerDestinations index session with
        | Ok _ -> ()
        | Error errors -> yield! errors

        for KeyValue(sourceId, _) in index.SourceLocations do
            if session.Layers |> Map.exists (fun _ layer -> layer.Source.Id = sourceId) |> not then
                yield ProcessCoreWritebackError.InitialLayerNotFound sourceId
    ]

let private canonicalAssignmentIds (session: CanonicalProjectionTypes.ProvenanceSession) =
    Set.union
        (session.Nodes
         |> Map.toSeq
         |> Seq.collect (fun (_, node) -> node.Assignments |> Map.keys)
         |> Set.ofSeq)
        (session.Processes
         |> Map.toSeq
         |> Seq.collect (fun (_, structuralProcess) -> structuralProcess.Assignments |> Map.keys)
         |> Set.ofSeq)

let private projectedBackings (projection: CanonicalProjectionTypes.CachedLayerProjection) = [
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
            | CanonicalProjectionTypes.AssignmentBacked payload -> Some payload.Backing
            | CanonicalProjectionTypes.CatalogBacked _ -> None
        )
]

/// Every projected availability reference a materialization depends on must
/// resolve to an originating assignment *and* to an adapter origin. Generic
/// preparation checks projections against canonical state only; this pass
/// additionally requires the indexed occurrence or indexed stored resource the
/// reference ultimately derives from to exist.
let private validateCanonicalAvailability
    (index: ProcessCoreWritebackIndex)
    (session: CanonicalProjectionTypes.ProvenanceSession)
    =
    let errors = ResizeArray<ProcessCoreWritebackError>()

    let sourceLocations =
        CanonicalPlan.tryResolveLayerDestinations index session
        |> Result.defaultValue index.SourceLocations

    let knownAssignmentIds = canonicalAssignmentIds session

    let isReferenceValue valueId =
        session.Values
        |> Map.tryFind valueId
        |> Option.exists (fun definition ->
            match definition.Value with
            | CanonicalValues.ProvenanceValue.Reference _ -> true
            | _ -> false
        )

    let validateLineage assignmentId valueId (lineage: CanonicalValues.AssignmentLineage) =
        match lineage with
        | CanonicalValues.AssignmentLineage.Created -> ()
        | CanonicalValues.AssignmentLineage.Loaded ->
            // A loaded reference occupies a storage slot rather than an
            // annotation position, so Recipe resolution - not an indexed
            // annotation occurrence - is its adapter origin.
            if
                not (isReferenceValue valueId)
                && not (index.AssignmentLocations.ContainsKey assignmentId)
            then
                errors.Add(
                    canonicalInvalidState
                        $"Projected reference for loaded assignment '{assignmentId}' has no indexed annotation occurrence."
                )
        | CanonicalValues.AssignmentLineage.DerivedFrom parentId ->
            if
                not (index.AssignmentLocations.ContainsKey parentId)
                && not (knownAssignmentIds.Contains parentId)
            then
                errors.Add(
                    canonicalInvalidState
                        $"Projected reference for assignment '{assignmentId}' derives from unknown assignment '{parentId}'."
                )
        | CanonicalValues.AssignmentLineage.DerivedFromCatalog(scheme, resourceId, _) ->
            if not (index.RecipeResources.ContainsKey(scheme, resourceId)) then
                errors.Add(
                    ProcessCoreWritebackError.RecipeResourceNotFound(
                        scheme,
                        Swate.Components.ProcessCore.Copy.RecipeResourceKey.ById resourceId
                    )
                )

    let validateIdentity
        (identity: CanonicalProjectionTypes.AssignmentProjectionIdentity)
        (valueId: CanonicalIdentifiers.PropertyValueDefinitionId)
        (propertyKind: CanonicalValues.AssignmentPropertyKind)
        (lineage: CanonicalValues.AssignmentLineage)
        =
        if identity.ValueId <> valueId || identity.PropertyKind <> propertyKind then
            errors.Add(
                canonicalInvalidState
                    $"Projected reference for assignment '{identity.AssignmentId}' disagrees with its canonical value identity."
            )

        match session.Values |> Map.tryFind valueId with
        | Some definition when definition.PropertyId = identity.PropertyId -> ()
        | _ -> errors.Add(ProcessCoreWritebackError.ValueNotFound valueId)

        validateLineage identity.AssignmentId valueId lineage

    let validateBacking backing =
        match backing with
        | CanonicalProjectionTypes.NodeAssignmentBacking(identity, ownerId, targetSource) ->
            match session.Nodes |> Map.tryFind ownerId with
            | None -> errors.Add(ProcessCoreWritebackError.NodeNotFound ownerId)
            | Some node ->
                match node.Assignments |> Map.tryFind identity.AssignmentId with
                | None -> errors.Add(ProcessCoreWritebackError.AssignmentNotFound identity.AssignmentId)
                | Some assignment ->
                    validateIdentity identity assignment.ValueId assignment.PropertyKind assignment.Lineage

            targetSource
            |> Option.iter (fun source ->
                if not (sourceLocations.ContainsKey source.Id) then
                    errors.Add(ProcessCoreWritebackError.SourceLocationNotFound source.Id)
            )
        | CanonicalProjectionTypes.ProcessAssignmentBacking(identity, ownerId, linkIds, _, _) ->
            match session.Processes |> Map.tryFind ownerId with
            | None -> errors.Add(ProcessCoreWritebackError.ProcessNotFound ownerId)
            | Some structuralProcess ->
                match structuralProcess.Assignments |> Map.tryFind identity.AssignmentId with
                | None -> errors.Add(ProcessCoreWritebackError.AssignmentNotFound identity.AssignmentId)
                | Some assignment ->
                    validateIdentity identity assignment.ValueId assignment.PropertyKind assignment.Lineage

                    for linkId in linkIds do
                        if
                            not (assignment.CoveredLinkIds.Contains linkId)
                            || not (structuralProcess.Links.ContainsKey linkId)
                        then
                            errors.Add(ProcessCoreWritebackError.LinkNotFound linkId)

    for KeyValue(_, projection) in session.LayerProjections do
        for backing in projectedBackings projection do
            validateBacking backing

    errors |> Seq.distinct |> Seq.toList

let private nodeAnnotationCollection (node: IONode) =
    match node with
    | SampleNode sample -> sample.AdditionalProperty
    | DataNode data -> data.AdditionalProperty

/// Count physical slots, not appearances of a node through several processes.
/// Include owners outside the loaded selection and immutable Recipe metadata:
/// an indexed YAML annotation can be shared with any of them.
let private physicalAnnotationSlots (arc: ARC) =
    let collections = HashSet<ResizeArray<Annotation>>(HashIdentity.Reference)
    let slots = ResizeArray<PhysicalAnnotationSlot>()

    let visit (items: ResizeArray<Annotation>) =
        if collections.Add items then
            for position in 0 .. items.Count - 1 do
                slots.Add {
                    Collection = items
                    Position = position
                    Annotation = items[position]
                }

    let visitNode node = visit (nodeAnnotationCollection node)

    let rec visitData (data: Data) =
        visit data.AdditionalProperty

        for child in data.HasPart do
            visitData child

    let visitAgent (agent: Agent) = visit agent.AdditionalProperty

    for entry in datasetEntries arc do
        visit entry.Dataset.AdditionalProperty

        for proc in entry.Dataset.Processes do
            visit proc.ParameterValue
            proc.Input |> Option.iter visitNode
            proc.Output |> Option.iter visitNode

        for data in entry.Dataset.DataFiles do
            visitData data

        for agent in entry.Dataset.Agents do
            visitAgent agent

        for citation in entry.Dataset.Citations do
            visit citation.AdditionalProperty

            for author in citation.Authors do
                visitAgent author

    // Stored samples can contain annotations without being referenced by a
    // selected process. They still participate in the indexed ARC registry.
    for sample in arc.Samples do
        visit sample.AdditionalProperty

    // Recipe Components and metadata are encoded through the same annotation
    // registry when the ARC is written with stores.
    for recipe in Swate.Components.ProcessCore.ObjectGraph.recipes arc do
        visit recipe.Components
        visit recipe.AdditionalProperty

    slots |> Seq.toList

/// Count physical slots, not appearances of a node through several processes.
/// Include owners outside the loaded selection and immutable Recipe metadata:
/// an indexed YAML annotation can be shared with any of them.
let private sharedAnnotationReferences (arc: ARC) =
    let counts = Dictionary<Annotation, int>(HashIdentity.Reference)

    for slot in physicalAnnotationSlots arc do
        match counts.TryGetValue slot.Annotation with
        | true, count -> counts[slot.Annotation] <- count + 1
        | _ -> counts.Add(slot.Annotation, 1)

    let shared = HashSet<Annotation>(HashIdentity.Reference)

    for KeyValue(annotation, count) in counts do
        if count > 1 then
            shared.Add annotation |> ignore

    shared

/// Removes the exact object. `Annotation.Equals` compares only name, value,
/// unit and nameTAN, so the published `Remove*` members would drop the first
/// equal occurrence instead of this one.
let private removeAnnotationByReference (annotations: ResizeArray<Annotation>) (target: Annotation) =
    let mutable position = -1

    for index in 0 .. annotations.Count - 1 do
        if position < 0 && obj.ReferenceEquals(annotations.[index], target) then
            position <- index

    if position >= 0 then
        annotations.RemoveAt position
        true
    else
        false

let private canonicalAnnotationAtPosition (position: int) (annotations: Annotation seq) =
    let items = annotations |> Seq.toList

    if position >= 0 && position < items.Length then
        Some items.[position]
    else
        None

/// Resolves one writable indexed occurrence to its live object and the
/// collection holding it. Recipe Components are read-only projections whose
/// positions and payloads the planner already validated against the stored
/// resource, so they never resolve to a writable target.
/// Resolves the exact live node at one physical occurrence via its owning
/// process and side, never by (kind, name) alone: a canonical node merged
/// from several equal-key physical occurrences (H.2) can have distinct live
/// objects sharing one key, which `tryResolveNode` cannot tell apart.
let private tryResolveExactNode (location: ProcessCoreCanonicalNodeSourceLocation) (arc: ARC) : IONode option =
    tryResolveProcess location.Process arc
    |> Option.bind (fun proc ->
        match location.Side with
        | CanonicalIdentifiers.ProvenanceSide.Input -> proc.Input
        | CanonicalIdentifiers.ProvenanceSide.Output -> proc.Output
    )

let private tryResolveCanonicalOccurrence (arc: ARC) (location: ProcessCoreCanonicalAnnotationLocation) =
    let inCollection (collection: ResizeArray<Annotation>) =
        canonicalAnnotationAtPosition location.Position collection
        |> Option.map (fun annotation -> annotation, collection)

    match location.Owner with
    | ProcessCoreCanonicalAnnotationOwner.NodeAdditionalProperty nodeSourceLocation ->
        tryResolveExactNode nodeSourceLocation arc
        |> Option.bind (fun node -> inCollection (nodeAnnotationCollection node))
    | ProcessCoreCanonicalAnnotationOwner.ProcessParameterValue processLocation ->
        tryResolveProcess processLocation arc
        |> Option.bind (fun proc -> inCollection proc.ParameterValue)
    | ProcessCoreCanonicalAnnotationOwner.RecipeComponent _ -> None

let private isWritableCanonicalOwner (owner: ProcessCoreCanonicalAnnotationOwner) =
    match owner with
    | ProcessCoreCanonicalAnnotationOwner.RecipeComponent _ -> false
    | _ -> true

/// Binds a fully validated plan to the exact live ProcessCore objects it will
/// mutate. Every lookup happens here, so `apply` performs only in-memory
/// mutations that cannot fail part-way through.
let private resolveCanonicalPlan
    (index: ProcessCoreWritebackIndex)
    (session: CanonicalProjectionTypes.ProvenanceSession)
    (plan: CanonicalPlan.ProcessCoreWritebackPlan)
    (arc: ARC)
    : Result<ResolvedCanonicalPlan, ProcessCoreWritebackError list> =
    let errors = ResizeArray<ProcessCoreWritebackError>()

    let occurrences =
        index.AssignmentLocations
        |> Map.map (fun assignmentId locations ->
            locations
            |> List.filter (fun location -> isWritableCanonicalOwner location.Owner)
            |> List.choose (fun location ->
                match tryResolveCanonicalOccurrence arc location with
                | None ->
                    errors.Add(ProcessCoreWritebackError.SourceLocationNotFound $"annotation:{assignmentId}")

                    None
                | Some(annotation, collection) ->
                    if canonicalAnnotationFingerprint annotation <> location.Fingerprint then
                        errors.Add ProcessCoreWritebackError.StaleArc

                    Some {
                        Owner = location.Owner
                        Annotation = annotation
                        Collection = collection
                        Position = location.Position
                    }
            )
        )

    let nodes =
        plan.Nodes
        |> List.choose (fun planned ->
            match
                planned.ExistingLocations
                |> List.tryPick (fun location -> tryResolveExactNode location arc)
            with
            | Some node ->
                Some(
                    planned.NodeId,
                    {
                        Node = node
                        IsNewObject = false
                        ExistingLocations = planned.ExistingLocations
                        Annotations = planned.Annotations
                    }
                )
            | None when not planned.ExistingLocations.IsEmpty ->
                errors.Add(
                    ProcessCoreWritebackError.SourceLocationNotFound $"node:{planned.Key.KindId}:{planned.Key.Name}"
                )

                None
            | None ->
                match session.Nodes |> Map.tryFind planned.NodeId with
                | None ->
                    errors.Add(ProcessCoreWritebackError.NodeNotFound planned.NodeId)
                    None
                | Some canonicalNode ->
                    match nodeFromCanonicalNode canonicalNode with
                    | Error resolutionError ->
                        errors.Add resolutionError
                        None
                    | Ok materialized ->
                        // ProcessCore canonicalizes by key when the node is linked into a
                        // process, so a node equal to one already in the ARC must be
                        // resolved up front - otherwise annotations would be written to a
                        // reference the graph then discards.
                        match tryResolveNode (nodeLocation materialized) arc with
                        | Some existing ->
                            Some(
                                planned.NodeId,
                                {
                                    Node = existing
                                    IsNewObject = false
                                    ExistingLocations = []
                                    Annotations = planned.Annotations
                                }
                            )
                        | None ->
                            Some(
                                planned.NodeId,
                                {
                                    Node = materialized
                                    IsNewObject = true
                                    ExistingLocations = []
                                    Annotations = planned.Annotations
                                }
                            )
        )

    let nodeObjects =
        nodes
        |> List.map (fun (nodeId, resolved) -> nodeId, resolved.Node)
        |> Map.ofList

    let resolveEndpoint nodeId =
        match nodeObjects |> Map.tryFind nodeId with
        | Some node -> [ node ]
        | None ->
            errors.Add(ProcessCoreWritebackError.NodeNotFound nodeId)
            []

    let partitionById =
        plan.Partitions
        |> List.map (fun partition -> partition.Id, partition)
        |> Map.ofList

    let processes =
        plan.Processes
        |> List.choose (fun planned ->
            let destination = tryResolveDataset planned.Destination.DatasetPath arc

            let indexed =
                planned.IndexedProcess
                |> Option.bind (fun location -> tryResolveProcess location arc)

            let annotations =
                match partitionById |> Map.tryFind planned.PartitionId with
                | Some partition -> partition.Assignments |> List.choose _.Annotation
                | None ->
                    errors.Add(canonicalInvalidState $"Planned partition '{planned.PartitionId}' is missing.")

                    []

            let inputs, outputs =
                match planned.Shape with
                | CanonicalValues.ProcessLinkShape.Between(inputId, outputId) ->
                    resolveEndpoint inputId, resolveEndpoint outputId
                | CanonicalValues.ProcessLinkShape.InputOnly inputId -> resolveEndpoint inputId, []
                | CanonicalValues.ProcessLinkShape.OutputOnly outputId -> [], resolveEndpoint outputId
                | CanonicalValues.ProcessLinkShape.Endpointless -> [], []

            let target =
                match destination with
                | None ->
                    errors.Add(
                        ProcessCoreWritebackError.SourceLocationNotFound(
                            String.concat "/" planned.Destination.DatasetPath
                        )
                    )

                    None
                | Some dataset ->
                    match planned.Disposition with
                    | CanonicalPlan.PlannedProcessDisposition.ReuseIndexed ->
                        match indexed with
                        | Some proc -> Some(CanonicalProcessTarget.Indexed proc)
                        | None ->
                            errors.Add(ProcessCoreWritebackError.ProcessNotFound planned.StructuralProcessId)

                            None
                    | CanonicalPlan.PlannedProcessDisposition.CloneIndexed ->
                        match indexed with
                        | Some proc -> Some(CanonicalProcessTarget.Clone(proc, dataset))
                        | None ->
                            errors.Add(ProcessCoreWritebackError.ProcessNotFound planned.StructuralProcessId)

                            None
                    | CanonicalPlan.PlannedProcessDisposition.NewProcess ->
                        Some(CanonicalProcessTarget.Created dataset)

            target
            |> Option.map (fun target -> {
                Planned = planned
                Target = target
                Inputs = inputs
                Outputs = outputs
                Annotations = annotations
            })
        )

    let removals =
        plan.ProcessRemovals
        |> List.choose (fun removal ->
            match tryResolveDataset removal.Location.DatasetPath arc, tryResolveProcess removal.Location arc with
            | Some dataset, Some proc -> Some(dataset, proc)
            | _ ->
                errors.Add(ProcessCoreWritebackError.ProcessNotFound removal.StructuralProcessId)

                None
        )

    if errors.Count > 0 then
        Error(errors |> Seq.distinct |> Seq.toList)
    else
        let shared = sharedAnnotationReferences arc

        let finalAnnotations =
            (nodes |> List.collect (snd >> _.Annotations))
            @ (processes |> List.collect _.Annotations)

        let changedAssignments =
            finalAnnotations
            |> List.filter (fun planned ->
                occurrences
                |> Map.tryFind planned.AssignmentId
                |> Option.defaultValue []
                |> List.exists (fun item -> canonicalAnnotationFingerprint item.Annotation <> planned.Fingerprint)
            )
            |> List.map _.AssignmentId
            |> Set.ofList

        // Resolve clones without mutating the ARC. Several indexed appearances
        // may name the same physical slot, which must be detached only once.
        let detached =
            Dictionary<ResizeArray<Annotation>, Dictionary<int, Annotation>>(HashIdentity.Reference)

        let occurrences =
            occurrences
            |> Map.map (fun assignmentId items ->
                items
                |> List.map (fun item ->
                    if changedAssignments.Contains assignmentId && shared.Contains item.Annotation then
                        let slots =
                            match detached.TryGetValue item.Collection with
                            | true, slots -> slots
                            | _ ->
                                let slots = Dictionary<int, Annotation>()
                                detached.Add(item.Collection, slots)
                                slots

                        let clone =
                            match slots.TryGetValue item.Position with
                            | true, clone -> clone
                            | _ ->
                                let payload = (canonicalAnnotationFingerprint item.Annotation).Payload
                                let clone = ProcessCore.Yaml.Annotation.fromYamlString false payload
                                slots.Add(item.Position, clone)
                                clone

                        { item with Annotation = clone }
                    else
                        item
                )
            )

        Ok {
            Plan = plan
            Nodes = nodes
            Processes = processes
            Removals = removals
            Occurrences = occurrences
            Remintings =
                plan.AnnotationRemintings
                |> List.map (fun reminting -> reminting.AssignmentId, reminting)
                |> Map.ofList
            DetachedAnnotations = [
                for KeyValue(collection, slots) in detached do
                    for KeyValue(position, annotation) in slots do
                        yield collection, position, annotation
            ]
        }

let private canonicalPreflight
    (index: ProcessCoreWritebackIndex)
    (session: CanonicalProjectionTypes.ProvenanceSession)
    (arc: ARC)
    : Result<ResolvedCanonicalPlan, ProcessCoreWritebackError list> =
    let adapterErrors =
        validateCanonicalProjections session
        @ validateCanonicalGraph index arc
        @ validateCanonicalSources index session
        @ validateCanonicalAvailability index session
        |> List.distinct

    if not adapterErrors.IsEmpty then
        Error adapterErrors
    else
        // Planning fails closed on a malformed stored Recipe payload, so it runs
        // before any phase that dereferences one.
        let externalAnnotations = seq {
            let indexedSlots =
                Dictionary<ResizeArray<Annotation>, HashSet<int>>(HashIdentity.Reference)

            for KeyValue(_, locations) in index.AssignmentLocations do
                for location in locations do
                    match tryResolveCanonicalOccurrence arc location with
                    | None -> ()
                    | Some(_, collection) ->
                        match indexedSlots.TryGetValue collection with
                        | true, positions -> positions.Add location.Position |> ignore
                        | _ ->
                            let positions = HashSet<int>()
                            positions.Add location.Position |> ignore
                            indexedSlots.Add(collection, positions)

            // Exclude exact writable slots, not annotation references: an
            // unloaded owner can hold the same object as an edited loaded one.
            for slot in physicalAnnotationSlots arc do
                let indexed =
                    match indexedSlots.TryGetValue slot.Collection with
                    | true, positions -> positions.Contains slot.Position
                    | _ -> false

                if not indexed then
                    yield slot.Annotation
        }

        CanonicalPlan.tryCreateWithExternalAnnotations externalAnnotations index session
        |> Result.bind (fun plan -> resolveCanonicalPlan index session plan arc)

/// Materializes one planned annotation. The planned fingerprint is the
/// complete payload the plan settled on, including nested and overflow data
/// carried over from the indexed occurrence and any planned reminting.
let private canonicalAnnotationFromPlan (planned: CanonicalPlan.PlannedAnnotation) =
    ProcessCore.Yaml.Annotation.fromYamlString false planned.Fingerprint.Payload

let private applyCanonicalAnnotation
    (resolved: ResolvedCanonicalPlan)
    (planned: CanonicalPlan.PlannedAnnotation)
    (existing: Annotation)
    =
    if canonicalAnnotationFingerprint existing = planned.Fingerprint then
        false
    else
        let requested = canonicalAnnotationFromPlan planned
        existing.Name <- requested.Name
        existing.Value <- requested.Value
        existing.Unit <- requested.Unit
        existing.NameTAN <- requested.NameTAN
        existing.ValueTAN <- requested.ValueTAN
        existing.UnitTAN <- requested.UnitTAN
        existing.AdditionalType <- requested.AdditionalType

        match resolved.Remintings |> Map.tryFind planned.AssignmentId with
        | Some reminting -> existing.SetProperty("@id", reminting.PlannedRegistryId)
        | None -> ()

        true

/// Writes one owner's complete final annotation set: an indexed occurrence on
/// this owner is updated in place, an occurrence whose assignment no longer
/// belongs to this owner is removed, and an assignment without an occurrence
/// here is added.
let private reconcileCanonicalAnnotations
    (resolved: ResolvedCanonicalPlan)
    (ownedHere: ProcessCoreCanonicalAnnotationOwner -> bool)
    (annotations: ResizeArray<Annotation>)
    (final: CanonicalPlan.PlannedAnnotation list)
    =
    let owned =
        resolved.Occurrences
        |> Map.toList
        |> List.collect (fun (assignmentId, items) ->
            items
            |> List.filter (fun item -> ownedHere item.Owner)
            |> List.map (fun item -> assignmentId, item)
        )

    let finalIds = final |> List.map _.AssignmentId |> Set.ofList

    // A merged canonical owner may back its occurrences with several physical
    // objects, so each stale occurrence is removed from its own collection.
    for assignmentId, item in owned do
        if not (finalIds.Contains assignmentId) then
            removeAnnotationByReference item.Collection item.Annotation |> ignore

    let mutable updated = 0
    let mutable added = 0

    for planned in final do
        match
            owned
            |> List.filter (fun (assignmentId, _) -> assignmentId = planned.AssignmentId)
            |> List.map (fun (_, item) -> item.Annotation)
        with
        | [] ->
            annotations.Add(canonicalAnnotationFromPlan planned)
            added <- added + 1
        | existing ->
            for annotation in existing do
                if applyCanonicalAnnotation resolved planned annotation then
                    updated <- updated + 1

    updated, added

let private applyCanonicalRecipeAssociation
    (target: CanonicalProcessTarget)
    (association: CanonicalPlan.PlannedRecipeAssociation option)
    (proc: Process)
    =
    match association with
    | None ->
        // A clone starts without an inherited association, so "no planned
        // change" means the process genuinely holds no Recipe.
        match target with
        | CanonicalProcessTarget.Indexed _ -> ()
        | _ -> proc.ExecutesRecipe <- None
    | Some association ->
        match target, association.Change with
        | CanonicalProcessTarget.Indexed _, CanonicalPlan.RecipeAssociationChange.Keep -> ()
        | _, CanonicalPlan.RecipeAssociationChange.Clear -> proc.ExecutesRecipe <- None
        | _, _ ->
            // Assign the exact indexed resource; a Recipe label never decides
            // resolution and no resource is created, cloned, or edited.
            proc.ExecutesRecipe <- association.FinalResource |> Option.map _.Resource

let private applyCanonicalPlan (resolved: ResolvedCanonicalPlan) : ProcessCoreWritebackSummary =
    let mutable addedProcesses = 0
    let mutable removedProcesses = 0
    let mutable updatedAnnotations = 0
    let mutable addedAnnotations = 0

    // Detach before any removals shift collection positions. Reconciliation
    // then updates these private copies without changing an unedited owner.
    for collection, position, annotation in resolved.DetachedAnnotations do
        collection[position] <- annotation

    // Assignments the final session no longer holds at all. Per-owner
    // reconciliation below covers occurrences that only moved owner.
    for removal in resolved.Plan.AnnotationRemovals do
        match resolved.Occurrences |> Map.tryFind removal.AssignmentId with
        | None -> ()
        | Some items ->
            for item in items do
                removeAnnotationByReference item.Collection item.Annotation |> ignore

    // Structural apply, in the plan's validated destination order.
    let processTargets =
        resolved.Processes
        |> List.map (fun planned ->
            let proc =
                match planned.Target with
                | CanonicalProcessTarget.Indexed proc -> proc
                | CanonicalProcessTarget.Clone(source, destination) ->
                    // A shell only: the plan owns this partition's complete
                    // annotation set, so copying the original's annotation
                    // objects would make two partitions share them.
                    let clone =
                        Process(planned.Planned.ProcessName, ?additionalType = source.AdditionalType)

                    addProcess destination clone
                    addedProcesses <- addedProcesses + 1
                    clone
                | CanonicalProcessTarget.Created destination ->
                    let created = Process(planned.Planned.ProcessName)
                    addProcess destination created
                    addedProcesses <- addedProcesses + 1
                    created

            replaceProcessIO planned.Inputs planned.Outputs proc
            applyCanonicalRecipeAssociation planned.Target planned.Planned.RecipeAssociation proc
            planned, proc
        )

    for _, node in resolved.Nodes do
        let updated, added =
            reconcileCanonicalAnnotations
                resolved
                (function
                | ProcessCoreCanonicalAnnotationOwner.NodeAdditionalProperty location ->
                    node.ExistingLocations |> List.contains location
                | _ -> false)
                (nodeAnnotationCollection node.Node)
                node.Annotations

        updatedAnnotations <- updatedAnnotations + updated
        addedAnnotations <- addedAnnotations + added

    for planned, proc in processTargets do
        let ownedHere =
            match planned.Target with
            | CanonicalProcessTarget.Indexed _ ->
                (function
                | ProcessCoreCanonicalAnnotationOwner.ProcessParameterValue location ->
                    Some location = planned.Planned.IndexedProcess
                | _ -> false)
            // A clone or a new process owns no indexed occurrence: the indexed
            // occurrences of its original belong to whichever partition reuses
            // that process.
            | _ -> (fun _ -> false)

        let updated, added =
            reconcileCanonicalAnnotations resolved ownedHere proc.ParameterValue planned.Annotations

        updatedAnnotations <- updatedAnnotations + updated
        addedAnnotations <- addedAnnotations + added

    for dataset, proc in resolved.Removals do
        removeProcess dataset proc
        removedProcesses <- removedProcesses + 1

    {
        UpdatedAnnotations = updatedAnnotations
        AddedAnnotations = addedAnnotations
        AddedNodes = resolved.Nodes |> List.filter (fun (_, node) -> node.IsNewObject) |> List.length
        AddedProcesses = addedProcesses
        RemovedProcesses = removedProcesses
    }

/// Writes one canonical session covering every selected process group back to
/// the ARC it was loaded from. Preflight resolves and validates everything it
/// will touch, so a rejected save leaves the ARC and the session untouched and
/// an accepted one applies completely.
let prepareWriteBackMany
    (index: ProcessCoreWritebackIndex)
    (session: CanonicalProjectionTypes.ProvenanceSession)
    (arc: ARC)
    : Result<(ARC -> ProcessCoreWritebackSummary), ProcessCoreWritebackError list> =
    canonicalPreflight index session arc
    |> Result.map (fun resolved ->
        // Every mutation target is already bound to the ARC preflight ran
        // against; the parameter keeps the established apply-function shape.
        fun (_: ARC) -> applyCanonicalPlan resolved
    )

let writeBackMany
    (index: ProcessCoreWritebackIndex)
    (session: CanonicalProjectionTypes.ProvenanceSession)
    (arc: ARC)
    : Result<ProcessCoreWritebackSummary, ProcessCoreWritebackError list> =
    prepareWriteBackMany index session arc
    |> Result.map (fun mutation -> mutation arc)

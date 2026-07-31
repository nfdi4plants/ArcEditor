module Swate.Components.Page.ProvenanceGrouping.Projection

open System
open System.Globalization
open Swate.Components.Page.ProvenanceGrouping.Identifiers
open Swate.Components.Page.ProvenanceGrouping.Values
open Swate.Components.Page.ProvenanceGrouping.Domain
open Swate.Components.Page.ProvenanceGrouping.AvailabilityTypes
open Swate.Components.Page.ProvenanceGrouping.ProjectionTypes
open Swate.Components.Page.ProvenanceGrouping.MutationTypes
open Swate.Components.Page.ProvenanceGrouping.Availability

type CompositeGroupingKey =
    | GroupedValues of GroupingValueKey list
    | MissingValueForItem of itemId: string

type GroupedProjectedValue = {
    Key: GroupingValueKey
    Annotations: ProjectedAnnotation list
}

let toGroupingValueIdentity =
    function
    | ProvenanceValue.Text value -> TextIdentity value
    | ProvenanceValue.Integer value -> IntegerIdentity value
    | ProvenanceValue.Float value -> FloatIdentity value
    | ProvenanceValue.Term value -> TermIdentity value
    | ProvenanceValue.Reference value -> ReferenceIdentity(value.Scheme, value.Id)

let private encodeString (value: string) =
    value.Length.ToString(CultureInfo.InvariantCulture) + ":" + value

let private encodeOption encode =
    function
    | None -> "0:"
    | Some value -> "1:" + encode value

let private termSortKey (value: ProvenanceTerm) =
    String.concat "|" [
        encodeString value.Name
        encodeOption encodeString value.TermSource
        encodeOption encodeString value.TermAccession
    ]

let private valueSortKey =
    function
    | TextIdentity value -> "0|" + encodeString value
    | IntegerIdentity value -> "1|" + value.ToString("D11", CultureInfo.InvariantCulture)
    | FloatIdentity value ->
        let bits = BitConverter.DoubleToInt64Bits value
        "2|" + bits.ToString("X16", CultureInfo.InvariantCulture)
    | TermIdentity value -> "3|" + termSortKey value
    | ReferenceIdentity(scheme, id) -> String.concat "|" [ "4"; encodeString scheme; encodeString id ]

let private groupingKeySortKey =
    function
    | NodeValue(header, value, unit) ->
        String.concat "|" [
            "0"
            termSortKey header
            valueSortKey value
            encodeOption termSortKey unit
        ]
    | ProcessValue(header, value, unit, sourceId) ->
        String.concat "|" [
            "1"
            termSortKey header
            valueSortKey value
            encodeOption termSortKey unit
            encodeString sourceId
        ]

let normalizeGroupingKeys (keys: GroupingValueKey list) =
    keys
    |> List.groupBy groupingKeySortKey
    |> List.sortBy fst
    |> List.map (snd >> List.head)

let compositeGroupingKey itemId keys =
    match normalizeGroupingKeys keys with
    | [] -> MissingValueForItem itemId
    | normalized -> GroupedValues normalized

let private projectionIdentity propertyId assignmentId valueId propertyKind = {
    PropertyId = propertyId
    ValueId = valueId
    AssignmentId = assignmentId
    PropertyKind = propertyKind
}

let private availabilityEvidence (reference: AvailableAnnotationRef) = {
    Relation = reference.Relation
    OriginatingLinkIds = reference.OriginatingLinkIds
    VisibleThroughLinkIds = reference.VisibleThroughLinkIds
}

let private valueAndProperty valueId (session: ProvenanceSession) =
    match session.Values |> Map.tryFind valueId with
    | None -> Error(ValueNotFound valueId)
    | Some definition ->
        match session.Properties |> Map.tryFind definition.PropertyId with
        | None -> Error(PropertyNotFound definition.PropertyId)
        | Some property -> Ok(definition, property)

let projectAnnotation
    (reference: AvailableAnnotationRef)
    (session: ProvenanceSession)
    : Result<ProjectedAnnotation, ProvenanceCommandError> =
    match reference.Owner with
    | NodeOwner nodeId ->
        match session.Nodes |> Map.tryFind nodeId with
        | None -> Error(NodeNotFound nodeId)
        | Some node ->
            match node.Assignments |> Map.tryFind reference.AssignmentId with
            | None -> Error(AssignmentNotFound(Some(NodeAssignmentOwner nodeId), reference.AssignmentId))
            | Some assignment ->
                valueAndProperty assignment.ValueId session
                |> Result.map (fun (definition, property) ->
                    let identity =
                        projectionIdentity property.Id assignment.Id assignment.ValueId assignment.PropertyKind

                    {
                        Key = NodeValue(property.Category, toGroupingValueIdentity definition.Value, definition.Unit)
                        Backing = NodeAssignmentBacking(identity, nodeId, assignment.TargetSource)
                        Availability = availabilityEvidence reference
                        OriginSource = assignment.TargetSource
                    }
                )
    | ProcessOwner processId ->
        match session.Processes |> Map.tryFind processId with
        | None -> Error(ProcessNotFound processId)
        | Some structuralProcess ->
            match structuralProcess.Assignments |> Map.tryFind reference.AssignmentId with
            | None -> Error(AssignmentNotFound(Some(ProcessAssignmentOwner processId), reference.AssignmentId))
            | Some assignment ->
                match session.Layers |> Map.tryFind structuralProcess.OriginLayerId with
                | None -> Error(LayerNotFound structuralProcess.OriginLayerId)
                | Some layer ->
                    valueAndProperty assignment.ValueId session
                    |> Result.map (fun (definition, property) ->
                        let identity =
                            projectionIdentity property.Id assignment.Id assignment.ValueId assignment.PropertyKind

                        {
                            Key =
                                ProcessValue(
                                    property.Category,
                                    toGroupingValueIdentity definition.Value,
                                    definition.Unit,
                                    layer.Source.Id
                                )
                            Backing =
                                ProcessAssignmentBacking(
                                    identity,
                                    processId,
                                    assignment.CoveredLinkIds,
                                    assignment.ContainerReferenceValueId,
                                    assignment.ReferenceSlotId
                                )
                            Availability = availabilityEvidence reference
                            OriginSource = Some layer.Source
                        }
                    )

let projectAnnotations references session =
    let folder state reference =
        state
        |> Result.bind (fun annotations ->
            projectAnnotation reference session
            |> Result.map (fun annotation -> annotation :: annotations)
        )

    references |> List.fold folder (Ok []) |> Result.map List.rev

let groupProjectedAnnotations (annotations: ProjectedAnnotation list) : GroupedProjectedValue list =
    annotations
    |> List.groupBy _.Key
    |> List.sortBy (fst >> groupingKeySortKey)
    |> List.map (fun (key, backing) -> { Key = key; Annotations = backing })

let availableReferenceOfAnnotation (annotation: ProjectedAnnotation) =
    let assignmentId, valueId, owner =
        match annotation.Backing with
        | NodeAssignmentBacking(identity, ownerId, _) -> identity.AssignmentId, identity.ValueId, NodeOwner ownerId
        | ProcessAssignmentBacking(identity, ownerId, _, _, _) ->
            identity.AssignmentId, identity.ValueId, ProcessOwner ownerId

    {
        AssignmentId = assignmentId
        ValueId = valueId
        Owner = owner
        Relation = annotation.Availability.Relation
        OriginatingLinkIds = annotation.Availability.OriginatingLinkIds
        VisibleThroughLinkIds = annotation.Availability.VisibleThroughLinkIds
    }

let availableReferencesForConnector (connector: DisplayConnector) =
    connector.Annotations
    |> List.map availableReferenceOfAnnotation
    |> List.filter (fun reference ->
        match reference.Owner with
        | ProcessOwner _ -> not (Set.intersect reference.OriginatingLinkIds connector.LinkIds).IsEmpty
        | NodeOwner _ -> false
    )

let isConnectorEditAmbiguous connector =
    availableReferencesForConnector connector
    |> List.sumBy (fun reference -> Set.intersect reference.OriginatingLinkIds connector.LinkIds |> Set.count)
    <> 1

let availableReferenceForShelfEntry entry =
    match entry.Payload with
    | CatalogBacked _ -> None
    | AssignmentBacked payload ->
        let identity, owner =
            match payload.Backing with
            | NodeAssignmentBacking(identity, ownerId, _) -> identity, NodeOwner ownerId
            | ProcessAssignmentBacking(identity, ownerId, _, _, _) -> identity, ProcessOwner ownerId

        Some {
            AssignmentId = identity.AssignmentId
            ValueId = identity.ValueId
            Owner = owner
            Relation = payload.Availability.Relation
            OriginatingLinkIds = payload.Availability.OriginatingLinkIds
            VisibleThroughLinkIds = payload.Availability.VisibleThroughLinkIds
        }

type private EndpointProjection = {
    Endpoint: LayerEndpoint
    Annotations: ProjectedAnnotation list
}

let private projectEndpoint session (endpoint: LayerEndpoint) =
    resolveNodeAvailability endpoint.Key.NodeId session
    |> Result.bind (fun references -> projectAnnotations references session)
    |> Result.map (fun annotations -> {
        Endpoint = endpoint
        Annotations = annotations
    })

/// The item-specific fallback key an endpoint keeps when it has no value for any
/// active grouping header (intent §7). It is per endpoint, so unrelated missing
/// items never collapse into one group.
let private endpointItemId (key: LayerEndpointKey) =
    String.concat ":" [ key.LayerId; string key.Side; key.NodeId ]

/// Which grouping values form a card's key. Grouping is driven by the side's
/// active grouping headers, so this is supplied per side by the caller rather
/// than derived from the annotations themselves: `Projection.fs` compiles before
/// `Types.fs`, where `AnnotationHeaderKey` lives, so the selection arrives as a
/// predicate over the grouping-value key instead of as a header set.
type ActiveGroupingKeys = ProvenanceSide -> GroupingValueKey -> bool

/// No header is active, so every item keeps its own fallback key and its own
/// card. This is what the cached layer projection is built with: the session
/// cannot see UI state, so it stores the finest partition and the UI coarsens it
/// (intent §6 - "regrouping a display group reconstructs the view").
let noActiveGroupingKeys: ActiveGroupingKeys = fun _ _ -> false

let private collectResults results =
    let folder state result =
        state
        |> Result.bind (fun collected -> result |> Result.map (fun value -> value :: collected))

    results |> List.fold folder (Ok []) |> Result.map List.rev

let private processLinks (session: ProvenanceSession) =
    session.Processes
    |> Map.toList
    |> List.collect (fun (processId, structuralProcess) ->
        structuralProcess.Links
        |> Map.toList
        |> List.map (fun (_, processLink) -> processId, processLink)
    )

let private groupEndpoints
    layerId
    (isActive: ActiveGroupingKeys)
    (session: ProvenanceSession)
    (items: (LayerEndpointKey * ProjectedAnnotation list) list)
    : DisplayGroup list =
    let linkIdsForNodes nodeIds =
        processLinks session
        |> List.choose (fun (_, processLink) ->
            let isIncident =
                match processLink.Shape with
                | ProcessLinkShape.Between(inputId, outputId) ->
                    nodeIds |> Set.contains inputId || nodeIds |> Set.contains outputId
                | ProcessLinkShape.InputOnly inputId -> nodeIds |> Set.contains inputId
                | ProcessLinkShape.OutputOnly outputId -> nodeIds |> Set.contains outputId
                | ProcessLinkShape.Endpointless -> false

            if isIncident then Some processLink.Id else None
        )
        |> Set.ofList

    items
    |> List.map (fun (endpointKey, annotations) ->
        let compositeKey =
            annotations
            |> List.map _.Key
            |> List.filter (isActive endpointKey.Side)
            |> compositeGroupingKey (endpointItemId endpointKey)

        endpointKey, annotations, compositeKey
    )
    |> List.groupBy (fun (endpointKey, _, compositeKey) -> endpointKey.Side, compositeKey)
    |> List.sortBy fst
    |> List.mapi (fun index ((side, compositeKey), members) ->
        let endpointKeys = members |> List.map (fun (key, _, _) -> key) |> Set.ofList
        let nodeIds = endpointKeys |> Set.map _.NodeId

        {
            Id = $"group:{layerId}:{side}:{index + 1}"
            Side = side
            GroupingValues =
                match compositeKey with
                | MissingValueForItem _ -> []
                | GroupedValues values -> values
            CanonicalNodeIds = nodeIds
            EndpointKeys = endpointKeys
            ProcessLinkIds = linkIdsForNodes nodeIds
            Annotations =
                members
                |> List.collect (fun (_, annotations, _) -> annotations)
                |> List.distinct
        }
    )

let private connectorAnnotations processId linkId (session: ProvenanceSession) =
    let structuralProcess = session.Processes[processId]

    structuralProcess.Assignments
    |> Map.toList
    |> List.choose (fun (_, assignment) ->
        if assignment.CoveredLinkIds |> Set.contains linkId then
            Some {
                AssignmentId = assignment.Id
                ValueId = assignment.ValueId
                Owner = ProcessOwner processId
                Relation = IncidentProcess linkId
                OriginatingLinkIds = Set.singleton linkId
                VisibleThroughLinkIds = Set.singleton linkId
            }
        else
            None
    )
    |> fun references -> projectAnnotations references session

let private projectConnectors
    (layer: ProvenanceLayer)
    (session: ProvenanceSession)
    (groups: DisplayGroup list)
    : Result<DisplayConnector list, ProvenanceCommandError> =
    let groupByEndpoint =
        groups
        |> List.collect (fun group ->
            group.EndpointKeys
            |> Set.toList
            |> List.map (fun endpointKey -> (endpointKey.Side, endpointKey.NodeId), group)
        )
        |> Map.ofList

    let candidates =
        layer.StructuralProcessIds
        |> Set.toList
        |> List.collect (fun processId ->
            session.Processes
            |> Map.tryFind processId
            |> Option.map (fun structuralProcess ->
                structuralProcess.Links
                |> Map.toList
                |> List.choose (fun (_, processLink) ->
                    match processLink.Shape with
                    | ProcessLinkShape.Between(inputId, outputId) ->
                        match
                            groupByEndpoint |> Map.tryFind (ProvenanceSide.Input, inputId),
                            groupByEndpoint |> Map.tryFind (ProvenanceSide.Output, outputId)
                        with
                        | Some inputGroup, Some outputGroup ->
                            Some(inputGroup, outputGroup, processId, processLink.Id)
                        | _ -> None
                    | ProcessLinkShape.InputOnly _
                    | ProcessLinkShape.OutputOnly _
                    | ProcessLinkShape.Endpointless -> None
                )
            )
            |> Option.defaultValue []
        )

    candidates
    |> List.groupBy (fun (inputGroup, outputGroup, _, _) -> inputGroup.Id, outputGroup.Id)
    |> List.sortBy fst
    |> List.mapi (fun index ((inputGroupId, outputGroupId), links) ->
        let annotationResults =
            links
            |> List.map (fun (_, _, processId, linkId) -> connectorAnnotations processId linkId session)

        collectResults annotationResults
        |> Result.map (fun annotations ->
            let inputGroups = links |> List.map (fun (inputGroup, _, _, _) -> inputGroup)
            let outputGroups = links |> List.map (fun (_, outputGroup, _, _) -> outputGroup)

            {
                Id = $"connector:{layer.Id}:{index + 1}"
                InputGroupId = inputGroupId
                OutputGroupId = outputGroupId
                StructuralProcessIds = links |> List.map (fun (_, _, processId, _) -> processId) |> Set.ofList
                LinkIds = links |> List.map (fun (_, _, _, linkId) -> linkId) |> Set.ofList
                InputEndpointKeys = inputGroups |> List.collect (_.EndpointKeys >> Set.toList) |> Set.ofList
                OutputEndpointKeys = outputGroups |> List.collect (_.EndpointKeys >> Set.toList) |> Set.ofList
                Annotations = annotations |> List.concat
            }
        )
    )
    |> collectResults

let private projectProcessOnlyEntries
    (layer: ProvenanceLayer)
    (session: ProvenanceSession)
    : Result<ProcessOnlyEntry list, ProvenanceCommandError> =
    layer.StructuralProcessIds
    |> Set.toList
    |> List.collect (fun processId ->
        session.Processes
        |> Map.tryFind processId
        |> Option.map (fun structuralProcess ->
            structuralProcess.Links
            |> Map.toList
            |> List.choose (fun (_, processLink) ->
                if processLink.Shape = ProcessLinkShape.Endpointless then
                    Some(processId, processLink.Id)
                else
                    None
            )
        )
        |> Option.defaultValue []
    )
    |> List.map (fun (processId, linkId) ->
        connectorAnnotations processId linkId session
        |> Result.map (fun annotations ->
            if annotations.IsEmpty then
                None
            else
                Some {
                    StructuralProcessId = processId
                    LinkId = linkId
                    Annotations = annotations
                }
        )
    )
    |> collectResults
    |> Result.map (List.choose id)

let private assignmentShelfEntries layerId (endpointProjections: EndpointProjection list) : PropertyShelfEntry list =
    endpointProjections
    |> List.collect (fun endpointProjection ->
        endpointProjection.Annotations
        |> List.choose (fun annotation ->
            match annotation.Backing with
            | NodeAssignmentBacking(identity, ownerId, _) ->
                Some(endpointProjection.Endpoint, annotation, identity.AssignmentId, ownerId)
            | ProcessAssignmentBacking _ -> None
        )
    )
    |> List.groupBy (fun (endpoint, annotation, assignmentId, ownerId) ->
        endpoint.Key.NodeId, assignmentId, ownerId, annotation.Availability.Relation
    )
    |> List.sortBy fst
    |> List.mapi (fun index (_, entries) ->
        let endpoints = entries |> List.map (fun (endpoint, _, _, _) -> endpoint)

        let annotation =
            entries |> List.map (fun (_, annotation, _, _) -> annotation) |> List.head

        {
            Id = $"shelf:{layerId}:assignment:{index + 1}"
            Payload =
                AssignmentBacked {
                    Backing = annotation.Backing
                    Availability = annotation.Availability
                    CanonicalNodeIds = endpoints |> List.map _.Key.NodeId |> Set.ofList
                    EndpointKeys = endpoints |> List.map _.Key |> Set.ofList
                }
        }
    )

let private catalogShelfEntries layerId (catalog: ReferenceCatalog) : PropertyShelfEntry list =
    catalog
    |> Map.toList
    |> List.map snd
    |> List.mapi (fun index entry -> {
        Id = $"shelf:{layerId}:catalog:{index + 1}"
        Payload = CatalogBacked { Entry = entry }
    })

// ── Origin sources ──────────────────────────────────────────────────────────
//
// Design §3.5: colour is resolved in the UI layer, but "the canonical projection
// supplies each item's backing references and origin sources". These are that
// supply side - pure over the session, with no UiState - and the resolver over
// them lives in `State.PropertyColors`.
//
// A node does not belong to a layer; it *appears* in layers. So a node
// annotation's origin sources are the sources of every layer its owning node
// appears in, which means every annotation an owner holds shares that set.

let private appearanceSourceIds nodeId (session: ProvenanceSession) =
    session.Layers
    |> Map.toSeq
    |> Seq.choose (fun (_, layer) ->
        if
            layer.InputEndpoints.ContainsKey nodeId
            || layer.OutputEndpoints.ContainsKey nodeId
        then
            Some layer.Source.Id
        else
            None
    )
    |> Set.ofSeq

/// A process assignment stores no origin either: it derives its layer, and so
/// its source, from the structural process that owns it.
let private processSourceIds processId (session: ProvenanceSession) =
    session.Processes
    |> Map.tryFind processId
    |> Option.bind (fun structuralProcess -> session.Layers |> Map.tryFind structuralProcess.OriginLayerId)
    |> Option.map (fun layer -> Set.singleton layer.Source.Id)
    |> Option.defaultValue Set.empty

let originSourceIdsForAnnotation (session: ProvenanceSession) (annotation: ProjectedAnnotation) =
    match annotation.Backing with
    | NodeAssignmentBacking(_, ownerId, _) -> appearanceSourceIds ownerId session
    | ProcessAssignmentBacking(_, ownerId, _, _, _) -> processSourceIds ownerId session

/// A grouping chip aggregates several assignments, so it takes the union of its
/// members' origin sources and may therefore differ between layers when the
/// aggregated set differs.
let originSourceIdsForGroupingValue
    (session: ProvenanceSession)
    (key: GroupingValueKey)
    (annotations: ProjectedAnnotation list)
    =
    annotations
    |> List.filter (fun annotation -> annotation.Key = key)
    |> List.fold
        (fun sourceIds annotation -> Set.union sourceIds (originSourceIdsForAnnotation session annotation))
        Set.empty

let originSourceIdsForShelfEntry (session: ProvenanceSession) (entry: PropertyShelfEntry) =
    match entry.Payload with
    | CatalogBacked _ -> Set.empty
    | AssignmentBacked payload ->
        match payload.Backing with
        | NodeAssignmentBacking(_, ownerId, _) -> appearanceSourceIds ownerId session
        | ProcessAssignmentBacking(_, ownerId, _, _, _) -> processSourceIds ownerId session

let projectLayer
    (layerId: ProvenanceLayerId)
    (catalog: ReferenceCatalog)
    (session: ProvenanceSession)
    : Result<CachedLayerProjection, ProvenanceCommandError> =
    match session.Layers |> Map.tryFind layerId with
    | None -> Error(LayerNotFound layerId)
    | Some layer ->
        let endpoints =
            [
                layer.InputEndpoints |> Map.toList |> List.map snd
                layer.OutputEndpoints |> Map.toList |> List.map snd
            ]
            |> List.concat
            |> List.sortBy (fun endpoint -> endpoint.Key.Side, endpoint.LayerOrderPosition, endpoint.Key.NodeId)

        endpoints
        |> List.map (projectEndpoint session)
        |> collectResults
        |> Result.bind (fun endpointProjections ->
            let groups =
                endpointProjections
                |> List.map (fun projection -> projection.Endpoint.Key, projection.Annotations)
                |> groupEndpoints layerId noActiveGroupingKeys session

            projectConnectors layer session groups
            |> Result.bind (fun connectors ->
                projectProcessOnlyEntries layer session
                |> Result.map (fun processOnlyEntries -> {
                    TopologyRevision = session.AvailabilityTopologyRevision
                    ValueRevision = session.AnnotationValueRevision
                    Stale = false
                    Groups = groups
                    Connectors = connectors
                    ProcessOnlyEntries = processOnlyEntries
                    ShelfEntries =
                        assignmentShelfEntries layerId endpointProjections
                        @ catalogShelfEntries layerId catalog
                })
            )
        )

/// Re-derives the cards and connectors of an already-projected layer for a
/// grouping selection (intent §6). The cached projection holds the finest
/// partition - `projectLayer` builds it with `noActiveGroupingKeys`, so every
/// group there is exactly one endpoint - and this coarsens it by the active
/// grouping headers without re-resolving availability. Connectors are re-pooled
/// from the resulting cards, because a connector is the edge between two cards.
let regroupLayer
    (isActive: ActiveGroupingKeys)
    (layer: ProvenanceLayer)
    (session: ProvenanceSession)
    (projection: CachedLayerProjection)
    : Result<DisplayGroup list * DisplayConnector list, ProvenanceCommandError> =
    let items =
        projection.Groups
        |> List.collect (fun group ->
            group.EndpointKeys
            |> Set.toList
            |> List.map (fun endpointKey -> endpointKey, group.Annotations)
        )

    let groups = groupEndpoints layer.Id isActive session items

    projectConnectors layer session groups
    |> Result.map (fun connectors -> groups, connectors)

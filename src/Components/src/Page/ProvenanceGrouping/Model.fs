module Swate.Components.Page.ProvenanceGrouping.Model

open System
open System.Globalization
open Swate.Components.Page.ProvenanceGrouping.Identifiers
open Swate.Components.Page.ProvenanceGrouping.Values
open Swate.Components.Page.ProvenanceGrouping.Domain
open Swate.Components.Page.ProvenanceGrouping.MutationTypes
open Swate.Components.Page.ProvenanceGrouping.ProjectionTypes

type IncidentLinks = {
    IncomingLinkIds: ProcessLinkId list
    OutgoingLinkIds: ProcessLinkId list
    OneSidedLinkIds: ProcessLinkId list
}

/// Definitions prepared for installation by the same command that creates an
/// assignment. Keeping this as a candidate avoids committing sidebar drafts or
/// otherwise exposing a successful session state containing a new orphan.
type ValueDefinitionPreparation = {
    PropertyDefinition: PropertyDefinition
    ValueDefinition: PropertyValueDefinition
}

[<RequireQualifiedAccess>]
type private AssignmentOccurrenceKey =
    | Node of CanonicalNodeId * AnnotationAssignmentId
    | Process of StructuralProcessId * AnnotationAssignmentId

type private AssignmentOccurrence = {
    Key: AssignmentOccurrenceKey
    StoredKey: AnnotationAssignmentId
    EmbeddedId: AnnotationAssignmentId
    ValueId: PropertyValueDefinitionId
    CoveredLinkIds: Set<ProcessLinkId>
    ContainerReferenceValueId: PropertyValueDefinitionId option
    IntrinsicallyValid: bool
}

let empty: ProvenanceSession = {
    Nodes = Map.empty
    Processes = Map.empty
    Properties = Map.empty
    Values = Map.empty
    Layers = Map.empty
    LayerOrder = []
    ActiveLayerId = ""
    AvailabilityTopologyRevision = 0
    AnnotationValueRevision = 0
    ReachabilityMemo = Map.empty
    LayerProjections = Map.empty
    MutationJournal = []
}

let canonicalKey (kind: ProvenanceKind) (name: string) : CanonicalNodeKey = { KindId = kind.Id; Name = name }

let private nextNodeId (session: ProvenanceSession) : CanonicalNodeId =
    Seq.initInfinite (fun index -> $"canonical-node-{index + 1}")
    |> Seq.find (fun candidate -> session.Nodes |> Map.containsKey candidate |> not)

let ensureNode
    (kind: ProvenanceKind)
    (name: string)
    (session: ProvenanceSession)
    : CanonicalNodeId * ProvenanceSession =
    let key = canonicalKey kind name

    session.Nodes
    |> Map.tryPick (fun nodeId node -> if node.Key = key then Some nodeId else None)
    |> function
        | Some nodeId -> nodeId, session
        | None ->
            let nodeId = nextNodeId session

            let node = {
                Id = nodeId
                Key = key
                Kind = kind
                Name = name
                Assignments = Map.empty
            }

            nodeId,
            {
                session with
                    Nodes = session.Nodes |> Map.add nodeId node
            }

let private encodeString (value: string) =
    value
    |> Seq.map (fun character -> (int character).ToString("X4", CultureInfo.InvariantCulture))
    |> String.concat ""

let private encodeOption encoder =
    function
    | None -> "n"
    | Some value -> "s" + encoder value

let private termIdentity (term: ProvenanceTerm) =
    String.concat "-" [
        encodeString term.Name
        encodeOption encodeString term.TermSource
        encodeOption encodeString term.TermAccession
    ]

let private floatSemanticIdentity value =
    if Double.IsNaN value then "nan"
    elif value = 0.0 then "zero"
    else value.ToString("R", CultureInfo.InvariantCulture)

let private valueSemanticIdentity =
    function
    | ProvenanceValue.Text value -> "text-" + encodeString value
    | ProvenanceValue.Integer value -> "integer-" + string value
    | ProvenanceValue.Float value -> "float-" + floatSemanticIdentity value
    | ProvenanceValue.Term value -> "term-" + termIdentity value
    | ProvenanceValue.Reference value ->
        // Reference labels are display-only; exact reference identity is scheme + ID.
        "reference-" + encodeString value.Scheme + "-" + encodeString value.Id

let private equivalentValue left right =
    valueSemanticIdentity left = valueSemanticIdentity right

let private firstUnusedId baseId existing =
    Seq.initInfinite (fun index -> if index = 0 then baseId else $"{baseId}-{index + 1}")
    |> Seq.find (fun candidate -> existing |> Map.containsKey candidate |> not)

let private preparePropertyDefinition category (session: ProvenanceSession) =
    session.Properties
    |> Map.toSeq
    |> Seq.tryPick (fun (_, property) -> if property.Category = category then Some property else None)
    |> Option.defaultWith (fun () ->
        let baseId = "property-definition-" + termIdentity category

        {
            Id = firstUnusedId baseId session.Properties
            Category = category
        }
    )

/// Reuses a structurally equal category/value/unit definition when present, or
/// returns a deterministic candidate for atomic installation with an assignment.
let ensureValueDefinition
    (category: ProvenanceTerm)
    (value: ProvenanceValue)
    (unit: ProvenanceTerm option)
    (session: ProvenanceSession)
    : ValueDefinitionPreparation =
    let existing =
        session.Values
        |> Map.toSeq
        |> Seq.tryPick (fun (_, definition) ->
            session.Properties
            |> Map.tryFind definition.PropertyId
            |> Option.bind (fun property ->
                if
                    property.Category = category
                    && equivalentValue definition.Value value
                    && definition.Unit = unit
                then
                    Some {
                        PropertyDefinition = property
                        ValueDefinition = definition
                    }
                else
                    None
            )
        )

    existing
    |> Option.defaultWith (fun () ->
        let property = preparePropertyDefinition category session

        let baseId =
            String.concat "-" [
                "property-value-definition"
                termIdentity category
                valueSemanticIdentity value
                encodeOption termIdentity unit
            ]

        {
            PropertyDefinition = property
            ValueDefinition = {
                Id = firstUnusedId baseId session.Values
                PropertyId = property.Id
                Value = value
                Unit = unit
            }
        }
    )

let referenceCatalogIdentity (entry: ReferenceCatalogEntry) =
    entry.Reference.Scheme, entry.Reference.Id

let normalizeCatalog (entries: ReferenceCatalogEntry list) : ReferenceCatalog =
    entries
    |> List.groupBy referenceCatalogIdentity
    |> List.map (fun (identity, duplicates) ->
        // A.1 exposes no catalog-conflict error, so an exact duplicate identity
        // deterministically keeps the structural minimum over the complete entry.
        // The display label participates in winner selection, never in identity.
        identity, duplicates |> List.min
    )
    |> Map.ofList

let catalogEntries (catalog: ReferenceCatalog) = catalog |> Map.toList |> List.map snd

let tryFindCatalogEntry scheme id (catalog: ReferenceCatalog) = catalog |> Map.tryFind (scheme, id)

/// Prepares catalog promotion without mutating either catalog or session. A
/// command can install the returned definitions and its first assignment atomically.
let promoteCatalogEntry (entry: ReferenceCatalogEntry) (session: ProvenanceSession) =
    ensureValueDefinition entry.Category (ProvenanceValue.Reference entry.Reference) entry.Unit session

let valueDefinitionReferenceCounts (session: ProvenanceSession) : Map<PropertyValueDefinitionId, int> =
    let increment valueId counts =
        counts
        |> Map.change valueId (fun current -> current |> Option.defaultValue 0 |> (+) 1 |> Some)

    let afterNodes =
        session.Nodes
        |> Map.fold
            (fun counts _ node ->
                node.Assignments
                |> Map.fold (fun counts _ assignment -> increment assignment.ValueId counts) counts
            )
            Map.empty

    session.Processes
    |> Map.fold
        (fun counts _ structuralProcess ->
            structuralProcess.Assignments
            |> Map.fold
                (fun counts _ assignment ->
                    let counts = increment assignment.ValueId counts

                    assignment.ContainerReferenceValueId
                    |> Option.map (fun valueId -> increment valueId counts)
                    |> Option.defaultValue counts
                )
                counts
        )
        afterNodes

let orphanValueDefinitionIds (session: ProvenanceSession) =
    let counts = valueDefinitionReferenceCounts session

    session.Values
    |> Map.toSeq
    |> Seq.choose (fun (valueId, _) ->
        if counts |> Map.tryFind valueId |> Option.defaultValue 0 = 0 then
            Some valueId
        else
            None
    )
    |> Set.ofSeq

let orphanAssignmentIds (session: ProvenanceSession) =
    let nodeOccurrences =
        session.Nodes
        |> Map.toSeq
        |> Seq.collect (fun (_, node) ->
            node.Assignments
            |> Map.toSeq
            |> Seq.map (fun (storedKey, assignment) -> {
                Key = AssignmentOccurrenceKey.Node(node.Id, storedKey)
                StoredKey = storedKey
                EmbeddedId = assignment.Id
                ValueId = assignment.ValueId
                CoveredLinkIds = Set.empty
                ContainerReferenceValueId = None
                IntrinsicallyValid =
                    storedKey = assignment.Id
                    && session.Values |> Map.containsKey assignment.ValueId
            })
        )
        |> Seq.toList

    let processOccurrences =
        session.Processes
        |> Map.toSeq
        |> Seq.collect (fun (_, structuralProcess) ->
            structuralProcess.Assignments
            |> Map.toSeq
            |> Seq.map (fun (storedKey, assignment) ->
                let hasTypedContainer =
                    match assignment.ContainerReferenceValueId with
                    | None -> true
                    | Some containerValueId ->
                        match session.Values |> Map.tryFind containerValueId with
                        | Some { Value = ProvenanceValue.Reference _ } -> true
                        | _ -> false

                let hasOnlyOwnedCoverage =
                    assignment.CoveredLinkIds
                    |> Seq.forall (fun linkId -> structuralProcess.Links |> Map.containsKey linkId)

                {
                    Key = AssignmentOccurrenceKey.Process(structuralProcess.Id, storedKey)
                    StoredKey = storedKey
                    EmbeddedId = assignment.Id
                    ValueId = assignment.ValueId
                    CoveredLinkIds = assignment.CoveredLinkIds
                    ContainerReferenceValueId = assignment.ContainerReferenceValueId
                    IntrinsicallyValid =
                        storedKey = assignment.Id
                        && session.Values |> Map.containsKey assignment.ValueId
                        && not assignment.CoveredLinkIds.IsEmpty
                        && hasOnlyOwnedCoverage
                        && hasTypedContainer
                }
            )
        )
        |> Seq.toList

    let occurrences = nodeOccurrences @ processOccurrences

    let duplicateEmbeddedIds =
        occurrences
        |> Seq.countBy _.EmbeddedId
        |> Seq.choose (fun (assignmentId, count) -> if count > 1 then Some assignmentId else None)
        |> Set.ofSeq

    let eligible occurrence =
        occurrence.IntrinsicallyValid
        && duplicateEmbeddedIds |> Set.contains occurrence.EmbeddedId |> not

    let initialValid =
        occurrences
        |> Seq.choose (fun occurrence ->
            if
                eligible occurrence
                && (
                    match occurrence.Key with
                    | AssignmentOccurrenceKey.Node _ -> true
                    | AssignmentOccurrenceKey.Process _ -> occurrence.ContainerReferenceValueId.IsNone
                )
            then
                Some occurrence.Key
            else
                None
        )
        |> Set.ofSeq

    let containerCount valid processId containerValueId linkId =
        processOccurrences
        |> Seq.filter (fun candidate ->
            match candidate.Key with
            | AssignmentOccurrenceKey.Process(candidateProcessId, _) ->
                candidateProcessId = processId
                && valid |> Set.contains candidate.Key
                && candidate.ValueId = containerValueId
                && candidate.CoveredLinkIds |> Set.contains linkId
            | AssignmentOccurrenceKey.Node _ -> false
        )
        |> Seq.length

    let rec closeReachableContainerDependencies reachable =
        let newlyReachable =
            processOccurrences
            |> Seq.choose (fun occurrence ->
                match occurrence.Key, occurrence.ContainerReferenceValueId with
                | AssignmentOccurrenceKey.Process(processId, _), Some containerValueId when
                    eligible occurrence && reachable |> Set.contains occurrence.Key |> not
                    ->
                    let hasReachableContainerPerLink =
                        occurrence.CoveredLinkIds
                        |> Seq.forall (fun linkId -> containerCount reachable processId containerValueId linkId >= 1)

                    if hasReachableContainerPerLink then
                        Some occurrence.Key
                    else
                        None
                | _ -> None
            )
            |> Set.ofSeq

        let next = Set.union reachable newlyReachable

        if next = reachable then
            reachable
        else
            closeReachableContainerDependencies next

    let reachable = closeReachableContainerDependencies initialValid

    let rec pruneInvalidContainerDependencies valid =
        let invalidDependents =
            processOccurrences
            |> Seq.choose (fun occurrence ->
                match occurrence.Key, occurrence.ContainerReferenceValueId with
                | AssignmentOccurrenceKey.Process(processId, _), Some containerValueId when
                    valid |> Set.contains occurrence.Key
                    ->
                    let hasExactlyOneValidContainerPerLink =
                        occurrence.CoveredLinkIds
                        |> Seq.forall (fun linkId -> containerCount valid processId containerValueId linkId = 1)

                    if hasExactlyOneValidContainerPerLink then
                        None
                    else
                        Some occurrence.Key
                | _ -> None
            )
            |> Set.ofSeq

        let next = Set.difference valid invalidDependents

        if next = valid then
            valid
        else
            pruneInvalidContainerDependencies next

    // Reachability rejects unsupported cycles. Pruning then removes ambiguity
    // and propagates invalid backing conservatively until every survivor has
    // exactly one final, non-orphan container assignment per covered link.
    let valid = pruneInvalidContainerDependencies reachable

    let invalidStoredKeys =
        occurrences
        |> Seq.choose (fun occurrence ->
            if valid |> Set.contains occurrence.Key then
                None
            else
                Some occurrence.StoredKey
        )
        |> Set.ofSeq

    // Stored keys let cleanup remove malformed map entries. A globally duplicate
    // embedded ID is also returned so cleanup by ID removes every occurrence.
    Set.union invalidStoredKeys duplicateEmbeddedIds

let addLayerEndpoint
    (endpoint: LayerEndpoint)
    (session: ProvenanceSession)
    : Result<ProvenanceSession, ProvenanceCommandError> =
    match session.Layers |> Map.tryFind endpoint.Key.LayerId with
    | None -> Error(LayerNotFound endpoint.Key.LayerId)
    | Some _ when session.Nodes |> Map.containsKey endpoint.Key.NodeId |> not -> Error(NodeNotFound endpoint.Key.NodeId)
    | Some layer ->
        let endpoints =
            match endpoint.Key.Side with
            | ProvenanceSide.Input -> layer.InputEndpoints
            | ProvenanceSide.Output -> layer.OutputEndpoints

        if endpoints |> Map.containsKey endpoint.Key.NodeId then
            Error(DuplicateEndpointAppearance endpoint.Key)
        else
            let updatedLayer =
                match endpoint.Key.Side with
                | ProvenanceSide.Input -> {
                    layer with
                        InputEndpoints = endpoints |> Map.add endpoint.Key.NodeId endpoint
                  }
                | ProvenanceSide.Output -> {
                    layer with
                        OutputEndpoints = endpoints |> Map.add endpoint.Key.NodeId endpoint
                  }

            Ok {
                session with
                    Layers = session.Layers |> Map.add layer.Id updatedLayer
            }

let private shapeNodeIds =
    function
    | ProcessLinkShape.Between(input, output) -> [ input; output ]
    | ProcessLinkShape.InputOnly input -> [ input ]
    | ProcessLinkShape.OutputOnly output -> [ output ]
    | ProcessLinkShape.Endpointless -> []

let private validateLinkNodes (session: ProvenanceSession) (link: ProcessLink) =
    link.Shape
    |> shapeNodeIds
    |> List.tryFind (fun nodeId -> session.Nodes |> Map.containsKey nodeId |> not)
    |> function
        | Some nodeId -> Error(NodeNotFound nodeId)
        | None -> Ok()

let private tryFindLinkOwner (linkId: ProcessLinkId) (session: ProvenanceSession) =
    session.Processes
    |> Map.tryPick (fun processId structuralProcess ->
        if structuralProcess.Links |> Map.containsKey linkId then
            Some processId
        else
            None
    )

let private validateProcess (structuralProcess: StructuralProcess) (session: ProvenanceSession) =
    let duplicateLink =
        structuralProcess.Links
        |> Map.toSeq
        |> Seq.tryPick (fun (_, link) ->
            tryFindLinkOwner link.Id session |> Option.map (fun ownerId -> link.Id, ownerId)
        )

    let mismatchedLink =
        structuralProcess.Links
        |> Map.toSeq
        |> Seq.tryFind (fun (mapKey, link) -> mapKey <> link.Id)

    let mismatchedAssignment =
        structuralProcess.Assignments
        |> Map.toSeq
        |> Seq.tryFind (fun (mapKey, assignment) -> mapKey <> assignment.Id)

    let emptyCoverage =
        structuralProcess.Assignments
        |> Map.toSeq
        |> Seq.tryPick (fun (_, assignment) ->
            if assignment.CoveredLinkIds.IsEmpty then
                Some assignment.Id
            else
                None
        )

    let missingCoveredLink =
        structuralProcess.Assignments
        |> Map.toSeq
        |> Seq.collect (snd >> _.CoveredLinkIds)
        |> Seq.tryFind (fun linkId -> structuralProcess.Links |> Map.containsKey linkId |> not)

    let invalidNode =
        structuralProcess.Links
        |> Map.toSeq
        |> Seq.collect (snd >> _.Shape >> shapeNodeIds)
        |> Seq.tryFind (fun nodeId -> session.Nodes |> Map.containsKey nodeId |> not)

    match duplicateLink, mismatchedLink, mismatchedAssignment, emptyCoverage, missingCoveredLink, invalidNode with
    | Some(linkId, ownerId), _, _, _, _, _ ->
        Error(InconsistentCanonicalState $"Process link '{linkId}' is already owned by structural process '{ownerId}'.")
    | None, Some(mapKey, link), _, _, _, _ ->
        Error(InconsistentCanonicalState $"Process link map key '{mapKey}' does not match embedded ID '{link.Id}'.")
    | None, None, Some(mapKey, assignment), _, _, _ ->
        Error(
            InconsistentCanonicalState
                $"Process assignment map key '{mapKey}' does not match embedded ID '{assignment.Id}'."
        )
    | None, None, None, Some assignmentId, _, _ ->
        Error(InconsistentCanonicalState $"Process assignment '{assignmentId}' must cover at least one link.")
    | None, None, None, None, Some linkId, _ -> Error(LinkNotFound linkId)
    | None, None, None, None, None, Some nodeId -> Error(NodeNotFound nodeId)
    | None, None, None, None, None, None -> Ok()

let addProcess
    (structuralProcess: StructuralProcess)
    (session: ProvenanceSession)
    : Result<ProvenanceSession, ProvenanceCommandError> =
    match session.Processes |> Map.containsKey structuralProcess.Id with
    | true -> Error(InconsistentCanonicalState $"Structural process '{structuralProcess.Id}' already exists.")
    | false ->
        match session.Layers |> Map.tryFind structuralProcess.OriginLayerId with
        | None -> Error(LayerNotFound structuralProcess.OriginLayerId)
        | Some layer ->
            match validateProcess structuralProcess session with
            | Error error -> Error error
            | Ok() ->
                let updatedLayer = {
                    layer with
                        StructuralProcessIds = layer.StructuralProcessIds |> Set.add structuralProcess.Id
                }

                Ok {
                    session with
                        Processes = session.Processes |> Map.add structuralProcess.Id structuralProcess
                        Layers = session.Layers |> Map.add layer.Id updatedLayer
                }

let addLink
    (processId: StructuralProcessId)
    (link: ProcessLink)
    (session: ProvenanceSession)
    : Result<ProvenanceSession, ProvenanceCommandError> =
    match session.Processes |> Map.tryFind processId with
    | None -> Error(ProcessNotFound processId)
    | Some structuralProcess ->
        match tryFindLinkOwner link.Id session with
        | Some ownerId ->
            Error(
                InconsistentCanonicalState
                    $"Process link '{link.Id}' is already owned by structural process '{ownerId}'."
            )
        | None ->
            match validateLinkNodes session link with
            | Error error -> Error error
            | Ok() ->
                let updatedProcess = {
                    structuralProcess with
                        Links = structuralProcess.Links |> Map.add link.Id link
                }

                Ok {
                    session with
                        Processes = session.Processes |> Map.add processId updatedProcess
                }

let linkAssignments (session: ProvenanceSession) : Map<ProcessLinkId, Set<AnnotationAssignmentId>> =
    session.Processes
    |> Map.toSeq
    |> Seq.collect (fun (_, structuralProcess) ->
        structuralProcess.Assignments
        |> Map.toSeq
        |> Seq.collect (fun (assignmentId, assignment) ->
            assignment.CoveredLinkIds |> Seq.map (fun linkId -> linkId, assignmentId)
        )
    )
    |> Seq.fold
        (fun index (linkId, assignmentId) ->
            index
            |> Map.change
                linkId
                (fun current -> current |> Option.defaultValue Set.empty |> Set.add assignmentId |> Some)
        )
        Map.empty

let incidentLinks (session: ProvenanceSession) (nodeId: CanonicalNodeId) : IncidentLinks =
    let allLinks =
        session.Processes
        |> Map.toList
        |> List.collect (snd >> _.Links >> Map.toList >> List.map snd)

    {
        IncomingLinkIds =
            allLinks
            |> List.choose (fun link ->
                match link.Shape with
                | ProcessLinkShape.Between(_, output) when output = nodeId -> Some link.Id
                | _ -> None
            )
            |> List.sort
        OutgoingLinkIds =
            allLinks
            |> List.choose (fun link ->
                match link.Shape with
                | ProcessLinkShape.Between(input, _) when input = nodeId -> Some link.Id
                | _ -> None
            )
            |> List.sort
        OneSidedLinkIds =
            allLinks
            |> List.choose (fun link ->
                match link.Shape with
                | ProcessLinkShape.InputOnly input when input = nodeId -> Some link.Id
                | ProcessLinkShape.OutputOnly output when output = nodeId -> Some link.Id
                | _ -> None
            )
            |> List.sort
    }

let nodeAppearances (session: ProvenanceSession) (nodeId: CanonicalNodeId) : LayerEndpoint list =
    session.Layers
    |> Map.toList
    |> List.collect (fun (_, layer) ->
        [
            layer.InputEndpoints |> Map.tryFind nodeId
            layer.OutputEndpoints |> Map.tryFind nodeId
        ]
        |> List.choose id
    )

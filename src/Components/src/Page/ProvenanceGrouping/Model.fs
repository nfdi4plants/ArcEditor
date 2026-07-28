module Swate.Components.Page.ProvenanceGrouping.Model

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

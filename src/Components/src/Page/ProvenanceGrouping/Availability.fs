module Swate.Components.Page.ProvenanceGrouping.Availability

open System.Collections.Generic
open Swate.Components.Page.ProvenanceGrouping.Identifiers
open Swate.Components.Page.ProvenanceGrouping.Values
open Swate.Components.Page.ProvenanceGrouping.Domain
open Swate.Components.Page.ProvenanceGrouping.AvailabilityTypes
open Swate.Components.Page.ProvenanceGrouping.MutationTypes
open Swate.Components.Page.ProvenanceGrouping.ProjectionTypes

let private allLinks (session: ProvenanceSession) =
    session.Processes
    |> Map.toList
    |> List.collect (fun (processId, structuralProcess) ->
        structuralProcess.Links
        |> Map.toList
        |> List.map (fun (_, processLink) -> processId, processLink)
    )
    |> List.sortBy (fun (_, processLink) -> processLink.Id)

let private endpointNodeIds =
    function
    | ProcessLinkShape.Between(inputId, outputId) -> [ inputId; outputId ]
    | ProcessLinkShape.InputOnly inputId -> [ inputId ]
    | ProcessLinkShape.OutputOnly outputId -> [ outputId ]
    | ProcessLinkShape.Endpointless -> []

let private isOutput nodeId =
    function
    | ProcessLinkShape.Between(_, outputId) -> outputId = nodeId
    | ProcessLinkShape.OutputOnly outputId -> outputId = nodeId
    | ProcessLinkShape.InputOnly _
    | ProcessLinkShape.Endpointless -> false

let private evidence assignmentId owner relation originatingLinkIds visibleThroughLinkIds : ReachabilityEvidence = {
    AssignmentId = assignmentId
    Owner = owner
    Relation = relation
    OriginatingLinkIds = originatingLinkIds
    VisibleThroughLinkIds = visibleThroughLinkIds
}

let private ownedNodeEvidence nodeId (node: CanonicalNode) =
    node.Assignments
    |> Map.toList
    |> List.map (fun (_, assignment) -> evidence assignment.Id (NodeOwner nodeId) OwnedNode Set.empty Set.empty)

let private incidentProcessEvidence nodeId (session: ProvenanceSession) =
    allLinks session
    |> List.collect (fun (processId, processLink) ->
        if processLink.Shape |> endpointNodeIds |> List.contains nodeId then
            session.Processes[processId].Assignments
            |> Map.toList
            |> List.choose (fun (_, assignment) ->
                if assignment.CoveredLinkIds |> Set.contains processLink.Id then
                    Some(
                        evidence
                            assignment.Id
                            (ProcessOwner processId)
                            (IncidentProcess processLink.Id)
                            (Set.singleton processLink.Id)
                            (Set.singleton processLink.Id)
                    )
                else
                    None
            )
        else
            []
    )

let private reverseConnectionLocalEvidence nodeId (session: ProvenanceSession) =
    allLinks session
    |> List.collect (fun (_, processLink) ->
        match processLink.Shape with
        | ProcessLinkShape.Between(inputId, outputId) when inputId = nodeId ->
            session.Nodes
            |> Map.tryFind outputId
            |> Option.map (fun outputNode ->
                outputNode.Assignments
                |> Map.toList
                |> List.map (fun (_, assignment) ->
                    evidence
                        assignment.Id
                        (NodeOwner outputId)
                        (ReverseConnectionLocal processLink.Id)
                        Set.empty
                        (Set.singleton processLink.Id)
                )
            )
            |> Option.defaultValue []
        | _ -> []
    )

let private inverseRoutes nodeId (session: ProvenanceSession) =
    let incomingByOutput =
        allLinks session
        |> List.choose (fun (_, processLink) ->
            match processLink.Shape with
            | ProcessLinkShape.Between(inputId, outputId) -> Some(outputId, (inputId, processLink.Id))
            | ProcessLinkShape.InputOnly _
            | ProcessLinkShape.OutputOnly _
            | ProcessLinkShape.Endpointless -> None
        )
        |> List.groupBy fst
        |> List.map (fun (outputId, entries) -> outputId, (entries |> List.map snd |> List.sortBy snd))
        |> Map.ofList

    // Keyed by node ID alone, not by (assignment, node, propagation-mode) as
    // intent §7 rule 5 literally states. This is behaviorally equivalent: the
    // propagation mode is decided later at emission, not during this
    // traversal, so revisiting a node can never discover a route this BFS
    // hasn't already recorded. `normalizeEvidence` below still deduplicates
    // by the exact (assignment, owner, mode) triple before evidence reaches
    // a caller, and termination on a cyclic graph is covered by
    // "a cycle terminates and yields each availability once" in
    // Availability.Tests.fs.
    let queue = Queue<CanonicalNodeId * ProcessLinkId list>()
    let mutable visited = Set.singleton nodeId
    let mutable routes = Map.ofList [ nodeId, [] ]
    queue.Enqueue(nodeId, [])

    while queue.Count > 0 do
        let currentId, route = queue.Dequeue()

        incomingByOutput
        |> Map.tryFind currentId
        |> Option.defaultValue []
        |> List.iter (fun (inputId, linkId) ->
            if visited |> Set.contains inputId |> not then
                let inputRoute = linkId :: route
                visited <- visited |> Set.add inputId
                routes <- routes |> Map.add inputId inputRoute
                queue.Enqueue(inputId, inputRoute)
        )

    routes

let private forwardEvidence nodeId (session: ProvenanceSession) =
    let routes = inverseRoutes nodeId session

    routes
    |> Map.toList
    |> List.collect (fun (reachedNodeId, route) ->
        if List.isEmpty route then
            []
        else
            let routeLinks = Set.ofList route

            let nodeEvidence =
                session.Nodes
                |> Map.tryFind reachedNodeId
                |> Option.map (fun reachedNode ->
                    reachedNode.Assignments
                    |> Map.toList
                    |> List.map (fun (_, assignment) ->
                        evidence
                            assignment.Id
                            (NodeOwner reachedNodeId)
                            (ForwardPropagated route)
                            Set.empty
                            routeLinks
                    )
                )
                |> Option.defaultValue []

            let processEvidence =
                allLinks session
                |> List.collect (fun (processId, processLink) ->
                    if isOutput reachedNodeId processLink.Shape then
                        session.Processes[processId].Assignments
                        |> Map.toList
                        |> List.choose (fun (_, assignment) ->
                            if assignment.CoveredLinkIds |> Set.contains processLink.Id then
                                let originatingLinks = Set.singleton processLink.Id

                                Some(
                                    evidence
                                        assignment.Id
                                        (ProcessOwner processId)
                                        (ForwardPropagated route)
                                        originatingLinks
                                        (Set.union originatingLinks routeLinks)
                                )
                            else
                                None
                        )
                    else
                        []
                )

            nodeEvidence @ processEvidence
    )

[<RequireQualifiedAccess>]
type private EvidenceMode =
    | Owned
    | Incident of ProcessLinkId
    | Forward
    | Reverse of ProcessLinkId

let private evidenceMode =
    function
    | OwnedNode -> EvidenceMode.Owned
    | IncidentProcess linkId -> EvidenceMode.Incident linkId
    | ForwardPropagated _ -> EvidenceMode.Forward
    | ReverseConnectionLocal linkId -> EvidenceMode.Reverse linkId

let private mergeEvidence (references: ReachabilityEvidence list) =
    let representative =
        references
        |> List.minBy (fun reference ->
            match reference.Relation with
            | ForwardPropagated route -> route.Length, route
            | _ -> 0, []
        )

    {
        representative with
            OriginatingLinkIds =
                references
                |> List.fold (fun links reference -> Set.union links reference.OriginatingLinkIds) Set.empty
            VisibleThroughLinkIds =
                references
                |> List.fold (fun links reference -> Set.union links reference.VisibleThroughLinkIds) Set.empty
    }

let private normalizeEvidence (references: ReachabilityEvidence list) =
    references
    |> List.groupBy (fun (reference: ReachabilityEvidence) ->
        reference.AssignmentId, reference.Owner, evidenceMode reference.Relation
    )
    |> List.map (snd >> mergeEvidence)
    |> List.sortBy (fun (reference: ReachabilityEvidence) ->
        reference.AssignmentId, reference.Owner, evidenceMode reference.Relation
    )

let coldReachabilityEvidence
    (nodeId: CanonicalNodeId)
    (session: ProvenanceSession)
    : Result<ReachabilityEvidence list, ProvenanceCommandError> =
    match session.Nodes |> Map.tryFind nodeId with
    | None -> Error(NodeNotFound nodeId)
    | Some node ->
        ownedNodeEvidence nodeId node
        @ incidentProcessEvidence nodeId session
        @ reverseConnectionLocalEvidence nodeId session
        @ forwardEvidence nodeId session
        |> normalizeEvidence
        |> Ok

let private tryFindAssignmentValueId owner assignmentId (session: ProvenanceSession) =
    match owner with
    | NodeOwner nodeId ->
        session.Nodes
        |> Map.tryFind nodeId
        |> Option.bind (fun node -> node.Assignments |> Map.tryFind assignmentId)
        |> Option.map _.ValueId
    | ProcessOwner processId ->
        session.Processes
        |> Map.tryFind processId
        |> Option.bind (fun structuralProcess -> structuralProcess.Assignments |> Map.tryFind assignmentId)
        |> Option.map _.ValueId

let materializeEvidence
    (evidenceReferences: ReachabilityEvidence list)
    (session: ProvenanceSession)
    : Result<AvailableAnnotationRef list, ProvenanceCommandError> =
    let folder state (reference: ReachabilityEvidence) =
        state
        |> Result.bind (fun available ->
            match tryFindAssignmentValueId reference.Owner reference.AssignmentId session with
            | None -> Error(AssignmentNotFound(None, reference.AssignmentId))
            | Some valueId when session.Values |> Map.containsKey valueId |> not -> Error(ValueNotFound valueId)
            | Some valueId ->
                Ok(
                    {
                        AssignmentId = reference.AssignmentId
                        ValueId = valueId
                        Owner = reference.Owner
                        Relation = reference.Relation
                        OriginatingLinkIds = reference.OriginatingLinkIds
                        VisibleThroughLinkIds = reference.VisibleThroughLinkIds
                    }
                    :: available
                )
        )

    evidenceReferences |> List.fold folder (Ok []) |> Result.map List.rev

let resolveNodeAvailability
    (nodeId: CanonicalNodeId)
    (session: ProvenanceSession)
    : Result<AvailableAnnotationRef list, ProvenanceCommandError> =
    coldReachabilityEvidence nodeId session
    |> Result.bind (fun references -> materializeEvidence references session)

let resolveNodeAvailabilityWithMemo
    (nodeId: CanonicalNodeId)
    (session: ProvenanceSession)
    : Result<AvailableAnnotationRef list * ProvenanceSession, ProvenanceCommandError> =
    match session.Nodes |> Map.tryFind nodeId with
    | None -> Error(NodeNotFound nodeId)
    | Some _ ->
        let cachedEvidence =
            session.ReachabilityMemo
            |> Map.tryFind nodeId
            |> Option.filter (fun memo -> memo.TopologyRevision = session.AvailabilityTopologyRevision)

        match cachedEvidence with
        | Some memo ->
            materializeEvidence memo.Evidence session
            |> Result.map (fun references -> references, session)
        | None ->
            coldReachabilityEvidence nodeId session
            |> Result.bind (fun evidence ->
                materializeEvidence evidence session
                |> Result.map (fun references ->
                    let memo = {
                        TopologyRevision = session.AvailabilityTopologyRevision
                        Evidence = evidence
                    }

                    references,
                    {
                        session with
                            ReachabilityMemo = session.ReachabilityMemo |> Map.add nodeId memo
                    }
                )
            )

let resolveLayerAvailability
    (layerId: ProvenanceLayerId)
    (session: ProvenanceSession)
    : Result<Map<LayerEndpointKey, AvailableAnnotationRef list>, ProvenanceCommandError> =
    match session.Layers |> Map.tryFind layerId with
    | None -> Error(LayerNotFound layerId)
    | Some layer ->
        let endpoints =
            [
                layer.InputEndpoints |> Map.toList |> List.map snd
                layer.OutputEndpoints |> Map.toList |> List.map snd
            ]
            |> List.concat
            |> List.sortBy (fun (endpoint: LayerEndpoint) ->
                endpoint.LayerOrderPosition, endpoint.Key.Side, endpoint.Key.NodeId
            )

        let folder state (endpoint: LayerEndpoint) =
            state
            |> Result.bind (fun resolved ->
                resolveNodeAvailability endpoint.Key.NodeId session
                |> Result.map (fun references -> resolved |> Map.add endpoint.Key references)
            )

        endpoints |> List.fold folder (Ok Map.empty)

let resolveLayerAvailabilityWithMemo
    (layerId: ProvenanceLayerId)
    (session: ProvenanceSession)
    : Result<Map<LayerEndpointKey, AvailableAnnotationRef list> * ProvenanceSession, ProvenanceCommandError> =
    match session.Layers |> Map.tryFind layerId with
    | None -> Error(LayerNotFound layerId)
    | Some layer ->
        let endpoints =
            [
                layer.InputEndpoints |> Map.toList |> List.map snd
                layer.OutputEndpoints |> Map.toList |> List.map snd
            ]
            |> List.concat
            |> List.sortBy (fun (endpoint: LayerEndpoint) ->
                endpoint.LayerOrderPosition, endpoint.Key.Side, endpoint.Key.NodeId
            )

        let folder state (endpoint: LayerEndpoint) =
            state
            |> Result.bind (fun (resolved, currentSession) ->
                resolveNodeAvailabilityWithMemo endpoint.Key.NodeId currentSession
                |> Result.map (fun (references, nextSession) ->
                    resolved |> Map.add endpoint.Key references, nextSession
                )
            )

        endpoints |> List.fold folder (Ok(Map.empty, session))

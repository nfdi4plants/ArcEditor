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

/// One-pass lookup structures over `allLinks`. Built once per resolution and
/// shared across a layer's endpoints, so per-node evidence collection no longer
/// rescans and re-sorts every link of every process. Every list retains the
/// link-ID order `allLinks` establishes, which keeps evidence emission order -
/// and with it memoized evidence and merge representatives - identical to the
/// unindexed implementation.
type private LinkIndex = {
    /// Links having the node as an endpoint, once per link (a self-loop counts once).
    LinksByEndpoint: Map<CanonicalNodeId, (StructuralProcessId * ProcessLink) list>
    /// Links having the node as their output endpoint.
    LinksByOutput: Map<CanonicalNodeId, (StructuralProcessId * ProcessLink) list>
    /// Two-ended links keyed by their input node: (output node, link).
    BetweenByInput: Map<CanonicalNodeId, (CanonicalNodeId * ProcessLinkId) list>
    /// Two-ended links keyed by their output node: (input node, link).
    BetweenByOutput: Map<CanonicalNodeId, (CanonicalNodeId * ProcessLinkId) list>
}

let private groupToMap entries =
    entries
    |> List.groupBy fst
    |> List.map (fun (key, grouped) -> key, grouped |> List.map snd)
    |> Map.ofList

let private buildLinkIndex (session: ProvenanceSession) : LinkIndex =
    let links = allLinks session

    {
        LinksByEndpoint =
            links
            |> List.collect (fun (processId, processLink) ->
                processLink.Shape
                |> endpointNodeIds
                |> List.distinct
                |> List.map (fun nodeId -> nodeId, (processId, processLink))
            )
            |> groupToMap
        LinksByOutput =
            links
            |> List.choose (fun (processId, processLink) ->
                match processLink.Shape with
                | ProcessLinkShape.Between(_, outputId)
                | ProcessLinkShape.OutputOnly outputId -> Some(outputId, (processId, processLink))
                | ProcessLinkShape.InputOnly _
                | ProcessLinkShape.Endpointless -> None
            )
            |> groupToMap
        BetweenByInput =
            links
            |> List.choose (fun (_, processLink) ->
                match processLink.Shape with
                | ProcessLinkShape.Between(inputId, outputId) -> Some(inputId, (outputId, processLink.Id))
                | _ -> None
            )
            |> groupToMap
        BetweenByOutput =
            links
            |> List.choose (fun (_, processLink) ->
                match processLink.Shape with
                | ProcessLinkShape.Between(inputId, outputId) -> Some(outputId, (inputId, processLink.Id))
                | _ -> None
            )
            |> groupToMap
    }

let private linksFor nodeId lookup =
    lookup |> Map.tryFind nodeId |> Option.defaultValue []

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

let private incidentProcessEvidence nodeId (session: ProvenanceSession) (index: LinkIndex) =
    linksFor nodeId index.LinksByEndpoint
    |> List.collect (fun (processId, processLink) ->
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
    )

let private reverseConnectionLocalEvidence nodeId (session: ProvenanceSession) (index: LinkIndex) =
    linksFor nodeId index.BetweenByInput
    |> List.collect (fun (outputId, linkId) ->
        session.Nodes
        |> Map.tryFind outputId
        |> Option.map (fun outputNode ->
            outputNode.Assignments
            |> Map.toList
            |> List.map (fun (_, assignment) ->
                evidence
                    assignment.Id
                    (NodeOwner outputId)
                    (ReverseConnectionLocal linkId)
                    Set.empty
                    (Set.singleton linkId)
            )
        )
        |> Option.defaultValue []
    )

// Keyed by node ID alone, not by (assignment, node, propagation-mode) as
// intent §7 rule 5 literally states. This is behaviorally equivalent: the
// propagation mode is decided later at emission, not during this
// traversal, so revisiting a node can never discover a route this BFS
// hasn't already recorded. `normalizeEvidence` below still deduplicates
// by the exact (assignment, owner, mode) triple before evidence reaches
// a caller, and termination on a cyclic graph is covered by
// "a cycle terminates and yields each availability once" in
// Availability.Tests.fs.
let private inverseRoutes nodeId (index: LinkIndex) =
    let queue = Queue<CanonicalNodeId * ProcessLinkId list>()
    let mutable visited = Set.singleton nodeId
    let mutable routes = Map.ofList [ nodeId, [] ]
    queue.Enqueue(nodeId, [])

    while queue.Count > 0 do
        let currentId, route = queue.Dequeue()

        linksFor currentId index.BetweenByOutput
        |> List.iter (fun (inputId, linkId) ->
            if visited |> Set.contains inputId |> not then
                let inputRoute = linkId :: route
                visited <- visited |> Set.add inputId
                routes <- routes |> Map.add inputId inputRoute
                queue.Enqueue(inputId, inputRoute)
        )

    routes

let private forwardEvidence nodeId (session: ProvenanceSession) (index: LinkIndex) =
    let routes = inverseRoutes nodeId index

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
                linksFor reachedNodeId index.LinksByOutput
                |> List.collect (fun (processId, processLink) ->
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

let private coldEvidenceWithIndex (index: LinkIndex) nodeId (node: CanonicalNode) (session: ProvenanceSession) =
    ownedNodeEvidence nodeId node
    @ incidentProcessEvidence nodeId session index
    @ reverseConnectionLocalEvidence nodeId session index
    @ forwardEvidence nodeId session index
    |> normalizeEvidence

let coldReachabilityEvidence
    (nodeId: CanonicalNodeId)
    (session: ProvenanceSession)
    : Result<ReachabilityEvidence list, ProvenanceCommandError> =
    match session.Nodes |> Map.tryFind nodeId with
    | None -> Error(NodeNotFound nodeId)
    | Some node -> Ok(coldEvidenceWithIndex (buildLinkIndex session) nodeId node session)

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

let private resolveNodeWithIndex
    (index: LinkIndex)
    (nodeId: CanonicalNodeId)
    (session: ProvenanceSession)
    : Result<AvailableAnnotationRef list, ProvenanceCommandError> =
    match session.Nodes |> Map.tryFind nodeId with
    | None -> Error(NodeNotFound nodeId)
    | Some node -> materializeEvidence (coldEvidenceWithIndex index nodeId node session) session

let resolveNodeAvailability
    (nodeId: CanonicalNodeId)
    (session: ProvenanceSession)
    : Result<AvailableAnnotationRef list, ProvenanceCommandError> =
    resolveNodeWithIndex (buildLinkIndex session) nodeId session

let private resolveNodeAvailabilityMemoized
    (getIndex: unit -> LinkIndex)
    (nodeId: CanonicalNodeId)
    (session: ProvenanceSession)
    : Result<AvailableAnnotationRef list * ProvenanceSession, ProvenanceCommandError> =
    match session.Nodes |> Map.tryFind nodeId with
    | None -> Error(NodeNotFound nodeId)
    | Some node ->
        let cachedEvidence =
            session.ReachabilityMemo
            |> Map.tryFind nodeId
            |> Option.filter (fun memo -> memo.TopologyRevision = session.AvailabilityTopologyRevision)

        match cachedEvidence with
        | Some memo ->
            materializeEvidence memo.Evidence session
            |> Result.map (fun references -> references, session)
        | None ->
            let evidence = coldEvidenceWithIndex (getIndex ()) nodeId node session

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

let resolveNodeAvailabilityWithMemo
    (nodeId: CanonicalNodeId)
    (session: ProvenanceSession)
    : Result<AvailableAnnotationRef list * ProvenanceSession, ProvenanceCommandError> =
    let index = lazy (buildLinkIndex session)
    resolveNodeAvailabilityMemoized (fun () -> index.Value) nodeId session

let private layerEndpoints (layer: ProvenanceLayer) =
    [
        layer.InputEndpoints |> Map.toList |> List.map snd
        layer.OutputEndpoints |> Map.toList |> List.map snd
    ]
    |> List.concat
    |> List.sortBy (fun (endpoint: LayerEndpoint) ->
        endpoint.LayerOrderPosition, endpoint.Key.Side, endpoint.Key.NodeId
    )

let resolveLayerAvailability
    (layerId: ProvenanceLayerId)
    (session: ProvenanceSession)
    : Result<Map<LayerEndpointKey, AvailableAnnotationRef list>, ProvenanceCommandError> =
    match session.Layers |> Map.tryFind layerId with
    | None -> Error(LayerNotFound layerId)
    | Some layer ->
        let index = lazy (buildLinkIndex session)

        let folder state (endpoint: LayerEndpoint) =
            state
            |> Result.bind (fun resolved ->
                resolveNodeWithIndex index.Value endpoint.Key.NodeId session
                |> Result.map (fun references -> resolved |> Map.add endpoint.Key references)
            )

        layerEndpoints layer |> List.fold folder (Ok Map.empty)

let resolveLayerAvailabilityWithMemo
    (layerId: ProvenanceLayerId)
    (session: ProvenanceSession)
    : Result<Map<LayerEndpointKey, AvailableAnnotationRef list> * ProvenanceSession, ProvenanceCommandError> =
    match session.Layers |> Map.tryFind layerId with
    | None -> Error(LayerNotFound layerId)
    | Some layer ->
        // The memo additions the fold threads through never change the link
        // topology, so one index over the incoming session serves every
        // endpoint; a fully memo-warm layer never builds it at all.
        let index = lazy (buildLinkIndex session)

        let folder state (endpoint: LayerEndpoint) =
            state
            |> Result.bind (fun (resolved, currentSession) ->
                resolveNodeAvailabilityMemoized (fun () -> index.Value) endpoint.Key.NodeId currentSession
                |> Result.map (fun (references, nextSession) ->
                    resolved |> Map.add endpoint.Key references, nextSession
                )
            )

        layerEndpoints layer |> List.fold folder (Ok(Map.empty, session))

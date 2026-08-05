module CanonicalAvailabilityTests

open Expecto
open Swate.Components.Page.ProvenanceGrouping.Identifiers
open Swate.Components.Page.ProvenanceGrouping.Values
open Swate.Components.Page.ProvenanceGrouping.Domain
open Swate.Components.Page.ProvenanceGrouping.AvailabilityTypes
open Swate.Components.Page.ProvenanceGrouping.ProjectionTypes
open Swate.Components.Page.ProvenanceGrouping.MutationTypes
open Swate.Components.Page.ProvenanceGrouping.Model
open Swate.Components.Page.ProvenanceGrouping.Availability
open Swate.Components.Page.ProvenanceGrouping.Commands

let private endpointKind = {
    Id = "canonical:endpoint:sample"
    Label = "Sample"
}

let private term name = {
    Name = name
    TermSource = Some "TEST"
    TermAccession = Some $"TEST:{name}"
}

let private node id name assignments : CanonicalNode = {
    Id = id
    Key = {
        KindId = endpointKind.Id
        Name = name
    }
    Kind = endpointKind
    Name = name
    Assignments =
        assignments
        |> List.map (fun (assignment: NodeAssignment) -> assignment.Id, assignment)
        |> Map.ofList
}

let private nodeAssignment id valueId : NodeAssignment = {
    Id = id
    ValueId = valueId
    PropertyKind = AssignmentPropertyKind.Generic
    TargetSource = None
    Lineage = AssignmentLineage.Loaded
}

let private processAssignment id valueId links : ProcessAssignment = {
    Id = id
    ValueId = valueId
    PropertyKind = AssignmentPropertyKind.Generic
    CoveredLinkIds = Set.ofList links
    ContainerReferenceValueId = None
    ReferenceSlotId = None
    Lineage = AssignmentLineage.Loaded
}

let private structuralProcess id links assignments : StructuralProcess = {
    Id = id
    OriginLayerId = "layer-one"
    Name = Some id
    Links =
        links
        |> List.map (fun (processLink: ProcessLink) -> processLink.Id, processLink)
        |> Map.ofList
    Assignments =
        assignments
        |> List.map (fun (assignment: ProcessAssignment) -> assignment.Id, assignment)
        |> Map.ofList
}

let private link id shape : ProcessLink = { Id = id; Shape = shape }

let private property id name : PropertyDefinition = { Id = id; Category = term name }

let private value id propertyId text : PropertyValueDefinition = {
    Id = id
    PropertyId = propertyId
    Value = ProvenanceValue.Text text
    Unit = None
}

let private appearance layerId side nodeId position : LayerEndpoint = {
    Key = {
        LayerId = layerId
        Side = side
        NodeId = nodeId
    }
    Header = { Kind = endpointKind; Text = nodeId }
    LayerOrderPosition = position
}

let private layer id sourceId (inputs: LayerEndpoint list) (outputs: LayerEndpoint list) processIds : ProvenanceLayer = {
    Id = id
    Label = id
    Source = { Id = sourceId; Name = sourceId }
    InputEndpoints = inputs |> List.map (fun endpoint -> endpoint.Key.NodeId, endpoint) |> Map.ofList
    OutputEndpoints =
        outputs
        |> List.map (fun endpoint -> endpoint.Key.NodeId, endpoint)
        |> Map.ofList
    StructuralProcessIds = Set.ofList processIds
}

let private expectOk =
    function
    | Ok value -> value
    | Error error -> failtestf "Expected Ok but received %A" error

let private resolve nodeId session =
    resolveNodeAvailability nodeId session |> expectOk

let private resolveWithMemo nodeId session =
    resolveNodeAvailabilityWithMemo nodeId session |> expectOk

let private runCommand command session =
    command session
    |> expectOk
    |> fun effect -> Swate.Components.Page.ProvenanceGrouping.Session.commit effect session

let private nodeDraft categoryName text : NodeAssignmentDraft = {
    Content = {
        Category = term categoryName
        Value = ProvenanceValue.Text text
        Unit = None
    }
    OwnerKind = AnnotationOwnerKind.Node
    PropertyKind = AssignmentPropertyKind.Generic
}

let private nodeContent categoryName text : NodeValueContent = {
    Category = term categoryName
    Value = ProvenanceValue.Text text
    Unit = None
}

let private processDraft categoryName text : ProcessAssignmentDraft = {
    Content = nodeContent categoryName text
    OwnerKind = AnnotationOwnerKind.Process
    PropertyKind = AssignmentPropertyKind.Generic
    ContainerReferenceValueId = None
    ReferenceSlotId = None
    Lineage = AssignmentLineage.Created
}

let private hasAssignment assignmentId (references: AvailableAnnotationRef list) =
    references
    |> List.exists (fun reference -> reference.AssignmentId = assignmentId)

let private referencesFor assignmentId (references: AvailableAnnotationRef list) =
    references
    |> List.filter (fun reference -> reference.AssignmentId = assignmentId)

let private branchFixture () =
    let x = nodeAssignment "assignment-x" "value-x"
    let y = nodeAssignment "assignment-y" "value-y"
    let p = processAssignment "assignment-p" "value-p" [ "link-ab" ]

    let nodes =
        [
            node "node-a" "A" [ y ]
            node "node-b" "B" [ x ]
            node "node-c" "C" []
            node "node-d" "D" []
        ]
        |> List.map (fun item -> item.Id, item)
        |> Map.ofList

    let processes =
        [
            structuralProcess "process-ab" [
                link "link-ab" (ProcessLinkShape.Between("node-a", "node-b"))
            ] [ p ]
            structuralProcess "process-ac" [
                link "link-ac" (ProcessLinkShape.Between("node-a", "node-c"))
            ] []
            structuralProcess "process-bd" [
                link "link-bd" (ProcessLinkShape.Between("node-b", "node-d"))
            ] []
        ]
        |> List.map (fun item -> item.Id, item)
        |> Map.ofList

    let properties =
        [
            property "property-x" "X"
            property "property-y" "Y"
            property "property-p" "P"
        ]
        |> List.map (fun item -> item.Id, item)
        |> Map.ofList

    let values =
        [
            value "value-x" "property-x" "x"
            value "value-y" "property-y" "y"
            value "value-p" "property-p" "p"
        ]
        |> List.map (fun item -> item.Id, item)
        |> Map.ofList

    let layerOne =
        layer
            "layer-one"
            "source-one"
            [
                appearance "layer-one" ProvenanceSide.Input "node-a" 0
                appearance "layer-one" ProvenanceSide.Input "node-b" 1
            ] [
                appearance "layer-one" ProvenanceSide.Output "node-b" 0
                appearance "layer-one" ProvenanceSide.Output "node-c" 1
                appearance "layer-one" ProvenanceSide.Output "node-d" 2
            ] [ "process-ab"; "process-ac"; "process-bd" ]

    let layerTwo =
        layer "layer-two" "source-two" [ appearance "layer-two" ProvenanceSide.Input "node-b" 0 ] [] []

    {
        empty with
            Nodes = nodes
            Processes = processes
            Properties = properties
            Values = values
            Layers = Map.ofList [ layerOne.Id, layerOne; layerTwo.Id, layerTwo ]
            LayerOrder = [ layerOne.Id; layerTwo.Id ]
            ActiveLayerId = layerOne.Id
    }

let private oneSidedFixture shape =
    let assignment =
        processAssignment "assignment-one-sided" "value-one-sided" [ "one-sided" ]

    let nodes =
        [
            node "node-i" "I" []
            node "node-o" "O" []
            node "node-d" "D" []
        ]
        |> List.map (fun item -> item.Id, item)
        |> Map.ofList

    let oneSided =
        structuralProcess "one-sided-process" [ link "one-sided" shape ] [ assignment ]

    let downstream =
        structuralProcess "downstream-process" [
            link "downstream" (ProcessLinkShape.Between("node-o", "node-d"))
        ] []

    {
        empty with
            Nodes = nodes
            Processes = Map.ofList [ oneSided.Id, oneSided; downstream.Id, downstream ]
            Properties =
                Map.ofList [
                    "property-one-sided", property "property-one-sided" "One-sided"
                ]
            Values =
                Map.ofList [
                    "value-one-sided", value "value-one-sided" "property-one-sided" "value"
                ]
    }

let tests =
    testList "CanonicalAvailability" [
        testCase "a node annotation owned by B is reverse-local on A"
        <| fun _ ->
            let references = branchFixture () |> resolve "node-a"

            let reference = referencesFor "assignment-x" references |> List.exactlyOne

            Expect.equal
                reference.Relation
                (ReverseConnectionLocal "link-ab")
                "The output-owned annotation is connection-local on the input."

        testCase "a node annotation owned by B is absent when resolving C"
        <| fun _ ->
            let references = branchFixture () |> resolve "node-c"
            Expect.isFalse (hasAssignment "assignment-x" references) "Reverse-local X never leaks to sibling C."

        testCase "a node annotation owned by B is present at D"
        <| fun _ ->
            let references = branchFixture () |> resolve "node-d"
            Expect.isTrue (hasAssignment "assignment-x" references) "B-owned X propagates forward to D."

        testCase "a process annotation on A to B is incident on A and B"
        <| fun _ ->
            let session = branchFixture ()

            for nodeId in [ "node-a"; "node-b" ] do
                let references = resolve nodeId session

                Expect.isTrue
                    (references
                     |> List.exists (fun reference ->
                         reference.AssignmentId = "assignment-p"
                         && reference.Relation = IncidentProcess "link-ab"
                     ))
                    $"P is incident on {nodeId}."

        testCase "a process annotation on A to B is absent at C"
        <| fun _ ->
            let references = branchFixture () |> resolve "node-c"
            Expect.isFalse (hasAssignment "assignment-p" references) "Input-side incident P does not seed sibling C."

        testCase "a process annotation on A to B is present at D"
        <| fun _ ->
            let references = branchFixture () |> resolve "node-d"
            Expect.isTrue (hasAssignment "assignment-p" references) "P propagates from covered output B to D."

        testCase "a node annotation owned by A is present at B, C and D"
        <| fun _ ->
            let session = branchFixture ()

            for nodeId in [ "node-b"; "node-c"; "node-d" ] do
                Expect.isTrue
                    (resolve nodeId session |> hasAssignment "assignment-y")
                    $"A-owned Y propagates to {nodeId}."

        testCase "an input-only process annotation stays on its input and does not propagate"
        <| fun _ ->
            let session = oneSidedFixture (ProcessLinkShape.InputOnly "node-o")

            Expect.isTrue
                (resolve "node-o" session
                 |> List.exists (fun reference ->
                     reference.AssignmentId = "assignment-one-sided"
                     && reference.Relation = IncidentProcess "one-sided"
                 ))
                "The input-only annotation is incident on its input."

            Expect.isFalse
                (resolve "node-d" session |> hasAssignment "assignment-one-sided")
                "Input-side incident availability is terminal."

        testCase "an output-only process annotation seeds forward from its output"
        <| fun _ ->
            let session = oneSidedFixture (ProcessLinkShape.OutputOnly "node-o")

            Expect.isTrue
                (resolve "node-o" session
                 |> List.exists (fun reference ->
                     reference.AssignmentId = "assignment-one-sided"
                     && reference.Relation = IncidentProcess "one-sided"
                 ))
                "The output-only assignment is incident on its output."

            Expect.isTrue
                (resolve "node-d" session |> hasAssignment "assignment-one-sided")
                "The output-only assignment seeds downstream carry."

        testCase "an endpointless process produces no item projection"
        <| fun _ ->
            let assignment =
                processAssignment "assignment-endpointless" "value-endpointless" [ "endpointless" ]

            let session = {
                empty with
                    Nodes =
                        Map.ofList [
                            "node-a", node "node-a" "A" []
                            "node-b", node "node-b" "B" []
                        ]
                    Processes =
                        Map.ofList [
                            "endpointless-process",
                            structuralProcess "endpointless-process" [
                                link "endpointless" ProcessLinkShape.Endpointless
                            ] [ assignment ]
                        ]
                    Properties =
                        Map.ofList [
                            "property-endpointless", property "property-endpointless" "Endpointless"
                        ]
                    Values =
                        Map.ofList [
                            "value-endpointless", value "value-endpointless" "property-endpointless" "value"
                        ]
            }

            for nodeId in [ "node-a"; "node-b" ] do
                Expect.isFalse
                    (resolve nodeId session |> hasAssignment "assignment-endpointless")
                    "Endpointless assignments have no node projection."

        testCase "a cycle terminates and yields each availability once"
        <| fun _ ->
            let assignment = nodeAssignment "assignment-cycle" "value-cycle"

            let session = {
                empty with
                    Nodes =
                        Map.ofList [
                            "node-a", node "node-a" "A" [ assignment ]
                            "node-b", node "node-b" "B" []
                        ]
                    Processes =
                        [
                            structuralProcess "process-ab" [
                                link "link-ab" (ProcessLinkShape.Between("node-a", "node-b"))
                            ] []
                            structuralProcess "process-ba" [
                                link "link-ba" (ProcessLinkShape.Between("node-b", "node-a"))
                            ] []
                        ]
                        |> List.map (fun item -> item.Id, item)
                        |> Map.ofList
                    Properties = Map.ofList [ "property-cycle", property "property-cycle" "Cycle" ]
                    Values =
                        Map.ofList [
                            "value-cycle", value "value-cycle" "property-cycle" "value"
                        ]
            }

            let relations =
                resolve "node-b" session |> referencesFor assignment.Id |> List.map _.Relation

            Expect.equal
                relations.Length
                (relations |> List.distinct |> List.length)
                "The cycle yields each assignment/relation state once."

        testCase "availability reaching a canonical node is visible on every appearance of it"
        <| fun _ ->
            let session = branchFixture ()
            let byAppearance = resolveLayerAvailability "layer-two" session |> expectOk

            let endpointKey = {
                LayerId = "layer-two"
                Side = ProvenanceSide.Input
                NodeId = "node-b"
            }

            Expect.isTrue
                (byAppearance[endpointKey] |> hasAssignment "assignment-y")
                "The later-layer B appearance receives availability propagated from A."

        testCase "every availability reference retains its assignment, value, owner and link evidence"
        <| fun _ ->
            let references = branchFixture () |> resolve "node-d"

            let processReference = referencesFor "assignment-p" references |> List.exactlyOne

            Expect.equal processReference.ValueId "value-p" "The current reusable value is retained."
            Expect.equal processReference.Owner (ProcessOwner "process-ab") "The process owner is retained."
            Expect.equal processReference.OriginatingLinkIds (Set.singleton "link-ab") "Origin links are exact."

            Expect.equal
                processReference.VisibleThroughLinkIds
                (Set.ofList [ "link-ab"; "link-bd" ])
                "Origin and forward route evidence are retained."

            match processReference.Relation with
            | ForwardPropagated route -> Expect.equal route [ "link-bd" ] "The directed carry route is retained."
            | relation -> failtestf "Expected forward propagation but got %A" relation

        testCase "the relation does not affect which values are available"
        <| fun _ ->
            let session = branchFixture ()

            let representatives = [
                resolve "node-a" session
                |> List.find (fun reference -> reference.Relation = OwnedNode)
                resolve "node-a" session
                |> List.find (fun reference -> reference.Relation = IncidentProcess "link-ab")
                resolve "node-d" session
                |> List.find (fun reference ->
                    match reference.Relation with
                    | ForwardPropagated _ -> true
                    | _ -> false
                )
                resolve "node-a" session
                |> List.find (fun reference -> reference.Relation = ReverseConnectionLocal "link-ab")
            ]

            Expect.isTrue
                (representatives
                 |> List.forall (fun (reference: AvailableAnnotationRef) ->
                     session.Values.ContainsKey reference.ValueId
                 ))
                "Owned, incident, forward, and reverse relations all expose ordinary current values."

        testCase "a value-only edit reuses the reachability memo and repaints the new value"
        <| fun _ ->
            let before = branchFixture ()
            let beforeReferences, memoized = resolveWithMemo "node-d" before

            let edited =
                memoized
                |> runCommand (editNodeAssignment "node-b" "assignment-x" (nodeContent "X" "x-updated"))

            Expect.equal
                edited.ReachabilityMemo
                memoized.ReachabilityMemo
                "A value-only edit retains current-topology evidence."

            let afterReferences, afterResolution = resolveWithMemo "node-d" edited
            Expect.equal afterReferences beforeReferences "Reachability and current value IDs are unchanged."
            Expect.equal afterResolution.ReachabilityMemo edited.ReachabilityMemo "The retained memo is reused."

            let xReference = afterReferences |> referencesFor "assignment-x" |> List.exactlyOne

            Expect.equal
                afterResolution.Values[xReference.ValueId].Value
                (ProvenanceValue.Text "x-updated")
                "Materialization resolves the edited value content."

        testCase "an assignment addition makes older memos unreadable"
        <| fun _ ->
            let before = branchFixture ()
            let _, memoized = resolveWithMemo "node-d" before
            let staleMemo = memoized.ReachabilityMemo["node-d"]

            let changed =
                memoized
                |> runCommand (assignNodeValue (Set.singleton "node-a") (nodeDraft "Added" "new") NoOverwrite)

            let references, refreshed = resolveWithMemo "node-d" changed

            Expect.isGreaterThan
                changed.AvailabilityTopologyRevision
                staleMemo.TopologyRevision
                "Assignment addition advances topology."

            Expect.equal
                refreshed.ReachabilityMemo["node-d"].TopologyRevision
                changed.AvailabilityTopologyRevision
                "The older memo was replaced."

            Expect.isTrue
                (references
                 |> List.exists (fun (reference: AvailableAnnotationRef) ->
                     refreshed.Properties[refreshed.Values[reference.ValueId].PropertyId].Category.Name = "Added"
                 ))
                "Recomputation includes the newly reachable assignment."

        testCase "an assignment removal makes older memos unreadable"
        <| fun _ ->
            let before = branchFixture ()
            let _, memoized = resolveWithMemo "node-d" before
            let staleMemo = memoized.ReachabilityMemo["node-d"]

            let changed = memoized |> runCommand (removeNodeAssignment "node-b" "assignment-x")

            let references, refreshed = resolveWithMemo "node-d" changed

            Expect.isGreaterThan
                changed.AvailabilityTopologyRevision
                staleMemo.TopologyRevision
                "Assignment removal advances topology."

            Expect.equal
                refreshed.ReachabilityMemo["node-d"].TopologyRevision
                changed.AvailabilityTopologyRevision
                "The stale memo was recomputed."

            Expect.isFalse
                (references |> hasAssignment "assignment-x")
                "Removed availability cannot survive through cached evidence."

        testCase "a covered-link change makes older memos unreadable"
        <| fun _ ->
            let before =
                branchFixture ()
                |> fun session ->
                    let combinedProcess = {
                        session.Processes["process-ab"] with
                            Links =
                                Map.ofList [
                                    "link-ab", session.Processes["process-ab"].Links["link-ab"]
                                    "link-ac", session.Processes["process-ac"].Links["link-ac"]
                                ]
                            Assignments =
                                session.Processes["process-ab"].Assignments
                                |> Map.map (fun _ assignment -> {
                                    assignment with
                                        CoveredLinkIds = Set.ofList [ "link-ab"; "link-ac" ]
                                })
                    }

                    {
                        session with
                            Processes =
                                session.Processes
                                |> Map.remove "process-ac"
                                |> Map.add combinedProcess.Id combinedProcess
                    }

            let _, memoized = resolveWithMemo "node-c" before
            let oldAssignment = memoized.Processes["process-ab"].Assignments["assignment-p"]
            let staleMemo = memoized.ReachabilityMemo["node-c"]

            let changed =
                memoized
                |> runCommand (removeProcessAssignmentLinks "process-ab" "assignment-p" (Set.singleton "link-ac"))

            let changedAssignment = changed.Processes["process-ab"].Assignments["assignment-p"]
            let references, refreshed = resolveWithMemo "node-c" changed

            Expect.equal
                changedAssignment.CoveredLinkIds
                (oldAssignment.CoveredLinkIds |> Set.remove "link-ac")
                "The process occurrence retains only its unremoved coverage."

            Expect.isGreaterThan
                changed.AvailabilityTopologyRevision
                staleMemo.TopologyRevision
                "Coverage change advances topology."

            Expect.equal
                refreshed.ReachabilityMemo["node-c"].TopologyRevision
                changed.AvailabilityTopologyRevision
                "The older coverage memo was replaced."

            Expect.isFalse
                (references |> hasAssignment "assignment-p")
                "Recomputation retracts availability from the removed covered link."

        testCase "a conservatively classified mutation produces the same availability as a cold resolution"
        <| fun _ ->
            let before = branchFixture ()
            let _, memoized = resolveWithMemo "node-d" before
            let oldRevision = memoized.AvailabilityTopologyRevision

            let changed =
                Swate.Components.Page.ProvenanceGrouping.Session.connectNodes
                    "layer-one"
                    [ "node-a", "node-d" ]
                    memoized
                |> expectOk

            let memoizedReferences, refreshed = resolveWithMemo "node-d" changed

            let coldReferences =
                resolveNodeAvailability "node-d" {
                    changed with
                        ReachabilityMemo = Map.empty
                }
                |> expectOk

            Expect.equal memoizedReferences coldReferences "Stale-memo recomputation equals a cold resolution."

            Expect.equal
                refreshed.AvailabilityTopologyRevision
                (oldRevision + 1)
                "The conservative structural mutation advanced topology."

        testCase "memo entries never carry value content"
        <| fun _ ->
            let before = branchFixture ()
            let _, memoized = resolveWithMemo "node-d" before
            let memoBefore = memoized.ReachabilityMemo["node-d"]

            let edited =
                memoized
                |> runCommand (editNodeAssignment "node-b" "assignment-x" (nodeContent "X" "fresh-content"))

            let references, resolved = resolveWithMemo "node-d" edited

            Expect.equal
                resolved.ReachabilityMemo["node-d"]
                memoBefore
                "Value content changes do not rewrite reachability evidence."

            Expect.isFalse
                (typeof<ReachabilityEvidence>.GetProperties()
                 |> Array.exists (fun field -> field.Name = "ValueId"))
                "The memo evidence type has no value ID or content field."

            let reference = references |> referencesFor "assignment-x" |> List.exactlyOne

            Expect.equal
                resolved.Values[reference.ValueId].Value
                (ProvenanceValue.Text "fresh-content")
                "Current value content is resolved outside the retained memo."

        testCase "repointing an assignment to another value with the memo retained projects the new value id"
        <| fun _ ->
            let before =
                branchFixture ()
                |> fun session -> {
                    session with
                        Values =
                            session.Values
                            |> Map.add "value-x-two" (value "value-x-two" "property-x" "x-two")
                }

            let _, memoized = resolveWithMemo "node-d" before
            let memoBefore = memoized.ReachabilityMemo["node-d"]
            let topologyBefore = memoized.AvailabilityTopologyRevision

            let changed =
                memoized
                |> runCommand (
                    assignNodeValue
                        (Set.singleton "node-b")
                        (nodeDraft "X" "x-two")
                        (OverwriteAssignments(Map.ofList [ "node-b", "assignment-x" ]))
                )

            let references, resolved = resolveWithMemo "node-d" changed
            let reference = references |> referencesFor "assignment-x" |> List.exactlyOne

            Expect.equal changed.AvailabilityTopologyRevision topologyBefore "Repointing is value-only."
            Expect.equal resolved.ReachabilityMemo["node-d"] memoBefore "The reachability memo survives."
            Expect.equal reference.ValueId "value-x-two" "Materialization reads the assignment's current value ID."
            Expect.isFalse (resolved.Values.ContainsKey "value-x") "The unreferenced former value is cleaned up."

        testCase "a propagated node annotation with exactly one origin is edited at its owner"
        <| fun _ ->
            let before = branchFixture ()

            let reference =
                resolve "node-d" before |> referencesFor "assignment-x" |> List.exactlyOne

            let actual =
                before
                |> runCommand (editAvailableReferences "node-d" [ reference ] (nodeContent "X" "edited-at-origin"))

            Expect.isEmpty actual.Nodes["node-d"].Assignments "The receiving node gains no ownership."

            let ownerAssignment = actual.Nodes["node-b"].Assignments["assignment-x"]

            Expect.equal
                actual.Values[ownerAssignment.ValueId].Value
                (ProvenanceValue.Text "edited-at-origin")
                "The originating node assignment is edited."

        testCase "a propagated node annotation with several origins is refused"
        <| fun _ ->
            let baseSession = branchFixture ()

            let secondOrigin = {
                nodeAssignment "assignment-x-second" "value-x" with
                    Lineage = AssignmentLineage.Created
            }

            let cNode = {
                baseSession.Nodes["node-c"] with
                    Assignments = Map.ofList [ secondOrigin.Id, secondOrigin ]
            }

            let cToD =
                structuralProcess "process-cd" [
                    link "link-cd" (ProcessLinkShape.Between("node-c", "node-d"))
                ] []

            let before = {
                baseSession with
                    Nodes = baseSession.Nodes |> Map.add cNode.Id cNode
                    Processes = baseSession.Processes |> Map.add cToD.Id cToD
            }

            let references =
                resolve "node-d" before
                |> List.filter (fun reference ->
                    reference.AssignmentId = "assignment-x"
                    || reference.AssignmentId = "assignment-x-second"
                )

            let result =
                editAvailableReferences "node-d" references (nodeContent "X" "ambiguous") before

            match result with
            | Error(AmbiguousPooledEdit(_, assignmentIds)) ->
                Expect.equal
                    assignmentIds
                    (Set.ofList [ "assignment-x"; "assignment-x-second" ])
                    "Every competing origin is reported."
            | outcome -> failtestf "Expected AmbiguousPooledEdit but got %A" outcome

            Expect.equal before.Nodes["node-b"].Assignments["assignment-x"].ValueId "value-x" "Nothing mutated."

        testCase "a propagated process annotation with exactly one originating link reference is editable"
        <| fun _ ->
            let before = branchFixture ()

            let reference =
                resolve "node-d" before |> referencesFor "assignment-p" |> List.exactlyOne

            let actual =
                before
                |> runCommand (editAvailableReferences "node-d" [ reference ] (nodeContent "P" "process-edited"))

            let assignment = actual.Processes["process-ab"].Assignments["assignment-p"]

            Expect.equal
                actual.Values[assignment.ValueId].Value
                (ProvenanceValue.Text "process-edited")
                "The exact originating process-link occurrence is edited."

        testCase "a pooled connector stays ambiguous even when its references share one assignment id"
        <| fun _ ->
            let p =
                processAssignment "assignment-pooled" "value-pooled" [ "link-ab"; "link-ac" ]

            let nodes =
                [ "node-a"; "node-b"; "node-c"; "node-d" ]
                |> List.map (fun nodeId -> nodeId, node nodeId nodeId [])
                |> Map.ofList

            let processes =
                [
                    structuralProcess "process-pooled" [
                        link "link-ab" (ProcessLinkShape.Between("node-a", "node-b"))
                        link "link-ac" (ProcessLinkShape.Between("node-a", "node-c"))
                    ] [ p ]
                    structuralProcess "process-bd" [
                        link "link-bd" (ProcessLinkShape.Between("node-b", "node-d"))
                    ] []
                    structuralProcess "process-cd" [
                        link "link-cd" (ProcessLinkShape.Between("node-c", "node-d"))
                    ] []
                ]
                |> List.map (fun item -> item.Id, item)
                |> Map.ofList

            let before = {
                empty with
                    Nodes = nodes
                    Processes = processes
                    Properties = Map.ofList [ "property-pooled", property "property-pooled" "Pooled" ]
                    Values =
                        Map.ofList [
                            "value-pooled", value "value-pooled" "property-pooled" "pooled"
                        ]
            }

            let reference = resolve "node-d" before |> referencesFor p.Id |> List.exactlyOne

            Expect.equal
                reference.OriginatingLinkIds
                (Set.ofList [ "link-ab"; "link-ac" ])
                "The pooled evidence retains two backing link references."

            let result =
                editAvailableReferences "node-d" [ reference ] (nodeContent "Pooled" "refused") before

            match result with
            | Error(AmbiguousPooledEdit(linkIds, assignmentIds)) ->
                Expect.equal linkIds reference.OriginatingLinkIds "Both backing links cause ambiguity."
                Expect.equal assignmentIds (Set.singleton p.Id) "One shared assignment ID does not make it editable."
            | outcome -> failtestf "Expected AmbiguousPooledEdit but got %A" outcome

        testCase "reverse-connection-local availability is not editable at the receiver"
        <| fun _ ->
            let before = branchFixture ()

            let reference =
                resolve "node-a" before |> referencesFor "assignment-x" |> List.exactlyOne

            let result =
                editAvailableReferences "node-a" [ reference ] (nodeContent "X" "read-only") before

            Expect.equal
                result
                (Error(ReadOnlyReverseLocalEdit("assignment-x", "link-ab")))
                "Reverse-local availability is grouping-only."

            Expect.equal before.Nodes["node-b"].Assignments["assignment-x"].ValueId "value-x" "Nothing mutated."

        testCase "a propagated reference cannot be removed at the receiving node"
        <| fun _ ->
            let before = branchFixture ()

            let reference =
                resolve "node-d" before |> referencesFor "assignment-x" |> List.exactlyOne

            let result = removeAvailableReferences "node-d" [ reference ] before

            Expect.equal
                result
                (Error(PropagatedRemovalAtReceiver("assignment-x", "node-d")))
                "Removal directs the user back to the owner."

            Expect.isTrue
                (before.Nodes["node-b"].Assignments.ContainsKey "assignment-x")
                "The owning assignment remains."
    ]

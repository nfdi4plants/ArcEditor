module CanonicalModelTests

open Expecto
open Swate.Components.Page.ProvenanceGrouping.Identifiers
open Swate.Components.Page.ProvenanceGrouping.Values
open Swate.Components.Page.ProvenanceGrouping.Domain
open Swate.Components.Page.ProvenanceGrouping.MutationTypes
open Swate.Components.Page.ProvenanceGrouping.ProjectionTypes
open Swate.Components.Page.ProvenanceGrouping.Model

let private sampleKind = {
    Id = "canonical:endpoint:sample"
    Label = "Sample"
}

let private dataKind = {
    Id = "canonical:endpoint:data"
    Label = "Data"
}

let private source id = { Id = id; Name = id }

let private layer id sourceId = {
    Id = id
    Label = id
    Source = source sourceId
    InputEndpoints = Map.empty
    OutputEndpoints = Map.empty
    StructuralProcessIds = Set.empty
}

let private withLayers (layers: ProvenanceLayer list) (session: ProvenanceSession) = {
    session with
        Layers = layers |> List.map (fun item -> item.Id, item) |> Map.ofList
        LayerOrder = layers |> List.map _.Id
        ActiveLayerId = layers.Head.Id
}

let private endpoint layerId side nodeId headerText position = {
    Key = {
        LayerId = layerId
        Side = side
        NodeId = nodeId
    }
    Header = { Kind = sampleKind; Text = headerText }
    LayerOrderPosition = position
}

let private expectOk =
    function
    | Ok value -> value
    | Error error -> failtestf "Expected Ok but received %A" error

let private emptyProcess id layerId = {
    Id = id
    OriginLayerId = layerId
    Name = Some id
    Links = Map.empty
    Assignments = Map.empty
}

let private processLink id shape = { Id = id; Shape = shape }

let private processAssignment id coveredLinkIds = {
    Id = id
    ValueId = "value-one"
    PropertyKind = AssignmentPropertyKind.Generic
    CoveredLinkIds = coveredLinkIds
    ContainerReferenceValueId = None
    ReferenceSlotId = None
    Lineage = AssignmentLineage.Created
}

let private expectInvariantError =
    function
    | Error(InconsistentCanonicalState details) ->
        Expect.isNotEmpty details "Invariant errors must explain the rejected state."
    | Error error -> failtestf "Expected InconsistentCanonicalState but received %A" error
    | Ok _ -> failtest "Expected InconsistentCanonicalState but operation succeeded."

let private identityTests =
    testList "canonical identity" [
        testCase "equal kind and name resolve to one canonical node across side, layer and source"
        <| fun _ ->
            let initial =
                empty
                |> withLayers [
                    layer "layer-one" "source-one"
                    layer "layer-two" "source-two"
                ]

            let nodeId, afterFirstEnsure = ensureNode sampleKind "S1" initial

            let assignment = {
                Id = "assignment-one"
                ValueId = "value-one"
                PropertyKind = AssignmentPropertyKind.Generic
                TargetSource = None
                Lineage = AssignmentLineage.Created
            }

            let assigned = {
                afterFirstEnsure with
                    Nodes =
                        afterFirstEnsure.Nodes
                        |> Map.change
                            nodeId
                            (Option.map (fun node -> {
                                node with
                                    Assignments = node.Assignments |> Map.add assignment.Id assignment
                            }))
            }

            let appearances = [
                "layer-one", ProvenanceSide.Input, "input-one", 0
                "layer-one", ProvenanceSide.Output, "output-one", 1
                "layer-two", ProvenanceSide.Input, "input-two", 7
                "layer-two", ProvenanceSide.Output, "output-two", 4
            ]

            let finalSession, resolvedIds =
                appearances
                |> List.fold
                    (fun (session, ids) (layerId, side, header, order) ->
                        let resolvedId, afterEnsure = ensureNode sampleKind "S1" session

                        let afterEndpoint =
                            addLayerEndpoint (endpoint layerId side resolvedId header order) afterEnsure
                            |> expectOk

                        afterEndpoint, resolvedId :: ids
                    )
                    (assigned, [])

            Expect.equal (resolvedIds |> Set.ofList) (Set.singleton nodeId) "All appearances must share one node."
            Expect.equal finalSession.Nodes.Count 1 "Only one canonical node must exist."
            Expect.equal finalSession.Nodes[nodeId].Assignments.Count 1 "Node assignments must remain stored once."

            Expect.equal
                (nodeAppearances finalSession nodeId |> List.length)
                4
                "All four appearances must be discoverable."

        testCase "names differing only in case are distinct nodes"
        <| fun _ ->
            let upperId, session = ensureNode sampleKind "S1" empty
            let lowerId, session = ensureNode sampleKind "s1" session
            Expect.notEqual upperId lowerId "Case must be significant."
            Expect.equal session.Nodes.Count 2 "Both nodes must be retained."

        testCase "names differing only in leading or trailing whitespace are distinct nodes"
        <| fun _ ->
            let plainId, session = ensureNode sampleKind "S1" empty
            let leadingId, session = ensureNode sampleKind " S1" session
            let trailingId, session = ensureNode sampleKind "S1 " session

            Expect.equal
                ([ plainId; leadingId; trailingId ] |> Set.ofList |> Set.count)
                3
                "Whitespace must be significant."

            Expect.equal session.Nodes.Count 3 "All exact names must be retained."

        testCase "different endpoint kinds with the same name are distinct nodes"
        <| fun _ ->
            let sampleId, session = ensureNode sampleKind "S1" empty
            let dataId, session = ensureNode dataKind "S1" session
            Expect.notEqual sampleId dataId "Kind ID must participate in canonical identity."
            Expect.equal session.Nodes.Count 2 "Both kinds must be retained."

        testCase "differently normalized Unicode names are distinct nodes"
        <| fun _ ->
            let precomposed = "\u00E9"
            let decomposed = "e\u0301"
            let precomposedId, session = ensureNode sampleKind precomposed empty
            let decomposedId, session = ensureNode sampleKind decomposed session
            Expect.notEqual precomposedId decomposedId "Unicode normalization must not be applied."
            Expect.equal session.Nodes.Count 2 "Both exact Unicode strings must be retained."

        testCase "names differing only in a data selector are distinct nodes"
        <| fun _ ->
            let fileId, session = ensureNode dataKind "f.csv" empty
            let rowId, session = ensureNode dataKind "f.csv#row=2" session
            Expect.notEqual fileId rowId "A selector is part of the exact name."
            Expect.equal session.Nodes.Count 2 "Both data identities must be retained."
    ]

let private endpointTests =
    testList "layer endpoints" [
        testCase "a layer endpoint is identified by layer, side and node only"
        <| fun _ ->
            let initial =
                empty
                |> withLayers [
                    layer "layer-one" "source-one"
                    layer "layer-two" "source-two"
                ]

            let nodeId, session = ensureNode sampleKind "S1" initial
            let first = endpoint "layer-one" ProvenanceSide.Input nodeId "first header" 2
            let afterFirst = addLayerEndpoint first session |> expectOk

            let duplicate = endpoint "layer-one" ProvenanceSide.Input nodeId "changed header" 99

            Expect.equal
                (addLayerEndpoint duplicate afterFirst)
                (Error(DuplicateEndpointAppearance duplicate.Key))
                "Header and order must not alter appearance identity."

            let otherSide = endpoint "layer-one" ProvenanceSide.Output nodeId "output" 3
            let otherLayer = endpoint "layer-two" ProvenanceSide.Input nodeId "other layer" 4

            let finalSession =
                afterFirst
                |> addLayerEndpoint otherSide
                |> expectOk
                |> addLayerEndpoint otherLayer
                |> expectOk

            Expect.equal (nodeAppearances finalSession nodeId |> List.length) 3 "Other sides and layers must succeed."

            let missingLayer = endpoint "missing-layer" ProvenanceSide.Input nodeId "missing" 0

            Expect.equal
                (addLayerEndpoint missingLayer finalSession)
                (Error(LayerNotFound "missing-layer"))
                "Missing layer is typed."

            let missingNode =
                endpoint "layer-one" ProvenanceSide.Input "missing-node" "missing" 0

            Expect.equal
                (addLayerEndpoint missingNode finalSession)
                (Error(NodeNotFound "missing-node"))
                "Missing node is typed."

        testCase "header and layer order position are independent per appearance"
        <| fun _ ->
            let initial =
                empty
                |> withLayers [
                    layer "layer-one" "source-one"
                    layer "layer-two" "source-two"
                ]

            let nodeId, session = ensureNode sampleKind "S1" initial
            let first = endpoint "layer-one" ProvenanceSide.Input nodeId "header one" 1
            let second = endpoint "layer-two" ProvenanceSide.Input nodeId "header two" 8

            let session =
                session
                |> addLayerEndpoint first
                |> expectOk
                |> addLayerEndpoint second
                |> expectOk

            let updatedFirst = {
                first with
                    Header = {
                        first.Header with
                            Text = "updated one"
                    }
                    LayerOrderPosition = 13
            }

            let updatedSession = {
                session with
                    Layers =
                        session.Layers
                        |> Map.change
                            "layer-one"
                            (Option.map (fun item -> {
                                item with
                                    InputEndpoints = item.InputEndpoints |> Map.add nodeId updatedFirst
                            }))
            }

            let appearances = nodeAppearances updatedSession nodeId

            let actualFirst =
                appearances |> List.find (fun item -> item.Key.LayerId = "layer-one")

            let actualSecond =
                appearances |> List.find (fun item -> item.Key.LayerId = "layer-two")

            Expect.equal
                (actualFirst.Header.Text, actualFirst.LayerOrderPosition)
                ("updated one", 13)
                "First appearance must update independently."

            Expect.equal
                (actualSecond.Header.Text, actualSecond.LayerOrderPosition)
                ("header two", 8)
                "Second appearance must remain unchanged."
    ]

let private structuralTests =
    testList "structural helpers" [
        testCase "addProcess rejects a link ID already owned by another process"
        <| fun _ ->
            let initial = empty |> withLayers [ layer "layer-one" "source-one" ]
            let nodeId, initial = ensureNode sampleKind "focus" initial

            let firstProcess = {
                emptyProcess "process-one" "layer-one" with
                    Links =
                        Map.ofList [
                            "shared-link", processLink "shared-link" ProcessLinkShape.Endpointless
                        ]
            }

            let beforeRejectedAdd = addProcess firstProcess initial |> expectOk

            let secondProcess = {
                emptyProcess "process-two" "layer-one" with
                    Links =
                        Map.ofList [
                            "shared-link", processLink "shared-link" (ProcessLinkShape.InputOnly nodeId)
                        ]
            }

            addProcess secondProcess beforeRejectedAdd |> expectInvariantError

            Expect.equal
                beforeRejectedAdd.Processes
                (Map.ofList [ "process-one", firstProcess ])
                "Rejected process must not change the existing process map."

            Expect.equal
                beforeRejectedAdd.Layers["layer-one"].StructuralProcessIds
                (Set.singleton "process-one")
                "Rejected process must not change layer ownership."

        testCase "addLink rejects a link ID already owned by another process"
        <| fun _ ->
            let initial = empty |> withLayers [ layer "layer-one" "source-one" ]
            let nodeId, initial = ensureNode sampleKind "focus" initial

            let initial =
                addProcess (emptyProcess "process-one" "layer-one") initial |> expectOk

            let initial =
                addProcess (emptyProcess "process-two" "layer-one") initial |> expectOk

            let existingLink = processLink "shared-link" ProcessLinkShape.Endpointless
            let beforeRejectedAdd = addLink "process-one" existingLink initial |> expectOk
            let duplicateLink = processLink "shared-link" (ProcessLinkShape.OutputOnly nodeId)
            addLink "process-two" duplicateLink beforeRejectedAdd |> expectInvariantError

            Expect.equal
                beforeRejectedAdd.Processes["process-one"].Links
                (Map.ofList [ "shared-link", existingLink ])
                "Original link owner must remain unchanged."

            Expect.isEmpty
                beforeRejectedAdd.Processes["process-two"].Links
                "Rejected link must not appear on the target process."

        testCase "addProcess rejects an assignment with empty link coverage"
        <| fun _ ->
            let initial = empty |> withLayers [ layer "layer-one" "source-one" ]
            let link = processLink "link-one" ProcessLinkShape.Endpointless

            let invalidProcess = {
                emptyProcess "process-one" "layer-one" with
                    Links = Map.ofList [ link.Id, link ]
                    Assignments =
                        Map.ofList [
                            "assignment-one", processAssignment "assignment-one" Set.empty
                        ]
            }

            addProcess invalidProcess initial |> expectInvariantError
            Expect.isEmpty initial.Processes "Rejected process must not be added."

            Expect.isEmpty
                initial.Layers["layer-one"].StructuralProcessIds
                "Rejected process must not gain layer ownership."

            let foreignCoverageProcess = {
                invalidProcess with
                    Assignments =
                        Map.ofList [
                            "assignment-one", processAssignment "assignment-one" (Set.singleton "foreign-link")
                        ]
            }

            Expect.equal
                (addProcess foreignCoverageProcess initial)
                (Error(LinkNotFound "foreign-link"))
                "Every covered link must belong to the submitted process."

            Expect.isEmpty initial.Processes "Rejected foreign coverage must not add the process."

        testCase "addProcess rejects a link map key that differs from the embedded link ID"
        <| fun _ ->
            let initial = empty |> withLayers [ layer "layer-one" "source-one" ]

            let invalidProcess = {
                emptyProcess "process-one" "layer-one" with
                    Links =
                        Map.ofList [
                            "map-key", processLink "embedded-link-id" ProcessLinkShape.Endpointless
                        ]
            }

            addProcess invalidProcess initial |> expectInvariantError
            Expect.isEmpty initial.Processes "Rejected process must not be added."

            Expect.isEmpty
                initial.Layers["layer-one"].StructuralProcessIds
                "Rejected process must not gain layer ownership."

        testCase "addProcess rejects an assignment map key that differs from the embedded assignment ID"
        <| fun _ ->
            let initial = empty |> withLayers [ layer "layer-one" "source-one" ]
            let link = processLink "link-one" ProcessLinkShape.Endpointless

            let invalidProcess = {
                emptyProcess "process-one" "layer-one" with
                    Links = Map.ofList [ link.Id, link ]
                    Assignments =
                        Map.ofList [
                            "map-key", processAssignment "embedded-assignment-id" (Set.singleton link.Id)
                        ]
            }

            addProcess invalidProcess initial |> expectInvariantError
            Expect.isEmpty initial.Processes "Rejected process must not be added."

            Expect.isEmpty
                initial.Layers["layer-one"].StructuralProcessIds
                "Rejected process must not gain layer ownership."

        testCase "shrinking coverage removes an assignment from the reverse index for the dropped link only"
        <| fun _ ->
            let assignment covered = {
                Id = "assignment-one"
                ValueId = "value-one"
                PropertyKind = AssignmentPropertyKind.Generic
                CoveredLinkIds = covered
                ContainerReferenceValueId = None
                ReferenceSlotId = None
                Lineage = AssignmentLineage.Created
            }

            let structuralProcess covered = {
                emptyProcess "process-one" "layer-one" with
                    Links =
                        [
                            "link-one",
                            {
                                Id = "link-one"
                                Shape = ProcessLinkShape.Endpointless
                            }
                            "link-two",
                            {
                                Id = "link-two"
                                Shape = ProcessLinkShape.Endpointless
                            }
                        ]
                        |> Map.ofList
                    Assignments = Map.ofList [ "assignment-one", assignment covered ]
            }

            let before = {
                empty with
                    Processes =
                        Map.ofList [
                            "process-one", structuralProcess (Set.ofList [ "link-one"; "link-two" ])
                        ]
            }

            let after = {
                before with
                    Processes =
                        Map.ofList [
                            "process-one", structuralProcess (Set.singleton "link-two")
                        ]
            }

            let beforeIndex = linkAssignments before
            let afterIndex = linkAssignments after
            Expect.equal beforeIndex["link-one"] (Set.singleton "assignment-one") "Dropped link starts covered."
            Expect.equal beforeIndex["link-two"] (Set.singleton "assignment-one") "Retained link starts covered."
            Expect.isFalse (afterIndex.ContainsKey "link-one") "Dropped coverage must disappear immediately."
            Expect.equal afterIndex["link-two"] (Set.singleton "assignment-one") "Retained coverage must remain."

        testCase "incident links classify all four link shapes"
        <| fun _ ->
            let initial = empty |> withLayers [ layer "layer-one" "source-one" ]
            let focusId, session = ensureNode sampleKind "focus" initial
            let inputId, session = ensureNode sampleKind "other-input" session
            let outputId, session = ensureNode sampleKind "other-output" session

            let missingLayerProcess = emptyProcess "missing-layer-process" "missing-layer"

            Expect.equal
                (addProcess missingLayerProcess session)
                (Error(LayerNotFound "missing-layer"))
                "A process must reference an existing origin layer."

            Expect.equal
                (addLink
                    "missing-process"
                    {
                        Id = "orphan"
                        Shape = ProcessLinkShape.Endpointless
                    }
                    session)
                (Error(ProcessNotFound "missing-process"))
                "A link owner must exist."

            let structuralProcess = emptyProcess "process-one" "layer-one"
            let session = addProcess structuralProcess session |> expectOk

            Expect.equal
                (addLink
                    "process-one"
                    {
                        Id = "bad-node"
                        Shape = ProcessLinkShape.InputOnly "missing-node"
                    }
                    session)
                (Error(NodeNotFound "missing-node"))
                "Link endpoints must reference existing nodes."

            let links = [
                {
                    Id = "between-outgoing"
                    Shape = ProcessLinkShape.Between(focusId, outputId)
                }
                {
                    Id = "between-incoming"
                    Shape = ProcessLinkShape.Between(inputId, focusId)
                }
                {
                    Id = "input-only"
                    Shape = ProcessLinkShape.InputOnly focusId
                }
                {
                    Id = "output-only"
                    Shape = ProcessLinkShape.OutputOnly focusId
                }
                {
                    Id = "endpointless"
                    Shape = ProcessLinkShape.Endpointless
                }
            ]

            let finalSession =
                links
                |> List.fold (fun state link -> addLink "process-one" link state |> expectOk) session

            let incidence = incidentLinks finalSession focusId
            Expect.equal incidence.OutgoingLinkIds [ "between-outgoing" ] "Between input is outgoing."
            Expect.equal incidence.IncomingLinkIds [ "between-incoming" ] "Between output is incoming."

            Expect.equal
                incidence.OneSidedLinkIds
                [ "input-only"; "output-only" ]
                "Both one-sided shapes are classified separately from direction."

            Expect.isFalse
                (incidence.OutgoingLinkIds
                 @ incidence.IncomingLinkIds
                 @ incidence.OneSidedLinkIds
                 |> List.contains "endpointless")
                "Endpointless links are absent from incidence."

            let storedShapes =
                finalSession.Processes["process-one"].Links
                |> Map.toList
                |> List.map (snd >> _.Shape)
                |> Set.ofList

            Expect.equal
                storedShapes
                (links |> List.map _.Shape |> Set.ofList)
                "All link shapes must be preserved exactly."

            Expect.equal
                finalSession.Layers["layer-one"].StructuralProcessIds
                (Set.singleton "process-one")
                "The origin layer owns the structural process."
    ]

let tests =
    testList "CanonicalModel" [ identityTests; endpointTests; structuralTests ]

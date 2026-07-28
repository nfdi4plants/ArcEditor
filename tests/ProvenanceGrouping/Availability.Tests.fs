module CanonicalAvailabilityTests

open Expecto
open Swate.Components.Page.ProvenanceGrouping.Identifiers
open Swate.Components.Page.ProvenanceGrouping.Values
open Swate.Components.Page.ProvenanceGrouping.Domain
open Swate.Components.Page.ProvenanceGrouping.AvailabilityTypes
open Swate.Components.Page.ProvenanceGrouping.ProjectionTypes
open Swate.Components.Page.ProvenanceGrouping.Model
open Swate.Components.Page.ProvenanceGrouping.Availability

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
    ]

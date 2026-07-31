module EditorActionsTests

open Expecto
open Swate.Components.Page.ProvenanceGrouping
open Swate.Components.Page.ProvenanceGrouping.Identifiers
open Swate.Components.Page.ProvenanceGrouping.Values
open Swate.Components.Page.ProvenanceGrouping.Domain
open Swate.Components.Page.ProvenanceGrouping.ProjectionTypes

let private sampleKind: ProvenanceKind = {
    Id = "test:endpoint:sample"
    Label = "Sample"
}

let private endpoint nodeId position : LayerEndpoint = {
    Key = {
        LayerId = "layer-1"
        Side = ProvenanceSide.Input
        NodeId = nodeId
    }
    Header = {
        Kind = sampleKind
        Text = $"Endpoint {nodeId}"
    }
    LayerOrderPosition = position
}

let private layer inputEndpoints outputEndpoints : ProvenanceLayer = {
    Id = "layer-1"
    Label = "Test Layer"
    Source = { Id = "src-1"; Name = "test" }
    InputEndpoints = inputEndpoints |> List.map (fun (id, pos) -> id, endpoint id pos) |> Map.ofList
    OutputEndpoints = outputEndpoints |> List.map (fun (id, pos) -> id, endpoint id pos) |> Map.ofList
    StructuralProcessIds = Set.empty
}

let private group side nodeIds : DisplayGroup = {
    Id = $"group-{side}"
    Side = side
    CanonicalNodeIds = Set.ofList nodeIds
    EndpointKeys = Set.empty
    ProcessLinkIds = Set.empty
    Annotations = []
}

let tests =
    testList "EditorActions" [
        test "sorts by LayerOrderPosition, not map order or node name" {
            let testLayer =
                layer [ "z-node", 2; "a-node", 0; "m-node", 1 ] [ "output-z", 2; "output-a", 0; "output-m", 1 ]

            let inputGroup = group ProvenanceSide.Input [ "z-node"; "a-node"; "m-node" ]
            let outputGroup = group ProvenanceSide.Output [ "output-z"; "output-a"; "output-m" ]

            let result = EditorActions.orderedMemberPairs testLayer inputGroup outputGroup

            Expect.isSome result "equal-count groups should produce pairs"
            let pairs = result.Value
            Expect.equal pairs.Length 3 "should have 3 pairs"
            Expect.equal (pairs.[0]) ("a-node", "output-a") "position 0 pair"
            Expect.equal (pairs.[1]) ("m-node", "output-m") "position 1 pair"
            Expect.equal (pairs.[2]) ("z-node", "output-z") "position 2 pair"
        }

        test "returns None when input and output counts differ" {
            let testLayer = layer [ "a", 0; "b", 1 ] [ "x", 0 ]

            let inputGroup = group ProvenanceSide.Input [ "a"; "b" ]
            let outputGroup = group ProvenanceSide.Output [ "x" ]

            let result = EditorActions.orderedMemberPairs testLayer inputGroup outputGroup
            Expect.isNone result "mismatched counts should return None"
        }

        test "handles single-member groups" {
            let testLayer = layer [ "only-in", 0 ] [ "only-out", 0 ]
            let inputGroup = group ProvenanceSide.Input [ "only-in" ]
            let outputGroup = group ProvenanceSide.Output [ "only-out" ]

            let result = EditorActions.orderedMemberPairs testLayer inputGroup outputGroup

            Expect.isSome result "single-member groups should pair"
            Expect.equal result.Value [ "only-in", "only-out" ] "single pair"
        }

        test "position gaps do not affect pairing order" {
            let testLayer =
                layer [ "first", 10; "second", 50; "third", 100 ] [ "out-1", 5; "out-2", 25; "out-3", 75 ]

            let inputGroup = group ProvenanceSide.Input [ "third"; "first"; "second" ]
            let outputGroup = group ProvenanceSide.Output [ "out-3"; "out-1"; "out-2" ]

            let result = EditorActions.orderedMemberPairs testLayer inputGroup outputGroup
            Expect.isSome result "should pair despite gaps"

            let pairs = result.Value
            Expect.equal (pairs.[0]) ("first", "out-1") "lowest positions pair"
            Expect.equal (pairs.[1]) ("second", "out-2") "middle positions pair"
            Expect.equal (pairs.[2]) ("third", "out-3") "highest positions pair"
        }
    ]

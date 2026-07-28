module CanonicalPreparationTests

open Expecto
open Swate.Components.Page.ProvenanceGrouping.Identifiers
open Swate.Components.Page.ProvenanceGrouping.Values
open Swate.Components.Page.ProvenanceGrouping.Domain
open Swate.Components.Page.ProvenanceGrouping.AvailabilityTypes
open Swate.Components.Page.ProvenanceGrouping.ProjectionTypes
open Swate.Components.Page.ProvenanceGrouping.Commands
open Swate.Components.Page.ProvenanceGrouping.Model
open Swate.Components.Page.ProvenanceGrouping.CanonicalSession

let private expectOk =
    function
    | Ok value -> value
    | Error error -> failtestf "Expected Ok but got %A" error

let private endpointKind = {
    Id = "canonical:endpoint:sample"
    Label = "Sample"
}

let private header = {
    Name = "Temperature"
    TermSource = Some "TEST"
    TermAccession = Some "TEST:temperature"
}

let private endpoint layerId side nodeId position : LayerEndpoint = {
    Key = {
        LayerId = layerId
        Side = side
        NodeId = nodeId
    }
    Header = { Kind = endpointKind; Text = nodeId }
    LayerOrderPosition = position
}

let private layer layerId sourceId : ProvenanceLayer = {
    Id = layerId
    Label = layerId
    Source = { Id = sourceId; Name = sourceId }
    InputEndpoints =
        Map.ofList [
            "node-a", endpoint layerId ProvenanceSide.Input "node-a" 0
        ]
    OutputEndpoints =
        Map.ofList [
            "node-b", endpoint layerId ProvenanceSide.Output "node-b" 0
        ]
    StructuralProcessIds =
        if layerId = "layer-one" then
            Set.singleton "process-ab"
        else
            Set.empty
}

let private canonicalNode id (assignments: NodeAssignment list) : CanonicalNode = {
    Id = id
    Key = { KindId = endpointKind.Id; Name = id }
    Kind = endpointKind
    Name = id
    Assignments =
        assignments
        |> List.map (fun assignment -> assignment.Id, assignment)
        |> Map.ofList
}

let private fixture () =
    let property = {
        Id = "property-temperature"
        Category = header
    }

    let value = {
        Id = "value-temperature"
        PropertyId = property.Id
        Value = ProvenanceValue.Text "before"
        Unit = None
    }

    let assignment = {
        Id = "assignment-temperature"
        ValueId = value.Id
        PropertyKind = AssignmentPropertyKind.Generic
        TargetSource = None
        Lineage = AssignmentLineage.Loaded
    }

    let processLink = {
        Id = "link-ab"
        Shape = ProcessLinkShape.Between("node-a", "node-b")
    }

    let structuralProcess = {
        Id = "process-ab"
        OriginLayerId = "layer-one"
        Name = Some "A to B"
        Links = Map.ofList [ processLink.Id, processLink ]
        Assignments = Map.empty
    }

    {
        empty with
            Nodes =
                Map.ofList [
                    "node-a", canonicalNode "node-a" [ assignment ]
                    "node-b", canonicalNode "node-b" []
                ]
            Processes = Map.ofList [ structuralProcess.Id, structuralProcess ]
            Properties = Map.ofList [ property.Id, property ]
            Values = Map.ofList [ value.Id, value ]
            Layers =
                Map.ofList [
                    "layer-one", layer "layer-one" "source-one"
                    "layer-two", layer "layer-two" "source-two"
                ]
            LayerOrder = [ "layer-one"; "layer-two" ]
            ActiveLayerId = "layer-one"
    }
    |> refreshLayer "layer-one"
    |> expectOk
    |> refreshLayer "layer-two"
    |> expectOk
    |> activateLayer "layer-one"
    |> expectOk

let private updatedContent text = {
    Category = header
    Value = ProvenanceValue.Text text
    Unit = None
}

let private editOwned text session =
    editNodeAssignment "node-a" "assignment-temperature" (updatedContent text) session
    |> Result.map (fun effect -> commit effect session)
    |> expectOk

let private annotationTexts projection =
    projection.Groups
    |> List.collect _.Annotations
    |> List.choose (fun annotation ->
        match annotation.Key with
        | NodeValue(_, TextIdentity text, _) -> Some text
        | _ -> None
    )
    |> Set.ofList

let tests =
    testList "CanonicalPreparation" [
        testCase "a command leaves the active layer current and other layers stale"
        <| fun _ ->
            let actual = fixture () |> editOwned "after"

            Expect.isFalse actual.LayerProjections["layer-one"].Stale "The active cache is refreshed."
            Expect.contains (annotationTexts actual.LayerProjections["layer-one"]) "after" "It has the new value."
            Expect.isTrue actual.LayerProjections["layer-two"].Stale "The inactive cache remains lazy."

        testCase "activating a stale layer refreshes it"
        <| fun _ ->
            let edited = fixture () |> editOwned "after"
            let actual = activateLayer "layer-two" edited |> expectOk

            Expect.equal actual.ActiveLayerId "layer-two" "The target becomes active."
            Expect.isFalse actual.LayerProjections["layer-two"].Stale "The target cache is current."
            Expect.contains (annotationTexts actual.LayerProjections["layer-two"]) "after" "Pending edits are visible."

        testCase "refreshing a layer does not clear the mutation journal"
        <| fun _ ->
            let edited = fixture () |> editOwned "after"
            let journal = edited.MutationJournal
            let actual = refreshLayer "layer-two" edited |> expectOk
            Expect.sequenceEqual actual.MutationJournal journal "Refresh preserves every journal entry."

        testCase "a refreshed layer records both current revisions"
        <| fun _ ->
            let edited = fixture () |> editOwned "after"
            let actual = refreshLayer "layer-two" edited |> expectOk
            let projection = actual.LayerProjections["layer-two"]

            Expect.equal
                projection.TopologyRevision
                actual.AvailabilityTopologyRevision
                "The topology revision is current."

            Expect.equal projection.ValueRevision actual.AnnotationValueRevision "The value revision is current."
            Expect.isFalse projection.Stale "The refreshed cache is not stale."

        testCase "an edit made through one appearance is visible on every other appearance after refresh"
        <| fun _ ->
            let session = fixture ()

            let reference = {
                AssignmentId = "assignment-temperature"
                ValueId = "value-temperature"
                Owner = NodeOwner "node-a"
                Relation = OwnedNode
                OriginatingLinkIds = Set.empty
                VisibleThroughLinkIds = Set.empty
            }

            let edited =
                editAvailableReferences "node-a" [ reference ] (updatedContent "through-appearance") session
                |> Result.map (fun effect -> commit effect session)
                |> expectOk

            let refreshed = activateLayer "layer-two" edited |> expectOk

            for layerId in [ "layer-one"; "layer-two" ] do
                Expect.contains
                    (annotationTexts refreshed.LayerProjections[layerId])
                    "through-appearance"
                    $"The shared owner is visible in {layerId}."

        testCase "a removed assignment disappears from every projection after refresh"
        <| fun _ ->
            let session = fixture ()

            let removed =
                removeNodeAssignment "node-a" "assignment-temperature" session
                |> Result.map (fun effect -> commit effect session)
                |> expectOk

            let actual = activateLayer "layer-two" removed |> expectOk

            for layerId in [ "layer-one"; "layer-two" ] do
                Expect.isEmpty
                    (annotationTexts actual.LayerProjections[layerId])
                    $"No owned or propagated projection survives in {layerId}."
    ]

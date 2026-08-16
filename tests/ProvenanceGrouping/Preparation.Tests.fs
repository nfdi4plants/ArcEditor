module CanonicalPreparationTests

open Expecto
open Swate.Components.Page.ProvenanceGrouping.Identifiers
open Swate.Components.Page.ProvenanceGrouping.Values
open Swate.Components.Page.ProvenanceGrouping.Domain
open Swate.Components.Page.ProvenanceGrouping.AvailabilityTypes
open Swate.Components.Page.ProvenanceGrouping.MutationTypes
open Swate.Components.Page.ProvenanceGrouping.ProjectionTypes
open Swate.Components.Page.ProvenanceGrouping.Commands
open Swate.Components.Page.ProvenanceGrouping.Model
open Swate.Components.Page.ProvenanceGrouping.Session

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

let private processAssignment id valueId coveredLinkIds containerValueId slotId : ProcessAssignment = {
    Id = id
    ValueId = valueId
    PropertyKind = AssignmentPropertyKind.Generic
    CoveredLinkIds = coveredLinkIds
    ContainerReferenceValueId = containerValueId
    ReferenceSlotId = slotId
    Lineage = AssignmentLineage.Loaded
}

let private addProcessAssignment (assignment: ProcessAssignment) (session: ProvenanceSession) = {
    session with
        Processes =
            session.Processes
            |> Map.change
                "process-ab"
                (Option.map (fun structuralProcess -> {
                    structuralProcess with
                        Assignments = structuralProcess.Assignments |> Map.add assignment.Id assignment
                }))
}

let private addValue (value: PropertyValueDefinition) (session: ProvenanceSession) = {
    session with
        Values = session.Values |> Map.add value.Id value
}

let private referenceValue id durableId : PropertyValueDefinition = {
    Id = id
    PropertyId = "property-temperature"
    Value =
        ProvenanceValue.Reference {
            Scheme = "test:resource"
            Id = durableId
            Label = durableId
        }
    Unit = None
}

let private withMissingContainer session =
    let container = referenceValue "value-container" "container"

    let dependent =
        processAssignment "assignment-dependent" "value-temperature" (Set.singleton "link-ab") (Some container.Id) None

    session |> addValue container |> addProcessAssignment dependent

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

        testCase "preparation refreshes every stale layer"
        <| fun _ ->
            let actual = fixture () |> editOwned "after" |> prepareForWriteback |> expectOk

            Expect.isTrue
                (actual.LayerProjections |> Map.forall (fun _ projection -> not projection.Stale))
                "Preparation resolves every layer invalidation."

            Expect.contains (annotationTexts actual.LayerProjections["layer-two"]) "after" "Inactive data is current."

        testCase "preparation records cleanup as tombstones and retains the journal"
        <| fun _ ->
            let edited = fixture () |> editOwned "after"
            let originalJournal = edited.MutationJournal

            let orphan =
                processAssignment "assignment-orphan" "value-temperature" Set.empty None None

            let actual =
                edited |> addProcessAssignment orphan |> prepareForWriteback |> expectOk

            Expect.sequenceEqual
                (actual.MutationJournal |> List.take originalJournal.Length)
                originalJournal
                "Every pre-existing mutation is retained in order."

            Expect.isTrue
                (actual.MutationJournal
                 |> List.exists (
                     function
                     | ProcessAssignmentRemoved(tombstone, _) ->
                         tombstone.OwnerId = "process-ab"
                         && tombstone.Assignment.Id = "assignment-orphan"
                     | _ -> false
                 ))
                "Cleanup appends an assignment tombstone."

        testCase "preparation advances revisions before refreshing"
        <| fun _ ->
            let session = fixture ()

            let orphan =
                processAssignment "assignment-orphan" "value-temperature" Set.empty None None

            let actual =
                session |> addProcessAssignment orphan |> prepareForWriteback |> expectOk

            Expect.equal
                actual.AvailabilityTopologyRevision
                (session.AvailabilityTopologyRevision + 1)
                "Assignment cleanup advances topology once."

            for KeyValue(_, projection) in actual.LayerProjections do
                Expect.equal
                    projection.TopologyRevision
                    actual.AvailabilityTopologyRevision
                    "Refresh observes the post-cleanup topology revision."

                Expect.equal
                    projection.ValueRevision
                    actual.AnnotationValueRevision
                    "Refresh observes the post-cleanup value revision."

        testCase "preparation deletes orphaned assignments and unreferenced value definitions"
        <| fun _ ->
            let orphanValue = {
                Id = "value-orphan"
                PropertyId = "property-temperature"
                Value = ProvenanceValue.Text "orphan"
                Unit = None
            }

            let orphanAssignment =
                processAssignment "assignment-orphan" orphanValue.Id Set.empty None None

            let actual =
                fixture ()
                |> addValue orphanValue
                |> addProcessAssignment orphanAssignment
                |> prepareForWriteback
                |> expectOk

            Expect.isFalse
                (actual.Processes["process-ab"].Assignments.ContainsKey orphanAssignment.Id)
                "The malformed assignment is removed."

            Expect.isFalse (actual.Values.ContainsKey orphanValue.Id) "Its now-unreferenced definition is removed."

        testCase "preparation rejects a container-bound assignment whose covered link lacks its reference"
        <| fun _ ->
            let actual = fixture () |> withMissingContainer |> prepareForWriteback

            Expect.equal
                actual
                (Error(MissingReferenceContainer("assignment-dependent", "value-container", Set.singleton "link-ab")))
                "The missing per-link container is a typed preparation error."

        testCase "preparation rejects two reference assignments in one slot on one link"
        <| fun _ ->
            let firstValue = referenceValue "value-reference-one" "one"
            let secondValue = referenceValue "value-reference-two" "two"

            let first =
                processAssignment
                    "assignment-reference-one"
                    firstValue.Id
                    (Set.singleton "link-ab")
                    None
                    (Some "slot-one")

            let second =
                processAssignment
                    "assignment-reference-two"
                    secondValue.Id
                    (Set.singleton "link-ab")
                    None
                    (Some "slot-one")

            let actual =
                fixture ()
                |> addValue firstValue
                |> addValue secondValue
                |> addProcessAssignment first
                |> addProcessAssignment second
                |> prepareForWriteback

            Expect.equal
                actual
                (Error(ReferenceSlotOccupied("slot-one", Set.singleton "link-ab", Set.ofList [ first.Id; second.Id ])))
                "A declared slot is unique per link."

        testCase "preparation returns an inconsistent-projection error rather than silently repairing"
        <| fun _ ->
            let session = fixture ()

            let corruptBacking =
                function
                | NodeAssignmentBacking(identity, ownerId, targetSource) ->
                    NodeAssignmentBacking(
                        {
                            identity with
                                ValueId = "conflicting-value"
                        },
                        ownerId,
                        targetSource
                    )
                | backing -> backing

            let corruptProjection = {
                session.LayerProjections["layer-one"] with
                    Groups =
                        session.LayerProjections["layer-one"].Groups
                        |> List.map (fun group -> {
                            group with
                                Annotations =
                                    group.Annotations
                                    |> List.map (fun annotation -> {
                                        annotation with
                                            Backing = corruptBacking annotation.Backing
                                    })
                        })
            }

            let inconsistent = {
                session with
                    LayerProjections = session.LayerProjections |> Map.add "layer-one" corruptProjection
            }

            match prepareForWriteback inconsistent with
            | Error(InconsistentLayerProjection("layer-one", _)) -> ()
            | actual -> failtestf "Expected an inconsistent layer projection, got %A" actual

        testCase "a failed preparation leaves the session and journal available for retry"
        <| fun _ ->
            let original = fixture () |> editOwned "after" |> withMissingContainer
            let journal = original.MutationJournal
            let result = prepareForWriteback original

            Expect.isError result "Preparation fails."
            Expect.sequenceEqual original.MutationJournal journal "The caller retains the complete retry journal."

            Expect.isTrue
                (original.Processes["process-ab"].Assignments.ContainsKey "assignment-dependent")
                "The caller's original session remains intact."
    ]

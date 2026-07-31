module CanonicalCommandsTests

open Expecto
open Swate.Components.Page.ProvenanceGrouping.Identifiers
open Swate.Components.Page.ProvenanceGrouping.Values
open Swate.Components.Page.ProvenanceGrouping.Domain
open Swate.Components.Page.ProvenanceGrouping.MutationTypes
open Swate.Components.Page.ProvenanceGrouping.ProjectionTypes
open Swate.Components.Page.ProvenanceGrouping.Model
open Swate.Components.Page.ProvenanceGrouping.Commands
open Swate.Components.Page.ProvenanceGrouping.CanonicalSession

module StoryFixtures = Swate.Components.Page.ProvenanceGrouping.StoryFixtures

module CanonicalCommand = Swate.Components.Page.ProvenanceGrouping.Commands

let private nodeKind = {
    Id = "canonical:endpoint:sample"
    Label = "Sample"
}

let private category name = {
    Name = name
    TermSource = Some "TEST"
    TermAccession = Some $"TEST:{name}"
}

let private content name value unit = {
    Category = category name
    Value = ProvenanceValue.Text value
    Unit = unit
}

let private draft name value unit : NodeAssignmentDraft = {
    Content = content name value unit
    OwnerKind = AnnotationOwnerKind.Node
    PropertyKind = AssignmentPropertyKind.Generic
}

let private processDraft name value unit : ProcessAssignmentDraft = {
    Content = content name value unit
    OwnerKind = AnnotationOwnerKind.Process
    PropertyKind = AssignmentPropertyKind.Generic
    ContainerReferenceValueId = None
    ReferenceSlotId = None
    Lineage = AssignmentLineage.Created
}

let private processReferenceDraft name reference propertyKind slot lineage : ProcessAssignmentDraft = {
    Content = {
        Category = category name
        Value = ProvenanceValue.Reference reference
        Unit = None
    }
    OwnerKind = AnnotationOwnerKind.Process
    PropertyKind = propertyKind
    ContainerReferenceValueId = None
    ReferenceSlotId = slot
    Lineage = lineage
}

let private processCatalogEntry id slot dependents : ReferenceCatalogEntry = {
    Category = category "Protocol reference"
    Reference = { Scheme = "arc"; Id = id; Label = id }
    Unit = None
    AssignmentKind = AnnotationOwnerKind.Process
    PropertyKind = AssignmentPropertyKind.Generic
    Cardinality =
        slot
        |> Option.map ReferenceCardinality.AtMostOnePerLink
        |> Option.defaultValue ReferenceCardinality.Many
    DependentProcessValues = dependents
}

let private dependent key name value : ReferenceDependentProcessValue = {
    Key = key
    Category = category name
    Value = ProvenanceValue.Text value
    Unit = None
    PropertyKind = AssignmentPropertyKind.Generic
}

let private expectOk =
    function
    | Ok value -> value
    | Error error -> failtestf "Expected Ok but received %A" error

let private installPreparation preparation session = {
    session with
        Properties =
            session.Properties
            |> Map.add preparation.PropertyDefinition.Id preparation.PropertyDefinition
        Values =
            session.Values
            |> Map.add preparation.ValueDefinition.Id preparation.ValueDefinition
}

let private addAssignment ownerId (assignment: NodeAssignment) session = {
    session with
        Nodes =
            session.Nodes
            |> Map.change
                ownerId
                (Option.map (fun node -> {
                    node with
                        Assignments = node.Assignments |> Map.add assignment.Id assignment
                }))
}

let private existingAssignment id valueId kind targetSource : NodeAssignment = {
    Id = id
    ValueId = valueId
    PropertyKind = kind
    TargetSource = targetSource
    Lineage = AssignmentLineage.Loaded
}

let private withNodes names =
    names
    |> List.fold
        (fun (ids, session) name ->
            let nodeId, session = ensureNode nodeKind name session
            nodeId :: ids, session
        )
        ([], empty)
    |> fun (ids, session) -> List.rev ids, session

let private addTestProcess (processId: StructuralProcessId) (links: ProcessLink list) session =
    let structuralProcess: StructuralProcess = {
        Id = processId
        OriginLayerId = "test-layer"
        Name = None
        Links = links |> List.map (fun link -> link.Id, link) |> Map.ofList
        Assignments = Map.empty
    }

    {
        session with
            Processes = session.Processes |> Map.add processId structuralProcess
    }

let private addProcessAssignment ownerId (assignment: ProcessAssignment) session = {
    session with
        Processes =
            session.Processes
            |> Map.change
                ownerId
                (Option.map (fun structuralProcess -> {
                    structuralProcess with
                        Assignments = structuralProcess.Assignments |> Map.add assignment.Id assignment
                }))
}

let private testLayer layerId : ProvenanceLayer = {
    Id = layerId
    Label = layerId
    Source = {
        Id = $"source:{layerId}"
        Name = layerId
    }
    InputEndpoints = Map.empty
    OutputEndpoints = Map.empty
    StructuralProcessIds = Set.empty
}

let private withTestLayer layerId session = {
    session with
        Layers = session.Layers |> Map.add layerId (testLayer layerId)
        LayerOrder = session.LayerOrder @ [ layerId ]
        ActiveLayerId =
            if session.ActiveLayerId = "" then
                layerId
            else
                session.ActiveLayerId
}

let private addTestAppearance layerId side nodeId position session =
    session
    |> addLayerEndpoint {
        Key = {
            LayerId = layerId
            Side = side
            NodeId = nodeId
        }
        Header = {
            Kind = nodeKind
            Text = if side = ProvenanceSide.Input then "Input" else "Output"
        }
        LayerOrderPosition = position
    }
    |> expectOk

let private addLayerProcess (processId: StructuralProcessId) (links: ProcessLink list) session =
    session
    |> addProcess {
        Id = processId
        OriginLayerId = "test-layer"
        Name = None
        Links = links |> List.map (fun processLink -> processLink.Id, processLink) |> Map.ofList
        Assignments = Map.empty
    }
    |> expectOk

let private link id shape : ProcessLink = { Id = id; Shape = shape }

let private processAssignments processId session =
    session.Processes[processId].Assignments |> Map.toList |> List.map snd

let private onlyProcessAssignment processId session =
    processAssignments processId session |> List.exactlyOne

let private processOwnerSelection processId assignmentId linkIds =
    Map.ofList [ processId, Map.ofList [ assignmentId, linkIds ] ]

let private nodeOwnerSelection ownerIds =
    ownerIds
    |> List.map (fun (ownerId, assignmentId) -> ownerId, Set.singleton assignmentId)
    |> Map.ofList

let private projection revision = {
    TopologyRevision = revision
    ValueRevision = revision
    Stale = false
    Groups = []
    Connectors = []
    ProcessOnlyEntries = []
    ShelfEntries = []
}

let private assignmentList nodeId session =
    session.Nodes[nodeId].Assignments |> Map.toList |> List.map snd

let private run command session =
    command session |> expectOk |> (fun effect -> commit effect session)

let private nodeAssignmentAddedMutations session =
    session.MutationJournal
    |> List.choose (
        function
        | NodeAssignmentAdded(ownerId, assignment, context) -> Some(ownerId, assignment, context)
        | _ -> None
    )

let private assignmentCommand targets value overwrite = assignNodeValue targets value overwrite

let private assignmentTests =
    testList "node assignment" [
        testCase "dropping a node value on a group assigns once per distinct canonical node"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "S1"; "S2" ]
            let targets = Set.ofList [ nodeIds[0]; nodeIds[1]; nodeIds[0] ]

            let actual =
                initial
                |> run (assignmentCommand targets (draft "Organism" "Human" None) NoOverwrite)

            Expect.equal
                (nodeIds |> List.map (fun id -> assignmentList id actual |> List.length))
                [ 1; 1 ]
                "Each distinct node is assigned once."

            Expect.equal actual.Values.Count 1 "The normalized value is shared."
            Expect.equal actual.AvailabilityTopologyRevision 1 "One command advances topology once."
            Expect.equal actual.AnnotationValueRevision 0 "Adding assignments does not advance value revision."

            Expect.equal
                (nodeAssignmentAddedMutations actual
                 |> List.map (fun (owner, _, _) -> owner)
                 |> Set.ofList)
                targets
                "The journal covers each resolved owner."

        testCase "a single node drop behaves as a group of one"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "S1" ]
            let target = nodeIds.Head

            let actual =
                initial
                |> run (assignmentCommand (Set.singleton target) (draft "Organism" "Human" None) NoOverwrite)

            Expect.equal (assignmentList target actual |> List.length) 1 "The one target receives one assignment."
            Expect.equal actual.AvailabilityTopologyRevision 1 "The single target is a topology command."

        testCase "assigning an equal header value and unit to a node is a no-op"
        <| fun _ ->
            let unit = Some(category "degree-Celsius")
            let nodeIds, initial = withNodes [ "S1" ]
            let target = nodeIds.Head

            let assigned =
                initial
                |> run (assignmentCommand (Set.singleton target) (draft "Temperature" "20" unit) NoOverwrite)

            let effect =
                assignmentCommand (Set.singleton target) (draft "Temperature" "20" unit) NoOverwrite assigned
                |> expectOk

            let actual = commit effect assigned
            Expect.equal actual assigned "A no-op commit preserves the whole session."

        testCase "an ordinary equal draft ignores loaded lineage and target source for idempotency"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "S1" ]
            let target = nodeIds.Head

            let prepared =
                ensureValueDefinition (category "Organism") (ProvenanceValue.Text "Human") None initial

            let loaded =
                existingAssignment
                    "loaded-assignment"
                    prepared.ValueDefinition.Id
                    AssignmentPropertyKind.Generic
                    (Some {
                        Id = "loaded-source"
                        Name = "loaded source"
                    })

            let before = initial |> installPreparation prepared |> addAssignment target loaded

            let effect =
                assignmentCommand (Set.singleton target) (draft "Organism" "Human" None) NoOverwrite before
                |> expectOk

            let actual = commit effect before
            Expect.equal actual before "Loaded ownership metadata does not make an equal ordinary draft distinct."
            Expect.equal actual.Nodes[target].Assignments.Count 1 "No duplicate ordinary assignment is added."

            let adapterNodeIds, adapterInitial = withNodes [ "adapter-only" ]
            let adapterTarget = adapterNodeIds.Head

            let adapterPrepared =
                ensureValueDefinition (category "Organism") (ProvenanceValue.Text "Human") None adapterInitial

            let adapterSpecific =
                existingAssignment
                    "adapter-assignment"
                    adapterPrepared.ValueDefinition.Id
                    (AssignmentPropertyKind.AdapterSpecific {
                        Id = "adapter:organism"
                        Label = "Organism"
                    })
                    None

            let adapterBefore =
                adapterInitial
                |> installPreparation adapterPrepared
                |> addAssignment adapterTarget adapterSpecific

            let adapterActual =
                adapterBefore
                |> run (assignmentCommand (Set.singleton adapterTarget) (draft "Organism" "Human" None) NoOverwrite)

            Expect.equal
                adapterActual.Nodes[adapterTarget].Assignments.Count
                2
                "An equal AdapterSpecific occurrence does not suppress a distinct Generic draft."

        testCase "a partial aggregate assigns only to the missing nodes"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "S1"; "S2" ]
            let firstId = nodeIds[0]
            let secondId = nodeIds[1]

            let first =
                initial
                |> run (assignmentCommand (Set.singleton firstId) (draft "Organism" "Human" None) NoOverwrite)

            let existingId = first.Nodes[firstId].Assignments |> Map.toSeq |> Seq.head |> fst

            let actual =
                first
                |> run (assignmentCommand (Set.ofList nodeIds) (draft "Organism" "Human" None) NoOverwrite)

            Expect.isTrue
                (actual.Nodes[firstId].Assignments |> Map.containsKey existingId)
                "Existing assignment identity is untouched."

            Expect.equal (assignmentList firstId actual |> List.length) 1 "The already assigned owner stays unchanged."
            Expect.equal (assignmentList secondId actual |> List.length) 1 "Only the missing owner is assigned."

            Expect.equal
                actual.AvailabilityTopologyRevision
                2
                "The partial aggregate is one additional topology command."

        testCase "replacing a different value for the same header requires explicit overwrite"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "S1" ]
            let target = nodeIds.Head

            let assigned =
                initial
                |> run (assignmentCommand (Set.singleton target) (draft "Organism" "Human" None) NoOverwrite)

            let assignmentId =
                assigned.Nodes[target].Assignments |> Map.toSeq |> Seq.head |> fst

            let propertyId =
                assigned.Values[assigned.Nodes[target].Assignments[assignmentId].ValueId].PropertyId

            let rejected =
                assignmentCommand (Set.singleton target) (draft "Organism" "Mouse" None) NoOverwrite assigned

            Expect.equal
                rejected
                (Error(OverwriteConfirmationRequired(propertyId, Set.singleton assignmentId)))
                "Overwrite must identify the conflicting assignment."

            let afterRejected =
                match rejected with
                | Error _ -> assigned
                | Ok effect -> commit effect assigned

            Expect.equal afterRejected assigned "Handling the failure preserves the pre-command session."

            let actual =
                assigned
                |> run (
                    assignmentCommand
                        (Set.singleton target)
                        (draft "Organism" "Mouse" None)
                        (OverwriteAssignments(Map.ofList [ target, assignmentId ]))
                )

            Expect.equal
                actual.Nodes[target].Assignments[assignmentId].Id
                assignmentId
                "Explicit overwrite retains assignment identity."

            Expect.equal
                actual.Values[actual.Nodes[target].Assignments[assignmentId].ValueId].Value
                (ProvenanceValue.Text "Mouse")
                "The selected assignment is repointed."

            let aggregateNodeIds, aggregateInitial = withNodes [ "A"; "B" ]

            let aggregateAssigned =
                aggregateInitial
                |> run (assignmentCommand (Set.ofList aggregateNodeIds) (draft "Organism" "Human" None) NoOverwrite)

            let aggregateAssignments =
                aggregateNodeIds
                |> List.map (fun ownerId ->
                    ownerId,
                    (aggregateAssigned.Nodes[ownerId].Assignments
                     |> Map.toSeq
                     |> Seq.exactlyOne
                     |> snd)
                )

            let aggregateAssignmentIds =
                aggregateAssignments |> List.map (snd >> _.Id) |> Set.ofList

            let aggregatePropertyId =
                aggregateAssignments.Head
                |> snd
                |> _.ValueId
                |> fun valueId -> aggregateAssigned.Values[valueId].PropertyId

            let aggregateRejected =
                assignmentCommand
                    (Set.ofList aggregateNodeIds)
                    (draft "Organism" "Mouse" None)
                    NoOverwrite
                    aggregateAssigned

            Expect.equal
                aggregateRejected
                (Error(OverwriteConfirmationRequired(aggregatePropertyId, aggregateAssignmentIds)))
                "Every conflicting owner must be confirmed."

            let afterAggregateRejection =
                match aggregateRejected with
                | Error _ -> aggregateAssigned
                | Ok effect -> commit effect aggregateAssigned

            Expect.equal
                afterAggregateRejection
                aggregateAssigned
                "The unconfirmed aggregate leaves every owner unchanged."

            let partialConfirmation =
                Map.ofList [
                    aggregateNodeIds[0], (aggregateAssignments[0] |> snd |> _.Id)
                ]

            let partialRejected =
                assignmentCommand
                    (Set.ofList aggregateNodeIds)
                    (draft "Organism" "Mouse" None)
                    (OverwriteAssignments partialConfirmation)
                    aggregateAssigned

            Expect.equal
                partialRejected
                (Error(OverwriteConfirmationRequired(aggregatePropertyId, aggregateAssignmentIds)))
                "A partial aggregate confirmation rejects the whole command."

            let afterPartialRejection =
                match partialRejected with
                | Error _ -> aggregateAssigned
                | Ok effect -> commit effect aggregateAssigned

            Expect.equal
                afterPartialRejection
                aggregateAssigned
                "A partial confirmation leaves the complete aggregate unchanged."

            let exactConfirmation =
                aggregateAssignments
                |> List.map (fun (ownerId, assignment) -> ownerId, assignment.Id)
                |> Map.ofList

            let aggregateActual =
                aggregateAssigned
                |> run (
                    assignmentCommand
                        (Set.ofList aggregateNodeIds)
                        (draft "Organism" "Mouse" None)
                        (OverwriteAssignments exactConfirmation)
                )

            for ownerId, before in aggregateAssignments do
                let assignments = aggregateActual.Nodes[ownerId].Assignments
                Expect.equal assignments.Count 1 "Overwrite does not add a sibling assignment."
                Expect.isTrue (assignments |> Map.containsKey before.Id) "The exact confirmed assignment is retained."

                Expect.equal
                    aggregateActual.Values[assignments[before.Id].ValueId].Value
                    (ProvenanceValue.Text "Mouse")
                    "Every confirmed owner is overwritten."

            Expect.equal
                aggregateActual.AnnotationValueRevision
                (aggregateAssigned.AnnotationValueRevision + 1)
                "The aggregate overwrite advances value revision once."

            Expect.equal
                aggregateActual.AvailabilityTopologyRevision
                aggregateAssigned.AvailabilityTopologyRevision
                "The aggregate overwrite does not change topology."
    ]

let private editTests =
    testList "node edit and removal" [
        testCase "editing an assignment that shares its value definition detaches only that assignment"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "S1"; "S2" ]

            let assigned =
                initial
                |> run (assignmentCommand (Set.ofList nodeIds) (draft "Organism" "Human" None) NoOverwrite)

            let firstId, secondId = nodeIds[0], nodeIds[1]

            let firstAssignment =
                assigned.Nodes[firstId].Assignments |> Map.toSeq |> Seq.head |> snd

            let secondAssignment =
                assigned.Nodes[secondId].Assignments |> Map.toSeq |> Seq.head |> snd

            let actual =
                assigned
                |> run (editNodeAssignment firstId firstAssignment.Id (content "Organism" "Mouse" None))

            let edited = actual.Nodes[firstId].Assignments[firstAssignment.Id]
            Expect.notEqual edited.ValueId firstAssignment.ValueId "The edited owner detaches."

            Expect.equal
                actual.Nodes[secondId].Assignments[secondAssignment.Id]
                secondAssignment
                "The other owner remains unchanged."

            Expect.isTrue
                (actual.Values |> Map.containsKey firstAssignment.ValueId)
                "The shared old definition remains."

            Expect.equal
                actual.AvailabilityTopologyRevision
                assigned.AvailabilityTopologyRevision
                "Topology is unchanged."

            Expect.equal
                actual.AnnotationValueRevision
                (assigned.AnnotationValueRevision + 1)
                "The edit advances value once."

        testCase "editing an assignment whose value is unshared updates it in place"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "S1" ]
            let target = nodeIds.Head

            let assigned =
                initial
                |> run (assignmentCommand (Set.singleton target) (draft "Organism" "Human" None) NoOverwrite)

            let before = assigned.Nodes[target].Assignments |> Map.toSeq |> Seq.head |> snd

            let actual =
                assigned
                |> run (editNodeAssignment target before.Id (content "Organism" "Mouse" None))

            let after = actual.Nodes[target].Assignments[before.Id]
            Expect.equal after.ValueId before.ValueId "An unshared value keeps its ID."
            Expect.equal actual.Values[before.ValueId].Value (ProvenanceValue.Text "Mouse") "Content updates in place."

            Expect.equal
                actual.AvailabilityTopologyRevision
                assigned.AvailabilityTopologyRevision
                "Topology is unchanged."

            Expect.equal
                actual.AnnotationValueRevision
                (assigned.AnnotationValueRevision + 1)
                "Value revision advances once."

        testCase "editing an unshared value to content already normalized elsewhere repoints to the existing definition"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]

            let withHuman =
                initial
                |> run (assignmentCommand (Set.singleton nodeIds[0]) (draft "Organism" "Human" None) NoOverwrite)

            let before =
                withHuman
                |> run (assignmentCommand (Set.singleton nodeIds[1]) (draft "Organism" "Mouse" None) NoOverwrite)

            let humanAssignment =
                before.Nodes[nodeIds[0]].Assignments |> Map.toSeq |> Seq.exactlyOne |> snd

            let mouseAssignment =
                before.Nodes[nodeIds[1]].Assignments |> Map.toSeq |> Seq.exactlyOne |> snd

            let actual =
                before
                |> run (editNodeAssignment nodeIds[0] humanAssignment.Id (content "Organism" "Mouse" None))

            Expect.equal
                actual.Nodes[nodeIds[0]].Assignments[humanAssignment.Id].ValueId
                mouseAssignment.ValueId
                "The edit repoints to the normalized definition that already exists."

            Expect.isFalse
                (actual.Values |> Map.containsKey humanAssignment.ValueId)
                "The old unreferenced value is removed."

            Expect.equal actual.Values.Count 1 "Only one normalized Mouse definition remains."

            Expect.equal
                actual.AvailabilityTopologyRevision
                before.AvailabilityTopologyRevision
                "Repointing does not change topology."

            Expect.equal
                actual.AnnotationValueRevision
                (before.AnnotationValueRevision + 1)
                "Repointing advances value revision once."

        testCase "semantic no-op edit tolerates pre-existing duplicate normalized definitions"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A" ]
            let target = nodeIds.Head

            let prepared =
                ensureValueDefinition (category "Organism") (ProvenanceValue.Text "Human") None initial

            let duplicateValue = {
                prepared.ValueDefinition with
                    Id = prepared.ValueDefinition.Id + "-duplicate"
            }

            let current =
                existingAssignment "loaded-assignment" duplicateValue.Id AssignmentPropertyKind.Generic None

            let before = {
                (initial |> installPreparation prepared |> addAssignment target current) with
                    Values =
                        Map.ofList [
                            prepared.ValueDefinition.Id, prepared.ValueDefinition
                            duplicateValue.Id, duplicateValue
                        ]
                    AvailabilityTopologyRevision = 7
                    AnnotationValueRevision = 9
                    LayerProjections = Map.ofList [ "layer-one", projection 3 ]
            }

            let effect =
                editNodeAssignment target current.Id (content "Organism" "Human" None) before
                |> expectOk

            let actual = commit effect before
            Expect.equal actual before "A semantically equal edit is a complete no-op despite duplicate definitions."

            Expect.equal
                actual.Nodes[target].Assignments[current.Id].ValueId
                duplicateValue.Id
                "The current duplicate value ID remains attached."

        testCase "an owned node annotation is editable through any appearance of its node"
        <| fun _ ->
            let startingSession () =
                let nodeIds, initial = withNodes [ "S1" ]
                let target = nodeIds.Head

                let layerOne = {
                    Id = "layer-one"
                    Label = "one"
                    Source = { Id = "source-one"; Name = "one" }
                    InputEndpoints = Map.empty
                    OutputEndpoints = Map.empty
                    StructuralProcessIds = Set.empty
                }

                let layerTwo = {
                    layerOne with
                        Id = "layer-two"
                        Label = "two"
                        Source = { Id = "source-two"; Name = "two" }
                }

                let endpoint layerId side = {
                    Key = {
                        LayerId = layerId
                        Side = side
                        NodeId = target
                    }
                    Header = { Kind = nodeKind; Text = "S1" }
                    LayerOrderPosition = 0
                }

                let appeared = {
                    initial with
                        Layers = Map.ofList [ layerOne.Id, layerOne; layerTwo.Id, layerTwo ]
                        LayerOrder = [ layerOne.Id; layerTwo.Id ]
                        ActiveLayerId = layerTwo.Id
                }

                appeared
                |> addLayerEndpoint (endpoint layerOne.Id ProvenanceSide.Input)
                |> expectOk
                |> addLayerEndpoint (endpoint layerTwo.Id ProvenanceSide.Output)
                |> expectOk
                |> run (assignmentCommand (Set.singleton target) (draft "Organism" "Human" None) NoOverwrite)

            let inputStart = startingSession ()
            let outputStart = startingSession ()

            let inputOwnerId =
                inputStart.Layers["layer-one"].InputEndpoints
                |> Map.toSeq
                |> Seq.exactlyOne
                |> fst

            let outputOwnerId =
                outputStart.Layers["layer-two"].OutputEndpoints
                |> Map.toSeq
                |> Seq.exactlyOne
                |> fst

            let inputAssignment =
                inputStart.Nodes[inputOwnerId].Assignments |> Map.toSeq |> Seq.exactlyOne |> snd

            let outputAssignment =
                outputStart.Nodes[outputOwnerId].Assignments
                |> Map.toSeq
                |> Seq.exactlyOne
                |> snd

            let editedThroughInput =
                inputStart
                |> run (editNodeAssignment inputOwnerId inputAssignment.Id (content "Organism" "Mouse" None))

            let editedThroughOutput =
                outputStart
                |> run (editNodeAssignment outputOwnerId outputAssignment.Id (content "Organism" "Mouse" None))

            Expect.equal inputOwnerId outputOwnerId "Both appearances resolve to the same canonical owner."
            Expect.equal editedThroughInput editedThroughOutput "Either appearance produces the same canonical edit."

            Expect.equal
                editedThroughInput.Values[inputAssignment.ValueId].Value
                (ProvenanceValue.Text "Mouse")
                "The input appearance path performs the edit."

            Expect.equal
                editedThroughOutput.Values[outputAssignment.ValueId].Value
                (ProvenanceValue.Text "Mouse")
                "The output appearance path performs the edit."

        testCase "removing a node assignment deletes its value definition only when it was the last reference"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "S1"; "S2" ]

            let assigned =
                initial
                |> run (assignmentCommand (Set.ofList nodeIds) (draft "Organism" "Human" None) NoOverwrite)

            let firstId, secondId = nodeIds[0], nodeIds[1]
            let first = assigned.Nodes[firstId].Assignments |> Map.toSeq |> Seq.head |> snd
            let second = assigned.Nodes[secondId].Assignments |> Map.toSeq |> Seq.head |> snd

            let afterFirst = assigned |> run (removeNodeAssignment firstId first.Id)

            Expect.isTrue
                (afterFirst.Values |> Map.containsKey first.ValueId)
                "A shared definition survives the first removal."

            let afterSecond = afterFirst |> run (removeNodeAssignment secondId second.Id)

            Expect.isFalse
                (afterSecond.Values |> Map.containsKey first.ValueId)
                "The last-reference definition is deleted."

            Expect.isFalse
                (afterSecond.Properties
                 |> Map.containsKey assigned.Values[first.ValueId].PropertyId)
                "Its empty property is deleted."

            Expect.equal afterSecond.Nodes.Count 2 "Structural nodes remain."

        testCase "removing a node assignment leaves equal values on unrelated nodes intact"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "S1"; "S2" ]

            let preparedOne =
                ensureValueDefinition (category "Organism") (ProvenanceValue.Text "Human") None initial

            let installed = installPreparation preparedOne initial

            let preparedTwo = {
                preparedOne with
                    ValueDefinition = {
                        preparedOne.ValueDefinition with
                            Id = preparedOne.ValueDefinition.Id + "-duplicate"
                    }
            }

            let installed = installPreparation preparedTwo installed

            let first =
                existingAssignment "assignment-one" preparedOne.ValueDefinition.Id AssignmentPropertyKind.Generic None

            let second =
                existingAssignment "assignment-two" preparedTwo.ValueDefinition.Id AssignmentPropertyKind.Generic None

            let installed =
                installed |> addAssignment nodeIds[0] first |> addAssignment nodeIds[1] second

            let actual = installed |> run (removeNodeAssignment nodeIds[0] first.Id)

            Expect.isFalse
                (actual.Values |> Map.containsKey first.ValueId)
                "The removed assignment's orphan value is deleted."

            Expect.equal
                actual.Nodes[nodeIds[1]].Assignments[second.Id]
                second
                "The unrelated equal assignment remains."

            Expect.isTrue (actual.Values |> Map.containsKey second.ValueId) "Its distinct equal definition remains."
    ]

let private promotionAndCopyTests =
    testList "draft, catalog, and copy" [
        testCase "a draft's first assignment creates the value definition and the assignment atomically"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "S1" ]
            let target = nodeIds.Head
            Expect.equal initial.Values.Count 0 "The draft is not stored before assignment."

            let actual =
                initial
                |> run (assignmentCommand (Set.singleton target) (draft "Organism" "Human" None) NoOverwrite)

            let assignment =
                actual.Nodes[target].Assignments |> Map.toSeq |> Seq.exactlyOne |> snd

            Expect.equal actual.Values.Count 1 "Exactly one value is installed."

            Expect.isTrue
                (actual.Values |> Map.containsKey assignment.ValueId)
                "The assignment references the installed value."

            Expect.equal actual.AvailabilityTopologyRevision 1 "Assignment creation advances topology once."
            Expect.equal (nodeAssignmentAddedMutations actual |> List.length) 1 "There is one semantic journal entry."

        testCase "first assignment promotes a catalog entry to a value definition"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "S1" ]
            let target = nodeIds.Head

            let entry = {
                Category = category "Protocol reference"
                Reference = {
                    Scheme = "arc"
                    Id = "protocol/a1"
                    Label = "Extraction"
                }
                Unit = None
                AssignmentKind = AnnotationOwnerKind.Node
                PropertyKind = AssignmentPropertyKind.Generic
                Cardinality = ReferenceCardinality.Many
                DependentProcessValues = []
            }

            let catalog = normalizeCatalog [ entry ]

            let actual =
                initial
                |> run (assignCatalogNodeValue (Set.singleton target) catalog entry NoOverwrite)

            let assignment =
                actual.Nodes[target].Assignments |> Map.toSeq |> Seq.exactlyOne |> snd

            Expect.equal
                actual.Values[assignment.ValueId].Value
                (ProvenanceValue.Reference entry.Reference)
                "The exact reference is promoted."

            Expect.equal catalog (normalizeCatalog [ entry ]) "The read-only catalog remains unchanged."

        testCase "an equal catalog assignment ignores loaded lineage and target source for idempotency"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "S1" ]
            let target = nodeIds.Head

            let concreteKind =
                AssignmentPropertyKind.AdapterSpecific {
                    Id = "adapter:protocol"
                    Label = "Protocol"
                }

            let entry = {
                Category = category "Protocol reference"
                Reference = {
                    Scheme = "arc"
                    Id = "protocol/a1"
                    Label = "Extraction"
                }
                Unit = None
                AssignmentKind = AnnotationOwnerKind.Node
                PropertyKind = concreteKind
                Cardinality = ReferenceCardinality.Many
                DependentProcessValues = []
            }

            let catalog = normalizeCatalog [ entry ]
            let prepared = promoteCatalogEntry entry initial

            let loaded =
                existingAssignment
                    "loaded-catalog-assignment"
                    prepared.ValueDefinition.Id
                    concreteKind
                    (Some {
                        Id = "loaded-source"
                        Name = "loaded source"
                    })

            let before = initial |> installPreparation prepared |> addAssignment target loaded

            let effect =
                assignCatalogNodeValue (Set.singleton target) catalog entry NoOverwrite before
                |> expectOk

            let actual = commit effect before
            Expect.equal actual before "Loaded ownership metadata does not duplicate an equal catalog assignment."
            Expect.equal actual.Nodes[target].Assignments.Count 1 "The existing catalog occurrence is reused."
            Expect.equal (tryFindCatalogEntry "arc" "protocol/a1" catalog) (Some entry) "The catalog is unchanged."

        testCase "removing the last assignment deletes the value definition but keeps the catalog entry"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "S1" ]
            let target = nodeIds.Head

            let entry = {
                Category = category "Protocol reference"
                Reference = {
                    Scheme = "arc"
                    Id = "protocol/a1"
                    Label = "Extraction"
                }
                Unit = None
                AssignmentKind = AnnotationOwnerKind.Node
                PropertyKind = AssignmentPropertyKind.Generic
                Cardinality = ReferenceCardinality.Many
                DependentProcessValues = []
            }

            let catalog = normalizeCatalog [ entry ]

            let assigned =
                initial
                |> run (assignCatalogNodeValue (Set.singleton target) catalog entry NoOverwrite)

            let assignment =
                assigned.Nodes[target].Assignments |> Map.toSeq |> Seq.exactlyOne |> snd

            let actual = assigned |> run (removeNodeAssignment target assignment.Id)

            Expect.isFalse (actual.Values |> Map.containsKey assignment.ValueId) "The promoted value is cleaned up."
            Expect.equal (tryFindCatalogEntry "arc" "protocol/a1" catalog) (Some entry) "The catalog entry remains."

        testCase "a second assignment of the same draft content reuses the promoted value definition"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "S1"; "S2" ]

            let first =
                initial
                |> run (assignmentCommand (Set.singleton nodeIds[0]) (draft "Organism" "Human" None) NoOverwrite)

            let firstValueId =
                first.Nodes[nodeIds[0]].Assignments
                |> Map.toSeq
                |> Seq.exactlyOne
                |> snd
                |> _.ValueId

            let actual =
                first
                |> run (assignmentCommand (Set.singleton nodeIds[1]) (draft "Organism" "Human" None) NoOverwrite)

            let secondValueId =
                actual.Nodes[nodeIds[1]].Assignments
                |> Map.toSeq
                |> Seq.exactlyOne
                |> snd
                |> _.ValueId

            Expect.equal secondValueId firstValueId "The normalized value definition is reused."
            Expect.equal actual.Values.Count 1 "No duplicate definition is installed."

        testCase "copying a loaded value to another owner keeps its concrete kind and creates a new assignment"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "source"; "target" ]
            let sourceId, targetId = nodeIds[0], nodeIds[1]

            let prepared =
                ensureValueDefinition (category "Adapter field") (ProvenanceValue.Text "loaded") None initial

            let installed = installPreparation prepared initial

            let sourceRef = {
                Id = "source-one"
                Name = "source one"
            }

            let concrete =
                AssignmentPropertyKind.AdapterSpecific {
                    Id = "adapter:field"
                    Label = "Adapter field"
                }

            let sourceAssignment =
                existingAssignment "assignment-1" prepared.ValueDefinition.Id concrete (Some sourceRef)

            let collisionProcess = {
                Id = "process-one"
                OriginLayerId = "layer-one"
                Name = None
                Links = Map.empty
                Assignments =
                    Map.ofList [
                        "assignment-2",
                        {
                            Id = "assignment-2"
                            ValueId = prepared.ValueDefinition.Id
                            PropertyKind = AssignmentPropertyKind.Generic
                            CoveredLinkIds = Set.empty
                            ContainerReferenceValueId = None
                            ReferenceSlotId = None
                            Lineage = AssignmentLineage.Loaded
                        }
                    ]
            }

            let installed = {
                (installed |> addAssignment sourceId sourceAssignment) with
                    Processes = Map.ofList [ collisionProcess.Id, collisionProcess ]
            }

            let previousTargetSource = {
                Id = "previous-target"
                Name = "previous target"
            }

            let existingTargetAssignment =
                existingAssignment "assignment-3" prepared.ValueDefinition.Id concrete (Some previousTargetSource)

            let installed = installed |> addAssignment targetId existingTargetAssignment

            let newTargetSource = {
                Id = "new-target"
                Name = "new target"
            }

            let actual =
                installed
                |> run (
                    copyLoadedNodeValue sourceId sourceAssignment.Id (Set.singleton targetId) (Some newTargetSource)
                )

            let copied =
                actual.Nodes[targetId].Assignments
                |> Map.toList
                |> List.map snd
                |> List.find (fun assignment -> assignment.Lineage = AssignmentLineage.DerivedFrom sourceAssignment.Id)

            Expect.notEqual copied.Id sourceAssignment.Id "The copy owns a new assignment ID."
            Expect.notEqual copied.Id "assignment-2" "The ID is collision-safe across process assignments."
            Expect.notEqual copied.Id existingTargetAssignment.Id "The pre-existing equal occurrence is not reused."
            Expect.equal copied.PropertyKind concrete "The concrete stored kind is preserved."

            Expect.equal
                copied.Lineage
                (AssignmentLineage.DerivedFrom sourceAssignment.Id)
                "Lineage identifies the source assignment."

            Expect.equal copied.TargetSource (Some newTargetSource) "The explicit new target source is used."

            Expect.equal
                actual.Nodes[targetId].Assignments.Count
                2
                "The ordinary equal occurrence coexists with the copy."

            Expect.equal
                actual.Nodes[targetId].Assignments[existingTargetAssignment.Id]
                existingTargetAssignment
                "The existing occurrence is untouched."

            Expect.equal
                actual.Nodes[sourceId].Assignments[sourceAssignment.Id]
                sourceAssignment
                "The source stays unchanged."

            let repeatedEffect =
                copyLoadedNodeValue sourceId sourceAssignment.Id (Set.singleton targetId) (Some newTargetSource) actual
                |> expectOk

            let repeated = commit repeatedEffect actual
            Expect.equal repeated actual "Repeating the identical derived copy is a no-op."

        testCase "a newly created node property carries only the generic node kind"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "S1" ]

            let actual =
                initial
                |> run (assignmentCommand (Set.singleton nodeIds.Head) (draft "Organism" "Human" None) NoOverwrite)

            let assignment =
                actual.Nodes[nodeIds.Head].Assignments |> Map.toSeq |> Seq.exactlyOne |> snd

            Expect.equal assignment.PropertyKind AssignmentPropertyKind.Generic "New node properties are generic."
    ]

let private revisionAndStalenessTests =
    testList "revision and staleness" [
        testCase
            "an explicit overwrite that repoints an assignment to an existing value advances only the value revision"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "S1"; "S2" ]

            let first =
                initial
                |> run (assignmentCommand (Set.singleton nodeIds[0]) (draft "Organism" "Human" None) NoOverwrite)

            let both =
                first
                |> run (assignmentCommand (Set.singleton nodeIds[1]) (draft "Organism" "Mouse" None) NoOverwrite)

            let firstAssignment =
                both.Nodes[nodeIds[0]].Assignments |> Map.toSeq |> Seq.exactlyOne |> snd

            let secondAssignment =
                both.Nodes[nodeIds[1]].Assignments |> Map.toSeq |> Seq.exactlyOne |> snd

            let oldValueId = firstAssignment.ValueId

            let actual =
                both
                |> run (
                    assignmentCommand
                        (Set.singleton nodeIds[0])
                        (draft "Organism" "Mouse" None)
                        (OverwriteAssignments(Map.ofList [ nodeIds[0], firstAssignment.Id ]))
                )

            Expect.equal
                actual.Nodes[nodeIds[0]].Assignments[firstAssignment.Id].ValueId
                secondAssignment.ValueId
                "The assignment repoints to the existing value."

            Expect.equal actual.Nodes[nodeIds[0]].Assignments.Count 1 "Owner coverage is unchanged."
            Expect.isFalse (actual.Values |> Map.containsKey oldValueId) "The old orphan value is removed."

            Expect.equal
                actual.AvailabilityTopologyRevision
                both.AvailabilityTopologyRevision
                "Topology does not advance."

            Expect.equal
                actual.AnnotationValueRevision
                (both.AnnotationValueRevision + 1)
                "Value advances exactly once."

        testCase "a successful command marks every layer projection stale"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "S1" ]

            let prepared = {
                initial with
                    ActiveLayerId = "layer-two"
                    LayerProjections =
                        Map.ofList [
                            "layer-one", projection 1
                            "layer-two", projection 2
                            "layer-three", projection 3
                        ]
            }

            let actual =
                prepared
                |> run (assignmentCommand (Set.singleton nodeIds.Head) (draft "Organism" "Human" None) NoOverwrite)

            Expect.isTrue
                (actual.LayerProjections |> Map.forall (fun _ item -> item.Stale))
                "Every cached layer, including active, is stale."
    ]

let private processAssignmentTests =
    testList "process assignment" [
        testCase "an input group targets outgoing and input-only links only"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "group"; "other" ]
            let group, other = nodeIds[0], nodeIds[1]

            let before =
                initial
                |> addTestProcess "p" [
                    link "outgoing" (ProcessLinkShape.Between(group, other))
                    link "incoming" (ProcessLinkShape.Between(other, group))
                    link "input-only" (ProcessLinkShape.InputOnly group)
                    link "output-only" (ProcessLinkShape.OutputOnly group)
                ]

            let incident = incidentLinks before group

            let targets =
                Set.ofList incident.OutgoingLinkIds
                |> Set.union (
                    incident.OneSidedLinkIds
                    |> List.filter (fun id ->
                        match before.Processes["p"].Links[id].Shape with
                        | ProcessLinkShape.InputOnly _ -> true
                        | _ -> false
                    )
                    |> Set.ofList
                )

            let actual =
                before
                |> run (assignProcessValue targets (processDraft "Temperature" "20" None))

            let assignment = onlyProcessAssignment "p" actual

            Expect.equal
                assignment.CoveredLinkIds
                (Set.ofList [ "outgoing"; "input-only" ])
                "Only input-side links are covered."

            Expect.isFalse (assignment.CoveredLinkIds.Contains "incoming") "The incoming link is unchanged."
            Expect.isFalse (assignment.CoveredLinkIds.Contains "output-only") "The output-only link is unchanged."

        testCase "an output group targets incoming and output-only links only"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "group"; "other" ]
            let group, other = nodeIds[0], nodeIds[1]

            let before =
                initial
                |> addTestProcess "p" [
                    link "outgoing" (ProcessLinkShape.Between(group, other))
                    link "incoming" (ProcessLinkShape.Between(other, group))
                    link "input-only" (ProcessLinkShape.InputOnly group)
                    link "output-only" (ProcessLinkShape.OutputOnly group)
                ]

            let incident = incidentLinks before group

            let targets =
                Set.ofList incident.IncomingLinkIds
                |> Set.union (
                    incident.OneSidedLinkIds
                    |> List.filter (fun id ->
                        match before.Processes["p"].Links[id].Shape with
                        | ProcessLinkShape.OutputOnly _ -> true
                        | _ -> false
                    )
                    |> Set.ofList
                )

            let actual =
                before
                |> run (assignProcessValue targets (processDraft "Temperature" "20" None))

            let assignment = onlyProcessAssignment "p" actual

            Expect.equal
                assignment.CoveredLinkIds
                (Set.ofList [ "incoming"; "output-only" ])
                "Only output-side links are covered."

            Expect.isFalse (assignment.CoveredLinkIds.Contains "outgoing") "The outgoing link is unchanged."
            Expect.isFalse (assignment.CoveredLinkIds.Contains "input-only") "The input-only link is unchanged."

        testCase "a drop resolving to no link is rejected as an empty target"
        <| fun _ ->
            let before = {
                empty with
                    AvailabilityTopologyRevision = 7
                    AnnotationValueRevision = 11
                    MutationJournal = []
            }

            let rejected =
                assignProcessValue Set.empty (processDraft "Temperature" "20" None) before

            Expect.equal rejected (Error EmptyTarget) "An empty resolved set is rejected."
            Expect.equal before.AvailabilityTopologyRevision 7 "Topology is unchanged."
            Expect.equal before.AnnotationValueRevision 11 "Value revision is unchanged."
            Expect.isEmpty before.MutationJournal "The journal is unchanged."

            let nodeIds, withNode = withNodes [ "A" ]

            let withLink =
                withNode
                |> addTestProcess "p" [ link "present" (ProcessLinkShape.InputOnly nodeIds.Head) ]

            let missing =
                assignProcessValue (Set.ofList [ "present"; "missing" ]) (processDraft "Temperature" "20" None) withLink

            Expect.equal missing (Error(LinkNotFound "missing")) "A missing link rejects the whole batch."
            Expect.equal withLink.Processes["p"].Assignments.Count 0 "Prevalidation prevents partial assignment."

        testCase "links from five processes produce five assignments"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]
            let a, b = nodeIds[0], nodeIds[1]

            let before, links =
                [ 1..5 ]
                |> List.fold
                    (fun (session, ids) index ->
                        let id = $"link-{index}"

                        addTestProcess $"process-{index}" [ link id (ProcessLinkShape.Between(a, b)) ] session,
                        id :: ids
                    )
                    (initial, [])

            let actual =
                before
                |> run (assignProcessValue (Set.ofList links) (processDraft "Temperature" "20" None))

            Expect.equal
                (actual.Processes |> Map.toList |> List.sumBy (snd >> _.Assignments.Count))
                5
                "Each owner receives one assignment."

            Expect.equal actual.AvailabilityTopologyRevision 1 "The whole aggregate advances topology once."

            let additions =
                actual.MutationJournal
                |> List.choose (
                    function
                    | ProcessAssignmentAdded(ownerId, assignment, context) -> Some(ownerId, assignment, context)
                    | _ -> None
                )

            Expect.equal additions.Length 5 "The journal records one addition per process."

            let _, _, context = additions.Head

            Expect.equal
                context.Scope
                (OwnerScoped(
                    [ 1..5 ]
                    |> List.map (fun index -> ProcessAssignmentOwner $"process-{index}")
                    |> Set.ofList
                ))
                "The mutation context names every exact process owner."

            Expect.equal context.Coverage.LinkIds (Set.ofList links) "The mutation context carries every exact link."

        testCase "several links of one process produce one assignment covering them"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]
            let a, b = nodeIds[0], nodeIds[1]

            let before =
                initial
                |> addTestProcess "p" [
                    link "l1" (ProcessLinkShape.Between(a, b))
                    link "l2" (ProcessLinkShape.InputOnly a)
                    link "l3" (ProcessLinkShape.OutputOnly b)
                ]

            let actual =
                before
                |> run (assignProcessValue (Set.ofList [ "l1"; "l2"; "l3" ]) (processDraft "Temperature" "20" None))

            let assignment = onlyProcessAssignment "p" actual

            Expect.equal
                assignment.CoveredLinkIds
                (Set.ofList [ "l1"; "l2"; "l3" ])
                "One assignment covers the selected links."

        testCase "process assignment is idempotent per covered link"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]

            let assigned =
                initial
                |> addTestProcess "p" [
                    link "l" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))
                ]
                |> run (assignProcessValue (Set.singleton "l") (processDraft "Temperature" "20" None))

            let effect =
                assignProcessValue (Set.singleton "l") (processDraft "Temperature" "20" None) assigned
                |> expectOk

            Expect.equal (commit effect assigned) assigned "Repeating equal coverage is an exact no-op."

        testCase "distinguishable same-header assignments coexist and stay independently addressable"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]

            let assigned =
                initial
                |> addTestProcess "p" [
                    link "l1" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))
                    link "l2" (ProcessLinkShape.InputOnly nodeIds[0])
                ]
                |> run (assignProcessValue (Set.ofList [ "l1"; "l2" ]) (processDraft "Temperature" "20" None))

            let original = onlyProcessAssignment "p" assigned

            let split =
                assigned
                |> run (
                    editProcessAssignmentSubset
                        "p"
                        original.Id
                        (Set.singleton "l2")
                        (content "Temperature" "30" (Some(category "degree-Celsius")))
                )

            let first =
                processAssignments "p" split
                |> List.find (fun assignment -> assignment.Id = original.Id)

            let second =
                processAssignments "p" split
                |> List.find (fun assignment -> assignment.Id <> original.Id)

            let editedFirst =
                split
                |> run (editProcessAssignment "p" first.Id (content "Temperature" "21" None))

            let editedBoth =
                editedFirst
                |> run (
                    editProcessAssignment "p" second.Id (content "Temperature" "31" (Some(category "degree-Celsius")))
                )

            Expect.equal
                editedBoth.Values[editedBoth.Processes["p"].Assignments[first.Id].ValueId].Value
                (ProvenanceValue.Text "21")
                "The first same-header assignment remains independently editable."

            Expect.equal
                editedBoth.Values[editedBoth.Processes["p"].Assignments[second.Id].ValueId].Value
                (ProvenanceValue.Text "31")
                "The second same-header assignment remains independently editable."

            Expect.equal
                editedBoth.Processes["p"].Assignments[first.Id].CoveredLinkIds
                (Set.singleton "l1")
                "Editing the first occurrence does not disturb the second occurrence's coverage."

            Expect.equal
                editedBoth.Processes["p"].Assignments[second.Id].CoveredLinkIds
                (Set.singleton "l2")
                "Editing the second occurrence does not disturb the first occurrence's coverage."

        testCase "an ambiguous same-header overwrite is rejected per link"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]
            let processLink = link "l" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))

            let firstPreparation =
                ensureValueDefinition (category "Temperature") (ProvenanceValue.Text "20") None initial

            let withFirst = installPreparation firstPreparation initial

            let secondPreparation =
                ensureValueDefinition
                    (category "Temperature")
                    (ProvenanceValue.Text "30")
                    (Some(category "degree-Celsius"))
                    withFirst

            let withDefinitions = installPreparation secondPreparation withFirst

            let first: ProcessAssignment = {
                Id = "loaded-temperature-20"
                ValueId = firstPreparation.ValueDefinition.Id
                PropertyKind = AssignmentPropertyKind.Generic
                CoveredLinkIds = Set.singleton processLink.Id
                ContainerReferenceValueId = None
                ReferenceSlotId = None
                Lineage = AssignmentLineage.Loaded
            }

            let second: ProcessAssignment = {
                Id = "loaded-temperature-30"
                ValueId = secondPreparation.ValueDefinition.Id
                PropertyKind = AssignmentPropertyKind.Generic
                CoveredLinkIds = Set.singleton processLink.Id
                ContainerReferenceValueId = None
                ReferenceSlotId = None
                Lineage = AssignmentLineage.Loaded
            }

            let before =
                withDefinitions
                |> addTestProcess "p" [ processLink ]
                |> addProcessAssignment "p" first
                |> addProcessAssignment "p" second

            let rejected =
                assignProcessValue (Set.singleton processLink.Id) (processDraft "Temperature" "40" None) before

            Expect.equal
                rejected
                (Error(
                    MultiplePropertyValues(firstPreparation.PropertyDefinition.Id, Set.ofList [ first.Id; second.Id ])
                ))
                "An unqualified overwrite cannot choose between same-header occurrences on one link."

            let after =
                match rejected with
                | Error _ -> before
                | Ok effect -> commit effect before

            Expect.equal after before "The ambiguous overwrite mutates nothing."

        testCase "an explicitly identified assignment is overwritten successfully"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]
            let processLink = link "l" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))

            let firstPreparation =
                ensureValueDefinition (category "Temperature") (ProvenanceValue.Text "20") None initial

            let withFirst = installPreparation firstPreparation initial

            let secondPreparation =
                ensureValueDefinition
                    (category "Temperature")
                    (ProvenanceValue.Text "30")
                    (Some(category "degree-Celsius"))
                    withFirst

            let withDefinitions = installPreparation secondPreparation withFirst

            let first: ProcessAssignment = {
                Id = "loaded-temperature-20"
                ValueId = firstPreparation.ValueDefinition.Id
                PropertyKind = AssignmentPropertyKind.Generic
                CoveredLinkIds = Set.singleton processLink.Id
                ContainerReferenceValueId = None
                ReferenceSlotId = None
                Lineage = AssignmentLineage.Loaded
            }

            let second: ProcessAssignment = {
                Id = "loaded-temperature-30"
                ValueId = secondPreparation.ValueDefinition.Id
                PropertyKind = AssignmentPropertyKind.Generic
                CoveredLinkIds = Set.singleton processLink.Id
                ContainerReferenceValueId = None
                ReferenceSlotId = None
                Lineage = AssignmentLineage.Loaded
            }

            let before =
                withDefinitions
                |> addTestProcess "p" [ processLink ]
                |> addProcessAssignment "p" first
                |> addProcessAssignment "p" second

            let actual =
                before
                |> run (
                    editProcessAssignmentSubset
                        "p"
                        first.Id
                        (Set.singleton processLink.Id)
                        (content "Temperature" "40" None)
                )

            Expect.equal
                actual.Values[actual.Processes["p"].Assignments[first.Id].ValueId].Value
                (ProvenanceValue.Text "40")
                "The identified assignment is overwritten."

            Expect.equal actual.Processes["p"].Assignments[second.Id] second "The same-header sibling is untouched."

        testCase "conflict detection is scoped to the kind-bearing property entry"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]
            let processLink = link "l" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))

            let existingDraft = {
                processDraft "Temperature" "20" None with
                    PropertyKind =
                        AssignmentPropertyKind.AdapterSpecific {
                            Id = "adapter:parameter"
                            Label = "Parameter"
                        }
                    Lineage = AssignmentLineage.Loaded
            }

            let before =
                initial
                |> addTestProcess "p" [ processLink ]
                |> run (assignProcessValue (Set.singleton processLink.Id) existingDraft)

            let actual =
                before
                |> run (assignProcessValue (Set.singleton processLink.Id) (processDraft "Temperature" "30" None))

            Expect.equal
                (processAssignments "p" actual).Length
                2
                "The same header under another property kind is a distinct entry, not a conflict."

        testCase "mixed same-header counts across an aggregate target are rejected"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]
            let firstLink = link "l1" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))
            let secondLink = link "l2" (ProcessLinkShape.InputOnly nodeIds[0])

            let firstPreparation =
                ensureValueDefinition (category "Temperature") (ProvenanceValue.Text "20") None initial

            let withFirst = installPreparation firstPreparation initial

            let secondPreparation =
                ensureValueDefinition (category "Temperature") (ProvenanceValue.Text "30") None withFirst

            let withDefinitions = installPreparation secondPreparation withFirst

            let shared: ProcessAssignment = {
                Id = "shared"
                ValueId = firstPreparation.ValueDefinition.Id
                PropertyKind = AssignmentPropertyKind.Generic
                CoveredLinkIds = Set.ofList [ firstLink.Id; secondLink.Id ]
                ContainerReferenceValueId = None
                ReferenceSlotId = None
                Lineage = AssignmentLineage.Loaded
            }

            let secondOnly: ProcessAssignment = {
                Id = "second-only"
                ValueId = secondPreparation.ValueDefinition.Id
                PropertyKind = AssignmentPropertyKind.Generic
                CoveredLinkIds = Set.singleton secondLink.Id
                ContainerReferenceValueId = None
                ReferenceSlotId = None
                Lineage = AssignmentLineage.Loaded
            }

            let before =
                withDefinitions
                |> addTestProcess "p" [ firstLink; secondLink ]
                |> addProcessAssignment "p" shared
                |> addProcessAssignment "p" secondOnly

            let rejected =
                assignProcessValue
                    (Set.ofList [ firstLink.Id; secondLink.Id ])
                    (processDraft "Temperature" "40" None)
                    before

            Expect.equal
                rejected
                (Error(
                    MixedPropertyValueCounts(
                        firstPreparation.PropertyDefinition.Id,
                        Map.ofList [ firstLink.Id, 1; secondLink.Id, 2 ]
                    )
                ))
                "Different same-header multiplicities reject the aggregate atomically."

            let after =
                match rejected with
                | Error _ -> before
                | Ok effect -> commit effect before

            Expect.equal after before "No aggregate member is partially overwritten."

        testCase "a generic draft does not extend an assignment with container or slot metadata"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]
            let firstLink = link "l1" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))
            let secondLink = link "l2" (ProcessLinkShape.InputOnly nodeIds[0])

            let containerPreparation =
                ensureValueDefinition
                    (category "Protocol reference")
                    (ProvenanceValue.Reference {
                        Scheme = "arc"
                        Id = "protocol/one"
                        Label = "Protocol one"
                    })
                    None
                    initial

            let withContainer = installPreparation containerPreparation initial

            let valuePreparation =
                ensureValueDefinition (category "Temperature") (ProvenanceValue.Text "20") None withContainer

            let withDefinitions = installPreparation valuePreparation withContainer

            let supportingContainer: ProcessAssignment = {
                Id = "container"
                ValueId = containerPreparation.ValueDefinition.Id
                PropertyKind = AssignmentPropertyKind.Generic
                CoveredLinkIds = Set.singleton firstLink.Id
                ContainerReferenceValueId = None
                ReferenceSlotId = None
                Lineage = AssignmentLineage.Loaded
            }

            let metadataBearing: ProcessAssignment = {
                Id = "metadata-bearing"
                ValueId = valuePreparation.ValueDefinition.Id
                PropertyKind = AssignmentPropertyKind.Generic
                CoveredLinkIds = Set.singleton firstLink.Id
                ContainerReferenceValueId = Some containerPreparation.ValueDefinition.Id
                ReferenceSlotId = Some "temperature-slot"
                Lineage = AssignmentLineage.Loaded
            }

            let before = {
                withDefinitions with
                    Processes =
                        Map.ofList [
                            "p",
                            {
                                Id = "p"
                                OriginLayerId = "test-layer"
                                Name = None
                                Links = Map.ofList [ firstLink.Id, firstLink; secondLink.Id, secondLink ]
                                Assignments =
                                    Map.ofList [
                                        supportingContainer.Id, supportingContainer
                                        metadataBearing.Id, metadataBearing
                                    ]
                            }
                        ]
            }

            let actual =
                before
                |> run (assignProcessValue (Set.singleton secondLink.Id) (processDraft "Temperature" "20" None))

            Expect.equal
                actual.Processes["p"].Assignments[metadataBearing.Id]
                metadataBearing
                "The metadata-bearing occurrence remains restricted to its original link."

            let ordinary =
                processAssignments "p" actual
                |> List.find (fun assignment ->
                    assignment.ValueId = valuePreparation.ValueDefinition.Id
                    && assignment.Id <> metadataBearing.Id
                )

            Expect.equal
                ordinary.CoveredLinkIds
                (Set.singleton secondLink.Id)
                "A distinct ordinary occurrence covers the new link."

            Expect.equal ordinary.ContainerReferenceValueId None "Container metadata is not leaked."
            Expect.equal ordinary.ReferenceSlotId None "Slot metadata is not leaked."

        testCase "a partial aggregate covers only the links that lacked the value"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]

            let first =
                initial
                |> addTestProcess "p" [
                    link "l1" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))
                    link "l2" (ProcessLinkShape.InputOnly nodeIds[0])
                ]
                |> run (assignProcessValue (Set.singleton "l1") (processDraft "Temperature" "20" None))

            let existing = onlyProcessAssignment "p" first

            let actual =
                first
                |> run (assignProcessValue (Set.ofList [ "l1"; "l2" ]) (processDraft "Temperature" "20" None))

            let after = onlyProcessAssignment "p" actual
            Expect.equal after.Id existing.Id "The compatible occurrence is extended."
            Expect.equal after.CoveredLinkIds (Set.ofList [ "l1"; "l2" ]) "Only missing coverage is added."

            match actual.MutationJournal |> List.last with
            | ProcessAssignmentCoverageChanged(ownerId, before, changed, context) ->
                Expect.equal ownerId "p" "The exact process owns the coverage mutation."
                Expect.equal before existing "The old occurrence is journaled."
                Expect.equal changed after "The extended occurrence is journaled."
                Expect.equal context.Coverage.LinkIds (Set.singleton "l2") "Only newly covered links are contextual."
            | mutation -> failtestf "Expected ProcessAssignmentCoverageChanged but got %A" mutation

        testCase "editing a process assignment updates every covered link"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]

            let assigned =
                initial
                |> addTestProcess "p" [
                    link "l1" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))
                    link "l2" (ProcessLinkShape.InputOnly nodeIds[0])
                ]
                |> run (assignProcessValue (Set.ofList [ "l1"; "l2" ]) (processDraft "Temperature" "20" None))

            let before = onlyProcessAssignment "p" assigned
            let beforeValue = assigned.Values[before.ValueId]

            let actual =
                assigned
                |> run (editProcessAssignment "p" before.Id (content "Temperature" "30" None))

            let after = onlyProcessAssignment "p" actual
            let afterValue = actual.Values[after.ValueId]

            Expect.equal after.CoveredLinkIds before.CoveredLinkIds "Full edit retains all covered links."
            Expect.equal after.ValueId before.ValueId "An unshared true-new edit updates the value definition in place."

            Expect.equal afterValue.Value (ProvenanceValue.Text "30") "Every covered link observes the edited value."

            Expect.equal
                actual.AvailabilityTopologyRevision
                assigned.AvailabilityTopologyRevision
                "Topology is unchanged."

            Expect.equal actual.AnnotationValueRevision (assigned.AnnotationValueRevision + 1) "Value advances once."

            match actual.MutationJournal |> List.last with
            | PropertyValueDefinitionUpdated(journalBefore, journalAfter, context) ->
                Expect.equal journalBefore beforeValue "The journal carries the old definition and content."
                Expect.equal journalAfter afterValue "The journal carries the new definition and content."

                Expect.equal
                    context.Scope
                    (OwnerScoped(Set.singleton (ProcessAssignmentOwner "p")))
                    "The value update is scoped to the exact process."

                Expect.equal context.Coverage.AssignmentIds (Set.singleton before.Id) "The assignment context is exact."
                Expect.equal context.Coverage.LinkIds before.CoveredLinkIds "Every covered link is contextual."
            | ProcessAssignmentValueChanged(_, journalBefore, journalAfter, _) when journalBefore = journalAfter ->
                failtest "An unchanged assignment record must not masquerade as a process assignment value change."
            | mutation -> failtestf "Expected PropertyValueDefinitionUpdated but got %A" mutation

        testCase "editing a subset detaches it into a new assignment"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]

            let assigned =
                initial
                |> addTestProcess "p" [
                    link "l1" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))
                    link "l2" (ProcessLinkShape.InputOnly nodeIds[0])
                ]
                |> run (assignProcessValue (Set.ofList [ "l1"; "l2" ]) (processDraft "Temperature" "20" None))

            let original = onlyProcessAssignment "p" assigned

            let actual =
                assigned
                |> run (
                    editProcessAssignmentSubset "p" original.Id (Set.singleton "l2") (content "Temperature" "30" None)
                )

            let retained = actual.Processes["p"].Assignments[original.Id]

            let split =
                processAssignments "p" actual
                |> List.find (fun assignment -> assignment.Id <> original.Id)

            Expect.equal retained.CoveredLinkIds (Set.singleton "l1") "The original retains the complement."

            Expect.equal
                actual.Values[retained.ValueId].Value
                (ProvenanceValue.Text "20")
                "The original retains old content."

            Expect.equal split.CoveredLinkIds (Set.singleton "l2") "The subset moves to the new assignment."
            Expect.equal split.PropertyKind original.PropertyKind "The property kind is inherited."
            Expect.equal split.Lineage (AssignmentLineage.DerivedFrom original.Id) "The split records its origin."

            Expect.equal
                actual.AvailabilityTopologyRevision
                (assigned.AvailabilityTopologyRevision + 1)
                "Topology advances once."

            Expect.equal actual.AnnotationValueRevision (assigned.AnnotationValueRevision + 1) "Value advances once."

            match actual.MutationJournal |> List.last with
            | ProcessAssignmentSplit(ownerId, journalOriginal, journalRetained, journalSplit, context) ->
                Expect.equal ownerId "p" "The split is owner-scoped."
                Expect.equal journalOriginal original "The original occurrence is journaled."
                Expect.equal journalRetained retained "The retained complement is journaled."
                Expect.equal journalSplit split "The detached occurrence is journaled."
                Expect.equal context.Coverage.LinkIds (Set.singleton "l2") "The exact detached links are contextual."
            | mutation -> failtestf "Expected ProcessAssignmentSplit but got %A" mutation

        testCase "editing a subset to the existing content is a no-op"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]

            let assigned =
                initial
                |> addTestProcess "p" [
                    link "l1" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))
                    link "l2" (ProcessLinkShape.InputOnly nodeIds[0])
                ]
                |> run (assignProcessValue (Set.ofList [ "l1"; "l2" ]) (processDraft "Temperature" "20" None))

            let assignment = onlyProcessAssignment "p" assigned

            let before = {
                assigned with
                    LayerProjections = Map.ofList [ "test-layer", projection 3 ]
            }

            let effect =
                editProcessAssignmentSubset
                    "p"
                    assignment.Id
                    (Set.singleton "l2")
                    (content "Temperature" "20" None)
                    before
                |> expectOk

            let actual = commit effect before
            Expect.equal actual before "A semantically equal subset edit preserves the exact session."
            Expect.equal actual.Processes["p"].Assignments.Count 1 "No split assignment is created."
            Expect.equal actual.MutationJournal before.MutationJournal "No mutation is appended."

            Expect.equal
                actual.AvailabilityTopologyRevision
                before.AvailabilityTopologyRevision
                "Topology is unchanged."

            Expect.equal actual.AnnotationValueRevision before.AnnotationValueRevision "Value revision is unchanged."
            Expect.isFalse actual.LayerProjections["test-layer"].Stale "Cached projections remain fresh."

        testCase "removing from a display connector removes exactly the represented links from coverage"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]

            let assigned =
                initial
                |> addTestProcess "p" [
                    link "l1" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))
                    link "l2" (ProcessLinkShape.InputOnly nodeIds[0])
                    link "l3" (ProcessLinkShape.OutputOnly nodeIds[1])
                ]
                |> run (assignProcessValue (Set.ofList [ "l1"; "l2"; "l3" ]) (processDraft "Temperature" "20" None))

            let assignment = onlyProcessAssignment "p" assigned

            let actual =
                assigned
                |> run (removeProcessAssignmentLinks "p" assignment.Id (Set.ofList [ "l1"; "l3" ]))

            Expect.equal
                (onlyProcessAssignment "p" actual).CoveredLinkIds
                (Set.singleton "l2")
                "Exactly represented links are removed."

            match actual.MutationJournal |> List.last with
            | ProcessAssignmentCoverageChanged(ownerId, before, after, context) ->
                Expect.equal ownerId "p" "The exact owner is journaled."
                Expect.equal before assignment "The old coverage is journaled."
                Expect.equal after.CoveredLinkIds (Set.singleton "l2") "The remaining coverage is journaled."
                Expect.equal context.Coverage.LinkIds (Set.ofList [ "l1"; "l3" ]) "The removed links are exact."
            | mutation -> failtestf "Expected ProcessAssignmentCoverageChanged but got %A" mutation

        testCase "an assignment whose coverage becomes empty is deleted"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]

            let assigned =
                initial
                |> addTestProcess "p" [
                    link "l" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))
                ]
                |> run (assignProcessValue (Set.singleton "l") (processDraft "Temperature" "20" None))

            let assignment = onlyProcessAssignment "p" assigned

            let actual =
                assigned
                |> run (removeProcessAssignmentLinks "p" assignment.Id (Set.singleton "l"))

            Expect.isEmpty actual.Processes["p"].Assignments "Empty coverage deletes the assignment."

            Expect.equal
                actual.AvailabilityTopologyRevision
                (assigned.AvailabilityTopologyRevision + 1)
                "Topology advances once."

            Expect.isTrue (actual.Processes["p"].Links.ContainsKey "l") "The structural link remains."

            match actual.MutationJournal |> List.last with
            | ProcessAssignmentRemoved(tombstone, context) ->
                Expect.equal tombstone.OwnerId "p" "The tombstone retains the owner."
                Expect.equal tombstone.Assignment assignment "The tombstone retains the deleted occurrence."
                Expect.equal context.Coverage.LinkIds (Set.singleton "l") "The exact removed link is contextual."
            | mutation -> failtestf "Expected ProcessAssignmentRemoved but got %A" mutation

        testCase "removing the last process assignment keeps the link and the process"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]

            let assigned =
                initial
                |> addTestProcess "p" [
                    link "l" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))
                ]
                |> run (assignProcessValue (Set.singleton "l") (processDraft "Temperature" "20" None))

            let assignment = onlyProcessAssignment "p" assigned

            let actual =
                assigned
                |> run (removeProcessAssignmentLinks "p" assignment.Id (Set.singleton "l"))

            Expect.isTrue (actual.Processes.ContainsKey "p") "The process remains."
            Expect.isTrue (actual.Processes["p"].Links.ContainsKey "l") "The link remains."
            Expect.isEmpty actual.Values "The last value is cleaned up."
            Expect.isEmpty actual.Properties "The last property is cleaned up."

        testCase "pooled connector removal removes across every represented backing link"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]

            let assigned =
                initial
                |> addTestProcess "p" [
                    link "l1" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))
                    link "l2" (ProcessLinkShape.InputOnly nodeIds[0])
                    link "l3" (ProcessLinkShape.OutputOnly nodeIds[1])
                ]
                |> run (assignProcessValue (Set.ofList [ "l1"; "l2"; "l3" ]) (processDraft "Temperature" "20" None))

            let assignment = onlyProcessAssignment "p" assigned

            let actual =
                assigned
                |> run (removeProcessAssignmentLinks "p" assignment.Id assignment.CoveredLinkIds)

            Expect.isEmpty actual.Processes["p"].Assignments "All pooled backing links are removed."

            Expect.equal
                actual.AvailabilityTopologyRevision
                (assigned.AvailabilityTopologyRevision + 1)
                "One command bumps topology once."

        testCase "removing a reference cascades to its dependent projections"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]
            let processLink = link "l" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))

            let containerPreparation =
                ensureValueDefinition
                    (category "Protocol reference")
                    (ProvenanceValue.Reference {
                        Scheme = "arc"
                        Id = "protocol/one"
                        Label = "Protocol one"
                    })
                    None
                    initial

            let withContainerDefinition = installPreparation containerPreparation initial

            let dependentPreparation =
                ensureValueDefinition (category "Temperature") (ProvenanceValue.Text "20") None withContainerDefinition

            let withDefinitions =
                installPreparation dependentPreparation withContainerDefinition

            let containerAssignment: ProcessAssignment = {
                Id = "a-container"
                ValueId = containerPreparation.ValueDefinition.Id
                PropertyKind = AssignmentPropertyKind.Generic
                CoveredLinkIds = Set.singleton processLink.Id
                ContainerReferenceValueId = None
                ReferenceSlotId = None
                Lineage = AssignmentLineage.Loaded
            }

            let dependentAssignment: ProcessAssignment = {
                Id = "b-dependent"
                ValueId = dependentPreparation.ValueDefinition.Id
                PropertyKind = AssignmentPropertyKind.Generic
                CoveredLinkIds = Set.singleton processLink.Id
                ContainerReferenceValueId = Some containerPreparation.ValueDefinition.Id
                ReferenceSlotId = None
                Lineage = AssignmentLineage.Loaded
            }

            let before = {
                withDefinitions with
                    Processes =
                        Map.ofList [
                            "p",
                            {
                                Id = "p"
                                OriginLayerId = "test-layer"
                                Name = None
                                Links = Map.ofList [ processLink.Id, processLink ]
                                Assignments =
                                    Map.ofList [
                                        containerAssignment.Id, containerAssignment
                                        dependentAssignment.Id, dependentAssignment
                                    ]
                            }
                        ]
            }

            let selections =
                Map.ofList [
                    "p",
                    Map.ofList [
                        containerAssignment.Id, containerAssignment.CoveredLinkIds
                    ]
                ]

            let actual = before |> run (removeProcessAssignmentsByOwner selections)

            Expect.isEmpty actual.Processes["p"].Assignments "The reference and its dependent projection are removed."
            Expect.isEmpty actual.Values "Final-state cleanup removes both orphan values."
            Expect.isEmpty actual.Properties "Final-state cleanup removes both orphan properties."
            Expect.isTrue (actual.Processes.ContainsKey "p") "The structural process remains."
            Expect.isTrue (actual.Processes["p"].Links.ContainsKey processLink.Id) "The structural link remains."

            Expect.equal
                actual.AvailabilityTopologyRevision
                (before.AvailabilityTopologyRevision + 1)
                "The atomic bulk removal bumps topology once."

        testCase "subset edit and removal reject stale foreign coverage atomically"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]

            let assigned =
                initial
                |> addTestProcess "p" [
                    link "owned" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))
                ]
                |> addTestProcess "foreign-owner" [ link "foreign" (ProcessLinkShape.InputOnly nodeIds[0]) ]
                |> run (assignProcessValue (Set.singleton "owned") (processDraft "Temperature" "20" None))

            let assignment = onlyProcessAssignment "p" assigned

            let malformedAssignment = {
                assignment with
                    CoveredLinkIds = Set.ofList [ "owned"; "foreign" ]
            }

            let before = {
                assigned with
                    Processes =
                        assigned.Processes
                        |> Map.change
                            "p"
                            (Option.map (fun structuralProcess -> {
                                structuralProcess with
                                    Assignments =
                                        structuralProcess.Assignments
                                        |> Map.add malformedAssignment.Id malformedAssignment
                            }))
                    LayerProjections = Map.ofList [ "test-layer", projection 3 ]
            }

            let editResult =
                editProcessAssignmentSubset
                    "p"
                    malformedAssignment.Id
                    (Set.singleton "owned")
                    (content "Temperature" "30" None)
                    before

            let removeResult =
                removeProcessAssignmentLinks "p" malformedAssignment.Id (Set.singleton "owned") before

            let expectMalformed =
                function
                | Error(InconsistentCanonicalState _) -> ()
                | result -> failtestf "Expected malformed ownership rejection but got %A" result

            expectMalformed editResult
            expectMalformed removeResult

            let afterEdit =
                match editResult with
                | Error _ -> before
                | Ok effect -> commit effect before

            let afterRemove =
                match removeResult with
                | Error _ -> before
                | Ok effect -> commit effect before

            Expect.equal afterEdit before "The rejected edit preserves the exact session."
            Expect.equal afterRemove before "The rejected removal preserves the exact session."
            Expect.isFalse before.LayerProjections["test-layer"].Stale "The unchanged projection remains fresh."

        testCase "group card process removal resolves the card's incident links and partitions by process"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "group"; "B"; "C" ]
            let group = nodeIds[0]

            let assigned =
                initial
                |> addTestProcess "p1" [ link "l1" (ProcessLinkShape.Between(group, nodeIds[1])) ]
                |> addTestProcess "p2" [ link "l2" (ProcessLinkShape.Between(nodeIds[2], group)) ]
                |> run (assignProcessValue (Set.ofList [ "l1"; "l2" ]) (processDraft "Temperature" "20" None))

            let a1 = onlyProcessAssignment "p1" assigned
            let a2 = onlyProcessAssignment "p2" assigned
            let incident = incidentLinks assigned group
            let represented = Set.ofList (incident.OutgoingLinkIds @ incident.IncomingLinkIds)

            let selections =
                Map.ofList [
                    "p1", Map.ofList [ a1.Id, represented |> Set.intersect a1.CoveredLinkIds ]
                    "p2", Map.ofList [ a2.Id, represented |> Set.intersect a2.CoveredLinkIds ]
                ]

            let actual = assigned |> run (removeProcessAssignmentsByOwner selections)
            Expect.isEmpty actual.Processes["p1"].Assignments "The first process occurrence is removed."
            Expect.isEmpty actual.Processes["p2"].Assignments "The second process occurrence is removed."
            Expect.isTrue (actual.Processes["p1"].Links.ContainsKey "l1") "The first link remains."
            Expect.isTrue (actual.Processes["p2"].Links.ContainsKey "l2") "The second link remains."

            Expect.equal
                actual.AvailabilityTopologyRevision
                (assigned.AvailabilityTopologyRevision + 1)
                "The aggregate bumps once."

        testCase "group card node removal removes the matching owned assignment from every distinct member node"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]

            let assigned =
                initial
                |> run (assignmentCommand (Set.ofList nodeIds) (draft "Organism" "Human" None) NoOverwrite)

            let owners =
                nodeIds
                |> List.map (fun ownerId ->
                    ownerId,
                    (assigned.Nodes[ownerId].Assignments
                     |> Map.toSeq
                     |> Seq.exactlyOne
                     |> snd
                     |> _.Id)
                )

            let actual =
                assigned |> run (removeNodeAssignmentsByOwner (nodeOwnerSelection owners))

            Expect.isTrue
                (nodeIds
                 |> List.forall (fun ownerId -> actual.Nodes[ownerId].Assignments.IsEmpty))
                "Every distinct member loses its exact assignment."

            Expect.equal actual.Nodes.Count assigned.Nodes.Count "Canonical nodes remain."

            Expect.equal
                actual.AvailabilityTopologyRevision
                (assigned.AvailabilityTopologyRevision + 1)
                "The aggregate bumps once."

            let removals =
                actual.MutationJournal
                |> List.choose (
                    function
                    | NodeAssignmentRemoved(tombstone, context) -> Some(tombstone, context)
                    | _ -> None
                )

            Expect.equal removals.Length 2 "One removal is journaled per exact owner."

            let expectedOwners = nodeIds |> List.map NodeAssignmentOwner |> Set.ofList

            Expect.isTrue
                (removals
                 |> List.forall (fun (_, context) -> context.Scope = OwnerScoped expectedOwners))
                "Every mutation carries the complete exact owner scope."

        testCase "process assignment matching preserves lineage metadata"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]

            let loadedDraft = {
                processDraft "Temperature" "20" None with
                    Lineage = AssignmentLineage.Loaded
            }

            let withLoaded =
                initial
                |> addTestProcess "p" [
                    link "loaded-link" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))
                    link "created-link" (ProcessLinkShape.InputOnly nodeIds[0])
                ]
                |> run (assignProcessValue (Set.singleton "loaded-link") loadedDraft)

            let createdDraft = processDraft "Temperature" "20" None

            let withCreated =
                withLoaded
                |> run (assignProcessValue (Set.singleton "created-link") createdDraft)

            let assignments = processAssignments "p" withCreated
            Expect.equal assignments.Length 2 "Different intended lineages remain distinct occurrences."

            let loaded =
                assignments
                |> List.find (fun assignment -> assignment.Lineage = AssignmentLineage.Loaded)

            let created =
                assignments
                |> List.find (fun assignment -> assignment.Lineage = AssignmentLineage.Created)

            Expect.equal loaded.CoveredLinkIds (Set.singleton "loaded-link") "Loaded coverage is not extended."

            Expect.equal
                created.CoveredLinkIds
                (Set.singleton "created-link")
                "Created coverage has its own occurrence."

            Expect.equal loaded.ValueId created.ValueId "Semantic definitions are still normalized and shared."
            Expect.equal loaded.PropertyKind created.PropertyKind "The property kind is equal."

            Expect.equal
                loaded.ContainerReferenceValueId
                created.ContainerReferenceValueId
                "The container pointer is equal."

            Expect.equal loaded.ReferenceSlotId created.ReferenceSlotId "The reference slot is equal."

            let repeatedEffect =
                assignProcessValue (Set.singleton "created-link") createdDraft withCreated
                |> expectOk

            Expect.equal
                (commit repeatedEffect withCreated)
                withCreated
                "Repeating the same exact-lineage payload is an exact no-op."

        testCase "same-value loaded reference in an occupied slot is replaced by catalog assignment"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]

            let entry =
                processCatalogEntry "protocol/same-value" (Some "protocol-slot") [
                    dependent "temperature" "Temperature" "20"
                ]

            let catalog = normalizeCatalog [ entry ]

            let loadedDraft =
                processReferenceDraft
                    "Protocol reference"
                    entry.Reference
                    entry.PropertyKind
                    (Some "protocol-slot")
                    AssignmentLineage.Loaded

            let withProcess =
                initial
                |> addTestProcess "p" [
                    link "l" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))
                ]

            let loadedPreparation =
                ensureValueDefinition
                    loadedDraft.Content.Category
                    loadedDraft.Content.Value
                    loadedDraft.Content.Unit
                    withProcess

            let loadedAssignment: ProcessAssignment = {
                Id = "loaded-reference"
                ValueId = loadedPreparation.ValueDefinition.Id
                PropertyKind = loadedDraft.PropertyKind
                CoveredLinkIds = Set.singleton "l"
                ContainerReferenceValueId = None
                ReferenceSlotId = loadedDraft.ReferenceSlotId
                Lineage = loadedDraft.Lineage
            }

            let withLoaded =
                let installed = installPreparation loadedPreparation withProcess
                let structuralProcess = installed.Processes["p"]

                {
                    installed with
                        Processes =
                            installed.Processes
                            |> Map.add "p" {
                                structuralProcess with
                                    Assignments =
                                        structuralProcess.Assignments |> Map.add loadedAssignment.Id loadedAssignment
                            }
                }

            let loaded = onlyProcessAssignment "p" withLoaded

            let actual =
                withLoaded |> run (assignCatalogProcessValue (Set.singleton "l") catalog entry)

            let slotAssignments =
                processAssignments "p" actual
                |> List.filter (fun assignment ->
                    assignment.ReferenceSlotId = Some "protocol-slot"
                    && assignment.CoveredLinkIds.Contains "l"
                )

            Expect.equal slotAssignments.Length 1 "The occupied slot has exactly one final reference occurrence."

            let replacement = slotAssignments.Head
            Expect.notEqual replacement.Id loaded.Id "The incompatible loaded occurrence is replaced."
            Expect.equal replacement.ValueId loaded.ValueId "The normalized reference definition remains shared."
            Expect.equal replacement.Lineage AssignmentLineage.Created "The catalog occurrence has intended lineage."

            let boundDependents =
                processAssignments "p" actual
                |> List.filter (fun assignment -> assignment.ContainerReferenceValueId = Some replacement.ValueId)

            Expect.equal boundDependents.Length 1 "The canonical catalog dependent is installed exactly once."

            let evidence =
                actual.MutationJournal
                |> List.choose (
                    function
                    | AdapterResourceReferenceReplaced(ownerId, before, after, removed, added, _) ->
                        Some(ownerId, before, after, removed, added)
                    | _ -> None
                )
                |> List.exactlyOne

            let ownerId, before, after, removed, added = evidence
            Expect.equal ownerId "p" "Replacement evidence is owner-exact."
            Expect.equal before loaded "The loaded occupied occurrence is the before evidence."
            Expect.equal after replacement "The intended catalog occurrence is the after evidence."
            Expect.isEmpty removed "No nonexistent old dependents are tombstoned."
            Expect.equal added boundDependents "Only the owner's exact new dependent is evidenced."

        testCase "a non-reference process value cannot carry a reference slot"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]

            let withProcess =
                initial
                |> addTestProcess "p" [
                    link "l" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))
                ]

            let before = {
                withProcess with
                    AvailabilityTopologyRevision = 5
                    AnnotationValueRevision = 7
                    LayerProjections = Map.ofList [ "test-layer", projection 11 ]
            }

            let invalidDraft = {
                processDraft "Temperature" "20" None with
                    ReferenceSlotId = Some "reference-only-slot"
                    Lineage = AssignmentLineage.Loaded
            }

            let result = assignProcessValue (Set.singleton "l") invalidDraft before

            match result with
            | Error(InconsistentCanonicalState _) -> ()
            | other -> failtestf "Expected non-reference slot rejection but got %A" other

            Expect.isEmpty before.Processes["p"].Assignments "No slot occupant is committed."
            Expect.isEmpty before.Values "No value definition is installed."
            Expect.isEmpty before.Properties "No property definition is installed."
            Expect.equal before.AvailabilityTopologyRevision 5 "Topology is unchanged."
            Expect.equal before.AnnotationValueRevision 7 "Value revision is unchanged."
            Expect.isEmpty before.MutationJournal "No journal entry is appended."
            Expect.isFalse before.LayerProjections["test-layer"].Stale "The projection remains fresh."

        testCase "reference removal rejects ambiguous exact backing before cascading dependents"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]
            let processLink = link "l" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))

            let referencePreparation =
                ensureValueDefinition
                    (category "Protocol reference")
                    (ProvenanceValue.Reference {
                        Scheme = "arc"
                        Id = "protocol/ambiguous"
                        Label = "Ambiguous"
                    })
                    None
                    initial

            let withReference = installPreparation referencePreparation initial

            let dependentPreparation =
                ensureValueDefinition (category "Temperature") (ProvenanceValue.Text "20") None withReference

            let withDefinitions = installPreparation dependentPreparation withReference

            let firstReference: ProcessAssignment = {
                Id = "reference-a"
                ValueId = referencePreparation.ValueDefinition.Id
                PropertyKind = AssignmentPropertyKind.Generic
                CoveredLinkIds = Set.singleton processLink.Id
                ContainerReferenceValueId = None
                ReferenceSlotId = Some "slot-a"
                Lineage = AssignmentLineage.Loaded
            }

            let secondReference: ProcessAssignment = {
                firstReference with
                    Id = "reference-b"
                    ReferenceSlotId = Some "slot-b"
                    Lineage = AssignmentLineage.DerivedFrom "other"
            }

            let dependentAssignment: ProcessAssignment = {
                Id = "dependent"
                ValueId = dependentPreparation.ValueDefinition.Id
                PropertyKind = AssignmentPropertyKind.Generic
                CoveredLinkIds = Set.singleton processLink.Id
                ContainerReferenceValueId = Some referencePreparation.ValueDefinition.Id
                ReferenceSlotId = None
                Lineage = AssignmentLineage.Loaded
            }

            let before = {
                withDefinitions with
                    Processes =
                        Map.ofList [
                            "p",
                            {
                                Id = "p"
                                OriginLayerId = "test-layer"
                                Name = None
                                Links = Map.ofList [ processLink.Id, processLink ]
                                Assignments =
                                    Map.ofList [
                                        firstReference.Id, firstReference
                                        secondReference.Id, secondReference
                                        dependentAssignment.Id, dependentAssignment
                                    ]
                            }
                        ]
                    AvailabilityTopologyRevision = 5
                    AnnotationValueRevision = 7
                    LayerProjections = Map.ofList [ "test-layer", projection 11 ]
            }

            let replacementEntry = processCatalogEntry "protocol/replacement" (Some "slot-a") []

            let replacementCatalog = normalizeCatalog [ replacementEntry ]

            let results = [
                removeProcessAssignmentLinks "p" firstReference.Id (Set.singleton processLink.Id) before
                assignCatalogProcessValue (Set.singleton processLink.Id) replacementCatalog replacementEntry before
                removeReferenceValueGlobally referencePreparation.ValueDefinition.Id before
            ]

            for result in results do
                match result with
                | Error(InconsistentCanonicalState _) -> ()
                | other -> failtestf "Expected ambiguous backing rejection but got %A" other

            Expect.equal before.Processes["p"].Assignments.Count 3 "All original assignments remain."
            Expect.equal before.Values.Count 2 "All original values remain."
            Expect.equal before.Properties.Count 2 "All original properties remain."
            Expect.equal before.AvailabilityTopologyRevision 5 "Topology is unchanged."
            Expect.equal before.AnnotationValueRevision 7 "Value revision is unchanged."
            Expect.isEmpty before.MutationJournal "No journal entry is appended."
            Expect.isFalse before.LayerProjections["test-layer"].Stale "The projection remains fresh."

        testCase "catalog assignment identity ignores display label"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]

            let canonicalEntry =
                processCatalogEntry "protocol/canonical" (Some "canonical-slot") [
                    dependent "canonical-dependent" "Temperature" "20"
                ]

            let catalog = normalizeCatalog [ canonicalEntry ]

            let callerEntry = {
                canonicalEntry with
                    Category = category "Caller category is ignored"
                    Reference = {
                        canonicalEntry.Reference with
                            Label = "Caller label is ignored"
                    }
                    Unit = Some(category "Caller unit is ignored")
                    AssignmentKind = AnnotationOwnerKind.Node
                    PropertyKind = AssignmentPropertyKind.AdapterSpecific nodeKind
                    Cardinality = ReferenceCardinality.Many
                    DependentProcessValues = []
            }

            let actual =
                initial
                |> addTestProcess "p" [
                    link "l" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))
                ]
                |> run (assignCatalogProcessValue (Set.singleton "l") catalog callerEntry)

            let referenceAssignment =
                processAssignments "p" actual
                |> List.find (fun assignment ->
                    actual.Values[assignment.ValueId].Value = ProvenanceValue.Reference canonicalEntry.Reference
                )

            Expect.equal
                actual.Values[referenceAssignment.ValueId].Value
                (ProvenanceValue.Reference canonicalEntry.Reference)
                "The catalog's canonical label and durable identity are stored."

            Expect.equal
                referenceAssignment.ReferenceSlotId
                (Some "canonical-slot")
                "The canonical catalog cardinality is authoritative."

            Expect.equal
                (processAssignments "p" actual).Length
                2
                "The canonical catalog dependent is created despite caller metadata differences."

            let repeatedEffect =
                assignCatalogProcessValue (Set.singleton "l") catalog callerEntry actual
                |> expectOk

            Expect.equal (commit repeatedEffect actual) actual "The durable identity is idempotent."

        testCase "duplicate catalog dependent keys reject the whole assignment"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]

            let entry =
                processCatalogEntry "protocol/duplicate-dependent" (Some "protocol-slot") [
                    dependent "duplicate" "Temperature" "20"
                    dependent "duplicate" "Temperature" "20"
                ]

            let catalog = normalizeCatalog [ entry ]

            let withProcess =
                initial
                |> addTestProcess "p" [
                    link "l" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))
                ]

            let before = {
                withProcess with
                    LayerProjections = Map.ofList [ "test-layer", projection 7 ]
            }

            let result = assignCatalogProcessValue (Set.singleton "l") catalog entry before

            match result with
            | Error(InconsistentCanonicalState _) -> ()
            | other -> failtestf "Expected duplicate dependent key rejection but got %A" other

            Expect.isEmpty before.Processes["p"].Assignments "No assignment is installed."
            Expect.isEmpty before.Values "No value definition is installed."
            Expect.isEmpty before.Properties "No property definition is installed."
            Expect.equal before.AvailabilityTopologyRevision 0 "Topology is unchanged."
            Expect.equal before.AnnotationValueRevision 0 "Value revision is unchanged."
            Expect.isEmpty before.MutationJournal "No journal entry is appended."
            Expect.isFalse before.LayerProjections["test-layer"].Stale "The cached projection remains fresh."

        testCase "multi-process replacement evidence contains only each owner's dependents"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]

            let firstEntry =
                processCatalogEntry "protocol/first" (Some "protocol-slot") [
                    dependent "first-dependent" "Temperature" "20"
                ]

            let secondEntry =
                processCatalogEntry "protocol/second" (Some "protocol-slot") [ dependent "second-dependent" "pH" "7" ]

            let catalog = normalizeCatalog [ firstEntry; secondEntry ]

            let assigned =
                initial
                |> addTestProcess "p1" [
                    link "l1" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))
                ]
                |> addTestProcess "p2" [ link "l2" (ProcessLinkShape.InputOnly nodeIds[0]) ]
                |> run (assignCatalogProcessValue (Set.ofList [ "l1"; "l2" ]) catalog firstEntry)

            let actual =
                assigned
                |> run (assignCatalogProcessValue (Set.ofList [ "l1"; "l2" ]) catalog secondEntry)

            let replacements =
                actual.MutationJournal
                |> List.choose (
                    function
                    | AdapterResourceReferenceReplaced(ownerId, _, _, _, addedDependents, _) ->
                        Some(ownerId, addedDependents)
                    | _ -> None
                )

            Expect.equal
                (replacements |> List.map fst |> Set.ofList)
                (Set.ofList [ "p1"; "p2" ])
                "Every affected owner has replacement evidence."

            for ownerId, addedDependents in replacements do
                Expect.equal addedDependents.Length 1 "Each owner lists only its one added dependent."

                Expect.isTrue
                    (addedDependents
                     |> List.forall (fun assignment -> actual.Processes[ownerId].Assignments.ContainsKey assignment.Id))
                    "Every listed dependent belongs to the journaled owner."

        testCase "a second reference on an occupied link slot replaces the first"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]

            let links = [
                link "l1" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))
                link "l2" (ProcessLinkShape.InputOnly nodeIds[0])
            ]

            let firstEntry = processCatalogEntry "protocol/first" (Some "protocol-slot") []
            let secondEntry = processCatalogEntry "protocol/second" (Some "protocol-slot") []
            let catalog = normalizeCatalog [ firstEntry; secondEntry ]

            let first =
                initial
                |> addTestProcess "p" links
                |> run (assignCatalogProcessValue (Set.ofList [ "l1"; "l2" ]) catalog firstEntry)

            let firstAssignment = onlyProcessAssignment "p" first

            let actual =
                first
                |> run (assignCatalogProcessValue (Set.singleton "l1") catalog secondEntry)

            let assignments = processAssignments "p" actual

            let firstRemainder =
                assignments |> List.find (fun item -> item.ValueId = firstAssignment.ValueId)

            let replacement =
                assignments |> List.find (fun item -> item.ValueId <> firstAssignment.ValueId)

            Expect.equal firstRemainder.CoveredLinkIds (Set.singleton "l2") "The old reference keeps unrelated links."
            Expect.equal replacement.CoveredLinkIds (Set.singleton "l1") "The new reference replaces the selected link."
            Expect.equal replacement.ReferenceSlotId (Some "protocol-slot") "The catalog slot is stamped."

            Expect.equal
                actual.AvailabilityTopologyRevision
                (first.AvailabilityTopologyRevision + 1)
                "Replacement is atomic."

            Expect.equal
                (tryFindCatalogEntry "arc" "protocol/first" catalog)
                (Some firstEntry)
                "The catalog stays read-only."

        testCase "references in different slots coexist on one link"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]
            let firstEntry = processCatalogEntry "protocol/first" (Some "protocol-slot") []
            let secondEntry = processCatalogEntry "instrument/first" (Some "instrument-slot") []
            let catalog = normalizeCatalog [ firstEntry; secondEntry ]

            let actual =
                initial
                |> addTestProcess "p" [
                    link "l" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))
                ]
                |> run (assignCatalogProcessValue (Set.singleton "l") catalog firstEntry)
                |> run (assignCatalogProcessValue (Set.singleton "l") catalog secondEntry)

            let assignments = processAssignments "p" actual
            Expect.equal assignments.Length 2 "Different slots retain both references."

            Expect.equal
                (assignments |> List.map _.ReferenceSlotId |> Set.ofList)
                (Set.ofList [ Some "protocol-slot"; Some "instrument-slot" ])
                "Slots remain distinct."

            Expect.isTrue
                (assignments |> List.forall (fun item -> item.CoveredLinkIds = Set.singleton "l"))
                "Both cover the link."

        testCase "assigning a catalog reference creates its dependent values bound to it"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]

            let entry =
                processCatalogEntry "protocol/with-dependent" (Some "protocol-slot") [
                    dependent "temperature" "Temperature" "20"
                ]

            let catalog = normalizeCatalog [ entry ]

            let actual =
                initial
                |> addTestProcess "p" [
                    link "l" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))
                ]
                |> run (assignCatalogProcessValue (Set.singleton "l") catalog entry)

            let assignments = processAssignments "p" actual

            let referenceAssignment =
                assignments
                |> List.find (fun item ->
                    match actual.Values[item.ValueId].Value with
                    | ProvenanceValue.Reference reference -> reference = entry.Reference
                    | _ -> false
                )

            let dependentAssignment =
                assignments |> List.find (fun item -> item.Id <> referenceAssignment.Id)

            Expect.equal
                dependentAssignment.ContainerReferenceValueId
                (Some referenceAssignment.ValueId)
                "The dependent points to the reference value."

            Expect.equal dependentAssignment.ReferenceSlotId None "Dependents do not carry slots."
            Expect.equal dependentAssignment.CoveredLinkIds (Set.singleton "l") "The dependent has identical coverage."

            Expect.equal
                dependentAssignment.Lineage
                (AssignmentLineage.DerivedFromCatalog("arc", "protocol/with-dependent", "temperature"))
                "Catalog lineage is exact."

            Expect.equal
                actual.Values[dependentAssignment.ValueId].Value
                (ProvenanceValue.Text "20")
                "Declared content is installed."

        testCase "replacing a reference replaces its bound dependents atomically"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]

            let firstEntry =
                processCatalogEntry "protocol/first" (Some "protocol-slot") [
                    dependent "temperature" "Temperature" "20"
                ]

            let secondEntry =
                processCatalogEntry "protocol/second" (Some "protocol-slot") [ dependent "ph" "pH" "7" ]

            let catalog = normalizeCatalog [ firstEntry; secondEntry ]

            let first =
                initial
                |> addTestProcess "p" [
                    link "l1" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))
                    link "l2" (ProcessLinkShape.InputOnly nodeIds[0])
                ]
                |> run (assignCatalogProcessValue (Set.ofList [ "l1"; "l2" ]) catalog firstEntry)

            let oldReference =
                processAssignments "p" first
                |> List.find (fun item ->
                    first.Values[item.ValueId].Value = ProvenanceValue.Reference firstEntry.Reference
                )

            let actual =
                first
                |> run (assignCatalogProcessValue (Set.singleton "l1") catalog secondEntry)

            let assignments = processAssignments "p" actual

            let oldFamily =
                assignments
                |> List.filter (fun item ->
                    item.ValueId = oldReference.ValueId
                    || item.ContainerReferenceValueId = Some oldReference.ValueId
                )

            let newReference =
                assignments
                |> List.find (fun item ->
                    actual.Values[item.ValueId].Value = ProvenanceValue.Reference secondEntry.Reference
                )

            let newFamily =
                assignments
                |> List.filter (fun item ->
                    item.ValueId = newReference.ValueId
                    || item.ContainerReferenceValueId = Some newReference.ValueId
                )

            Expect.isTrue
                (oldFamily |> List.forall (fun item -> item.CoveredLinkIds = Set.singleton "l2"))
                "The old family is subtracted link-granularly."

            Expect.equal oldFamily.Length 2 "The old reference and dependent remain on the unselected link."

            Expect.isTrue
                (newFamily |> List.forall (fun item -> item.CoveredLinkIds = Set.singleton "l1"))
                "The replacement family covers the selected link."

            Expect.equal newFamily.Length 2 "The new reference and dependent are installed together."

            Expect.equal
                actual.AvailabilityTopologyRevision
                (first.AvailabilityTopologyRevision + 1)
                "The aggregate bumps once."

            Expect.isTrue
                (actual.MutationJournal
                 |> List.exists (
                     function
                     | AdapterResourceReferenceReplaced _ -> true
                     | _ -> false
                 ))
                "Replacement evidence is journaled."

        testCase "catalog references and their dependent values reject direct mutation commands"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]

            let entry =
                processCatalogEntry "protocol/read-only" (Some "protocol-slot") [
                    dependent "temperature" "Temperature" "20"
                ]

            let catalog = normalizeCatalog [ entry ]

            let before =
                initial
                |> addTestProcess "p" [
                    link "l" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))
                ]
                |> run (assignCatalogProcessValue (Set.singleton "l") catalog entry)

            let assignments = processAssignments "p" before

            let referenceAssignment =
                assignments
                |> List.find (fun assignment -> assignment.ContainerReferenceValueId.IsNone)

            let dependentAssignment =
                assignments
                |> List.find (fun assignment -> assignment.ContainerReferenceValueId.IsSome)

            let dependentValue = before.Values[dependentAssignment.ValueId]
            let dependentProperty = before.Properties[dependentValue.PropertyId]

            let dependentDraft = {
                Content = {
                    Category = dependentProperty.Category
                    Value = dependentValue.Value
                    Unit = dependentValue.Unit
                }
                OwnerKind = AnnotationOwnerKind.Process
                PropertyKind = dependentAssignment.PropertyKind
                ContainerReferenceValueId = dependentAssignment.ContainerReferenceValueId
                ReferenceSlotId = None
                Lineage = AssignmentLineage.DerivedFrom dependentAssignment.Id
            }

            let changedDependent = {
                Category = dependentProperty.Category
                Value = ProvenanceValue.Text "changed"
                Unit = dependentValue.Unit
            }

            let changedReference = {
                Category = before.Properties[before.Values[referenceAssignment.ValueId].PropertyId].Category
                Value =
                    ProvenanceValue.Reference {
                        entry.Reference with
                            Label = "edited metadata"
                    }
                Unit = None
            }

            let rejected = [
                assignProcessValue (Set.singleton "l") dependentDraft before
                editProcessAssignment "p" dependentAssignment.Id changedDependent before
                editProcessAssignmentSubset "p" dependentAssignment.Id (Set.singleton "l") changedDependent before
                removeProcessAssignmentLinks "p" dependentAssignment.Id (Set.singleton "l") before
                CanonicalCommand.editValueGlobally dependentAssignment.ValueId changedDependent before
                CanonicalCommand.removeValuesGlobally (Set.singleton dependentAssignment.ValueId) before
                CanonicalCommand.removePropertyGlobally dependentProperty.Id before
                editProcessAssignment "p" referenceAssignment.Id changedReference before
                editProcessAssignmentSubset "p" referenceAssignment.Id (Set.singleton "l") changedReference before
                CanonicalCommand.editValueGlobally referenceAssignment.ValueId changedReference before
            ]

            for result in rejected do
                Expect.equal
                    result
                    (Error ProvenanceCommandError.ReadOnlyAdapterResourceMutation)
                    "Direct Recipe/Component mutation must be rejected before canonical state or journal changes."

            Expect.equal
                (tryFindCatalogEntry "arc" "protocol/read-only" catalog)
                (Some entry)
                "The catalog is unchanged."

            Expect.equal before.Processes["p"].Assignments.Count 2 "The reference family is unchanged."

        testCase "a container-bound projection cannot be assigned directly"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]
            let entry = processCatalogEntry "protocol/one" (Some "protocol-slot") []
            let catalog = normalizeCatalog [ entry ]

            let before =
                initial
                |> addTestProcess "p" [
                    link "has-reference" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))
                    link "missing-reference" (ProcessLinkShape.InputOnly nodeIds[0])
                ]
                |> run (assignCatalogProcessValue (Set.singleton "has-reference") catalog entry)

            let container = onlyProcessAssignment "p" before

            let boundDraft = {
                processDraft "Temperature" "20" None with
                    ContainerReferenceValueId = Some container.ValueId
                    Lineage = AssignmentLineage.Loaded
            }

            let result =
                assignProcessValue (Set.singleton "missing-reference") boundDraft before

            Expect.equal
                result
                (Error ProvenanceCommandError.ReadOnlyAdapterResourceMutation)
                "Dependent Recipe projections are installed only by catalog assignment."

            Expect.equal before.Processes["p"].Assignments.Count 1 "No implicit reference is assigned."
            Expect.equal before.AvailabilityTopologyRevision 1 "The rejected session is unchanged."

        testCase "a container-bound projection cannot be assigned as a mixed-link batch"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]
            let entry = processCatalogEntry "protocol/one" (Some "protocol-slot") []
            let catalog = normalizeCatalog [ entry ]

            let before =
                initial
                |> addTestProcess "p" [
                    link "l1" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))
                    link "l2" (ProcessLinkShape.InputOnly nodeIds[0])
                ]
                |> run (assignCatalogProcessValue (Set.singleton "l1") catalog entry)

            let container = onlyProcessAssignment "p" before

            let boundDraft = {
                processDraft "Temperature" "20" None with
                    ContainerReferenceValueId = Some container.ValueId
                    Lineage = AssignmentLineage.Loaded
            }

            let result = assignProcessValue (Set.ofList [ "l1"; "l2" ]) boundDraft before

            Expect.equal
                result
                (Error ProvenanceCommandError.ReadOnlyAdapterResourceMutation)
                "The whole direct projection batch is rejected before per-link container validation."

            Expect.equal before.Processes["p"].Assignments.Count 1 "The valid link is not partially assigned."
            Expect.equal before.AvailabilityTopologyRevision 1 "The rejected batch does not advance revisions."

        testCase "removing a reference from part of a process's links subtracts only those links from bound coverage"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]

            let entry =
                processCatalogEntry "protocol/one" (Some "protocol-slot") [ dependent "temperature" "Temperature" "20" ]

            let catalog = normalizeCatalog [ entry ]

            let assigned =
                initial
                |> addTestProcess "p" [
                    link "l1" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))
                    link "l2" (ProcessLinkShape.InputOnly nodeIds[0])
                ]
                |> run (assignCatalogProcessValue (Set.ofList [ "l1"; "l2" ]) catalog entry)

            let referenceAssignment =
                processAssignments "p" assigned
                |> List.find (fun item -> item.ReferenceSlotId = Some "protocol-slot")

            let actual =
                assigned
                |> run (removeProcessAssignmentLinks "p" referenceAssignment.Id (Set.singleton "l1"))

            let family =
                processAssignments "p" actual
                |> List.filter (fun item ->
                    item.ValueId = referenceAssignment.ValueId
                    || item.ContainerReferenceValueId = Some referenceAssignment.ValueId
                )

            Expect.equal family.Length 2 "The reference and dependent remain."

            Expect.isTrue
                (family |> List.forall (fun item -> item.CoveredLinkIds = Set.singleton "l2"))
                "Only removed links are subtracted."

        testCase "a bound assignment is deleted only when its coverage empties"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]

            let entry =
                processCatalogEntry "protocol/one" (Some "protocol-slot") [ dependent "temperature" "Temperature" "20" ]

            let catalog = normalizeCatalog [ entry ]

            let assigned =
                initial
                |> addTestProcess "p" [
                    link "l1" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))
                    link "l2" (ProcessLinkShape.InputOnly nodeIds[0])
                ]
                |> run (assignCatalogProcessValue (Set.ofList [ "l1"; "l2" ]) catalog entry)

            let referenceAssignment =
                processAssignments "p" assigned
                |> List.find (fun item -> item.ReferenceSlotId = Some "protocol-slot")

            let partiallyRemoved =
                assigned
                |> run (removeProcessAssignmentLinks "p" referenceAssignment.Id (Set.singleton "l1"))

            let retainedReference =
                processAssignments "p" partiallyRemoved
                |> List.find (fun item -> item.ValueId = referenceAssignment.ValueId)

            Expect.equal (processAssignments "p" partiallyRemoved).Length 2 "Non-empty bound coverage is retained."

            let actual =
                partiallyRemoved
                |> run (removeProcessAssignmentLinks "p" retainedReference.Id (Set.singleton "l2"))

            Expect.isEmpty
                actual.Processes["p"].Assignments
                "The reference and bound assignment delete at empty coverage."

        testCase "Recipe references are assigned, replaced and detached only through resource commands"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]
            let sourceEntry = processCatalogEntry "protocol/source" (Some "protocol-slot") []

            let replacementEntry =
                processCatalogEntry "protocol/replacement" (Some "protocol-slot") []

            let catalog = normalizeCatalog [ sourceEntry; replacementEntry ]

            let source =
                initial
                |> addTestProcess "source" [
                    link "source-link" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))
                ]
                |> addTestProcess "target" [
                    link "target-1" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))
                    link "target-2" (ProcessLinkShape.InputOnly nodeIds[0])
                ]
                |> run (assignCatalogProcessValue (Set.singleton "source-link") catalog sourceEntry)

            let sourceAssignment = onlyProcessAssignment "source" source

            let copiedDraft =
                processReferenceDraft
                    "Protocol reference"
                    sourceEntry.Reference
                    sourceEntry.PropertyKind
                    sourceAssignment.ReferenceSlotId
                    (AssignmentLineage.DerivedFrom sourceAssignment.Id)

            let directCopy = assignProcessValue (Set.singleton "target-1") copiedDraft source

            Expect.equal
                directCopy
                (Error ProvenanceCommandError.ReadOnlyAdapterResourceMutation)
                "A Recipe reference cannot be copied through the generic value command."

            let assigned =
                source
                |> run (assignCatalogProcessValue (Set.ofList [ "target-1"; "target-2" ]) catalog sourceEntry)

            let extended = onlyProcessAssignment "target" assigned

            Expect.equal extended.ReferenceSlotId (Some "protocol-slot") "Catalog assignment stamps the declared slot."

            Expect.equal
                extended.CoveredLinkIds
                (Set.ofList [ "target-1"; "target-2" ])
                "Catalog assignment covers the selected links."

            let directEdit =
                editProcessAssignmentSubset
                    "target"
                    extended.Id
                    (Set.singleton "target-2")
                    {
                        Category = category "Protocol reference"
                        Value =
                            ProvenanceValue.Reference {
                                sourceEntry.Reference with
                                    Label = "edited"
                            }
                        Unit = None
                    }
                    assigned

            Expect.equal
                directEdit
                (Error ProvenanceCommandError.ReadOnlyAdapterResourceMutation)
                "Recipe metadata cannot be split or edited through generic value commands."

            let replaced =
                assigned
                |> run (assignCatalogProcessValue (Set.singleton "target-1") catalog replacementEntry)

            let replacement =
                processAssignments "target" replaced
                |> List.filter (fun item ->
                    item.ReferenceSlotId = Some "protocol-slot"
                    && item.CoveredLinkIds.Contains "target-1"
                )

            Expect.equal replacement.Length 1 "The selected slot has exactly one assigned Recipe."

            Expect.equal
                replaced.Values[replacement.Head.ValueId].Value
                (ProvenanceValue.Reference replacementEntry.Reference)
                "The stored replacement Recipe wins on the selected link."

            let detached =
                replaced
                |> run (removeProcessAssignmentLinks "target" replacement.Head.Id (Set.singleton "target-1"))

            Expect.isFalse
                (processAssignments "target" detached
                 |> List.exists (fun item ->
                     detached.Values[item.ValueId].Value = ProvenanceValue.Reference replacementEntry.Reference
                 ))
                "Detach removes the selected Recipe association without mutating either catalog entry."

            Expect.equal
                (tryFindCatalogEntry "arc" sourceEntry.Reference.Id catalog)
                (Some sourceEntry)
                "The original stored Recipe remains unchanged."

            Expect.equal
                (tryFindCatalogEntry "arc" replacementEntry.Reference.Id catalog)
                (Some replacementEntry)
                "The replacement stored Recipe remains unchanged."

        testCase "global removal of a reference value applies the same link-scoped subtraction"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]

            let firstEntry =
                processCatalogEntry "protocol/first" (Some "protocol-slot") [
                    dependent "temperature" "Temperature" "20"
                ]

            let secondEntry =
                processCatalogEntry "protocol/second" (Some "other-slot") [ dependent "pressure" "Pressure" "1" ]

            let catalog = normalizeCatalog [ firstEntry; secondEntry ]

            let assigned =
                initial
                |> addTestProcess "p1" [
                    link "l1" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))
                ]
                |> addTestProcess "p2" [ link "l2" (ProcessLinkShape.InputOnly nodeIds[0]) ]
                |> run (assignCatalogProcessValue (Set.ofList [ "l1"; "l2" ]) catalog firstEntry)
                |> run (assignCatalogProcessValue (Set.singleton "l1") catalog secondEntry)

            let removedValueId =
                processAssignments "p1" assigned
                |> List.find (fun item ->
                    assigned.Values[item.ValueId].Value = ProvenanceValue.Reference firstEntry.Reference
                )
                |> _.ValueId

            let actual = assigned |> run (removeReferenceValueGlobally removedValueId)

            Expect.isTrue
                (actual.Processes
                 |> Map.forall (fun _ structuralProcess ->
                     structuralProcess.Assignments
                     |> Map.forall (fun _ item ->
                         item.ValueId <> removedValueId
                         && item.ContainerReferenceValueId <> Some removedValueId
                     )
                 ))
                "Every association to the selected reference is detached."

            Expect.isFalse (actual.Values.ContainsKey removedValueId) "The unreferenced session value is cleaned up."

            Expect.equal
                (tryFindCatalogEntry "arc" "protocol/first" catalog)
                (Some firstEntry)
                "The read-only catalog remains."

            Expect.equal (processAssignments "p1" actual).Length 2 "The unrelated reference family remains."
            Expect.isEmpty actual.Processes["p2"].Assignments "The selected family is removed from the second process."

            match
                actual.MutationJournal
                |> List.choose (
                    function
                    | ProcessAssignmentRemoved(tombstone, context) -> Some(tombstone, context)
                    | _ -> None
                )
                |> List.tryLast
            with
            | Some(_, context) ->
                Expect.equal context.Scope GlobalDefinition "Global removal carries global scope."
                Expect.equal context.Coverage.LinkIds (Set.ofList [ "l1"; "l2" ]) "All affected links are exact."
            | None -> failtest "Expected a global process-assignment removal."

        testCase "a newly created process property carries only the generic process kind"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]

            let actual =
                initial
                |> addTestProcess "p" [
                    link "l" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))
                ]
                |> run (assignProcessValue (Set.singleton "l") (processDraft "Temperature" "20" None))

            let assignment = onlyProcessAssignment "p" actual
            Expect.equal assignment.PropertyKind AssignmentPropertyKind.Generic "New process properties are generic."
            Expect.equal assignment.ContainerReferenceValueId None "No reference container is inferred."
            Expect.equal assignment.ReferenceSlotId None "No reference slot is inferred."
            Expect.equal assignment.Lineage AssignmentLineage.Created "The editor draft is created lineage."

            let invalidDraft = {
                processDraft "Temperature" "30" None with
                    PropertyKind = AssignmentPropertyKind.AdapterSpecific nodeKind
            }

            let rejected = assignProcessValue (Set.singleton "l") invalidDraft actual

            Expect.equal
                rejected
                (Error(
                    InconsistentCanonicalState
                        "A newly created process property must use AssignmentPropertyKind.Generic."
                ))
                "A new process draft cannot smuggle an adapter-specific kind."
    ]

let private structuralEditingTests =
    testList "structural editing" [
        testCase "adding an endpoint with a new kind and name creates a canonical node"
        <| fun _ ->
            let before = empty |> withTestLayer "test-layer"

            let actual =
                before
                |> run (
                    CanonicalCommand.addEndpoint
                        "test-layer"
                        ProvenanceSide.Input
                        nodeKind
                        {
                            Kind = nodeKind
                            Text = "Input [Sample Name]"
                        }
                        "new-sample"
                        0
                )

            let node = actual.Nodes |> Map.toSeq |> Seq.exactlyOne |> snd
            Expect.equal node.Name "new-sample" "The requested canonical node is created."
            Expect.equal node.Kind nodeKind "The adapter-declared endpoint kind is retained."
            Expect.equal actual.AvailabilityTopologyRevision 1 "The atomic endpoint command advances topology once."

            Expect.isTrue
                (actual.MutationJournal
                 |> List.exists (
                     function
                     | CanonicalNodeCreated created -> created = node
                     | _ -> false
                 ))
                "Canonical node creation is journalled."

        testCase "adding an endpoint reuses an existing equal canonical node"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "shared" ]
            let before = initial |> withTestLayer "test-layer"

            let actual =
                before
                |> run (
                    CanonicalCommand.addEndpoint
                        "test-layer"
                        ProvenanceSide.Output
                        nodeKind
                        {
                            Kind = nodeKind
                            Text = "Output [Sample Name]"
                        }
                        "shared"
                        0
                )

            Expect.equal actual.Nodes.Count 1 "No second canonical node is created."

            Expect.isTrue
                (actual.Layers["test-layer"].OutputEndpoints.ContainsKey nodeIds.Head)
                "The new appearance references the existing canonical owner."

            Expect.isFalse
                (actual.MutationJournal
                 |> List.exists (
                     function
                     | CanonicalNodeCreated _ -> true
                     | _ -> false
                 ))
                "Reuse does not claim another canonical-node creation."

            Expect.equal actual.AvailabilityTopologyRevision 1 "The appearance/link gesture advances topology once."

        testCase "a disconnected new endpoint is writeable through a one-sided link"
        <| fun _ ->
            let actual =
                empty
                |> withTestLayer "test-layer"
                |> run (
                    CanonicalCommand.addEndpoint
                        "test-layer"
                        ProvenanceSide.Output
                        nodeKind
                        { Kind = nodeKind; Text = "Output" }
                        "writeable-output"
                        0
                )

            let nodeId = actual.Nodes |> Map.toSeq |> Seq.exactlyOne |> fst
            let structuralProcess = actual.Processes |> Map.toSeq |> Seq.exactlyOne |> snd
            let processLink = structuralProcess.Links |> Map.toSeq |> Seq.exactlyOne |> snd

            Expect.equal
                processLink.Shape
                (ProcessLinkShape.OutputOnly nodeId)
                "The disconnected endpoint has a writeable one-sided structural link."

            Expect.contains
                actual.Layers["test-layer"].StructuralProcessIds
                structuralProcess.Id
                "The owning layer retains the structural process."

        testCase "a group connection gesture advances topology exactly once"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "I1"; "O1"; "I2"; "O2" ]

            let before =
                initial
                |> withTestLayer "test-layer"
                |> addTestAppearance "test-layer" ProvenanceSide.Input nodeIds[0] 0
                |> addTestAppearance "test-layer" ProvenanceSide.Output nodeIds[1] 0
                |> addTestAppearance "test-layer" ProvenanceSide.Input nodeIds[2] 1
                |> addTestAppearance "test-layer" ProvenanceSide.Output nodeIds[3] 1

            let actual =
                before
                |> run (CanonicalCommand.connectNodes "test-layer" [ nodeIds[0], nodeIds[1]; nodeIds[2], nodeIds[3] ])

            Expect.equal
                (actual.Processes |> Map.toList |> List.sumBy (snd >> _.Links.Count))
                2
                "Every resolved pair becomes one exact structural link."

            Expect.equal actual.AvailabilityTopologyRevision 1 "The complete group gesture advances topology once."

        testCase "connecting an existing pair again changes nothing"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "I"; "O" ]

            let before =
                initial
                |> withTestLayer "test-layer"
                |> addTestAppearance "test-layer" ProvenanceSide.Input nodeIds[0] 0
                |> addTestAppearance "test-layer" ProvenanceSide.Output nodeIds[1] 0
                |> run (CanonicalCommand.connectNodes "test-layer" [ nodeIds[0], nodeIds[1] ])

            let effect =
                CanonicalCommand.connectNodes "test-layer" [ nodeIds[0], nodeIds[1] ] before
                |> expectOk

            Expect.equal (commit effect before) before "A duplicate pair is an exact idempotent no-op."

        testCase "a new connection carries no annotations"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "I"; "O" ]

            let annotated =
                initial
                |> run (assignmentCommand (Set.ofList nodeIds) (draft "Organism" "Human" None) NoOverwrite)

            let actual =
                annotated
                |> withTestLayer "test-layer"
                |> addTestAppearance "test-layer" ProvenanceSide.Input nodeIds[0] 0
                |> addTestAppearance "test-layer" ProvenanceSide.Output nodeIds[1] 0
                |> run (CanonicalCommand.connectNodes "test-layer" [ nodeIds[0], nodeIds[1] ])

            let structuralProcess = actual.Processes |> Map.toSeq |> Seq.exactlyOne |> snd
            Expect.isEmpty structuralProcess.Assignments "Endpoint annotations are never copied onto the new process."

        testCase "assigning a process value then creating a new link leaves the new link uncovered"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "I"; "O1"; "O2" ]

            let connected =
                initial
                |> withTestLayer "test-layer"
                |> addTestAppearance "test-layer" ProvenanceSide.Input nodeIds[0] 0
                |> addTestAppearance "test-layer" ProvenanceSide.Output nodeIds[1] 0
                |> addTestAppearance "test-layer" ProvenanceSide.Output nodeIds[2] 1
                |> run (CanonicalCommand.connectNodes "test-layer" [ nodeIds[0], nodeIds[1] ])

            let existingLink =
                connected.Processes
                |> Map.toSeq
                |> Seq.collect (snd >> _.Links >> Map.toSeq)
                |> Seq.exactlyOne
                |> fst

            let assigned =
                connected
                |> run (assignProcessValue (Set.singleton existingLink) (processDraft "Temperature" "20" None))

            let actual =
                assigned
                |> run (CanonicalCommand.connectNodes "test-layer" [ nodeIds[0], nodeIds[2] ])

            let newLink =
                actual.Processes
                |> Map.toSeq
                |> Seq.collect (snd >> _.Links >> Map.toSeq)
                |> Seq.map snd
                |> Seq.find (fun item -> item.Shape = ProcessLinkShape.Between(nodeIds[0], nodeIds[2]))

            Expect.isTrue
                (actual.Processes
                 |> Map.forall (fun _ structuralProcess ->
                     structuralProcess.Assignments
                     |> Map.forall (fun _ assignment -> not (assignment.CoveredLinkIds.Contains newLink.Id))
                 ))
                "A later connection receives no existing process assignment."

        testCase "a group-to-group process drop covers only existing links, never their Cartesian product"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "I1"; "I2"; "I3"; "O1"; "O2"; "O3" ]

            let connected =
                initial
                |> withTestLayer "test-layer"
                |> addTestAppearance "test-layer" ProvenanceSide.Input nodeIds[0] 0
                |> addTestAppearance "test-layer" ProvenanceSide.Input nodeIds[1] 1
                |> addTestAppearance "test-layer" ProvenanceSide.Input nodeIds[2] 2
                |> addTestAppearance "test-layer" ProvenanceSide.Output nodeIds[3] 0
                |> addTestAppearance "test-layer" ProvenanceSide.Output nodeIds[4] 1
                |> addTestAppearance "test-layer" ProvenanceSide.Output nodeIds[5] 2
                |> run (CanonicalCommand.connectNodes "test-layer" [ nodeIds[0], nodeIds[3]; nodeIds[2], nodeIds[5] ])

            let existingLinks =
                connected.Processes
                |> Map.toSeq
                |> Seq.collect (snd >> _.Links >> Map.keys)
                |> Set.ofSeq

            let actual =
                connected
                |> run (assignProcessValue existingLinks (processDraft "Temperature" "20" None))

            let coverage =
                actual.Processes
                |> Map.toSeq
                |> Seq.collect (snd >> _.Assignments >> Map.toSeq)
                |> Seq.collect (snd >> _.CoveredLinkIds)
                |> Set.ofSeq

            Expect.equal coverage existingLinks "Only the two exact pre-existing links are targeted."
            Expect.equal coverage.Count 2 "The 3x3 group is never expanded to nine links."

        testCase "promotion is deterministic regardless of the order pairs are supplied in"
        <| fun _ ->
            let runOrder pairOrder =
                let nodeIds, initial = withNodes [ "I"; "O-first"; "O-second"; "O-third" ]

                let before =
                    initial
                    |> withTestLayer "test-layer"
                    |> addTestAppearance "test-layer" ProvenanceSide.Input nodeIds[0] 0
                    |> addTestAppearance "test-layer" ProvenanceSide.Output nodeIds[1] 0
                    |> addTestAppearance "test-layer" ProvenanceSide.Output nodeIds[2] 1
                    |> addTestAppearance "test-layer" ProvenanceSide.Output nodeIds[3] 2
                    |> addLayerProcess "loaded-input" [
                        link "retained-link" (ProcessLinkShape.InputOnly nodeIds[0])
                    ]

                let pairs =
                    pairOrder |> List.map (fun outputIndex -> nodeIds[0], nodeIds[outputIndex])

                let actual = before |> run (CanonicalCommand.connectNodes "test-layer" pairs)
                actual.Processes["loaded-input"].Links["retained-link"].Shape

            let expected = runOrder [ 1; 2; 3 ]
            Expect.equal (runOrder [ 3; 1; 2 ]) expected "Supplied pair order does not select retention."
            Expect.equal (runOrder [ 2; 3; 1 ]) expected "Another supplied order gives the same retained pair."

        testCase "a loaded one-sided process is never absorbed as scaffolding"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "I"; "O" ]

            let before =
                initial
                |> withTestLayer "test-layer"
                |> addTestAppearance "test-layer" ProvenanceSide.Input nodeIds[0] 0
                |> addTestAppearance "test-layer" ProvenanceSide.Output nodeIds[1] 0
                |> addLayerProcess "loaded-input" [
                    link "loaded-input-link" (ProcessLinkShape.InputOnly nodeIds[0])
                ]
                |> addLayerProcess "loaded-output" [
                    link "loaded-output-link" (ProcessLinkShape.OutputOnly nodeIds[1])
                ]

            let actual =
                before
                |> run (CanonicalCommand.connectNodes "test-layer" [ nodeIds[0], nodeIds[1] ])

            Expect.isTrue
                (actual.Processes.ContainsKey "loaded-input")
                "The promoted loaded process retains its identity."

            Expect.isTrue
                (actual.Processes.ContainsKey "loaded-output")
                "The other loaded one-sided process is not absorbed."

            Expect.isFalse
                (actual.MutationJournal
                 |> List.exists (
                     function
                     | ProcessLinkRemoved(processId, _, _) when processId = "loaded-output" -> true
                     | _ -> false
                 ))
                "No loaded scaffolding link is removed."

        testCase "a reshape is journalled as a reshape, not as a removal plus a creation"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "I"; "O" ]

            let before =
                initial
                |> withTestLayer "test-layer"
                |> addTestAppearance "test-layer" ProvenanceSide.Input nodeIds[0] 0
                |> addTestAppearance "test-layer" ProvenanceSide.Output nodeIds[1] 0
                |> addLayerProcess "loaded" [ link "retained" (ProcessLinkShape.InputOnly nodeIds[0]) ]

            let actual =
                before
                |> run (CanonicalCommand.connectNodes "test-layer" [ nodeIds[0], nodeIds[1] ])

            Expect.isTrue
                (actual.MutationJournal
                 |> List.exists (
                     function
                     | StructuralProcessReshaped(beforeProcess, afterProcess) ->
                         beforeProcess.Id = "loaded" && afterProcess.Id = "loaded"
                     | _ -> false
                 ))
                "Promotion emits one in-place reshape."

            Expect.isFalse
                (actual.MutationJournal
                 |> List.exists (
                     function
                     | StructuralProcessCreated _
                     | ProcessLinkRemoved _ -> true
                     | _ -> false
                 ))
                "The loaded process is neither removed nor recreated."

            Expect.equal actual.AvailabilityTopologyRevision 1 "The reshape advances topology once."

        testCase "a 1x1 promotion keeps the process id, the link id and its coverage"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "I"; "O" ]

            let loaded =
                initial
                |> withTestLayer "test-layer"
                |> addTestAppearance "test-layer" ProvenanceSide.Input nodeIds[0] 0
                |> addTestAppearance "test-layer" ProvenanceSide.Output nodeIds[1] 0
                |> addLayerProcess "loaded" [ link "retained" (ProcessLinkShape.InputOnly nodeIds[0]) ]
                |> run (assignProcessValue (Set.singleton "retained") (processDraft "Temperature" "20" None))

            let before = { loaded with MutationJournal = [] }
            let assignment = onlyProcessAssignment "loaded" before

            let actual =
                before
                |> run (CanonicalCommand.connectNodes "test-layer" [ nodeIds[0], nodeIds[1] ])

            Expect.equal
                actual.Processes["loaded"].Links["retained"].Shape
                (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))
                "The retained link is completed in place."

            Expect.equal
                actual.Processes["loaded"].Assignments[assignment.Id].CoveredLinkIds
                (Set.singleton "retained")
                "Coverage on the retained link survives promotion."

        testCase "a fan-out promotion retains the link id on the first pair by layer order position"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "I"; "O-late"; "O-first" ]

            let loaded =
                initial
                |> withTestLayer "test-layer"
                |> addTestAppearance "test-layer" ProvenanceSide.Input nodeIds[0] 0
                |> addTestAppearance "test-layer" ProvenanceSide.Output nodeIds[1] 5
                |> addTestAppearance "test-layer" ProvenanceSide.Output nodeIds[2] 1
                |> addLayerProcess "loaded" [ link "retained" (ProcessLinkShape.InputOnly nodeIds[0]) ]
                |> run (assignProcessValue (Set.singleton "retained") (processDraft "Temperature" "20" None))

            let before = { loaded with MutationJournal = [] }
            let assignment = onlyProcessAssignment "loaded" before

            let actual =
                before
                |> run (CanonicalCommand.connectNodes "test-layer" [ nodeIds[0], nodeIds[1]; nodeIds[0], nodeIds[2] ])

            Expect.equal
                actual.Processes["loaded"].Links["retained"].Shape
                (ProcessLinkShape.Between(nodeIds[0], nodeIds[2]))
                "The lowest layer-order output receives the retained ID."

            let additional =
                actual.Processes["loaded"].Links
                |> Map.toSeq
                |> Seq.map snd
                |> Seq.find (fun processLink -> processLink.Id <> "retained")

            Expect.equal
                additional.Shape
                (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))
                "The later pair receives a fresh link."

            Expect.equal
                actual.Processes["loaded"].Assignments[assignment.Id].CoveredLinkIds
                (Set.singleton "retained")
                "The additional fan-out link starts without copied coverage."

        testCase "removing a connection strands only endpoints with no other incidence"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "I"; "O-removed"; "O-connected" ]

            let before =
                initial
                |> withTestLayer "test-layer"
                |> addTestAppearance "test-layer" ProvenanceSide.Input nodeIds[0] 0
                |> addTestAppearance "test-layer" ProvenanceSide.Output nodeIds[1] 0
                |> addTestAppearance "test-layer" ProvenanceSide.Output nodeIds[2] 1
                |> addLayerProcess "removed-process" [
                    link "removed-link" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))
                ]
                |> addLayerProcess "remaining-process" [
                    link "remaining-link" (ProcessLinkShape.Between(nodeIds[0], nodeIds[2]))
                ]

            let actual =
                before |> run (CanonicalCommand.disconnectLinks (Set.singleton "removed-link"))

            let allShapes =
                actual.Processes
                |> Map.toSeq
                |> Seq.collect (snd >> _.Links >> Map.toSeq)
                |> Seq.map (snd >> _.Shape)
                |> Set.ofSeq

            Expect.contains
                allShapes
                (ProcessLinkShape.OutputOnly nodeIds[1])
                "The disconnected output receives a one-sided continuation."

            Expect.isFalse
                (allShapes.Contains(ProcessLinkShape.InputOnly nodeIds[0]))
                "The input remains incident through its other connection."

        testCase "removing a connection subtracts coverage and deletes emptied assignments"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "I"; "O" ]

            let assigned =
                initial
                |> withTestLayer "test-layer"
                |> addTestAppearance "test-layer" ProvenanceSide.Input nodeIds[0] 0
                |> addTestAppearance "test-layer" ProvenanceSide.Output nodeIds[1] 0
                |> addLayerProcess "loaded" [
                    link "removed" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))
                ]
                |> run (assignProcessValue (Set.singleton "removed") (processDraft "Temperature" "20" None))

            let before = { assigned with MutationJournal = [] }
            let assignment = onlyProcessAssignment "loaded" before

            let actual =
                before |> run (CanonicalCommand.disconnectLinks (Set.singleton "removed"))

            Expect.isEmpty actual.Processes["loaded"].Assignments "Empty assignment coverage is deleted."
            Expect.isFalse (actual.Values.ContainsKey assignment.ValueId) "The orphan value is removed."
            Expect.isTrue (actual.Processes.ContainsKey "loaded") "The structural process remains."
            Expect.equal actual.AvailabilityTopologyRevision (before.AvailabilityTopologyRevision + 1) "One bump."

            Expect.isTrue
                (actual.MutationJournal
                 |> List.exists (
                     function
                     | ProcessAssignmentRemoved(tombstone, context) ->
                         tombstone.OwnerId = "loaded"
                         && tombstone.Assignment = assignment
                         && context.Coverage.LinkIds = Set.singleton "removed"
                     | _ -> false
                 ))
                "The emptied assignment has a complete tombstone."

        testCase "the output continuation keeps the loaded process identity"
        <| fun _ ->
            let runOnce () =
                let nodeIds, initial = withNodes [ "I"; "O" ]

                let before =
                    initial
                    |> withTestLayer "test-layer"
                    |> addTestAppearance "test-layer" ProvenanceSide.Input nodeIds[0] 0
                    |> addTestAppearance "test-layer" ProvenanceSide.Output nodeIds[1] 0
                    |> addLayerProcess "loaded" [
                        link "loaded-link" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))
                    ]

                let actual =
                    before |> run (CanonicalCommand.disconnectLinks (Set.singleton "loaded-link"))

                nodeIds, actual

            let firstIds, first = runOnce ()
            let secondIds, second = runOnce ()

            let shapes session =
                session.Processes["loaded"].Links
                |> Map.toSeq
                |> Seq.map (snd >> _.Shape)
                |> Set.ofSeq

            Expect.contains
                (shapes first)
                (ProcessLinkShape.OutputOnly firstIds[1])
                "The loaded process owns the output continuation."

            Expect.contains
                (shapes first)
                (ProcessLinkShape.InputOnly firstIds[0])
                "The stranded input partition remains writeable."

            Expect.equal
                (shapes second)
                (Set.ofList [
                    ProcessLinkShape.OutputOnly secondIds[1]
                    ProcessLinkShape.InputOnly secondIds[0]
                ])
                "Repeating the operation chooses the same semantic partitions."
    ]

let private globalSidebarTests =
    testList "global sidebar operations" [
        testCase "a global value edit updates every referencing assignment"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]

            let assigned =
                initial
                |> addTestProcess "p" [
                    link "l" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))
                ]
                |> run (assignmentCommand (Set.singleton nodeIds[0]) (draft "Temperature" "20" None) NoOverwrite)
                |> run (assignProcessValue (Set.singleton "l") (processDraft "Temperature" "20" None))

            let before = { assigned with MutationJournal = [] }

            let nodeAssignment =
                before.Nodes[nodeIds[0]].Assignments |> Map.toSeq |> Seq.exactlyOne |> snd

            let processAssignment = onlyProcessAssignment "p" before
            Expect.equal nodeAssignment.ValueId processAssignment.ValueId "The reusable definition is shared."

            let actual =
                before
                |> run (CanonicalCommand.editValueGlobally nodeAssignment.ValueId (content "Temperature" "30" None))

            let editedNode = actual.Nodes[nodeIds[0]].Assignments[nodeAssignment.Id]
            let editedProcess = actual.Processes["p"].Assignments[processAssignment.Id]

            Expect.equal
                actual.Values[editedNode.ValueId].Value
                (ProvenanceValue.Text "30")
                "The node owner observes the global content."

            Expect.equal
                actual.Values[editedProcess.ValueId].Value
                (ProvenanceValue.Text "30")
                "The process owner observes the same global content."

            let context =
                actual.MutationJournal
                |> List.pick (
                    function
                    | PropertyValueDefinitionUpdated(_, _, context) -> Some context
                    | NodeAssignmentValueChanged(_, _, _, context)
                    | ProcessAssignmentValueChanged(_, _, _, context) when context.Scope = GlobalDefinition ->
                        Some context
                    | _ -> None
                )

            Expect.equal context.Scope GlobalDefinition "The edit remains explicitly global."

            Expect.equal
                context.Coverage.AssignmentIds
                (Set.ofList [ nodeAssignment.Id; processAssignment.Id ])
                "Every exact referencing assignment is recorded."

            Expect.equal context.Coverage.LinkIds (Set.singleton "l") "Process coverage is exact."

        testCase "a global value edit advances only the value revision"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A" ]

            let assigned =
                initial
                |> run (assignmentCommand (Set.singleton nodeIds.Head) (draft "Temperature" "20" None) NoOverwrite)

            let before = { assigned with MutationJournal = [] }

            let assignment =
                before.Nodes[nodeIds.Head].Assignments |> Map.toSeq |> Seq.exactlyOne |> snd

            let actual =
                before
                |> run (CanonicalCommand.editValueGlobally assignment.ValueId (content "Temperature" "30" None))

            Expect.equal
                actual.AvailabilityTopologyRevision
                before.AvailabilityTopologyRevision
                "Global content editing leaves reachability unchanged."

            Expect.equal
                actual.AnnotationValueRevision
                (before.AnnotationValueRevision + 1)
                "Global content editing advances the value revision once."

        testCase "global value removal removes every referencing assignment from node and process owners"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]

            let assigned =
                initial
                |> addTestProcess "p" [
                    link "l" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))
                ]
                |> run (assignmentCommand (Set.singleton nodeIds[0]) (draft "Temperature" "20" None) NoOverwrite)
                |> run (assignProcessValue (Set.singleton "l") (processDraft "Temperature" "20" None))

            let before = { assigned with MutationJournal = [] }

            let valueId =
                before.Nodes[nodeIds[0]].Assignments
                |> Map.toSeq
                |> Seq.exactlyOne
                |> snd
                |> _.ValueId

            let actual =
                before |> run (CanonicalCommand.removeValuesGlobally (Set.singleton valueId))

            Expect.isEmpty actual.Nodes[nodeIds[0]].Assignments "The node bucket loses the selected value."
            Expect.isEmpty actual.Processes["p"].Assignments "The process bucket loses the selected value."
            Expect.isFalse (actual.Values.ContainsKey valueId) "The selected value definition is deleted."

            Expect.isTrue
                (actual.MutationJournal
                 |> List.exists (
                     function
                     | PropertyValueDefinitionDeleted(value, tombstones, context) ->
                         value.Id = valueId && tombstones.Length = 2 && context.Scope = GlobalDefinition
                     | _ -> false
                 ))
                "The global deletion carries both owner tombstones."

        testCase "global value removal with assignments advances both revisions"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A" ]

            let assigned =
                initial
                |> run (assignmentCommand (Set.singleton nodeIds.Head) (draft "Temperature" "20" None) NoOverwrite)

            let before = { assigned with MutationJournal = [] }

            let valueId =
                before.Nodes[nodeIds.Head].Assignments
                |> Map.toSeq
                |> Seq.exactlyOne
                |> snd
                |> _.ValueId

            let actual =
                before |> run (CanonicalCommand.removeValuesGlobally (Set.singleton valueId))

            Expect.equal
                actual.AvailabilityTopologyRevision
                (before.AvailabilityTopologyRevision + 1)
                "Removing ownership advances topology once."

            Expect.equal
                actual.AnnotationValueRevision
                (before.AnnotationValueRevision + 1)
                "Deleting displayed content advances value once."

        testCase "global property removal removes the property, its values and every referencing assignment"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]

            let assigned =
                initial
                |> addTestProcess "p" [
                    link "l" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))
                ]
                |> run (assignmentCommand (Set.singleton nodeIds[0]) (draft "Temperature" "20" None) NoOverwrite)
                |> run (assignProcessValue (Set.singleton "l") (processDraft "Temperature" "30" None))

            let before = { assigned with MutationJournal = [] }

            let propertyId =
                before.Nodes[nodeIds[0]].Assignments
                |> Map.toSeq
                |> Seq.exactlyOne
                |> snd
                |> _.ValueId
                |> fun valueId -> before.Values[valueId].PropertyId

            let removedValueIds =
                before.Values
                |> Map.toSeq
                |> Seq.choose (fun (valueId, value) -> if value.PropertyId = propertyId then Some valueId else None)
                |> Set.ofSeq

            let actual = before |> run (CanonicalCommand.removePropertyGlobally propertyId)

            Expect.isFalse (actual.Properties.ContainsKey propertyId) "The property definition is deleted."

            Expect.isTrue
                (removedValueIds
                 |> Set.forall (fun valueId -> actual.Values.ContainsKey valueId |> not))
                "Every value under the property is deleted."

            Expect.isEmpty actual.Nodes[nodeIds[0]].Assignments "Node references are removed."
            Expect.isEmpty actual.Processes["p"].Assignments "Process references are removed."

            Expect.isTrue
                (actual.MutationJournal
                 |> List.exists (
                     function
                     | PropertyDefinitionDeleted(property, values, tombstones, context) ->
                         property.Id = propertyId
                         && (values |> List.map _.Id |> Set.ofList) = removedValueIds
                         && tombstones.Length = 2
                         && context.Scope = GlobalDefinition
                     | _ -> false
                 ))
                "The property deletion carries all values and backing tombstones."

            Expect.equal
                actual.AvailabilityTopologyRevision
                (before.AvailabilityTopologyRevision + 1)
                "Removing assignments advances topology once."

            Expect.equal
                actual.AnnotationValueRevision
                (before.AnnotationValueRevision + 1)
                "Removing the global property advances value once."

        testCase "global removal aggregating several backing value ids applies to all of them"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]

            let prepared =
                ensureValueDefinition (category "Temperature") (ProvenanceValue.Text "20") None initial

            let duplicate = {
                prepared.ValueDefinition with
                    Id = prepared.ValueDefinition.Id + "-duplicate"
            }

            let first =
                existingAssignment "first" prepared.ValueDefinition.Id AssignmentPropertyKind.Generic None

            let second =
                existingAssignment "second" duplicate.Id AssignmentPropertyKind.Generic None

            let before = {
                (initial
                 |> installPreparation prepared
                 |> addAssignment nodeIds[0] first
                 |> addAssignment nodeIds[1] second) with
                    Values =
                        Map.ofList [
                            prepared.ValueDefinition.Id, prepared.ValueDefinition
                            duplicate.Id, duplicate
                        ]
            }

            let backingValueIds = Set.ofList [ first.ValueId; second.ValueId ]

            let actual = before |> run (CanonicalCommand.removeValuesGlobally backingValueIds)

            Expect.isTrue
                (nodeIds |> List.forall (fun nodeId -> actual.Nodes[nodeId].Assignments.IsEmpty))
                "Every aggregate member loses its exact backing assignment."

            Expect.isTrue
                (backingValueIds
                 |> Set.forall (fun valueId -> actual.Values.ContainsKey valueId |> not))
                "Every distinct backing value definition is deleted."

            Expect.equal actual.AvailabilityTopologyRevision 1 "The aggregate removal bumps topology once."
            Expect.equal actual.AnnotationValueRevision 1 "The aggregate removal bumps value once."
    ]

let private journalScopeAndCoverageTests =
    testList "journal scope and resolved coverage" [
        testCase
            "a global sidebar edit and an owner-scoped edit resolving to the same assignments remain distinguishable in the journal"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]
            let entry = processCatalogEntry "protocol/one" (Some "protocol-slot") []
            let catalog = normalizeCatalog [ entry ]

            let assigned =
                initial
                |> addTestProcess "p" [
                    link "l" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))
                ]
                |> run (assignCatalogProcessValue (Set.singleton "l") catalog entry)

            let before = { assigned with MutationJournal = [] }
            let assignment = onlyProcessAssignment "p" before

            let ownerScoped =
                before
                |> run (removeProcessAssignmentLinks "p" assignment.Id assignment.CoveredLinkIds)

            let globalResult = before |> run (removeReferenceValueGlobally assignment.ValueId)

            Expect.equal
                {
                    ownerScoped with
                        MutationJournal = []
                }
                {
                    globalResult with
                        MutationJournal = []
                }
                "The two operations have identical final canonical state."

            let removalScope session =
                session.MutationJournal
                |> List.pick (
                    function
                    | ProcessAssignmentRemoved(_, context) -> Some context.Scope
                    | _ -> None
                )

            Expect.equal
                (removalScope ownerScoped)
                (OwnerScoped(Set.singleton (ProcessAssignmentOwner "p")))
                "The direct owner operation remains owner-scoped."

            Expect.equal
                (removalScope globalResult)
                GlobalDefinition
                "The global sidebar operation remains globally scoped despite the same final state."

        testCase "a value change records its resolved assignment and link coverage"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]

            let assigned =
                initial
                |> addTestProcess "p" [
                    link "l1" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))
                    link "l2" (ProcessLinkShape.InputOnly nodeIds[0])
                ]
                |> run (assignProcessValue (Set.ofList [ "l1"; "l2" ]) (processDraft "Temperature" "20" None))

            let before = { assigned with MutationJournal = [] }
            let assignment = onlyProcessAssignment "p" before

            let actual =
                before
                |> run (editProcessAssignment "p" assignment.Id (content "Temperature" "30" None))

            let context =
                actual.MutationJournal
                |> List.pick (
                    function
                    | PropertyValueDefinitionUpdated(_, _, context)
                    | ProcessAssignmentValueChanged(_, _, _, context) -> Some context
                    | _ -> None
                )

            Expect.equal
                context.Scope
                (OwnerScoped(Set.singleton (ProcessAssignmentOwner "p")))
                "The value change records its exact process owner."

            Expect.equal
                context.Coverage.AssignmentIds
                (Set.singleton assignment.Id)
                "The value change records the exact assignment."

            Expect.equal
                context.Coverage.LinkIds
                assignment.CoveredLinkIds
                "The value change records every covered link resolved by the command."

        testCase "an assignment removal records the owner and links that lost it"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]
            let entry = processCatalogEntry "protocol/one" (Some "protocol-slot") []
            let catalog = normalizeCatalog [ entry ]

            let assigned =
                initial
                |> addTestProcess "p" [
                    link "l1" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))
                    link "l2" (ProcessLinkShape.InputOnly nodeIds[0])
                ]
                |> run (assignCatalogProcessValue (Set.ofList [ "l1"; "l2" ]) catalog entry)

            let before = { assigned with MutationJournal = [] }
            let assignment = onlyProcessAssignment "p" before

            let actual = before |> run (removeReferenceValueGlobally assignment.ValueId)

            let removal =
                actual.MutationJournal
                |> List.pick (
                    function
                    | ProcessAssignmentRemoved(tombstone, context) -> Some(tombstone, context)
                    | _ -> None
                )

            let tombstone, removalContext = removal
            Expect.equal tombstone.OwnerId "p" "The removal tombstone retains the canonical process owner."
            Expect.equal tombstone.Assignment assignment "The complete removed assignment is retained."

            Expect.equal
                removalContext.Coverage.LinkIds
                assignment.CoveredLinkIds
                "The exact links that lost the assignment are retained."

            let deletion =
                actual.MutationJournal
                |> List.tryPick (
                    function
                    | PropertyValueDefinitionDeleted(value, tombstones, context) when value.Id = assignment.ValueId ->
                        Some(tombstones, context)
                    | _ -> None
                )

            Expect.isSome deletion "Global deletion must have an explicit value-definition record."

            let backingTombstones, deletionContext =
                deletion
                |> Option.defaultWith (fun () ->
                    [],
                    {
                        Scope = OwnerScoped Set.empty
                        Coverage = {
                            AssignmentIds = Set.empty
                            LinkIds = Set.empty
                        }
                    }
                )

            Expect.contains
                backingTombstones
                (ProcessTombstone tombstone)
                "The global value deletion retains the affected backing assignment."

            Expect.equal
                deletionContext.Scope
                GlobalDefinition
                "The explicit deletion record says that the value definition was globally deleted."

            Expect.equal
                deletionContext.Coverage.AssignmentIds
                (Set.singleton assignment.Id)
                "The deletion record carries its exact removed assignment."

            Expect.equal
                deletionContext.Coverage.LinkIds
                assignment.CoveredLinkIds
                "The deletion record carries its exact removed links."

        testCase "an absent value is not a removal record"
        <| fun _ ->
            let nodeIds, initial = withNodes [ "A"; "B" ]

            let neverAssigned =
                initial
                |> addTestProcess "p" [
                    link "l" (ProcessLinkShape.Between(nodeIds[0], nodeIds[1]))
                ]

            let assigned =
                neverAssigned
                |> run (assignProcessValue (Set.singleton "l") (processDraft "Temperature" "20" None))

            let beforeRemoval = { assigned with MutationJournal = [] }
            let assignment = onlyProcessAssignment "p" beforeRemoval

            let removed =
                beforeRemoval
                |> run (removeProcessAssignmentLinks "p" assignment.Id assignment.CoveredLinkIds)

            Expect.equal removed.Nodes neverAssigned.Nodes "The final node state alone is identical."
            Expect.equal removed.Processes neverAssigned.Processes "The final process state alone is identical."
            Expect.equal removed.Properties neverAssigned.Properties "The final property state alone is identical."
            Expect.equal removed.Values neverAssigned.Values "The final value state alone is identical."

            Expect.isTrue
                (removed.MutationJournal
                 |> List.exists (
                     function
                     | ProcessAssignmentRemoved(tombstone, context) ->
                         tombstone.OwnerId = "p"
                         && tombstone.Assignment = assignment
                         && context.Coverage.LinkIds = assignment.CoveredLinkIds
                     | _ -> false
                 ))
                "Only the explicit tombstone distinguishes removal from never having been assigned."
    ]

/// A layer seeded from a selection. The behavior these assert is the canonical
/// difference from the old `Session.addLayer`, which copied each selected set
/// into the new layer, gave the copies fresh IDs, projected their property
/// values across, and joined them with a `ProvenanceReferenceLink`. Canonically
/// the same canonical node simply gains an appearance, so nothing is copied and
/// there is nothing to reconcile.
let private layerSeedingTests =
    /// `layer-1` with one annotated output node, ready to seed a second layer.
    /// The assignment's property and value definition are installed too, because
    /// `commit` projects the active layer and a dangling value reference is an
    /// inconsistent session rather than a valid starting point.
    let seeded () =
        let nodeId, session = ensureNode nodeKind "boundary" empty

        let withDefinitions = {
            session with
                Properties =
                    session.Properties
                    |> Map.add "property-boundary" {
                        Id = "property-boundary"
                        Category = category "Boundary"
                    }
                Values =
                    session.Values
                    |> Map.add "value-boundary" {
                        Id = "value-boundary"
                        PropertyId = "property-boundary"
                        Value = ProvenanceValue.Text "greenhouse"
                        Unit = None
                    }
        }

        let withLayer =
            withDefinitions
            |> withTestLayer "layer-1"
            |> addTestAppearance "layer-1" ProvenanceSide.Output nodeId 0
            |> addAssignment nodeId (existingAssignment "assignment-boundary" "value-boundary" Generic None)

        nodeId, withLayer

    testList "layer seeding" [
        testCase "a layer created after load still shows the host's catalog"
        <| fun _ ->
            // The catalog is host-controlled load-boundary data, recovered on
            // refresh from a layer's own cached shelf. A layer `addLayer` just
            // created has no cached shelf, so without the host's copy its
            // catalog folder would come back empty.
            let session, catalog = StoryFixtures.createReferenceCatalogSession ()

            let seedNodeId =
                session.Layers[session.ActiveLayerId].OutputEndpoints
                |> Map.toList
                |> List.head
                |> fst

            let withNewLayer =
                session
                |> run (CanonicalCommand.addLayer "Second" [ ProvenanceSide.Output, seedNodeId ])

            let catalogEntriesOf (candidate: ProvenanceSession) =
                candidate.LayerProjections[candidate.ActiveLayerId].ShelfEntries
                |> List.choose (fun entry ->
                    match entry.Payload with
                    | CatalogBacked payload -> Some payload.Entry.Reference.Id
                    | AssignmentBacked _ -> None
                )
                |> List.sort

            let withoutCatalog =
                refreshLayer withNewLayer.ActiveLayerId withNewLayer |> expectOk

            Expect.isEmpty
                (catalogEntriesOf withoutCatalog)
                "Without the host catalog the new layer has nothing to recover it from."

            let withCatalog =
                refreshLayerWithCatalog catalog withNewLayer.ActiveLayerId withNewLayer
                |> expectOk

            Expect.equal
                (catalogEntriesOf withCatalog)
                (catalog
                 |> Map.toList
                 |> List.map (fun (_, entry) -> entry.Reference.Id)
                 |> List.sort)
                "The host's controlled catalog reaches a layer that never had a cached shelf."

        testCase "an added layer gives the selected node an appearance instead of copying it"
        <| fun _ ->
            let nodeId, before = seeded ()

            let actual =
                before
                |> run (CanonicalCommand.addLayer "Second" [ ProvenanceSide.Output, nodeId ])

            Expect.equal (actual.Nodes |> Map.count) 1 "Seeding creates no second node for the same endpoint."

            Expect.equal
                (actual.Layers["layer-2"].InputEndpoints |> Map.toList |> List.map fst)
                [ nodeId ]
                "The seed appears on the new layer's input side as the same canonical node."

            Expect.equal
                (assignmentList nodeId actual |> List.map _.Id)
                [ "assignment-boundary" ]
                "Its annotation is not duplicated: one owner still holds exactly one assignment."

            Expect.equal
                actual.Layers["layer-1"].OutputEndpoints[nodeId].Header
                actual.Layers["layer-2"].InputEndpoints[nodeId].Header
                "The new appearance keeps the header of the appearance it was seeded from."

        testCase "an added layer becomes active with a name-namespaced source"
        <| fun _ ->
            let nodeId, before = seeded ()

            let actual =
                before
                |> run (CanonicalCommand.addLayer "Second" [ ProvenanceSide.Output, nodeId ])

            Expect.equal actual.ActiveLayerId "layer-2" "The new layer becomes active."
            Expect.equal actual.LayerOrder [ "layer-1"; "layer-2" ] "It is appended to the layer order."
            Expect.equal actual.Layers["layer-2"].Label "Second" "The entered name is the label."

            // Two layers added under one name must not collide: source colours and
            // process origin sources are keyed by Source.Id.
            Expect.equal
                actual.Layers["layer-2"].Source.Id
                "layer-2:Second"
                "The source id is namespaced with the layer id."

        testCase "an empty selection seeds from the active layer's outputs"
        <| fun _ ->
            let nodeId, before = seeded ()
            let actual = before |> run (CanonicalCommand.addLayer "Second" [])

            Expect.equal
                (actual.Layers["layer-2"].InputEndpoints |> Map.toList |> List.map fst)
                [ nodeId ]
                "The active layer's output appearances are the default seed."

        testCase "seeding advances topology once and journals one endpoint per appearance"
        <| fun _ ->
            let nodeId, before = seeded ()
            let topologyBefore = before.AvailabilityTopologyRevision
            let valueBefore = before.AnnotationValueRevision

            let actual =
                before
                |> run (CanonicalCommand.addLayer "Second" [ ProvenanceSide.Output, nodeId ])

            Expect.equal
                actual.AvailabilityTopologyRevision
                (topologyBefore + 1)
                "A new appearance changes reachability, so the atomic command advances topology exactly once."

            Expect.equal actual.AnnotationValueRevision valueBefore "No annotation content changed."

            let added =
                actual.MutationJournal
                |> List.choose (
                    function
                    | LayerEndpointAdded endpoint -> Some endpoint
                    | _ -> None
                )
                |> List.filter (fun endpoint -> endpoint.Key.LayerId = "layer-2")

            Expect.equal
                (added |> List.map (fun endpoint -> endpoint.Key.NodeId))
                [ nodeId ]
                "One appearance, one entry."

            // Intent §10: a change marks every projection stale, then the active
            // layer is refreshed immediately while the others stay stale. The new
            // layer is the active one, so it is current rather than stale.
            Expect.isFalse actual.LayerProjections["layer-2"].Stale "The newly active layer is refreshed immediately."

            Expect.isTrue
                (actual.LayerProjections
                 |> Map.forall (fun layerId projection -> layerId = actual.ActiveLayerId || projection.Stale))
                "Every layer other than the active one stays stale."

        testCase "a seed that is not an appearance of the active layer is rejected"
        <| fun _ ->
            let _, before = seeded ()
            let strayId, withStray = ensureNode nodeKind "stray" before

            // The stray node exists but appears in no layer, so a stale selection
            // must not be able to fabricate an appearance for it.
            let error =
                CanonicalCommand.addLayer "Second" [ ProvenanceSide.Output, strayId ] withStray
                |> function
                    | Error error -> error
                    | Ok _ -> failtest "Expected the unknown seed to be rejected"

            Expect.equal error (NodeNotFound strayId) "The absent appearance is named."

        testCase "a rejected seeding changes nothing"
        <| fun _ ->
            let _, before = seeded ()
            let strayId, withStray = ensureNode nodeKind "stray" before

            CanonicalCommand.addLayer "Second" [ ProvenanceSide.Output, strayId ] withStray
            |> function
                | Error _ -> ()
                | Ok _ -> failtest "Expected the unknown seed to be rejected"

            Expect.equal withStray.LayerOrder [ "layer-1" ] "No layer is added."
            Expect.equal withStray.ActiveLayerId "layer-1" "The active layer is unchanged."
            Expect.isEmpty withStray.MutationJournal "A failed command records nothing."

            Expect.equal
                withStray.AvailabilityTopologyRevision
                before.AvailabilityTopologyRevision
                "A failed command advances no revision."
    ]

let tests =
    testList "CanonicalCommands" [
        assignmentTests
        editTests
        promotionAndCopyTests
        revisionAndStalenessTests
        processAssignmentTests
        structuralEditingTests
        layerSeedingTests
        globalSidebarTests
        journalScopeAndCoverageTests
    ]

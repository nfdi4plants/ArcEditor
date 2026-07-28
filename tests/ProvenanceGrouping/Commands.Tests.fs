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

        testCase "bulk removal cleans container values after all selected dependents are removed"
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
                        dependentAssignment.Id, dependentAssignment.CoveredLinkIds
                    ]
                ]

            let actual = before |> run (removeProcessAssignmentsByOwner selections)

            Expect.isEmpty actual.Processes["p"].Assignments "Both selected assignments are removed."
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

let tests =
    testList "CanonicalCommands" [
        assignmentTests
        editTests
        promotionAndCopyTests
        revisionAndStalenessTests
        processAssignmentTests
    ]

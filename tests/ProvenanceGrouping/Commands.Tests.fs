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

let private draft name value unit = {
    Content = content name value unit
    OwnerKind = AnnotationOwnerKind.Node
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

let tests =
    testList "CanonicalCommands" [
        assignmentTests
        editTests
        promotionAndCopyTests
        revisionAndStalenessTests
    ]

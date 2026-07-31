module ProcessCoreWritebackTests

open Expecto
open ProcessCore
open ProcessCoreProvenanceFixtures
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreAdapterTypes
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreConverter
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreWriteback

module CanonicalCommands = Swate.Components.Page.ProvenanceGrouping.Commands
module CanonicalDomain = Swate.Components.Page.ProvenanceGrouping.Domain
module CanonicalGraph = Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreGraph
module CanonicalIdentifiers = Swate.Components.Page.ProvenanceGrouping.Identifiers
module CanonicalMutation = Swate.Components.Page.ProvenanceGrouping.MutationTypes
module CanonicalPlanner = Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreWritebackPlan
module CanonicalProjectionTypes = Swate.Components.Page.ProvenanceGrouping.ProjectionTypes
module CanonicalSession = Swate.Components.Page.ProvenanceGrouping.CanonicalSession
module CanonicalValues = Swate.Components.Page.ProvenanceGrouping.Values

let private annotationPayload (annotation: Annotation) =
    annotation.Name,
    annotation.Value,
    annotation.Unit,
    annotation.NameTAN,
    annotation.ValueTAN,
    annotation.UnitTAN,
    annotation.AdditionalType

let private canonicalLocation processGroupName : ProcessCoreProcessGroupLocation = {
    DatasetPath = [ "arc-neutral"; "dataset-neutral" ]
    ProcessGroupName = processGroupName
}

let private convertCanonical locations arc = fromArcMany locations arc |> expectOk

let private canonicalOwnerAndLink (session: CanonicalProjectionTypes.ProvenanceSession) =
    let ownerId, structuralProcess = session.Processes |> Map.toList |> List.head
    let linkId, processLink = structuralProcess.Links |> Map.toList |> List.head
    ownerId, structuralProcess, linkId, processLink

let private canonicalMutationContext assignmentIds linkIds : CanonicalMutation.MutationContext = {
    Scope = CanonicalMutation.MutationSemanticScope.GlobalDefinition
    Coverage = {
        AssignmentIds = assignmentIds
        LinkIds = linkIds
    }
}

let private addCanonicalProcessValue
    (assignmentId: string)
    (categoryName: string)
    (value: CanonicalValues.ProvenanceValue)
    (propertyKind: CanonicalValues.AssignmentPropertyKind)
    (coveredLinkIds: Set<string>)
    (ownerId: string)
    (session: CanonicalProjectionTypes.ProvenanceSession)
    =
    let propertyId = $"property:{assignmentId}"
    let valueId = $"value:{assignmentId}"

    let property: CanonicalValues.PropertyDefinition = {
        Id = propertyId
        Category = {
            Name = categoryName
            TermSource = None
            TermAccession = None
        }
    }

    let definition: CanonicalValues.PropertyValueDefinition = {
        Id = valueId
        PropertyId = propertyId
        Value = value
        Unit = None
    }

    let assignment: CanonicalDomain.ProcessAssignment = {
        Id = assignmentId
        ValueId = valueId
        PropertyKind = propertyKind
        CoveredLinkIds = coveredLinkIds
        ContainerReferenceValueId = None
        ReferenceSlotId =
            match propertyKind with
            | CanonicalValues.AssignmentPropertyKind.AdapterSpecific kind when
                kind.Id = ProcessCoreCanonicalKinds.processCoreRecipeKind.Id
                ->
                Some ProcessCoreCanonicalKinds.processCoreExecutesRecipeSlot
            | _ -> None
        Lineage = CanonicalValues.AssignmentLineage.Created
    }

    let structuralProcess = session.Processes[ownerId]
    let context = canonicalMutationContext (Set.singleton assignmentId) coveredLinkIds

    {
        session with
            Properties = session.Properties |> Map.add property.Id property
            Values = session.Values |> Map.add definition.Id definition
            Processes =
                session.Processes
                |> Map.add ownerId {
                    structuralProcess with
                        Assignments = structuralProcess.Assignments |> Map.add assignment.Id assignment
                }
            MutationJournal =
                session.MutationJournal
                @ [
                    CanonicalMutation.ProvenanceMutation.ProcessAssignmentAdded(ownerId, assignment, context)
                ]
    }

let private addParallelCanonicalLink
    (linkId: string)
    (ownerId: string)
    (session: CanonicalProjectionTypes.ProvenanceSession)
    =
    let structuralProcess = session.Processes[ownerId]
    let template = structuralProcess.Links |> Map.toList |> List.head |> snd
    let link: CanonicalDomain.ProcessLink = { Id = linkId; Shape = template.Shape }

    let after = {
        structuralProcess with
            Links = structuralProcess.Links |> Map.add link.Id link
    }

    {
        session with
            Processes = session.Processes |> Map.add ownerId after
            MutationJournal =
                session.MutationJournal
                @ [
                    CanonicalMutation.ProvenanceMutation.StructuralProcessReshaped(structuralProcess, after)
                    CanonicalMutation.ProvenanceMutation.ProcessLinkAdded(ownerId, link)
                ]
    }

let private addCanonicalNodeValue
    (assignmentId: string)
    (categoryName: string)
    (value: CanonicalValues.ProvenanceValue)
    (propertyKind: CanonicalValues.AssignmentPropertyKind)
    (targetSource: CanonicalIdentifiers.ProvenanceSourceRef option)
    (nodeId: string)
    (session: CanonicalProjectionTypes.ProvenanceSession)
    =
    let propertyId = $"property:{assignmentId}"
    let valueId = $"value:{assignmentId}"

    let property: CanonicalValues.PropertyDefinition = {
        Id = propertyId
        Category = {
            Name = categoryName
            TermSource = None
            TermAccession = None
        }
    }

    let definition: CanonicalValues.PropertyValueDefinition = {
        Id = valueId
        PropertyId = propertyId
        Value = value
        Unit = None
    }

    let assignment: CanonicalDomain.NodeAssignment = {
        Id = assignmentId
        ValueId = valueId
        PropertyKind = propertyKind
        TargetSource = targetSource
        Lineage = CanonicalValues.AssignmentLineage.Created
    }

    let node = session.Nodes[nodeId]
    let context = canonicalMutationContext (Set.singleton assignmentId) Set.empty

    {
        session with
            Properties = session.Properties |> Map.add property.Id property
            Values = session.Values |> Map.add definition.Id definition
            Nodes =
                session.Nodes
                |> Map.add nodeId {
                    node with
                        Assignments = node.Assignments |> Map.add assignment.Id assignment
                }
            MutationJournal =
                session.MutationJournal
                @ [
                    CanonicalMutation.ProvenanceMutation.NodeAssignmentAdded(nodeId, assignment, context)
                ]
    }

let private commitCanonical
    (effect: CanonicalCommands.CommandEffect)
    (session: CanonicalProjectionTypes.ProvenanceSession)
    =
    CanonicalSession.commit effect session

let private assignCanonicalRecipe
    (linkIds: Set<string>)
    (entry: CanonicalDomain.ReferenceCatalogEntry)
    (catalog: CanonicalDomain.ReferenceCatalog)
    (session: CanonicalProjectionTypes.ProvenanceSession)
    =
    CanonicalCommands.assignCatalogProcessValue linkIds catalog entry session
    |> expectOk
    |> fun effect -> commitCanonical effect session

let private recipeWithId id name version =
    let recipeComponent =
        Annotation("component", value = $"component:{id}", additionalType = "Component")

    let recipe =
        Recipe(name = name, version = version, components = [ recipeComponent ])

    recipe.SetProperty("@id", id)
    recipe

let private recipeFixture initiallyAssigned =
    let first = recipeWithId "recipe:first" "same-label" "1"
    let second = recipeWithId "recipe:second" "same-label" "2"
    let input = Sample("input-neutral")
    let output = Sample("output-neutral")

    let processObject =
        mkProcessFull "stage-neutral" (if initiallyAssigned then Some first else None) [ SampleNode input ] [
            SampleNode output
        ] []

    let dataset = Dataset("dataset-neutral", processes = [ processObject ])
    let arc = ARC("arc-neutral", hasPart = [ dataset ])
    arc.AddRecipe first
    arc.AddRecipe second
    arc, dataset, processObject, first, second

let private emptyRecipeFixture initiallyAssigned =
    let first = Recipe(name = "empty-recipe", version = "1")
    first.SetProperty("@id", "recipe:empty-first")
    let second = Recipe(name = "empty-recipe", version = "2")
    second.SetProperty("@id", "recipe:empty-second")
    let input = Sample("empty-input")
    let output = Sample("empty-output")

    let processObject =
        mkProcessFull "stage-neutral" (if initiallyAssigned then Some first else None) [ SampleNode input ] [
            SampleNode output
        ] []

    let dataset = Dataset("dataset-neutral", processes = [ processObject ])
    let arc = ARC("arc-neutral", hasPart = [ dataset ])
    arc.AddRecipe first
    arc.AddRecipe second
    arc, processObject, first, second

let private richAnnotationFixture () =
    let defaultValue =
        DefinedTerm("nested-default", tan = "term:nested-default", inDefinedTermSet = "https://example.org/defaults")

    defaultValue.SetProperty("default-overflow", "preserve-default")

    let instance =
        FormalParameter("nested-instance", nameTAN = "term:nested-instance", defaultValue = defaultValue)

    instance.SetProperty("instance-overflow", "preserve-instance")

    let annotation =
        Annotation(
            "rich-parameter",
            value = "before",
            valueTAN = "term:before",
            additionalType = "ParameterValue",
            instanceOf = instance
        )

    annotation.SetProperty("@id", "annotation:rich")
    annotation.SetProperty("annotation-overflow", "preserve-annotation")

    let input = Sample("input-neutral")
    let output = Sample("output-neutral")

    let proc =
        mkProcessFull "stage-neutral" None [ SampleNode input ] [ SampleNode output ] [ annotation ]

    let dataset = Dataset("dataset-neutral", processes = [ proc ])
    let arc = ARC("arc-neutral", hasPart = [ dataset ])
    arc, annotation

let private loadedNodeAnnotationFixture () =
    let annotation =
        Annotation("loaded-node-property", value = "before", additionalType = "CharacteristicValue")

    let input = Sample("input-neutral")
    input.AddAdditionalProperty annotation

    let processObject =
        mkProcessFull "stage-neutral" None [ SampleNode input ] [ SampleNode(Sample("output-neutral")) ] []

    let dataset = Dataset("dataset-neutral", processes = [ processObject ])
    ARC("arc-neutral", hasPart = [ dataset ])

let private twoStageFixture () =
    let stage name =
        mkProcessFull name None [ SampleNode(Sample($"{name}-input")) ] [ SampleNode(Sample($"{name}-output")) ] []

    let dataset =
        Dataset("dataset-neutral", processes = [ stage "stage-one"; stage "stage-two" ])

    ARC("arc-neutral", hasPart = [ dataset ])

let private recipeEntryFor (recipe: Recipe) (converted: ProcessCoreCanonicalConversionResult) =
    let key =
        Swate.Components.ProcessCore.Copy.RecipeResourceKey.ofRecipeStableString recipe

    converted.ReferenceCatalog[ProcessCoreCanonicalKinds.processCoreRecipeScheme, key]

let private canonicalEndpointPairs (plan: CanonicalPlanner.ProcessCoreWritebackPlan) =
    plan.Processes
    |> List.map (fun plannedProcess ->
        match plannedProcess.Shape with
        | CanonicalValues.ProcessLinkShape.Between(inputId, outputId) -> Some inputId, Some outputId
        | CanonicalValues.ProcessLinkShape.InputOnly inputId -> Some inputId, None
        | CanonicalValues.ProcessLinkShape.OutputOnly outputId -> None, Some outputId
        | CanonicalValues.ProcessLinkShape.Endpointless -> None, None
    )

let private canonicalPlanTests =
    testList "pure canonical ProcessCore writeback planning" [
        testCase "two distinct assignments with equal content on different links each materialize"
        <| fun _ ->
            let fixture = basic ()
            let converted = convertCanonical [ canonicalLocation "stage-neutral" ] fixture.Arc
            let ownerId, _, firstLinkId, _ = canonicalOwnerAndLink converted.Session

            let session =
                converted.Session
                |> addParallelCanonicalLink "parallel-link" ownerId
                |> addCanonicalProcessValue
                    "assignment:first"
                    "parameter"
                    (CanonicalValues.ProvenanceValue.Text "equal")
                    (CanonicalValues.AssignmentPropertyKind.AdapterSpecific ProcessCoreCanonicalKinds.parameter)
                    (Set.singleton firstLinkId)
                    ownerId
                |> addCanonicalProcessValue
                    "assignment:second"
                    "parameter"
                    (CanonicalValues.ProvenanceValue.Text "equal")
                    (CanonicalValues.AssignmentPropertyKind.AdapterSpecific ProcessCoreCanonicalKinds.parameter)
                    (Set.singleton "parallel-link")
                    ownerId

            let plan = CanonicalPlanner.tryCreate converted.Index session |> expectOk

            Expect.equal plan.Partitions.Length 2 "Assignment identity must split equal-content signatures."

            Expect.sequenceEqual
                (plan.Partitions
                 |> List.collect _.Assignments
                 |> List.map _.AssignmentId
                 |> List.sort)
                [ "assignment:first"; "assignment:second" ]
                "Both independent assignment identities must materialize."

        testCase "one assignment covering several links of one surviving process is written once"
        <| fun _ ->
            let fixture = basic ()
            let converted = convertCanonical [ canonicalLocation "stage-neutral" ] fixture.Arc
            let ownerId, _, firstLinkId, _ = canonicalOwnerAndLink converted.Session

            let session =
                converted.Session
                |> addParallelCanonicalLink "parallel-link" ownerId
                |> addCanonicalProcessValue
                    "assignment:shared"
                    "parameter"
                    (CanonicalValues.ProvenanceValue.Text "shared")
                    (CanonicalValues.AssignmentPropertyKind.AdapterSpecific ProcessCoreCanonicalKinds.parameter)
                    (Set.ofList [ firstLinkId; "parallel-link" ])
                    ownerId

            let plan = CanonicalPlanner.tryCreate converted.Index session |> expectOk
            let partition = plan.Partitions |> List.exactlyOne
            Expect.equal partition.Links.Count 2 "Both exact links must share the partition."
            Expect.equal partition.Assignments.Length 1 "The shared assignment is materialized once."
            Expect.equal plan.Processes.Length 2 "The singular API still receives one Process per exact link."

            let richArc, richSource = richAnnotationFixture ()
            let richConverted = convertCanonical [ canonicalLocation "stage-neutral" ] richArc

            let richOwnerId, richProcess, richLinkId, _ =
                canonicalOwnerAndLink richConverted.Session

            let richAssignment =
                richProcess.Assignments |> Map.toList |> List.map snd |> List.exactlyOne

            let withRichLink =
                addParallelCanonicalLink "rich-coverage-link" richOwnerId richConverted.Session

            let expandedRichAssignment = {
                richAssignment with
                    CoveredLinkIds = Set.ofList [ richLinkId; "rich-coverage-link" ]
            }

            let richSession = {
                withRichLink with
                    Processes =
                        withRichLink.Processes
                        |> Map.change
                            richOwnerId
                            (Option.map (fun current -> {
                                current with
                                    Assignments =
                                        current.Assignments |> Map.add expandedRichAssignment.Id expandedRichAssignment
                            }))
                    MutationJournal =
                        withRichLink.MutationJournal
                        @ [
                            CanonicalMutation.ProvenanceMutation.ProcessAssignmentCoverageChanged(
                                richOwnerId,
                                richAssignment,
                                expandedRichAssignment,
                                canonicalMutationContext
                                    (Set.singleton richAssignment.Id)
                                    (Set.singleton "rich-coverage-link")
                            )
                        ]
            }

            let richPlan =
                CanonicalPlanner.tryCreate richConverted.Index richSession |> expectOk

            let richPlanned =
                richPlan.Partitions
                |> List.collect _.Assignments
                |> List.choose _.Annotation
                |> List.distinctBy _.AssignmentId
                |> List.exactlyOne

            Expect.equal
                richPlanned.Fingerprint
                (CanonicalGraph.canonicalAnnotationFingerprint richSource)
                "A coverage-only change preserves the complete indexed annotation payload."

        testCase "separate original processes with equal final assignment sets stay separate"
        <| fun _ ->
            let arc, _, _ = positional ()
            let converted = convertCanonical [ canonicalLocation "stage-neutral" ] arc
            let plan = CanonicalPlanner.tryCreate converted.Index converted.Session |> expectOk

            Expect.equal plan.Processes.Length 2 "Both original Process identities must remain."

            Expect.equal
                (plan.Processes |> List.choose _.IndexedProcess |> Set.ofList |> Set.count)
                2
                "Equal empty assignment sets must not merge indexed Process identities."

            let removedOwnerId, removedStructuralProcess =
                converted.Session.Processes |> Map.toList |> List.head

            let removedLink =
                removedStructuralProcess.Links |> Map.toList |> List.map snd |> List.exactlyOne

            let emptiedStructuralProcess = {
                removedStructuralProcess with
                    Links = Map.empty
            }

            let withRemovedProcess = {
                converted.Session with
                    Processes = converted.Session.Processes |> Map.add removedOwnerId emptiedStructuralProcess
                    MutationJournal =
                        converted.Session.MutationJournal
                        @ [
                            CanonicalMutation.ProvenanceMutation.ProcessLinkRemoved(
                                removedOwnerId,
                                removedLink,
                                canonicalMutationContext Set.empty (Set.singleton removedLink.Id)
                            )
                        ]
            }

            let removalPlan =
                CanonicalPlanner.tryCreate converted.Index withRemovedProcess |> expectOk

            let processRemoval = removalPlan.ProcessRemovals |> List.exactlyOne

            Expect.equal
                processRemoval.StructuralProcessId
                removedOwnerId
                "The removal record retains the canonical owner."

            Expect.equal
                processRemoval.Location
                converted.Index.ProcessLocations[removedOwnerId]
                "The removal record carries the exact obsolete indexed Process location."

            Expect.equal
                removalPlan.Summary.RemovedProcesses
                removalPlan.ProcessRemovals.Length
                "The summary derives from actionable removal records."

            let wrongRemovedLink = {
                removedLink with
                    Id = "process-link:wrong-removal-witness"
            }

            let wrongLinkRemovalSession = {
                withRemovedProcess with
                    MutationJournal = [
                        CanonicalMutation.ProvenanceMutation.ProcessLinkRemoved(
                            removedOwnerId,
                            wrongRemovedLink,
                            canonicalMutationContext Set.empty (Set.singleton wrongRemovedLink.Id)
                        )
                    ]
            }

            let wrongLinkRemovalErrors =
                CanonicalPlanner.tryCreate converted.Index wrongLinkRemovalSession
                |> expectError

            Expect.isTrue
                (wrongLinkRemovalErrors
                 |> List.exists (
                     function
                     | ProcessCoreCanonicalWritebackError.InvalidPreparedState _ -> true
                     | _ -> false
                 ))
                "A same-owner removal record for the wrong link cannot justify deleting an indexed Process."

            let wrongBeforeProcess = {
                removedStructuralProcess with
                    Links = Map.ofList [ wrongRemovedLink.Id, wrongRemovedLink ]
            }

            let wrongReshapeRemovalSession = {
                withRemovedProcess with
                    MutationJournal = [
                        CanonicalMutation.ProvenanceMutation.StructuralProcessReshaped(
                            wrongBeforeProcess,
                            emptiedStructuralProcess
                        )
                    ]
            }

            let wrongReshapeRemovalErrors =
                CanonicalPlanner.tryCreate converted.Index wrongReshapeRemovalSession
                |> expectError

            Expect.isTrue
                (wrongReshapeRemovalErrors
                 |> List.exists (
                     function
                     | ProcessCoreCanonicalWritebackError.InvalidPreparedState _ -> true
                     | _ -> false
                 ))
                "A reshape whose before snapshot is not the exact indexed Process cannot justify removal."

        testCase "links with different signatures split into separate processes"
        <| fun _ ->
            let fixture = basic ()
            let converted = convertCanonical [ canonicalLocation "stage-neutral" ] fixture.Arc
            let ownerId, _, firstLinkId, _ = canonicalOwnerAndLink converted.Session

            let session =
                converted.Session
                |> addParallelCanonicalLink "different-signature-link" ownerId
                |> addCanonicalProcessValue
                    "assignment:only-first"
                    "parameter"
                    (CanonicalValues.ProvenanceValue.Text "one")
                    (CanonicalValues.AssignmentPropertyKind.AdapterSpecific ProcessCoreCanonicalKinds.parameter)
                    (Set.singleton firstLinkId)
                    ownerId

            let plan = CanonicalPlanner.tryCreate converted.Index session |> expectOk
            Expect.equal plan.Partitions.Length 2 "The empty and annotated signatures must be distinct."
            Expect.equal plan.Summary.AddedProcesses 1 "The second exact link requires one cloned Process."

            let richArc, richSource = richAnnotationFixture ()
            let richConverted = convertCanonical [ canonicalLocation "stage-neutral" ] richArc

            let richOwnerId, richProcess, richLinkId, _ =
                canonicalOwnerAndLink richConverted.Session

            let richAssignment =
                richProcess.Assignments |> Map.toList |> List.map snd |> List.exactlyOne

            let withRichLink =
                addParallelCanonicalLink "rich-split-link" richOwnerId richConverted.Session

            let expandedRichAssignment = {
                richAssignment with
                    CoveredLinkIds = Set.ofList [ richLinkId; "rich-split-link" ]
            }

            let expandedRich = {
                withRichLink with
                    Processes =
                        withRichLink.Processes
                        |> Map.change
                            richOwnerId
                            (Option.map (fun current -> {
                                current with
                                    Assignments =
                                        current.Assignments |> Map.add expandedRichAssignment.Id expandedRichAssignment
                            }))
                    MutationJournal =
                        withRichLink.MutationJournal
                        @ [
                            CanonicalMutation.ProvenanceMutation.ProcessAssignmentCoverageChanged(
                                richOwnerId,
                                richAssignment,
                                expandedRichAssignment,
                                canonicalMutationContext
                                    (Set.singleton richAssignment.Id)
                                    (Set.singleton "rich-split-link")
                            )
                        ]
            }

            let splitSession =
                CanonicalCommands.editProcessAssignmentSubset
                    richOwnerId
                    richAssignment.Id
                    (Set.singleton "rich-split-link")
                    {
                        Category = {
                            Name = "rich-parameter"
                            TermSource = None
                            TermAccession = None
                        }
                        Value = CanonicalValues.ProvenanceValue.Text "after"
                        Unit = None
                    }
                    expandedRich
                |> expectOk
                |> fun effect -> commitCanonical effect expandedRich

            let richSplitPlan =
                CanonicalPlanner.tryCreate richConverted.Index splitSession |> expectOk

            let splitAssignment =
                splitSession.Processes[richOwnerId].Assignments
                |> Map.toList
                |> List.map snd
                |> List.find (fun assignment ->
                    assignment.Lineage = CanonicalValues.AssignmentLineage.DerivedFrom richAssignment.Id
                )

            let splitPlanned =
                richSplitPlan.Partitions
                |> List.collect _.Assignments
                |> List.find (fun assignment -> assignment.AssignmentId = splitAssignment.Id)
                |> _.Annotation
                |> Option.get

            let expectedSplit =
                richSource
                |> CanonicalGraph.canonicalAnnotationFingerprint
                |> fun fingerprint -> ProcessCore.Yaml.Annotation.fromYamlString false fingerprint.Payload

            expectedSplit.Value <- Some "after"
            expectedSplit.ValueTAN <- None

            let splitRemint =
                richSplitPlan.AnnotationRemintings
                |> List.find (fun reminting -> reminting.AssignmentId = splitAssignment.Id)

            expectedSplit.SetProperty("@id", splitRemint.PlannedRegistryId)

            Expect.equal
                splitPlanned.Fingerprint
                (CanonicalGraph.canonicalAnnotationFingerprint expectedSplit)
                "A derived split preserves the complete ancestor payload except for its explicit collision remint."

        testCase "an empty assignment set is a valid partition"
        <| fun _ ->
            let fixture = basic ()
            let converted = convertCanonical [ canonicalLocation "stage-neutral" ] fixture.Arc
            let plan = CanonicalPlanner.tryCreate converted.Index converted.Session |> expectOk
            let partition = plan.Partitions |> List.exactlyOne
            Expect.isEmpty partition.Assignments "An annotation-free process has an empty signature."
            Expect.equal plan.Processes.Length 1 "The relationship itself must still be planned."

            let ownerId, structuralProcess, originalLinkId, templateLink =
                canonicalOwnerAndLink converted.Session

            let newProcessId = "structural-process:journalled"

            let newLink = {
                templateLink with
                    Id = "process-link:journalled"
            }

            let newProcess: CanonicalDomain.StructuralProcess = {
                structuralProcess with
                    Id = newProcessId
                    Links = Map.ofList [ newLink.Id, newLink ]
                    Assignments = Map.empty
            }

            let layer = converted.Session.Layers[structuralProcess.OriginLayerId]

            let unjournalled = {
                converted.Session with
                    Processes = converted.Session.Processes |> Map.add newProcess.Id newProcess
                    Layers =
                        converted.Session.Layers
                        |> Map.add layer.Id {
                            layer with
                                StructuralProcessIds = layer.StructuralProcessIds |> Set.add newProcess.Id
                        }
            }

            let unjournalledErrors =
                CanonicalPlanner.tryCreate converted.Index unjournalled |> expectError

            Expect.isTrue
                (unjournalledErrors
                 |> List.exists (
                     function
                     | ProcessCoreCanonicalWritebackError.InvalidPreparedState _ -> true
                     | _ -> false
                 ))
                "Final-state presence alone cannot invent an unindexed structural process."

            let journalled = {
                unjournalled with
                    MutationJournal =
                        unjournalled.MutationJournal
                        @ [
                            CanonicalMutation.ProvenanceMutation.StructuralProcessCreated newProcess
                            CanonicalMutation.ProvenanceMutation.ProcessLinkAdded(newProcess.Id, newLink)
                        ]
            }

            let journalledPlan =
                CanonicalPlanner.tryCreate converted.Index journalled |> expectOk

            Expect.isTrue
                (journalledPlan.Processes
                 |> List.exists (fun planned ->
                     planned.StructuralProcessId = newProcessId
                     && planned.Disposition = CanonicalPlanner.PlannedProcessDisposition.NewProcess
                 ))
                "A journalled structural-process creation remains plannable."

            let forgedIndexedLink = {
                newLink with
                    Id = "process-link:forged-on-indexed-process"
            }

            let forgedIndexedProcess = {
                converted.Session with
                    Processes =
                        converted.Session.Processes
                        |> Map.change
                            ownerId
                            (Option.map (fun current -> {
                                current with
                                    Links = current.Links |> Map.add forgedIndexedLink.Id forgedIndexedLink
                            }))
            }

            let forgedIndexedLinkErrors =
                CanonicalPlanner.tryCreate converted.Index forgedIndexedProcess |> expectError

            Expect.isTrue
                (forgedIndexedLinkErrors
                 |> List.exists (
                     function
                     | ProcessCoreCanonicalWritebackError.InvalidPreparedState _ -> true
                     | _ -> false
                 ))
                "An indexed structural process cannot acquire a final exact link without link-addition or reshape evidence."

            let forgedIndexedFinal = forgedIndexedProcess.Processes[ownerId]

            let wrongBeforeReshape = {
                structuralProcess with
                    Links = Map.empty
            }

            let wrongReshapeWitness = {
                forgedIndexedProcess with
                    MutationJournal = [
                        CanonicalMutation.ProvenanceMutation.StructuralProcessReshaped(
                            wrongBeforeReshape,
                            forgedIndexedFinal
                        )
                    ]
            }

            let wrongReshapeWitnessErrors =
                CanonicalPlanner.tryCreate converted.Index wrongReshapeWitness |> expectError

            Expect.isTrue
                (wrongReshapeWitnessErrors
                 |> List.exists (
                     function
                     | ProcessCoreCanonicalWritebackError.InvalidPreparedState _ -> true
                     | _ -> false
                 ))
                "A reshape after-snapshot cannot authorize a final link when its before-snapshot is unrelated."

            let indexedLinkLocation = converted.Index.LinkLocations[originalLinkId]

            let malformedLinkLocation =
                match indexedLinkLocation.Input, indexedLinkLocation.Output with
                | Some input, _ -> {
                    indexedLinkLocation with
                        Input =
                            Some {
                                input with
                                    Node = {
                                        input.Node with
                                            Key = "missing-indexed-input"
                                    }
                            }
                  }
                | None, Some output -> {
                    indexedLinkLocation with
                        Output =
                            Some {
                                output with
                                    Node = {
                                        output.Node with
                                            Key = "missing-indexed-output"
                                    }
                            }
                  }
                | None, None -> failtest "Expected the basic fixture to have at least one endpoint."

            let replacementLink = {
                templateLink with
                    Id = "process-link:replacement-for-malformed-index"
            }

            let replacementSession = {
                converted.Session with
                    Processes =
                        converted.Session.Processes
                        |> Map.change
                            ownerId
                            (Option.map (fun current -> {
                                current with
                                    Links = Map.ofList [ replacementLink.Id, replacementLink ]
                            }))
                    MutationJournal = [
                        CanonicalMutation.ProvenanceMutation.ProcessLinkAdded(ownerId, replacementLink)
                    ]
            }

            let malformedLinkIndex = {
                converted.Index with
                    LinkLocations = converted.Index.LinkLocations |> Map.add originalLinkId malformedLinkLocation
            }

            let malformedLinkErrors =
                CanonicalPlanner.tryCreate malformedLinkIndex replacementSession |> expectError

            Expect.isTrue
                (malformedLinkErrors
                 |> List.exists (
                     function
                     | ProcessCoreCanonicalWritebackError.InvalidPreparedState _ -> true
                     | _ -> false
                 ))
                "Every indexed link must reconstruct before source-to-journal replay can authorize a replacement."

        testCase "wrong-owner assignment journal mention cannot authorize loaded value divergence"
        <| fun _ ->
            let arc, _ = richAnnotationFixture ()
            let converted = convertCanonical [ canonicalLocation "stage-neutral" ] arc
            let ownerId, structuralProcess, linkId, _ = canonicalOwnerAndLink converted.Session

            let assignment =
                structuralProcess.Assignments |> Map.toList |> List.map snd |> List.exactlyOne

            let beforeValue = converted.Session.Values[assignment.ValueId]

            let afterValue = {
                beforeValue with
                    Value = CanonicalValues.ProvenanceValue.Text "forged-divergence"
            }

            let context: CanonicalMutation.MutationContext = {
                Scope =
                    CanonicalMutation.MutationSemanticScope.OwnerScoped(
                        Set.singleton (
                            CanonicalMutation.AssignmentOwnerRef.ProcessAssignmentOwner "structural-process:wrong-owner"
                        )
                    )
                Coverage = {
                    AssignmentIds = Set.singleton assignment.Id
                    LinkIds = Set.singleton linkId
                }
            }

            let forged = {
                converted.Session with
                    Values = converted.Session.Values |> Map.add afterValue.Id afterValue
                    MutationJournal = [
                        CanonicalMutation.ProvenanceMutation.ProcessAssignmentValueChanged(
                            "structural-process:wrong-owner",
                            assignment,
                            assignment,
                            context
                        )
                    ]
            }

            let errors = CanonicalPlanner.tryCreate converted.Index forged |> expectError

            Expect.isTrue
                (errors
                 |> List.exists (
                     function
                     | ProcessCoreCanonicalWritebackError.InvalidPreparedState _ -> true
                     | _ -> false
                 ))
                $"A wrong-owner transition mentioning '{ownerId}'s assignment cannot control its loaded value."

        testCase "unrelated value before-record cannot authorize loaded value divergence"
        <| fun _ ->
            let arc, _ = richAnnotationFixture ()
            let converted = convertCanonical [ canonicalLocation "stage-neutral" ] arc
            let _, structuralProcess, linkId, _ = canonicalOwnerAndLink converted.Session

            let assignment =
                structuralProcess.Assignments |> Map.toList |> List.map snd |> List.exactlyOne

            let indexedValue = converted.Session.Values[assignment.ValueId]

            let unrelatedBefore = {
                indexedValue with
                    Value = CanonicalValues.ProvenanceValue.Text "not-the-indexed-before"
            }

            let afterValue = {
                indexedValue with
                    Value = CanonicalValues.ProvenanceValue.Text "forged-divergence"
            }

            let forged = {
                converted.Session with
                    Values = converted.Session.Values |> Map.add afterValue.Id afterValue
                    MutationJournal = [
                        CanonicalMutation.ProvenanceMutation.PropertyValueDefinitionUpdated(
                            unrelatedBefore,
                            afterValue,
                            canonicalMutationContext (Set.singleton assignment.Id) (Set.singleton linkId)
                        )
                    ]
            }

            let errors = CanonicalPlanner.tryCreate converted.Index forged |> expectError

            Expect.isTrue
                (errors
                 |> List.exists (
                     function
                     | ProcessCoreCanonicalWritebackError.InvalidPreparedState _ -> true
                     | _ -> false
                 ))
                "A value transition must start from the exact indexed definition, not merely mention its ID."

        testCase "loaded node assignment cannot move to another owner without an exact transition"
        <| fun _ ->
            let converted =
                loadedNodeAnnotationFixture ()
                |> convertCanonical [ canonicalLocation "stage-neutral" ]

            let sourceNodeId, sourceNode, assignment =
                converted.Session.Nodes
                |> Map.toList
                |> List.pick (fun (nodeId, node) ->
                    node.Assignments
                    |> Map.toList
                    |> List.tryHead
                    |> Option.map (fun (_, assignment) -> nodeId, node, assignment)
                )

            let targetNodeId, targetNode =
                converted.Session.Nodes
                |> Map.toList
                |> List.find (fun (nodeId, _) -> nodeId <> sourceNodeId)

            let moved = {
                converted.Session with
                    Nodes =
                        converted.Session.Nodes
                        |> Map.add sourceNodeId {
                            sourceNode with
                                Assignments = sourceNode.Assignments |> Map.remove assignment.Id
                        }
                        |> Map.add targetNodeId {
                            targetNode with
                                Assignments = targetNode.Assignments |> Map.add assignment.Id assignment
                        }
            }

            let errors = CanonicalPlanner.tryCreate converted.Index moved |> expectError

            Expect.isTrue
                (errors
                 |> List.exists (
                     function
                     | ProcessCoreCanonicalWritebackError.InvalidPreparedState _ -> true
                     | _ -> false
                 ))
                "Indexed node assignment ownership is immutable without an exact owner transition."

        testCase "exact node process and global ordinary edits remain controlled"
        <| fun _ ->
            let nodeConverted =
                loadedNodeAnnotationFixture ()
                |> convertCanonical [ canonicalLocation "stage-neutral" ]

            let nodeOwnerId, nodeAssignment =
                nodeConverted.Session.Nodes
                |> Map.toList
                |> List.pick (fun (nodeId, node) ->
                    node.Assignments
                    |> Map.toList
                    |> List.tryHead
                    |> Option.map (fun (_, assignment) -> nodeId, assignment)
                )

            let nodeEdited =
                CanonicalCommands.editNodeAssignment
                    nodeOwnerId
                    nodeAssignment.Id
                    {
                        Category = {
                            Name = "loaded-node-property"
                            TermSource = None
                            TermAccession = None
                        }
                        Value = CanonicalValues.ProvenanceValue.Text "node-after"
                        Unit = None
                    }
                    nodeConverted.Session
                |> expectOk
                |> fun effect -> commitCanonical effect nodeConverted.Session

            CanonicalPlanner.tryCreate nodeConverted.Index nodeEdited |> expectOk |> ignore

            let processArc, _ = richAnnotationFixture ()

            let processConverted =
                convertCanonical [ canonicalLocation "stage-neutral" ] processArc

            let processOwnerId, structuralProcess, _, _ =
                canonicalOwnerAndLink processConverted.Session

            let processAssignment =
                structuralProcess.Assignments |> Map.toList |> List.map snd |> List.exactlyOne

            let processContent: CanonicalCommands.NodeValueContent = {
                Category = {
                    Name = "rich-parameter"
                    TermSource = None
                    TermAccession = None
                }
                Value = CanonicalValues.ProvenanceValue.Text "process-after"
                Unit = None
            }

            let processEdited =
                CanonicalCommands.editProcessAssignment
                    processOwnerId
                    processAssignment.Id
                    processContent
                    processConverted.Session
                |> expectOk
                |> fun effect -> commitCanonical effect processConverted.Session

            CanonicalPlanner.tryCreate processConverted.Index processEdited
            |> expectOk
            |> ignore

            let globallyEdited =
                CanonicalCommands.editValueGlobally
                    processAssignment.ValueId
                    {
                        processContent with
                            Value = CanonicalValues.ProvenanceValue.Text "global-after"
                    }
                    processConverted.Session
                |> expectOk
                |> fun effect -> commitCanonical effect processConverted.Session

            CanonicalPlanner.tryCreate processConverted.Index globallyEdited
            |> expectOk
            |> ignore

        testCase "a loaded structural process cannot be renamed without a journal entry"
        <| fun _ ->
            let arc, _ = richAnnotationFixture ()
            let converted = convertCanonical [ canonicalLocation "stage-neutral" ] arc
            let ownerId, structuralProcess, _, _ = canonicalOwnerAndLink converted.Session

            let renamed = {
                converted.Session with
                    Processes =
                        converted.Session.Processes
                        |> Map.add ownerId {
                            structuralProcess with
                                Name = Some "renamed-without-evidence"
                        }
            }

            let errors = CanonicalPlanner.tryCreate converted.Index renamed |> expectError

            Expect.isTrue
                (errors
                 |> List.exists (
                     function
                     | ProcessCoreCanonicalWritebackError.InvalidPreparedState _ -> true
                     | _ -> false
                 ))
                "A loaded process name must be validated against its indexed snapshot, not against final state."

        testCase "a loaded structural process cannot change origin layer without evidence"
        <| fun _ ->
            let converted =
                twoStageFixture ()
                |> convertCanonical [
                    canonicalLocation "stage-one"
                    canonicalLocation "stage-two"
                ]

            let (firstId, firstProcess), (_, secondProcess) =
                match converted.Session.Processes |> Map.toList with
                | first :: second :: _ -> first, second
                | _ -> failwith "The two-stage fixture must produce two structural processes."

            Expect.notEqual
                firstProcess.OriginLayerId
                secondProcess.OriginLayerId
                "The two-stage fixture must produce two distinct origin layers."

            let moved = {
                converted.Session with
                    Processes =
                        converted.Session.Processes
                        |> Map.add firstId {
                            firstProcess with
                                OriginLayerId = secondProcess.OriginLayerId
                        }
            }

            let errors = CanonicalPlanner.tryCreate converted.Index moved |> expectError

            Expect.isTrue
                (errors
                 |> List.exists (
                     function
                     | ProcessCoreCanonicalWritebackError.InvalidPreparedState _ -> true
                     | _ -> false
                 ))
                "A loaded process origin layer must be validated against its indexed snapshot."

        testCase "a malformed indexed recipe resource fails closed instead of throwing"
        <| fun _ ->
            let arc, _, _, _, _ = recipeFixture true
            let converted = convertCanonical [ canonicalLocation "stage-neutral" ] arc

            let malformedIndex = {
                converted.Index with
                    RecipeResources =
                        converted.Index.RecipeResources
                        |> Map.map (fun _ resource -> {
                            resource with
                                Resource = Unchecked.defaultof<Recipe>
                        })
            }

            let errors =
                CanonicalPlanner.tryCreate malformedIndex converted.Session |> expectError

            Expect.isNonEmpty errors "A malformed indexed Recipe resource must return an error instead of throwing."

        testCase "assigning an existing recipe plans the exact indexed resource"
        <| fun _ ->
            let arc, _, _, first, second = recipeFixture false
            let converted = convertCanonical [ canonicalLocation "stage-neutral" ] arc
            let _, _, linkId, _ = canonicalOwnerAndLink converted.Session
            let entry = recipeEntryFor second converted

            let session =
                assignCanonicalRecipe (Set.singleton linkId) entry converted.ReferenceCatalog converted.Session

            let plan = CanonicalPlanner.tryCreate converted.Index session |> expectOk
            let association = plan.RecipeAssociations |> List.exactlyOne

            Expect.isTrue
                (obj.ReferenceEquals(association.FinalResource.Value.Resource, second))
                "Planning must retain the exact indexed Recipe object."

            Expect.equal association.Change CanonicalPlanner.RecipeAssociationChange.Set "A new association is set."
            Expect.equal plan.RecipeResourcesAdded 0 "The planner has no Recipe-resource creation path."

            let mismatchedWitness =
                assignCanonicalRecipe
                    (Set.singleton linkId)
                    (recipeEntryFor first converted)
                    converted.ReferenceCatalog
                    converted.Session

            let forgedJournal = {
                session with
                    MutationJournal = mismatchedWitness.MutationJournal
            }

            let forgedJournalErrors =
                CanonicalPlanner.tryCreate converted.Index forgedJournal |> expectError

            Expect.isTrue
                (forgedJournalErrors
                 |> List.exists (
                     function
                     | ProcessCoreCanonicalWritebackError.InvalidPreparedState _
                     | ProcessCoreCanonicalWritebackError.ReadOnlyRecipeComponentMutation _ -> true
                     | _ -> false
                 ))
                "Journal evidence for one Recipe resource cannot authorize a different final Recipe resource."

            let emptyFirst = Recipe(name = "empty-recipe", version = "1")
            emptyFirst.SetProperty("@id", "recipe:empty-first")
            let emptySecond = Recipe(name = "empty-recipe", version = "2")
            emptySecond.SetProperty("@id", "recipe:empty-second")

            let emptyProcess =
                mkProcessFull "stage-neutral" None [ SampleNode(Sample("empty-input")) ] [
                    SampleNode(Sample("empty-output"))
                ] []

            let emptyDataset = Dataset("dataset-neutral", processes = [ emptyProcess ])
            let emptyArc = ARC("arc-neutral", hasPart = [ emptyDataset ])
            emptyArc.AddRecipe emptyFirst
            emptyArc.AddRecipe emptySecond

            let emptyConverted = convertCanonical [ canonicalLocation "stage-neutral" ] emptyArc
            let emptyOwnerId, _, emptyLinkId, _ = canonicalOwnerAndLink emptyConverted.Session

            let journalledEmptyFirst =
                assignCanonicalRecipe
                    (Set.singleton emptyLinkId)
                    (recipeEntryFor emptyFirst emptyConverted)
                    emptyConverted.ReferenceCatalog
                    emptyConverted.Session

            let emptyRecipeAssignment =
                journalledEmptyFirst.Processes[emptyOwnerId].Assignments
                |> Map.toList
                |> List.map snd
                |> List.find (fun assignment -> assignment.ReferenceSlotId.IsSome)

            let emptySecondEntry = recipeEntryFor emptySecond emptyConverted

            let forgedReferenceDefinition = {
                journalledEmptyFirst.Values[emptyRecipeAssignment.ValueId] with
                    Value = CanonicalValues.ProvenanceValue.Reference emptySecondEntry.Reference
            }

            let forgedSameValueId = {
                journalledEmptyFirst with
                    Values =
                        journalledEmptyFirst.Values
                        |> Map.add forgedReferenceDefinition.Id forgedReferenceDefinition
            }

            let forgedSameValueIdErrors =
                CanonicalPlanner.tryCreate emptyConverted.Index forgedSameValueId |> expectError

            Expect.isTrue
                (forgedSameValueIdErrors
                 |> List.exists (
                     function
                     | ProcessCoreCanonicalWritebackError.InvalidPreparedState _ -> true
                     | _ -> false
                 ))
                "A journalled zero-Component Recipe cannot change resource identity under the same ValueId."

            let loadedEmptyProcess =
                mkProcessFull "stage-neutral" (Some emptyFirst) [ SampleNode(Sample("loaded-empty-input")) ] [
                    SampleNode(Sample("loaded-empty-output"))
                ] []

            let loadedEmptyDataset =
                Dataset("dataset-neutral", processes = [ loadedEmptyProcess ])

            let loadedEmptyArc = ARC("arc-neutral", hasPart = [ loadedEmptyDataset ])
            loadedEmptyArc.AddRecipe emptyFirst

            let loadedEmptyConverted =
                convertCanonical [ canonicalLocation "stage-neutral" ] loadedEmptyArc

            let loadedEmptyOwnerId, loadedEmptyProcessState, _, _ =
                canonicalOwnerAndLink loadedEmptyConverted.Session

            let exactLoadedRecipe =
                loadedEmptyProcessState.Assignments
                |> Map.toList
                |> List.map snd
                |> List.find (fun assignment -> assignment.ReferenceSlotId.IsSome)

            let forgedLoadedValue = {
                loadedEmptyConverted.Session.Values[exactLoadedRecipe.ValueId] with
                    Id = "value:forged-loaded-recipe"
            }

            let forgedLoadedRecipe = {
                exactLoadedRecipe with
                    Id = "assignment:forged-loaded-recipe"
                    ValueId = forgedLoadedValue.Id
            }

            let forgedLoadedSession = {
                loadedEmptyConverted.Session with
                    Values =
                        loadedEmptyConverted.Session.Values
                        |> Map.add forgedLoadedValue.Id forgedLoadedValue
                    Processes =
                        loadedEmptyConverted.Session.Processes
                        |> Map.change
                            loadedEmptyOwnerId
                            (Option.map (fun current -> {
                                current with
                                    Assignments =
                                        current.Assignments
                                        |> Map.remove exactLoadedRecipe.Id
                                        |> Map.add forgedLoadedRecipe.Id forgedLoadedRecipe
                            }))
            }

            let forgedLoadedErrors =
                CanonicalPlanner.tryCreate loadedEmptyConverted.Index forgedLoadedSession
                |> expectError

            Expect.isTrue
                (forgedLoadedErrors
                 |> List.exists (
                     function
                     | ProcessCoreCanonicalWritebackError.InvalidPreparedState _ -> true
                     | _ -> false
                 ))
                "A forged orphan Loaded Recipe assignment cannot witness indexed resource identity."

        testCase "replacing or detaching a recipe plans only an ExecutesRecipe change"
        <| fun _ ->
            let arc, _, _, first, second = recipeFixture true
            let converted = convertCanonical [ canonicalLocation "stage-neutral" ] arc
            let ownerId, structuralProcess, linkId, _ = canonicalOwnerAndLink converted.Session
            let firstPayload = ProcessCore.Yaml.Recipe.toYamlString None first
            let secondPayload = ProcessCore.Yaml.Recipe.toYamlString None second
            let replacementEntry = recipeEntryFor second converted

            let replaced =
                assignCanonicalRecipe
                    (Set.singleton linkId)
                    replacementEntry
                    converted.ReferenceCatalog
                    converted.Session

            let replacedPlan = CanonicalPlanner.tryCreate converted.Index replaced |> expectOk
            let replacement = replacedPlan.RecipeAssociations |> List.exactlyOne

            Expect.equal
                replacement.Change
                CanonicalPlanner.RecipeAssociationChange.Replace
                "The reference is replaced."

            let recipeAssignment =
                structuralProcess.Assignments
                |> Map.toList
                |> List.map snd
                |> List.find (fun (assignment: CanonicalDomain.ProcessAssignment) -> assignment.ReferenceSlotId.IsSome)

            let detached =
                CanonicalCommands.removeProcessAssignmentLinks
                    ownerId
                    recipeAssignment.Id
                    (Set.singleton linkId)
                    converted.Session
                |> expectOk
                |> fun effect -> commitCanonical effect converted.Session

            let detachedPlan = CanonicalPlanner.tryCreate converted.Index detached |> expectOk
            let detachment = detachedPlan.RecipeAssociations |> List.exactlyOne

            Expect.equal
                detachment.Change
                CanonicalPlanner.RecipeAssociationChange.Clear
                "Only the association is cleared."

            Expect.equal
                (ProcessCore.Yaml.Recipe.toYamlString None first)
                firstPayload
                "The original stored Recipe payload remains unchanged."

            Expect.equal
                (ProcessCore.Yaml.Recipe.toYamlString None second)
                secondPayload
                "The replacement stored Recipe payload remains unchanged."

            let emptyArc, _, emptyFirst, emptySecond = emptyRecipeFixture true
            let emptyConverted = convertCanonical [ canonicalLocation "stage-neutral" ] emptyArc

            let emptyOwnerId, emptyProcess, emptyLinkId, _ =
                canonicalOwnerAndLink emptyConverted.Session

            let emptyReplaced =
                assignCanonicalRecipe
                    (Set.singleton emptyLinkId)
                    (recipeEntryFor emptySecond emptyConverted)
                    emptyConverted.ReferenceCatalog
                    emptyConverted.Session

            let forgedReplacePrevious = {
                emptyReplaced with
                    MutationJournal =
                        emptyReplaced.MutationJournal
                        |> List.map (
                            function
                            | CanonicalMutation.ProvenanceMutation.AdapterResourceReferenceReplaced(mutationOwnerId,
                                                                                                    before,
                                                                                                    after,
                                                                                                    removed,
                                                                                                    added,
                                                                                                    context) when
                                mutationOwnerId = emptyOwnerId
                                ->
                                CanonicalMutation.ProvenanceMutation.AdapterResourceReferenceReplaced(
                                    mutationOwnerId,
                                    {
                                        before with
                                            ValueId = "processcore-value-999999"
                                    },
                                    after,
                                    removed,
                                    added,
                                    context
                                )
                            | mutation -> mutation
                        )
            }

            let forgedReplacePreviousErrors =
                CanonicalPlanner.tryCreate emptyConverted.Index forgedReplacePrevious
                |> expectError

            Expect.isTrue
                (forgedReplacePreviousErrors
                 |> List.exists (
                     function
                     | ProcessCoreCanonicalWritebackError.InvalidPreparedState _ -> true
                     | _ -> false
                 ))
                "Replace must bind its before record to the exact previously indexed Recipe identity."

            let emptyRecipeAssignment =
                emptyProcess.Assignments
                |> Map.toList
                |> List.map snd
                |> List.find (fun assignment -> assignment.ReferenceSlotId.IsSome)

            let emptyDetached =
                CanonicalCommands.removeProcessAssignmentLinks
                    emptyOwnerId
                    emptyRecipeAssignment.Id
                    (Set.singleton emptyLinkId)
                    emptyConverted.Session
                |> expectOk
                |> fun effect -> commitCanonical effect emptyConverted.Session

            let forgedClearPrevious = {
                emptyDetached with
                    MutationJournal =
                        emptyDetached.MutationJournal
                        |> List.map (
                            function
                            | CanonicalMutation.ProvenanceMutation.ProcessAssignmentRemoved(tombstone, context) when
                                tombstone.Assignment.ReferenceSlotId.IsSome
                                ->
                                CanonicalMutation.ProvenanceMutation.ProcessAssignmentRemoved(
                                    {
                                        tombstone with
                                            Assignment = {
                                                tombstone.Assignment with
                                                    ValueId = "processcore-value-999999"
                                            }
                                    },
                                    context
                                )
                            | mutation -> mutation
                        )
            }

            let forgedClearPreviousErrors =
                CanonicalPlanner.tryCreate emptyConverted.Index forgedClearPrevious
                |> expectError

            Expect.isTrue
                (forgedClearPreviousErrors
                 |> List.exists (
                     function
                     | ProcessCoreCanonicalWritebackError.InvalidPreparedState _ -> true
                     | _ -> false
                 ))
                "Clear must bind its tombstone to the exact previously indexed Recipe value identity."

        testCase "a split or new process reuses its assigned stored recipe"
        <| fun _ ->
            let arc, _, _, first, _ = recipeFixture true
            let converted = convertCanonical [ canonicalLocation "stage-neutral" ] arc

            let ownerId, structuralProcess, firstLinkId, _ =
                canonicalOwnerAndLink converted.Session

            let withLink =
                addParallelCanonicalLink "recipe-split-link" ownerId converted.Session

            let expandedAssignments =
                structuralProcess.Assignments
                |> Map.map (fun _ (assignment: CanonicalDomain.ProcessAssignment) -> {
                    assignment with
                        CoveredLinkIds = Set.ofList [ firstLinkId; "recipe-split-link" ]
                })

            let coverageContext =
                canonicalMutationContext
                    (expandedAssignments |> Map.keys |> Set.ofSeq)
                    (Set.singleton "recipe-split-link")

            let coverageJournal =
                structuralProcess.Assignments
                |> Map.toList
                |> List.map (fun (assignmentId, before) ->
                    CanonicalMutation.ProvenanceMutation.ProcessAssignmentCoverageChanged(
                        ownerId,
                        before,
                        expandedAssignments[assignmentId],
                        coverageContext
                    )
                )

            let session = {
                withLink with
                    Processes =
                        withLink.Processes
                        |> Map.change
                            ownerId
                            (Option.map (fun current -> {
                                current with
                                    Assignments = expandedAssignments
                            }))
                    MutationJournal = withLink.MutationJournal @ coverageJournal
            }

            let recipeCount = arc.Recipes.Count
            let plan = CanonicalPlanner.tryCreate converted.Index session |> expectOk

            Expect.equal plan.Processes.Length 2 "Both exact links are emitted."

            for association in plan.RecipeAssociations do
                Expect.isTrue
                    (obj.ReferenceEquals(association.FinalResource.Value.Resource, first))
                    "Every split Process reuses the stored Recipe object."

            Expect.equal arc.Recipes.Count recipeCount "Pure planning cannot grow the Recipe catalog."
            Expect.equal plan.RecipeResourcesAdded 0 "No Recipe copy is planned."

        testCase "an unknown or ambiguous recipe resource key invalidates the plan"
        <| fun _ ->
            let arc, _, _, _, second = recipeFixture false
            let converted = convertCanonical [ canonicalLocation "stage-neutral" ] arc
            let ownerId, _, linkId, _ = canonicalOwnerAndLink converted.Session

            let unknown =
                converted.Session
                |> addCanonicalProcessValue
                    "assignment:unknown-recipe"
                    "Recipe"
                    (CanonicalValues.ProvenanceValue.Reference {
                        Scheme = ProcessCoreCanonicalKinds.processCoreRecipeScheme
                        Id = "I14:recipe:missing"
                        Label = "same-label"
                    })
                    (CanonicalValues.AssignmentPropertyKind.AdapterSpecific
                        ProcessCoreCanonicalKinds.processCoreRecipeKind)
                    (Set.singleton linkId)
                    ownerId

            let unknownErrors =
                CanonicalPlanner.tryCreate converted.Index unknown |> expectError

            Expect.isTrue
                (unknownErrors
                 |> List.exists (
                     function
                     | ProcessCoreCanonicalWritebackError.RecipeResourceNotFound _ -> true
                     | _ -> false
                 ))
                "An unknown exact resource key rejects the whole plan."

            let entry = recipeEntryFor second converted

            let assigned =
                assignCanonicalRecipe (Set.singleton linkId) entry converted.ReferenceCatalog converted.Session

            let resource =
                converted.Index.RecipeResources[entry.Reference.Scheme, entry.Reference.Id]

            let ambiguousIndex = {
                converted.Index with
                    RecipeResources =
                        converted.Index.RecipeResources
                        |> Map.add ("forged-scheme-key", "forged-resource-key") resource
            }

            let ambiguousErrors =
                CanonicalPlanner.tryCreate ambiguousIndex assigned |> expectError

            Expect.isTrue
                (ambiguousErrors
                 |> List.exists (
                     function
                     | ProcessCoreCanonicalWritebackError.AmbiguousRecipeResourceKey _ -> true
                     | _ -> false
                 ))
                "A duplicated exact key in malformed index data rejects the whole plan."

            let malformedResource = {
                resource with
                    Components =
                        resource.Components
                        |> List.map (fun location -> { location with Position = 10_000 })
            }

            let malformedComponentIndex = {
                converted.Index with
                    RecipeResources =
                        converted.Index.RecipeResources
                        |> Map.add (entry.Reference.Scheme, entry.Reference.Id) malformedResource
            }

            let malformedComponentErrors =
                CanonicalPlanner.tryCreate malformedComponentIndex assigned |> expectError

            Expect.isTrue
                (malformedComponentErrors
                 |> List.exists (
                     function
                     | ProcessCoreCanonicalWritebackError.StaleRecipeResource _
                     | ProcessCoreCanonicalWritebackError.ReadOnlyRecipeComponentMutation _ -> true
                     | _ -> false
                 ))
                "Malformed indexed Component positions return typed plan errors instead of throwing."

            let malformedPayloadResource = {
                resource with
                    Components =
                        resource.Components
                        |> List.map (fun location -> {
                            location with
                                Fingerprint = { Payload = null }
                        })
            }

            let malformedPayloadIndex = {
                converted.Index with
                    RecipeResources =
                        converted.Index.RecipeResources
                        |> Map.add (entry.Reference.Scheme, entry.Reference.Id) malformedPayloadResource
            }

            let malformedPayloadErrors =
                CanonicalPlanner.tryCreate malformedPayloadIndex assigned |> expectError

            Expect.isTrue
                (malformedPayloadErrors
                 |> List.exists (
                     function
                     | ProcessCoreCanonicalWritebackError.StaleRecipeResource _ -> true
                     | _ -> false
                 ))
                "Malformed indexed Component payloads return typed plan errors instead of throwing."

            let nullKeyResource = {
                resource with
                    ResourceKey = Swate.Components.ProcessCore.Copy.RecipeResourceKey.ById null
            }

            let nullKeyIndex = {
                converted.Index with
                    RecipeResources =
                        converted.Index.RecipeResources
                        |> Map.add (entry.Reference.Scheme, entry.Reference.Id) nullKeyResource
            }

            let nullKeyErrors = CanonicalPlanner.tryCreate nullKeyIndex assigned |> expectError

            Expect.isTrue
                (nullKeyErrors
                 |> List.exists (
                     function
                     | ProcessCoreCanonicalWritebackError.InvalidPreparedState _
                     | ProcessCoreCanonicalWritebackError.StaleRecipeResource _ -> true
                     | _ -> false
                 ))
                "A null Recipe resource key returns typed planning errors instead of escaping as an exception."

            let firstEntry =
                arc.Recipes
                |> Seq.find (fun recipe -> not (obj.ReferenceEquals(recipe, second)))
                |> fun recipe -> recipeEntryFor recipe converted

            let multiplyAssigned =
                assigned
                |> addCanonicalProcessValue
                    "assignment:second-recipe"
                    "Recipe"
                    (CanonicalValues.ProvenanceValue.Reference firstEntry.Reference)
                    (CanonicalValues.AssignmentPropertyKind.AdapterSpecific
                        ProcessCoreCanonicalKinds.processCoreRecipeKind)
                    (Set.singleton linkId)
                    ownerId

            let multipleErrors =
                CanonicalPlanner.tryCreate converted.Index multiplyAssigned |> expectError

            Expect.contains
                multipleErrors
                (ProcessCoreCanonicalWritebackError.InvalidProcessLink linkId)
                "Several Recipe assignments on one exact link reject through the Result boundary."

        testCase "a component or recipe-resource mutation invalidates the plan"
        <| fun _ ->
            let arc, _, _, _, _ = recipeFixture true
            let converted = convertCanonical [ canonicalLocation "stage-neutral" ] arc

            let recipeOwnerId, structuralProcess, recipeLinkId, _ =
                canonicalOwnerAndLink converted.Session

            let componentAssignment =
                structuralProcess.Assignments
                |> Map.toList
                |> List.map snd
                |> List.find (fun (assignment: CanonicalDomain.ProcessAssignment) ->
                    assignment.ContainerReferenceValueId.IsSome
                )

            let componentBefore = converted.Session.Values[componentAssignment.ValueId]

            let componentAfter = {
                componentBefore with
                    Value = CanonicalValues.ProvenanceValue.Text "forged-component"
            }

            let componentSession = {
                converted.Session with
                    Values = converted.Session.Values |> Map.add componentAfter.Id componentAfter
                    MutationJournal =
                        converted.Session.MutationJournal
                        @ [
                            CanonicalMutation.ProvenanceMutation.PropertyValueDefinitionUpdated(
                                componentBefore,
                                componentAfter,
                                canonicalMutationContext
                                    (Set.singleton componentAssignment.Id)
                                    componentAssignment.CoveredLinkIds
                            )
                        ]
            }

            let componentErrors =
                CanonicalPlanner.tryCreate converted.Index componentSession |> expectError

            Expect.contains
                componentErrors
                (ProcessCoreCanonicalWritebackError.ReadOnlyRecipeComponentMutation(Some componentAssignment.Id))
                "A forged Component edit is invalid prepared state."

            let forgedComponent = {
                componentAssignment with
                    Id = "assignment:forged-matching-component"
            }

            let forgedComponentSession = {
                converted.Session with
                    Processes =
                        converted.Session.Processes
                        |> Map.change
                            structuralProcess.Id
                            (Option.map (fun current -> {
                                current with
                                    Assignments =
                                        current.Assignments
                                        |> Map.remove componentAssignment.Id
                                        |> Map.add forgedComponent.Id forgedComponent
                            }))
            }

            let forgedComponentErrors =
                CanonicalPlanner.tryCreate converted.Index forgedComponentSession |> expectError

            Expect.isTrue
                (forgedComponentErrors
                 |> List.exists (
                     function
                     | ProcessCoreCanonicalWritebackError.ReadOnlyRecipeComponentMutation _ -> true
                     | _ -> false
                 ))
                "A matching-scalar Component replacement without the exact indexed occurrence is rejected."

            let withForgedCoverageLink =
                addParallelCanonicalLink "component-forged-coverage-link" structuralProcess.Id converted.Session

            let forgedCoverageAssignments =
                structuralProcess.Assignments
                |> Map.map (fun _ assignment -> {
                    assignment with
                        CoveredLinkIds = assignment.CoveredLinkIds |> Set.add "component-forged-coverage-link"
                })

            let forgedCoverageSession = {
                withForgedCoverageLink with
                    Processes =
                        withForgedCoverageLink.Processes
                        |> Map.change
                            structuralProcess.Id
                            (Option.map (fun current -> {
                                current with
                                    Assignments = forgedCoverageAssignments
                            }))
            }

            let forgedCoverageErrors =
                CanonicalPlanner.tryCreate converted.Index forgedCoverageSession |> expectError

            Expect.isTrue
                (forgedCoverageErrors
                 |> List.exists (
                     function
                     | ProcessCoreCanonicalWritebackError.ReadOnlyRecipeComponentMutation _
                     | ProcessCoreCanonicalWritebackError.InvalidPreparedState _ -> true
                     | _ -> false
                 ))
                "Recipe and Component coverage cannot change solely through forged final state."

            let componentProperty = converted.Session.Properties[componentBefore.PropertyId]

            let componentPropertyAfter = {
                componentProperty with
                    Category = {
                        componentProperty.Category with
                            TermSource = Some "forged-source"
                    }
            }

            let componentPropertySession = {
                converted.Session with
                    Properties =
                        converted.Session.Properties
                        |> Map.add componentPropertyAfter.Id componentPropertyAfter
                    MutationJournal =
                        converted.Session.MutationJournal
                        @ [
                            CanonicalMutation.ProvenanceMutation.PropertyDefinitionUpdated(
                                componentProperty,
                                componentPropertyAfter,
                                canonicalMutationContext
                                    (Set.singleton componentAssignment.Id)
                                    componentAssignment.CoveredLinkIds
                            )
                        ]
            }

            let componentPropertyErrors =
                CanonicalPlanner.tryCreate converted.Index componentPropertySession
                |> expectError

            Expect.isTrue
                (componentPropertyErrors
                 |> List.exists (
                     function
                     | ProcessCoreCanonicalWritebackError.ReadOnlyRecipeComponentMutation _ -> true
                     | _ -> false
                 ))
                "A forged Component property-definition edit is invalid prepared state."

            let recipeAssignment =
                structuralProcess.Assignments
                |> Map.toList
                |> List.map snd
                |> List.find (fun (assignment: CanonicalDomain.ProcessAssignment) -> assignment.ReferenceSlotId.IsSome)

            let recipeBefore = converted.Session.Values[recipeAssignment.ValueId]

            let recipeAfter = {
                recipeBefore with
                    Value =
                        match recipeBefore.Value with
                        | CanonicalValues.ProvenanceValue.Reference reference ->
                            CanonicalValues.ProvenanceValue.Reference {
                                reference with
                                    Label = "forged-label"
                            }
                        | _ -> failtest "Expected a Recipe reference value."
            }

            let resourceSession = {
                converted.Session with
                    Values = converted.Session.Values |> Map.add recipeAfter.Id recipeAfter
                    MutationJournal =
                        converted.Session.MutationJournal
                        @ [
                            CanonicalMutation.ProvenanceMutation.PropertyValueDefinitionUpdated(
                                recipeBefore,
                                recipeAfter,
                                canonicalMutationContext
                                    (Set.singleton recipeAssignment.Id)
                                    recipeAssignment.CoveredLinkIds
                            )
                        ]
            }

            let resourceErrors =
                CanonicalPlanner.tryCreate converted.Index resourceSession |> expectError

            Expect.contains
                resourceErrors
                ProcessCoreCanonicalWritebackError.ReadOnlyRecipeResourceMutation
                "A forged Recipe resource-value edit is invalid prepared state."

            let deletedResourceSession = {
                converted.Session with
                    Values = converted.Session.Values |> Map.remove recipeBefore.Id
                    MutationJournal =
                        converted.Session.MutationJournal
                        @ [
                            CanonicalMutation.ProvenanceMutation.PropertyValueDefinitionDeleted(
                                recipeBefore,
                                [],
                                canonicalMutationContext
                                    (Set.singleton recipeAssignment.Id)
                                    recipeAssignment.CoveredLinkIds
                            )
                        ]
            }

            let deletedResourceErrors =
                CanonicalPlanner.tryCreate converted.Index deletedResourceSession |> expectError

            Expect.contains
                deletedResourceErrors
                ProcessCoreCanonicalWritebackError.ReadOnlyRecipeResourceMutation
                "A deleted Recipe reference definition remains detectable from its journal payload."

            let ordinaryArc, _ = richAnnotationFixture ()

            let ordinaryConverted =
                convertCanonical [ canonicalLocation "stage-neutral" ] ordinaryArc

            let ordinaryOwnerId, ordinaryProcess, ordinaryLinkId, _ =
                canonicalOwnerAndLink ordinaryConverted.Session

            let ordinaryAssignment =
                ordinaryProcess.Assignments |> Map.toList |> List.map snd |> List.exactlyOne

            let missingWithoutTombstone = {
                ordinaryConverted.Session with
                    Processes =
                        ordinaryConverted.Session.Processes
                        |> Map.change
                            ordinaryOwnerId
                            (Option.map (fun current -> {
                                current with
                                    Assignments = current.Assignments |> Map.remove ordinaryAssignment.Id
                            }))
            }

            let missingErrors =
                CanonicalPlanner.tryCreate ordinaryConverted.Index missingWithoutTombstone
                |> expectError

            Expect.isTrue
                (missingErrors
                 |> List.exists (
                     function
                     | ProcessCoreCanonicalWritebackError.InvalidPreparedState _ -> true
                     | _ -> false
                 ))
                "An indexed assignment cannot disappear without a removal tombstone."

            let wrongTypeTombstone: CanonicalDomain.NodeAssignment = {
                Id = ordinaryAssignment.Id
                ValueId = ordinaryAssignment.ValueId
                PropertyKind = ordinaryAssignment.PropertyKind
                TargetSource = None
                Lineage = ordinaryAssignment.Lineage
            }

            let wrongTypeRemoval = {
                missingWithoutTombstone with
                    MutationJournal = [
                        CanonicalMutation.ProvenanceMutation.NodeAssignmentRemoved(
                            {
                                OwnerId = "canonical-node-1"
                                Assignment = wrongTypeTombstone
                            },
                            canonicalMutationContext
                                (Set.singleton ordinaryAssignment.Id)
                                (Set.singleton ordinaryLinkId)
                        )
                    ]
            }

            let wrongTypeRemovalErrors =
                CanonicalPlanner.tryCreate ordinaryConverted.Index wrongTypeRemoval
                |> expectError

            Expect.isTrue
                (wrongTypeRemovalErrors
                 |> List.exists (
                     function
                     | ProcessCoreCanonicalWritebackError.InvalidPreparedState _ -> true
                     | _ -> false
                 ))
                "A node tombstone with the same ID cannot remove an indexed process assignment."

            let wrongRecordRemoval = {
                missingWithoutTombstone with
                    MutationJournal = [
                        CanonicalMutation.ProvenanceMutation.ProcessAssignmentRemoved(
                            {
                                OwnerId = ordinaryOwnerId
                                Assignment = {
                                    ordinaryAssignment with
                                        ValueId = "processcore-value-999999"
                                }
                            },
                            canonicalMutationContext
                                (Set.singleton ordinaryAssignment.Id)
                                (Set.singleton ordinaryLinkId)
                        )
                    ]
            }

            let wrongRecordRemovalErrors =
                CanonicalPlanner.tryCreate ordinaryConverted.Index wrongRecordRemoval
                |> expectError

            Expect.isTrue
                (wrongRecordRemovalErrors
                 |> List.exists (
                     function
                     | ProcessCoreCanonicalWritebackError.InvalidPreparedState _ -> true
                     | _ -> false
                 ))
                "A matching assignment ID cannot hide a forged owner record, lineage, or value identity."

            let commandRemoved =
                CanonicalCommands.removeProcessAssignmentLinks
                    ordinaryOwnerId
                    ordinaryAssignment.Id
                    (Set.singleton ordinaryLinkId)
                    ordinaryConverted.Session
                |> expectOk
                |> fun effect -> commitCanonical effect ordinaryConverted.Session

            CanonicalPlanner.tryCreate ordinaryConverted.Index commandRemoved
            |> expectOk
            |> ignore

            let journalledCreation =
                ordinaryConverted.Session
                |> addCanonicalProcessValue
                    "assignment:journal-witness-mismatch"
                    "parameter"
                    (CanonicalValues.ProvenanceValue.Text "before")
                    (CanonicalValues.AssignmentPropertyKind.AdapterSpecific ProcessCoreCanonicalKinds.parameter)
                    (Set.singleton ordinaryLinkId)
                    ordinaryOwnerId

            let createdAssignment =
                journalledCreation.Processes[ordinaryOwnerId].Assignments["assignment:journal-witness-mismatch"]

            let forgedValue = {
                journalledCreation.Values[createdAssignment.ValueId] with
                    Id = "value:journal-witness-mismatch"
                    Value = CanonicalValues.ProvenanceValue.Text "after"
            }

            let forgedAssignment = {
                createdAssignment with
                    ValueId = forgedValue.Id
            }

            let forgedAfterJournal = {
                journalledCreation with
                    Values = journalledCreation.Values |> Map.add forgedValue.Id forgedValue
                    Processes =
                        journalledCreation.Processes
                        |> Map.change
                            ordinaryOwnerId
                            (Option.map (fun current -> {
                                current with
                                    Assignments = current.Assignments |> Map.add forgedAssignment.Id forgedAssignment
                            }))
            }

            let forgedAfterJournalErrors =
                CanonicalPlanner.tryCreate ordinaryConverted.Index forgedAfterJournal
                |> expectError

            Expect.isTrue
                (forgedAfterJournalErrors
                 |> List.exists (
                     function
                     | ProcessCoreCanonicalWritebackError.InvalidPreparedState _ -> true
                     | _ -> false
                 ))
                "A journalled assignment creation must match the exact final assignment record."

            let recipeOnlyAssignmentsRemoved = {
                converted.Session with
                    Processes =
                        converted.Session.Processes
                        |> Map.change
                            structuralProcess.Id
                            (Option.map (fun current -> { current with Assignments = Map.empty }))
            }

            let unjustifiedRecipeClearErrors =
                CanonicalPlanner.tryCreate converted.Index recipeOnlyAssignmentsRemoved
                |> expectError

            Expect.isTrue
                (unjustifiedRecipeClearErrors
                 |> List.exists (
                     function
                     | ProcessCoreCanonicalWritebackError.InvalidPreparedState _ -> true
                     | _ -> false
                 ))
                "A Recipe clear cannot be inferred from final-state absence without atomic removal evidence."

            let detachedRecipe =
                CanonicalCommands.removeProcessAssignmentLinks
                    recipeOwnerId
                    recipeAssignment.Id
                    (Set.singleton recipeLinkId)
                    converted.Session
                |> expectOk
                |> fun effect -> commitCanonical effect converted.Session

            let componentLocation =
                converted.Index.AssignmentLocations[componentAssignment.Id] |> List.exactlyOne

            let mixedComponentLocation = {
                componentLocation with
                    Owner =
                        ProcessCoreCanonicalAnnotationOwner.ProcessParameterValue
                            converted.Index.ProcessLocations[recipeOwnerId]
            }

            let mixedComponentIndex = {
                converted.Index with
                    AssignmentLocations =
                        converted.Index.AssignmentLocations
                        |> Map.add componentAssignment.Id [ componentLocation; mixedComponentLocation ]
            }

            let mixedComponentErrors =
                CanonicalPlanner.tryCreate mixedComponentIndex detachedRecipe |> expectError

            Expect.isTrue
                (mixedComponentErrors
                 |> List.exists (
                     function
                     | ProcessCoreCanonicalWritebackError.ReadOnlyRecipeComponentMutation _
                     | ProcessCoreCanonicalWritebackError.InvalidPreparedState _ -> true
                     | _ -> false
                 ))
                "A Component tombstone cannot authorize a malformed assignment index with extra or mixed locations."

            let forgedComponentDetachment = {
                detachedRecipe with
                    MutationJournal =
                        detachedRecipe.MutationJournal
                        |> List.map (
                            function
                            | CanonicalMutation.ProvenanceMutation.ProcessAssignmentRemoved(tombstone, context) when
                                tombstone.Assignment.Id = componentAssignment.Id
                                ->
                                CanonicalMutation.ProvenanceMutation.ProcessAssignmentRemoved(
                                    {
                                        tombstone with
                                            Assignment = {
                                                tombstone.Assignment with
                                                    ValueId = "processcore-value-999999"
                                            }
                                    },
                                    context
                                )
                            | mutation -> mutation
                        )
            }

            let forgedComponentDetachmentErrors =
                CanonicalPlanner.tryCreate converted.Index forgedComponentDetachment
                |> expectError

            Expect.isTrue
                (forgedComponentDetachmentErrors
                 |> List.exists (
                     function
                     | ProcessCoreCanonicalWritebackError.ReadOnlyRecipeComponentMutation _
                     | ProcessCoreCanonicalWritebackError.InvalidPreparedState _ -> true
                     | _ -> false
                 ))
                "Detaching a Recipe requires the exact indexed Component tombstone, including its catalog lineage and value identity."

        testCase "a divergent-content identity collision is reminted only where the operation controls it"
        <| fun _ ->
            let existing =
                Annotation("collision", value = "same", valueTAN = "term:existing", additionalType = "ParameterValue")

            let input = Sample("input-neutral")
            let output = Sample("output-neutral")

            let proc =
                mkProcessFull "stage-neutral" None [ SampleNode input ] [ SampleNode output ] [ existing ]

            let dataset = Dataset("dataset-neutral", processes = [ proc ])
            let arc = ARC("arc-neutral", hasPart = [ dataset ])
            let converted = convertCanonical [ canonicalLocation "stage-neutral" ] arc
            let ownerId, _, _, _ = canonicalOwnerAndLink converted.Session
            let existingPayload = ProcessCore.Yaml.Annotation.toYamlString None existing

            let session =
                converted.Session
                |> addParallelCanonicalLink "collision-link" ownerId
                |> addCanonicalProcessValue
                    "assignment:controlled"
                    "collision"
                    (CanonicalValues.ProvenanceValue.Term {
                        Name = "same"
                        TermSource = None
                        TermAccession = Some "term:controlled"
                    })
                    (CanonicalValues.AssignmentPropertyKind.AdapterSpecific ProcessCoreCanonicalKinds.parameter)
                    (Set.singleton "collision-link")
                    ownerId

            let plan = CanonicalPlanner.tryCreate converted.Index session |> expectOk
            let remint = plan.AnnotationRemintings |> List.exactlyOne
            Expect.equal remint.AssignmentId "assignment:controlled" "Only the created occurrence is controlled."
            Expect.notEqual remint.OriginalRegistryId remint.PlannedRegistryId "The controlled identity is reminted."

            Expect.equal
                (ProcessCore.Yaml.Annotation.toYamlString None existing)
                existingPayload
                "Planning leaves unrelated stored annotation metadata untouched."

            let resourceArc, _, _, firstRecipe, _ = recipeFixture false
            let storedComponent = firstRecipe.Components |> Seq.head
            storedComponent.AdditionalType <- Some "ParameterValue"
            storedComponent.ValueTAN <- Some "term:stored"

            let storedComponentPayload =
                ProcessCore.Yaml.Annotation.toYamlString None storedComponent

            let resourceConverted =
                convertCanonical [ canonicalLocation "stage-neutral" ] resourceArc

            let resourceOwnerId, _, resourceLinkId, _ =
                canonicalOwnerAndLink resourceConverted.Session

            let resourceCollisionSession =
                resourceConverted.Session
                |> addCanonicalProcessValue
                    "assignment:resource-collision"
                    storedComponent.Name
                    (CanonicalValues.ProvenanceValue.Term {
                        Name = storedComponent.Value.Value
                        TermSource = None
                        TermAccession = Some "term:controlled"
                    })
                    (CanonicalValues.AssignmentPropertyKind.AdapterSpecific ProcessCoreCanonicalKinds.parameter)
                    (Set.singleton resourceLinkId)
                    resourceOwnerId

            let resourceCollisionPlan =
                CanonicalPlanner.tryCreate resourceConverted.Index resourceCollisionSession
                |> expectOk

            Expect.isTrue
                (resourceCollisionPlan.AnnotationRemintings
                 |> List.exists (fun remint -> remint.AssignmentId = "assignment:resource-collision"))
                "A controlled writable occurrence is reminted away from stored Recipe Component metadata."

            Expect.equal
                (ProcessCore.Yaml.Annotation.toYamlString None storedComponent)
                storedComponentPayload
                "Read-only stored Recipe Component metadata remains untouched."

            let metadataArc, _, _, metadataRecipe, _ = recipeFixture false

            let storedAdditionalProperty =
                Annotation(
                    "metadata-collision",
                    value = "same",
                    valueTAN = "term:stored",
                    additionalType = "ParameterValue"
                )

            metadataRecipe.AddAdditionalProperty storedAdditionalProperty
            let metadataRegistryId = ProcessCore.Yaml.Annotation.genID storedAdditionalProperty
            let metadataAssignmentId = "assignment:recipe-metadata-collision"

            let encodedAssignment =
                metadataAssignmentId
                |> Seq.map (fun character ->
                    (int character).ToString("X4", System.Globalization.CultureInfo.InvariantCulture)
                )
                |> String.concat ""

            let remintBaseId = $"{metadataRegistryId}__arc_{encodedAssignment}"

            let reservedAdditionalProperty =
                Annotation("reserved-additional-property", value = "reserved")

            reservedAdditionalProperty.SetProperty("@id", remintBaseId)

            let additionalDefault = DefinedTerm("additional-default")
            additionalDefault.SetProperty("@id", remintBaseId + "_8")

            let additionalInstance =
                FormalParameter("additional-instance", defaultValue = additionalDefault)

            additionalInstance.SetProperty("@id", remintBaseId + "_7")
            reservedAdditionalProperty.InstanceOf <- Some additionalInstance
            metadataRecipe.AddAdditionalProperty reservedAdditionalProperty

            let parameterDefault = DefinedTerm("parameter-default")
            parameterDefault.SetProperty("@id", remintBaseId + "_3")

            let reservedParameter =
                FormalParameter("reserved-parameter", defaultValue = parameterDefault)

            reservedParameter.SetProperty("@id", remintBaseId + "_2")
            metadataRecipe.AddParameter reservedParameter

            let intendedUse = DefinedTerm("reserved-intended-use")
            intendedUse.SetProperty("@id", remintBaseId + "_4")
            metadataRecipe.IntendedUse <- Some intendedUse

            let componentDefault = DefinedTerm("component-default")
            componentDefault.SetProperty("@id", remintBaseId + "_6")

            let componentInstance =
                FormalParameter("component-instance", defaultValue = componentDefault)

            componentInstance.SetProperty("@id", remintBaseId + "_5")
            metadataRecipe.Components[0].InstanceOf <- Some componentInstance

            let metadataPayloadBefore = CanonicalGraph.recipePayloadFingerprint metadataRecipe

            let metadataConverted =
                convertCanonical [ canonicalLocation "stage-neutral" ] metadataArc

            let metadataOwnerId, _, metadataLinkId, _ =
                canonicalOwnerAndLink metadataConverted.Session

            let metadataCollisionSession =
                metadataConverted.Session
                |> addCanonicalProcessValue
                    metadataAssignmentId
                    storedAdditionalProperty.Name
                    (CanonicalValues.ProvenanceValue.Term {
                        Name = storedAdditionalProperty.Value.Value
                        TermSource = None
                        TermAccession = Some "term:controlled"
                    })
                    (CanonicalValues.AssignmentPropertyKind.AdapterSpecific ProcessCoreCanonicalKinds.parameter)
                    (Set.singleton metadataLinkId)
                    metadataOwnerId

            let metadataCollisionPlan =
                CanonicalPlanner.tryCreate metadataConverted.Index metadataCollisionSession
                |> expectOk

            let metadataRemint =
                metadataCollisionPlan.AnnotationRemintings
                |> List.tryFind (fun remint -> remint.AssignmentId = metadataAssignmentId)

            Expect.isSome
                metadataRemint
                "Recipe AdditionalProperty participates in divergent annotation-registry collision detection."

            metadataRemint
            |> Option.iter (fun remint ->
                Expect.equal
                    remint.PlannedRegistryId
                    (remintBaseId + "_9")
                    "Every immutable Recipe-owned explicit identity reserves its deterministic remint candidate."
            )

            Expect.equal
                (CanonicalGraph.recipePayloadFingerprint metadataRecipe)
                metadataPayloadBefore
                "Collision planning leaves the complete stored Recipe metadata payload unchanged."

            let second =
                Annotation("collision", value = "same", valueTAN = "term:other", additionalType = "ParameterValue")

            let proc2 =
                mkProcessFull "stage-neutral" None [ SampleNode(Sample("input-two")) ] [
                    SampleNode(Sample("output-two"))
                ] [ second ]

            dataset.AddProcess proc2
            let reloaded = convertCanonical [ canonicalLocation "stage-neutral" ] arc

            let errors =
                CanonicalPlanner.tryCreate reloaded.Index reloaded.Session |> expectError

            Expect.isTrue
                (errors
                 |> List.exists (
                     function
                     | ProcessCoreCanonicalWritebackError.ConflictingAnnotationIdentity _ -> true
                     | _ -> false
                 ))
                "Two uncontrolled divergent identities are unresolvable."

        testCase "destination locations and order are derived from the final exact links"
        <| fun _ ->
            let arc, _, _ = positional ()
            let converted = convertCanonical [ canonicalLocation "stage-neutral" ] arc

            let poisonedIndex = {
                converted.Index with
                    NodeLocations =
                        converted.Index.NodeLocations
                        |> Map.map (fun _ locations ->
                            locations
                            |> List.rev
                            |> List.mapi (fun index location -> {
                                location with
                                    SourceOrderHint = 100 - index
                            })
                        )
            }

            let plan = CanonicalPlanner.tryCreate poisonedIndex converted.Session |> expectOk

            Expect.sequenceEqual
                (canonicalEndpointPairs plan)
                [
                    Some "canonical-node-1", Some "canonical-node-2"
                    Some "canonical-node-3", Some "canonical-node-4"
                ]
                "Destination pairing follows final exact canonical links."

            Expect.sequenceEqual
                (plan.Processes |> List.map _.DestinationOrder)
                [ 0; 1 ]
                "Destination order follows planned final Process order, not endpoint hints."

            let existingNodeId, _ = converted.Index.NodeLocations |> Map.toList |> List.head
            let existingNode = converted.Session.Nodes[existingNodeId]

            let forgedExistingKind =
                if existingNode.Kind.Id = ProcessCoreCanonicalKinds.dataEndpoint.Id then
                    ProcessCoreCanonicalKinds.sampleEndpoint
                else
                    ProcessCoreCanonicalKinds.dataEndpoint

            let forgedExistingNode = {
                existingNode with
                    Key = {
                        existingNode.Key with
                            KindId = forgedExistingKind.Id
                    }
                    Kind = forgedExistingKind
            }

            let forgedExistingNodeSession = {
                converted.Session with
                    Nodes = converted.Session.Nodes |> Map.add existingNodeId forgedExistingNode
            }

            let forgedExistingNodeErrors =
                CanonicalPlanner.tryCreate converted.Index forgedExistingNodeSession
                |> expectError

            Expect.isTrue
                (forgedExistingNodeErrors
                 |> List.exists (
                     function
                     | ProcessCoreCanonicalWritebackError.InvalidPreparedState _
                     | ProcessCoreCanonicalWritebackError.InconsistentCanonicalState _ -> true
                     | _ -> false
                 ))
                "An indexed canonical node's kind and key must still match its indexed ProcessCore source node."

        testCase "reordering appearances after links exist does not change the planned links"
        <| fun _ ->
            let arc, _, _ = positional ()
            let converted = convertCanonical [ canonicalLocation "stage-neutral" ] arc

            let before =
                CanonicalPlanner.tryCreate converted.Index converted.Session |> expectOk

            let reorderedLayers =
                converted.Session.Layers
                |> Map.map (fun _ layer -> {
                    layer with
                        InputEndpoints =
                            layer.InputEndpoints
                            |> Map.map (fun _ endpoint -> {
                                endpoint with
                                    LayerOrderPosition = 100 - endpoint.LayerOrderPosition
                            })
                        OutputEndpoints =
                            layer.OutputEndpoints
                            |> Map.map (fun _ endpoint -> {
                                endpoint with
                                    LayerOrderPosition = 100 - endpoint.LayerOrderPosition
                            })
                })

            let reordered = {
                converted.Session with
                    Layers = reorderedLayers
            }

            let after = CanonicalPlanner.tryCreate converted.Index reordered |> expectOk

            Expect.sequenceEqual
                (canonicalEndpointPairs after)
                (canonicalEndpointPairs before)
                "Appearance order never changes exact link semantics."

        testCase "a loaded one-sided promotion plans an in-place update"
        <| fun _ ->
            let arc, _, _ = inputOnly ()
            let converted = convertCanonical [ canonicalLocation "stage-neutral" ] arc

            let ownerId, structuralProcess, linkId, processLink =
                canonicalOwnerAndLink converted.Session

            let layer: CanonicalDomain.ProvenanceLayer =
                converted.Session.Layers[structuralProcess.OriginLayerId]

            let newNodeId = "canonical-node:promoted-output"

            let plannedDataKind: CanonicalIdentifiers.ProvenanceKind = {
                ProcessCoreCanonicalKinds.dataEndpoint with
                    Label = "Measurement data"
            }

            let newNode: CanonicalDomain.CanonicalNode = {
                Id = newNodeId
                Key = {
                    KindId = plannedDataKind.Id
                    Name = "promoted-output.dat"
                }
                Kind = plannedDataKind
                Name = "promoted-output.dat"
                Assignments = Map.empty
            }

            let outputEndpoint: CanonicalDomain.LayerEndpoint = {
                Key = {
                    LayerId = layer.Id
                    Side = CanonicalIdentifiers.ProvenanceSide.Output
                    NodeId = newNodeId
                }
                Header = {
                    Kind = plannedDataKind
                    Text = "Data"
                }
                LayerOrderPosition = 0
            }

            let promotedLink = {
                processLink with
                    Shape =
                        match processLink.Shape with
                        | CanonicalValues.ProcessLinkShape.InputOnly inputId ->
                            CanonicalValues.ProcessLinkShape.Between(inputId, newNodeId)
                        | other -> failtestf "Expected InputOnly but received %A" other
            }

            let promotedProcess = {
                structuralProcess with
                    Links = Map.ofList [ linkId, promotedLink ]
            }

            let session = {
                converted.Session with
                    Nodes = converted.Session.Nodes |> Map.add newNode.Id newNode
                    Processes = converted.Session.Processes |> Map.add ownerId promotedProcess
                    Layers =
                        converted.Session.Layers
                        |> Map.add layer.Id {
                            layer with
                                OutputEndpoints = layer.OutputEndpoints |> Map.add newNodeId outputEndpoint
                        }
                    MutationJournal =
                        converted.Session.MutationJournal
                        @ [
                            CanonicalMutation.ProvenanceMutation.CanonicalNodeCreated newNode
                            CanonicalMutation.ProvenanceMutation.LayerEndpointAdded outputEndpoint
                            CanonicalMutation.ProvenanceMutation.StructuralProcessReshaped(
                                structuralProcess,
                                promotedProcess
                            )
                        ]
            }

            let targetedSession =
                session
                |> addCanonicalNodeValue
                    "assignment:targeted-node"
                    "characteristic"
                    (CanonicalValues.ProvenanceValue.Text "targeted")
                    (CanonicalValues.AssignmentPropertyKind.AdapterSpecific ProcessCoreCanonicalKinds.characteristic)
                    (Some layer.Source)
                    newNodeId

            let plan = CanonicalPlanner.tryCreate converted.Index targetedSession |> expectOk
            let plannedProcess = plan.Processes |> List.exactlyOne
            let plannedNode = plan.Nodes |> List.find (fun node -> node.NodeId = newNodeId)
            let plannedNodeAnnotation = plannedNode.Annotations |> List.exactlyOne

            Expect.isTrue plannedProcess.ReusesIndexedProcess "Promotion updates the indexed Process in place."
            Expect.equal plan.Summary.AddedProcesses 0 "Promotion adds no Process."
            Expect.equal plan.Summary.RemovedProcesses 0 "Promotion removes no Process."

            Expect.equal
                plannedNode.Kind
                plannedDataKind
                "New-node planning preserves the full endpoint kind including its label."

            Expect.equal
                plannedNodeAnnotation.TargetSource
                (Some layer.Source)
                "The node annotation plan retains its exact target source."

            Expect.equal
                plannedNodeAnnotation.TargetDestination
                (Some(canonicalLocation "stage-neutral"))
                "The target source resolves to its exact canonical destination during planning."

            let unwitnessedNodeSession = {
                targetedSession with
                    MutationJournal =
                        targetedSession.MutationJournal
                        |> List.filter (
                            function
                            | CanonicalMutation.ProvenanceMutation.CanonicalNodeCreated created ->
                                created.Id <> newNodeId
                            | _ -> true
                        )
            }

            let unwitnessedNodeErrors =
                CanonicalPlanner.tryCreate converted.Index unwitnessedNodeSession |> expectError

            Expect.isTrue
                (unwitnessedNodeErrors
                 |> List.exists (
                     function
                     | ProcessCoreCanonicalWritebackError.InvalidPreparedState _ -> true
                     | _ -> false
                 ))
                "Every newly materialized node requires a CanonicalNodeCreated witness."

            let forgedCreatedNode = {
                newNode with
                    Key = {
                        newNode.Key with
                            Name = "forged-created-name"
                    }
                    Name = "forged-created-name"
            }

            let forgedNodeWitnessSession = {
                targetedSession with
                    MutationJournal =
                        targetedSession.MutationJournal
                        |> List.map (
                            function
                            | CanonicalMutation.ProvenanceMutation.CanonicalNodeCreated created when
                                created.Id = newNodeId
                                ->
                                CanonicalMutation.ProvenanceMutation.CanonicalNodeCreated forgedCreatedNode
                            | mutation -> mutation
                        )
            }

            let forgedNodeWitnessErrors =
                CanonicalPlanner.tryCreate converted.Index forgedNodeWitnessSession
                |> expectError

            Expect.isTrue
                (forgedNodeWitnessErrors
                 |> List.exists (
                     function
                     | ProcessCoreCanonicalWritebackError.InvalidPreparedState _ -> true
                     | _ -> false
                 ))
                "CanonicalNodeCreated must witness the exact identity of the node being materialized."

            let missingTarget: CanonicalIdentifiers.ProvenanceSourceRef = {
                Id = "source:missing"
                Name = "Missing source"
            }

            let missingTargetSession =
                session
                |> addCanonicalNodeValue
                    "assignment:missing-target"
                    "characteristic"
                    (CanonicalValues.ProvenanceValue.Text "targeted")
                    (CanonicalValues.AssignmentPropertyKind.AdapterSpecific ProcessCoreCanonicalKinds.characteristic)
                    (Some missingTarget)
                    newNodeId

            let missingTargetErrors =
                CanonicalPlanner.tryCreate converted.Index missingTargetSession |> expectError

            Expect.contains
                missingTargetErrors
                (ProcessCoreCanonicalWritebackError.SourceLocationNotFound missingTarget.Id)
                "An unresolved node-assignment target source fails pure planning."

            let unsupportedKind: CanonicalIdentifiers.ProvenanceKind = {
                Id = "process-core:endpoint:unsupported"
                Label = "Unsupported endpoint"
            }

            let unsupportedNode = {
                newNode with
                    Key = {
                        newNode.Key with
                            KindId = unsupportedKind.Id
                    }
                    Kind = unsupportedKind
            }

            let unsupportedEndpoint = {
                outputEndpoint with
                    Header = {
                        outputEndpoint.Header with
                            Kind = unsupportedKind
                    }
            }

            let unsupportedSession = {
                session with
                    Nodes = session.Nodes |> Map.add unsupportedNode.Id unsupportedNode
                    Layers =
                        session.Layers
                        |> Map.change
                            layer.Id
                            (Option.map (fun current -> {
                                current with
                                    OutputEndpoints =
                                        current.OutputEndpoints |> Map.add unsupportedNode.Id unsupportedEndpoint
                            }))
                    MutationJournal =
                        session.MutationJournal
                        |> List.map (
                            function
                            | CanonicalMutation.ProvenanceMutation.CanonicalNodeCreated created when
                                created.Id = newNode.Id
                                ->
                                CanonicalMutation.ProvenanceMutation.CanonicalNodeCreated unsupportedNode
                            | mutation -> mutation
                        )
            }

            let unsupportedErrors =
                CanonicalPlanner.tryCreate converted.Index unsupportedSession |> expectError

            Expect.contains
                unsupportedErrors
                (ProcessCoreCanonicalWritebackError.UnsupportedEndpointKind unsupportedKind.Id)
                "Unsupported new endpoint kinds fail during pure planning."

        testCase "a disconnection keeps the output continuation on the indexed process across repeated planning"
        <| fun _ ->
            let fixture = basic ()
            let converted = convertCanonical [ canonicalLocation "stage-neutral" ] fixture.Arc
            let _, _, linkId, _ = canonicalOwnerAndLink converted.Session

            let disconnected =
                CanonicalCommands.disconnectLinks (Set.singleton linkId) converted.Session
                |> expectOk
                |> fun effect -> commitCanonical effect converted.Session

            let first = CanonicalPlanner.tryCreate converted.Index disconnected |> expectOk
            let second = CanonicalPlanner.tryCreate converted.Index disconnected |> expectOk

            let reused (plan: CanonicalPlanner.ProcessCoreWritebackPlan) =
                plan.Processes |> List.find _.ReusesIndexedProcess

            Expect.equal (reused first).Shape (reused second).Shape "Repeated planning chooses the same continuation."

            match (reused first).Shape with
            | CanonicalValues.ProcessLinkShape.OutputOnly _ -> ()
            | other -> failtestf "The indexed Process must retain the output continuation, received %A" other

            let disconnectedOwnerId, disconnectedProcess, _, _ =
                canonicalOwnerAndLink disconnected

            let originalContinuation = disconnectedProcess.Links[linkId]

            let renamedContinuation = {
                originalContinuation with
                    Id = "process-link:renamed-output-continuation"
            }

            let renamedProcess = {
                disconnectedProcess with
                    Links =
                        disconnectedProcess.Links
                        |> Map.remove originalContinuation.Id
                        |> Map.add renamedContinuation.Id renamedContinuation
            }

            let renamedSession = {
                disconnected with
                    Processes = disconnected.Processes |> Map.add disconnectedOwnerId renamedProcess
                    MutationJournal =
                        disconnected.MutationJournal
                        @ [
                            CanonicalMutation.ProvenanceMutation.StructuralProcessReshaped(
                                disconnectedProcess,
                                renamedProcess
                            )
                        ]
            }

            let renamedPlan =
                CanonicalPlanner.tryCreate converted.Index renamedSession |> expectOk

            let hintOnlyIndex = {
                converted.Index with
                    NodeLocations =
                        converted.Index.NodeLocations
                        |> Map.map (fun _ locations ->
                            locations
                            |> List.map (fun location -> {
                                location with
                                    SourceOrderHint = location.SourceOrderHint + 10_000
                            })
                        )
            }

            let hintOnlyPlan =
                CanonicalPlanner.tryCreate hintOnlyIndex renamedSession |> expectOk

            Expect.equal
                (reused hintOnlyPlan).Shape
                (reused renamedPlan).Shape
                "Source-order hints cannot change which disconnected continuation reuses the indexed Process."

        testCase "the plan is identical when computed from a session whose display projections are cleared"
        <| fun _ ->
            let fixture = basic ()
            let converted = convertCanonical [ canonicalLocation "stage-neutral" ] fixture.Arc

            let withProjection =
                CanonicalPlanner.tryCreate converted.Index converted.Session |> expectOk

            let cleared = {
                converted.Session with
                    LayerProjections = Map.empty
            }

            let withoutProjection =
                CanonicalPlanner.tryCreate converted.Index cleared |> expectOk

            Expect.equal
                withoutProjection
                withProjection
                "Every actionable plan field is independent of display projections."
    ]

// ── Canonical apply fixtures and helpers ────────────────────────────────────

type private CanonicalApplyFixture = {
    Arc: ARC
    Dataset: Dataset
    Process: Process
    Input: Sample
    Output: Sample
    Characteristic: Annotation
    Factor: Annotation
    Parameter: Annotation
    AssignedRecipe: Recipe
    UnassignedRecipe: Recipe
}

/// One loaded process group carrying a node characteristic and factor, a
/// process parameter, an assigned stored Recipe with one Component, and a
/// second, unassigned stored Recipe.
let private canonicalApplyFixture () =
    let characteristic =
        Annotation("characteristic-neutral", value = "before", additionalType = "CharacteristicValue")

    let factor =
        Annotation("factor-neutral", value = "level-neutral", additionalType = "FactorValue")

    let parameter =
        Annotation("parameter-neutral", value = "before", additionalType = "ParameterValue")

    let input = Sample("input-neutral")
    input.AddAdditionalProperty characteristic
    let output = Sample("output-neutral")
    output.AddAdditionalProperty factor
    let assigned = recipeWithId "recipe:assigned" "assigned-recipe" "1"
    let unassigned = recipeWithId "recipe:unassigned" "unassigned-recipe" "1"

    let processObject =
        mkProcessFull "stage-neutral" (Some assigned) [ SampleNode input ] [ SampleNode output ] [ parameter ]

    let dataset = Dataset("dataset-neutral", processes = [ processObject ])
    let arc = ARC("arc-neutral", hasPart = [ dataset ])
    arc.AddRecipe assigned
    arc.AddRecipe unassigned

    {
        Arc = arc
        Dataset = dataset
        Process = processObject
        Input = input
        Output = output
        Characteristic = characteristic
        Factor = factor
        Parameter = parameter
        AssignedRecipe = assigned
        UnassignedRecipe = unassigned
    }

let private arcPayload (arc: ARC) = arc.toYamlString ()

let private recipePayload (recipe: Recipe) =
    ProcessCore.Yaml.Recipe.toYamlString None recipe

let private prepareCanonical (session: CanonicalProjectionTypes.ProvenanceSession) =
    CanonicalSession.prepareForWriteback session |> expectOk

let private canonicalNodeIdByName name (session: CanonicalProjectionTypes.ProvenanceSession) =
    session.Nodes
    |> Map.toList
    |> List.find (fun (_, node) -> node.Name = name)
    |> fst

let private endpointName (node: IONode) =
    match node with
    | SampleNode sample -> sample.Name
    | DataNode data -> data.Path

let private processShapes (dataset: Dataset) =
    dataset.Processes
    |> Seq.map (fun proc ->
        (proc.Input |> Option.toList |> List.map endpointName), (proc.Output |> Option.toList |> List.map endpointName)
    )
    |> List.ofSeq

let private canonicalHeader (kind: CanonicalIdentifiers.ProvenanceKind) : CanonicalIdentifiers.ProvenanceIOHeader = {
    Kind = kind
    Text = kind.Label
}

let private canonicalContent name value : CanonicalCommands.NodeValueContent = {
    Category = {
        Name = name
        TermSource = None
        TermAccession = None
    }
    Value = CanonicalValues.ProvenanceValue.Text value
    Unit = None
}

let private addCanonicalEndpoint layerId side kind name position session =
    CanonicalCommands.addEndpoint layerId side kind (canonicalHeader kind) name position session
    |> expectOk
    |> fun effect -> commitCanonical effect session

let private connectCanonicalNodes layerId pairs session =
    CanonicalCommands.connectNodes layerId pairs session
    |> expectOk
    |> fun effect -> commitCanonical effect session

let private unresolvableRecipeSession (converted: ProcessCoreCanonicalConversionResult) =
    let _, structuralProcess, _, _ = canonicalOwnerAndLink converted.Session

    let recipeAssignment =
        structuralProcess.Assignments
        |> Map.toList
        |> List.map snd
        |> List.find (fun (assignment: CanonicalDomain.ProcessAssignment) -> assignment.ReferenceSlotId.IsSome)

    let unknown = {
        converted.Session.Values[recipeAssignment.ValueId] with
            Value =
                CanonicalValues.ProvenanceValue.Reference {
                    Scheme = ProcessCoreCanonicalKinds.processCoreRecipeScheme
                    Id = "recipe:missing"
                    Label = "missing"
                }
    }

    {
        converted.Session with
            Values = converted.Session.Values |> Map.add unknown.Id unknown
    }

let private canonicalApplyTests =
    testList "canonical ProcessCore writeback apply" [
        testCase "preflight is non-mutating"
        <| fun _ ->
            let staleFixture = canonicalApplyFixture ()

            let staleConverted =
                convertCanonical [ canonicalLocation "stage-neutral" ] staleFixture.Arc

            staleFixture.Dataset.AddProcess(mkProcess "stage-neutral" [ SampleNode(Sample("external-input")) ] [])

            let stalePayload = arcPayload staleFixture.Arc

            let staleErrors =
                canonicalWriteBackMany staleConverted.Index staleConverted.Session staleFixture.Arc
                |> expectError

            Expect.contains
                staleErrors
                ProcessCoreCanonicalWritebackError.StaleArc
                "An externally changed ARC is refused."

            Expect.equal
                (arcPayload staleFixture.Arc)
                stalePayload
                "A refused stale-graph check leaves the ARC byte-identical."

            let sourceFixture = canonicalApplyFixture ()

            let sourceConverted =
                convertCanonical [ canonicalLocation "stage-neutral" ] sourceFixture.Arc

            let layerId = sourceConverted.Session.ActiveLayerId
            let layer = sourceConverted.Session.Layers[layerId]

            let forgedSource = {
                sourceConverted.Session with
                    Layers =
                        sourceConverted.Session.Layers
                        |> Map.add layerId {
                            layer with
                                Source = {
                                    Id = "unknown-source"
                                    Name = "unknown-source"
                                }
                        }
            }

            let sourcePayload = arcPayload sourceFixture.Arc

            let sourceErrors =
                canonicalWriteBackMany sourceConverted.Index forgedSource sourceFixture.Arc
                |> expectError

            Expect.isTrue
                (sourceErrors
                 |> List.exists (
                     function
                     | ProcessCoreCanonicalWritebackError.SourceLocationNotFound _
                     | ProcessCoreCanonicalWritebackError.InitialLayerNotFound _ -> true
                     | _ -> false
                 ))
                "An unresolvable layer source is refused."

            Expect.equal
                (arcPayload sourceFixture.Arc)
                sourcePayload
                "A refused source-resolution check leaves the ARC byte-identical."

            let recipeFailureFixture = canonicalApplyFixture ()

            let recipeConverted =
                convertCanonical [ canonicalLocation "stage-neutral" ] recipeFailureFixture.Arc

            let unresolvable = unresolvableRecipeSession recipeConverted
            let recipeFailurePayload = arcPayload recipeFailureFixture.Arc

            let recipeErrors =
                canonicalWriteBackMany recipeConverted.Index unresolvable recipeFailureFixture.Arc
                |> expectError

            Expect.isTrue
                (recipeErrors
                 |> List.exists (
                     function
                     | ProcessCoreCanonicalWritebackError.RecipeResourceNotFound _ -> true
                     | _ -> false
                 ))
                "An unresolvable Recipe reference is refused."

            Expect.equal
                (arcPayload recipeFailureFixture.Arc)
                recipeFailurePayload
                "A refused Recipe resolution leaves the ARC byte-identical."

            let linkFixture = canonicalApplyFixture ()

            let linkConverted =
                convertCanonical [ canonicalLocation "stage-neutral" ] linkFixture.Arc

            let linkOwnerId, linkProcess, _, templateLink =
                canonicalOwnerAndLink linkConverted.Session

            let forgedLink = {
                templateLink with
                    Id = "process-link:unjournalled"
            }

            let forgedLinkSession = {
                linkConverted.Session with
                    Processes =
                        linkConverted.Session.Processes
                        |> Map.add linkOwnerId {
                            linkProcess with
                                Links = linkProcess.Links |> Map.add forgedLink.Id forgedLink
                        }
            }

            let linkPayload = arcPayload linkFixture.Arc

            let linkErrors =
                canonicalWriteBackMany linkConverted.Index forgedLinkSession linkFixture.Arc
                |> expectError

            Expect.isNonEmpty linkErrors "An unjournalled exact link is refused."

            Expect.equal
                (arcPayload linkFixture.Arc)
                linkPayload
                "A refused exact-link check leaves the ARC byte-identical."

        testCase "a valid plan applies atomically"
        <| fun _ ->
            let fixture = canonicalApplyFixture ()
            let converted = convertCanonical [ canonicalLocation "stage-neutral" ] fixture.Arc
            let layerId = converted.Session.ActiveLayerId
            let inputNodeId = canonicalNodeIdByName "input-neutral" converted.Session

            let characteristicAssignment =
                converted.Session.Nodes[inputNodeId].Assignments
                |> Map.toList
                |> List.map snd
                |> List.exactlyOne

            let edited =
                CanonicalCommands.editNodeAssignment
                    inputNodeId
                    characteristicAssignment.Id
                    (canonicalContent "characteristic-neutral" "after")
                    converted.Session
                |> expectOk
                |> fun effect -> commitCanonical effect converted.Session

            let withEndpoint =
                addCanonicalEndpoint
                    layerId
                    CanonicalIdentifiers.ProvenanceSide.Output
                    ProcessCoreCanonicalKinds.dataEndpoint
                    "extra-output.dat"
                    9
                    edited

            let extraOutputId = canonicalNodeIdByName "extra-output.dat" withEndpoint

            let prepared =
                connectCanonicalNodes layerId [ inputNodeId, extraOutputId ] withEndpoint
                |> prepareCanonical

            let summary =
                canonicalWriteBackMany converted.Index prepared fixture.Arc |> expectOk

            Expect.equal fixture.Characteristic.Value (Some "after") "The loaded node annotation is updated in place."
            Expect.equal summary.AddedProcesses 1 "The new exact link materializes exactly one Process."
            Expect.equal summary.AddedNodes 1 "The new canonical node materializes exactly once."
            Expect.equal summary.RemovedProcesses 0 "No indexed Process becomes obsolete."

            Expect.isTrue
                (obj.ReferenceEquals(fixture.Process.ExecutesRecipe.Value, fixture.AssignedRecipe))
                "A retained association still points at the exact stored Recipe."

            Expect.equal fixture.Arc.Recipes.Count 2 "Applying a plan never grows the Recipe store."

            let shapes = processShapes fixture.Dataset

            Expect.contains shapes ([ "input-neutral" ], [ "output-neutral" ]) "The pre-existing exact link survives."

            Expect.contains shapes ([ "input-neutral" ], [ "extra-output.dat" ]) "The added exact link materializes."

            let rejectedFixture = canonicalApplyFixture ()

            let rejectedConverted =
                convertCanonical [ canonicalLocation "stage-neutral" ] rejectedFixture.Arc

            let rejectedNodeId = canonicalNodeIdByName "input-neutral" rejectedConverted.Session

            let rejectedAssignment =
                rejectedConverted.Session.Nodes[rejectedNodeId].Assignments
                |> Map.toList
                |> List.map snd
                |> List.exactlyOne

            let rejectedEdit =
                CanonicalCommands.editNodeAssignment
                    rejectedNodeId
                    rejectedAssignment.Id
                    (canonicalContent "characteristic-neutral" "after")
                    rejectedConverted.Session
                |> expectOk
                |> fun effect -> commitCanonical effect rejectedConverted.Session
                |> prepareCanonical

            let rejectedRecipeAssignment =
                rejectedConverted.Session.Processes
                |> Map.toList
                |> List.collect (fun (_, structuralProcess) ->
                    structuralProcess.Assignments |> Map.toList |> List.map snd
                )
                |> List.find (fun (assignment: CanonicalDomain.ProcessAssignment) -> assignment.ReferenceSlotId.IsSome)

            let unresolvableDefinition = {
                rejectedEdit.Values[rejectedRecipeAssignment.ValueId] with
                    Value =
                        CanonicalValues.ProvenanceValue.Reference {
                            Scheme = ProcessCoreCanonicalKinds.processCoreRecipeScheme
                            Id = "recipe:missing"
                            Label = "missing"
                        }
            }

            let rejected = {
                rejectedEdit with
                    Values = rejectedEdit.Values |> Map.add unresolvableDefinition.Id unresolvableDefinition
            }

            let rejectedPayload = arcPayload rejectedFixture.Arc

            canonicalWriteBackMany rejectedConverted.Index rejected rejectedFixture.Arc
            |> expectError
            |> ignore

            Expect.equal
                rejectedFixture.Characteristic.Value
                (Some "before")
                "A rejected plan applies none of its valid annotation changes."

            Expect.equal
                (arcPayload rejectedFixture.Arc)
                rejectedPayload
                "A rejected plan leaves the ARC byte-identical."

        testCase "a subset edit lands its retained and split annotations on their own processes"
        <| fun _ ->
            let fixture = canonicalApplyFixture ()
            let converted = convertCanonical [ canonicalLocation "stage-neutral" ] fixture.Arc

            let ownerId, structuralProcess, firstLinkId, _ =
                canonicalOwnerAndLink converted.Session

            let parameterAssignment =
                structuralProcess.Assignments
                |> Map.toList
                |> List.map snd
                |> List.find (fun (assignment: CanonicalDomain.ProcessAssignment) ->
                    assignment.ReferenceSlotId.IsNone && assignment.ContainerReferenceValueId.IsNone
                )

            let withLink = addParallelCanonicalLink "subset-edit-link" ownerId converted.Session

            let expanded =
                structuralProcess.Assignments
                |> Map.map (fun _ (assignment: CanonicalDomain.ProcessAssignment) -> {
                    assignment with
                        CoveredLinkIds = Set.ofList [ firstLinkId; "subset-edit-link" ]
                })

            let coverageContext =
                canonicalMutationContext (expanded |> Map.keys |> Set.ofSeq) (Set.singleton "subset-edit-link")

            let covered = {
                withLink with
                    Processes =
                        withLink.Processes
                        |> Map.change ownerId (Option.map (fun current -> { current with Assignments = expanded }))
                    MutationJournal =
                        withLink.MutationJournal
                        @ (structuralProcess.Assignments
                           |> Map.toList
                           |> List.map (fun (assignmentId, before) ->
                               CanonicalMutation.ProvenanceMutation.ProcessAssignmentCoverageChanged(
                                   ownerId,
                                   before,
                                   expanded[assignmentId],
                                   coverageContext
                               )
                           ))
            }

            let prepared =
                CanonicalCommands.editProcessAssignmentSubset
                    ownerId
                    parameterAssignment.Id
                    (Set.singleton "subset-edit-link")
                    (canonicalContent "parameter-neutral" "after")
                    covered
                |> expectOk
                |> fun effect -> commitCanonical effect covered

            let summary =
                canonicalWriteBackMany converted.Index prepared fixture.Arc |> expectOk

            Expect.equal summary.AddedProcesses 1 "The detached subset materializes its own Process."

            Expect.equal
                fixture.Parameter.Value
                (Some "before")
                "The retained assignment keeps the indexed annotation object and its value."

            let values =
                fixture.Dataset.Processes
                |> Seq.map (fun proc ->
                    proc.ParameterValue
                    |> Seq.filter (fun annotation -> annotation.Name = "parameter-neutral")
                    |> Seq.map _.Value
                    |> List.ofSeq
                )
                |> List.ofSeq

            Expect.sequenceEqual
                (values |> List.sort)
                [ [ Some "after" ]; [ Some "before" ] ]
                "Each partition carries exactly its own annotation occurrence."

        testCase "a new canonical node equal to an unloaded ARC node attaches to that node"
        <| fun _ ->
            let shared = Sample("shared-node")

            let loadedProcess =
                mkProcess "stage-neutral" [ SampleNode(Sample("input-neutral")) ] [
                    SampleNode(Sample("output-neutral"))
                ]

            let unloadedProcess = mkProcess "other-stage" [ SampleNode shared ] []

            let dataset =
                Dataset("dataset-neutral", processes = [ loadedProcess; unloadedProcess ])

            let arc = ARC("arc-neutral", hasPart = [ dataset ])
            let converted = convertCanonical [ canonicalLocation "stage-neutral" ] arc
            let nodeCount = arc.AllNodes().Count

            let prepared =
                addCanonicalEndpoint
                    converted.Session.ActiveLayerId
                    CanonicalIdentifiers.ProvenanceSide.Input
                    ProcessCoreCanonicalKinds.sampleEndpoint
                    "shared-node"
                    7
                    converted.Session
                |> prepareCanonical

            let summary = canonicalWriteBackMany converted.Index prepared arc |> expectOk

            Expect.equal summary.AddedNodes 0 "An equal-key node outside the loaded selection is reused."
            Expect.equal (arc.AllNodes().Count) nodeCount "No duplicate ProcessCore node is created."

            let attached =
                dataset.Processes
                |> Seq.filter (fun proc -> proc.Name = "stage-neutral")
                |> Seq.choose _.Input
                |> Seq.filter (fun node -> endpointName node = "shared-node")
                |> Seq.exactlyOne

            Expect.isTrue
                (obj.ReferenceEquals(attached.AsSample(), shared))
                "The materialized endpoint is the exact existing ProcessCore node."

        testCase "endpointless and annotation-free processes survive a no-op save"
        <| fun _ ->
            let endpointless = mkProcess "stage-neutral" [] []

            let connected =
                mkProcess "stage-neutral" [ SampleNode(Sample("input-neutral")) ] [
                    SampleNode(Sample("output-neutral"))
                ]

            let dataset = Dataset("dataset-neutral", processes = [ endpointless; connected ])

            let arc = ARC("arc-neutral", hasPart = [ dataset ])
            let converted = convertCanonical [ canonicalLocation "stage-neutral" ] arc
            let prepared = prepareCanonical converted.Session
            let summary = canonicalWriteBackMany converted.Index prepared arc |> expectOk

            Expect.equal summary.AddedProcesses 0 "A no-op save adds no Process."
            Expect.equal summary.RemovedProcesses 0 "A no-op save removes no Process."
            Expect.equal summary.UpdatedAnnotations 0 "A no-op save updates no annotation."
            Expect.equal dataset.Processes.Count 2 "Both original Processes survive."

            Expect.isTrue
                (dataset.Processes
                 |> Seq.exists (fun proc -> obj.ReferenceEquals(proc, endpointless)))
                "The endpointless Process is neither replaced nor removed."

            let reloaded = convertCanonical [ canonicalLocation "stage-neutral" ] arc

            Expect.isTrue
                (reloaded.Session.Processes
                 |> Map.exists (fun _ structuralProcess ->
                     structuralProcess.Links
                     |> Map.exists (fun _ link -> link.Shape = CanonicalValues.ProcessLinkShape.Endpointless)
                 ))
                "The endpointless relationship reloads as an endpointless canonical link."

        testCase "a node annotation is written once to the interned node and visible on every referencing process"
        <| fun _ ->
            let shared = Sample("shared-input")

            let first =
                mkProcess "stage-neutral" [ SampleNode shared ] [ SampleNode(Sample("output-one")) ]

            let second =
                mkProcess "stage-neutral" [ SampleNode shared ] [ SampleNode(Sample("output-two")) ]

            let dataset = Dataset("dataset-neutral", processes = [ first; second ])
            let arc = ARC("arc-neutral", hasPart = [ dataset ])
            let converted = convertCanonical [ canonicalLocation "stage-neutral" ] arc
            let sharedNodeId = canonicalNodeIdByName "shared-input" converted.Session

            let draft: CanonicalCommands.NodeAssignmentDraft = {
                Content = canonicalContent "interned-characteristic" "written-once"
                OwnerKind = CanonicalValues.AnnotationOwnerKind.Node
                PropertyKind = CanonicalValues.AssignmentPropertyKind.Generic
            }

            let prepared =
                CanonicalCommands.assignNodeValue
                    (Set.singleton sharedNodeId)
                    draft
                    CanonicalCommands.NoOverwrite
                    converted.Session
                |> expectOk
                |> fun effect -> commitCanonical effect converted.Session
                |> prepareCanonical

            let summary = canonicalWriteBackMany converted.Index prepared arc |> expectOk

            Expect.equal summary.AddedAnnotations 1 "The node annotation is written exactly once."

            Expect.equal
                (shared.AdditionalProperty
                 |> Seq.filter (fun annotation -> annotation.Name = "interned-characteristic")
                 |> Seq.length)
                1
                "The interned node carries exactly one occurrence."

            Expect.isTrue
                (obj.ReferenceEquals(first.Input.Value.AsSample(), second.Input.Value.AsSample()))
                "Both Processes reference the same interned node."

        testCase "characteristics, factors, parameters and components round-trip with their remembered kinds"
        <| fun _ ->
            let fixture = canonicalApplyFixture ()
            let converted = convertCanonical [ canonicalLocation "stage-neutral" ] fixture.Arc
            let outputNodeId = canonicalNodeIdByName "output-neutral" converted.Session
            let _, _, linkId, _ = canonicalOwnerAndLink converted.Session

            let factorDraft: CanonicalCommands.NodeAssignmentDraft = {
                Content = canonicalContent "generic-node-property" "generic-node-value"
                OwnerKind = CanonicalValues.AnnotationOwnerKind.Node
                PropertyKind = CanonicalValues.AssignmentPropertyKind.Generic
            }

            let withGenericNode =
                CanonicalCommands.assignNodeValue
                    (Set.singleton outputNodeId)
                    factorDraft
                    CanonicalCommands.NoOverwrite
                    converted.Session
                |> expectOk
                |> fun effect -> commitCanonical effect converted.Session

            let processDraft: CanonicalCommands.ProcessAssignmentDraft = {
                Content = canonicalContent "generic-process-property" "generic-process-value"
                OwnerKind = CanonicalValues.AnnotationOwnerKind.Process
                PropertyKind = CanonicalValues.AssignmentPropertyKind.Generic
                ContainerReferenceValueId = None
                ReferenceSlotId = None
                Lineage = CanonicalValues.AssignmentLineage.Created
            }

            let prepared =
                CanonicalCommands.assignProcessValue (Set.singleton linkId) processDraft withGenericNode
                |> expectOk
                |> fun effect -> commitCanonical effect withGenericNode
                |> prepareCanonical

            canonicalWriteBackMany converted.Index prepared fixture.Arc
            |> expectOk
            |> ignore

            let reloaded = convertCanonical [ canonicalLocation "stage-neutral" ] fixture.Arc

            let nodeKinds =
                reloaded.Session.Nodes
                |> Map.toList
                |> List.collect (fun (_, node) ->
                    node.Assignments
                    |> Map.toList
                    |> List.map (fun (_, assignment) ->
                        reloaded.Session.Properties[reloaded.Session.Values[assignment.ValueId].PropertyId]
                            .Category.Name,
                        assignment.PropertyKind
                    )
                )
                |> Map.ofList

            let processKinds =
                reloaded.Session.Processes
                |> Map.toList
                |> List.collect (fun (_, structuralProcess) ->
                    structuralProcess.Assignments
                    |> Map.toList
                    |> List.map (fun (_, assignment) ->
                        reloaded.Session.Properties[reloaded.Session.Values[assignment.ValueId].PropertyId]
                            .Category.Name,
                        assignment.PropertyKind
                    )
                )
                |> Map.ofList

            Expect.equal
                nodeKinds["characteristic-neutral"]
                (CanonicalValues.AssignmentPropertyKind.AdapterSpecific ProcessCoreCanonicalKinds.characteristic)
                "A loaded characteristic reloads with its remembered concrete kind."

            Expect.equal
                nodeKinds["factor-neutral"]
                (CanonicalValues.AssignmentPropertyKind.AdapterSpecific ProcessCoreCanonicalKinds.factor)
                "A loaded factor reloads with its remembered concrete kind."

            Expect.equal
                nodeKinds["generic-node-property"]
                CanonicalValues.AssignmentPropertyKind.Generic
                "A generic node property reloads as the same generic kind."

            Expect.equal
                processKinds["parameter-neutral"]
                (CanonicalValues.AssignmentPropertyKind.AdapterSpecific ProcessCoreCanonicalKinds.parameter)
                "A loaded parameter reloads with its remembered concrete kind."

            Expect.equal
                processKinds["generic-process-property"]
                CanonicalValues.AssignmentPropertyKind.Generic
                "A generic process property reloads as the same generic kind."

            Expect.equal
                processKinds["component"]
                (CanonicalValues.AssignmentPropertyKind.AdapterSpecific ProcessCoreCanonicalKinds.componentKind)
                "A Recipe Component reloads with its remembered concrete kind."

            Expect.equal
                processKinds["Recipe"]
                (CanonicalValues.AssignmentPropertyKind.AdapterSpecific ProcessCoreCanonicalKinds.processCoreRecipeKind)
                "The Recipe reference reloads with its remembered concrete kind."

        testCase "a recipe association writes the exact indexed resource, never a label match"
        <| fun _ ->
            let arc, _, processObject, first, second = recipeFixture false
            let converted = convertCanonical [ canonicalLocation "stage-neutral" ] arc
            let _, _, linkId, _ = canonicalOwnerAndLink converted.Session

            let prepared =
                assignCanonicalRecipe
                    (Set.singleton linkId)
                    (recipeEntryFor second converted)
                    converted.ReferenceCatalog
                    converted.Session
                |> prepareCanonical

            canonicalWriteBackMany converted.Index prepared arc |> expectOk |> ignore

            Expect.isTrue
                (obj.ReferenceEquals(processObject.ExecutesRecipe.Value, second))
                "The association points at the exact indexed resource."

            Expect.isFalse
                (obj.ReferenceEquals(processObject.ExecutesRecipe.Value, first))
                "An equal Recipe label must never decide resolution."

            Expect.equal arc.Recipes.Count 2 "Assignment adds no Recipe resource."

        testCase "a new process can be assigned an existing recipe"
        <| fun _ ->
            let arc, dataset, _, _, second = recipeFixture false
            let converted = convertCanonical [ canonicalLocation "stage-neutral" ] arc
            let layerId = converted.Session.ActiveLayerId
            let inputNodeId = canonicalNodeIdByName "input-neutral" converted.Session

            let withEndpoint =
                addCanonicalEndpoint
                    layerId
                    CanonicalIdentifiers.ProvenanceSide.Output
                    ProcessCoreCanonicalKinds.sampleEndpoint
                    "added-output"
                    4
                    converted.Session

            let addedOutputId = canonicalNodeIdByName "added-output" withEndpoint

            let connected =
                connectCanonicalNodes layerId [ inputNodeId, addedOutputId ] withEndpoint

            let addedLinkId =
                connected.Processes
                |> Map.toList
                |> List.collect (fun (_, structuralProcess) -> structuralProcess.Links |> Map.toList)
                |> List.find (fun (_, link) ->
                    link.Shape = CanonicalValues.ProcessLinkShape.Between(inputNodeId, addedOutputId)
                )
                |> fst

            let prepared =
                assignCanonicalRecipe
                    (Set.singleton addedLinkId)
                    (recipeEntryFor second converted)
                    converted.ReferenceCatalog
                    connected
                |> prepareCanonical

            let recipeCount = arc.Recipes.Count

            canonicalWriteBackMany converted.Index prepared arc |> expectOk |> ignore

            let addedProcess =
                dataset.Processes
                |> Seq.filter (fun proc ->
                    proc.Output |> Option.exists (fun node -> endpointName node = "added-output")
                )
                |> Seq.exactlyOne

            Expect.isTrue
                (obj.ReferenceEquals(addedProcess.ExecutesRecipe.Value, second))
                "The new Process references the exact stored resource."

            Expect.equal arc.Recipes.Count recipeCount "A new Process never copies a Recipe resource."

        testCase "a split process reuses the original stored recipe"
        <| fun _ ->
            let arc, dataset, processObject, first, _ = recipeFixture true
            let converted = convertCanonical [ canonicalLocation "stage-neutral" ] arc

            let ownerId, structuralProcess, firstLinkId, _ =
                canonicalOwnerAndLink converted.Session

            let withLink =
                addParallelCanonicalLink "recipe-apply-split-link" ownerId converted.Session

            let expandedAssignments =
                structuralProcess.Assignments
                |> Map.map (fun _ (assignment: CanonicalDomain.ProcessAssignment) -> {
                    assignment with
                        CoveredLinkIds = Set.ofList [ firstLinkId; "recipe-apply-split-link" ]
                })

            let coverageContext =
                canonicalMutationContext
                    (expandedAssignments |> Map.keys |> Set.ofSeq)
                    (Set.singleton "recipe-apply-split-link")

            let coverageJournal =
                structuralProcess.Assignments
                |> Map.toList
                |> List.map (fun (assignmentId, before) ->
                    CanonicalMutation.ProvenanceMutation.ProcessAssignmentCoverageChanged(
                        ownerId,
                        before,
                        expandedAssignments[assignmentId],
                        coverageContext
                    )
                )

            let session = {
                withLink with
                    Processes =
                        withLink.Processes
                        |> Map.change
                            ownerId
                            (Option.map (fun current -> {
                                current with
                                    Assignments = expandedAssignments
                            }))
                    MutationJournal = withLink.MutationJournal @ coverageJournal
            }

            let recipeCount = arc.Recipes.Count
            let firstPayload = recipePayload first

            let summary = canonicalWriteBackMany converted.Index session arc |> expectOk

            Expect.equal summary.AddedProcesses 1 "The second exact link materializes one additional Process."
            Expect.equal dataset.Processes.Count 2 "The split emits exactly two Processes."

            for proc in dataset.Processes do
                Expect.isTrue
                    (obj.ReferenceEquals(proc.ExecutesRecipe.Value, first))
                    "Every emitted Process references the exact stored Recipe."

            Expect.isTrue
                (dataset.Processes
                 |> Seq.exists (fun proc -> obj.ReferenceEquals(proc, processObject)))
                "The indexed Process itself is retained by the split."

            Expect.equal arc.Recipes.Count recipeCount "A split never copies a Recipe resource."
            Expect.equal (recipePayload first) firstPayload "The stored Recipe payload is untouched."

        testCase "replacing a recipe association swaps only the reference"
        <| fun _ ->
            let arc, _, processObject, first, second = recipeFixture true
            let converted = convertCanonical [ canonicalLocation "stage-neutral" ] arc
            let _, _, linkId, _ = canonicalOwnerAndLink converted.Session
            let firstPayload = recipePayload first
            let secondPayload = recipePayload second

            let prepared =
                assignCanonicalRecipe
                    (Set.singleton linkId)
                    (recipeEntryFor second converted)
                    converted.ReferenceCatalog
                    converted.Session
                |> prepareCanonical

            canonicalWriteBackMany converted.Index prepared arc |> expectOk |> ignore

            Expect.isTrue
                (obj.ReferenceEquals(processObject.ExecutesRecipe.Value, second))
                "Only the association changes."

            Expect.equal (recipePayload first) firstPayload "The replaced Recipe payload is byte-identical."
            Expect.equal (recipePayload second) secondPayload "The replacement Recipe payload is byte-identical."
            Expect.equal arc.Recipes.Count 2 "Replacement adds no Recipe resource."

        testCase "removing a recipe association clears only the association"
        <| fun _ ->
            let arc, _, processObject, first, _ = recipeFixture true
            let converted = convertCanonical [ canonicalLocation "stage-neutral" ] arc
            let ownerId, structuralProcess, linkId, _ = canonicalOwnerAndLink converted.Session
            let firstPayload = recipePayload first

            let recipeAssignment =
                structuralProcess.Assignments
                |> Map.toList
                |> List.map snd
                |> List.find (fun (assignment: CanonicalDomain.ProcessAssignment) -> assignment.ReferenceSlotId.IsSome)

            let prepared =
                CanonicalCommands.removeProcessAssignmentLinks
                    ownerId
                    recipeAssignment.Id
                    (Set.singleton linkId)
                    converted.Session
                |> expectOk
                |> fun effect -> commitCanonical effect converted.Session
                |> prepareCanonical

            canonicalWriteBackMany converted.Index prepared arc |> expectOk |> ignore

            Expect.isNone processObject.ExecutesRecipe "The association is cleared."

            Expect.isTrue
                (arc.Recipes |> Seq.exists (fun recipe -> obj.ReferenceEquals(recipe, first)))
                "The detached Recipe remains stored."

            Expect.equal (recipePayload first) firstPayload "The detached Recipe payload is byte-identical."

        testCase "unassigned stored recipes survive no-op and association-removal saves"
        <| fun _ ->
            let arc, _, _, first, second = recipeFixture true
            let converted = convertCanonical [ canonicalLocation "stage-neutral" ] arc

            canonicalWriteBackMany converted.Index (prepareCanonical converted.Session) arc
            |> expectOk
            |> ignore

            Expect.equal arc.Recipes.Count 2 "A no-op save retains every stored Recipe."

            let afterNoOp = convertCanonical [ canonicalLocation "stage-neutral" ] arc

            let ownerId, structuralProcess, linkId, _ = canonicalOwnerAndLink afterNoOp.Session

            let recipeAssignment =
                structuralProcess.Assignments
                |> Map.toList
                |> List.map snd
                |> List.find (fun (assignment: CanonicalDomain.ProcessAssignment) -> assignment.ReferenceSlotId.IsSome)

            let detached =
                CanonicalCommands.removeProcessAssignmentLinks
                    ownerId
                    recipeAssignment.Id
                    (Set.singleton linkId)
                    afterNoOp.Session
                |> expectOk
                |> fun effect -> commitCanonical effect afterNoOp.Session
                |> prepareCanonical

            canonicalWriteBackMany afterNoOp.Index detached arc |> expectOk |> ignore

            Expect.equal arc.Recipes.Count 2 "Detachment retains every stored Recipe."

            let reloaded = convertCanonical [ canonicalLocation "stage-neutral" ] arc

            for recipe in [ first; second ] do
                let key =
                    Swate.Components.ProcessCore.Copy.RecipeResourceKey.ofRecipeStableString recipe

                Expect.isTrue
                    (reloaded.ReferenceCatalog.ContainsKey(ProcessCoreCanonicalKinds.processCoreRecipeScheme, key))
                    "Every stored Recipe stays catalog-available after reload."

        testCase "component and recipe-resource edits are rejected without mutation"
        <| fun _ ->
            let arc, _, _, first, _ = recipeFixture true
            let converted = convertCanonical [ canonicalLocation "stage-neutral" ] arc
            let _, structuralProcess, _, _ = canonicalOwnerAndLink converted.Session

            let componentAssignment =
                structuralProcess.Assignments
                |> Map.toList
                |> List.map snd
                |> List.find (fun (assignment: CanonicalDomain.ProcessAssignment) ->
                    assignment.ContainerReferenceValueId.IsSome
                )

            let before = converted.Session.Values[componentAssignment.ValueId]

            let after = {
                before with
                    Value = CanonicalValues.ProvenanceValue.Text "forged-component"
            }

            let forged = {
                converted.Session with
                    Values = converted.Session.Values |> Map.add after.Id after
                    MutationJournal =
                        converted.Session.MutationJournal
                        @ [
                            CanonicalMutation.ProvenanceMutation.PropertyValueDefinitionUpdated(
                                before,
                                after,
                                canonicalMutationContext
                                    (Set.singleton componentAssignment.Id)
                                    componentAssignment.CoveredLinkIds
                            )
                        ]
            }

            let payload = arcPayload arc
            let firstPayload = recipePayload first

            let errors = canonicalWriteBackMany converted.Index forged arc |> expectError

            Expect.isTrue
                (errors
                 |> List.exists (
                     function
                     | ProcessCoreCanonicalWritebackError.ReadOnlyRecipeComponentMutation _ -> true
                     | _ -> false
                 ))
                "A Component edit cannot reach apply."

            Expect.equal (arcPayload arc) payload "A rejected Component edit leaves the ARC byte-identical."
            Expect.equal (recipePayload first) firstPayload "A rejected Component edit leaves the Recipe untouched."

        testCase "repeated assignment replacement and detachment never grow the Recipe store"
        <| fun _ ->
            let arc, _, _, first, second = recipeFixture false
            let expected = arc.Recipes.Count

            let assign (recipe: Recipe) =
                let converted = convertCanonical [ canonicalLocation "stage-neutral" ] arc
                let _, _, linkId, _ = canonicalOwnerAndLink converted.Session

                let prepared =
                    assignCanonicalRecipe
                        (Set.singleton linkId)
                        (recipeEntryFor recipe converted)
                        converted.ReferenceCatalog
                        converted.Session
                    |> prepareCanonical

                canonicalWriteBackMany converted.Index prepared arc |> expectOk |> ignore
                Expect.equal arc.Recipes.Count expected "Assignment never grows the Recipe store."

            let detach () =
                let converted = convertCanonical [ canonicalLocation "stage-neutral" ] arc
                let ownerId, structuralProcess, linkId, _ = canonicalOwnerAndLink converted.Session

                let recipeAssignment =
                    structuralProcess.Assignments
                    |> Map.toList
                    |> List.map snd
                    |> List.find (fun (assignment: CanonicalDomain.ProcessAssignment) ->
                        assignment.ReferenceSlotId.IsSome
                    )

                let prepared =
                    CanonicalCommands.removeProcessAssignmentLinks
                        ownerId
                        recipeAssignment.Id
                        (Set.singleton linkId)
                        converted.Session
                    |> expectOk
                    |> fun effect -> commitCanonical effect converted.Session
                    |> prepareCanonical

                canonicalWriteBackMany converted.Index prepared arc |> expectOk |> ignore
                Expect.equal arc.Recipes.Count expected "Detachment never grows the Recipe store."

            assign first
            assign second
            detach ()
            assign first

            Expect.equal arc.Recipes.Count expected "Repeated provenance grouping saves create no Recipe versions."

        testCase "two components differing only in ValueTAN, UnitTAN or AdditionalType survive decode and write"
        <| fun _ ->
            let componentAnnotation identity annotation =
                (annotation: Annotation).SetProperty("@id", identity)
                annotation

            let plain =
                componentAnnotation
                    "annotation:component-plain"
                    (Annotation("component", value = "shared", additionalType = "Component"))

            let valueAnnotated =
                componentAnnotation
                    "annotation:component-value-tan"
                    (Annotation("component", value = "shared", valueTAN = "term:shared", additionalType = "Component"))

            let unitOnly =
                componentAnnotation
                    "annotation:component-unit"
                    (Annotation("component", value = "shared", unit = "unit-neutral", additionalType = "Component"))

            let unitAnnotated =
                componentAnnotation
                    "annotation:component-unit-tan"
                    (Annotation(
                        "component",
                        value = "shared",
                        unit = "unit-neutral",
                        unitTAN = "term:unit",
                        additionalType = "Component"
                    ))

            let recipe = Recipe(name = "component-variants", version = "1")
            recipe.SetProperty("@id", "recipe:component-variants")

            // The published `Recipe.AddComponent` - and the published Recipe YAML
            // decoder with it - drops an occurrence that is `Annotation.Equals` to
            // one already present, and that equality ignores ValueTAN, UnitTAN and
            // AdditionalType. Seeding the exposed collection directly is the only
            // way to present ArcEditor with all four distinguishable occurrences.
            for annotation in [ plain; valueAnnotated; unitOnly; unitAnnotated ] do
                recipe.Components.Add annotation

            let processObject =
                mkProcessFull "stage-neutral" (Some recipe) [ SampleNode(Sample("input-neutral")) ] [
                    SampleNode(Sample("output-neutral"))
                ] []

            let dataset = Dataset("dataset-neutral", processes = [ processObject ])
            let arc = ARC("arc-neutral", hasPart = [ dataset ])
            arc.AddRecipe recipe

            let payloads = recipe.Components |> Seq.map annotationPayload |> List.ofSeq

            let converted = convertCanonical [ canonicalLocation "stage-neutral" ] arc

            canonicalWriteBackMany converted.Index (prepareCanonical converted.Session) arc
            |> expectOk
            |> ignore

            Expect.equal recipe.Components.Count 4 "Every distinguishable Component survives the write."

            Expect.sequenceEqual
                (recipe.Components |> Seq.map annotationPayload |> List.ofSeq)
                payloads
                "No Component is collapsed onto another by narrower ProcessCore equality."

            let reloaded = convertCanonical [ canonicalLocation "stage-neutral" ] arc

            let componentAssignments =
                reloaded.Session.Processes
                |> Map.toList
                |> List.collect (fun (_, structuralProcess) ->
                    structuralProcess.Assignments
                    |> Map.toList
                    |> List.map snd
                    |> List.filter (fun assignment -> assignment.ContainerReferenceValueId.IsSome)
                )

            Expect.hasLength componentAssignments 4 "Each Component occurrence reloads as its own assignment."

        testCase "two parameters differing only in NameTAN or DefaultValue survive"
        <| fun _ ->
            let parameterAnnotations identify =
                let identified identity (annotation: Annotation) =
                    if identify then
                        annotation.SetProperty("@id", identity)

                    annotation

                let defaulted identity defaultName =
                    let instance =
                        FormalParameter(
                            "parameter",
                            defaultValue = DefinedTerm(defaultName, tan = $"term:{defaultName}")
                        )

                    identified
                        identity
                        (Annotation(
                            "parameter",
                            value = "shared",
                            additionalType = "ParameterValue",
                            instanceOf = instance
                        ))

                [
                    identified
                        "annotation:parameter-plain"
                        (Annotation("parameter", value = "shared", additionalType = "ParameterValue"))

                    identified
                        "annotation:parameter-name-tan"
                        (Annotation(
                            "parameter",
                            value = "shared",
                            nameTAN = "term:parameter",
                            additionalType = "ParameterValue"
                        ))

                    defaulted "annotation:parameter-default-one" "default-one"
                    defaulted "annotation:parameter-default-two" "default-two"
                ]

            let fixtureFor parameters =
                let processObject =
                    mkProcessFull
                        "stage-neutral"
                        None
                        [ SampleNode(Sample("input-neutral")) ]
                        [ SampleNode(Sample("output-neutral")) ]
                        parameters

                let dataset = Dataset("dataset-neutral", processes = [ processObject ])
                ARC("arc-neutral", hasPart = [ dataset ]), processObject

            // Without a distinguishing identity these occupy one ProcessCore
            // registry identity while carrying divergent content, and nothing in
            // this operation controls them - so the save is refused rather than
            // silently collapsing one onto the other.
            let collidingArc, collidingProcess = fixtureFor (parameterAnnotations false)

            let collidingConverted =
                convertCanonical [ canonicalLocation "stage-neutral" ] collidingArc

            let collidingPayload = arcPayload collidingArc

            let collisionErrors =
                canonicalWriteBackMany
                    collidingConverted.Index
                    (prepareCanonical collidingConverted.Session)
                    collidingArc
                |> expectError

            Expect.isTrue
                (collisionErrors
                 |> List.exists (
                     function
                     | ProcessCoreCanonicalWritebackError.ConflictingAnnotationIdentity _ -> true
                     | _ -> false
                 ))
                "An unresolvable registry-identity collision is refused."

            Expect.equal collidingProcess.ParameterValue.Count 4 "The refused save drops no parameter."
            Expect.equal (arcPayload collidingArc) collidingPayload "The refused save leaves the ARC byte-identical."

            let arc, processObject = fixtureFor (parameterAnnotations true)

            let payloads =
                processObject.ParameterValue |> Seq.map annotationPayload |> List.ofSeq

            let converted = convertCanonical [ canonicalLocation "stage-neutral" ] arc

            canonicalWriteBackMany converted.Index (prepareCanonical converted.Session) arc
            |> expectOk
            |> ignore

            Expect.equal processObject.ParameterValue.Count 4 "Every distinguishable parameter survives the write."

            Expect.sequenceEqual
                (processObject.ParameterValue |> Seq.map annotationPayload |> List.ofSeq)
                payloads
                "No parameter is collapsed onto another."

            Expect.equal
                (processObject.ParameterValue
                 |> Seq.choose (fun annotation -> annotation.InstanceOf)
                 |> Seq.choose (fun instance -> instance.DefaultValue)
                 |> Seq.map (fun term -> term.Name)
                 |> Set.ofSeq)
                (Set.ofList [ "default-one"; "default-two" ])
                "Nested default values survive the write."

        testCase "annotations differing only in InstanceOf payload or overflow are not fingerprint-equal"
        <| fun _ ->
            let instance name =
                let parameter = FormalParameter("nested", nameTAN = "term:nested")
                parameter.SetProperty("instance-overflow", name)
                parameter

            let firstInstance =
                Annotation(
                    "parameter",
                    value = "shared",
                    additionalType = "ParameterValue",
                    instanceOf = instance "one"
                )

            let secondInstance =
                Annotation(
                    "parameter",
                    value = "shared",
                    additionalType = "ParameterValue",
                    instanceOf = instance "two"
                )

            let firstOverflow =
                Annotation("parameter", value = "shared", additionalType = "ParameterValue")

            firstOverflow.SetProperty("annotation-overflow", "one")

            let secondOverflow =
                Annotation("parameter", value = "shared", additionalType = "ParameterValue")

            secondOverflow.SetProperty("annotation-overflow", "two")

            Expect.notEqual
                (CanonicalGraph.canonicalAnnotationFingerprint firstInstance)
                (CanonicalGraph.canonicalAnnotationFingerprint secondInstance)
                "A differing InstanceOf payload is not fingerprint-equal."

            Expect.notEqual
                (CanonicalGraph.canonicalAnnotationFingerprint firstOverflow)
                (CanonicalGraph.canonicalAnnotationFingerprint secondOverflow)
                "A differing overflow field is not fingerprint-equal."

            firstInstance.SetProperty("@id", "annotation:instance-one")
            secondInstance.SetProperty("@id", "annotation:instance-two")
            firstOverflow.SetProperty("@id", "annotation:overflow-one")
            secondOverflow.SetProperty("@id", "annotation:overflow-two")

            let processObject =
                mkProcessFull "stage-neutral" None [ SampleNode(Sample("input-neutral")) ] [
                    SampleNode(Sample("output-neutral"))
                ] [
                    firstInstance
                    secondInstance
                    firstOverflow
                    secondOverflow
                ]

            let dataset = Dataset("dataset-neutral", processes = [ processObject ])
            let arc = ARC("arc-neutral", hasPart = [ dataset ])
            let converted = convertCanonical [ canonicalLocation "stage-neutral" ] arc

            canonicalWriteBackMany converted.Index (prepareCanonical converted.Session) arc
            |> expectOk
            |> ignore

            Expect.equal
                (processObject.ParameterValue
                 |> Seq.map (fun annotation -> (CanonicalGraph.canonicalAnnotationFingerprint annotation).Payload)
                 |> Set.ofSeq
                 |> Set.count)
                4
                "Every distinct fingerprint survives the write as its own occurrence."

        testCase
            "every projected availability reference needed for materialization resolves to an originating assignment"
        <| fun _ ->
            let fixture = canonicalApplyFixture ()
            let converted = convertCanonical [ canonicalLocation "stage-neutral" ] fixture.Arc
            let prepared = prepareCanonical converted.Session
            let layerId = prepared.ActiveLayerId
            let projection = prepared.LayerProjections[layerId]

            let group =
                projection.Groups
                |> List.find (fun candidate -> not candidate.Annotations.IsEmpty)

            let projected = group.Annotations |> List.head

            let forgedBacking =
                match projected.Backing with
                | CanonicalProjectionTypes.NodeAssignmentBacking(identity, ownerId, targetSource) ->
                    CanonicalProjectionTypes.NodeAssignmentBacking(
                        {
                            identity with
                                AssignmentId = "assignment:unresolvable"
                        },
                        ownerId,
                        targetSource
                    )
                | CanonicalProjectionTypes.ProcessAssignmentBacking(identity, ownerId, linkIds, container, slot) ->
                    CanonicalProjectionTypes.ProcessAssignmentBacking(
                        {
                            identity with
                                AssignmentId = "assignment:unresolvable"
                        },
                        ownerId,
                        linkIds,
                        container,
                        slot
                    )

            let forgedSession = {
                prepared with
                    LayerProjections =
                        prepared.LayerProjections
                        |> Map.add layerId {
                            projection with
                                Groups =
                                    projection.Groups
                                    |> List.map (fun candidate ->
                                        if candidate.Id = group.Id then
                                            {
                                                candidate with
                                                    Annotations = [
                                                        {
                                                            projected with
                                                                Backing = forgedBacking
                                                        }
                                                    ]
                                            }
                                        else
                                            candidate
                                    )
                        }
            }

            let payload = arcPayload fixture.Arc

            let errors =
                canonicalWriteBackMany converted.Index forgedSession fixture.Arc |> expectError

            Expect.contains
                errors
                (ProcessCoreCanonicalWritebackError.AssignmentNotFound "assignment:unresolvable")
                "A projected reference with no originating assignment fails preflight."

            Expect.equal (arcPayload fixture.Arc) payload "A failed availability check leaves the ARC byte-identical."

        testCase "an unsupported endpoint kind or new assignment mapping fails preflight"
        <| fun _ ->
            let kindFixture = canonicalApplyFixture ()

            let kindConverted =
                convertCanonical [ canonicalLocation "stage-neutral" ] kindFixture.Arc

            let foreignKind: CanonicalIdentifiers.ProvenanceKind = {
                Id = "foreign:endpoint:unsupported"
                Label = "Unsupported"
            }

            let withForeignEndpoint =
                addCanonicalEndpoint
                    kindConverted.Session.ActiveLayerId
                    CanonicalIdentifiers.ProvenanceSide.Input
                    foreignKind
                    "foreign-endpoint"
                    9
                    kindConverted.Session
                |> prepareCanonical

            let kindPayload = arcPayload kindFixture.Arc

            let kindErrors =
                canonicalWriteBackMany kindConverted.Index withForeignEndpoint kindFixture.Arc
                |> expectError

            Expect.contains
                kindErrors
                (ProcessCoreCanonicalWritebackError.UnsupportedEndpointKind foreignKind.Id)
                "An unsupported endpoint kind fails preflight."

            Expect.equal
                (arcPayload kindFixture.Arc)
                kindPayload
                "A rejected endpoint kind leaves the ARC byte-identical."

            let mappingFixture = canonicalApplyFixture ()

            let mappingConverted =
                convertCanonical [ canonicalLocation "stage-neutral" ] mappingFixture.Arc

            let _, _, mappingLinkId, _ = canonicalOwnerAndLink mappingConverted.Session
            let mappingOwnerId, _, _, _ = canonicalOwnerAndLink mappingConverted.Session

            let foreignPropertyKind: CanonicalIdentifiers.ProvenanceKind = {
                Id = "foreign:property:unsupported"
                Label = "Unsupported property"
            }

            let unmapped =
                mappingConverted.Session
                |> addCanonicalProcessValue
                    "assignment:unsupported-mapping"
                    "unsupported-property"
                    (CanonicalValues.ProvenanceValue.Text "unsupported")
                    (CanonicalValues.AssignmentPropertyKind.AdapterSpecific foreignPropertyKind)
                    (Set.singleton mappingLinkId)
                    mappingOwnerId

            let mappingPayload = arcPayload mappingFixture.Arc

            let mappingErrors =
                canonicalWriteBackMany mappingConverted.Index unmapped mappingFixture.Arc
                |> expectError

            Expect.contains
                mappingErrors
                (ProcessCoreCanonicalWritebackError.UnsupportedPropertyKind foreignPropertyKind.Id)
                "An unsupported assignment mapping fails preflight."

            Expect.equal
                (arcPayload mappingFixture.Arc)
                mappingPayload
                "A rejected assignment mapping leaves the ARC byte-identical."

        testCase "writeBackMany called directly with an unprepared session is refused"
        <| fun _ ->
            // A command commit refreshes only the active layer, so an unprepared
            // multi-layer session still carries an unresolved invalidation.
            let characteristic =
                Annotation("characteristic-neutral", value = "before", additionalType = "CharacteristicValue")

            let stageOneInput = Sample("stage-one-input")
            stageOneInput.AddAdditionalProperty characteristic

            let stageOne =
                mkProcess "stage-one" [ SampleNode stageOneInput ] [ SampleNode(Sample("stage-one-output")) ]

            let stageTwo =
                mkProcess "stage-two" [ SampleNode(Sample("stage-two-input")) ] [
                    SampleNode(Sample("stage-two-output"))
                ]

            let dataset = Dataset("dataset-neutral", processes = [ stageOne; stageTwo ])
            let arc = ARC("arc-neutral", hasPart = [ dataset ])

            let converted =
                convertCanonical
                    [
                        canonicalLocation "stage-one"
                        canonicalLocation "stage-two"
                    ]
                    arc

            let inputNodeId = canonicalNodeIdByName "stage-one-input" converted.Session

            let assignment =
                converted.Session.Nodes[inputNodeId].Assignments
                |> Map.toList
                |> List.map snd
                |> List.exactlyOne

            let unprepared =
                CanonicalCommands.editNodeAssignment
                    inputNodeId
                    assignment.Id
                    (canonicalContent "characteristic-neutral" "after")
                    converted.Session
                |> expectOk
                |> fun effect -> commitCanonical effect converted.Session

            let payload = arcPayload arc

            let directErrors =
                canonicalWriteBackMany converted.Index unprepared arc |> expectError

            Expect.isTrue
                (directErrors
                 |> List.exists (
                     function
                     | ProcessCoreCanonicalWritebackError.InvalidPreparedState _ -> true
                     | _ -> false
                 ))
                "writeBackMany refuses a session with unresolved projection invalidations."

            let prepareErrors =
                prepareCanonicalWriteBackMany converted.Index unprepared arc |> expectError

            Expect.isTrue
                (prepareErrors
                 |> List.exists (
                     function
                     | ProcessCoreCanonicalWritebackError.InvalidPreparedState _ -> true
                     | _ -> false
                 ))
                "prepareWriteBackMany refuses the same session, so no caller can bypass preparation."

            Expect.equal characteristic.Value (Some "before") "An unprepared session applies nothing."
            Expect.equal (arcPayload arc) payload "An unprepared session leaves the ARC byte-identical."

        testCase "a malformed stored recipe resource is refused without mutation"
        <| fun _ ->
            let arc, _, _, _, _ = recipeFixture true
            let converted = convertCanonical [ canonicalLocation "stage-neutral" ] arc

            let malformedIndex = {
                converted.Index with
                    RecipeResources =
                        converted.Index.RecipeResources
                        |> Map.map (fun _ resource -> {
                            resource with
                                Resource = Unchecked.defaultof<Recipe>
                        })
            }

            let payload = arcPayload arc

            let errors =
                canonicalWriteBackMany malformedIndex (prepareCanonical converted.Session) arc
                |> expectError

            Expect.isNonEmpty errors "A malformed stored payload returns Error instead of throwing."
            Expect.equal (arcPayload arc) payload "A malformed stored payload leaves the ARC byte-identical."

        testCase "a float value is written with invariant culture"
        <| fun _ ->
            let fixture = canonicalApplyFixture ()

            let converted = convertCanonical [ canonicalLocation "stage-neutral" ] fixture.Arc

            let parameterAssignment =
                converted.Session.Processes
                |> Map.toList
                |> List.collect (fun (_, structuralProcess) ->
                    structuralProcess.Assignments |> Map.toList |> List.map snd
                )
                |> List.find (fun assignment ->
                    let definition = converted.Session.Values[assignment.ValueId]
                    converted.Session.Properties[definition.PropertyId].Category.Name = "parameter-neutral"
                )

            let edited =
                CanonicalCommands.editValueGlobally
                    parameterAssignment.ValueId
                    {
                        Category = {
                            Name = "parameter-neutral"
                            TermSource = None
                            TermAccession = None
                        }
                        Value = CanonicalValues.ProvenanceValue.Float 1.5
                        Unit = None
                    }
                    converted.Session
                |> expectOk
                |> fun effect -> commitCanonical effect converted.Session
                |> prepareCanonical

            let originalCulture = System.Globalization.CultureInfo.CurrentCulture

            try
                System.Globalization.CultureInfo.CurrentCulture <- System.Globalization.CultureInfo("de-DE")

                canonicalWriteBackMany converted.Index edited fixture.Arc |> expectOk |> ignore

                Expect.equal
                    fixture.Parameter.Value
                    (Some "1.5")
                    "A float must use an invariant decimal separator regardless of the ambient culture."
            finally
                System.Globalization.CultureInfo.CurrentCulture <- originalCulture

        testCase "a sample name keeps its hash while a data name splits into path and selector"
        <| fun _ ->
            let fixture = canonicalApplyFixture ()

            let converted = convertCanonical [ canonicalLocation "stage-neutral" ] fixture.Arc

            let layerId = converted.Session.ActiveLayerId

            let withEndpoints =
                converted.Session
                |> addCanonicalEndpoint
                    layerId
                    CanonicalIdentifiers.ProvenanceSide.Output
                    ProcessCoreCanonicalKinds.sampleEndpoint
                    "sample#name"
                    7
                |> addCanonicalEndpoint
                    layerId
                    CanonicalIdentifiers.ProvenanceSide.Output
                    ProcessCoreCanonicalKinds.dataEndpoint
                    "table.csv#row=1"
                    8
                |> prepareCanonical

            canonicalWriteBackMany converted.Index withEndpoints fixture.Arc
            |> expectOk
            |> ignore

            let writtenNodes =
                fixture.Dataset.Processes
                |> Seq.collect (fun proc -> proc.Output |> Option.toList)
                |> List.ofSeq

            Expect.isTrue
                (writtenNodes
                 |> List.exists (
                     function
                     | SampleNode sample -> sample.Name = "sample#name"
                     | _ -> false
                 ))
                "A hash is an opaque part of a ProcessCore sample name."

            Expect.isTrue
                (writtenNodes
                 |> List.exists (
                     function
                     | DataNode data -> data.Path = "table.csv" && data.Selector = Some "row=1"
                     | _ -> false
                 ))
                "A data endpoint name splits at its final hash into path and selector."

            // The identity must survive the round trip, not merely the write.
            let reconverted = convertCanonical [ canonicalLocation "stage-neutral" ] fixture.Arc

            let reloadedNames =
                reconverted.Session.Nodes
                |> Map.toList
                |> List.map (fun (_, node) -> node.Key.Name)
                |> Set.ofList

            Expect.contains reloadedNames "sample#name" "The sample identity must reload unchanged."
            Expect.contains reloadedNames "table.csv#row=1" "The data identity must reload with its selector."
    ]

let tests =
    testList "ProcessCore writeback" [ canonicalPlanTests; canonicalApplyTests ]

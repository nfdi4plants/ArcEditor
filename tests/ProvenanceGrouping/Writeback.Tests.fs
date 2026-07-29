module ProcessCoreWritebackTests

open Expecto
open ProcessCore
open ProcessCoreProvenanceFixtures
open Swate.Components.Page.ProvenanceGrouping.ProvenanceTypes
open Swate.Components.Page.ProvenanceGrouping.Edit
open Swate.Components.Page.ProvenanceGrouping.Session
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

let private propertyByName name model =
    model.PropertyValues
    |> Map.toList
    |> List.find (fun (_, value) -> value.Header.Category.Name = name)

let private annotationPayload (annotation: Annotation) =
    annotation.Name,
    annotation.Value,
    annotation.Unit,
    annotation.NameTAN,
    annotation.ValueTAN,
    annotation.UnitTAN,
    annotation.AdditionalType

let private update propertyId value unit session =
    Session.updatePropertyValue propertyId value unit session |> expectOk |> fst

let private createSet side header name session =
    Session.createLoadedSet
        {
            Side = side
            Header = header
            Name = name
        }
        session
    |> expectOk
    |> fst

let private connect inputId outputId session =
    Session.connectSets inputId outputId None session |> expectOk |> fst

let private addLayer name selectedSets session =
    Session.addLayer
        {
            Name = name
            SelectedSets = selectedSets
        }
        session
    |> expectOk
    |> fst

let private createProperty target kind category value session =
    Session.createLoadedPropertyValue
        {
            Target = target
            CopiedFrom = None
            Header = {
                Kind = kind
                Category = {
                    Name = category
                    TermSource = None
                    TermAccession = None
                }
            }
            Value = ProvenanceValue.Text value
            Unit = None
        }
        session
    |> expectOk
    |> fst

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

let tests =
    testList "ProcessCore writeback" [
        canonicalPlanTests

        testCase "updates every indexed duplicate annotation in memory"
        <| fun _ ->
            let arc, parameterOne, parameterTwo = annotated ()
            let converted = fromArc loadedTable arc |> expectOk
            let propertyId, _ = propertyByName "parameter-neutral" converted.Model

            let session =
                Session.init converted.Model
                |> update propertyId (ProvenanceValue.Integer 9) None

            let summary = writeBack converted.Index session arc |> expectOk

            Expect.equal summary.UpdatedAnnotations 2 "Both occurrences must be updated."
            Expect.equal parameterOne.Value (Some "9") "First annotation must contain the invariant integer."
            Expect.equal parameterTwo.Value (Some "9") "Second annotation must contain the invariant integer."

        testCase "writes floats with invariant culture"
        <| fun _ ->
            let arc, parameterOne, parameterTwo = annotated ()
            let converted = fromArc loadedTable arc |> expectOk
            let propertyId, _ = propertyByName "parameter-neutral" converted.Model

            let session =
                Session.init converted.Model
                |> update propertyId (ProvenanceValue.Float 1.5) None

            let originalCulture = System.Globalization.CultureInfo.CurrentCulture

            try
                System.Globalization.CultureInfo.CurrentCulture <- System.Globalization.CultureInfo("de-DE")
                writeBack converted.Index session arc |> expectOk |> ignore
                Expect.equal parameterOne.Value (Some "1.5") "Float must use an invariant decimal separator."
                Expect.equal parameterTwo.Value (Some "1.5") "Every duplicate must use invariant formatting."
            finally
                System.Globalization.CultureInfo.CurrentCulture <- originalCulture

        testCase "writes term and unit accessions and clears them for text"
        <| fun _ ->
            let arc, _, _ = annotated ()
            let converted = fromArc loadedTable arc |> expectOk
            let propertyId, _ = propertyByName "category-neutral" converted.Model

            let term = {
                Name = "changed-neutral"
                TermSource = None
                TermAccession = Some "term:changed"
            }

            let unit = {
                Name = "changed-unit"
                TermSource = None
                TermAccession = Some "term:changed-unit"
            }

            let first =
                Session.init converted.Model
                |> update propertyId (ProvenanceValue.Term term) (Some unit)

            writeBack converted.Index first arc |> expectOk |> ignore
            let reconverted = fromArc loadedTable arc |> expectOk
            let nextId, _ = propertyByName "category-neutral" reconverted.Model

            let second =
                Session.init reconverted.Model
                |> update nextId (ProvenanceValue.Text "plain-neutral") None

            writeBack reconverted.Index second arc |> expectOk |> ignore

            let annotation =
                arc.AllProcesses().[0].Input.Value.AsSample().AdditionalProperty.[0]

            Expect.equal annotation.Value (Some "plain-neutral") "Text value must be written."
            Expect.isNone annotation.ValueTAN "Text write must clear the value accession."
            Expect.isNone annotation.Unit "Removing the unit must clear its text."
            Expect.isNone annotation.UnitTAN "Removing the unit must clear its accession."

        testCase "updates an upstream value at its original location"
        <| fun _ ->
            let arc, previousAnnotation = withPreviousContext ()
            let converted = fromArc loadedTable arc |> expectOk
            let propertyId, _ = propertyByName "previous-parameter" converted.Model

            let session =
                Session.init converted.Model
                |> update propertyId (ProvenanceValue.Text "changed-upstream") None

            writeBack converted.Index session arc |> expectOk |> ignore

            Expect.equal
                previousAnnotation.Value
                (Some "changed-upstream")
                "Writer must mutate the upstream annotation."

        testCase "adds a disconnected set as a one-sided ProcessCore process"
        <| fun _ ->
            let fixture = basic ()
            let converted = fromArc loadedTable fixture.Arc |> expectOk

            let session =
                Session.init converted.Model
                |> createSet
                    ProvenanceSide.Output
                    {
                        Kind = ProcessCoreKinds.sampleEndpoint
                        Text = "Sample"
                    }
                    "added-output"

            let summary = writeBack converted.Index session fixture.Arc |> expectOk

            let added =
                fixture.Dataset.Processes
                |> Seq.find (fun proc -> proc.Output |> Option.exists (fun node -> node.Key() = "M:added-output"))

            Expect.equal added.Name "stage-neutral" "Added set must remain in the loaded logical group."
            Expect.isEmpty (added.Input |> Option.toList) "Disconnected output must use a one-sided process."
            Expect.equal summary.AddedProcesses 1 "One one-sided process must be added."

        testCase "adds a connection without leaving added sets as placeholder rows"
        <| fun _ ->
            let fixture = basic ()
            let converted = fromArc loadedTable fixture.Arc |> expectOk
            let initial = Session.init converted.Model

            let withInput =
                createSet
                    ProvenanceSide.Input
                    {
                        Kind = ProcessCoreKinds.sampleEndpoint
                        Text = "Sample"
                    }
                    "added-input"
                    initial

            let withBoth =
                createSet
                    ProvenanceSide.Output
                    {
                        Kind = ProcessCoreKinds.sampleEndpoint
                        Text = "Sample"
                    }
                    "added-output"
                    withInput

            let layer = Session.activeLayer withBoth

            let inputId =
                layer.Model.InputSets
                |> Map.toList
                |> List.find (fun (_, set) -> set.Name = "added-input")
                |> fst

            let outputId =
                layer.Model.OutputSets
                |> Map.toList
                |> List.find (fun (_, set) -> set.Name = "added-output")
                |> fst

            let finalSession = connect inputId outputId withBoth

            writeBack converted.Index finalSession fixture.Arc |> expectOk |> ignore

            let matching =
                fixture.Dataset.Processes
                |> Seq.filter (fun proc ->
                    (proc.Input |> Option.exists (fun node -> node.Key() = "M:added-input"))
                    || (proc.Output |> Option.exists (fun node -> node.Key() = "M:added-output"))
                )
                |> Seq.toList

            Expect.equal matching.Length 1 "The final connected pair must not leave one-sided placeholders."

            Expect.equal
                (matching.Head.Input |> Option.toList |> List.length)
                1
                "Connection process must have one input."

            Expect.equal
                (matching.Head.Output |> Option.toList |> List.length)
                1
                "Connection process must have one output."

        testCase "removes one all-to-all edge while preserving both endpoint sets"
        <| fun _ ->
            let arc, dataset, _ = allToAll ()
            let converted = fromArc loadedTable arc |> expectOk
            let connectionId = converted.Model.Connections |> Map.toList |> List.head |> fst

            let session =
                Session.init converted.Model
                |> Session.removeConnection connectionId
                |> expectOk
                |> fst

            writeBack converted.Index session arc |> expectOk |> ignore
            let reconverted = fromArc loadedTable arc |> expectOk
            Expect.equal reconverted.Model.InputSets.Count 1 "Removed edge must not remove its input set."
            Expect.equal reconverted.Model.OutputSets.Count 2 "Removed edge must not remove either output set."
            Expect.equal reconverted.Model.Connections.Count 1 "Exactly one all-to-all edge must remain."

        testCase "consumes a loaded-layer connection that was added and then removed"
        <| fun _ ->
            let fixture = basic ()
            let converted = fromArc loadedTable fixture.Arc |> expectOk
            let inputId = converted.Model.InputSets |> Map.toList |> List.head |> fst

            let withOutput =
                Session.init converted.Model
                |> createSet
                    ProvenanceSide.Output
                    {
                        Kind = ProcessCoreKinds.sampleEndpoint
                        Text = "Sample"
                    }
                    "transient-output"

            let outputId =
                (Session.activeLayer withOutput).Model.OutputSets
                |> Map.toList
                |> List.find (fun (_, set) -> set.Name = "transient-output")
                |> fst

            let connected = connect inputId outputId withOutput

            let connectionId =
                (Session.activeLayer connected).Model.Connections
                |> Map.toList
                |> List.find (fun (_, connection) -> connection.OutputSetId = outputId)
                |> fst

            let finalSession =
                Session.removeConnection connectionId connected |> expectOk |> fst

            writeBack converted.Index finalSession fixture.Arc |> expectOk |> ignore
            let reconverted = fromArc loadedTable fixture.Arc |> expectOk

            let transient =
                reconverted.Model.OutputSets
                |> Map.toList
                |> List.map snd
                |> List.find (fun set -> set.Name = "transient-output")

            Expect.isFalse
                (reconverted.Model.Connections
                 |> Map.exists (fun _ connection ->
                     reconverted.Model.OutputSets.[connection.OutputSetId].Name = "transient-output"
                 ))
                "The added-then-removed connection must not materialize."

            Expect.equal transient.Name "transient-output" "The transient set must remain as a disconnected endpoint."

        testCase "rejects two loaded sets that materialize to one node with conflicting headers"
        <| fun _ ->
            let fixture = basic ()
            let converted = fromArc loadedTable fixture.Arc |> expectOk

            let session =
                Session.init converted.Model
                |> createSet
                    ProvenanceSide.Output
                    {
                        Kind = ProcessCoreKinds.sampleEndpoint
                        Text = "Sample"
                    }
                    "conflicted-name"
                |> createSet
                    ProvenanceSide.Output
                    {
                        Kind = ProcessCoreKinds.sampleEndpoint
                        Text = "Aliquot"
                    }
                    "conflicted-name"

            let beforeCount = fixture.Dataset.Processes.Count
            let errors = writeBack converted.Index session fixture.Arc |> expectError

            Expect.isTrue
                (errors
                 |> List.exists (
                     function
                     | ProcessCoreWritebackError.ConflictingNodeIdentity _ -> true
                     | _ -> false
                 ))
                "Distinct sets sharing one node identity must fail validation."

            Expect.equal fixture.Dataset.Processes.Count beforeCount "Conflicting identity must not mutate the graph."

        testCase "round-trips a sample set name containing a hash"
        <| fun _ ->
            let fixture = basic ()
            let converted = fromArc loadedTable fixture.Arc |> expectOk

            let session =
                Session.init converted.Model
                |> createSet
                    ProvenanceSide.Output
                    {
                        Kind = ProcessCoreKinds.sampleEndpoint
                        Text = "Sample"
                    }
                    "sample#name"

            writeBack converted.Index session fixture.Arc |> expectOk |> ignore
            let reconverted = fromArc loadedTable fixture.Arc |> expectOk

            Expect.isTrue
                (reconverted.Model.OutputSets
                 |> Map.exists (fun _ set -> set.Name = "sample#name"))
                "A hash is an opaque part of a ProcessCore sample name."

        testCase "creates a Data endpoint by splitting the final hash"
        <| fun _ ->
            let fixture = basic ()
            let converted = fromArc loadedTable fixture.Arc |> expectOk

            let session =
                Session.init converted.Model
                |> createSet
                    ProvenanceSide.Output
                    {
                        Kind = ProcessCoreKinds.dataEndpoint
                        Text = "Data"
                    }
                    "file#section#row-neutral"

            writeBack converted.Index session fixture.Arc |> expectOk |> ignore

            let added =
                fixture.Dataset.Processes
                |> Seq.collect (fun proc -> proc.Output |> Option.toList)
                |> Seq.choose (
                    function
                    | DataNode data -> Some data
                    | _ -> None
                )
                |> Seq.find (fun data -> data.Path = "file#section")

            Expect.equal added.Selector (Some "row-neutral") "The final hash suffix must become the data selector."

            let reconverted = fromArc loadedTable fixture.Arc |> expectOk

            Expect.isTrue
                (reconverted.Model.OutputSets
                 |> Map.exists (fun _ set -> set.Name = "file#section#row-neutral"))
                "The data path and selector must reconvert to the editor name."

        testCase "rejects an unsupported endpoint kind before mutation"
        <| fun _ ->
            let fixture = basic ()
            let converted = fromArc loadedTable fixture.Arc |> expectOk

            let session =
                Session.init converted.Model
                |> createSet
                    ProvenanceSide.Output
                    {
                        Kind = ProvenanceKind.create "process-core:endpoint:unsupported" "Unsupported"
                        Text = "Unsupported"
                    }
                    "unsupported-neutral"

            let beforeCount = fixture.Dataset.Processes.Count
            let errors = writeBack converted.Index session fixture.Arc |> expectError

            Expect.isTrue
                (errors
                 |> List.exists (
                     function
                     | ProcessCoreWritebackError.UnsupportedEndpointKind "process-core:endpoint:unsupported" -> true
                     | _ -> false
                 ))
                "Unsupported endpoint kinds must return a typed error."

            Expect.equal fixture.Dataset.Processes.Count beforeCount "Unsupported endpoint validation must be atomic."

        testCase "replays generated set IDs in numeric ordinal order"
        <| fun _ ->
            let fixture = basic ()
            let converted = fromArc loadedTable fixture.Arc |> expectOk
            let names = [ 1..11 ] |> List.map (fun ordinal -> $"ordered-set-{ordinal}")

            let session =
                names
                |> List.fold
                    (fun state name ->
                        createSet
                            ProvenanceSide.Output
                            {
                                Kind = ProcessCoreKinds.sampleEndpoint
                                Text = "Sample"
                            }
                            name
                            state
                    )
                    (Session.init converted.Model)

            writeBack converted.Index session fixture.Arc |> expectOk |> ignore

            let addedNames =
                fixture.Dataset.Processes
                |> Seq.skip 1
                |> Seq.map (fun proc -> proc.Output.Value.AsSample().Name)
                |> Seq.toList

            Expect.sequenceEqual
                addedNames
                names
                "Generated ordinals must be parsed numerically; lexical order would place 10 before 2."

        testCase "preserves parameters and components when a removed edge splits a process"
        <| fun _ ->
            let input = Sample("split-input")
            let outputOne = Sample("split-output-one")
            let outputTwo = Sample("split-output-two")

            let parameter =
                Annotation("split-parameter", value = "parameter-value", additionalType = "ParameterValue")

            let recipeComponent =
                Annotation("split-component", value = "component-value", additionalType = "Component")

            let recipe = Recipe(name = "split-recipe", components = [ recipeComponent ])

            let first =
                mkProcessFull "stage-neutral" (Some recipe) [ SampleNode input ] [ SampleNode outputOne ] [ parameter ]

            let second =
                mkProcessFull "stage-neutral" (Some recipe) [ SampleNode input ] [ SampleNode outputTwo ] [ parameter ]

            let dataset = Dataset("dataset-neutral", processes = [ first; second ])
            let arc = ARC("arc-neutral", hasPart = [ dataset ])
            let converted = fromArc loadedTable arc |> expectOk

            let removedId =
                converted.Model.Connections
                |> Map.toList
                |> List.find (fun (_, connection) ->
                    converted.Model.OutputSets.[connection.OutputSetId].Name = "split-output-one"
                )
                |> fst

            let finalSession =
                Session.init converted.Model
                |> Session.removeConnection removedId
                |> expectOk
                |> fst

            writeBack converted.Index finalSession arc |> expectOk |> ignore
            let reconverted = fromArc loadedTable arc |> expectOk

            let namesForSet set =
                ProvenanceSet.effectivePropertyValueIds set
                |> List.choose (fun propertyId -> reconverted.Model.PropertyValues.TryFind propertyId)
                |> List.map (fun property -> property.Header.Category.Name)

            let disconnectedOutput =
                reconverted.Model.OutputSets
                |> Map.toList
                |> List.map snd
                |> List.find (fun set -> set.Name = "split-output-one")

            Expect.contains
                (namesForSet disconnectedOutput)
                "split-parameter"
                "Disconnected replacement must retain the parameter."

            Expect.contains
                (namesForSet disconnectedOutput)
                "split-component"
                "Disconnected replacement must retain the component."

        testCase "stores an explicit characteristic on an output node"
        <| fun _ ->
            let fixture = basic ()
            let converted = fromArc loadedTable fixture.Arc |> expectOk
            let outputId = converted.Model.OutputSets |> Map.toList |> List.head |> fst

            let session =
                Session.init converted.Model
                |> createProperty
                    (ProvenancePropertyTarget.OutputSets [ outputId ])
                    ProcessCoreKinds.characteristic
                    "unfinished-characteristic"
                    "value-neutral"

            writeBack converted.Index session fixture.Arc |> expectOk |> ignore

            Expect.isTrue
                (fixture.Process.Output.Value.AsSample().AdditionalProperty
                 |> Seq.exists (fun annotation ->
                     annotation.Name = "unfinished-characteristic"
                     && annotation.AdditionalType = Some "CharacteristicValue"
                 ))
                "Explicit output placement must be retained."

        testCase "stores an explicit factor on an input node"
        <| fun _ ->
            let fixture = basic ()
            let converted = fromArc loadedTable fixture.Arc |> expectOk
            let inputId = converted.Model.InputSets |> Map.toList |> List.head |> fst

            let session =
                Session.init converted.Model
                |> createProperty
                    (ProvenancePropertyTarget.InputSets [ inputId ])
                    ProcessCoreKinds.factor
                    "unfinished-factor"
                    "level-neutral"

            writeBack converted.Index session fixture.Arc |> expectOk |> ignore

            Expect.isTrue
                (fixture.Process.Input.Value.AsSample().AdditionalProperty
                 |> Seq.exists (fun annotation ->
                     annotation.Name = "unfinished-factor"
                     && annotation.AdditionalType = Some "FactorValue"
                 ))
                "Explicit input placement must be retained."

        testCase "stores a set-targeted parameter only on the exact output node"
        <| fun _ ->
            let fixture = basic ()
            let converted = fromArc loadedTable fixture.Arc |> expectOk
            let inputId = converted.Model.InputSets |> Map.toList |> List.head |> fst
            let outputId = converted.Model.OutputSets |> Map.toList |> List.head |> fst

            let session =
                Session.init converted.Model
                |> createProperty
                    (ProvenancePropertyTarget.OutputSets [ outputId ])
                    ProcessCoreKinds.parameter
                    "set-parameter"
                    "parameter-value"

            writeBack converted.Index session fixture.Arc |> expectOk |> ignore

            Expect.isTrue
                (fixture.Process.Output.Value.AsSample().AdditionalProperty
                 |> Seq.exists (fun annotation ->
                     annotation.Name = "set-parameter"
                     && annotation.AdditionalType = Some "ParameterValue"
                 ))
                "A set-targeted parameter must be stored on the selected node."

            Expect.isFalse
                (fixture.Process.ParameterValue
                 |> Seq.exists (fun annotation -> annotation.Name = "set-parameter"))
                "A set-targeted parameter must not spread through the process."

            let reconverted = fromArc loadedTable fixture.Arc |> expectOk
            let propertyId, _ = propertyByName "set-parameter" reconverted.Model

            Expect.contains
                reconverted.Model.OutputSets.[outputId].PropertyValueIds
                propertyId
                "The parameter must reconvert on the selected output."

            Expect.isFalse
                (reconverted.Model.InputSets.[inputId].PropertyValueIds
                 |> List.contains propertyId)
                "The parameter must not reconvert on the input."

        testCase "rejects adding a recipe component to a set at the session boundary"
        <| fun _ ->
            let fixture = basic ()
            let converted = fromArc loadedTable fixture.Arc |> expectOk
            let inputId = converted.Model.InputSets |> Map.toList |> List.head |> fst
            let session = Session.init converted.Model

            let error =
                Session.createLoadedPropertyValue
                    {
                        Target = ProvenancePropertyTarget.InputSets [ inputId ]
                        CopiedFrom = None
                        Header = {
                            Kind = ProcessCoreKinds.componentKind
                            Category = {
                                Name = "set-component"
                                TermSource = None
                                TermAccession = None
                            }
                        }
                        Value = ProvenanceValue.Text "component-value"
                        Unit = None
                    }
                    session
                |> expectError

            match error with
            | SessionError.EditFailed(EditError.ReadOnlyPropertyKind kind) ->
                Expect.equal kind ProcessCoreKinds.componentKind "The adapter's read-only kind must be reported."
            | _ -> failtestf "Expected a read-only property-kind error, got %A" error

            Expect.isEmpty session.PatchLog "Rejected Component creation must not append a patch."
            Expect.isNone fixture.Process.ExecutesRecipe "Rejected creation must not assign a Recipe."

            Expect.isFalse
                (fixture.Process.Input.Value.AsSample().AdditionalProperty
                 |> Seq.exists (fun annotation -> annotation.Name = "set-component"))
                "Rejected creation must not add an endpoint property."

        testCase "stores connection-targeted node properties on both endpoints"
        <| fun _ ->
            let fixture = basic ()
            let converted = fromArc loadedTable fixture.Arc |> expectOk
            let connectionId = converted.Model.Connections |> Map.toList |> List.head |> fst

            let session =
                Session.init converted.Model
                |> createProperty
                    (ProvenancePropertyTarget.Connections [ connectionId ])
                    ProcessCoreKinds.additionalProperty
                    "edge-note"
                    "note-neutral"

            writeBack converted.Index session fixture.Arc |> expectOk |> ignore

            Expect.isTrue
                (fixture.Process.Input.Value.AsSample().AdditionalProperty
                 |> Seq.exists (fun a -> a.Name = "edge-note"))
                "Input node must receive the edge property."

            Expect.isTrue
                (fixture.Process.Output.Value.AsSample().AdditionalProperty
                 |> Seq.exists (fun a -> a.Name = "edge-note"))
                "Output node must receive the edge property."

        testCase "stores a connection parameter only on its exact process"
        <| fun _ ->
            let fixture = basic ()
            let converted = fromArc loadedTable fixture.Arc |> expectOk
            let connectionId = converted.Model.Connections |> Map.toList |> List.head |> fst

            let session =
                Session.init converted.Model
                |> createProperty
                    (ProvenancePropertyTarget.Connections [ connectionId ])
                    ProcessCoreKinds.parameter
                    "edge-parameter"
                    "parameter-value"

            writeBack converted.Index session fixture.Arc |> expectOk |> ignore

            Expect.isTrue
                (fixture.Process.ParameterValue
                 |> Seq.exists (fun annotation -> annotation.Name = "edge-parameter"))
                "The exact connection process must receive the parameter."

        testCase "rejects creating a recipe component for an exact connection without mutation"
        <| fun _ ->
            let fixture = basic ()
            let converted = fromArc loadedTable fixture.Arc |> expectOk
            let connectionId = converted.Model.Connections |> Map.toList |> List.head |> fst

            let session = Session.init converted.Model

            let error =
                Session.createLoadedPropertyValue
                    {
                        Target = ProvenancePropertyTarget.Connections [ connectionId ]
                        CopiedFrom = None
                        Header = {
                            Kind = ProcessCoreKinds.componentKind
                            Category = {
                                Name = "edge-component"
                                TermSource = None
                                TermAccession = None
                            }
                        }
                        Value = ProvenanceValue.Text "component-value"
                        Unit = None
                    }
                    session
                |> expectError

            let recipeCountBefore = fixture.Arc.Recipes.Count

            match error with
            | SessionError.EditFailed(EditError.ReadOnlyPropertyKind kind) ->
                Expect.equal kind ProcessCoreKinds.componentKind "The adapter's read-only kind must be reported."
            | _ -> failtestf "Expected a read-only property-kind error, got %A" error

            Expect.isNone fixture.Process.ExecutesRecipe "Writeback must not construct or assign a Recipe."

            Expect.equal
                fixture.Arc.Recipes.Count
                recipeCountBefore
                "Writeback must not grow the stored Recipe catalog."

            Expect.isEmpty session.PatchLog "Rejected Component creation must not append a patch."

        testCase "rejects adding a recipe component to an endpoint even for a forged session"
        <| fun _ ->
            let fixture = basic ()
            let converted = fromArc loadedTable fixture.Arc |> expectOk
            let outputId = converted.Model.OutputSets |> Map.toList |> List.head |> fst

            let mutableSession =
                Session.init converted.Model
                |> createProperty
                    (ProvenancePropertyTarget.OutputSets [ outputId ])
                    ProcessCoreKinds.additionalProperty
                    "forged-component"
                    "component-value"

            let layer = Session.activeLayer mutableSession
            let propertyId, propertyValue = propertyByName "forged-component" layer.Model

            let componentHeader = {
                propertyValue.Header with
                    Kind = ProcessCoreKinds.componentKind
            }

            let forgedLayer = {
                layer with
                    Model = {
                        layer.Model with
                            PropertyValues =
                                layer.Model.PropertyValues
                                |> Map.add propertyId {
                                    propertyValue with
                                        Header = componentHeader
                                }
                    }
            }

            let forgedSession = {
                mutableSession with
                    Layers =
                        mutableSession.Layers
                        |> List.map (fun current -> if current.Id = forgedLayer.Id then forgedLayer else current)
                    PatchLog = [
                        ProvenanceTablePatch.AddLoadedPropertyValue(
                            ProvenancePropertyTarget.OutputSets [ outputId ],
                            None,
                            componentHeader,
                            propertyValue.Value,
                            propertyValue.Unit
                        )
                    ]
            }

            let recipeCountBefore = fixture.Arc.Recipes.Count
            let output = fixture.Process.Output.Value.AsSample()
            let outputPropertyCountBefore = output.AdditionalProperty.Count
            let errors = writeBack converted.Index forgedSession fixture.Arc |> expectError

            Expect.contains
                errors
                ProcessCoreWritebackError.ReadOnlyRecipeComponentMutation
                "Every Component placement must fail before target-specific planning."

            Expect.isNone fixture.Process.ExecutesRecipe "Writeback must not construct or assign a Recipe."

            Expect.equal
                fixture.Arc.Recipes.Count
                recipeCountBefore
                "Writeback must not grow the stored Recipe catalog."

            Expect.equal
                output.AdditionalProperty.Count
                outputPropertyCountBefore
                "Writeback must not smuggle a Component into endpoint additional properties."

        testCase "rejects loaded recipe component mutations before session or adapter state changes"
        <| fun _ ->
            let arc, _, _ = annotated ()
            let processObject = arc.HasPart[0].Processes[0]
            let converted = fromArc loadedTable arc |> expectOk
            let componentId, componentValue = propertyByName "component-neutral" converted.Model
            let recipe = processObject.ExecutesRecipe.Value
            let storedComponent = recipe.Components |> Seq.exactlyOne
            let beforePayload = annotationPayload storedComponent

            let session = Session.init converted.Model

            let updateError =
                Session.updatePropertyValue
                    componentId
                    (ProvenanceValue.Text "changed-component")
                    componentValue.Unit
                    session
                |> expectError

            let outputId = converted.Model.OutputSets |> Map.toList |> List.head |> fst

            let copyError =
                Session.copyPropertyValueToLoadedTarget
                    componentId
                    (ProvenancePropertyTarget.OutputSets [ outputId ])
                    session
                |> expectError

            let createError =
                Session.createLoadedPropertyValue
                    {
                        Target = ProvenancePropertyTarget.OutputSets [ outputId ]
                        CopiedFrom = None
                        Header = componentValue.Header
                        Value = ProvenanceValue.Text "new-component"
                        Unit = None
                    }
                    session
                |> expectError

            for error in [ updateError; copyError; createError ] do
                match error with
                | SessionError.EditFailed _ -> ()
                | _ -> failtestf "Expected a read-only edit failure, got %A" error

            Expect.equal
                (Session.activeLayer session).Model
                converted.Model
                "Rejected Component mutations must not change the active model."

            Expect.isEmpty
                session.DirtyPropertyValueIds
                "Rejected Component mutations must not dirty the projected value."

            Expect.isEmpty session.PatchLog "Rejected Component mutations must not append journal patches."

            let anchor = ProvenancePropertyOrigin.anchor componentValue.Origin

            let forgedSession = {
                session with
                    PatchLog = [
                        ProvenanceTablePatch.UpdatePropertyValue(
                            componentId,
                            anchor,
                            componentValue.Value,
                            ProvenanceValue.Text "changed-component",
                            componentValue.Unit
                        )
                    ]
            }

            let errors = writeBack converted.Index forgedSession arc |> expectError

            Expect.contains
                errors
                ProcessCoreWritebackError.ReadOnlyRecipeComponentMutation
                "The adapter must still reject forged loaded Component edits during preflight."

            Expect.equal
                (annotationPayload storedComponent)
                beforePayload
                "The stored Component payload must remain byte-for-byte equivalent at the adapter boundary."

            Expect.isTrue
                (obj.ReferenceEquals(processObject.ExecutesRecipe.Value, recipe))
                "The Process must keep the exact assigned Recipe reference."

        testCase "stores a parameter targeting existing and added connections on both processes"
        <| fun _ ->
            let fixture = basic ()
            let converted = fromArc loadedTable fixture.Arc |> expectOk

            let existingConnectionId =
                converted.Model.Connections |> Map.toList |> List.head |> fst

            let withSets =
                Session.init converted.Model
                |> createSet
                    ProvenanceSide.Input
                    {
                        Kind = ProcessCoreKinds.sampleEndpoint
                        Text = "Sample"
                    }
                    "mixed-input"
                |> createSet
                    ProvenanceSide.Output
                    {
                        Kind = ProcessCoreKinds.sampleEndpoint
                        Text = "Sample"
                    }
                    "mixed-output"

            let layer = Session.activeLayer withSets

            let inputId =
                layer.Model.InputSets
                |> Map.toList
                |> List.find (fun (_, set) -> set.Name = "mixed-input")
                |> fst

            let outputId =
                layer.Model.OutputSets
                |> Map.toList
                |> List.find (fun (_, set) -> set.Name = "mixed-output")
                |> fst

            let connected = connect inputId outputId withSets

            let addedConnectionId =
                (Session.activeLayer connected).Model.Connections
                |> Map.toList
                |> List.find (fun (_, connection) -> connection.OutputSetId = outputId)
                |> fst

            let session =
                connected
                |> createProperty
                    (ProvenancePropertyTarget.Connections [ existingConnectionId; addedConnectionId ])
                    ProcessCoreKinds.parameter
                    "mixed-parameter"
                    "mixed-value"

            writeBack converted.Index session fixture.Arc |> expectOk |> ignore

            Expect.isTrue
                (fixture.Process.ParameterValue
                 |> Seq.exists (fun annotation -> annotation.Name = "mixed-parameter"))
                "The existing connection process must receive the parameter."

            let addedProcess =
                fixture.Dataset.Processes
                |> Seq.find (fun proc -> proc.Input |> Option.exists (fun node -> node.Key() = "M:mixed-input"))

            Expect.isTrue
                (addedProcess.ParameterValue
                 |> Seq.exists (fun annotation -> annotation.Name = "mixed-parameter"))
                "The editor-created connection process must receive the parameter."

        testCase "writes the final value of a property that was added and then updated"
        <| fun _ ->
            let fixture = basic ()
            let converted = fromArc loadedTable fixture.Arc |> expectOk
            let outputId = converted.Model.OutputSets |> Map.toList |> List.head |> fst

            let withProperty =
                Session.init converted.Model
                |> createProperty
                    (ProvenancePropertyTarget.OutputSets [ outputId ])
                    ProcessCoreKinds.factor
                    "final-factor"
                    "initial-value"

            let propertyId, _ =
                propertyByName "final-factor" (Session.activeLayer withProperty).Model

            let finalSession =
                update propertyId (ProvenanceValue.Text "final-value") None withProperty

            writeBack converted.Index finalSession fixture.Arc |> expectOk |> ignore

            let written =
                fixture.Process.Output.Value.AsSample().AdditionalProperty
                |> Seq.find (fun annotation -> annotation.Name = "final-factor")

            Expect.equal written.Value (Some "final-value") "Final session state must override the add-patch payload."

        testCase "replays same-category property IDs in numeric ordinal order"
        <| fun _ ->
            let fixture = basic ()
            let converted = fromArc loadedTable fixture.Arc |> expectOk
            let inputId = converted.Model.InputSets |> Map.toList |> List.head |> fst
            let values = [ 1..11 ] |> List.map (fun ordinal -> $"value-{ordinal}")

            let session =
                values
                |> List.fold
                    (fun state value ->
                        createProperty
                            (ProvenancePropertyTarget.InputSets [ inputId ])
                            ProcessCoreKinds.characteristic
                            "duplicate-category"
                            value
                            state
                    )
                    (Session.init converted.Model)

            writeBack converted.Index session fixture.Arc |> expectOk |> ignore

            let written =
                fixture.Process.Input.Value.AsSample().AdditionalProperty
                |> Seq.filter (fun annotation -> annotation.Name = "duplicate-category")
                |> Seq.map (fun annotation -> annotation.Value)
                |> Seq.toList

            Expect.sequenceEqual
                written
                (values |> List.map Some)
                "Generated ordinals must be parsed numerically; lexical order would place 10 before 2."

        testCase "rejects adding a recipe component even when it collides with stored content"
        <| fun _ ->
            let fixture = basic ()

            let existing =
                Annotation(
                    "collision-category",
                    value = "collision-value",
                    valueTAN = "term:existing",
                    additionalType = "Component"
                )

            let recipe = Recipe(name = "collision-recipe", components = [ existing ])
            fixture.Process.ExecutesRecipe <- Some recipe
            let converted = fromArc loadedTable fixture.Arc |> expectOk
            let connectionId = converted.Model.Connections |> Map.toList |> List.head |> fst

            let forgedEditableComponentKind =
                ProvenanceKind.create ProcessCoreKinds.componentKind.Id ProcessCoreKinds.componentKind.Label

            let session =
                Session.createLoadedPropertyValue
                    {
                        Target = ProvenancePropertyTarget.Connections [ connectionId ]
                        CopiedFrom = None
                        Header = {
                            // Simulates a stale or malicious caller that knows the
                            // adapter discriminator but omits its read-only capability.
                            Kind = forgedEditableComponentKind
                            Category = {
                                Name = "collision-category"
                                TermSource = None
                                TermAccession = None
                            }
                        }
                        Value =
                            ProvenanceValue.Term {
                                Name = "collision-value"
                                TermSource = None
                                TermAccession = Some "term:requested"
                            }
                        Unit = None
                    }
                    (Session.init converted.Model)
                |> expectOk
                |> fst

            let beforeCount = recipe.Components.Count
            let errors = writeBack converted.Index session fixture.Arc |> expectError

            Expect.contains
                errors
                ProcessCoreWritebackError.ReadOnlyRecipeComponentMutation
                "Read-only validation must run before any Recipe Component collision handling."

            Expect.equal recipe.Components.Count beforeCount "A collision must not partially add a component."

            Expect.equal
                existing.ValueTAN
                (Some "term:existing")
                "A collision must leave the existing annotation unchanged."

        testCase "rejects a node annotation collision that differs only by discriminator"
        <| fun _ ->
            let fixture = basic ()
            let output = fixture.Process.Output.Value.AsSample()

            output.AddAdditionalProperty(
                Annotation("kind-collision", value = "same-value", additionalType = "CharacteristicValue")
            )

            let converted = fromArc loadedTable fixture.Arc |> expectOk
            let outputId = converted.Model.OutputSets |> Map.toList |> List.head |> fst

            let session =
                Session.init converted.Model
                |> createProperty
                    (ProvenancePropertyTarget.OutputSets [ outputId ])
                    ProcessCoreKinds.factor
                    "kind-collision"
                    "same-value"

            let beforeCount = output.AdditionalProperty.Count
            let errors = writeBack converted.Index session fixture.Arc |> expectError

            Expect.isTrue
                (errors
                 |> List.exists (
                     function
                     | ProcessCoreWritebackError.ConflictingAnnotationIdentity _ -> true
                     | _ -> false
                 ))
                "Different property kinds must not be silently deduplicated."

            Expect.equal output.AdditionalProperty.Count beforeCount "A discriminator collision must be atomic."

        testCase "copies an upstream property into the current group without changing its original"
        <| fun _ ->
            let arc, upstreamAnnotation = withPreviousContext ()
            let converted = fromArc loadedTable arc |> expectOk
            let previousId, _ = propertyByName "previous-parameter" converted.Model
            let inputId = converted.Model.InputSets |> Map.toList |> List.head |> fst

            let session =
                Session.init converted.Model
                |> Session.copyPropertyValueToLoadedTarget previousId (ProvenancePropertyTarget.InputSets [ inputId ])
                |> expectOk
                |> fst

            writeBack converted.Index session arc |> expectOk |> ignore

            let current =
                arc.AllProcesses() |> Seq.find (fun proc -> proc.Name = "stage-neutral")

            Expect.equal
                upstreamAnnotation.Value
                (Some "previous-value")
                "Copying must not mutate the upstream annotation."

            Expect.isTrue
                (current.Input.Value.AsSample().AdditionalProperty
                 |> Seq.exists (fun annotation ->
                     annotation.Name = "previous-parameter"
                     && annotation.Value = Some "previous-value"
                     && annotation.AdditionalType = Some "ParameterValue"
                 ))
                "A parameter copied to an input set must be stored on that exact node."

        testCase "adds multiple new logical groups to the selected dataset in layer order"
        <| fun _ ->
            let fixture = basic ()
            let converted = fromArc loadedTable fixture.Arc |> expectOk
            let first = Session.init converted.Model |> addLayer "new-stage-one" []
            let second = first |> addLayer "new-stage-two" []

            let summary = writeBack converted.Index second fixture.Arc |> expectOk

            let addedNames =
                fixture.Dataset.Processes
                |> Seq.map (fun proc -> proc.Name)
                |> Seq.filter (fun name -> name.StartsWith("new-stage-"))
                |> Seq.toList

            Expect.sequenceEqual
                addedNames
                [ "new-stage-one"; "new-stage-two" ]
                "Layer order must determine process-group order."

            Expect.isGreaterThanOrEqual summary.AddedProcesses 2 "Each new layer must materialize."

        testCase "reuses a reference-linked canonical node"
        <| fun _ ->
            let fixture = basic ()
            let converted = fromArc loadedTable fixture.Arc |> expectOk
            let outputId = converted.Model.OutputSets |> Map.toList |> List.head |> fst

            let session =
                Session.init converted.Model
                |> addLayer "new-stage" [ ProvenanceSide.Output, outputId ]

            writeBack converted.Index session fixture.Arc |> expectOk |> ignore

            let created =
                fixture.Dataset.Processes |> Seq.find (fun proc -> proc.Name = "new-stage")

            Expect.isTrue
                (obj.ReferenceEquals(created.Input.Value.AsSample(), fixture.Process.Output.Value.AsSample()))
                "Reference link must resolve to the same canonical node object."

        testCase "retains an empty new layer as an empty process"
        <| fun _ ->
            let fixture = basic ()
            let converted = fromArc loadedTable fixture.Arc |> expectOk
            let session = Session.init converted.Model |> addLayer "empty-stage" []
            let emptyLayer = Session.activeLayer session

            let session = {
                session with
                    Layers =
                        session.Layers
                        |> List.map (fun layer ->
                            if layer.Id = emptyLayer.Id then
                                {
                                    layer with
                                        Model = {
                                            layer.Model with
                                                InputSets = Map.empty
                                        }
                                }
                            else
                                layer
                        )
                    ReferenceLinks = []
            }

            writeBack converted.Index session fixture.Arc |> expectOk |> ignore

            let created =
                fixture.Dataset.Processes |> Seq.find (fun proc -> proc.Name = "empty-stage")

            Expect.isEmpty (created.Input |> Option.toList) "Empty layer must have no inputs."
            Expect.isEmpty (created.Output |> Option.toList) "Empty layer must have no outputs."

        testCase "materializes an added-then-removed connection only as disconnected endpoints"
        <| fun _ ->
            let fixture = basic ()
            let converted = fromArc loadedTable fixture.Arc |> expectOk
            let withLayer = Session.init converted.Model |> addLayer "unfinished-stage" []

            let projectedInputId =
                (Session.activeLayer withLayer).Model.InputSets
                |> Map.toList
                |> List.head
                |> fst

            let withOutput =
                createSet
                    ProvenanceSide.Output
                    {
                        Kind = ProcessCoreKinds.sampleEndpoint
                        Text = "Sample"
                    }
                    "unfinished-output"
                    withLayer

            let outputId =
                (Session.activeLayer withOutput).Model.OutputSets
                |> Map.toList
                |> List.head
                |> fst

            let connected = connect projectedInputId outputId withOutput

            let connectionId =
                (Session.activeLayer connected).Model.Connections
                |> Map.toList
                |> List.head
                |> fst

            let finalSession =
                Session.removeConnection connectionId connected |> expectOk |> fst

            writeBack converted.Index finalSession fixture.Arc |> expectOk |> ignore

            let rows =
                fixture.Dataset.Processes
                |> Seq.filter (fun proc -> proc.Name = "unfinished-stage")
                |> Seq.toList

            Expect.isTrue
                (rows |> List.forall (fun proc -> proc.Input.IsNone || proc.Output.IsNone))
                "Removed connection must not reappear."

        testCase "rejects a blank new layer name without mutation"
        <| fun _ ->
            let fixture = basic ()
            let converted = fromArc loadedTable fixture.Arc |> expectOk
            let session = Session.init converted.Model |> addLayer "   " []
            let layer = Session.activeLayer session
            let beforeCount = fixture.Dataset.Processes.Count
            let errors = writeBack converted.Index session fixture.Arc |> expectError

            Expect.contains
                errors
                (ProcessCoreWritebackError.BlankLayerName layer.Id)
                "Blank layer must fail validation."

            Expect.equal fixture.Dataset.Processes.Count beforeCount "Blank-name failure must not add a process."

        testCase "rejects a new layer name that already exists in the dataset"
        <| fun _ ->
            let fixture = basic ()
            let converted = fromArc loadedTable fixture.Arc |> expectOk
            let session = Session.init converted.Model |> addLayer "stage-neutral" []
            let beforeCount = fixture.Dataset.Processes.Count
            let errors = writeBack converted.Index session fixture.Arc |> expectError

            Expect.contains
                errors
                (ProcessCoreWritebackError.DuplicateLayerName "stage-neutral")
                "Existing group name must fail validation."

            Expect.equal fixture.Dataset.Processes.Count beforeCount "Duplicate-name failure must not add a process."

        testCase "rejects incompatible reference links without mutation"
        <| fun _ ->
            let fixture = basic ()
            let converted = fromArc loadedTable fixture.Arc |> expectOk
            let initial = Session.init converted.Model
            let initialLayer = Session.activeLayer initial
            let inputId = initialLayer.Model.InputSets |> Map.toList |> List.head |> fst
            let outputId = initialLayer.Model.OutputSets |> Map.toList |> List.head |> fst
            let withLayer = addLayer "linked-stage" [ ProvenanceSide.Output, outputId ] initial
            let targetLayer = Session.activeLayer withLayer
            let targetId = targetLayer.Model.InputSets |> Map.toList |> List.head |> fst

            let incompatible = {
                Source = {
                    LayerId = initialLayer.Id
                    Side = ProvenanceSide.Input
                    SetId = inputId
                }
                Target = {
                    LayerId = targetLayer.Id
                    Side = ProvenanceSide.Input
                    SetId = targetId
                }
            }

            let invalidSession = {
                withLayer with
                    ReferenceLinks = withLayer.ReferenceLinks @ [ incompatible ]
            }

            let beforeCount = fixture.Dataset.Processes.Count
            let errors = writeBack converted.Index invalidSession fixture.Arc |> expectError

            Expect.contains
                errors
                (ProcessCoreWritebackError.InvalidReferenceLink incompatible)
                "Conflicting node keys must fail validation."

            Expect.equal fixture.Dataset.Processes.Count beforeCount "Invalid-link failure must not add a process."

        testCase "rejects a stale source before applying any edit"
        <| fun _ ->
            let fixture = basic ()
            let converted = fromArc loadedTable fixture.Arc |> expectOk

            let session =
                Session.init converted.Model
                |> createSet
                    ProvenanceSide.Output
                    {
                        Kind = ProcessCoreKinds.sampleEndpoint
                        Text = "Sample"
                    }
                    "must-not-appear"

            fixture.Process.Name <- "changed-concurrently"
            let beforeCount = fixture.Dataset.Processes.Count
            let result = writeBack converted.Index session fixture.Arc |> expectError

            Expect.contains
                result
                ProcessCoreWritebackError.StaleArc
                "Concurrent graph change must invalidate the index."

            Expect.equal fixture.Dataset.Processes.Count beforeCount "No process may be added after stale validation."

            Expect.isFalse
                (fixture.Dataset.AllSamples()
                 |> Seq.exists (fun sample -> sample.Name = "must-not-appear"))
                "No planned node may be created after stale validation."

        testCase "collects domain errors before applying valid earlier patches"
        <| fun _ ->
            let fixture = basic ()
            let converted = fromArc loadedTable fixture.Arc |> expectOk

            let withValidSet =
                Session.init converted.Model
                |> createSet
                    ProvenanceSide.Output
                    {
                        Kind = ProcessCoreKinds.sampleEndpoint
                        Text = "Sample"
                    }
                    "must-not-appear"

            let missingPropertyId = "missing-property-neutral"

            let missingHeader = {
                Kind = ProcessCoreKinds.parameter
                Category = {
                    Name = "missing-category"
                    TermSource = None
                    TermAccession = None
                }
            }

            let missingAnchor = {
                Source = converted.Model.Source
                ProcessId = None
                ProcessName = Some converted.Model.Source.Name
                Header = missingHeader
                InputNames = []
                OutputNames = []
            }

            let invalidSession = {
                withValidSet with
                    PatchLog =
                        withValidSet.PatchLog
                        @ [
                            ProvenanceTablePatch.UpdatePropertyValue(
                                missingPropertyId,
                                missingAnchor,
                                ProvenanceValue.Text "old",
                                ProvenanceValue.Text "new",
                                None
                            )
                        ]
            }

            let beforeCount = fixture.Dataset.Processes.Count
            let errors = writeBack converted.Index invalidSession fixture.Arc |> expectError

            Expect.contains
                errors
                (ProcessCoreWritebackError.PropertyNotFound missingPropertyId)
                "Missing final property must be reported."

            Expect.equal
                fixture.Dataset.Processes.Count
                beforeCount
                "Valid earlier patches must not apply when a later patch is invalid."

            Expect.isFalse
                (fixture.Dataset.AllSamples()
                 |> Seq.exists (fun sample -> sample.Name = "must-not-appear"))
                "The valid earlier set patch must remain unapplied."

        testCase "rejects structural creation in previous context"
        <| fun _ ->
            let arc, _ = withPreviousContext ()
            let converted = fromArc loadedTable arc |> expectOk
            let _, previousProperty = propertyByName "previous-parameter" converted.Model

            let previousSource =
                match previousProperty.Origin with
                | ProvenancePropertyOrigin.Real anchor -> anchor.Source
                | other -> failtestf "Expected real previous origin but received %A" other

            let forged = {
                Session.init converted.Model with
                    PatchLog = [
                        ProvenanceTablePatch.AddLoadedSet(
                            ProvenanceSide.Output,
                            previousSource.Name,
                            {
                                Kind = ProcessCoreKinds.sampleEndpoint
                                Text = "Sample"
                            },
                            "forbidden-previous-set"
                        )
                    ]
            }

            let beforeCount = arc.AllProcesses().Count
            let errors = writeBack converted.Index forged arc |> expectError

            Expect.contains
                errors
                (ProcessCoreWritebackError.StructuralPreviousContextEdit previousSource.Id)
                "Previous context must allow value updates only, not structural creation."

            Expect.equal (arc.AllProcesses().Count) beforeCount "Rejected previous structure must not mutate the graph."

        testCase "reconverts the complete final session from the mutated ARC"
        <| fun _ ->
            let arc, _, _ = annotated ()
            let dataset = arc.HasPart.[0]
            let converted = fromArc loadedTable arc |> expectOk
            let categoryId, _ = propertyByName "category-neutral" converted.Model

            let initialConnectionId =
                converted.Model.Connections |> Map.toList |> List.head |> fst

            let afterValue =
                Session.init converted.Model
                |> update categoryId (ProvenanceValue.Text "roundtrip-value") None

            let afterRemoval =
                Session.removeConnection initialConnectionId afterValue |> expectOk |> fst

            let afterSet =
                afterRemoval
                |> createSet
                    ProvenanceSide.Output
                    {
                        Kind = ProcessCoreKinds.sampleEndpoint
                        Text = "Sample"
                    }
                    "roundtrip-output"

            let initialLayer = Session.activeLayer afterSet

            let inputId =
                initialLayer.Model.InputSets
                |> Map.toList
                |> List.find (fun (_, set) -> set.Name = "input-neutral")
                |> fst

            let originalOutputId =
                initialLayer.Model.OutputSets
                |> Map.toList
                |> List.find (fun (_, set) -> set.Name = "output-neutral")
                |> fst

            let addedOutputId =
                initialLayer.Model.OutputSets
                |> Map.toList
                |> List.find (fun (_, set) -> set.Name = "roundtrip-output")
                |> fst

            let afterConnection = connect inputId addedOutputId afterSet

            let retainedConnectionId =
                (Session.activeLayer afterConnection).Model.Connections
                |> Map.toList
                |> List.find (fun (_, connection) -> connection.OutputSetId = addedOutputId)
                |> fst

            let afterCharacteristic =
                afterConnection
                |> createProperty
                    (ProvenancePropertyTarget.OutputSets [ originalOutputId ])
                    ProcessCoreKinds.characteristic
                    "roundtrip-characteristic"
                    "roundtrip-characteristic-value"

            let afterParameter =
                afterCharacteristic
                |> createProperty
                    (ProvenancePropertyTarget.Connections [ retainedConnectionId ])
                    ProcessCoreKinds.parameter
                    "roundtrip-parameter"
                    "roundtrip-parameter-value"

            let withUnfinished =
                addLayer "roundtrip-unfinished" [ ProvenanceSide.Output, addedOutputId ] afterParameter

            let finalSession = addLayer "roundtrip-empty" [] withUnfinished

            writeBack converted.Index finalSession arc |> expectOk |> ignore

            let loadedAgain = fromArc loadedTable arc |> expectOk

            let pairs =
                loadedAgain.Model.Connections
                |> Map.toList
                |> List.map (fun (_, connection) ->
                    loadedAgain.Model.InputSets.[connection.InputSetId].Name,
                    loadedAgain.Model.OutputSets.[connection.OutputSetId].Name
                )

            Expect.contains pairs ("input-neutral", "roundtrip-output") "Retained added connection must reconvert."

            Expect.isFalse
                (pairs |> List.contains ("input-neutral", "output-neutral"))
                "Removed original connection must stay removed."

            let _, category = propertyByName "category-neutral" loadedAgain.Model
            Expect.equal category.Value (ProvenanceValue.Text "roundtrip-value") "Updated value must reconvert."
            propertyByName "roundtrip-characteristic" loadedAgain.Model |> ignore
            propertyByName "roundtrip-parameter" loadedAgain.Model |> ignore

            let unfinishedLocation = {
                loadedTable with
                    TableName = "roundtrip-unfinished"
            }

            let unfinished = fromArc unfinishedLocation arc |> expectOk

            Expect.sequenceEqual
                (unfinished.Model.InputSets |> Map.toList |> List.map (fun (_, set) -> set.Name))
                [ "roundtrip-output" ]
                "Reference-linked input must reconvert."

            Expect.isEmpty unfinished.Model.OutputSets "Unfinished layer must remain output-free."
            Expect.isEmpty unfinished.Model.Connections "Unfinished layer must remain disconnected."

            let emptyLocation = {
                loadedTable with
                    TableName = "roundtrip-empty"
            }

            let empty = fromArc emptyLocation arc |> expectOk
            Expect.isEmpty empty.Model.InputSets "Empty layer must remain input-free."
            Expect.isEmpty empty.Model.OutputSets "Empty layer must remain output-free."
            Expect.isEmpty empty.Model.Connections "Empty layer must remain connection-free."

            let newGroupOrder =
                dataset.Processes
                |> Seq.map (fun proc -> proc.Name)
                |> Seq.filter (fun name -> name.StartsWith("roundtrip-"))
                |> Seq.distinct
                |> Seq.toList

            Expect.sequenceEqual
                newGroupOrder
                [ "roundtrip-unfinished"; "roundtrip-empty" ]
                "New groups must be appended in session layer order."

        testCase "does not persist through the ARC path"
        <| fun _ ->
            let fixture = basic ()

            let isolatedPath =
                System.IO.Path.Combine(
                    System.IO.Path.GetTempPath(),
                    "swate-processcore-" + System.Guid.NewGuid().ToString("N")
                )

            fixture.Arc.ArcPath <- Some isolatedPath
            let converted = fromArc loadedTable fixture.Arc |> expectOk
            let outputId = converted.Model.OutputSets |> Map.toList |> List.head |> fst

            let session =
                Session.init converted.Model
                |> createProperty
                    (ProvenancePropertyTarget.OutputSets [ outputId ])
                    ProcessCoreKinds.factor
                    "memory-only"
                    "value-neutral"

            writeBack converted.Index session fixture.Arc |> expectOk |> ignore
            Expect.isFalse (System.IO.Directory.Exists isolatedPath) "Adapter must not write through ARC.ArcPath."
    ]

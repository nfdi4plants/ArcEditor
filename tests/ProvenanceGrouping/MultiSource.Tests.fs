module ProcessCoreMultiSourceTests

open Expecto
open ProcessCore
open ProcessCoreProvenanceFixtures
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreAdapterTypes
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreConverter
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreWriteback

module CanonicalCommands = Swate.Components.Page.ProvenanceGrouping.Commands
module CanonicalProjectionTypes = Swate.Components.Page.ProvenanceGrouping.ProjectionTypes
module CanonicalSession = Swate.Components.Page.ProvenanceGrouping.CanonicalSession
module CanonicalValues = Swate.Components.Page.ProvenanceGrouping.Values

let private chainedTables () =
    let input = Sample("chain-input")
    let mid = Sample("chain-mid")
    let output = Sample("chain-output")

    let parameterOne =
        Annotation("param-one", value = "1", additionalType = "ParameterValue")

    let parameterTwo =
        Annotation("param-two", value = "2", additionalType = "ParameterValue")

    let stageOne =
        mkProcessFull "stage-one" None [ SampleNode input ] [ SampleNode mid ] [ parameterOne ]

    let stageTwo =
        mkProcessFull "stage-two" None [ SampleNode mid ] [ SampleNode output ] [ parameterTwo ]

    let dataset = Dataset("dataset-neutral", processes = [ stageOne; stageTwo ])
    let arc = ARC("arc-neutral", hasPart = [ dataset ])
    arc, dataset, parameterOne, parameterTwo

let private canonicalTable groupName : ProcessCoreProcessGroupLocation = {
    DatasetPath = [ "arc-neutral"; "dataset-neutral" ]
    ProcessGroupName = groupName
}

let private processCountByName (dataset: Dataset) name =
    dataset.Processes |> Seq.filter (fun proc -> proc.Name = name) |> Seq.length

/// Both chained groups as one canonical session, which is what the renderer
/// loads: `loadCanonical` calls `fromArcMany` once for the whole selection.
let private loadChainedCanonical arc =
    fromArcMany [ canonicalTable "stage-one"; canonicalTable "stage-two" ] arc
    |> expectOk

let private prepareCanonical (session: CanonicalProjectionTypes.ProvenanceSession) =
    CanonicalSession.prepareForWriteback session |> expectOk

let private commitCanonical effect (session: CanonicalProjectionTypes.ProvenanceSession) =
    CanonicalSession.commit effect session

let private assignmentByCategory
    categoryName
    (session: CanonicalProjectionTypes.ProvenanceSession)
    : Swate.Components.Page.ProvenanceGrouping.Domain.ProcessAssignment =
    session.Processes
    |> Map.toList
    |> List.collect (fun (_, structuralProcess) -> structuralProcess.Assignments |> Map.toList |> List.map snd)
    |> List.find (fun assignment ->
        let definition = session.Values[assignment.ValueId]
        session.Properties[definition.PropertyId].Category.Name = categoryName
    )

let private editValue categoryName newValue (session: CanonicalProjectionTypes.ProvenanceSession) =
    let assignment = assignmentByCategory categoryName session

    CanonicalCommands.editValueGlobally
        assignment.ValueId
        {
            Category = {
                Name = categoryName
                TermSource = None
                TermAccession = None
            }
            Value = newValue
            Unit = None
        }
        session
    |> expectOk
    |> fun effect -> commitCanonical effect session

let private nodeValueContent categoryName value : CanonicalCommands.NodeValueContent = {
    Category = {
        Name = categoryName
        TermSource = None
        TermAccession = None
    }
    Value = value
    Unit = None
}

let tests =
    testList "ProcessCore multi-source sessions" [
        testList "fromArcMany" [
            testCase "equal kind and name nodes from different layers and sources resolve to one canonical node"
            <| fun _ ->
                let firstShared = Sample("shared", additionalType = "First header")
                let secondShared = Sample("shared", additionalType = "Second header")

                let firstEarlier =
                    mkProcess "stage-one" [ SampleNode(Sample("earlier-source")) ] [
                        SampleNode(Sample("earlier-output"))
                    ]

                let first =
                    mkProcess "stage-one" [ SampleNode(Sample("source")) ] [ SampleNode firstShared ]

                let second =
                    mkProcess "stage-two" [ SampleNode secondShared ] [ SampleNode(Sample("result")) ]

                let dataset =
                    Dataset("dataset-neutral", processes = [ firstEarlier; first; second ])

                let arc = ARC("arc-neutral", hasPart = [ dataset ])

                // ProcessCore normally interns equal-key nodes and therefore
                // keeps only one AdditionalType. Its public mutable boundary
                // can still expose a detached occurrence with a distinct
                // header, which is the adversarial input needed to prove that
                // converter identity ignores appearance metadata.
                second.ProcessOf <- None
                second.SetInput(SampleNode secondShared)
                second.ProcessOf <- Some dataset

                let result =
                    fromArcMany [ canonicalTable "stage-one"; canonicalTable "stage-two" ] arc
                    |> expectOk

                let sharedNodes =
                    result.Session.Nodes
                    |> Map.toList
                    |> List.filter (fun (_, node) -> node.Name = "shared")

                Expect.equal sharedNodes.Length 1 "Exact kind/name equality must canonicalize across layers."
                let sharedId, _ = List.exactlyOne sharedNodes
                let firstLayer = result.Session.Layers[result.Session.LayerOrder[0]]
                let secondLayer = result.Session.Layers[result.Session.LayerOrder[1]]
                Expect.isTrue (firstLayer.OutputEndpoints.ContainsKey sharedId) "The first appearance is an output."
                Expect.isTrue (secondLayer.InputEndpoints.ContainsKey sharedId) "The second appearance is an input."

                Expect.equal
                    result.Index.NodeLocations[sharedId].Length
                    2
                    "Both exact source occurrences remain indexed."

                Expect.equal
                    firstLayer.OutputEndpoints[sharedId].Header.Text
                    "First header"
                    "The first layer keeps its source-specific appearance header."

                Expect.equal
                    secondLayer.InputEndpoints[sharedId].Header.Text
                    "Second header"
                    "The second layer keeps its different source-specific appearance header."

                Expect.equal
                    firstLayer.OutputEndpoints[sharedId].LayerOrderPosition
                    1
                    "The later first-layer occurrence retains its source order."

                Expect.equal
                    secondLayer.InputEndpoints[sharedId].LayerOrderPosition
                    0
                    "The earlier second-layer occurrence retains its independent source order."

                Expect.equal
                    (result.Index.NodeLocations[sharedId] |> List.map _.SourceOrderHint |> Set.ofList)
                    (Set.ofList [ 0; 1 ])
                    "Source-order hints remain indexed but do not split canonical identity."

                Expect.equal result.Session.LayerOrder.Length 2 "Input location order must become layer order."

                Expect.equal
                    result.Session.LayerProjections.Count
                    2
                    "Every loaded layer must have an initial projection."

            testCase "canonical node annotations include every distinct physical occurrence"
            <| fun _ ->
                let firstShared = Sample("shared")
                firstShared.AddAdditionalProperty(Annotation("first-category", value = "first-value"))
                let secondShared = Sample("shared")
                secondShared.AddAdditionalProperty(Annotation("second-category", value = "second-value"))

                let first =
                    mkProcess "stage-one" [ SampleNode(Sample("source")) ] [ SampleNode firstShared ]

                let second =
                    mkProcess "stage-two" [ SampleNode secondShared ] [ SampleNode(Sample("result")) ]

                let dataset = Dataset("dataset-neutral", processes = [ first; second ])
                let arc = ARC("arc-neutral", hasPart = [ dataset ])

                second.ProcessOf <- None
                second.SetInput(SampleNode secondShared)
                second.ProcessOf <- Some dataset

                let result =
                    fromArcMany [ canonicalTable "stage-one"; canonicalTable "stage-two" ] arc
                    |> expectOk

                let sharedNode =
                    result.Session.Nodes
                    |> Map.toList
                    |> List.map snd
                    |> List.find (fun node -> node.Name = "shared")

                let categories =
                    sharedNode.Assignments
                    |> Map.toList
                    |> List.map (fun (_, assignment) ->
                        let value = result.Session.Values[assignment.ValueId]
                        result.Session.Properties[value.PropertyId].Category.Name
                    )
                    |> Set.ofList

                Expect.equal
                    categories
                    (Set.ofList [ "first-category"; "second-category" ])
                    "Canonical ownership must merge, not discard, annotations from distinct equal-key source nodes."

                Expect.equal
                    (sharedNode.Assignments
                     |> Map.toList
                     |> List.sumBy (fun (assignmentId, _) -> result.Index.AssignmentLocations[assignmentId].Length))
                    2
                    "Every physical annotation occurrence must retain an indexed source location."

            testCase "distinct process-group tuples cannot collide through slash-containing segments"
            <| fun _ ->
                let firstProcess =
                    mkProcess "b/c" [ SampleNode(Sample("first-input")) ] [ SampleNode(Sample("first-output")) ]

                let secondProcess =
                    mkProcess "c" [ SampleNode(Sample("second-input")) ] [ SampleNode(Sample("second-output")) ]

                let firstDataset = Dataset("a", processes = [ firstProcess ])
                let secondDataset = Dataset("a/b", processes = [ secondProcess ])
                let arc = ARC("arc", hasPart = [ firstDataset; secondDataset ])

                let firstLocation = {
                    DatasetPath = [ "arc"; "a" ]
                    ProcessGroupName = "b/c"
                }

                let secondLocation = {
                    DatasetPath = [ "arc"; "a/b" ]
                    ProcessGroupName = "c"
                }

                let result = fromArcMany [ firstLocation; secondLocation ] arc |> expectOk

                Expect.equal result.Index.SourceLocations.Count 2 "Distinct source tuples need collision-free IDs."
                Expect.equal result.Session.Layers.Count 2 "Both distinct locations must retain their own layer."
        ]


        testList "canonicalWriteBackMany" [
            testCase "routes value edits to each loaded group's annotations"
            <| fun _ ->
                let arc, _, parameterOne, parameterTwo = chainedTables ()
                let converted = loadChainedCanonical arc

                let edited =
                    converted.Session
                    |> editValue "param-one" (CanonicalValues.ProvenanceValue.Integer 9)
                    |> editValue "param-two" (CanonicalValues.ProvenanceValue.Integer 11)
                    |> prepareCanonical

                let summary = canonicalWriteBackMany converted.Index edited arc |> expectOk

                Expect.equal summary.UpdatedAnnotations 2 "One annotation per loaded group must be updated."
                Expect.equal parameterOne.Value (Some "9") "stage-one's parameter receives its own edit."
                Expect.equal parameterTwo.Value (Some "11") "stage-two's parameter receives its own edit."

            testCase "an untouched multi-source session writes back as a no-op"
            <| fun _ ->
                let arc, dataset, _, _ = chainedTables ()
                let converted = loadChainedCanonical arc
                let payload = arc.toYamlString ()

                let summary =
                    canonicalWriteBackMany converted.Index (prepareCanonical converted.Session) arc
                    |> expectOk

                Expect.equal summary.AddedProcesses 0 "Nothing was edited."
                Expect.equal summary.RemovedProcesses 0 "Nothing was edited."
                Expect.equal summary.UpdatedAnnotations 0 "Nothing was edited."
                Expect.equal summary.AddedAnnotations 0 "Nothing was edited."
                Expect.equal (Seq.length dataset.Processes) 2 "The dataset keeps its two loaded processes."
                Expect.equal (arc.toYamlString ()) payload "A no-op save leaves the ARC byte-identical."

            testCase "a structural edit materializes under the edited group only"
            <| fun _ ->
                let arc, dataset, _, _ = chainedTables ()
                let converted = loadChainedCanonical arc

                // stage-two's layer, so the new row must belong to stage-two.
                let stageTwoLayerId =
                    converted.Session.Layers
                    |> Map.toList
                    |> List.find (fun (_, layer) -> layer.Source.Name = "stage-two")
                    |> fst

                let inputNodeId =
                    converted.Session.Layers[stageTwoLayerId].InputEndpoints
                    |> Map.toList
                    |> List.head
                    |> fst

                let withEndpoint =
                    CanonicalCommands.addEndpoint
                        stageTwoLayerId
                        Swate.Components.Page.ProvenanceGrouping.Identifiers.ProvenanceSide.Output
                        ProcessCoreCanonicalKinds.sampleEndpoint
                        {
                            Kind = ProcessCoreCanonicalKinds.sampleEndpoint
                            Text = ProcessCoreCanonicalKinds.sampleEndpoint.Label
                        }
                        "extra-output"
                        9
                        converted.Session
                    |> expectOk
                    |> fun effect -> commitCanonical effect converted.Session

                let newNodeId =
                    withEndpoint.Layers[stageTwoLayerId].OutputEndpoints
                    |> Map.toList
                    |> List.map fst
                    |> List.find (fun nodeId -> withEndpoint.Nodes[nodeId].Name = "extra-output")

                let connected =
                    CanonicalCommands.connectNodes stageTwoLayerId [ inputNodeId, newNodeId ] withEndpoint
                    |> expectOk
                    |> fun effect -> commitCanonical effect withEndpoint
                    |> prepareCanonical

                let summary = canonicalWriteBackMany converted.Index connected arc |> expectOk

                Expect.equal summary.AddedProcesses 1 "The new link materializes one new process row."
                Expect.equal (processCountByName dataset "stage-two") 2 "The new row belongs to stage-two."
                Expect.equal (processCountByName dataset "stage-one") 1 "stage-one must stay untouched."

            testCase "editing a value with several indexed physical occurrences updates and counts every occurrence"
            <| fun _ ->
                let firstShared = Sample("shared")

                firstShared.AddAdditionalProperty(
                    Annotation("category-neutral", value = "before", additionalType = "CharacteristicValue")
                )

                let secondShared = Sample("shared")

                secondShared.AddAdditionalProperty(
                    Annotation("category-neutral", value = "before", additionalType = "CharacteristicValue")
                )

                let first =
                    mkProcess "stage-one" [ SampleNode(Sample("source")) ] [ SampleNode firstShared ]

                let second =
                    mkProcess "stage-two" [ SampleNode secondShared ] [ SampleNode(Sample("result")) ]

                let dataset = Dataset("dataset-neutral", processes = [ first; second ])
                let arc = ARC("arc-neutral", hasPart = [ dataset ])

                // Without detaching and re-setting, ProcessCore's own node
                // interning would collapse these into the same live object
                // before the converter ever sees two physical occurrences.
                second.ProcessOf <- None
                second.SetInput(SampleNode secondShared)
                second.ProcessOf <- Some dataset

                let result =
                    fromArcMany [ canonicalTable "stage-one"; canonicalTable "stage-two" ] arc
                    |> expectOk

                let sharedNode =
                    result.Session.Nodes
                    |> Map.toList
                    |> List.map snd
                    |> List.find (fun node -> node.Name = "shared")

                Expect.equal
                    sharedNode.Assignments.Count
                    1
                    "Two exact-duplicate physical occurrences at the same position collapse into one assignment."

                let assignmentId, assignment =
                    sharedNode.Assignments |> Map.toList |> List.exactlyOne

                Expect.equal
                    result.Index.AssignmentLocations[assignmentId].Length
                    2
                    "Both physical occurrences remain indexed under the one collapsed assignment."

                let content =
                    nodeValueContent "category-neutral" (CanonicalValues.ProvenanceValue.Text "after")

                let edited =
                    CanonicalCommands.editValueGlobally assignment.ValueId content result.Session
                    |> expectOk
                    |> fun effect -> commitCanonical effect result.Session
                    |> prepareCanonical

                let summary = canonicalWriteBackMany result.Index edited arc |> expectOk

                Expect.equal
                    summary.UpdatedAnnotations
                    2
                    "Editing the one collapsed assignment updates both of its physical occurrences."

                Expect.equal
                    (Seq.exactlyOne firstShared.AdditionalProperty).Value
                    (Some "after")
                    "The first physical occurrence receives the new value."

                Expect.equal
                    (Seq.exactlyOne secondShared.AdditionalProperty).Value
                    (Some "after")
                    "The second physical occurrence receives the new value."

                let reloaded =
                    fromArcMany [ canonicalTable "stage-one"; canonicalTable "stage-two" ] arc
                    |> expectOk

                let reloadedSharedNode =
                    reloaded.Session.Nodes
                    |> Map.toList
                    |> List.map snd
                    |> List.find (fun node -> node.Name = "shared")

                Expect.equal reloadedSharedNode.Assignments.Count 1 "The reload still sees one assignment, not two."

                let reloadedAssignment =
                    reloadedSharedNode.Assignments |> Map.toList |> List.exactlyOne |> snd

                Expect.equal
                    reloaded.Session.Values[reloadedAssignment.ValueId].Value
                    (CanonicalValues.ProvenanceValue.Text "after")
                    "The edited value survives reload."
        ]
    ]

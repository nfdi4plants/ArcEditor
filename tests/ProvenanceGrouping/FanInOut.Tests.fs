module ProcessCoreFanInOutTests

open Expecto
open ProcessCore
open ProcessCoreProvenanceFixtures
open Swate.Components.Page.ProvenanceGrouping.ProvenanceTypes
open Swate.Components.Page.ProvenanceGrouping.Edit
open Swate.Components.Page.ProvenanceGrouping.Session
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreAdapterTypes
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreConverter
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreWriteback

/// Two processes in one group feeding one shared output sample - the fan-in
/// mirror of the `allToAll` fan-out fixture. Each edge belongs to its own
/// ProcessCore process, so connection-targeted parameters must separate.
let private fanIn () =
    let inputOne = Sample("fan-input-one")
    let inputTwo = Sample("fan-input-two")
    let shared = Sample("fan-output-shared")

    let processOne =
        mkProcess "stage-neutral" [ SampleNode inputOne ] [ SampleNode shared ]

    let processTwo =
        mkProcess "stage-neutral" [ SampleNode inputTwo ] [ SampleNode shared ]

    let dataset = Dataset("dataset-neutral", processes = [ processOne; processTwo ])

    let arc = ARC("arc-neutral", hasPart = [ dataset ])
    arc, dataset, processOne, processTwo, shared

let private update propertyId value unit session =
    Session.updatePropertyValue propertyId value unit session |> expectOk |> fst

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

let private removeConnection connectionId session =
    Session.removeConnection connectionId session |> expectOk |> fst

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

let private inputSetIdByName name (model: ProvenanceModel) =
    model.InputSets
    |> Map.toList
    |> List.find (fun (_, set) -> set.Name = name)
    |> fst

let private outputSetIdByName name (model: ProvenanceModel) =
    model.OutputSets
    |> Map.toList
    |> List.find (fun (_, set) -> set.Name = name)
    |> fst

let private connectionIdByOutputName name (model: ProvenanceModel) =
    model.Connections
    |> Map.toList
    |> List.find (fun (_, connection) -> model.OutputSets.[connection.OutputSetId].Name = name)
    |> fst

let private connectionIdByInputName name (model: ProvenanceModel) =
    model.Connections
    |> Map.toList
    |> List.find (fun (_, connection) -> model.InputSets.[connection.InputSetId].Name = name)
    |> fst

let private effectiveCategoryNames (model: ProvenanceModel) (set: ProvenanceSet) =
    ProvenanceSet.effectivePropertyValueIds set
    |> List.choose (fun id -> model.PropertyValues.TryFind id)
    |> List.map (fun value -> value.Header.Category.Name)

let private nodeAnnotationNames (node: Sample) =
    node.AdditionalProperty
    |> Seq.map (fun annotation -> annotation.Name)
    |> List.ofSeq

module CanonicalProjectionTypes = Swate.Components.Page.ProvenanceGrouping.ProjectionTypes
module CanonicalSession = Swate.Components.Page.ProvenanceGrouping.CanonicalSession
module CanonicalValues = Swate.Components.Page.ProvenanceGrouping.Values

let private canonicalLocation: ProcessCoreProcessGroupLocation = {
    DatasetPath = [ "arc-neutral"; "dataset-neutral" ]
    ProcessGroupName = "stage-neutral"
}

let private convertCanonical locations arc = fromArcMany locations arc |> expectOk

let private prepareCanonical (session: CanonicalProjectionTypes.ProvenanceSession) =
    CanonicalSession.prepareForWriteback session |> expectOk

/// Three explicit pairs over two inputs and two outputs: Cartesian inference
/// would add a fourth, so the missing pair is the assertion that matters.
let private explicitPairs = [
    "fan-input-one", "fan-output-one"
    "fan-input-one", "fan-output-two"
    "fan-input-two", "fan-output-one"
]

/// One process group whose processes materialize exactly the requested pairs,
/// each endpoint name interned to one shared ProcessCore node.
let private canonicalPairFixture (pairs: (string * string) list) =
    let nodes = System.Collections.Generic.Dictionary<string, Sample>()

    let node name =
        match nodes.TryGetValue name with
        | true, existing -> existing
        | false, _ ->
            let created = Sample(name)
            nodes[name] <- created
            created

    let processes =
        pairs
        |> List.map (fun (input, output) ->
            mkProcess "stage-neutral" [ SampleNode(node input) ] [ SampleNode(node output) ]
        )

    let dataset = Dataset("dataset-neutral", processes = processes)
    ARC("arc-neutral", hasPart = [ dataset ]), dataset

let private canonicalPairs (dataset: Dataset) =
    dataset.Processes
    |> Seq.choose (fun proc ->
        match proc.Input, proc.Output with
        | Some input, Some output -> Some(input.AsSample().Name, output.AsSample().Name)
        | _ -> None
    )
    |> List.ofSeq

let private canonicalLinkPairs (converted: ProcessCoreCanonicalConversionResult) =
    converted.Session.Processes
    |> Map.toList
    |> List.collect (fun (_, structuralProcess) ->
        structuralProcess.Links
        |> Map.toList
        |> List.choose (fun (_, link) ->
            match link.Shape with
            | CanonicalValues.ProcessLinkShape.Between(inputId, outputId) ->
                Some(converted.Session.Nodes[inputId].Name, converted.Session.Nodes[outputId].Name)
            | _ -> None
        )
    )

let tests =
    testList "ProcessCore fan-in/fan-out property assignment" [
        testCase "fan-out: inherited property retracts from the disconnected endpoint only"
        <| fun _ ->
            // allToAll: input-one -> output-one, output-two.
            let arc, _, _ = allToAll ()
            let converted = fromArc loadedTable arc |> expectOk
            let inputId = inputSetIdByName "input-one" converted.Model

            let session =
                Session.init converted.Model
                |> createProperty
                    (ProvenancePropertyTarget.InputSets [ inputId ])
                    ProcessCoreKinds.characteristic
                    "shared-characteristic"
                    "shared-value"

            let model = (Session.activeLayer session).Model
            let outputOne = model.OutputSets.[outputSetIdByName "output-one" model]
            let outputTwo = model.OutputSets.[outputSetIdByName "output-two" model]

            Expect.contains
                (effectiveCategoryNames model outputOne)
                "shared-characteristic"
                "Both outputs inherit the input's property through their edges."

            Expect.contains
                (effectiveCategoryNames model outputTwo)
                "shared-characteristic"
                "Both outputs inherit the input's property through their edges."

            let removedId = connectionIdByOutputName "output-two" model
            let session = removeConnection removedId session
            let model = (Session.activeLayer session).Model
            let outputOne = model.OutputSets.[outputSetIdByName "output-one" model]
            let outputTwo = model.OutputSets.[outputSetIdByName "output-two" model]

            Expect.contains
                (effectiveCategoryNames model outputOne)
                "shared-characteristic"
                "The still-connected output keeps the inherited property."

            Expect.isFalse
                (effectiveCategoryNames model outputTwo |> List.contains "shared-characteristic")
                "The disconnected output loses the inherited property."

        testCase "fan-out: set property survives an edge removal and lands on its own node only"
        <| fun _ ->
            let arc, dataset, _ = allToAll ()
            let converted = fromArc loadedTable arc |> expectOk
            let inputId = inputSetIdByName "input-one" converted.Model
            let removedId = connectionIdByOutputName "output-two" converted.Model

            let session =
                Session.init converted.Model
                |> createProperty
                    (ProvenancePropertyTarget.InputSets [ inputId ])
                    ProcessCoreKinds.characteristic
                    "owner-characteristic"
                    "owner-value"
                |> removeConnection removedId

            writeBack converted.Index session arc |> expectOk |> ignore

            let inputNode =
                dataset.Processes
                |> Seq.collect (fun proc -> proc.Input |> Option.toList)
                |> Seq.pick (
                    function
                    | SampleNode sample when sample.Name = "input-one" -> Some sample
                    | _ -> None
                )

            Expect.contains
                (nodeAnnotationNames inputNode)
                "owner-characteristic"
                "The owning input node stores the annotation."

            let outputNodes =
                dataset.Processes
                |> Seq.collect (fun proc -> proc.Output |> Option.toList)
                |> Seq.choose (
                    function
                    | SampleNode sample -> Some sample
                    | _ -> None
                )

            for node in outputNodes do
                Expect.isFalse
                    (nodeAnnotationNames node |> List.contains "owner-characteristic")
                    $"Inherited display never materializes on the other side's node ({node.Name})."

            // The full final session reconverts - fan-out roundtrip stays coherent.
            let reconverted = fromArc loadedTable arc |> expectOk
            let model = reconverted.Model
            let inputSet = model.InputSets.[inputSetIdByName "input-one" model]

            Expect.contains
                (effectiveCategoryNames model inputSet)
                "owner-characteristic"
                "Reconversion reads the annotation back on the input set."

            Expect.equal model.Connections.Count 1 "Only the surviving edge reconverts."

        testCase "fan-out: group-targeted property stays on every member when an edge is removed"
        <| fun _ ->
            // Dropping onto a (display) group targets each member set id directly,
            // so later structural changes must not move or drop it.
            let arc, dataset, _ = allToAll ()
            let converted = fromArc loadedTable arc |> expectOk
            let outputOneId = outputSetIdByName "output-one" converted.Model
            let outputTwoId = outputSetIdByName "output-two" converted.Model
            let removedId = connectionIdByOutputName "output-two" converted.Model

            let session =
                Session.init converted.Model
                |> createProperty
                    (ProvenancePropertyTarget.OutputSets [ outputOneId; outputTwoId ])
                    ProcessCoreKinds.characteristic
                    "group-characteristic"
                    "group-value"
                |> removeConnection removedId

            writeBack converted.Index session arc |> expectOk |> ignore

            let outputNodes =
                dataset.Processes
                |> Seq.collect (fun proc -> proc.Output |> Option.toList)
                |> Seq.choose (
                    function
                    | SampleNode sample -> Some sample
                    | _ -> None
                )
                |> Seq.distinctBy (fun sample -> sample.Name)
                |> List.ofSeq

            Expect.equal outputNodes.Length 2 "Both output nodes survive the edge removal."

            for node in outputNodes do
                Expect.contains
                    (nodeAnnotationNames node)
                    "group-characteristic"
                    $"Every group member keeps its directly assigned annotation ({node.Name})."

        testCase "fan-in: node property on the shared output is written once to the shared node"
        <| fun _ ->
            let arc, _, _, _, shared = fanIn ()
            let converted = fromArc loadedTable arc |> expectOk

            Expect.equal converted.Model.Connections.Count 2 "Two edges converge on the shared output."

            let sharedId = outputSetIdByName "fan-output-shared" converted.Model

            let session =
                Session.init converted.Model
                |> createProperty
                    (ProvenancePropertyTarget.OutputSets [ sharedId ])
                    ProcessCoreKinds.characteristic
                    "fan-in-characteristic"
                    "fan-in-value"

            writeBack converted.Index session arc |> expectOk |> ignore

            let occurrences =
                nodeAnnotationNames shared |> List.filter ((=) "fan-in-characteristic")

            Expect.equal occurrences.Length 1 "The shared node stores the annotation exactly once."

        testCase "fan-in: a connection parameter reaches only its own process"
        <| fun _ ->
            let arc, _, processOne, processTwo, _ = fanIn ()
            let converted = fromArc loadedTable arc |> expectOk
            let edgeOne = connectionIdByInputName "fan-input-one" converted.Model

            let session =
                Session.init converted.Model
                |> createProperty
                    (ProvenancePropertyTarget.Connections [ edgeOne ])
                    ProcessCoreKinds.parameter
                    "fan-in-parameter"
                    "parameter-value"

            writeBack converted.Index session arc |> expectOk |> ignore

            Expect.isTrue
                (processOne.ParameterValue
                 |> Seq.exists (fun annotation -> annotation.Name = "fan-in-parameter"))
                "The targeted edge's process receives the parameter."

            Expect.isFalse
                (processTwo.ParameterValue
                 |> Seq.exists (fun annotation -> annotation.Name = "fan-in-parameter"))
                "The other process of the fan-in must not receive it."

        testCase "fan-in: a connection parameter survives removing the other edge"
        <| fun _ ->
            let arc, _, processOne, processTwo, _ = fanIn ()
            let converted = fromArc loadedTable arc |> expectOk
            let edgeOne = connectionIdByInputName "fan-input-one" converted.Model
            let edgeTwo = connectionIdByInputName "fan-input-two" converted.Model

            let session =
                Session.init converted.Model
                |> createProperty
                    (ProvenancePropertyTarget.Connections [ edgeOne ])
                    ProcessCoreKinds.parameter
                    "surviving-parameter"
                    "parameter-value"
                |> removeConnection edgeTwo

            writeBack converted.Index session arc |> expectOk |> ignore

            Expect.isTrue
                (processOne.ParameterValue
                 |> Seq.exists (fun annotation -> annotation.Name = "surviving-parameter"))
                "The targeted process keeps the parameter after the other edge is removed."

            Expect.isFalse
                (processTwo.ParameterValue
                 |> Seq.exists (fun annotation -> annotation.Name = "surviving-parameter"))
                "The removed edge's process never receives it."

        testCase "fan-in: removing the assigned connection also retracts its parameter"
        <| fun _ ->
            let arc, _, processOne, processTwo, _ = fanIn ()
            let converted = fromArc loadedTable arc |> expectOk
            let edgeOne = connectionIdByInputName "fan-input-one" converted.Model

            let session =
                Session.init converted.Model
                |> createProperty
                    (ProvenancePropertyTarget.Connections [ edgeOne ])
                    ProcessCoreKinds.parameter
                    "retracted-parameter"
                    "parameter-value"
                |> removeConnection edgeOne

            // The editor no longer shows the value anywhere: it belonged to
            // the removed edge, so it leaves the model with it.
            let model = (Session.activeLayer session).Model

            Expect.isFalse
                (model.PropertyValues
                 |> Map.exists (fun _ value -> value.Header.Category.Name = "retracted-parameter"))
                "The retracted value must leave the session model."

            for _, set in (model.InputSets |> Map.toList) @ (model.OutputSets |> Map.toList) do
                Expect.isFalse
                    (effectiveCategoryNames model set |> List.contains "retracted-parameter")
                    $"No set may keep displaying the retracted value ({set.Name})."

            let result = writeBack converted.Index session arc

            match result with
            | Ok _ ->
                Expect.isFalse
                    (processOne.ParameterValue
                     |> Seq.exists (fun annotation -> annotation.Name = "retracted-parameter"))
                    "A parameter assigned through a removed edge must not be written to its old process."

                Expect.isFalse
                    (processTwo.ParameterValue
                     |> Seq.exists (fun annotation -> annotation.Name = "retracted-parameter"))
                    "The unrelated process must not receive it either."
            | Error errors -> failtestf "Writeback failed instead of retracting the parameter: %A" errors

        testCase "fan-in: removing one edge of a two-edge assignment keeps the other edge's share"
        <| fun _ ->
            let arc, _, processOne, processTwo, _ = fanIn ()
            let converted = fromArc loadedTable arc |> expectOk
            let edgeOne = connectionIdByInputName "fan-input-one" converted.Model
            let edgeTwo = connectionIdByInputName "fan-input-two" converted.Model

            let session =
                Session.init converted.Model
                |> createProperty
                    (ProvenancePropertyTarget.Connections [ edgeOne; edgeTwo ])
                    ProcessCoreKinds.parameter
                    "shared-parameter"
                    "parameter-value"
                |> removeConnection edgeTwo

            // The editor keeps the value on the surviving edge's endpoints and
            // drops it from the removed edge's own input.
            let model = (Session.activeLayer session).Model
            let inputOne = model.InputSets.[inputSetIdByName "fan-input-one" model]
            let inputTwo = model.InputSets.[inputSetIdByName "fan-input-two" model]
            let shared = model.OutputSets.[outputSetIdByName "fan-output-shared" model]

            Expect.contains
                (effectiveCategoryNames model inputOne)
                "shared-parameter"
                "The surviving edge's input keeps the value."

            Expect.contains
                (effectiveCategoryNames model shared)
                "shared-parameter"
                "The shared output keeps the value - the surviving edge still carries it there."

            Expect.isFalse
                (effectiveCategoryNames model inputTwo |> List.contains "shared-parameter")
                "The removed edge's input loses the value."

            writeBack converted.Index session arc |> expectOk |> ignore

            Expect.isTrue
                (processOne.ParameterValue
                 |> Seq.exists (fun annotation -> annotation.Name = "shared-parameter"))
                "The surviving edge's process receives the parameter."

            Expect.isFalse
                (processTwo.ParameterValue
                 |> Seq.exists (fun annotation -> annotation.Name = "shared-parameter"))
                "The removed edge's process must not receive it."

        testCase "re-adding a removed edge does not resurrect its retracted value"
        <| fun _ ->
            // `connectSets` reuses freed connection ids, so the re-added edge
            // carries the exact id the retracted value was assigned to - the
            // value must stay gone and the save must stay clean regardless.
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
                    "extra-output"

            let model = (Session.activeLayer session).Model
            let inputId = model.InputSets |> Map.toList |> List.head |> fst
            let extraId = outputSetIdByName "extra-output" model
            let session = connect inputId extraId session

            let firstConnectionId =
                (Session.activeLayer session).Model.Connections
                |> Map.toList
                |> List.find (fun (_, connection) -> connection.OutputSetId = extraId)
                |> fst

            let session =
                session
                |> createProperty
                    (ProvenancePropertyTarget.Connections [ firstConnectionId ])
                    ProcessCoreKinds.parameter
                    "readd-parameter"
                    "parameter-value"
                |> removeConnection firstConnectionId
                |> connect inputId extraId

            let secondConnectionId =
                (Session.activeLayer session).Model.Connections
                |> Map.toList
                |> List.find (fun (_, connection) -> connection.OutputSetId = extraId)
                |> fst

            Expect.equal secondConnectionId firstConnectionId "Precondition: the freed connection id is reused."

            let model = (Session.activeLayer session).Model

            Expect.isFalse
                (model.PropertyValues
                 |> Map.exists (fun _ value -> value.Header.Category.Name = "readd-parameter"))
                "The re-added edge starts assignment-free - the retracted value stays gone."

            let summary = writeBack converted.Index session fixture.Arc |> expectOk

            Expect.equal summary.AddedAnnotations 0 "The stale assignment must not be written to the new edge."

            Expect.isFalse
                (fixture.Process.ParameterValue
                 |> Seq.exists (fun annotation -> annotation.Name = "readd-parameter"))
                "No process receives the retracted parameter."

        testCase "re-adding a removed edge restores inherited display for set-assigned values"
        <| fun _ ->
            let arc, _, _ = allToAll ()
            let converted = fromArc loadedTable arc |> expectOk
            let inputId = inputSetIdByName "input-one" converted.Model
            let removedId = connectionIdByOutputName "output-two" converted.Model

            let session =
                Session.init converted.Model
                |> createProperty
                    (ProvenancePropertyTarget.InputSets [ inputId ])
                    ProcessCoreKinds.characteristic
                    "returning-characteristic"
                    "returning-value"
                |> removeConnection removedId

            let model = (Session.activeLayer session).Model
            let outputTwo = model.OutputSets.[outputSetIdByName "output-two" model]

            Expect.isFalse
                (effectiveCategoryNames model outputTwo
                 |> List.contains "returning-characteristic")
                "The disconnected output loses the inherited display."

            let session = connect inputId (outputSetIdByName "output-two" model) session
            let model = (Session.activeLayer session).Model
            let outputTwo = model.OutputSets.[outputSetIdByName "output-two" model]

            Expect.contains
                (effectiveCategoryNames model outputTwo)
                "returning-characteristic"
                "Reconnecting restores the transitive display - the value never left its owner."

        testCase "fan-in: an updated connection parameter still retracts cleanly with its edge"
        <| fun _ ->
            let arc, _, processOne, processTwo, _ = fanIn ()
            let converted = fromArc loadedTable arc |> expectOk
            let edgeOne = connectionIdByInputName "fan-input-one" converted.Model

            let session =
                Session.init converted.Model
                |> createProperty
                    (ProvenancePropertyTarget.Connections [ edgeOne ])
                    ProcessCoreKinds.parameter
                    "edited-parameter"
                    "first-value"

            let propertyId =
                (Session.activeLayer session).Model.PropertyValues
                |> Map.toList
                |> List.find (fun (_, value) -> value.Header.Category.Name = "edited-parameter")
                |> fst

            let session =
                session
                |> update propertyId (ProvenanceValue.Text "second-value") None
                |> removeConnection edgeOne

            let summary = writeBack converted.Index session arc |> expectOk

            Expect.equal summary.AddedAnnotations 0 "Nothing may be written for the retracted value."
            Expect.equal summary.UpdatedAnnotations 0 "The stranded update patch must be retracted with it."

            for proc in [ processOne; processTwo ] do
                Expect.isFalse
                    (proc.ParameterValue
                     |> Seq.exists (fun annotation -> annotation.Name = "edited-parameter"))
                    $"No process may receive the retracted parameter ({proc.Name})."

        testCase "explicit all-to-all pairs round-trip as exact links"
        <| fun _ ->
            let arc, dataset = canonicalPairFixture explicitPairs
            let converted = convertCanonical [ canonicalLocation ] arc

            canonicalWriteBackMany converted.Index (prepareCanonical converted.Session) arc
            |> expectOk
            |> ignore

            Expect.equal
                (canonicalPairs dataset |> List.sort)
                (explicitPairs |> List.sort)
                "Every explicit pair survives."

            let reloaded = convertCanonical [ canonicalLocation ] arc

            Expect.equal
                (canonicalLinkPairs reloaded |> List.sort)
                (explicitPairs |> List.sort)
                "Reload reconstructs exactly the explicit pairs."

            Expect.isFalse
                (canonicalLinkPairs reloaded |> List.contains ("fan-input-two", "fan-output-two"))
                "No Cartesian pair is inferred from the independent endpoint collections."

        testCase "YAML grouping of equal-state singular processes preserves positional pairs and repeated endpoints"
        <| fun _ ->
            let repeatedPairs = [
                "fan-input-one", "fan-output-one"
                "fan-input-one", "fan-output-one"
                "fan-input-one", "fan-output-two"
            ]

            let arc, _ = canonicalPairFixture repeatedPairs
            let converted = convertCanonical [ canonicalLocation ] arc

            canonicalWriteBackMany converted.Index (prepareCanonical converted.Session) arc
            |> expectOk
            |> ignore

            let roundTripped = ARC.fromYamlString (arc.toYamlString ())
            let reloaded = convertCanonical [ canonicalLocation ] roundTripped

            Expect.equal
                (canonicalLinkPairs reloaded |> List.sort)
                (repeatedPairs |> List.sort)
                "YAML grouping preserves every positional pair, including the repeated endpoints."
    ]

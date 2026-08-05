module ProcessCoreSupersedeTests

open Expecto
open ProcessCore
open ProcessCoreProvenanceFixtures
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreAdapterTypes
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreConverter
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreWriteback

let private nodeName (node: IONode) =
    match node with
    | SampleNode sample -> sample.Name
    | DataNode data -> data.Path

let private processShapes (dataset: Dataset) name =
    dataset.Processes
    |> Seq.filter (fun proc -> proc.Name = name)
    |> Seq.map (fun proc ->
        (proc.Input |> Option.toList |> List.map nodeName), (proc.Output |> Option.toList |> List.map nodeName)
    )
    |> List.ofSeq

module CanonicalCommands = Swate.Components.Page.ProvenanceGrouping.Commands
module CanonicalIdentifiers = Swate.Components.Page.ProvenanceGrouping.Identifiers
module CanonicalProjectionTypes = Swate.Components.Page.ProvenanceGrouping.ProjectionTypes
module Session = Swate.Components.Page.ProvenanceGrouping.Session

let private canonicalLocation processGroupName : ProcessCoreProcessGroupLocation = {
    DatasetPath = [ "arc-neutral"; "dataset-neutral" ]
    ProcessGroupName = processGroupName
}

let private convertCanonical locations arc = fromArcMany locations arc |> expectOk

let private prepareCanonical (session: CanonicalProjectionTypes.ProvenanceSession) =
    Session.prepareForWriteback session |> expectOk

let private commitCanonical effect session = Session.commit effect session

let private sampleHeader: CanonicalIdentifiers.ProvenanceIOHeader = {
    Kind = ProcessCoreKinds.sampleEndpoint
    Text = "Sample"
}

/// Adds a disconnected input endpoint to the active layer, the shape all three
/// save/reload rows start from.
let private addLateInput position (session: CanonicalProjectionTypes.ProvenanceSession) =
    CanonicalCommands.addEndpoint
        session.ActiveLayerId
        CanonicalIdentifiers.ProvenanceSide.Input
        ProcessCoreKinds.sampleEndpoint
        sampleHeader
        "late-input"
        position
        session
    |> expectOk
    |> fun effect -> commitCanonical effect session

let private canonicalNodeIdByName name (session: CanonicalProjectionTypes.ProvenanceSession) =
    session.Nodes
    |> Map.toList
    |> List.find (fun (_, node) -> node.Name = name)
    |> fst

/// Save one: a disconnected endpoint materializes as a one-sided process.
/// Save two (after reconversion): connecting it must reuse that process
/// instead of appending a second one and stranding the first.
let tests =
    testList "ProcessCore one-sided process supersession" [
        testCase "a loaded one-sided promotion updates the same ProcessCore object"
        <| fun _ ->
            let arc, dataset, loadedProcess = inputOnly ()
            let converted = convertCanonical [ canonicalLocation "stage-neutral" ] arc
            let layerId = converted.Session.ActiveLayerId
            let inputNodeId = canonicalNodeIdByName "input.dat" converted.Session

            let withOutput =
                CanonicalCommands.addEndpoint
                    layerId
                    CanonicalIdentifiers.ProvenanceSide.Output
                    ProcessCoreKinds.sampleEndpoint
                    {
                        Kind = ProcessCoreKinds.sampleEndpoint
                        Text = "Sample"
                    }
                    "promoted-output"
                    3
                    converted.Session
                |> expectOk
                |> fun effect -> commitCanonical effect converted.Session

            let outputNodeId = canonicalNodeIdByName "promoted-output" withOutput

            let prepared =
                CanonicalCommands.connectNodes layerId [ inputNodeId, outputNodeId ] withOutput
                |> expectOk
                |> fun effect -> commitCanonical effect withOutput
                |> prepareCanonical

            let summary = writeBackMany converted.Index prepared arc |> expectOk

            Expect.equal summary.AddedProcesses 0 "Promotion adds no Process."
            Expect.equal summary.RemovedProcesses 0 "Promotion removes no Process."
            Expect.equal dataset.Processes.Count 1 "The promotion still materializes exactly one Process."

            Expect.isTrue
                (obj.ReferenceEquals(dataset.Processes[0], loadedProcess))
                "The promotion updates the indexed ProcessCore object in place."

            Expect.equal
                (processShapes dataset "stage-neutral")
                [ [ "input.dat" ], [ "promoted-output" ] ]
                "The promoted Process carries the exact new link."

        testCase "repeated save/reload after disconnection keeps the output continuation on the original process"
        <| fun _ ->
            let fixture = basic ()
            let arc = fixture.Arc
            let dataset = fixture.Dataset
            let converted = convertCanonical [ canonicalLocation "stage-neutral" ] arc

            let linkId =
                converted.Session.Processes
                |> Map.toList
                |> List.collect (fun (_, structuralProcess) -> structuralProcess.Links |> Map.toList |> List.map fst)
                |> List.exactlyOne

            let prepared =
                CanonicalCommands.disconnectLinks (Set.singleton linkId) converted.Session
                |> expectOk
                |> fun effect -> commitCanonical effect converted.Session
                |> prepareCanonical

            writeBackMany converted.Index prepared arc |> expectOk |> ignore

            let continuation =
                dataset.Processes
                |> Seq.filter (fun proc ->
                    proc.Input.IsNone
                    && proc.Output |> Option.exists (fun node -> nodeName node = "output-neutral")
                )
                |> Seq.exactlyOne

            Expect.isTrue
                (obj.ReferenceEquals(continuation, fixture.Process))
                "The output continuation reuses the indexed Process."

            let firstShapes = processShapes dataset "stage-neutral"

            for _ in 1..2 do
                let reloaded = convertCanonical [ canonicalLocation "stage-neutral" ] arc

                writeBackMany reloaded.Index (prepareCanonical reloaded.Session) arc
                |> expectOk
                |> ignore

                Expect.equal
                    (processShapes dataset "stage-neutral")
                    firstShapes
                    "Repeated save/reload never alternates the disconnected Process shapes."

                Expect.isTrue
                    (dataset.Processes
                     |> Seq.exists (fun proc ->
                         obj.ReferenceEquals(proc, fixture.Process)
                         && proc.Input.IsNone
                         && proc.Output |> Option.exists (fun node -> nodeName node = "output-neutral")
                     ))
                    "The output continuation stays on the original ProcessCore object."

        testCase "connecting a saved disconnected endpoint reuses its process"
        <| fun _ ->
            let fixture = basic ()
            let arc = fixture.Arc
            let dataset = fixture.Dataset

            // Save one: add a disconnected input endpoint.
            let first = convertCanonical [ canonicalLocation "stage-neutral" ] arc

            let firstSession = first.Session |> addLateInput 3 |> prepareCanonical

            let firstSummary = writeBackMany first.Index firstSession arc |> expectOk
            Expect.equal firstSummary.AddedProcesses 1 "The disconnected endpoint materializes one process."

            Expect.contains
                (processShapes dataset "stage-neutral")
                ([ "late-input" ], [])
                "The disconnected endpoint is written as a one-sided process."

            // Save two: reconvert, then connect that endpoint to an output.
            let second = convertCanonical [ canonicalLocation "stage-neutral" ] arc
            let inputId = canonicalNodeIdByName "late-input" second.Session
            let outputId = canonicalNodeIdByName "output-neutral" second.Session

            let secondSession =
                CanonicalCommands.connectNodes second.Session.ActiveLayerId [ inputId, outputId ] second.Session
                |> expectOk
                |> fun effect -> commitCanonical effect second.Session
                |> prepareCanonical

            let secondSummary = writeBackMany second.Index secondSession arc |> expectOk

            Expect.equal secondSummary.AddedProcesses 0 "The connection reuses the disconnected process."

            let shapes = processShapes dataset "stage-neutral"

            Expect.contains shapes ([ "late-input" ], [ "output-neutral" ]) "The reused process now carries the edge."

            Expect.isFalse
                (shapes |> List.contains ([ "late-input" ], []))
                "No redundant disconnected process may survive the connection."

            // The original edge is untouched, so exactly two rows remain.
            Expect.contains shapes ([ "input-neutral" ], [ "output-neutral" ]) "The pre-existing edge is preserved."
            Expect.hasLength shapes 2 "Exactly the original edge and the reused row remain."

        testCase "reconverts to one connected set after supersession"
        <| fun _ ->
            let fixture = basic ()
            let arc = fixture.Arc

            let first = convertCanonical [ canonicalLocation "stage-neutral" ] arc

            let firstSession = first.Session |> addLateInput 3 |> prepareCanonical

            writeBackMany first.Index firstSession arc |> expectOk |> ignore

            let second = convertCanonical [ canonicalLocation "stage-neutral" ] arc
            let inputId = canonicalNodeIdByName "late-input" second.Session
            let outputId = canonicalNodeIdByName "output-neutral" second.Session

            let secondSession =
                CanonicalCommands.connectNodes second.Session.ActiveLayerId [ inputId, outputId ] second.Session
                |> expectOk
                |> fun effect -> commitCanonical effect second.Session
                |> prepareCanonical

            writeBackMany second.Index secondSession arc |> expectOk |> ignore

            let reconverted = convertCanonical [ canonicalLocation "stage-neutral" ] arc
            let lateInputId = canonicalNodeIdByName "late-input" reconverted.Session
            let layer = reconverted.Session.Layers[reconverted.Session.ActiveLayerId]

            Expect.equal layer.InputEndpoints.Count 2 "The endpoint reconverts once, alongside the original input."

            Expect.isTrue
                (reconverted.Session.Processes
                 |> Map.exists (fun _ structuralProcess ->
                     structuralProcess.Links
                     |> Map.exists (fun _ link ->
                         match link.Shape with
                         | Swate.Components.Page.ProvenanceGrouping.Values.ProcessLinkShape.Between(input, _) ->
                             input = lateInputId
                         | _ -> false
                     )
                 ))
                "The reconverted endpoint is connected."

        testCase "an unconnected saved endpoint keeps its one-sided process"
        <| fun _ ->
            let fixture = basic ()
            let arc = fixture.Arc
            let dataset = fixture.Dataset

            let first = convertCanonical [ canonicalLocation "stage-neutral" ] arc

            let firstSession = first.Session |> addLateInput 3 |> prepareCanonical

            writeBackMany first.Index firstSession arc |> expectOk |> ignore

            // A second save that touches nothing must not disturb the row.
            let second = convertCanonical [ canonicalLocation "stage-neutral" ] arc
            let secondSession = prepareCanonical second.Session
            let summary = writeBackMany second.Index secondSession arc |> expectOk

            Expect.equal summary.AddedProcesses 0 "An untouched session adds nothing."
            Expect.equal summary.RemovedProcesses 0 "An untouched session removes nothing."

            Expect.contains
                (processShapes dataset "stage-neutral")
                ([ "late-input" ], [])
                "The still-disconnected endpoint keeps its one-sided process."
    ]

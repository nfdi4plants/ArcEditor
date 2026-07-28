module ProcessCoreSupersedeTests

open Expecto
open ProcessCore
open ProcessCoreProvenanceFixtures
open Swate.Components.Page.ProvenanceGrouping.ProvenanceTypes
open Swate.Components.Page.ProvenanceGrouping.Session
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreAdapterTypes
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreConverter
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreWriteback

let private nodeName (node: IONode) =
    match node with
    | SampleNode sample -> sample.Name
    | DataNode data -> data.Path

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

let private sampleHeader = {
    Kind = ProcessCoreKinds.sampleEndpoint
    Text = "Sample"
}

let private setIdByName side name (model: ProvenanceModel) =
    let sets =
        match side with
        | ProvenanceSide.Input -> model.InputSets
        | ProvenanceSide.Output -> model.OutputSets

    sets |> Map.toList |> List.find (fun (_, set) -> set.Name = name) |> fst

let private processShapes (dataset: Dataset) name =
    dataset.Processes
    |> Seq.filter (fun proc -> proc.Name = name)
    |> Seq.map (fun proc ->
        (proc.Inputs |> Seq.map nodeName |> List.ofSeq), (proc.Outputs |> Seq.map nodeName |> List.ofSeq)
    )
    |> List.ofSeq

/// Save one: a disconnected endpoint materializes as a one-sided process.
/// Save two (after reconversion): connecting it must reuse that process
/// instead of appending a second one and stranding the first.
let tests =
    testList "ProcessCore one-sided process supersession" [
        testCase "connecting a saved disconnected endpoint reuses its process"
        <| fun _ ->
            let fixture = basic ()
            let arc = fixture.Arc
            let dataset = fixture.Dataset

            // Save one: add a disconnected input endpoint.
            let first = fromArc loadedTable arc |> expectOk

            let firstSession =
                Session.init first.Model
                |> createSet ProvenanceSide.Input sampleHeader "late-input"

            let firstSummary = writeBack first.Index firstSession arc |> expectOk
            Expect.equal firstSummary.AddedProcesses 1 "The disconnected endpoint materializes one process."

            Expect.contains
                (processShapes dataset "stage-neutral")
                ([ "late-input" ], [])
                "The disconnected endpoint is written as a one-sided process."

            // Save two: reconvert, then connect that endpoint to an output.
            let second = fromArc loadedTable arc |> expectOk
            let inputId = setIdByName ProvenanceSide.Input "late-input" second.Model
            let outputId = setIdByName ProvenanceSide.Output "output-neutral" second.Model

            let secondSession = Session.init second.Model |> connect inputId outputId
            let secondSummary = writeBack second.Index secondSession arc |> expectOk

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

            let first = fromArc loadedTable arc |> expectOk

            let firstSession =
                Session.init first.Model
                |> createSet ProvenanceSide.Input sampleHeader "late-input"

            writeBack first.Index firstSession arc |> expectOk |> ignore

            let second = fromArc loadedTable arc |> expectOk
            let inputId = setIdByName ProvenanceSide.Input "late-input" second.Model
            let outputId = setIdByName ProvenanceSide.Output "output-neutral" second.Model

            let secondSession = Session.init second.Model |> connect inputId outputId
            writeBack second.Index secondSession arc |> expectOk |> ignore

            let reconverted = fromArc loadedTable arc |> expectOk
            let lateInputId = setIdByName ProvenanceSide.Input "late-input" reconverted.Model

            Expect.equal
                reconverted.Model.InputSets.Count
                2
                "The endpoint reconverts once, alongside the original input."

            Expect.isTrue
                (reconverted.Model.Connections
                 |> Map.exists (fun _ connection -> connection.InputSetId = lateInputId))
                "The reconverted endpoint is connected."

        testCase "an unconnected saved endpoint keeps its one-sided process"
        <| fun _ ->
            let fixture = basic ()
            let arc = fixture.Arc
            let dataset = fixture.Dataset

            let first = fromArc loadedTable arc |> expectOk

            let firstSession =
                Session.init first.Model
                |> createSet ProvenanceSide.Input sampleHeader "late-input"

            writeBack first.Index firstSession arc |> expectOk |> ignore

            // A second save that touches nothing must not disturb the row.
            let second = fromArc loadedTable arc |> expectOk
            let secondSession = Session.init second.Model
            let summary = writeBack second.Index secondSession arc |> expectOk

            Expect.equal summary.AddedProcesses 0 "An untouched session adds nothing."
            Expect.equal summary.RemovedProcesses 0 "An untouched session removes nothing."

            Expect.contains
                (processShapes dataset "stage-neutral")
                ([ "late-input" ], [])
                "The still-disconnected endpoint keeps its one-sided process."
    ]

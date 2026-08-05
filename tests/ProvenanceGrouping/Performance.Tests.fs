module ProcessCorePerformanceTests

open System.Diagnostics
open Expecto
open Swate.Components.Page.ProvenanceGrouping.Identifiers
open Swate.Components.Page.ProvenanceGrouping.Values
open Swate.Components.Page.ProvenanceGrouping.ProjectionTypes
open Swate.Components.Page.ProvenanceGrouping.Commands
open Swate.Components.Page.ProvenanceGrouping.StoryFixtures

let private expectOk =
    function
    | Ok value -> value
    | Error error -> failtestf "Expected Ok but received %A" error

let private commit effect session =
    Swate.Components.Page.ProvenanceGrouping.Session.commit effect session

let private resolveLayerAvailability layerId session =
    Swate.Components.Page.ProvenanceGrouping.Session.resolveLayerAvailability layerId session

let private percentile p (samples: float list) =
    let sorted = samples |> List.sort |> List.toArray
    let rank = System.Math.Ceiling(p * float sorted.Length) |> int
    sorted[max 0 (min (sorted.Length - 1) (rank - 1))]

let private elapsedMillis (f: unit -> 'a) =
    let stopwatch = Stopwatch.StartNew()
    f () |> ignore
    stopwatch.Stop()
    stopwatch.Elapsed.TotalMilliseconds

type private Scenario = { Name: string; Samples: float list }

let private p50 scenario = percentile 0.50 scenario.Samples
let private p95 scenario = percentile 0.95 scenario.Samples

/// Runs `mutate` from the same warm baseline `repetitions` times, timing only
/// the availability-resolution call after each mutation - the phase this step
/// benchmarks, per intent §11's "separate canonical-mutation,
/// availability-resolution, and display-projection phases". `mutate` itself
/// (the canonical commit) is not timed: it always runs `Session.commit`,
/// which repaints the active layer's display projection regardless of
/// scenario, so it belongs to the Storybook-measured repaint half, not here.
let private runScenario name repetitions activeLayerId (mutate: ProvenanceSession -> ProvenanceSession) warmSession =
    let samples =
        [ 1..repetitions ]
        |> List.map (fun _ ->
            let mutated = mutate warmSession
            elapsedMillis (fun () -> resolveLayerAvailability activeLayerId mutated |> expectOk)
        )

    { Name = name; Samples = samples }

let private addedTerm: ProvenanceTerm = {
    Name = "Performance added property"
    TermSource = Some "PERF"
    TermAccession = None
}

let private editedTerm: ProvenanceTerm = {
    Name = "Performance edited value"
    TermSource = Some "PERF"
    TermAccession = None
}

let tests =
    testList "CanonicalPerformance" [
        testCase "the generated workload has the stated layer, node and edge counts"
        <| fun _ ->
            let layers, nodesPerSide, edgeDensity = 3, 12, 0.3
            let fanIn = performanceFanIn nodesPerSide edgeDensity
            let session = createPerformanceSession layers nodesPerSide edgeDensity

            Expect.equal session.Layers.Count layers "One canonical layer per requested layer."

            Expect.equal session.Nodes.Count (layers * nodesPerSide * 2) "Two sides of nodesPerSide nodes, per layer."

            let actualLinks =
                session.Processes
                |> Map.toList
                |> List.sumBy (fun (_, item) -> item.Links.Count)

            Expect.equal
                actualLinks
                (layers * (nodesPerSide - 1) * fanIn)
                "Every connectable output fans in to exactly the stated number of inputs."

        testCase "the three benchmark scenarios each report p50 and p95"
        <| fun _ ->
            let layers, nodesPerSide, edgeDensity = 6, 80, 0.1
            let repetitions = 11

            let session = createPerformanceSession layers nodesPerSide edgeDensity
            let activeLayerId = session.ActiveLayerId
            let _, warmSession = resolveLayerAvailability activeLayerId session |> expectOk

            let editValueId = performanceEditValueId 0
            let unassignedNodeId = performanceUnassignedNodeId 0 nodesPerSide
            let unconnectedInput, unconnectedOutput = performanceUnconnectedPair 0 nodesPerSide

            let valueOnlyEdit session =
                editValueGlobally
                    editValueId
                    {
                        Category = editedTerm
                        Value = ProvenanceValue.Text "Edited batch"
                        Unit = None
                    }
                    session
                |> expectOk
                |> fun effect -> commit effect session

            let assignmentAdd session =
                let draft: NodeAssignmentDraft = {
                    Content = {
                        Category = addedTerm
                        Value = ProvenanceValue.Text "Added"
                        Unit = None
                    }
                    OwnerKind = AnnotationOwnerKind.Node
                    PropertyKind = AssignmentPropertyKind.Generic
                }

                assignNodeValue (Set.singleton unassignedNodeId) draft NoOverwrite session
                |> expectOk
                |> fun effect -> commit effect session

            let structuralEdit session =
                connectNodes activeLayerId [ unconnectedInput, unconnectedOutput ] session
                |> expectOk
                |> fun effect -> commit effect session

            let scenarios = [
                runScenario "value-only edit, memo retained" repetitions activeLayerId valueOnlyEdit warmSession
                runScenario "assignment add, cold reachability" repetitions activeLayerId assignmentAdd warmSession
                runScenario
                    "structural link edit, cold reachability"
                    repetitions
                    activeLayerId
                    structuralEdit
                    warmSession
            ]

            printfn ""

            printfn
                "Provenance availability-resolution benchmark (layers=%d, nodesPerSide=%d, edgeDensity=%g, repetitions=%d)"
                layers
                nodesPerSide
                edgeDensity
                repetitions

            printfn "%-42s %10s %10s" "scenario" "p50 (ms)" "p95 (ms)"

            for scenario in scenarios do
                printfn "%-42s %10.3f %10.3f" scenario.Name (p50 scenario) (p95 scenario)

            Expect.equal scenarios.Length 3 "All three scenarios ran."

            for scenario in scenarios do
                Expect.equal scenario.Samples.Length repetitions $"{scenario.Name} ran every repetition."
    ]

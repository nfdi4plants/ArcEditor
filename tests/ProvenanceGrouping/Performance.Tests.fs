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

/// The recorded-run workload knobs. Routine S2 runs keep the default smoke
/// size so the suite stays usable for iteration; the plan's pinned recorded
/// run (L = 10, N = 200) is executed by setting these and its table lives in
/// the handoff's Phase L record. That split is a recorded decision, not an
/// accident.
let private envInt name fallback =
    match System.Environment.GetEnvironmentVariable name with
    | null
    | "" -> fallback
    | text ->
        match System.Int32.TryParse text with
        | true, value -> value
        | _ -> fallback

let private envFloat name fallback =
    match System.Environment.GetEnvironmentVariable name with
    | null
    | "" -> fallback
    | text ->
        match System.Double.TryParse(text, System.Globalization.CultureInfo.InvariantCulture) with
        | true, value -> value
        | _ -> fallback

type private Scenario = {
    Name: string
    /// Availability resolution alone - the phase intent §11's 33 ms p95 target
    /// applies to.
    ResolveSamples: float list
    /// Canonical commit (including the active layer's display-projection
    /// repaint inside `Session.commit`) plus availability resolution - the
    /// .NET-observable share of edit-to-correct-paint; the browser half adds
    /// React's render on top.
    TotalSamples: float list
}

/// Runs `mutate` from the same warm baseline `repetitions` times, timing the
/// canonical commit and the availability resolution separately, per intent
/// §11's "separate canonical-mutation, availability-resolution, and
/// display-projection phases".
let private runScenario name repetitions activeLayerId (mutate: ProvenanceSession -> ProvenanceSession) warmSession =
    let samples =
        [ 1..repetitions ]
        |> List.map (fun _ ->
            let mutable mutated = warmSession
            let commitMs = elapsedMillis (fun () -> mutated <- mutate warmSession)

            let resolveMs =
                elapsedMillis (fun () -> resolveLayerAvailability activeLayerId mutated |> expectOk)

            resolveMs, commitMs + resolveMs
        )

    {
        Name = name
        ResolveSamples = samples |> List.map fst
        TotalSamples = samples |> List.map snd
    }

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
            let layers = envInt "PROVENANCE_BENCH_LAYERS" 6
            let nodesPerSide = envInt "PROVENANCE_BENCH_NODES" 80
            let edgeDensity = envFloat "PROVENANCE_BENCH_DENSITY" 0.1
            let repetitions = envInt "PROVENANCE_BENCH_REPETITIONS" 11

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

            printfn "%-42s %12s %12s %12s %12s" "scenario" "resolve p50" "resolve p95" "commit+res p50" "commit+res p95"

            for scenario in scenarios do
                printfn
                    "%-42s %12.3f %12.3f %12.3f %12.3f"
                    scenario.Name
                    (percentile 0.50 scenario.ResolveSamples)
                    (percentile 0.95 scenario.ResolveSamples)
                    (percentile 0.50 scenario.TotalSamples)
                    (percentile 0.95 scenario.TotalSamples)

            Expect.equal scenarios.Length 3 "All three scenarios ran."

            for scenario in scenarios do
                Expect.equal scenario.ResolveSamples.Length repetitions $"{scenario.Name} ran every repetition."

                Expect.equal
                    scenario.TotalSamples.Length
                    repetitions
                    $"{scenario.Name} reported commit+resolve for every repetition."
    ]

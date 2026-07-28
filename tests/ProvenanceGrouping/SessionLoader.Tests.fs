module ProcessCoreSessionLoaderTests

open Expecto
open ProcessCore
open ProcessCoreProvenanceFixtures
open Swate.Components.Page.ProvenanceGrouping.ProvenanceTypes
open Swate.Components.Page.ProvenanceGrouping.Session
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreAdapterTypes
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreSessionLoader

let private chainedDataset () =
    let input = Sample("chain-input")
    let mid = Sample("chain-mid")
    let output = Sample("chain-output")
    let stageOne = mkProcess "stage-one" [ SampleNode input ] [ SampleNode mid ]
    let stageTwo = mkProcess "stage-two" [ SampleNode mid ] [ SampleNode output ]
    let dataset = Dataset("dataset-neutral", processes = [ stageOne; stageTwo ])
    let arc = ARC("arc-neutral", hasPart = [ dataset ])
    arc, dataset, stageOne

let tests =
    testList "ProcessCore session loader" [
        testCase "resolves a process to its owning dataset's table location"
        <| fun _ ->
            let arc, _, stageOne = chainedDataset ()

            let location =
                Expect.wantSome (tryLocationForProcess stageOne arc) "The process must resolve."

            Expect.equal location.DatasetPath [ "arc-neutral"; "dataset-neutral" ] "Path walks from the ARC root."
            Expect.equal location.TableName "stage-one" "The table is the process group name."

        testCase "resolves a dataset to one location per process group in order"
        <| fun _ ->
            let arc, dataset, _ = chainedDataset ()
            let locations = locationsForDataset dataset arc

            Expect.equal
                (locations |> List.map (fun location -> location.TableName))
                [ "stage-one"; "stage-two" ]
                "One location per distinct group, in first-occurrence order."

        testCase "loads a dataset's groups as one chained multi-layer session"
        <| fun _ ->
            let arc, dataset, _ = chainedDataset ()
            let loaded = locationsForDataset dataset arc |> fun l -> load l arc |> expectOk

            Expect.equal loaded.Session.Layers.Length 2 "One layer per process group."
            Expect.equal loaded.Indices.Count 2 "One writeback index per loaded table."
            Expect.hasLength loaded.Session.ReferenceLinks 1 "The chained boundary sample links the layers."
            Expect.isTrue (isCurrent loaded arc) "A freshly loaded session matches its ARC."

        testCase "offers only endpoint kinds the writeback can materialize"
        <| fun _ ->
            // Regression: the editor used to offer hardcoded ISA endpoint
            // kinds, so every endpoint created in a ProcessCore session was
            // rejected on save with UnsupportedEndpointKind.
            let arc, dataset, _ = chainedDataset ()
            let loaded = locationsForDataset dataset arc |> fun l -> load l arc |> expectOk

            let offered =
                loaded.Session.Layers
                |> Seq.collect (fun layer ->
                    Seq.append
                        (layer.Model.InputSets |> Map.toSeq |> Seq.map (fun (_, set) -> set.Header.Kind))
                        (layer.Model.OutputSets |> Map.toSeq |> Seq.map (fun (_, set) -> set.Header.Kind))
                )
                |> Swate.Components.Page.ProvenanceGrouping.Endpoints.kindsForSets

            Expect.isNonEmpty offered "A loaded session must offer its own endpoint kinds."

            let supported =
                set [
                    ProcessCoreKinds.sampleEndpoint.Id
                    ProcessCoreKinds.dataEndpoint.Id
                ]

            for kind in offered do
                Expect.isTrue
                    (supported.Contains kind.Id)
                    $"Offered endpoint kind '{kind.Id}' is not one the ProcessCore writeback can materialize."

        testCase "detects a stale session after the ARC changed"
        <| fun _ ->
            let arc, dataset, _ = chainedDataset ()
            let loaded = locationsForDataset dataset arc |> fun l -> load l arc |> expectOk
            dataset.AddProcess(mkProcess "later-stage" [ SampleNode(Sample("x")) ] [])
            Expect.isFalse (isCurrent loaded arc) "A graph change must flip the fingerprint check."
    ]

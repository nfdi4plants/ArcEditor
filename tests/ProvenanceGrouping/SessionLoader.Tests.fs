module ProcessCoreSessionLoaderTests

open Expecto
open ProcessCore
open ProcessCoreProvenanceFixtures
open Swate.Components.ProcessCore.Copy
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

let private storedRecipeDataset () =
    let assigned = Recipe(name = "assigned")
    assigned.SetProperty("@id", "recipe:assigned")
    let unassigned = Recipe(name = "unassigned")
    unassigned.SetProperty("@id", "recipe:unassigned")

    let processObject =
        mkProcessFull "stage-neutral" (Some assigned) [ SampleNode(Sample("recipe-input")) ] [
            SampleNode(Sample("recipe-output"))
        ] []

    let dataset = Dataset("dataset-neutral", processes = [ processObject ])
    let arc = ARC("arc-neutral", hasPart = [ dataset ])
    arc.AddRecipe assigned
    arc.AddRecipe unassigned
    arc, dataset, assigned, unassigned

let tests =
    testList "ProcessCore session loader" [
        testCase "resolves a process to its owning canonical process-group location by identity"
        <| fun _ ->
            let arc, _, stageOne = chainedDataset ()

            let location =
                Expect.wantSome (tryLocationForProcess stageOne arc) "The process must resolve by reference identity."

            Expect.equal location.DatasetPath [ "arc-neutral"; "dataset-neutral" ] "Path walks from the ARC root."
            Expect.equal location.ProcessGroupName "stage-one" "The canonical field is the process group name."

            let equalNameButDetached = mkProcess "stage-one" [] []

            Expect.isNone
                (tryLocationForProcess equalNameButDetached arc)
                "An equal process name outside the ARC must not resolve."

        testCase "resolves canonical dataset locations in first-occurrence order"
        <| fun _ ->
            let arc, dataset, _ = chainedDataset ()
            dataset.AddProcess(mkProcess "stage-one" [] [])
            let locations = locationsForDataset dataset arc

            Expect.equal
                (locations |> List.map (fun location -> location.ProcessGroupName))
                [ "stage-one"; "stage-two" ]
                "One canonical location is returned per distinct process group in first-occurrence order."

            Expect.all
                locations
                (fun location -> location.DatasetPath = [ "arc-neutral"; "dataset-neutral" ])
                "Every canonical location retains the owning dataset path."

        testCase "loading several locations produces one canonical session and one index"
        <| fun _ ->
            let arc, dataset, _ = chainedDataset ()

            let locations = locationsForDataset dataset arc |> List.rev

            let loaded = load locations arc |> expectOk

            Expect.equal loaded.Locations locations "The canonical loader retains the exact selection order."
            Expect.equal loaded.Index.LoadedProcessGroups locations "One index covers the exact location selection."
            Expect.equal loaded.Index.SourceLocations.Count 2 "The one index owns both source locations."
            Expect.equal loaded.Session.Layers.Count 2 "One canonical session contains both layers."

            Expect.equal
                (loaded.Session.LayerOrder
                 |> List.map (fun layerId -> loaded.Session.Layers[layerId].Label))
                [ "stage-two"; "stage-one" ]
                "Canonical layer order follows selection order."

        testCase "offers only endpoint kinds the writeback can materialize"
        <| fun _ ->
            // Regression: the editor used to offer hardcoded ISA endpoint
            // kinds, so every endpoint created in a ProcessCore session was
            // rejected on save with UnsupportedEndpointKind.
            let arc, dataset, _ = chainedDataset ()

            let loaded = load (locationsForDataset dataset arc) arc |> expectOk

            let offered =
                loaded.Session.Nodes
                |> Map.toSeq
                |> Seq.map (fun (_, node) -> node.Kind)
                |> Swate.Components.Page.ProvenanceGrouping.Endpoints.kindsForNodes

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

        testCase "a stale ARC is detected through the captured graph fingerprint"
        <| fun _ ->
            let graphArc, graphDataset, _ = chainedDataset ()

            let graphLoaded =
                locationsForDataset graphDataset graphArc
                |> fun locations -> load locations graphArc
                |> expectOk

            Expect.isTrue (isCurrent graphLoaded graphArc) "A freshly loaded graph must be current."
            graphDataset.AddProcess(mkProcess "later-stage" [ SampleNode(Sample("external")) ] [])

            Expect.isFalse
                (isCurrent graphLoaded graphArc)
                "An external graph change must invalidate the captured fingerprint."

            let assignedArc, assignedDataset, assigned, _ = storedRecipeDataset ()

            let assignedLoaded =
                locationsForDataset assignedDataset assignedArc
                |> fun locations -> load locations assignedArc
                |> expectOk

            assigned.Description <- Some "externally changed assigned payload"

            Expect.isFalse
                (isCurrent assignedLoaded assignedArc)
                "An assigned stored Recipe payload change must invalidate the captured fingerprint."

            let unassignedArc, unassignedDataset, _, unassigned = storedRecipeDataset ()

            let unassignedLoaded =
                locationsForDataset unassignedDataset unassignedArc
                |> fun locations -> load locations unassignedArc
                |> expectOk

            unassigned.Description <- Some "externally changed unassigned payload"

            Expect.isFalse
                (isCurrent unassignedLoaded unassignedArc)
                "An unassigned stored Recipe payload change must invalidate the captured fingerprint."

        testCase "the loaded catalog contains every stored recipe"
        <| fun _ ->
            let arc, dataset, assigned, unassigned = storedRecipeDataset ()

            let loaded =
                locationsForDataset dataset arc
                |> fun locations -> load locations arc
                |> expectOk

            let scheme = ProcessCoreKinds.processCoreRecipeScheme
            let assignedId = RecipeResourceKey.ofRecipeStableString assigned
            let unassignedId = RecipeResourceKey.ofRecipeStableString unassigned

            Expect.equal loaded.ReferenceCatalog.Count 2 "Referenced and unreferenced Recipes are both cataloged."

            Expect.isTrue
                (loaded.ReferenceCatalog.ContainsKey(scheme, assignedId))
                "The referenced stored Recipe is present."

            Expect.isTrue
                (loaded.ReferenceCatalog.ContainsKey(scheme, unassignedId))
                "The unreferenced stored Recipe is present."

        testCase "dataset selection errors are reported as before"
        <| fun _ ->
            let fixture = basic ()

            Expect.throwsT<System.ArgumentException>
                (fun () -> load [] fixture.Arc |> ignore)
                "The canonical loader must preserve the empty-selection boundary."

            let canonicalLocation path groupName : ProcessCoreProcessGroupLocation = {
                DatasetPath = path
                ProcessGroupName = groupName
            }

            let emptyPath = canonicalLocation [] "stage-neutral"

            let missingPath =
                canonicalLocation [ "arc-neutral"; "missing-neutral" ] "stage-neutral"

            let missingGroup =
                canonicalLocation [ "arc-neutral"; "dataset-neutral" ] "missing-stage"

            Expect.equal
                (load [ emptyPath ] fixture.Arc |> expectError)
                [ ProcessCoreConversionError.EmptyDatasetPath ]
                "An empty canonical dataset path remains typed."

            Expect.equal
                (load [ missingPath ] fixture.Arc |> expectError)
                [
                    ProcessCoreConversionError.DatasetNotFound missingPath.DatasetPath
                ]
                "A missing canonical dataset path remains typed."

            Expect.equal
                (load [ missingGroup ] fixture.Arc |> expectError)
                [
                    ProcessCoreConversionError.ProcessGroupNotFound missingGroup
                ]
                "A missing canonical process group remains typed."

            let first =
                Dataset(
                    "dataset-neutral",
                    processes = [
                        mkProcess "stage-neutral" [ SampleNode(Sample("ambiguous-input")) ] []
                    ]
                )

            let second = Dataset("dataset-shadow")
            let ambiguousArc = ARC("arc-neutral", hasPart = [ first; second ])
            second.Identifier <- "dataset-neutral"

            let ambiguous =
                canonicalLocation [ "arc-neutral"; "dataset-neutral" ] "stage-neutral"

            Expect.equal
                (load [ ambiguous ] ambiguousArc |> expectError)
                [
                    ProcessCoreConversionError.AmbiguousDatasetPath ambiguous.DatasetPath
                ]
                "An ambiguous canonical dataset path remains typed."
    ]

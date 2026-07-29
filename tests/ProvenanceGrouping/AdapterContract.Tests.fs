module ProcessCoreAdapterContractTests

open Expecto
open ProcessCore
open Swate.Components.Page.ProvenanceGrouping.Session
open Swate.Components.ProcessCore.Copy
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreAdapterTypes
open ProcessCoreProvenanceFixtures
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreConverter
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreWriteback

let private contractTests =
    testList "ProcessCore adapter contract" [
        testCase "exposes source-specific endpoint and property kinds"
        <| fun _ ->
            Expect.equal ProcessCoreKinds.sampleEndpoint.Id "process-core:endpoint:sample" "Sample kind must be stable."

            Expect.equal
                ProcessCoreKinds.parameter.Id
                "process-core:property:parameter"
                "Parameter kind must be stable."

            Expect.equal
                ProcessCoreKinds.componentKind.Id
                "process-core:property:component"
                "Component kind must be stable without using a reserved F# identifier."

        testCase "represents selection as a dataset path and process-group name"
        <| fun _ ->
            let location = {
                DatasetPath = [ "arc-neutral"; "dataset-neutral" ]
                TableName = "stage-neutral"
            }

            Expect.sequenceEqual
                location.DatasetPath
                [ "arc-neutral"; "dataset-neutral" ]
                "Dataset path must retain order."

            Expect.equal location.TableName "stage-neutral" "Logical table name must be retained."

        testCase "ambiguous fallback recipe identities are rejected by catalog construction"
        <| fun _ ->
            let first =
                Recipe(name = "same recipe", version = "1.0", url = "https://example.org/recipe")

            let second =
                Recipe(name = "same recipe", version = "1.0", url = "https://example.org/recipe")

            match RecipeResourceIndex.tryCreate [ first; second ] with
            | Error(RecipeResourceIndexError.AmbiguousKey key) ->
                Expect.equal
                    key
                    (RecipeResourceKey.ByMetadata(Some "same recipe", Some "1.0", Some "https://example.org/recipe"))
                    "The complete fallback tuple must identify the ambiguity."
            | Ok _ -> failtest "Catalog construction must reject an ambiguous fallback identity."

        testCase "a split process reuses the exact stored recipe resource"
        <| fun _ ->
            let recipe = Recipe()
            recipe.SetProperty("@id", "recipe:stored")

            let input = Sample("split-input")
            let outputOne = Sample("split-output-one")
            let outputTwo = Sample("split-output-two")

            let first =
                mkProcessFull "stage-neutral" (Some recipe) [ SampleNode input ] [ SampleNode outputOne ] []

            let second =
                mkProcessFull "stage-neutral" (Some recipe) [ SampleNode input ] [ SampleNode outputTwo ] []

            let dataset = Dataset("dataset-neutral", processes = [ first; second ])
            let arc = ARC("arc-neutral", hasPart = [ dataset ])
            arc.AddRecipe recipe
            let converted = fromArc loadedTable arc |> expectOk

            let removedId =
                converted.Model.Connections
                |> Map.toList
                |> List.find (fun (_, connection) ->
                    converted.Model.OutputSets.[connection.OutputSetId].Name = "split-output-one"
                )
                |> fst

            let session =
                Session.init converted.Model
                |> Session.removeConnection removedId
                |> expectOk
                |> fst

            writeBack converted.Index session arc |> expectOk |> ignore

            Expect.equal arc.Recipes.Count 1 "Splitting must not grow the stored Recipe catalog."

            let assignedRecipes =
                dataset.Processes |> Seq.choose _.ExecutesRecipe |> Seq.toArray

            Expect.isNonEmpty assignedRecipes "The structural rewrite must retain at least one Recipe-bearing Process."

            assignedRecipes
            |> Array.iter (fun candidate ->
                Expect.isTrue
                    (obj.ReferenceEquals(candidate, recipe))
                    "Every split Process must reuse the exact stored Recipe object."
            )
    ]

let private selectionTests =
    testList "selection" [
        testCase "selects an exact dataset path and process group"
        <| fun _ ->
            let fixture = basic ()
            let result = fromArc loadedTable fixture.Arc |> expectOk

            Expect.equal result.Model.Source.Name "stage-neutral" "Selected group name must become the source name."
            Expect.equal result.Index.LoadedTable loadedTable "The exact selector must be retained."
            Expect.isNotEmpty result.Index.ArcFingerprint "The source graph must be fingerprinted."

        testCase "returns a typed error for a missing dataset"
        <| fun _ ->
            let fixture = basic ()

            let missing = {
                loadedTable with
                    DatasetPath = [ "arc-neutral"; "missing-neutral" ]
            }

            match fromArc missing fixture.Arc |> expectError with
            | ProcessCoreConversionError.DatasetNotFound path ->
                Expect.sequenceEqual path missing.DatasetPath "Error must retain the requested path."
            | other -> failtestf "Expected DatasetNotFound but received %A" other

        testCase "returns a typed error for an ambiguous dataset path"
        <| fun _ ->
            let first =
                Dataset(
                    "dataset-neutral",
                    processes = [
                        mkProcess "stage-neutral" [ SampleNode(Sample("ambiguous-input")) ] []
                    ]
                )

            let second = Dataset("dataset-shadow")
            let arc = ARC("arc-neutral", hasPart = [ first; second ])
            // AddPart deduplicates equal identifiers, so create a valid graph first and
            // then model a corrupted in-memory graph with an ambiguous path.
            second.Identifier <- "dataset-neutral"

            Expect.equal
                (fromArc loadedTable arc |> expectError)
                (ProcessCoreConversionError.AmbiguousDatasetPath loadedTable.DatasetPath)
                "Duplicate sibling dataset identifiers must fail conversion instead of first-match-wins."

        testCase "returns a typed error for a missing process group"
        <| fun _ ->
            let fixture = basic ()

            let missing = {
                loadedTable with
                    TableName = "missing-stage"
            }

            Expect.equal
                (fromArc missing fixture.Arc |> expectError)
                (ProcessCoreConversionError.ProcessGroupNotFound missing)
                "A dataset without the selected group must fail."

        testCase "produces stable source identity for an unchanged graph"
        <| fun _ ->
            let fixture = basic ()
            let first = fromArc loadedTable fixture.Arc |> expectOk
            let second = fromArc loadedTable fixture.Arc |> expectOk

            Expect.equal first.Model.Source second.Model.Source "Source identity must be deterministic."
            Expect.equal first.Index.ArcFingerprint second.Index.ArcFingerprint "Fingerprint must be deterministic."
    ]

let tests = testList "ProcessCore adapter" [ contractTests; selectionTests ]

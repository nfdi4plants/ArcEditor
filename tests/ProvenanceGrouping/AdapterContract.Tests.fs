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
        testCase "the published ProcessCore access surface is available"
        <| fun _ ->
            let recipe = Recipe(name = "published recipe", version = "1.0")
            let input = Sample("published-input")
            let output = Data("published-output.txt")
            let processObject = Process("published-process")

            processObject.SetInput(SampleNode input)
            processObject.SetOutput(DataNode output)
            processObject.ExecutesRecipe <- Some recipe

            let dataset = Dataset("published-dataset", processes = [ processObject ])
            let arc = ARC("published-arc", hasPart = [ dataset ])
            arc.AddRecipe recipe

            Expect.isTrue
                (processObject.Input
                 |> Option.exists (
                     function
                     | SampleNode candidate -> obj.ReferenceEquals(candidate, input)
                     | _ -> false
                 ))
                "The singular Input property must expose the exact node assigned through SetInput."

            Expect.isTrue
                (processObject.Output
                 |> Option.exists (
                     function
                     | DataNode candidate -> obj.ReferenceEquals(candidate, output)
                     | _ -> false
                 ))
                "The singular Output property must expose the exact node assigned through SetOutput."

            Expect.isTrue
                (processObject.ExecutesRecipe
                 |> Option.exists (fun candidate -> obj.ReferenceEquals(candidate, recipe)))
                "ExecutesRecipe must expose the exact canonical Recipe reference."

            Expect.equal arc.Recipes.Count 1 "ARC.Recipes must expose the stored resource."
            Expect.isTrue (obj.ReferenceEquals(arc.Recipes[0], recipe)) "ARC.AddRecipe must store the exact resource."

            processObject.ClearInput()
            processObject.ClearOutput()
            processObject.ExecutesRecipe <- None

            Expect.isNone processObject.Input "ClearInput must clear the singular input."
            Expect.isNone processObject.Output "ClearOutput must clear the singular output."
            Expect.isNone processObject.ExecutesRecipe "The published Recipe reference must be detachable."

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

        testCase "stored recipe references resolve exactly"
        <| fun _ ->
            let assigned = Recipe(name = "assigned recipe", version = "1.0")
            assigned.SetProperty("@id", "recipe:assigned")

            let unassigned =
                Recipe(name = "unassigned recipe", version = "2.0", url = "https://example.org/recipes/unassigned")

            let processObject = mkProcessFull "stage-neutral" (Some assigned) [] [] []
            let dataset = Dataset("dataset-neutral", processes = [ processObject ])
            let arc = ARC("arc-neutral", hasPart = [ dataset ])
            arc.AddRecipe assigned
            arc.AddRecipe unassigned

            let index = RecipeResourceIndex.tryCreate arc.Recipes |> expectOk

            let indexedAssigned = index |> Map.find (RecipeResourceKey.ById "recipe:assigned")

            let indexedUnassigned =
                index
                |> Map.find (
                    RecipeResourceKey.ByMetadata(
                        Some "unassigned recipe",
                        Some "2.0",
                        Some "https://example.org/recipes/unassigned"
                    )
                )

            Expect.isTrue
                (obj.ReferenceEquals(indexedAssigned, assigned))
                "The durable-id entry must resolve to the exact assigned resource."

            Expect.isTrue
                (obj.ReferenceEquals(indexedUnassigned, unassigned))
                "The metadata fallback must resolve to the exact unassigned stored resource."

            Expect.isTrue
                (processObject.ExecutesRecipe
                 |> Option.exists (fun candidate -> obj.ReferenceEquals(candidate, indexedAssigned)))
                "The Process must point at the same Recipe object exposed by the resource index."

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

        testCase "recipe assignment reuses the indexed resource"
        <| fun _ ->
            let recipe = Recipe()
            recipe.SetProperty("@id", "recipe:stored")

            let input = Sample("split-input")
            let output = Sample("split-output")

            let processObject =
                mkProcessFull "stage-neutral" (Some recipe) [ SampleNode input ] [ SampleNode output ] []

            let dataset = Dataset("dataset-neutral", processes = [ processObject ])
            let arc = ARC("arc-neutral", hasPart = [ dataset ])
            arc.AddRecipe recipe
            let recipeCountBefore = arc.Recipes.Count

            let indexedRecipe =
                RecipeResourceIndex.tryCreate arc.Recipes
                |> expectOk
                |> Map.find (RecipeResourceKey.ById "recipe:stored")

            let converted = fromArc loadedTable arc |> expectOk

            let removedId = converted.Model.Connections |> Map.toList |> List.exactlyOne |> fst

            let session =
                Session.init converted.Model
                |> Session.removeConnection removedId
                |> expectOk
                |> fst

            let summary = writeBack converted.Index session arc |> expectOk

            Expect.isGreaterThan summary.AddedProcesses 0 "The fixture must exercise a real structural split."
            Expect.equal arc.Recipes.Count recipeCountBefore "Splitting must not grow the stored Recipe catalog."

            let assignedRecipes =
                dataset.Processes |> Seq.choose _.ExecutesRecipe |> Seq.toArray

            Expect.equal
                assignedRecipes.Length
                dataset.Processes.Count
                "Every resulting Process must retain the Recipe assignment."

            assignedRecipes
            |> Array.iter (fun candidate ->
                Expect.isTrue
                    (obj.ReferenceEquals(candidate, indexedRecipe))
                    "Every split Process must reuse the exact stored Recipe object."
            )

        testCase "recipe resources are never mutated by provenance writeback"
        <| fun _ ->
            let fixture = basic ()

            let first =
                Recipe(
                    name = "first recipe",
                    description = "first description",
                    version = "1.0",
                    url = "https://example.org/recipes/first",
                    components = [
                        Annotation(
                            "first component",
                            value = "first payload",
                            valueTAN = "https://example.org/terms/first",
                            additionalType = "Component"
                        )
                    ]
                )

            first.SetProperty("@id", "recipe:first")

            let second =
                Recipe(
                    name = "second recipe",
                    description = "second description",
                    version = "2.0",
                    url = "https://example.org/recipes/second",
                    components = [
                        Annotation(
                            "second component",
                            value = "second payload",
                            unit = "second unit",
                            additionalType = "Component"
                        )
                    ]
                )

            second.SetProperty("@id", "recipe:second")
            fixture.Arc.AddRecipe first
            fixture.Arc.AddRecipe second

            let storedBefore = fixture.Arc.Recipes |> Seq.toArray

            let payloadBefore =
                storedBefore
                |> Array.map (fun recipe ->
                    RecipeResourceKey.tryDurableId recipe, ProcessCore.Yaml.Recipe.toYamlString None recipe
                )

            let assertStoredResourcesUnchanged () =
                let storedAfter = fixture.Arc.Recipes |> Seq.toArray
                Expect.equal storedAfter.Length storedBefore.Length "Writeback must preserve the Recipe catalog size."

                Array.zip storedBefore storedAfter
                |> Array.iter (fun (before, after) ->
                    Expect.isTrue
                        (obj.ReferenceEquals(before, after))
                        "Writeback must preserve each exact stored Recipe reference and its order."
                )

                let payloadAfter =
                    storedAfter
                    |> Array.map (fun recipe ->
                        RecipeResourceKey.tryDurableId recipe, ProcessCore.Yaml.Recipe.toYamlString None recipe
                    )

                Expect.sequenceEqual
                    payloadAfter
                    payloadBefore
                    "Writeback must preserve every stored Recipe identity and serialized payload byte-for-byte."

            let writeBackNoOp expectedRecipe =
                let converted = fromArc loadedTable fixture.Arc |> expectOk

                let summary =
                    writeBack converted.Index (Session.init converted.Model) fixture.Arc |> expectOk

                Expect.equal
                    summary
                    {
                        UpdatedAnnotations = 0
                        AddedAnnotations = 0
                        AddedNodes = 0
                        AddedProcesses = 0
                        RemovedProcesses = 0
                    }
                    "A Recipe reference-only change must not create provenance writeback work."

                match expectedRecipe, fixture.Process.ExecutesRecipe with
                | Some expected, Some actual ->
                    Expect.isTrue
                        (obj.ReferenceEquals(expected, actual))
                        "The Process must retain the exact assigned stored Recipe resource."
                | None, None -> ()
                | _ -> failtest "The Process Recipe assignment did not match the requested operation."

                assertStoredResourcesUnchanged ()

            fixture.Process.ExecutesRecipe <- Some first
            writeBackNoOp (Some first)

            fixture.Process.ExecutesRecipe <- Some second
            writeBackNoOp (Some second)

            fixture.Process.ExecutesRecipe <- None
            writeBackNoOp None
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

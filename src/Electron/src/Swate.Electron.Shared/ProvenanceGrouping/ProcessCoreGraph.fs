module Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreGraph

open System.Globalization
open System.Text
open ProcessCore
open Swate.Components.ProcessCore.Copy
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreAdapterTypes

type DatasetEntry = { Path: string list; Dataset: Dataset }

/// Length-prefixed encoding for an optional string field so concatenated
/// fingerprint segments cannot collide across different field boundaries.
let private field (value: string option) =
    match value with
    | None -> "-1:"
    | Some text -> string text.Length + ":" + text

let datasetEntries (arc: ARC) : DatasetEntry list =
    let rec walk (path: string list) (dataset: Dataset) : DatasetEntry list =
        let currentPath = path @ [ dataset.Identifier ]

        {
            Path = currentPath
            Dataset = dataset
        }
        :: (dataset.HasPart |> Seq.toList |> List.collect (walk currentPath))

    walk [] (arc :> Dataset)

let resolveDatasetMatches (path: string list) (arc: ARC) : Dataset list =
    datasetEntries arc
    |> List.filter (fun entry -> entry.Path = path)
    |> List.map (fun entry -> entry.Dataset)

let tryResolveDataset (path: string list) (arc: ARC) : Dataset option =
    match resolveDatasetMatches path arc with
    | [ dataset ] -> Some dataset
    | _ -> None

let tryDatasetPath (dataset: Dataset) (arc: ARC) : string list option =
    datasetEntries arc
    |> List.tryFind (fun entry -> obj.ReferenceEquals(entry.Dataset, dataset))
    |> Option.map (fun entry -> entry.Path)

let processLocation (datasetPath: string list) (index: int) (proc: Process) : ProcessCoreProcessLocation = {
    DatasetPath = datasetPath
    ProcessIndex = index
    ExpectedName = proc.Name
}

let tryResolveProcess (location: ProcessCoreProcessLocation) (arc: ARC) : Process option =
    tryResolveDataset location.DatasetPath arc
    |> Option.bind (fun dataset ->
        if location.ProcessIndex >= 0 && location.ProcessIndex < dataset.Processes.Count then
            let proc = dataset.Processes.[location.ProcessIndex]

            if proc.Name = location.ExpectedName then
                Some proc
            else
                None
        else
            None
    )

/// Complete, converter-owned payload fingerprint. ProcessCore's equality
/// intentionally ignores several published fields, so it is not suitable for
/// stale-source detection or writeback collision checks.
let canonicalAnnotationFingerprint (annotation: Annotation) : ProcessCoreCanonicalAnnotationFingerprint = {
    Payload = ProcessCore.Yaml.Annotation.toYamlString None annotation
}

/// Complete Recipe serialization including metadata, nested Parameters,
/// Components, additional properties, and dynamic/overflow data.
let recipePayloadFingerprint (recipe: Recipe) : ProcessCoreRecipePayloadFingerprint = {
    Payload = ProcessCore.Yaml.Recipe.toYamlString None recipe
}

let private appendAnnotation (sb: StringBuilder) (annotation: Annotation) =
    sb.Append(field (Some((canonicalAnnotationFingerprint annotation).Payload)))
    |> ignore

let private nodeAdditionalType (node: IONode) =
    match node with
    | SampleNode sample -> sample.AdditionalType
    | DataNode data -> data.AdditionalType

let nodeAdditionalProperties (node: IONode) : Annotation seq =
    match node with
    | SampleNode sample -> sample.AdditionalProperty :> seq<Annotation>
    | DataNode data -> data.AdditionalProperty :> seq<Annotation>

/// Canonical, Fable-friendly, length-prefixed encoding of the reachable
/// graph state used for round-trip and staleness detection. Deliberately not
/// `GetHashCode()`, which is unstable across runs and does not distinguish
/// content changes from unrelated objects.
let graphFingerprint (arc: ARC) : string =
    let sb = StringBuilder()

    for entry in datasetEntries arc do
        sb.Append(field (Some(String.concat "/" entry.Path))) |> ignore

        for index in 0 .. entry.Dataset.Processes.Count - 1 do
            let proc = entry.Dataset.Processes.[index]
            sb.Append(field (Some(string index))) |> ignore
            sb.Append(field (Some proc.Name)) |> ignore
            sb.Append(field proc.AdditionalType) |> ignore

            for node in Seq.append (proc.Input |> Option.toList) (proc.Output |> Option.toList) do
                let kind =
                    match node with
                    | SampleNode _ -> "S"
                    | DataNode _ -> "D"

                sb.Append(field (Some kind)) |> ignore
                sb.Append(field (Some(node.Key()))) |> ignore
                sb.Append(field (nodeAdditionalType node)) |> ignore

                for annotation in nodeAdditionalProperties node do
                    appendAnnotation sb annotation

            for parameterValue in proc.ParameterValue do
                appendAnnotation sb parameterValue

            match proc.ExecutesRecipe with
            | Some recipe -> sb.Append(field (Some(RecipeResourceKey.ofRecipeStableString recipe))) |> ignore
            | None -> sb.Append("-1:") |> ignore

    // Stored Recipes are resources in their own right, including unassigned
    // resources. Preserve resource order and fingerprint every complete
    // payload so external resource edits invalidate a loaded session.
    sb.Append(field (Some "stored-recipes")) |> ignore

    for recipe in arc.Recipes do
        sb.Append(field (Some(RecipeResourceKey.ofRecipeStableString recipe))) |> ignore

        sb.Append(field (Some((recipePayloadFingerprint recipe).Payload))) |> ignore

    sb.ToString()

let private componentLocations resourceKey (recipe: Recipe) =
    let resourceId = RecipeResourceKey.toStableString resourceKey

    recipe.Components
    |> Seq.mapi (fun position recipeComponent -> {
        ComponentKey = $"{resourceId}/component/{position}"
        Position = position
        Fingerprint = canonicalAnnotationFingerprint recipeComponent
    })
    |> Seq.toList

let private tryRecipeResources
    (referencingProcesses: Map<RecipeResourceKey, ProcessCoreProcessLocation list>)
    (arc: ARC)
    =
    match RecipeResourceIndex.tryCreate arc.Recipes with
    | Error(RecipeResourceIndexError.AmbiguousKey key) ->
        Error(ProcessCoreCanonicalConversionError.AmbiguousRecipeResourceKey key)
    | Ok resources ->
        let missingReference =
            referencingProcesses
            |> Map.toList
            |> List.tryPick (fun (key, _) -> if resources.ContainsKey key then None else Some key)

        match missingReference with
        | Some key -> Error(ProcessCoreCanonicalConversionError.RecipeResourceNotFound key)
        | None ->
            resources
            |> Map.toList
            |> List.map (fun (resourceKey, recipe) ->
                let scheme = ProcessCoreCanonicalKinds.processCoreRecipeScheme
                let resourceId = RecipeResourceKey.toStableString resourceKey

                (scheme, resourceId),
                {
                    Scheme = scheme
                    ResourceKey = resourceKey
                    Resource = recipe
                    LoadFingerprint = recipePayloadFingerprint recipe
                    Components = componentLocations resourceKey recipe
                    ReferencingProcesses = referencingProcesses |> Map.tryFind resourceKey |> Option.defaultValue []
                }
            )
            |> Map.ofList
            |> Ok

/// Validates the one-source/one-selection boundary before collapsing source
/// ownership into a map, indexes every stored Recipe exactly, and captures the
/// complete graph fingerprint.
let tryCreateCanonicalIndex
    (seed: ProcessCoreCanonicalIndexSeed)
    (arc: ARC)
    : Result<ProcessCoreCanonicalIndex, ProcessCoreCanonicalConversionError> =
    let duplicateSource =
        seed.SourceLocations
        |> List.countBy fst
        |> List.tryPick (fun (sourceId, count) -> if count = 1 then None else Some sourceId)

    match duplicateSource with
    | Some sourceId -> Error(ProcessCoreCanonicalConversionError.DuplicateSourceOwnership sourceId)
    | None ->
        let selected = seed.LoadedProcessGroups |> Set.ofList

        let unselectedOwnership =
            seed.SourceLocations
            |> List.tryPick (fun (sourceId, location) ->
                if selected.Contains location then
                    None
                else
                    Some(sourceId, location)
            )

        match unselectedOwnership with
        | Some(sourceId, location) ->
            Error(ProcessCoreCanonicalConversionError.SourceOwnsUnselectedProcessGroup(sourceId, location))
        | None ->
            let invalidLocationOwnership =
                seed.LoadedProcessGroups
                |> List.tryPick (fun selectedLocation ->
                    let owningSources =
                        seed.SourceLocations
                        |> List.choose (fun (sourceId, ownedLocation) ->
                            if ownedLocation = selectedLocation then
                                Some sourceId
                            else
                                None
                        )

                    match owningSources with
                    | [ _ ] -> None
                    | [] -> Some(ProcessCoreCanonicalConversionError.ProcessGroupWithoutSource selectedLocation)
                    | sources ->
                        Some(
                            ProcessCoreCanonicalConversionError.ProcessGroupOwnedByMultipleSources(
                                selectedLocation,
                                sources
                            )
                        )
                )

            match invalidLocationOwnership with
            | Some error -> Error error
            | None ->
                tryRecipeResources seed.ReferencingProcessesByRecipe arc
                |> Result.map (fun recipeResources ->
                    let existingProcessGroupNamesByDataset =
                        datasetEntries arc
                        |> List.groupBy _.Path
                        |> List.map (fun (path, entries) ->
                            path,
                            entries
                            |> List.collect (fun entry -> entry.Dataset.Processes |> Seq.map _.Name |> Seq.toList)
                            |> Set.ofList
                        )
                        |> Map.ofList

                    {
                        LoadedProcessGroups = seed.LoadedProcessGroups
                        SourceLocations = seed.SourceLocations |> Map.ofList
                        ExistingProcessGroupNamesByDataset = existingProcessGroupNamesByDataset
                        ArcFingerprint = graphFingerprint arc
                        NodeLocations = seed.NodeLocations
                        ProcessLocations = seed.ProcessLocations
                        LinkLocations = seed.LinkLocations
                        AssignmentLocations = seed.AssignmentLocations
                        AssignmentValueIds = seed.AssignmentValueIds
                        RecipeResources = recipeResources
                        GenericPropertyMappings = seed.GenericPropertyMappings
                    }
                )

let nodeLocation (node: IONode) : ProcessCoreNodeLocation =
    match node with
    | SampleNode _ -> {
        Kind = ProcessCoreNodeKind.Sample
        Key = node.Key()
      }
    | DataNode _ -> {
        Kind = ProcessCoreNodeKind.Data
        Key = node.Key()
      }

let nodeDisplayName (node: IONode) =
    match node with
    | SampleNode sample -> sample.Name
    | DataNode data -> data.Name

/// Canonical annotation conversion. Recipe references are created separately
/// from the stored-resource index; an ordinary Annotation yields Text or Term.
open Swate.Components.Page.ProvenanceGrouping.Values

let canonicalValueFromAnnotation (annotation: Annotation) : ProvenanceValue =
    match annotation.ValueTAN with
    | Some accession ->
        ProvenanceValue.Term {
            Name = annotation.ValueText
            TermSource = None
            TermAccession = Some accession
        }
    | None -> ProvenanceValue.Text annotation.ValueText

let tryResolveNode (location: ProcessCoreNodeLocation) (arc: ARC) : IONode option =
    arc.AllNodes() |> Seq.tryFind (fun node -> node.Key() = location.Key)

/// Builds a fresh ProcessCore node from canonical identity only. Source
/// appearance/header metadata is deliberately not consulted: canonical node
/// identity is exactly (kind ID, name).
open Swate.Components.Page.ProvenanceGrouping.Domain

let nodeFromCanonicalNode (node: CanonicalNode) : Result<IONode, ProcessCoreCanonicalWritebackError> =
    let additionalType defaultLabel =
        if
            System.String.IsNullOrWhiteSpace node.Kind.Label
            || node.Kind.Label = defaultLabel
        then
            None
        else
            Some node.Kind.Label

    if node.Key.KindId = ProcessCoreCanonicalKinds.sampleEndpoint.Id then
        Ok(
            SampleNode(
                Sample(node.Key.Name, ?additionalType = additionalType ProcessCoreCanonicalKinds.sampleEndpoint.Label)
            )
        )
    elif node.Key.KindId = ProcessCoreCanonicalKinds.dataEndpoint.Id then
        let path, selector =
            match node.Key.Name.LastIndexOf '#' with
            | -1 -> node.Key.Name, None
            | index -> node.Key.Name.Substring(0, index), Some(node.Key.Name.Substring(index + 1))

        Ok(
            DataNode(
                Data(
                    path,
                    ?selector = selector,
                    ?additionalType = additionalType ProcessCoreCanonicalKinds.dataEndpoint.Label
                )
            )
        )
    else
        Error(ProcessCoreCanonicalWritebackError.UnsupportedEndpointKind node.Key.KindId)

/// Uses the public singular input/output APIs so back-edges and
/// canonicalization remain consistent.
let replaceProcessIO (inputs: IONode list) (outputs: IONode list) (proc: Process) : unit =
    proc.ClearInput()
    proc.ClearOutput()

    for node in inputs do
        proc.SetInput node

    for node in outputs do
        proc.SetOutput node

let addProcess (dataset: Dataset) (proc: Process) : unit = dataset.AddProcess proc

let removeProcess (dataset: Dataset) (proc: Process) : unit = dataset.RemoveProcess proc

module Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreGraph

open System.Globalization
open System.Text
open ProcessCore
open Swate.Components.ProcessCore.Copy
open Swate.Components.Page.ProvenanceGrouping.ProvenanceTypes
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

let annotationFingerprint (annotation: Annotation) : ProcessCoreAnnotationFingerprint = {
    Name = annotation.Name
    Value = annotation.Value
    Unit = annotation.Unit
    NameTAN = annotation.NameTAN
    ValueTAN = annotation.ValueTAN
    UnitTAN = annotation.UnitTAN
    AdditionalType = annotation.AdditionalType
}

/// Mirrors ProcessCore's own `Annotation.Equals` (Name, Value, Unit, NameTAN).
/// Used only to detect public-API deduplication collisions, never as a
/// substitute for the full fingerprint when deciding round-trip identity.
let annotationsEqualByProcessCoreKey (left: Annotation) (right: Annotation) : bool =
    left.Name = right.Name
    && left.Value = right.Value
    && left.Unit = right.Unit
    && left.NameTAN = right.NameTAN

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
                |> Result.map (fun recipeResources -> {
                    LoadedProcessGroups = seed.LoadedProcessGroups
                    SourceLocations = seed.SourceLocations |> Map.ofList
                    ArcFingerprint = graphFingerprint arc
                    NodeLocations = seed.NodeLocations
                    ProcessLocations = seed.ProcessLocations
                    LinkLocations = seed.LinkLocations
                    AssignmentLocations = seed.AssignmentLocations
                    AssignmentValueIds = seed.AssignmentValueIds
                    RecipeResources = recipeResources
                    GenericPropertyMappings = seed.GenericPropertyMappings
                })

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

/// `ValueTAN` present means the value is ontology-backed. ProcessCore has no
/// separate ontology-source field, so converted terms always use
/// `TermSource = None`; writeback stores only the TAN.
let valueFromAnnotation (annotation: Annotation) : ProvenanceValue =
    match annotation.ValueTAN with
    | Some accession ->
        ProvenanceValue.Term {
            Name = annotation.ValueText
            TermSource = None
            TermAccession = Some accession
        }
    | None -> ProvenanceValue.Text annotation.ValueText

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

open Swate.Components.Page.ProvenanceGrouping.ProvenanceTypes

let unitFromAnnotation (annotation: Annotation) : ProvenanceTerm option =
    match annotation.Unit with
    | Some unitText ->
        Some {
            Name = unitText
            TermSource = None
            TermAccession = annotation.UnitTAN
        }
    | None -> None

let sourceRef (location: ProcessCoreTableLocation) : ProvenanceSourceRef = {
    Id = String.concat "/" (location.DatasetPath @ [ location.TableName ])
    Name = location.TableName
}

let processId (location: ProcessCoreProcessLocation) : ProvenanceProcessId =
    String.concat "/" (location.DatasetPath @ [ string location.ProcessIndex; location.ExpectedName ])

let tryResolveNode (location: ProcessCoreNodeLocation) (arc: ARC) : IONode option =
    arc.AllNodes() |> Seq.tryFind (fun node -> node.Key() = location.Key)

let tryResolveAnnotation (location: ProcessCoreAnnotationLocation) (arc: ARC) : Annotation option =
    let atPosition (position: int) (annotations: Annotation seq) =
        let list = annotations |> Seq.toList

        if position >= 0 && position < list.Length then
            Some list.[position]
        else
            None

    match location.Owner with
    | ProcessCoreAnnotationOwner.NodeAdditionalProperty nodeLocation ->
        tryResolveNode nodeLocation arc
        |> Option.bind (fun node -> nodeAdditionalProperties node |> atPosition location.Position)
    | ProcessCoreAnnotationOwner.ProcessParameterValue procLocation ->
        tryResolveProcess procLocation arc
        |> Option.bind (fun proc -> proc.ParameterValue :> Annotation seq |> atPosition location.Position)
    | ProcessCoreAnnotationOwner.RecipeComponent procLocation ->
        tryResolveProcess procLocation arc
        |> Option.bind (fun proc -> proc.ExecutesRecipe)
        |> Option.bind (fun recipe -> recipe.Components :> Annotation seq |> atPosition location.Position)

/// Mutates only `Value`/`ValueTAN`/`Unit`/`UnitTAN`. Category (`Name`/`NameTAN`)
/// is set once at annotation creation and is never changed by a value update.
let applyValue (value: ProvenanceValue) (unit: ProvenanceTerm option) (annotation: Annotation) : unit =
    match value with
    | ProvenanceValue.Text text ->
        annotation.Value <- Some text
        annotation.ValueTAN <- None
    | ProvenanceValue.Integer intValue ->
        annotation.Value <- Some(intValue.ToString(CultureInfo.InvariantCulture))
        annotation.ValueTAN <- None
    | ProvenanceValue.Float floatValue ->
        annotation.Value <-
#if FABLE_COMPILER
            Some(string floatValue)
#else
            Some(floatValue.ToString("R", CultureInfo.InvariantCulture))
#endif
        annotation.ValueTAN <- None
    | ProvenanceValue.Term term ->
        annotation.Value <- Some term.Name
        annotation.ValueTAN <- term.TermAccession

    match unit with
    | Some unitTerm ->
        annotation.Unit <- Some unitTerm.Name
        annotation.UnitTAN <- unitTerm.TermAccession
    | None ->
        annotation.Unit <- None
        annotation.UnitTAN <- None

/// Creates a brand-new annotation for a value/unit created in the editor.
/// `additionalType` carries the ProcessCore discriminator (e.g.
/// `CharacteristicValue`, `ParameterValue`, `Component`); `None` leaves it
/// unset for the generic node-annotation kind.
let annotationFromValue
    (additionalType: string option)
    (header: ProvenancePropertyHeader)
    (value: ProvenanceValue)
    (unit: ProvenanceTerm option)
    : Annotation =
    let annotation =
        Annotation(header.Category.Name, ?nameTAN = header.Category.TermAccession, ?additionalType = additionalType)

    applyValue value unit annotation
    annotation

// ── Graph mutation primitives ───────────────────────────────────────────────

/// Builds a fresh `Sample`/`Data` node from a set's final editor name.
/// ProcessCore canonicalizes by key when the node is later added to a
/// process via `SetInput`/`SetOutput`, so a freshly constructed node
/// converges onto any already-registered node with the same key.
let nodeFromSet (set: ProvenanceSet) : Result<IONode, ProcessCoreWritebackError> =
    let additionalType =
        if
            System.String.IsNullOrWhiteSpace set.Header.Text
            || set.Header.Text = set.Header.Kind.Label
        then
            None
        else
            Some set.Header.Text

    if set.Header.Kind.Id = ProcessCoreKinds.sampleEndpoint.Id then
        Ok(SampleNode(Sample(set.Name, ?additionalType = additionalType)))
    elif set.Header.Kind.Id = ProcessCoreKinds.dataEndpoint.Id then
        let path, selector =
            match set.Name.LastIndexOf '#' with
            | -1 -> set.Name, None
            | index -> set.Name.Substring(0, index), Some(set.Name.Substring(index + 1))

        Ok(DataNode(Data(path, ?selector = selector, ?additionalType = additionalType)))
    else
        Error(ProcessCoreWritebackError.UnsupportedEndpointKind set.Header.Kind.Id)

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

let cloneProcessShell (proc: Process) : Process =
    let clone = Process(proc.Name, ?additionalType = proc.AdditionalType)

    match proc.ExecutesRecipe with
    | Some recipe -> clone.ExecutesRecipe <- Some recipe
    | None -> ()

    for parameterValue in proc.ParameterValue do
        clone.AddParameterValue parameterValue

    clone

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

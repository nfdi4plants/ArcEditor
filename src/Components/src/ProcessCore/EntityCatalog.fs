module Swate.Components.ProcessCore.EntityCatalog

open System
open ProcessCore
open Swate.Components.ProcessCore.ObjectGraph
open Swate.Components.ProcessCore.Types

/// Normalizes nonblank text for use by fallback-aware display names.
let private nonEmpty (value: string) =
    if String.IsNullOrWhiteSpace value then
        None
    else
        Some(value.Trim())

/// Returns normalized text or the supplied fallback when it is blank.
let nonEmptyOr fallback value =
    nonEmpty value |> Option.defaultValue fallback

/// Returns the first nonblank optional name or the supplied fallback.
let nameOr fallback values =
    values |> Seq.choose id |> Seq.tryPick nonEmpty |> Option.defaultValue fallback

/// Returns the preferred display name for a dataset.
let datasetName (dataset: Dataset) =
    nameOr "Unnamed dataset" [ dataset.Title; Some dataset.Identifier ]

/// Returns the preferred display name for a data context.
let dataContextName (dataContext: DataContext) =
    nameOr "Unnamed data context" [ dataContext.Label; Some dataContext.Data.Name ]

/// Returns the preferred display name for an agent.
let agentName (agent: Agent) =
    let fullName =
        [|
            nonEmpty agent.GivenName
            agent.FamilyName |> Option.bind nonEmpty
        |]
        |> Array.choose id
        |> String.concat " "
        |> nonEmpty

    nameOr "Unnamed agent" [ fullName; agent.Identifier; agent.Email ]

let private valueKey (value: string) = $"{value.Length}:{value}"

let private optionKey value =
    value
    |> Option.map (fun value -> "S" + valueKey value)
    |> Option.defaultValue "N"

let private fieldsKey values =
    values |> Seq.map valueKey |> String.concat ""

/// Creates a stable value-based identity for a defined term.
let private definedTermKey (term: DefinedTerm) =
    fieldsKey [
        term.Name
        optionKey term.TAN
        optionKey term.InDefinedTermSet
    ]

/// Creates a stable value-based identity for an annotation.
let annotationKey (annotation: Annotation) =
    fieldsKey [
        annotation.Name
        optionKey annotation.Value
        optionKey annotation.NameTAN
    ]

/// Creates a stable value-based identity for a data object.
let dataKey (data: Data) =
    fieldsKey [ data.Path; optionKey data.Selector ]

/// Creates a stable value-based identity for a data context and its semantic terms.
let dataContextKey (dataContext: DataContext) =
    let termKey prefix term =
        term |> Option.map (definedTermKey >> (+) prefix) |> Option.defaultValue "N"

    fieldsKey [
        dataContext.Data.Path
        optionKey dataContext.Data.Selector
        termKey "E" dataContext.Explication
        termKey "O" dataContext.ObjectType
        termKey "U" dataContext.Unit
        optionKey dataContext.Label
        optionKey dataContext.Description
        optionKey dataContext.GeneratedBy
    ]

/// Creates a stable value-based identity for a recipe.
let recipeKey (recipe: Recipe) =
    fieldsKey [ optionKey recipe.Name; optionKey recipe.Version ]

/// Uses the persistent identifier when available and otherwise derives an agent identity.
let agentKey (agent: Agent) =
    agent.Id
    |> Option.defaultValue (
        fieldsKey [
            agent.GivenName
            optionKey agent.FamilyName
            optionKey agent.Email
        ]
    )

/// Uses the persistent identifier when available and otherwise uses the organization name.
let organizationKey (organization: Organization) =
    organization.Id |> Option.defaultValue organization.Name

/// Uses the persistent identifier when available and otherwise derives an article identity.
let articleKey (article: ScholarlyArticle) =
    article.Id
    |> Option.defaultValue (fieldsKey [ article.Headline; optionKey article.Identifier ])

/// Returns the distinct agents referenced by datasets and citation authors in the ARC.
let agents (arc: ARC) =
    datasetsIncludingRoot arc
    |> Seq.collect (fun dataset -> Seq.append dataset.Agents (dataset.Citations |> Seq.collect _.Authors))
    |> Seq.distinctBy agentKey
    |> Seq.toArray

/// Returns the distinct organizations referenced by ARC agents.
let organizations (arc: ARC) =
    agents arc
    |> Seq.choose _.Affiliation
    |> Seq.distinctBy organizationKey
    |> Seq.toArray

/// Traverses the current ARC and builds the candidate snapshot. Types that are not
/// exposed by a direct ARC traversal are collected through their owning relationships.
let createImportCatalog (arc: ARC) =
    let samples = arc.AllSamples() |> Seq.toArray
    let data = arc.AllData() |> Seq.toArray

    {
        Datasets = descendantDatasets arc
        Processes = arc.AllProcesses() |> Seq.toArray
        Samples = samples
        Data = data
        Recipes = recipes arc
        Annotations = arc.AllAnnotations() |> Seq.toArray
        DataContexts = arc.AllDataContexts() |> Seq.toArray
        Agents = agents arc
        ScholarlyArticles = arc.AllCitations() |> Seq.toArray
        IONodes = Array.append (samples |> Array.map SampleNode) (data |> Array.map DataNode)
    }

/// Tests whether target is candidate or one of candidate's nested datasets.
let rec containsDataset (target: Dataset) (candidate: Dataset) =
    obj.ReferenceEquals(target, candidate)
    || (candidate.HasPart |> Seq.exists (containsDataset target))

/// Follows dataset ownership to the root dataset.
let rec rootDataset (current: Dataset) =
    current.PartOf |> Option.map rootDataset |> Option.defaultValue current

/// Enumerates a data object and all of its nested parts.
let rec private dataAndParts (data: Data) = seq {
    yield data

    for child in data.HasPart do
        yield! dataAndParts child
}

/// Enumerates data occurrences owned by datasets or referenced by process rows.
let dataOccurrences (datasets: Dataset array) (processes: Process array) = seq {
    for dataset in datasets do
        for data in dataset.DataFiles do
            yield! dataAndParts data

    for processObject in processes do
        for node in Seq.append processObject.Inputs processObject.Outputs do
            match node with
            | DataNode data -> yield! dataAndParts data
            | SampleNode _ -> ()
}

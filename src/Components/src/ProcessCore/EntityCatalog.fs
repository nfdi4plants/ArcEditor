module Swate.Components.ProcessCore.EntityCatalog

open System
open ProcessCore
open Swate.Components.ProcessCore.ObjectGraph
open Swate.Components.ProcessCore.Types

let nonEmpty (value: string) =
    if String.IsNullOrWhiteSpace value then
        None
    else
        Some(value.Trim())

let nonEmptyOr fallback value =
    nonEmpty value |> Option.defaultValue fallback

let nameOr fallback values =
    values |> Seq.choose id |> Seq.tryPick nonEmpty |> Option.defaultValue fallback

let datasetName (dataset: Dataset) =
    nameOr "Unnamed dataset" [ dataset.Title; Some dataset.Identifier ]

let dataContextName (dataContext: DataContext) =
    nameOr "Unnamed data context" [ dataContext.Label; Some dataContext.Data.Name ]

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

let definedTermKey (term: DefinedTerm) =
    fieldsKey [
        term.Name
        optionKey term.TAN
        optionKey term.InDefinedTermSet
    ]

let annotationKey (annotation: Annotation) =
    fieldsKey [
        annotation.Name
        optionKey annotation.Value
        optionKey annotation.NameTAN
    ]

let dataKey (data: Data) =
    fieldsKey [ data.Path; optionKey data.Selector ]

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

let recipeKey (recipe: Recipe) =
    fieldsKey [ optionKey recipe.Name; optionKey recipe.Version ]

let agentKey (agent: Agent) =
    agent.Id
    |> Option.defaultValue (
        fieldsKey [
            agent.GivenName
            optionKey agent.FamilyName
            optionKey agent.Email
        ]
    )

let organizationKey (organization: Organization) =
    organization.Id |> Option.defaultValue organization.Name

let articleKey (article: ScholarlyArticle) =
    article.Id
    |> Option.defaultValue (fieldsKey [ article.Headline; optionKey article.Identifier ])

let agents (arc: ARC) =
    datasetsIncludingRoot arc
    |> Seq.collect (fun dataset -> Seq.append dataset.Agents (dataset.Citations |> Seq.collect _.Authors))
    |> Seq.distinctBy agentKey
    |> Seq.toArray

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

let rec containsDataset (target: Dataset) (candidate: Dataset) =
    obj.ReferenceEquals(target, candidate)
    || (candidate.HasPart |> Seq.exists (containsDataset target))

let rec rootDataset (current: Dataset) =
    current.PartOf |> Option.map rootDataset |> Option.defaultValue current

let rec dataAndParts (data: Data) = seq {
    yield data

    for child in data.HasPart do
        yield! dataAndParts child
}

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

module Swate.Components.ProcessCore.EntityCommands

open ProcessCore
open Swate.Components.ProcessCore.ObjectGraph
open Swate.Components.ProcessCore.EntityCatalog

let private removeMatching key getKey remove (items: seq<'T>) =
    items |> Seq.filter (getKey >> (=) key) |> Seq.toArray |> Array.iter remove

let private removeNodeFromProcesses predicate (processes: Process array) =
    for processObject in processes do
        processObject.Inputs
        |> Seq.filter predicate
        |> Seq.toArray
        |> Array.iter processObject.RemoveInput

        processObject.Outputs
        |> Seq.filter predicate
        |> Seq.toArray
        |> Array.iter processObject.RemoveOutput

let removeSample (arc: ARC) (sample: Sample) =
    removeNodeFromProcesses
        (function
        | SampleNode candidate -> candidate.Name = sample.Name
        | _ -> false)
        (arc.AllProcesses() |> Seq.toArray)

let removeData (arc: ARC) (data: Data) =
    let datasets = datasetsIncludingRoot arc |> Seq.toArray
    let processes = arc.AllProcesses() |> Seq.toArray
    let key = dataKey data
    let allData = dataOccurrences datasets processes |> Seq.toArray

    removeNodeFromProcesses
        (function
        | DataNode candidate -> dataKey candidate = key
        | _ -> false)
        processes

    for dataset in datasets do
        removeMatching key dataKey dataset.RemoveDataFile dataset.DataFiles

        removeMatching
            key
            (fun (context: DataContext) -> dataKey context.Data)
            dataset.RemoveDataContext
            dataset.DataContexts

    for parent in allData do
        removeMatching key dataKey parent.RemovePart parent.HasPart

let removeRecipe (arc: ARC) (recipe: Recipe) =
    let key = recipeKey recipe

    for processObject in arc.AllProcesses() do
        match processObject.ExecutesProtocol with
        | Some candidate when recipeKey candidate = key -> processObject.ExecutesProtocol <- None
        | _ -> ()

let removeAnnotation (arc: ARC) (annotation: Annotation) =
    let key = annotationKey annotation

    let removeFrom items remove =
        removeMatching key annotationKey remove items

    let datasets = datasetsIncludingRoot arc |> Seq.toArray
    let processes = arc.AllProcesses() |> Seq.toArray

    for dataset in datasets do
        removeFrom dataset.AdditionalProperty dataset.RemoveAdditionalProperty

    for processObject in processes do
        removeFrom processObject.ParameterValue processObject.RemoveParameterValue

    for sample in arc.AllSamples() |> Seq.toArray do
        removeFrom sample.AdditionalProperty sample.RemoveAdditionalProperty

    for recipe in recipes arc do
        removeFrom recipe.Components recipe.RemoveComponent
        removeFrom recipe.AdditionalProperty recipe.RemoveAdditionalProperty

    for data in dataOccurrences datasets processes |> Seq.toArray do
        removeFrom data.AdditionalProperty data.RemoveAdditionalProperty

    for agent in agents arc do
        removeFrom agent.AdditionalProperty agent.RemoveAdditionalProperty

    for article in arc.AllCitations() |> Seq.toArray do
        removeFrom article.AdditionalProperty article.RemoveAdditionalProperty

let removeDataContext (arc: ARC) (dataContext: DataContext) =
    let key = dataContextKey dataContext

    for dataset in datasetsIncludingRoot arc do
        removeMatching key dataContextKey dataset.RemoveDataContext dataset.DataContexts

let removeAgent (arc: ARC) (agent: Agent) =
    let key = agentKey agent

    for dataset in datasetsIncludingRoot arc do
        removeMatching key agentKey dataset.RemoveAgent dataset.Agents

        for article in dataset.Citations |> Seq.toArray do
            removeMatching key agentKey article.RemoveAuthor article.Authors

let removeOrganization (arc: ARC) (organization: Organization) =
    let key = organizationKey organization

    for agent in agents arc do
        match agent.Affiliation with
        | Some affiliation when organizationKey affiliation = key -> agent.Affiliation <- None
        | _ -> ()

let removeScholarlyArticle (arc: ARC) (article: ScholarlyArticle) =
    let key = articleKey article

    for dataset in datasetsIncludingRoot arc do
        removeMatching key articleKey dataset.RemoveCitation dataset.Citations

let addSample (arc: ARC) value =
    let processObject = Process($"Process for {value}")
    processObject.AddInputSample(Sample(value))
    arc.AddProcess processObject

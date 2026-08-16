module Swate.Components.ProcessCore.EntityCatalog

open ProcessCore
open Swate.Components.ProcessCore.ObjectGraph
open Swate.Components.ProcessCore.Types

let agents (arc: ARC) =
    datasetsIncludingRoot arc
    |> Seq.collect (fun dataset -> Seq.append dataset.Agents (dataset.Citations |> Seq.collect _.Authors))
    |> Seq.distinctBy EntityIdentity.agentKey
    |> Seq.toArray

let organizations (arc: ARC) =
    agents arc
    |> Seq.choose _.Affiliation
    |> Seq.distinctBy EntityIdentity.organizationKey
    |> Seq.toArray

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

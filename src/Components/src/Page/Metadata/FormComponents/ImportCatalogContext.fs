module Swate.Components.Page.Metadata.FormComponents.ImportCatalogContext

open System.Collections.Generic
open Feliz
open ProcessCore

/// Snapshot of existing Process Core object references that metadata relationships can import.
/// Entries are grouped by type and deduplicated by reference identity; importing reuses an
/// entry rather than cloning it. See docs/ImportCatalog.md for the complete data flow.
type ImportCatalog = {
    Datasets: Dataset array
    Processes: Process array
    Samples: Sample array
    Data: Data array
    Recipes: Recipe array
    Annotations: Annotation array
    DataContexts: DataContext array
    Agents: Agent array
    ScholarlyArticles: ScholarlyArticle array
    IONodes: IONode array
}

module ImportCatalogContextHelper =

    // Process Core objects are mutable. Reference identity therefore distinguishes two
    // independent objects even when their current metadata values happen to be equal.
    let private distinctReferences items =
        let seen = HashSet<obj>(HashIdentity.Reference)
        items |> Seq.filter (box >> seen.Add) |> Seq.toArray

    let private descendantDatasets (arc: ARC) =
        let rec descendants (dataset: Dataset) = seq {
            for child in dataset.HasPart do
                yield child
                yield! descendants child
        }

        descendants arc |> distinctReferences

    let private datasetProtocols datasets =
        datasets
        |> Seq.collect (fun (dataset: Dataset) ->
            match dataset.TryGetPropertyValue("Protocols") with
            | Some(:? System.Collections.IEnumerable as protocols) ->
                protocols
                |> Seq.cast<obj>
                |> Seq.choose (
                    function
                    | :? Recipe as protocol -> Some protocol
                    | _ -> None
                )
            | _ -> Seq.empty
        )

    /// Traverses the current ARC and builds the candidate snapshot. Types that are not
    /// exposed by a direct ARC traversal are collected through their owning relationships.
    let create (arc: ARC) =
        let datasets = descendantDatasets arc
        let processes = arc.AllProcesses() |> distinctReferences

        let recipes =
            seq {
                yield! processes |> Seq.choose (fun processObject -> processObject.ExecutesProtocol)
                yield! datasetProtocols (Seq.append [ arc :> Dataset ] datasets)
            }
            |> distinctReferences

        let annotations = arc.AllAnnotations() |> distinctReferences

        let agents = arc.AllAgents() |> distinctReferences
        let articles = arc.AllCitations() |> distinctReferences
        let dataContexts = arc.AllDataContexts() |> distinctReferences

        let samples = arc.AllSamples() |> distinctReferences
        let data = arc.AllData() |> distinctReferences

        {
            Datasets = datasets
            Processes = processes
            Samples = samples
            Data = data
            Recipes = recipes
            Annotations = annotations
            DataContexts = dataContexts
            Agents = agents
            ScholarlyArticles = articles
            IONodes = Array.append (samples |> Array.map SampleNode) (data |> Array.map DataNode)
        }

/// Provided by MetadataBrowser so relationship components do not need to know ARC ownership.
/// None also allows the reusable metadata components to render outside MetadataBrowser.
let ImportCatalogCtx = React.createContext<ImportCatalog option> None

[<Hook>]
let useImportCatalogCtx () = React.useContext ImportCatalogCtx

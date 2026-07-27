module Swate.Components.Page.Metadata.FormComponents.ImportCatalogContext

open Feliz
open ProcessCore
open Swate.Components.ProcessCore.ObjectGraph

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

    /// Traverses the current ARC and builds the candidate snapshot. Types that are not
    /// exposed by a direct ARC traversal are collected through their owning relationships.
    let create (arc: ARC) =
        let datasets = descendantDatasets arc
        let processes = arc.AllProcesses() |> Seq.toArray
        let recipes = recipes arc

        let annotations = arc.AllAnnotations() |> Seq.toArray

        let agents = arc.AllAgents() |> Seq.toArray
        let articles = arc.AllCitations() |> Seq.toArray
        let dataContexts = arc.AllDataContexts() |> Seq.toArray

        let samples = arc.AllSamples() |> Seq.toArray
        let data = arc.AllData() |> Seq.toArray

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

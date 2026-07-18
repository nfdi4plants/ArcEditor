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
    FormalParameters: FormalParameter array
    DefinedTerms: DefinedTerm array
    Annotations: Annotation array
    DataContexts: DataContext array
    Agents: Agent array
    Organizations: Organization array
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

    /// Traverses the current ARC and builds the candidate snapshot. Types that are not
    /// exposed by a direct ARC traversal are collected through their owning relationships.
    let create (arc: ARC) =
        let processes = arc.AllProcesses() |> distinctReferences

        let recipes =
            processes
            |> Seq.choose (fun processObject -> processObject.ExecutesProtocol)
            |> distinctReferences

        let annotations = arc.AllAnnotations() |> distinctReferences

        let formalParameters =
            seq {
                for recipe in recipes do
                    yield! recipe.Parameters

                for annotation in annotations do
                    yield! annotation.InstanceOf |> Option.toList
            }
            |> distinctReferences

        let agents = arc.AllAgents() |> distinctReferences
        let articles = arc.AllCitations() |> distinctReferences
        let dataContexts = arc.AllDataContexts() |> distinctReferences

        let definedTerms =
            seq {
                for recipe in recipes do
                    yield! recipe.IntendedUse |> Option.toList

                for parameter in formalParameters do
                    yield! parameter.DefaultValue |> Option.toList

                for agent in agents do
                    yield! agent.JobTitles

                for context in dataContexts do
                    yield! context.Explication |> Option.toList
                    yield! context.ObjectType |> Option.toList
                    yield! context.Unit |> Option.toList

                for article in articles do
                    yield! article.CreativeWorkStatus |> Option.toList
            }
            |> distinctReferences

        let samples = arc.AllSamples() |> distinctReferences
        let data = arc.AllData() |> distinctReferences

        {
            Datasets = descendantDatasets arc
            Processes = processes
            Samples = samples
            Data = data
            Recipes = recipes
            FormalParameters = formalParameters
            DefinedTerms = definedTerms
            Annotations = annotations
            DataContexts = dataContexts
            Agents = agents
            Organizations = arc.AllOrganizations() |> distinctReferences
            ScholarlyArticles = articles
            IONodes = Array.append (samples |> Array.map SampleNode) (data |> Array.map DataNode)
        }

/// Provided by MetadataBrowser so relationship components do not need to know ARC ownership.
/// None also allows the reusable metadata components to render outside MetadataBrowser.
let ImportCatalogCtx = React.createContext<ImportCatalog option> None

[<Hook>]
let useImportCatalogCtx () = React.useContext ImportCatalogCtx

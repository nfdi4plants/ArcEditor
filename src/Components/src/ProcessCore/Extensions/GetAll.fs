[<AutoOpen>]
module ProcessCore.ExtensionsGetAll

open System.Collections.Generic
open ProcessCore

module ExtensionsGetAllHelper =

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
    let allDataSets (arc: ARC) : Dataset[] = descendantDatasets arc


    let allRecipes (arc: ARC) : Recipe[] =
        let datasets = descendantDatasets arc
        let processes = arc.AllProcesses() |> distinctReferences

        seq {
            yield! processes |> Seq.choose (fun processObject -> processObject.ExecutesProtocol)
            yield! datasetProtocols (Seq.append [ arc :> Dataset ] datasets)
        }
        |> distinctReferences


    let allFormalParameters (arc: ARC) : FormalParameter[] =
        let recipes = allRecipes arc
        let annotations = arc.AllAnnotations() |> distinctReferences

        seq {
            for recipe in recipes do
                yield! recipe.Parameters

            for annotation in annotations do
                yield! annotation.InstanceOf |> Option.toList
        }
        |> distinctReferences

    let allDefinedTerms (arc: ARC) : DefinedTerm[] =
        let recipes = allRecipes arc
        let formalParameters = allFormalParameters arc
        let agents = arc.AllAgents() |> distinctReferences
        let dataContexts = arc.AllDataContexts() |> distinctReferences
        let articles = arc.AllCitations() |> distinctReferences

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


type ARC with
    member this.GetAllDatasets() =
        ExtensionsGetAllHelper.allDataSets (this)

    member this.GetAllProcesses() = this.AllProcesses()
    member this.GetAllSamples() = this.AllSamples()
    member this.GetAllData() = this.AllData()

    member this.GetAllRecipes() =
        ExtensionsGetAllHelper.allRecipes (this)

    member this.GetAllFormalParameters() =
        ExtensionsGetAllHelper.allFormalParameters (this)

    member this.GetAllDefinedTerms() =
        ExtensionsGetAllHelper.allDefinedTerms (this)

    member this.GetAllAnnotations() = this.AllAnnotations()
    member this.GetAllDataContexts() = this.AllDataContexts()
    member this.GetAllAgents() = this.AllAgents()
    member this.GetAllOrganizations() = this.AllOrganizations()
    member this.GetAllScholarlyArticles() = this.AllCitations()

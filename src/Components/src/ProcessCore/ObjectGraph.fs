module Swate.Components.ProcessCore.ObjectGraph

open System.Collections.Generic
open ProcessCore

// Process Core objects are mutable. Reference identity therefore distinguishes two
// independent objects even when their current metadata values happen to be equal.
let private distinctReferences items =
    let seen = HashSet<obj>(HashIdentity.Reference)
    items |> Seq.filter (box >> seen.Add) |> Seq.toArray

/// Returns every dataset below the ARC root, excluding the root itself.
let descendantDatasets (arc: ARC) =
    let rec descendants (dataset: Dataset) = seq {
        for child in dataset.HasPart do
            yield child
            yield! descendants child
    }

    descendants arc |> distinctReferences

/// Returns the ARC root dataset and all of its descendant datasets.
let datasetsIncludingRoot (arc: ARC) =
    seq {
        yield arc :> Dataset
        yield! descendantDatasets arc
    }
    |> distinctReferences

/// Reads recipes stored in the dataset protocol relationship not exposed by the typed API.
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

/// Returns reference-unique recipes reachable from processes or dataset protocols.
let recipes (arc: ARC) : Recipe[] =
    seq {
        yield!
            arc.AllProcesses()
            |> Seq.choose (fun processObject -> processObject.ExecutesRecipe)

        yield! datasetProtocols (datasetsIncludingRoot arc)
    }
    |> distinctReferences

/// Returns reference-unique formal parameters reachable from recipes or annotations.
let formalParameters (arc: ARC) : FormalParameter[] =
    let recipes = recipes arc

    seq {
        for recipe in recipes do
            yield! recipe.Parameters

        for annotation in arc.AllAnnotations() do
            yield! annotation.InstanceOf |> Option.toList
    }
    |> distinctReferences

/// Returns reference-unique defined terms reachable through supported ProcessCore relationships.
let definedTerms (arc: ARC) : DefinedTerm[] =
    let recipes = recipes arc
    let formalParameters = formalParameters arc

    seq {
        for recipe in recipes do
            yield! recipe.IntendedUse |> Option.toList

        for parameter in formalParameters do
            yield! parameter.DefaultValue |> Option.toList

        for agent in arc.AllAgents() do
            yield! agent.JobTitles

        for context in arc.AllDataContexts() do
            yield! context.Explication |> Option.toList
            yield! context.ObjectType |> Option.toList
            yield! context.Unit |> Option.toList

        for article in arc.AllCitations() do
            yield! article.CreativeWorkStatus |> Option.toList
    }
    |> distinctReferences

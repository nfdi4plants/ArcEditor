module Swate.Components.Page.ArcObjectExplorer.ExplorerTree

open ProcessCore
open Swate.Components.Page.ArcObjectExplorer.Types
open Swate.Components.Page.ObjectBrowser
open Swate.Components.Page.ObjectBrowser.Types
open Swate.Components.Primitive.Tree.Types

let private traversalKey entity = $"{entity.memberKind}/{entity.key}"

let rec private createCollectionNode
    arcView
    (dataset: ProcessCoreEntity)
    levels
    relationshipKeys
    visited
    (collections: EntityCollection array)
    =
    let representative = collections.[0]

    let members =
        collections
        |> Array.collect _.members
        |> Array.distinctBy (fun entity -> entity.memberKind, entity.key)

    let nextLevels =
        levels
        @ [
            {
                RelationshipKey = representative.key
                Label = representative.label
                Members = members
                AllowedMemberKinds = representative.allowedMemberKinds
            }
        ]

    let nextRelationshipKeys = relationshipKeys @ [ representative.key ]

    let nextVisited =
        members |> Array.map traversalKey |> Set.ofArray |> Set.union visited

    let children =
        members
        |> Array.collect (fun entity ->
            if Set.contains (traversalKey entity) visited then
                [||]
            else
                MemberTree.entityCollections arcView entity
                |> Array.filter (fun collection -> not (Array.isEmpty collection.members))
        )
        |> Array.groupBy _.key
        |> Array.map (fun (_, grouped) ->
            createCollectionNode arcView dataset nextLevels nextRelationshipKeys nextVisited grouped
        )

    let relationshipPath = String.concat "/" nextRelationshipKeys

    {
        key = $"{dataset.key}/collection/{relationshipPath}"
        label = representative.label
        icon = Some representative.icon
        data =
            Some {
                Dataset = dataset
                Levels = nextLevels
            }
        children = children
    }

let private createDatasetNode arcView (dataset: Dataset) =
    let entity = MemberTree.createDatasetEntity dataset

    let children =
        MemberTree.entityCollections arcView entity
        |> Array.filter (fun collection -> not (Array.isEmpty collection.members))
        |> Array.groupBy _.key
        |> Array.map (fun (_, collections) ->
            createCollectionNode arcView entity [] [] (Set.singleton (traversalKey entity)) collections
        )

    {
        key = $"explorer-dataset/{entity.key}"
        label = entity.displayName
        icon = Some(MemberCatalog.iconForKind MemberKind.Dataset)
        data = Some { Dataset = entity; Levels = [] }
        children = children
    }

/// Creates the collection-oriented tree used by the ARC object explorer.
let createNodes arcView (arc: ARC) : TreeNode<ExplorerTreeTarget> array =
    arc.HasPart |> Seq.map (createDatasetNode arcView) |> Seq.toArray

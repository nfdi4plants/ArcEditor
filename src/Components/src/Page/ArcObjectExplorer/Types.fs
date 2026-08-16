module Swate.Components.Page.ArcObjectExplorer.Types

open Swate.Components.Page.ObjectBrowser.Types

/// One navigable relationship level in the explorer's collection view.
type ExplorerCollectionLevel = {
    RelationshipKey: string
    Label: string
    Members: ProcessCoreEntity array
    AllowedMemberKinds: MemberKind array
}

/// The selected dataset and collection path rendered by the explorer tree.
type ExplorerTreeTarget = {
    Dataset: ProcessCoreEntity
    Levels: ExplorerCollectionLevel list
}

module ExplorerTreeTarget =

    /// Creates the stable key shared by explorer navigation and tree nodes.
    let key target =
        target.Levels
        |> List.map _.RelationshipKey
        |> fun relationshipKeys -> target.Dataset.key :: relationshipKeys
        |> String.concat "/"

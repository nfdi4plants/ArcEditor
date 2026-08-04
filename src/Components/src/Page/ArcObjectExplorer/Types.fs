module Swate.Components.Page.ArcObjectExplorer.Types

open Swate.Components.Page.ObjectBrowser.Types

type ExplorerCollectionLevel = {
    RelationshipKey: string
    Label: string
    Members: ProcessCoreEntity array
    AllowedMemberKinds: MemberKind array
}

type ExplorerTreeTarget = {
    Dataset: ProcessCoreEntity
    Levels: ExplorerCollectionLevel list
}

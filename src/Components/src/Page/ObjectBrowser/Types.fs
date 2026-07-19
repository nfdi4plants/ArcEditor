module Swate.Components.Page.ObjectBrowser.Types

open ProcessCore

[<RequireQualifiedAccess>]
type MemberKind =
    | Dataset
    | Process
    | Sample
    | Data
    | Recipe
    | Annotation
    | DataContext
    | Agent
    | Organization
    | ScholarlyArticle

[<RequireQualifiedAccess>]
type ProcessCoreEntityValue =
    | Dataset of Dataset
    | Process of Process
    | Sample of Sample
    | Data of Data
    | Recipe of Recipe
    | FormalParameter of FormalParameter
    | DefinedTerm of DefinedTerm
    | Annotation of Annotation
    | DataContext of DataContext
    | Agent of Agent
    | Organization of Organization
    | ScholarlyArticle of ScholarlyArticle

type ProcessCoreEntity = {
    memberKind: MemberKind
    key: string
    displayName: string
    value: ProcessCoreEntityValue
}

[<RequireQualifiedAccess>]
type ContextMenuRequest =
    | AddMember of MemberKind
    | DeleteMembers of MemberKind
    | DeleteEntity of ProcessCoreEntity

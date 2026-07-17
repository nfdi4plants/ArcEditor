module Swate.Components.Page.ProcessCore.Types

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

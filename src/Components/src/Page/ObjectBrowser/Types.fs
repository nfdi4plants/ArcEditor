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

module ProcessCoreEntityValue =

    let tryGetProcessCoreObjectKind =
        function
        | ProcessCoreEntityValue.Dataset _ -> Some MemberKind.Dataset
        | ProcessCoreEntityValue.Process _ -> Some MemberKind.Process
        | ProcessCoreEntityValue.Sample _ -> Some MemberKind.Sample
        | ProcessCoreEntityValue.Data _ -> Some MemberKind.Data
        | ProcessCoreEntityValue.Recipe _ -> Some MemberKind.Recipe
        | ProcessCoreEntityValue.FormalParameter _
        | ProcessCoreEntityValue.DefinedTerm _ -> None
        | ProcessCoreEntityValue.Annotation _ -> Some MemberKind.Annotation
        | ProcessCoreEntityValue.DataContext _ -> Some MemberKind.DataContext
        | ProcessCoreEntityValue.Agent _ -> Some MemberKind.Agent
        | ProcessCoreEntityValue.Organization _ -> Some MemberKind.Organization
        | ProcessCoreEntityValue.ScholarlyArticle _ -> Some MemberKind.ScholarlyArticle

type ProcessCoreEntity = {
    memberKind: MemberKind
    key: string
    displayName: string
    value: ProcessCoreEntityValue
}

/// A named ProcessCore relationship and its immediate members.
type EntityCollection = {
    key: string
    label: string
    icon: string
    members: ProcessCoreEntity array
    allowedMemberKinds: MemberKind array
}

[<RequireQualifiedAccess>]
type ProcessRelationship =
    | Input
    | Output
    | ParameterValue

[<RequireQualifiedAccess>]
type ContextMenuRequest =
    | AddMember of MemberKind
    | AddProcessRelationship of Process * ProcessRelationship
    | DeleteMembers of MemberKind
    | DeleteEntity of ProcessCoreEntity

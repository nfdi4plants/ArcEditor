module Swate.Components.Page.ObjectBrowser.Types

open ProcessCore

/// ProcessCore entity categories supported by the Object Browser.
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

/// Strongly typed ProcessCore values that can be displayed by the Object Browser.
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

    /// Maps browser values to their selectable object kind when the value is independently browsable.
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

/// UI-facing identity and display data for one ProcessCore value.
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

/// Relationships that can be edited from a process context menu.
[<RequireQualifiedAccess>]
type ProcessRelationship =
    | Input
    | Output
    | ParameterValue

/// Object Browser action requested by a context-menu item.
[<RequireQualifiedAccess>]
type ContextMenuRequest =
    | AddMember of MemberKind
    | AddProcessRelationship of Process * ProcessRelationship
    | DeleteMembers of MemberKind
    | DeleteEntity of ProcessCoreEntity

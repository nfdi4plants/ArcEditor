namespace Swate.Components.Composite.ProcessCore

open Swate.Components.Composite.InteractiveList.Types

[<RequireQualifiedAccess>]
type ProcessCoreMemberKind =
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

module ProcessCoreMemberCatalog =

    let private create data label icon : InteractiveListData<ProcessCoreMemberKind> = {
        icon = icon
        label = label
        data = data
    }

    let Items: InteractiveListData<ProcessCoreMemberKind>[] = [|
        create ProcessCoreMemberKind.Dataset "Datasets" "swt:iconify-color swt:fluent-color--database-20"
        create
            ProcessCoreMemberKind.Process
            "Processes"
            "swt:iconify-color swt:fluent-color--arrow-clockwise-dashes-settings-20"
        create ProcessCoreMemberKind.Sample "Samples" "swt:iconify-color swt:fluent-color--molecule-20"
        create ProcessCoreMemberKind.Data "Data" "swt:iconify-color swt:fluent-color--data-line-20"
        create ProcessCoreMemberKind.Recipe "Recipes" "swt:iconify-color swt:fluent-color--clipboard-text-edit-20"
        create ProcessCoreMemberKind.Annotation "Annotations" "swt:iconify-color swt:fluent-color--comment-multiple-20"
        create ProcessCoreMemberKind.DataContext "DataContexts" "swt:iconify-color swt:fluent-color--content-view-20"
        create ProcessCoreMemberKind.Agent "Agents" "swt:iconify-color swt:fluent-color--agents-20"
        create ProcessCoreMemberKind.Organization "Organizations" "swt:iconify-color swt:fluent-color--org-20"
        create
            ProcessCoreMemberKind.ScholarlyArticle
            "ScholarlyArticles"
            "swt:iconify-color swt:fluent-color--document-text-20"
    |]

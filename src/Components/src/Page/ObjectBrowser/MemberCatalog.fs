module Swate.Components.Page.ObjectBrowser.MemberCatalog

open Swate.Components.Composite.InteractiveList.Types
open Swate.Components.Page.ObjectBrowser.Types

let private create data label icon : InteractiveListData<MemberKind> = {
    icon = icon
    label = label
    data = data
}

let iconForKind =
    function
    | MemberKind.Dataset -> "swt:iconify-color swt:fluent-color--database-20"
    | MemberKind.Process -> "swt:iconify-color swt:fluent-color--arrow-clockwise-dashes-settings-20"
    | MemberKind.Sample -> "swt:iconify-color swt:fluent-color--molecule-20"
    | MemberKind.Data -> "swt:iconify-color swt:fluent-color--data-line-20"
    | MemberKind.Recipe -> "swt:iconify-color swt:fluent-color--clipboard-text-edit-20"
    | MemberKind.Annotation -> "swt:iconify-color swt:fluent-color--comment-multiple-20"
    | MemberKind.DataContext -> "swt:iconify-color swt:fluent-color--content-view-20"
    | MemberKind.Agent -> "swt:iconify-color swt:fluent-color--agents-20"
    | MemberKind.Organization -> "swt:iconify-color swt:fluent-color--org-20"
    | MemberKind.ScholarlyArticle -> "swt:iconify-color swt:fluent-color--document-text-20"

let Items: InteractiveListData<MemberKind>[] = [|
    create MemberKind.Dataset "Datasets" (iconForKind MemberKind.Dataset)
    create MemberKind.Process "Processes" (iconForKind MemberKind.Process)
    create MemberKind.Sample "Samples" (iconForKind MemberKind.Sample)
    create MemberKind.Data "Data" (iconForKind MemberKind.Data)
    create MemberKind.Recipe "Recipes" (iconForKind MemberKind.Recipe)
    create MemberKind.Annotation "Annotations" (iconForKind MemberKind.Annotation)
    create MemberKind.DataContext "DataContexts" (iconForKind MemberKind.DataContext)
    create MemberKind.Agent "Agents" (iconForKind MemberKind.Agent)
    create MemberKind.Organization "Organizations" (iconForKind MemberKind.Organization)
    create MemberKind.ScholarlyArticle "ScholarlyArticles" (iconForKind MemberKind.ScholarlyArticle)
|]

let find kind =
    Items |> Array.find (fun item -> item.data = kind)

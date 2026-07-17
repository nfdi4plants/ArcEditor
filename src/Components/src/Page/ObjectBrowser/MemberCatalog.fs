module Swate.Components.Page.ObjectBrowser.MemberCatalog

open Swate.Components.Composite.InteractiveList.Types
open Swate.Components.Page.ObjectBrowser.Types

let private create data label icon : InteractiveListData<MemberKind> = {
    icon = icon
    label = label
    data = data
}

let Items: InteractiveListData<MemberKind>[] = [|
    create MemberKind.Dataset "Datasets" "swt:iconify-color swt:fluent-color--database-20"
    create MemberKind.Process "Processes" "swt:iconify-color swt:fluent-color--arrow-clockwise-dashes-settings-20"
    create MemberKind.Sample "Samples" "swt:iconify-color swt:fluent-color--molecule-20"
    create MemberKind.Data "Data" "swt:iconify-color swt:fluent-color--data-line-20"
    create MemberKind.Recipe "Recipes" "swt:iconify-color swt:fluent-color--clipboard-text-edit-20"
    create MemberKind.Annotation "Annotations" "swt:iconify-color swt:fluent-color--comment-multiple-20"
    create MemberKind.DataContext "DataContexts" "swt:iconify-color swt:fluent-color--content-view-20"
    create MemberKind.Agent "Agents" "swt:iconify-color swt:fluent-color--agents-20"
    create MemberKind.Organization "Organizations" "swt:iconify-color swt:fluent-color--org-20"
    create MemberKind.ScholarlyArticle "ScholarlyArticles" "swt:iconify-color swt:fluent-color--document-text-20"
|]

let find kind =
    Items |> Array.find (fun item -> item.data = kind)

module Swate.Components.Page.ObjectBrowser.MemberCatalog

open Swate.Components.Composite.InteractiveList.Types
open Swate.Components.ProcessCore
open Swate.Components.Page.ObjectBrowser.Types

let private create data label icon : InteractiveListData<MemberKind> = {
    icon = icon
    label = label
    data = data
}

let iconForKind kind = Icons.forMemberKindName (string kind)

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

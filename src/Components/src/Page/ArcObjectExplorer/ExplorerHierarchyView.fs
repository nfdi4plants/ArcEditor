namespace Swate.Components.Page.ArcObjectExplorer

open Fable.Core
open Feliz
open ProcessCore
open Swate.Components
open Swate.Components.Page.ArcObjectExplorer.Types
open Swate.Components.Page.ObjectBrowser
open Swate.Components.Page.ObjectBrowser.Types
open Swate.Components.Primitive.Tree
open Swate.Components.Primitive.Tree.Types

[<Erase; Mangle(false)>]
type ExplorerHierarchyView =

    [<ReactComponent(true)>]
    static member ExplorerHierarchyView
        (
            arcStateCtx: StateUpdaterContext<ARC option>,
            arcView: Swate.Components.ProcessCore.Types.ArcView,
            onSelect: ExplorerTreeTarget -> unit,
            onSelectCollection: MemberKind -> ProcessCoreEntity array -> unit,
            ?onOpenInMetadataEditor: ProcessCoreEntity -> unit
        ) =
        let containerRef = React.useElementRef ()
        let expandedKeys, setExpandedKeys = React.useState Set.empty

        let nodes =
            React.useMemo (
                (fun () ->
                    arcStateCtx.state
                    |> Option.map (ExplorerTree.createNodes arcView)
                    |> Option.defaultValue [||]
                ),
                [| box arcStateCtx.state; box arcView |]
            )

        let rec collectTargets (node: TreeNode<ExplorerTreeTarget>) = seq {
            match node.data with
            | Some target -> yield node.key, target
            | None -> ()

            for child in node.children do
                yield! collectTargets child
        }

        let targetsByIndex = nodes |> Seq.collect collectTargets |> Seq.toArray

        let indicesByKey =
            targetsByIndex |> Seq.mapi (fun index (key, _) -> key, index) |> Map.ofSeq

        let scopedEntities =
            deepestExpandedNodes expandedKeys nodes
            |> Option.map (fun deepestNodes ->
                deepestNodes
                |> Array.collect (fun node ->
                    node.children
                    |> Array.choose _.data
                    |> Array.collect (fun target ->
                        target.Levels
                        |> List.tryLast
                        |> Option.map _.Members
                        |> Option.defaultValue [||]
                    )
                )
                |> ObjectViewModel.distinctEntities
            )

        Html.div [
            prop.children [
                Html.div [
                    prop.ref containerRef
                    prop.children [
                        Tree.Tree(
                            nodes,
                            onSelect,
                            contextMenuIndex = (fun key -> indicesByKey |> Map.tryFind key),
                            onExpandedKeysChange = setExpandedKeys,
                            className = "swt:mt-1",
                            testId = "explorer-collection-tree"
                        )
                        ContextMenu.ContextMenu(
                            containerRef,
                            arcStateCtx,
                            arcView,
                            None,
                            ignore,
                            tryGetContextMenuEntity =
                                (fun index ->
                                    targetsByIndex
                                    |> Array.tryItem index
                                    |> Option.bind (fun (_, target) ->
                                        if target.Levels.IsEmpty then Some target.Dataset else None
                                    )
                                ),
                            tryGetContextMenuMemberKinds =
                                (fun index ->
                                    targetsByIndex
                                    |> Array.tryItem index
                                    |> Option.bind (fun (_, target) ->
                                        target.Levels |> List.tryLast |> Option.map _.AllowedMemberKinds
                                    )
                                ),
                            allowDeleteMembers = false,
                            ?onOpenInMetadataEditor = onOpenInMetadataEditor
                        )
                    ]
                ]
                Html.hr [ prop.className "swt:my-2 swt:border-base-300" ]
                HierarchyView.ObjectCollections(
                    arcStateCtx,
                    arcView,
                    (fun kind ->
                        arcStateCtx.state
                        |> Option.map (fun arc -> ObjectViewModel.getEntities arcView arc kind)
                        |> Option.defaultValue [||]
                        |> onSelectCollection kind
                    ),
                    ?scopedEntities = scopedEntities,
                    onSelectScoped = onSelectCollection
                )
            ]
        ]

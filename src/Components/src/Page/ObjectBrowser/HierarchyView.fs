namespace Swate.Components.Page.ObjectBrowser

open Fable.Core
open Feliz
open Swate.Components.Composite.InteractiveList
open Swate.Components.Composite.InteractiveList.Types
open Swate.Components.Primitive.Tree
open Swate.Components.Primitive.Tree.Types
open Swate.Components.Page.ObjectBrowser.Types
open Swate.Components.Primitive
open Swate.Components.Primitive.Buttons

[<Erase; Mangle(false)>]
type HierarchyView =

    [<ReactComponent>]
    static member private InteractiveListRow
        (
            entry: InteractiveListData<MemberKind>,
            rowIndex: int,
            request: ContextMenuRequest -> unit,
            onClick,
            allowActions: bool,
            ?isSelected: bool
        ) =
        let memberLabel = (MemberCatalog.find entry.data).label

        InteractiveList.Row(
            React.Fragment [
                InteractiveList.IconCell(entry.icon)
                InteractiveList.LabelCell(entry.label)
                Html.td [
                    prop.className "swt:w-max swt:whitespace-nowrap swt:py-1 swt:text-right"
                    prop.children [
                        if allowActions then
                            Buttons.IconButton(
                                $"Add {memberLabel}",
                                "swt:fluent--document-add-24-regular",
                                (fun event ->
                                    event.stopPropagation ()
                                    request (ContextMenuRequest.AddMember entry.data)
                                ),
                                size = DaisyuiSize.XS,
                                iconClassName = "swt:size-4"
                            )

                            Buttons.IconButton(
                                $"Delete {memberLabel}",
                                "swt:fluent--delete-20-filled",
                                (fun event ->
                                    event.stopPropagation ()
                                    request (ContextMenuRequest.DeleteMembers entry.data)
                                ),
                                size = DaisyuiSize.XS,
                                className = "swt:text-error",
                                iconClassName = "swt:size-4"
                            )
                    ]
                ]
            ],
            onClick = onClick,
            props = [
                prop.custom (Attributes.RowIndex, rowIndex)
                match isSelected with
                | Some true ->
                    prop.className "swt:bg-base-300"
                    prop.ariaSelected true
                | _ -> ()
            ]
        )

    /// The object-category list shared by the Editor and Explorer sidebars.
    [<ReactComponent>]
    static member ObjectCollections
        (
            arcStateCtx: Swate.Components.StateUpdaterContext<ProcessCore.ARC option>,
            arcView: Swate.Components.ProcessCore.Types.ArcView,
            onSelect: MemberKind -> unit,
            ?selectedDataset: ProcessCoreEntity,
            ?selectedKind: MemberKind,
            ?scopedEntities: ProcessCoreEntity array,
            ?onSelectScoped: MemberKind -> ProcessCoreEntity array -> unit
        ) =
        let containerRef = React.useElementRef ()
        let actionRequest, setActionRequest = React.useState<ContextMenuRequest option> None

        let counts =
            match scopedEntities, selectedDataset, arcStateCtx.state with
            | Some entities, _, _ -> entities |> Array.countBy _.memberKind |> Map.ofArray
            | None, Some dataset, _ -> MemberTree.directMemberCounts arcView dataset
            | None, None, Some arc ->
                MemberCatalog.Items
                |> Array.map (fun entry ->
                    entry.data, ObjectViewModel.getEntitiesWithView arcView arc entry.data |> Array.length
                )
                |> Map.ofArray
            | None, None, None -> Map.empty

        let entries =
            MemberCatalog.Items
            |> Array.map (fun entry ->
                let count = counts |> Map.tryFind entry.data |> Option.defaultValue 0

                {
                    entry with
                        label = $"{entry.label} ({count})"
                }
            )
            |> Array.filter (fun entry ->
                scopedEntities.IsNone
                || (counts |> Map.tryFind entry.data |> Option.defaultValue 0) > 0
            )

        let selectKind kind =
            match scopedEntities, onSelectScoped with
            | Some entities, Some select ->
                entities |> Array.filter (fun entity -> entity.memberKind = kind) |> select kind
            | _ -> onSelect kind

        Html.div [
            prop.ref containerRef
            prop.children [
                InteractiveList.InteractiveList(
                    entries,
                    (fun entry -> selectKind entry.data),
                    rowRender =
                        (fun entry ->
                            let rowIndex =
                                MemberCatalog.Items
                                |> Array.findIndex (fun catalogEntry -> catalogEntry.data = entry.data)

                            HierarchyView.InteractiveListRow(
                                entry,
                                rowIndex,
                                (fun action -> setActionRequest (Some action)),
                                (fun () -> selectKind entry.data),
                                scopedEntities.IsNone,
                                ?isSelected = (selectedKind |> Option.map ((=) entry.data))
                            )
                        ),
                    styles = InteractiveListStyles(tableClassName = "swt:table-sm")
                )
                if scopedEntities.IsNone then
                    ContextMenu.ContextMenu(
                        containerRef,
                        arcStateCtx,
                        arcView,
                        None,
                        onSelect,
                        ?actionRequest = actionRequest,
                        onActionRequestClosed = (fun () -> setActionRequest None)
                    )
            ]
        ]

    [<ReactComponent(true)>]
    static member HierarchyView
        (
            arcStateCtx: Swate.Components.StateUpdaterContext<ProcessCore.ARC option>,
            arcView: Swate.Components.ProcessCore.Types.ArcView,
            onSelect: MemberKind -> unit,
            ?onSelectEntity: ProcessCoreEntity -> unit,
            ?selectedKind: MemberKind,
            ?onSelectScoped: MemberKind -> ProcessCoreEntity array -> unit
        ) =
        let expandedKeys, setExpandedKeys = React.useState Set.empty

        let treeNodes =
            React.useMemo (
                (fun () ->
                    arcStateCtx.state
                    |> Option.map (MemberTree.createDatasetNodes arcView)
                    |> Option.defaultValue [||]
                ),
                [| box arcStateCtx.state; box arcView |]
            )

        let scopedEntities =
            deepestExpandedNodes expandedKeys treeNodes
            |> Option.map (fun deepestNodes ->
                deepestNodes
                |> Array.collect (fun node ->
                    node.children
                    |> Array.collect (fun child ->
                        match child.data with
                        | Some entity -> [| entity |]
                        | None -> child.children |> Array.choose _.data
                    )
                )
                |> Array.distinctBy (fun entity -> entity.memberKind, entity.key)
            )

        Html.div [
            prop.children [
                Tree.Tree(
                    treeNodes,
                    (fun entity ->
                        onSelectEntity
                        |> Option.defaultValue (fun entity -> onSelect entity.memberKind)
                        |> fun select -> select entity
                    ),
                    onExpandedKeysChange = setExpandedKeys,
                    className = "swt:mt-1",
                    testId = "dataset-tree"
                )
                Html.hr [ prop.className "swt:my-2 swt:border-base-300" ]
                HierarchyView.ObjectCollections(
                    arcStateCtx,
                    arcView,
                    onSelect,
                    ?selectedKind = selectedKind,
                    ?scopedEntities = scopedEntities,
                    ?onSelectScoped = onSelectScoped
                )
            ]
        ]

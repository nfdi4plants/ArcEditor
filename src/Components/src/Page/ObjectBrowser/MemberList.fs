namespace Swate.Components.Page.ObjectBrowser

open Fable.Core
open Feliz
open Swate.Components.Composite.InteractiveList
open Swate.Components.Composite.InteractiveList.Types
open Swate.Components.Primitive.Tree
open Swate.Components.Page.ObjectBrowser.Types
open Swate.Components.Primitive
open Swate.Components.Primitive.Buttons

[<Erase; Mangle(false)>]
type MemberList =

    [<ReactComponent>]
    static member private InteractiveListRow
        (
            entry: InteractiveListData<MemberKind>,
            rowIndex: int,
            request: ContextMenuRequest -> unit,
            onClick,
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

    [<ReactComponent(true)>]
    static member Main
        (
            arcStateCtx: Swate.Components.StateUpdaterContext<ProcessCore.ARC option>,
            arcView: Swate.Components.ProcessCore.RendererModel.ArcView,
            onSelect: MemberKind -> unit,
            ?onSelectEntity: ProcessCoreEntity -> unit,
            ?selectedKind: MemberKind
        ) =
        let containerRef = React.useElementRef ()
        let actionRequest, setActionRequest = React.useState<ContextMenuRequest option> None

        let selectedDataset, setSelectedDataset =
            React.useState<ProcessCoreEntity option> None

        let request action = action |> Some |> setActionRequest

        let calculateCounts selectedDataset =
            match selectedDataset, arcStateCtx.state with
            | Some dataset, _ -> MemberTree.directMemberCounts arcView dataset
            | None, Some arc ->
                MemberCatalog.Items
                |> Array.map (fun entry ->
                    entry.data, ObjectViewModel.getEntities arcView arc entry.data |> Array.length
                )
                |> Map.ofArray
            | None, None -> Map.empty

        let treeNodes =
            React.useMemo (
                (fun () ->
                    arcStateCtx.state
                    |> Option.map (MemberTree.datasetNodes arcView)
                    |> Option.defaultValue [||]
                ),
                [| box arcStateCtx.state; box arcView |]
            )

        let activateEntity entity nextExpansion =
            match entity.memberKind, nextExpansion with
            | MemberKind.Dataset, Some false -> setSelectedDataset None
            | MemberKind.Dataset, _ -> setSelectedDataset (Some entity)
            | _ -> ()

            if nextExpansion <> Some false then
                onSelectEntity
                |> Option.defaultValue (fun entity -> onSelect entity.memberKind)
                |> fun select -> select entity

        let activateEntityRef = React.useRef activateEntity
        activateEntityRef.current <- activateEntity

        let stableActivateEntity =
            React.useCallback (
                (fun entity nextExpansion -> activateEntityRef.current entity nextExpansion),
                [||]
            )

        let entries =
            let counts = calculateCounts selectedDataset

            MemberCatalog.Items
            |> Array.map (fun entry ->
                let count =
                    counts |> Map.tryFind entry.data |> Option.defaultValue 0

                {
                    entry with
                        label = $"{entry.label} ({count})"
                }
            )

        Html.div [
            prop.ref containerRef
            prop.children [
                Tree.Main(
                    treeNodes,
                    stableActivateEntity,
                    className = "swt:mt-1",
                    testId = "dataset-tree"
                )
                Html.hr [ prop.className "swt:my-2 swt:border-base-300" ]
                InteractiveList.InteractiveList(
                    entries,
                    (fun entry -> onSelect entry.data),
                    rowRender =
                        (fun entry ->
                            let rowIndex =
                                MemberCatalog.Items
                                |> Array.findIndex (fun catalogEntry -> catalogEntry.data = entry.data)

                            MemberList.InteractiveListRow(
                                entry,
                                rowIndex,
                                request,
                                (fun () -> onSelect entry.data),
                                ?isSelected = (selectedKind |> Option.map ((=) entry.data))
                            )
                        ),
                    styles = InteractiveListStyles(tableClassName = "swt:table-sm")
                )
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

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
            onSelect: MemberKind -> unit,
            ?onSelectEntity: ProcessCoreEntity -> unit,
            ?selectedKind: MemberKind
        ) =
        let containerRef = React.useElementRef ()
        let actionRequest, setActionRequest = React.useState<ContextMenuRequest option> None

        let expandedEntities, setExpandedEntities =
            React.useState<ProcessCoreEntity array> [||]

        let request action = action |> Some |> setActionRequest

        let selectEntity entity =
            onSelectEntity
            |> Option.defaultValue (fun entity -> onSelect entity.memberKind)
            |> fun select -> select entity

        let datasets =
            arcStateCtx.state
            |> Option.map (fun arc -> ObjectViewModel.getEntities arc MemberKind.Dataset)
            |> Option.defaultValue [||]

        // With no expanded entity the flat list shows ARC-wide totals. Once entities
        // are expanded, it shows reference-unique direct members at the deepest level.
        let scopedCounts =
            if Array.isEmpty expandedEntities then
                None
            else
                expandedEntities
                |> Array.collect MemberTree.directMembers
                |> Array.distinctBy (fun entity -> entity.memberKind, entity.key)
                |> Array.countBy _.memberKind
                |> Map.ofArray
                |> Some

        let entries =
            MemberCatalog.Items
            |> Array.map (fun entry ->
                let count =
                    match scopedCounts with
                    | Some counts -> counts |> Map.tryFind entry.data |> Option.defaultValue 0
                    | None ->
                        arcStateCtx.state
                        |> Option.map (fun arc -> ObjectViewModel.getEntities arc entry.data |> Array.length)
                        |> Option.defaultValue 0

                {
                    entry with
                        label = $"{entry.label} ({count})"
                }
            )

        Html.div [
            prop.ref containerRef
            prop.children [
                Tree.Main(
                    MemberTree.datasetNodes datasets,
                    selectEntity,
                    className = "swt:mt-1",
                    testId = "dataset-tree",
                    onExpandedDataChange = setExpandedEntities
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
                    None,
                    onSelect,
                    ?actionRequest = actionRequest,
                    onActionRequestClosed = (fun () -> setActionRequest None)
                )
            ]
        ]

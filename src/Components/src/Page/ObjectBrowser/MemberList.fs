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

    [<ReactComponent>]
    static member private RootFolder
        (
            entry: InteractiveListData<MemberKind>,
            itemCount: int,
            isExpanded: bool,
            setIsExpanded: bool -> unit,
            request: ContextMenuRequest -> unit,
            onSelect: MemberKind -> unit,
            children: ReactElement,
            ?testId: string,
            ?isSelected: bool
        ) =
        let toggleFolder () =
            onSelect entry.data
            setIsExpanded (not isExpanded)

        Html.div [
            match testId with
            | Some testId -> prop.testId testId
            | None -> ()
            prop.children [
                Html.table [
                    prop.className "swt:table swt:table-sm"
                    prop.children [
                        Html.tbody [
                            prop.children [
                                InteractiveList.Row(
                                    React.Fragment [
                                        Html.td [
                                            prop.className "swt:w-px"
                                            prop.children [
                                                Html.i [
                                                    prop.className [
                                                        "swt:iconify swt:size-4 swt:shrink-0"
                                                        if isExpanded then
                                                            "swt:fluent--chevron-down-20-filled"
                                                        else
                                                            "swt:fluent--chevron-right-20-filled"
                                                    ]
                                                ]
                                            ]
                                        ]
                                        Html.td [
                                            prop.className "swt:px-4 swt:py-2"
                                            prop.children [
                                                Html.div [
                                                    prop.className "swt:flex swt:items-center swt:gap-2"
                                                    prop.children [
                                                        Html.i [
                                                            prop.className [ entry.icon; "swt:size-6 swt:shrink-0" ]
                                                        ]
                                                        Html.span $"{entry.label} ({itemCount})"
                                                    ]
                                                ]
                                            ]
                                        ]
                                        Html.td [
                                            prop.className "swt:w-max swt:whitespace-nowrap swt:py-1 swt:text-right"
                                            prop.children [
                                                Buttons.IconButton(
                                                    $"Add {entry.label}",
                                                    "swt:fluent--document-add-24-regular",
                                                    (fun event ->
                                                        event.stopPropagation ()
                                                        request (ContextMenuRequest.AddMember entry.data)
                                                    ),
                                                    size = DaisyuiSize.XS,
                                                    iconClassName = "swt:size-4"
                                                )
                                                Buttons.IconButton(
                                                    $"Delete {entry.label}",
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
                                    onClick = toggleFolder,
                                    props = [
                                        prop.custom (Attributes.RowIndex, 0)
                                        prop.ariaExpanded isExpanded
                                        match isSelected with
                                        | Some true ->
                                            prop.className "swt:bg-base-300"
                                            prop.ariaSelected true
                                        | _ -> ()
                                    ]
                                )
                            ]
                        ]
                    ]
                ]
                if isExpanded then
                    children
            ]
        ]

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
        let datasetsExpanded, setDatasetsExpanded = React.useState true

        let request action = action |> Some |> setActionRequest

        let selectEntity entity =
            onSelectEntity
            |> Option.defaultValue (fun entity -> onSelect entity.memberKind)
            |> fun select -> select entity

        let datasets =
            arcStateCtx.state
            |> Option.map (fun arc -> ObjectViewModel.getEntities arc MemberKind.Dataset)
            |> Option.defaultValue [||]

        let entries =
            MemberCatalog.Items
            |> Array.map (fun entry ->
                let count =
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
                MemberList.RootFolder(
                    MemberCatalog.find MemberKind.Dataset,
                    datasets.Length,
                    datasetsExpanded,
                    setDatasetsExpanded,
                    request,
                    onSelect,
                    Tree.Main(
                        MemberTree.datasetNodes datasets,
                        selectEntity,
                        className = "swt:ml-6 swt:mt-1",
                        testId = "dataset-folder-children"
                    ),
                    testId = "dataset-folder",
                    ?isSelected = (selectedKind |> Option.map ((=) MemberKind.Dataset))
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

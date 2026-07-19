namespace Swate.Components.Page.ObjectBrowser

open Fable.Core
open Feliz
open Swate.Components.Composite.InteractiveList
open Swate.Components.Composite.InteractiveList.Types
open Swate.Components.Page.ObjectBrowser.Types

[<Erase; Mangle(false)>]
type MemberList =

    [<ReactComponent>]
    static member private ActionButton(label: string, icon: string, onClick: unit -> unit, ?className: string) =
        Html.button [
            prop.type'.button
            prop.className [
                "swt:btn swt:btn-ghost swt:btn-xs swt:btn-square"
                className |> Option.defaultValue ""
            ]
            prop.ariaLabel label
            prop.title label
            prop.onClick (fun event ->
                event.stopPropagation ()
                onClick ()
            )
            prop.children [
                Html.i [ prop.className [ "swt:iconify swt:size-4"; icon ] ]
            ]
        ]

    [<ReactComponent>]
    static member private InteractiveListRow
        (entry: InteractiveListData<MemberKind>, request: ContextMenuRequest -> unit, onClick, ?isSelected: bool)
        =
        let memberLabel = (MemberCatalog.find entry.data).label

        InteractiveList.Row(
            React.Fragment [
                InteractiveList.IconCell(entry.icon)
                InteractiveList.LabelCell(entry.label)
                Html.td [
                    prop.className "swt:w-max swt:whitespace-nowrap swt:py-1 swt:text-right"
                    prop.children [
                        MemberList.ActionButton(
                            $"Add {memberLabel}",
                            "swt:fluent--document-add-24-regular",
                            (fun () -> request (ContextMenuRequest.AddMember entry.data))
                        )
                        MemberList.ActionButton(
                            $"Delete {memberLabel}",
                            "swt:fluent--delete-20-filled",
                            (fun () -> request (ContextMenuRequest.DeleteMembers entry.data)),
                            className = "swt:text-error"
                        )
                    ]
                ]
            ],
            onClick = onClick,
            props = [
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
            ?selectedKind: MemberKind
        ) =
        let containerRef = React.useElementRef ()
        let actionRequest, setActionRequest = React.useState<ContextMenuRequest option> None

        let request action = action |> Some |> setActionRequest

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
                InteractiveList.InteractiveList(
                    entries,
                    (fun entry -> onSelect entry.data),
                    rowRender =
                        (fun entry ->
                            MemberList.InteractiveListRow(
                                entry,
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

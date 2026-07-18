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
    static member private RowActions(entry: InteractiveListData<MemberKind>, request: ContextMenuRequest -> unit) =
        React.Fragment [
            MemberList.ActionButton(
                $"Add {entry.label}",
                "swt:fluent--document-add-24-regular",
                (fun () -> request (ContextMenuRequest.AddMember entry.data))
            )
            MemberList.ActionButton(
                $"Delete {entry.label}",
                "swt:fluent--delete-20-filled",
                (fun () -> request (ContextMenuRequest.DeleteMembers entry.data)),
                className = "swt:text-error"
            )
        ]

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

        Html.div [
            prop.ref containerRef
            prop.children [
                InteractiveList.InteractiveList(
                    MemberCatalog.Items,
                    (fun entry -> onSelect entry.data),
                    rowEndRender = (fun entry -> MemberList.RowActions(entry, request)),
                    isSelected = (fun entry -> selectedKind = Some entry.data),
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

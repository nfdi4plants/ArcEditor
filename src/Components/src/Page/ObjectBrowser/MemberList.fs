namespace Swate.Components.Page.ObjectBrowser

open Fable.Core
open Feliz
open Swate.Components.Composite.InteractiveList
open Swate.Components.Page.ObjectBrowser.Types

[<Erase; Mangle(false)>]
type MemberList =

    [<ReactComponent(true)>]
    static member Main
        (
            arcStateCtx: Swate.Components.StateUpdaterContext<ProcessCore.ARC option>,
            onSelect: MemberKind -> unit,
            ?selectedKind: MemberKind
        ) =
        let containerRef = React.useElementRef ()

        Html.div [
            prop.ref containerRef
            prop.children [
                InteractiveList.DefaultRow(
                    MemberCatalog.Items,
                    (fun entry -> onSelect entry.data),
                    isSelected = (fun entry -> selectedKind = Some entry.data),
                    tableClassName = "swt:table-sm"
                )
                ContextMenu.ContextMenu(containerRef, arcStateCtx, None, onSelect)
            ]
        ]

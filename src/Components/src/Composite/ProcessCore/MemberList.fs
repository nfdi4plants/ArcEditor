namespace Swate.Components.Composite.ProcessCore

open Fable.Core
open Feliz
open Swate.Components.Composite.InteractiveList
open Swate.Components.Composite.InteractiveList.Types

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

        let renderMemberRow (entry: InteractiveListData<MemberKind>) =
            let isSelected = selectedKind = Some entry.data

            let memberKindIndex =
                MemberCatalog.Items |> Array.findIndex (fun item -> item.data = entry.data)

            Html.tr [
                prop.custom ("data-process-core-kind", memberKindIndex)
                prop.className [
                    "swt:cursor-pointer swt:align-middle swt:hover:bg-base-300"

                    if isSelected then
                        "swt:bg-base-300"
                ]
                prop.ariaSelected isSelected
                prop.onClick (fun _ -> onSelect entry.data)
                prop.children [
                    Html.td [
                        prop.className "swt:w-px"
                        prop.children [
                            Html.div [
                                prop.className "swt:flex swt:items-center"
                                prop.children [ Html.i [ prop.className [ entry.icon; "swt:size-6" ] ] ]
                            ]
                        ]
                    ]
                    Html.td [
                        prop.className "swt:px-4 swt:py-2"
                        prop.text entry.label
                    ]
                ]
            ]

        Html.div [
            prop.ref containerRef
            prop.children [
                InteractiveList.InteractiveList(
                    MemberCatalog.Items,
                    (fun entry -> onSelect entry.data),
                    rowRender = renderMemberRow,
                    styles = InteractiveListStyles(tableClassName = "swt:table-sm")
                )
                ContextMenu.ContextMenu(containerRef, arcStateCtx, onSelect)
            ]
        ]

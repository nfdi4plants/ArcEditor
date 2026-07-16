namespace Swate.Components.Composite.InteractiveList

open Fable.Core
open Feliz
open Types

module Attributes =

    [<Literal>]
    let RowIndex = "data-interactive-list-index"

[<Erase; Mangle(false)>]
type InteractiveList =

    [<ReactComponent(true)>]
    static member DefaultRow<'A>
        (
            data: InteractiveListData<'A>[],
            onClick: InteractiveListData<'A> -> unit,
            ?isSelected: InteractiveListData<'A> -> bool,
            ?tableClassName: string
        ) =

        let isSelected = defaultArg isSelected (fun _ -> false)
        let tableClassName = defaultArg tableClassName ""

        let renderRow rowIndex entry =
            let isSelected = isSelected entry

            Html.tr [
                prop.custom (Attributes.RowIndex, rowIndex)
                prop.className [
                    "swt:cursor-pointer swt:align-middle swt:hover:bg-base-300 swt:focus:bg-base-300 swt:focus:outline-none"

                    if isSelected then
                        "swt:bg-base-300"
                ]
                prop.ariaSelected isSelected
                prop.tabIndex 0
                prop.onClick (fun _ -> onClick entry)
                prop.onKeyDown (fun event ->
                    if event.key = "Enter" || event.key = " " then
                        event.preventDefault ()
                        onClick entry
                )
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
            prop.className "swt:overflow-x-auto"
            prop.children [
                Html.table [
                    prop.className [ "swt:table"; tableClassName ]
                    prop.children [
                        Html.tbody [
                            prop.children [
                                for rowIndex, entry in Array.indexed data do
                                    renderRow rowIndex entry
                            ]
                        ]
                    ]
                ]
            ]
        ]

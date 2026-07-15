namespace Swate.Components.Composite.InteractiveList

open Fable.Core
open Feliz
open Types

module Attributes =

    [<Literal>]
    let RowIndex = "data-interactive-list-index"

[<Erase; Mangle(false)>]
type InteractiveList =

    [<ReactComponent>]
    static member private DefaultRow
        (entry: InteractiveListData<'A>, rowIndex: int, onClick: InteractiveListData<'A> -> unit, isSelected: bool)
        =
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

    [<ReactComponent(true)>]
    static member InteractiveList<'A>
        (
            data: InteractiveListData<'A>[],
            onClick: InteractiveListData<'A> -> unit,
            ?rowRender: InteractiveListData<'A> -> ReactElement,
            ?headerRender: unit -> ReactElement,
            ?sortFn: InteractiveListData<'A>[] -> InteractiveListData<'A>[],
            ?isSelected: InteractiveListData<'A> -> bool,
            ?styles: InteractiveListStyles
        ) =

        let dataSorted =
            sortFn |> Option.map (fun sort -> sort data) |> Option.defaultValue data

        let tableClassName =
            styles
            |> Option.bind (fun styles -> styles.tableClassName)
            |> Option.defaultValue ""

        let renderRow rowIndex entry =
            match rowRender with
            | Some render -> render entry
            | None ->
                let selected = isSelected |> Option.exists (fun predicate -> predicate entry)
                InteractiveList.DefaultRow(entry, rowIndex, onClick, selected)

        Html.div [
            prop.className "swt:overflow-x-auto"
            prop.children [
                Html.table [
                    prop.className [ "swt:table"; tableClassName ]
                    prop.children [
                        Html.tbody [
                            prop.children [
                                for rowIndex, entry in Array.indexed dataSorted do
                                    renderRow rowIndex entry
                            ]
                        ]
                    ]
                ]
            ]
        ]

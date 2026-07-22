namespace Swate.Components.Composite.InteractiveList

open Swate.Components
open Fable.Core
open Feliz
open Types

module Attributes =

    [<Literal>]
    let RowIndex = "data-interactive-list-index"

[<Erase; Mangle(false)>]
type InteractiveList =

    [<ReactComponent>]
    static member IconCell(icon: string) =
        Html.td [
            prop.className "swt:w-px"
            prop.children [
                Html.div [
                    prop.className "swt:flex swt:items-center"
                    prop.children [ Html.i [ prop.className [ icon; "swt:size-6" ] ] ]
                ]
            ]
        ]

    [<ReactComponent>]
    static member LabelCell(label: string) =
        Html.td [ prop.className "swt:px-4 swt:py-2"; prop.text label ]

    /// Renders a styled row for the interactive list.
    ///
    /// Careful usage of the ``props`` input, as it can override the following properties of the row
    /// - tabIndex
    /// - onClick
    /// - className
    /// - onKeyDown
    /// - children
    [<ReactComponent>]
    static member Row(children: ReactElement, ?onClick: unit -> unit, ?props: IReactProperty list, ?className: string) =
        Html.tr [
            prop.className [
                "swt:cursor-pointer swt:align-middle swt:hover:bg-base-300 swt:focus:bg-base-300 swt:focus:outline-none"
                if className.IsSome then
                    className.Value
            ]
            prop.tabIndex 0
            if onClick.IsSome then
                prop.onClick (fun _ -> onClick.Value())
            prop.onKeyDown (fun event ->
                if event.key = kbdEventCode.enter || event.key = " " then
                    event.preventDefault ()

                    if onClick.IsSome then
                        onClick.Value()
            )
            prop.children children
            if props.IsSome then
                yield! props.Value
        ]

    [<ReactComponent>]
    static member private DefaultRow
        (entry: InteractiveListData<'A>, rowIndex: int, onClick: InteractiveListData<'A> -> unit, isSelected: bool)
        =
        InteractiveList.Row(
            React.Fragment [
                InteractiveList.IconCell(entry.icon)
                InteractiveList.LabelCell(entry.label)
            ],
            onClick = (fun () -> onClick entry),
            className = (if isSelected then "swt:bg-base-300" else null),
            props = [
                prop.custom (Attributes.RowIndex, rowIndex)
                prop.ariaSelected isSelected
            ]
        )

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

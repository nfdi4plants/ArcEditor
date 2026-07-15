namespace Swate.Components.Composite.InteractiveList

open Fable.Core
open Feliz
open Types

[<Erase; Mangle(false)>]
type InteractiveList =

    [<ReactComponent>]
    static member private DefaultRow(entry: InteractiveListData<'A>, onClick: InteractiveListData<'A> -> unit) =
        Html.tr [
            prop.className "swt:cursor-pointer swt:hover:bg-base-300 swt:align-middle"
            prop.onClick (fun _ -> onClick entry)
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
            ?styles: InteractiveListStyles
        ) =

        let dataSorted =
            match sortFn with
            | Some fn -> fn data
            | None -> data

        Html.div [
            prop.className "swt:overflow-x-auto"
            prop.children [
                Html.table [
                    prop.className [
                        "swt:table"
                        if styles.IsSome && styles.Value.tableClassName.IsSome then
                            styles.Value.tableClassName.Value
                        else
                            ""
                    ]
                    prop.children [
                        Html.tbody [
                            prop.children [
                                for data in dataSorted do
                                    match rowRender with
                                    | Some render -> render data
                                    | None -> InteractiveList.DefaultRow(data, onClick)
                            ]
                        ]
                    ]
                ]
            ]
        ]

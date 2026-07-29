namespace Swate.Components.Page.Metadata.FormComponents

open Browser.Types
open Fable.Core
open Feliz

open Swate.Components.Primitive.BaseModal
open Swate.Components.Primitive.Buttons
open Swate.Components.Primitive.LayoutComponents

[<Erase; Mangle(false)>]
type InputSequence =

    [<ReactComponent>]
    static member InputSequence<'T>
        (
            inputs: ResizeArray<'T>,
            constructor: unit -> 'T,
            setter: ResizeArray<'T> -> unit,
            inputComponent: 'T * ('T -> unit) * (MouseEvent -> unit) -> ReactElement,
            ?addItem: 'T -> unit,
            ?newItemError: 'T -> string option,
            ?removeItem: 'T -> unit,
            ?validator: ResizeArray<'T> -> Result<unit, string>,
            ?label: string,
            ?extendedElements: ReactElement,
            ?footerElements: ReactElement,
            ?createOptions: (string * (unit -> 'T)) array,
            ?stickyFooter: bool
        ) =
        let message, setMessage = React.useState (None: string option)
        let isCreateModalOpen, setCreateModalOpen = React.useState false
        let addItem = defaultArg addItem inputs.Add
        let newItemError = defaultArg newItemError (fun _ -> None)
        let removeItem = defaultArg removeItem (fun item -> inputs.Remove(item) |> ignore)
        let createOptions = defaultArg createOptions [||]
        let createLabel = label |> Option.defaultValue "item"
        let previousValidInputs = React.useRef inputs

        let validateSetter next =
            match validator with
            | Some validate ->
                match validate next with
                | Ok() ->
                    previousValidInputs.current <- next
                    setter next
                | Error message ->
                    setter previousValidInputs.current
                    setMessage (Some $"Validation Error: {message}")
            | None ->
                previousValidInputs.current <- next
                setter next

        let addNewItem create =
            let item = create ()

            match newItemError item with
            | Some error -> setMessage (Some error)
            | None ->
                addItem item
                validateSetter inputs

        Html.div [
            prop.className "swt:space-y-2"
            prop.children [
                BaseModal.Modal(
                    isOpen = message.IsSome,
                    setIsOpen =
                        (fun isOpen ->
                            if not isOpen then
                                setMessage None
                        ),
                    header = Html.text "Unable to update list",
                    children = Html.text (message |> Option.defaultValue ""),
                    debug = "metadata-input-error"
                )
                BaseModal.Modal(
                    isOpen = isCreateModalOpen,
                    setIsOpen = setCreateModalOpen,
                    header = Html.text $"Add {createLabel}",
                    children =
                        Html.div [
                            prop.className "swt:grid swt:grid-cols-2 swt:gap-3"
                            prop.children [
                                for optionLabel, create in createOptions do
                                    Html.button [
                                        prop.className "swt:btn swt:btn-outline"
                                        prop.text optionLabel
                                        prop.onClick (fun _ ->
                                            setCreateModalOpen false
                                            addNewItem create
                                        )
                                    ]
                            ]
                        ],
                    debug = "metadata-create-choice"
                )
                if label.IsSome then
                    LayoutComponents.FieldTitle label.Value
                if extendedElements.IsSome then
                    extendedElements.Value
                Html.div [
                    prop.className "swt:space-y-2"
                    prop.children [
                        for index in 0 .. (inputs.Count - 1) do
                            let item = inputs.[index]

                            Html.div [
                                prop.key index
                                prop.children (
                                    inputComponent (
                                        item,
                                        (fun updated ->
                                            inputs.[index] <- updated
                                            validateSetter inputs
                                        ),
                                        (fun _ ->
                                            removeItem item
                                            validateSetter inputs
                                        )
                                    )
                                )
                            ]
                    ]
                ]
                Html.div [
                    prop.className [
                        "swt:flex swt:justify-center swt:gap-2 swt:w-full swt:mt-2 swt:py-2"
                        if defaultArg stickyFooter false then
                            "swt:sticky swt:bottom-0 swt:z-10 swt:bg-base-200"
                    ]
                    prop.children [
                        Buttons.AddButton(fun _ ->
                            if Array.isEmpty createOptions then
                                addNewItem constructor
                            else
                                setCreateModalOpen true
                        )
                        footerElements |> Option.defaultValue Html.none
                    ]
                ]
            ]
        ]

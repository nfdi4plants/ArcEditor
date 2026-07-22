namespace Swate.Components.Page.Metadata.FormComponents

open Browser.Types
open Fable.Core
open Feliz

open Swate.Components.Primitive.BaseModal
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
            ?footerElements: ReactElement
        ) =
        let message, setMessage = React.useState (None: string option)
        let addItem = defaultArg addItem inputs.Add
        let newItemError = defaultArg newItemError (fun _ -> None)
        let removeItem = defaultArg removeItem (fun item -> inputs.Remove(item) |> ignore)
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
                    prop.className "swt:flex swt:justify-center swt:gap-2 swt:w-full swt:mt-2"
                    prop.children [
                        Helpers.AddButton(fun _ ->
                            let item = constructor ()

                            match newItemError item with
                            | Some error -> setMessage (Some error)
                            | None ->
                                addItem item
                                validateSetter inputs
                        )
                        footerElements |> Option.defaultValue Html.none
                    ]
                ]
            ]
        ]

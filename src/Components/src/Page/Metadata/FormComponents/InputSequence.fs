namespace Swate.Components.Page.Metadata.FormComponents

open System
open Browser.Types
open Fable.Core
open Fable.Core.JsInterop
open Feliz

open Swate.Components
open Swate.Components.Primitive
open Swate.Components.Primitive.BaseModal
open Swate.Components.Primitive.LayoutComponents
open Swate.Components.JsBindings

[<Erase; Mangle(false)>]
type InputSequence =

    static member private MoveItem(items: ResizeArray<'T>, oldIndex: int, newIndex: int) =
        let item = items.[oldIndex]
        items.RemoveAt oldIndex
        items.Insert(newIndex, item)

    [<ReactComponent>]
    static member private InputSequenceElement(key: string, id: string, listComponent: ReactElement) =
        let sortable = JsBindings.DndKit.useSortable ({| id = id |})

        let style = {|
            transform = DndKit.CSS.Transform.toString sortable.transform
            transition = sortable.transition
        |}

        Html.div [
            prop.ref sortable.setNodeRef
            prop.id id
            for attribute in Object.keys sortable.attributes do
                prop.custom (attribute, sortable.attributes.get attribute)
            prop.className "swt:flex swt:flex-row swt:gap-2"
            prop.custom ("style", style)
            prop.children [
                Html.span [
                    for listener in Object.keys sortable.listeners do
                        prop.custom (listener, sortable.listeners.get listener)
                    prop.className "swt:cursor-grab swt:flex swt:items-center"
                    prop.children [ Icons.ArrowUpDown() ]
                ]
                Html.div [ prop.className "swt:grow"; prop.children listComponent ]
            ]
        ]

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
            ?reorderItems: ResizeArray<'T> -> unit,
            ?validator: ResizeArray<'T> -> Result<unit, string>,
            ?label: string,
            ?extendedElements: ReactElement,
            ?footerElements: ReactElement
        ) =
        let sensors = DndKit.useSensors [| DndKit.useSensor DndKit.PointerSensor |]
        let message, setMessage = React.useState (None: string option)
        let addItem = defaultArg addItem inputs.Add
        let newItemError = defaultArg newItemError (fun _ -> None)

        let removeItem = defaultArg removeItem (fun item -> inputs.Remove(item) |> ignore)
        let reorderItems = defaultArg reorderItems setter

        let guids =
            React.useMemo (
                (fun () ->
                    ResizeArray [
                        for _ in inputs do
                            Guid.NewGuid()
                    ]
                ),
                [| box inputs.Count |]
            )

        let mkId index = guids.[index].ToString()

        let getIndexFromId (id: string) =
            guids.FindIndex(fun guid -> guid = Guid id)

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

        let handleDragEnd (event: DndKit.IDndKitEvent) =
            let active = event.active
            let over = event.over

            if isNull over |> not && active.id <> over.id then
                let oldIndex = getIndexFromId (string active.id)
                let newIndex = getIndexFromId (string over.id)

                if oldIndex >= 0 && newIndex >= 0 then
                    // Keep the sortable id attached to the item that owns it. If the
                    // ids remain in positional order, DnD Kit sees the active id at
                    // its old index after the ProcessCore mutation and animates the
                    // dragged row back to where it started.
                    InputSequence.MoveItem(guids, oldIndex, newIndex)

                    let reorderedInputs = ResizeArray inputs
                    InputSequence.MoveItem(reorderedInputs, oldIndex, newIndex)
                    reorderItems reorderedInputs

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
                DndKit.DndContext(
                    sensors = sensors,
                    onDragEnd = handleDragEnd,
                    collisionDetection = DndKit.closestCenter,
                    children =
                        DndKit.SortableContext(
                            items = guids,
                            strategy = DndKit.verticalListSortingStrategy,
                            children =
                                Html.div [
                                    prop.className "swt:space-y-2"
                                    prop.children [
                                        for index in 0 .. (inputs.Count - 1) do
                                            let item = inputs.[index]
                                            let id = mkId index

                                            InputSequence.InputSequenceElement(
                                                id,
                                                id,
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
                        )
                )
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

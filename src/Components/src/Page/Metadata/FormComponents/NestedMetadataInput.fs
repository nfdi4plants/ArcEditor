namespace Swate.Components.Page.Metadata.FormComponents

open Fable.Core
open Feliz
open ProcessCore
open Swate.Components.ProcessCore
open Swate.Components.Primitive.Buttons
open Swate.Components.Primitive.BaseModal
open Swate.Components.Primitive.Select.Types
open Swate.Components.Primitive.LayoutComponents
open Swate.Components.Page.Metadata.FormComponents.ImportCatalogContext

[<Erase; Mangle(false)>]
type NestedMetadataInput =

    [<ReactComponent>]
    static member private ImportButton(onClick: unit -> unit) =
        Html.button [
            prop.type'.button
            prop.className "swt:btn swt:btn-secondary"
            prop.text "Import"
            prop.onClick (fun _ -> onClick ())
        ]

    [<ReactComponent>]
    static member private ImportModal<'T>
        (
            isOpen: bool,
            setIsOpen: bool -> unit,
            candidates: 'T array,
            presentation: 'T -> string * string,
            allowMultiple: bool,
            onImport: 'T array -> JS.Promise<unit>
        ) =
        let selectedIndices, setSelectedIndices = React.useState<Set<int>> Set.empty
        let isImporting, setIsImporting = React.useState false

        let options: SelectItem<'T>[] =
            candidates
            |> Array.map (fun item -> {|
                item = item
                label = presentation item |> snd
            |})

        let close () =
            setSelectedIndices Set.empty
            setIsOpen false

        let importSelected () =
            if not isImporting && not selectedIndices.IsEmpty then
                let selected =
                    selectedIndices
                    |> Seq.choose (fun index -> Array.tryItem index candidates)
                    |> Array.ofSeq

                setIsImporting true

                promise {
                    try
                        do! onImport selected
                        close ()
                    finally
                        setIsImporting false
                }
                |> Promise.start

        let singleSelect =
            Html.select [
                prop.className "swt:select swt:w-full"
                prop.value (selectedIndices |> Seq.tryHead |> Option.map string |> Option.defaultValue "")
                prop.onChange (fun (value: string) ->
                    match System.Int32.TryParse value with
                    | true, index -> setSelectedIndices (Set.singleton index)
                    | false, _ -> setSelectedIndices Set.empty
                )
                prop.children [
                    Html.option [
                        prop.value ""
                        prop.disabled true
                        prop.text "Select an object"
                    ]

                    for index, option in Array.indexed options do
                        Html.option [ prop.value (string index); prop.text option.label ]
                ]
            ]

        let selectionControl =
            if Array.isEmpty candidates then
                Html.p "No other compatible objects are available in this ARC."
            elif allowMultiple then
                Swate.Components.Primitive.Select.Select.Select(options, selectedIndices, setSelectedIndices)
            else
                singleSelect

        let modalChildren =
            Html.fieldSet [
                prop.disabled isImporting
                prop.className [
                    if isImporting then
                        "swt:opacity-50"
                ]
                prop.children [ selectionControl ]
            ]

        BaseModal.Modal(
            isOpen = isOpen,
            setIsOpen =
                (fun open' ->
                    if not isImporting then
                        if open' then setIsOpen true else close ()
                ),
            header = Html.text "Import existing object",
            children = modalChildren,
            footer =
                React.Fragment [
                    Html.button [
                        prop.className "swt:btn"
                        prop.disabled isImporting
                        prop.text "Cancel"
                        prop.onClick (fun _ -> close ())
                    ]
                    Html.button [
                        prop.className "swt:btn swt:btn-primary swt:ml-auto"
                        prop.text (if isImporting then "Importing..." else "Import")
                        prop.disabled (selectedIndices.IsEmpty || isImporting)
                        prop.onClick (fun _ -> importSelected ())
                    ]
                ],
            debug = "process-core-import",
            canClose = not isImporting
        )

    /// Shared import boundary for every relationship row. It resolves candidates lazily
    /// from the catalog, applies relationship-specific safety rules, and owns modal state.
    /// Keeping this here avoids duplicating context and modal logic in metadata views.
    [<ReactComponent>]
    static member private ImportControl<'T>
        (
            presentation: 'T -> string * string,
            allowMultiple: bool,
            onImport: 'T array -> unit,
            ?imports: ImportCatalog -> 'T array,
            ?isImportable: 'T -> bool
        ) =
        let importContext = useImportCtx ()
        let isOpen, setIsOpen = React.useState false
        let isImportable = defaultArg isImportable (fun _ -> true)

        let runImport selected =
            match importContext |> Option.bind _.RunAsyncMutation with
            | Some runAsyncMutation -> runAsyncMutation (fun () -> onImport selected)
            | None -> promise { onImport selected }

        let candidates =
            Option.map2 (fun getCandidates context -> getCandidates context.Catalog) imports importContext
            |> Option.map (Array.filter isImportable)
            |> Option.defaultValue [||]

        match imports with
        | Some _ ->
            React.Fragment [
                NestedMetadataInput.ImportButton(fun () -> setIsOpen true)
                NestedMetadataInput.ImportModal(isOpen, setIsOpen, candidates, presentation, allowMultiple, runImport)
            ]
        | None -> Html.none

    [<ReactComponent>]
    // ProcessCore hotfix: mandatory nested fields omit the optional removal action.
    static member Row(icon: string, label: string, navigate: unit -> unit, ?remove: Browser.Types.MouseEvent -> unit) =
        Html.div [
            prop.className "swt:flex swt:w-full swt:items-center swt:gap-2"
            prop.children [
                Html.button [
                    prop.className "swt:btn swt:btn-ghost swt:h-auto swt:min-h-10 swt:flex-1 swt:justify-start swt:px-3"
                    prop.ariaLabel $"Open {label} metadata"
                    prop.onClick (fun _ -> navigate ())
                    prop.children [
                        Html.i [ prop.className [ icon; "swt:size-6" ] ]
                        Html.span label
                    ]
                ]
                if remove.IsSome then
                    Buttons.DeleteButton("Delete", remove.Value)
            ]
        ]

    [<ReactComponent>]
    static member RequiredRow<'T>
        (fieldLabel: string, value: 'T, presentation: 'T -> string * string, navigate: 'T -> unit)
        =
        let icon, label = presentation value

        Html.div [
            prop.className "swt:space-y-2"
            prop.children [
                LayoutComponents.FieldTitle fieldLabel
                NestedMetadataInput.Row(icon, label, (fun () -> navigate value))
            ]
        ]

    static member nonEmptyOr fallback (value: string) =
        if System.String.IsNullOrWhiteSpace value then
            fallback
        else
            value

    static member optionOr fallback value =
        value |> Option.defaultValue "" |> NestedMetadataInput.nonEmptyOr fallback

    static member Annotation(item: Annotation) =
        Icons.annotationIcon, NestedMetadataInput.nonEmptyOr "Unnamed annotation" item.Name

    static member DefinedTerm(item: DefinedTerm) =
        Icons.definedTermIcon, NestedMetadataInput.nonEmptyOr "Unnamed defined term" item.Name

    static member FormalParameter(item: FormalParameter) =
        Icons.formalParameterIcon, NestedMetadataInput.nonEmptyOr "Unnamed formal parameter" item.Name

    static member Data(item: Data) =
        Icons.dataIcon, NestedMetadataInput.nonEmptyOr "Unnamed data" item.Name

    static member agent(item: Agent) =
        let label =
            [
                item.GivenName
                item.FamilyName |> Option.defaultValue ""
            ]
            |> List.filter (System.String.IsNullOrWhiteSpace >> not)
            |> String.concat " "

        "swt:iconify-color swt:fluent-color--person-20", NestedMetadataInput.nonEmptyOr "Unnamed agent" label

    [<ReactComponent>]
    static member OptionalRow<'T>
        (
            fieldLabel: string,
            value: 'T option,
            constructor: unit -> 'T,
            setter: 'T option -> unit,
            icon: string,
            label: 'T -> string,
            navigate: 'T -> unit,
            ?imports: ImportCatalog -> 'T array
        ) =
        Html.div [
            prop.className "swt:space-y-2"
            prop.children [
                LayoutComponents.FieldTitle fieldLabel

                match value with
                | Some item ->
                    NestedMetadataInput.Row(
                        icon,
                        label item,
                        (fun () -> navigate item),
                        remove = (fun _ -> setter None)
                    )
                | None ->
                    Html.div [
                        prop.className "swt:flex swt:justify-center swt:w-full swt:gap-2"
                        prop.children [
                            Buttons.AddButton(fun _ -> setter (Some(constructor ())))
                            NestedMetadataInput.ImportControl(
                                (fun item -> icon, label item),
                                false,
                                (fun selected -> selected |> Array.tryHead |> Option.iter (Some >> setter)),
                                ?imports = imports
                            )
                        ]
                    ]
            ]
        ]

    [<ReactComponent>]
    static member OptionalDefinedTerm
        (
            fieldLabel: string,
            value: DefinedTerm option,
            setter: DefinedTerm option -> unit,
            navigate: DefinedTerm -> unit,
            ?imports: ImportCatalog -> DefinedTerm array
        ) =
        let icon, _ = NestedMetadataInput.DefinedTerm(DefinedTerm("New Defined Term"))

        NestedMetadataInput.OptionalRow(
            fieldLabel,
            value,
            (fun () -> DefinedTerm("New Defined Term")),
            setter,
            icon,
            (NestedMetadataInput.DefinedTerm >> snd),
            navigate,
            ?imports = imports
        )

    [<ReactComponent>]
    static member CreatePCInputSequence<'T>
        (
            inputs: ResizeArray<'T>,
            constructor: unit -> 'T,
            fieldLabel: string,
            presentation: 'T -> string * string,
            navigate: 'T -> unit,
            ?addItem: 'T -> unit,
            ?removeItem: 'T -> unit,
            ?imports: ImportCatalog -> 'T array,
            ?duplicateCandidates: ImportCatalog -> 'T array,
            ?isImportable: 'T -> bool,
            ?showLabel: bool,
            ?createOptions: (string * (unit -> 'T)) array,
            ?stickyFooter: bool
        ) =
        let catalog = useImportCatalogCtx ()
        let addItem = defaultArg addItem inputs.Add
        let isImportable = defaultArg isImportable (fun _ -> true)

        let newItemError candidate =
            let _, candidateName = presentation candidate

            let existingItems =
                Option.map2
                    (fun getItems catalog -> getItems catalog :> seq<'T>)
                    (duplicateCandidates |> Option.orElse imports)
                    catalog
                |> Option.defaultValue (inputs :> seq<'T>)

            let alreadyExists =
                existingItems
                |> Seq.exists (fun existing ->
                    let _, existingName = presentation existing

                    System.String.Equals(existingName, candidateName, System.StringComparison.OrdinalIgnoreCase)
                )

            if alreadyExists then
                Some $"An item named '{candidateName}' already exists in {fieldLabel.ToLowerInvariant()}."
            else
                None

        InputSequence.InputSequence(
            inputs,
            constructor = constructor,
            setter = ignore,
            addItem = addItem,
            newItemError = newItemError,
            ?removeItem = removeItem,
            inputComponent =
                (fun (item, _, remove) ->
                    let icon, label = presentation item
                    NestedMetadataInput.Row(icon, label, (fun () -> navigate item), remove = remove)
                ),
            ?label = (if defaultArg showLabel true then Some fieldLabel else None),
            footerElements =
                NestedMetadataInput.ImportControl(
                    presentation,
                    true,
                    (fun selected -> selected |> Array.iter addItem),
                    ?imports = imports,
                    isImportable =
                        (fun candidate ->
                            isImportable candidate
                            && (inputs |> Seq.exists (fun input -> obj.ReferenceEquals(candidate, input)) |> not)
                        )
                ),
            ?createOptions = createOptions,
            ?stickyFooter = stickyFooter
        )

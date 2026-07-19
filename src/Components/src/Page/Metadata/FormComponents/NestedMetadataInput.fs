namespace Swate.Components.Page.Metadata.FormComponents

open Fable.Core
open Feliz
open ProcessCore
open Swate.Components.Primitive.LayoutComponents

[<Erase; Mangle(false)>]
type NestedMetadataInput =

    [<ReactComponent>]
    static member Row(icon: string, label: string, navigate: unit -> unit, remove: Browser.Types.MouseEvent -> unit) =
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
                Helpers.DeleteButton remove
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
        "swt:iconify-color swt:fluent-color--comment-multiple-20",
        NestedMetadataInput.nonEmptyOr "Unnamed annotation" item.Name

    static member DefinedTerm(item: DefinedTerm) =
        "swt:iconify swt:fluent--tag-20-regular", NestedMetadataInput.nonEmptyOr "Unnamed defined term" item.Name

    static member FormalParameter(item: FormalParameter) =
        "swt:iconify swt:fluent--options-20-regular",
        NestedMetadataInput.nonEmptyOr "Unnamed formal parameter" item.Name

    static member Data(item: Data) =
        "swt:iconify-color swt:fluent-color--data-line-20", NestedMetadataInput.nonEmptyOr "Unnamed data" item.Name

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
            navigate: 'T -> unit
        ) =
        Html.div [
            prop.className "swt:space-y-2"
            prop.children [
                LayoutComponents.FieldTitle fieldLabel

                match value with
                | Some item ->
                    NestedMetadataInput.Row(icon, label item, (fun () -> navigate item), (fun _ -> setter None))
                | None ->
                    Html.div [
                        prop.className "swt:flex swt:justify-center swt:w-full"
                        prop.children [
                            Helpers.AddButton(fun _ -> setter (Some(constructor ())))
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
            navigate: DefinedTerm -> unit
        ) =
        let icon, _ = NestedMetadataInput.DefinedTerm(DefinedTerm(""))

        NestedMetadataInput.OptionalRow(
            fieldLabel,
            value,
            (fun () -> DefinedTerm("")),
            setter,
            icon,
            (NestedMetadataInput.DefinedTerm >> snd),
            navigate
        )

    [<ReactComponent>]
    static member CreatePCInputSequence<'T>
        (
            inputs: ResizeArray<'T>,
            constructor: unit -> 'T,
            setter: ResizeArray<'T> -> unit,
            fieldLabel: string,
            presentation: 'T -> string * string,
            navigate: 'T -> unit
        ) =
        InputSequence.InputSequence(
            inputs,
            constructor = constructor,
            setter = setter,
            inputComponent =
                (fun (item, _, remove) ->
                    let icon, label = presentation item
                    NestedMetadataInput.Row(icon, label, (fun () -> navigate item), remove)
                ),
            label = fieldLabel
        )

module Swate.Components.Page.Metadata.FormComponents.NestedMetadataInput

open Feliz
open ProcessCore
open Swate.Components.Primitive.LayoutComponents

let row (icon: string) (label: string) (navigate: unit -> unit) (remove: Browser.Types.MouseEvent -> unit) =
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
            Helpers.deleteButton remove
        ]
    ]

let nonEmptyOr fallback (value: string) =
    if System.String.IsNullOrWhiteSpace value then
        fallback
    else
        value

let optionOr fallback value =
    value |> Option.defaultValue "" |> nonEmptyOr fallback

let annotation (item: Annotation) =
    "swt:iconify-color swt:fluent-color--comment-multiple-20", nonEmptyOr "Unnamed annotation" item.Name

let definedTerm (item: DefinedTerm) =
    "swt:iconify swt:fluent--tag-20-regular", nonEmptyOr "Unnamed defined term" item.Name

let formalParameter (item: FormalParameter) =
    "swt:iconify swt:fluent--options-20-regular", nonEmptyOr "Unnamed formal parameter" item.Name

let data (item: Data) =
    "swt:iconify-color swt:fluent-color--data-line-20", nonEmptyOr "Unnamed data" item.Name

let agent (item: Agent) =
    let label =
        [
            item.GivenName
            item.FamilyName |> Option.defaultValue ""
        ]
        |> List.filter (System.String.IsNullOrWhiteSpace >> not)
        |> String.concat " "

    "swt:iconify-color swt:fluent-color--person-20", nonEmptyOr "Unnamed agent" label

let optionalRow
    (fieldLabel: string)
    (value: 'T option)
    (constructor: unit -> 'T)
    (setter: 'T option -> unit)
    (icon: string)
    (label: 'T -> string)
    (navigate: 'T -> unit)
    =
    Html.div [
        prop.className "swt:space-y-2"
        prop.children [
            LayoutComponents.FieldTitle fieldLabel

            match value with
            | Some item -> row icon (label item) (fun () -> navigate item) (fun _ -> setter None)
            | None ->
                Html.div [
                    prop.className "swt:flex swt:justify-center swt:w-full"
                    prop.children [
                        Helpers.addButton (fun _ -> setter (Some(constructor ())))
                    ]
                ]
        ]
    ]

let optionalDefinedTerm fieldLabel value setter navigate =
    let icon, _ = definedTerm (DefinedTerm(""))
    optionalRow fieldLabel value (fun () -> DefinedTerm("")) setter icon (definedTerm >> snd) navigate

let sequence
    (inputs: ResizeArray<'T>)
    (constructor: unit -> 'T)
    (setter: ResizeArray<'T> -> unit)
    (fieldLabel: string)
    (presentation: 'T -> string * string)
    (navigate: 'T -> unit)
    =
    InputSequence.InputSequence(
        inputs,
        constructor = constructor,
        setter = setter,
        inputComponent =
            (fun (item, _, remove) ->
                let icon, label = presentation item
                row icon label (fun () -> navigate item) remove
            ),
        label = fieldLabel
    )

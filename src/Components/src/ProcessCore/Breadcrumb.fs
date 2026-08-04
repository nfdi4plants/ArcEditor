module Swate.Components.ProcessCore.Breadcrumb

open Feliz

[<Literal>]
let MaxLabelLength = 25

let separator () =
    Html.i [
        prop.ariaHidden true
        prop.className "swt:iconify swt:fluent--chevron-right-16-regular swt:size-4 swt:shrink-0 swt:opacity-50"
    ]

let item (label: string) isEmphasized onClick =
    let text =
        if label.Length <= MaxLabelLength then
            label
        else
            $"{label.Substring(0, MaxLabelLength)}…"

    let commonProperties = [
        prop.className [
            "swt:min-w-0 swt:truncate swt:px-1"
            if isEmphasized then
                "swt:font-medium"
            if onClick |> Option.isSome then
                "swt:rounded swt:hover:bg-base-300"
        ]
        prop.title label
        prop.text text
    ]

    match onClick with
    | Some click ->
        Html.button [
            prop.type'.button
            prop.onClick (fun _ -> click ())
            yield! commonProperties
        ]
    | None -> Html.span commonProperties

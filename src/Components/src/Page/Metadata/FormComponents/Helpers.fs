namespace Swate.Components.Page.Metadata.FormComponents

open Browser.Types
open Fable.Core
open Feliz

[<Erase; Mangle(false)>]
type Helpers =

    [<ReactComponent>]
    static member AddButton(clickEvent: MouseEvent -> unit) =
        Html.button [
            prop.className "swt:btn swt:btn-info"
            prop.text "+"
            prop.onClick clickEvent
        ]

    [<ReactComponent>]
    static member DeleteButton(clickEvent: MouseEvent -> unit) =
        Html.button [
            prop.className "swt:btn swt:btn-sm swt:btn-error swt:grow-0"
            prop.text "Delete"
            prop.onClick clickEvent
        ]

    [<ReactComponent>]
    static member CardFormGroup(content: ReactElement list) =
        Html.div [
            prop.className "swt:grid swt:@md/main:grid-cols-2 swt:@xl/main:grid-flow-col swt:gap-4 not-prose"
            prop.children content
        ]

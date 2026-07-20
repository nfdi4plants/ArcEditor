namespace Swate.Components.Page.Metadata.FormComponents

open Browser.Types
open Fable.Core
open Feliz
open Swate.Components.Composite.TermSearch
open Swate.Components.Composite.TermSearch.Types
open Swate.Components.Primitive.LayoutComponents

[<Erase; Mangle(false)>]
type Helpers =

    [<ReactComponent>]
    static member TermInput(term: Term option, onTermChange: Term option -> unit, label: string) =
        Html.div [
            prop.className "swt:space-y-2"
            prop.children [
                LayoutComponents.FieldTitle label
                TermSearch.TermSearch(term, onTermChange, classNames = TermSearchStyle(U2.Case1 "swt:w-full"))
            ]
        ]

    [<ReactComponent>]
    static member RequiredTermInput(term: Term, onTermChange: Term -> unit, label: string) =
        Helpers.TermInput(
            Some term,
            (fun nextTerm ->
                nextTerm
                |> Option.filter (fun term -> term.name |> Option.exists (System.String.IsNullOrWhiteSpace >> not))
                |> Option.iter onTermChange
            ),
            label
        )

    [<ReactComponent>]
    static member AddButton(clickEvent: MouseEvent -> unit) =
        Html.button [
            prop.className "swt:btn swt:btn-info"
            prop.text "+"
            prop.onClick clickEvent
        ]

    [<ReactComponent>]
    static member CardFormGroup(content: ReactElement list) =
        Html.div [
            prop.className "swt:grid swt:@md/main:grid-cols-2 swt:@xl/main:grid-flow-col swt:gap-4 not-prose"
            prop.children content
        ]

namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
open ProcessCore
open Swate.Components.Shared
open Swate.Components.Page.Metadata
open Swate.Components.Composite.TermSearch.Types
open Swate.Components.Primitive.LayoutComponents

[<Erase; Mangle(false)>]
type DefinedTermMetadata =

    [<ReactComponent(true)>]
    static member DefinedTermView(definedTerm: ProcessCore.DefinedTerm, mutate: (ARC -> unit) -> unit) =

        let term =
            Term(name = definedTerm.Name, ?id = definedTerm.TAN, ?source = definedTerm.InDefinedTermSet)

        let handleTermSelect (selectedTerm: Term) =
            mutate (fun _ ->
                definedTerm.Name <- Option.defaultValue "" selectedTerm.name
                definedTerm.TAN <- selectedTerm.id
                definedTerm.InDefinedTermSet <- selectedTerm.source
            )

        LayoutComponents.Section [
            LayoutComponents.BoxedField(
                "Defined Term Metadata",
                content = [
                    FormComponents.Helpers.RequiredTermInput(term, handleTermSelect, "Name")
                    FormComponents.TextInput.TextInput(
                        definedTerm.TAN |> Option.defaultValue "",
                        (fun value ->
                            mutate (fun _ -> definedTerm.TAN <- Option.whereNot System.String.IsNullOrWhiteSpace value)
                        ),
                        label = "Term Accession Number"
                    )
                    FormComponents.TextInput.TextInput(
                        definedTerm.InDefinedTermSet |> Option.defaultValue "",
                        (fun value ->
                            mutate (fun _ ->
                                definedTerm.InDefinedTermSet <- Option.whereNot System.String.IsNullOrWhiteSpace value
                            )
                        ),
                        label = "Defined Term Set"
                    )
                ]
            )
        ]

type DefinedTermMetadata with

    [<ReactComponent>]
    static member DefinedTerms(terms: ResizeArray<DefinedTerm>, mutate: (ARC -> unit) -> unit) =
        Html.div [
            prop.className "swt:space-y-4"
            prop.children [
                for term in terms do
                    Html.div [
                        prop.className "swt:space-y-2"
                        prop.children [ DefinedTermMetadata.DefinedTermView(term, mutate) ]
                    ]
            ]
        ]

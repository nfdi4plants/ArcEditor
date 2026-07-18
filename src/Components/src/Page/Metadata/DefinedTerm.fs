namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
open Swate.Components.Composite.TermSearch.Types
open Swate.Components.Page.Metadata
open Swate.Components.Primitive.LayoutComponents

[<Erase; Mangle(false)>]
type DefinedTermMetadata =

    [<ReactComponent(true)>]
    static member DefinedTermView
        (definedTerm: ProcessCore.DefinedTerm, setDefinedTerm: ProcessCore.DefinedTerm -> unit)
        =

        let updateDefinedTerm (updateFn: ProcessCore.DefinedTerm -> ProcessCore.DefinedTerm) =
            let copy =
                ProcessCore.DefinedTerm(
                    definedTerm.Name,
                    ?tan = definedTerm.TAN,
                    ?inDefinedTermSet = definedTerm.InDefinedTermSet
                )

            let updatedDefinedTerm = updateFn copy
            setDefinedTerm updatedDefinedTerm

        let term =
            Term(name = definedTerm.Name, ?id = definedTerm.TAN, ?source = definedTerm.InDefinedTermSet)

        let updateTerm (selectedTerm: Term) =
            updateDefinedTerm (fun updatedDefinedTerm ->
                updatedDefinedTerm.Name <- selectedTerm.name |> Option.defaultValue definedTerm.Name
                updatedDefinedTerm.TAN <- selectedTerm.id
                updatedDefinedTerm.InDefinedTermSet <- selectedTerm.source
                updatedDefinedTerm
            )

        LayoutComponents.Section [
            LayoutComponents.BoxedField(
                "Defined Term Metadata",
                content = [
                    FormComponents.Helpers.RequiredTermInput(term, updateTerm, "Name")
                    FormComponents.TextInput.TextInput(
                        definedTerm.TAN |> Option.defaultValue "",
                        (fun value ->
                            updateDefinedTerm (fun updatedDefinedTerm ->
                                updatedDefinedTerm.TAN <- Some value
                                updatedDefinedTerm
                            )
                        ),
                        label = "Term Accession Number"
                    )
                    FormComponents.TextInput.TextInput(
                        definedTerm.InDefinedTermSet |> Option.defaultValue "",
                        (fun value ->
                            updateDefinedTerm (fun updatedDefinedTerm ->
                                updatedDefinedTerm.InDefinedTermSet <- Some value
                                updatedDefinedTerm
                            )
                        ),
                        label = "Defined Term Set"
                    )
                ]
            )
        ]

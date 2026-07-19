namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
open ProcessCore
open Swate.Components.Composite.TermSearch.Types
open Swate.Components.Page.Metadata
open Swate.Components.Primitive.LayoutComponents
open Swate.Components.Shared

[<Erase; Mangle(false)>]
type DefinedTermMetadata =

    [<ReactComponent(true)>]
    static member DefinedTermView
        (definedTerm: ProcessCore.DefinedTerm, setDefinedTerm: ProcessCore.DefinedTerm -> unit)
        =

        // let updateDefinedTerm (updateFn: ProcessCore.DefinedTerm -> ProcessCore.DefinedTerm) =
        //     let copy =
        //         ProcessCore.DefinedTerm(
        //             definedTerm.Name,
        //             ?tan = definedTerm.TAN,
        //             ?inDefinedTermSet = definedTerm.InDefinedTermSet
        //         )

        //     let updatedDefinedTerm = updateFn copy
        //     setDefinedTerm updatedDefinedTerm

        let term =
            Term(name = definedTerm.Name, ?id = definedTerm.TAN, ?source = definedTerm.InDefinedTermSet)

        let handleTermSelect (selectedTerm: Term) =
            definedTerm.Copy(
                name = Option.defaultValue "" selectedTerm.name,
                tan = selectedTerm.id,
                inDefinedTermSet = selectedTerm.source
            )
            |> setDefinedTerm

        LayoutComponents.Section [
            LayoutComponents.BoxedField(
                "Defined Term Metadata",
                content = [
                    FormComponents.Helpers.RequiredTermInput(term, handleTermSelect, "Name")
                    FormComponents.TextInput.TextInput(
                        definedTerm.TAN |> Option.defaultValue "",
                        (fun value ->
                            definedTerm.Copy(tan = Option.whereNot System.String.IsNullOrWhiteSpace value)
                            |> setDefinedTerm
                        ),
                        label = "Term Accession Number"
                    )
                    FormComponents.TextInput.TextInput(
                        definedTerm.InDefinedTermSet |> Option.defaultValue "",
                        (fun value ->
                            definedTerm.Copy(inDefinedTermSet = Option.whereNot System.String.IsNullOrWhiteSpace value)
                            |> setDefinedTerm
                        ),
                        label = "Defined Term Set"
                    )
                ]
            )
        ]

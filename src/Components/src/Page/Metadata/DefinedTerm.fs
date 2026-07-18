namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
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

        LayoutComponents.Section [
            LayoutComponents.BoxedField(
                "Defined Term Metadata",
                content = [
                    FormComponents.TextInput.TextInput(
                        definedTerm.Name,
                        (fun value ->
                            updateDefinedTerm (fun updatedDefinedTerm ->
                                updatedDefinedTerm.Name <- value
                                updatedDefinedTerm
                            )
                        ),
                        label = "Name",
                        // ProcessCore hotfix: prevent clearing this mandatory primary field.
                        validator = Swate.Components.ProcessCoreHotfixes.required "Name"
                    )
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

namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
open Swate.Components.Page.Metadata
open Swate.Components.Primitive.LayoutComponents

[<Erase; Mangle(false)>]
type FormalParameterMetadata =

    [<ReactComponent(true)>]
    static member FormalParameterMetadata
        (formalParameter: ProcessCore.FormalParameter, setFormalParameter: ProcessCore.FormalParameter -> unit)
        =

        let updateFormalParameter (updateFn: ProcessCore.FormalParameter -> ProcessCore.FormalParameter) =
            let copy =
                ProcessCore.FormalParameter(
                    formalParameter.Name,
                    ?nameTAN = formalParameter.NameTAN,
                    ?defaultValue = formalParameter.DefaultValue
                )

            let updatedFormalParameter = updateFn copy
            setFormalParameter updatedFormalParameter

        LayoutComponents.Section [
            LayoutComponents.BoxedField(
                "Formal Parameter Metadata",
                content = [
                    FormComponents.TextInput.TextInput(
                        formalParameter.Name,
                        (fun value ->
                            updateFormalParameter (fun updatedFormalParameter ->
                                updatedFormalParameter.Name <- value
                                updatedFormalParameter
                            )
                        ),
                        label = "Name"
                    )
                    FormComponents.TextInput.TextInput(
                        formalParameter.NameTAN |> Option.defaultValue "",
                        (fun value ->
                            updateFormalParameter (fun updatedFormalParameter ->
                                updatedFormalParameter.NameTAN <- Some value
                                updatedFormalParameter
                            )
                        ),
                        label = "Name TAN"
                    )
                    // TODO DefaultValue is a DefinedTerm, which is a complex type. We need a way to select or create an DefinedTerm.
                    Html.div
                        "Placeholder for DefaultValue (DefinedTerm) input. This should be a dropdown or a search field to select an existing DefinedTerm or create a new one."
                    FormComponents.TextInput.TextInput(
                        formalParameter.Name,
                        (fun input ->
                            updateFormalParameter (fun updatedSample ->
                                updatedSample.Name <- input
                                updatedSample
                            )
                        ),
                        label = "Default Value"
                    )
                ]
            )
        ]

namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
open ProcessCore
open Swate.Components.Page.Metadata
open Swate.Components.Page.ObjectBrowser.Types
open Swate.Components.Primitive.LayoutComponents
open Swate.Components.Page.Metadata.FormComponents

[<Erase; Mangle(false)>]
type FormalParameterMetadata =

    [<ReactComponent(true)>]
    static member FormalParameterView
        (
            formalParameter: ProcessCore.FormalParameter,
            setFormalParameter: ProcessCore.FormalParameter -> unit,
            ?onNavigate: ProcessCoreEntityValue -> unit
        ) =

        let navigate = defaultArg onNavigate ignore

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
                    (NestedMetadataInput.OptionalDefinedTerm(
                        "Default Value",
                        formalParameter.DefaultValue,
                        (fun defaultValue ->
                            FormalParameter(
                                formalParameter.Name,
                                ?nameTAN = formalParameter.NameTAN,
                                ?defaultValue = defaultValue
                            )
                            |> setFormalParameter
                        ),
                        (ProcessCoreEntityValue.DefinedTerm >> navigate)
                    ))
                ]
            )
        ]

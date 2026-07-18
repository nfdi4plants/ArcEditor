namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
open ProcessCore
open Swate.Components.Composite.TermSearch.Types
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

        let nameTerm = Term(name = formalParameter.Name, ?id = formalParameter.NameTAN)

        let updateNameTerm (selectedTerm: Term) =
            updateFormalParameter (fun updatedFormalParameter ->
                updatedFormalParameter.Name <- selectedTerm.name |> Option.defaultValue formalParameter.Name

                updatedFormalParameter.NameTAN <- selectedTerm.id
                updatedFormalParameter
            )

        LayoutComponents.Section [
            LayoutComponents.BoxedField(
                "Formal Parameter Metadata",
                content = [
                    Helpers.RequiredTermInput(nameTerm, updateNameTerm, "Name")
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
                        (ProcessCoreEntityValue.DefinedTerm >> navigate),
                        imports = (fun catalog -> catalog.DefinedTerms)
                    ))
                ]
            )
        ]

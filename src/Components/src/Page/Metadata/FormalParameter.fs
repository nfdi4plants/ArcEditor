namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
open ProcessCore
open Swate.Components.Shared
open Swate.Components.Composite.TermSearch.Types
open Swate.Components.Primitive.LayoutComponents
open Swate.Components.Page.Metadata
open Swate.Components.Page.ObjectBrowser.Types
open Swate.Components.Page.Metadata.FormComponents

[<Erase; Mangle(false)>]
type FormalParameterMetadata =

    [<ReactComponent(true)>]
    static member FormalParameterView
        (
            formalParameter: ProcessCore.FormalParameter,
            mutate: (ARC -> unit) -> unit,
            ?onNavigate: ProcessCoreEntityValue -> unit
        ) =

        let navigate = defaultArg onNavigate ignore

        let nameTerm = Term(name = formalParameter.Name, ?id = formalParameter.NameTAN)

        let handleTermSelect (selectedTerm: Term) =
            mutate (fun _ ->
                formalParameter.Name <- Option.defaultValue "" selectedTerm.name
                formalParameter.NameTAN <- selectedTerm.id
            )

        LayoutComponents.Section [
            LayoutComponents.BoxedField(
                "Formal Parameter Metadata",
                content = [
                    Helpers.RequiredTermInput(nameTerm, handleTermSelect, "Name")
                    FormComponents.TextInput.TextInput(
                        formalParameter.NameTAN |> Option.defaultValue "",
                        (fun value ->
                            mutate (fun _ ->
                                formalParameter.NameTAN <- Option.whereNot System.String.IsNullOrWhiteSpace value
                            )
                        ),
                        label = "Name TAN"
                    )
                    (NestedMetadataInput.OptionalDefinedTerm(
                        "Default Value",
                        formalParameter.DefaultValue,
                        (fun defaultValue -> mutate (fun _ -> formalParameter.DefaultValue <- defaultValue)),
                        (ProcessCoreEntityValue.DefinedTerm >> navigate)
                    ))
                ]
            )
        ]

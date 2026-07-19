namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
open ProcessCore
open Swate.Components.Shared // Option extension
open Swate.Components.Composite.TermSearch.Types
open Swate.Components.Page.Metadata
open Swate.Components.Page.ObjectBrowser.Types
open Swate.Components.Primitive.LayoutComponents
open Swate.Components.Page.Metadata.FormComponents

[<Erase; Mangle(false)>]
type AnnotationMetadata =

    [<ReactComponent(true)>]
    static member AnnotationView
        (
            annotation: ProcessCore.Annotation,
            setAnnotation: ProcessCore.Annotation -> unit,
            ?onNavigate: ProcessCoreEntityValue -> unit
        ) =

        let navigate = defaultArg onNavigate ignore

        let nameTerm = Term(name = annotation.Name, ?id = annotation.NameTAN)

        let unitTerm =
            annotation.Unit
            |> Option.map (fun unitName -> Term(name = unitName, ?id = annotation.UnitTAN))

        let updateNameTerm (selectedTerm: Term) =
            annotation.Copy(name = Option.defaultValue "" selectedTerm.name, nameTAN = selectedTerm.id)
            |> setAnnotation

        let updateUnit (selectedTerm: Term option) =
            annotation.Copy(
                unit = (selectedTerm |> Option.bind (fun term -> term.name)),
                unitTAN = (selectedTerm |> Option.bind (fun term -> term.id))
            )
            |> setAnnotation

        LayoutComponents.Section [
            LayoutComponents.BoxedField(
                "Annotation Metadata",
                content = [
                    Helpers.RequiredTermInput(nameTerm, updateNameTerm, "Name")
                    FormComponents.TextInput.TextInput(
                        annotation.Value |> Option.defaultValue "",
                        (fun value ->
                            annotation.Copy(value = Option.whereNot System.String.IsNullOrWhiteSpace value)
                            |> setAnnotation
                        ),
                        label = "Value"
                    )
                    Helpers.TermInput(unitTerm, updateUnit, "Unit")
                    FormComponents.TextInput.TextInput(
                        annotation.NameTAN |> Option.defaultValue "",
                        (fun value ->
                            annotation.Copy(nameTAN = Option.whereNot System.String.IsNullOrWhiteSpace value)
                            |> setAnnotation
                        ),
                        label = "Name TAN"
                    )
                    FormComponents.TextInput.TextInput(
                        annotation.ValueTAN |> Option.defaultValue "",
                        (fun value ->
                            annotation.Copy(valueTAN = Option.whereNot System.String.IsNullOrWhiteSpace value)
                            |> setAnnotation
                        ),
                        label = "Value TAN"
                    )
                    FormComponents.TextInput.TextInput(
                        annotation.UnitTAN |> Option.defaultValue "",
                        (fun value ->
                            annotation.Copy(unitTAN = Option.whereNot System.String.IsNullOrWhiteSpace value)
                            |> setAnnotation
                        ),
                        label = "Unit TAN"
                    )
                    FormComponents.TextInput.TextInput(
                        annotation.AdditionalType |> Option.defaultValue "",
                        (fun value ->
                            annotation.Copy(additionalType = Option.whereNot System.String.IsNullOrWhiteSpace value)
                            |> setAnnotation
                        ),
                        label = "Additional Type"
                    )
                    (NestedMetadataInput.OptionalRow(
                        "Instance Of",
                        annotation.InstanceOf,
                        (fun () -> FormalParameter("")),
                        (fun instanceOf -> annotation.Copy(instanceOf = instanceOf) |> setAnnotation),
                        "swt:iconify swt:fluent--options-20-regular",
                        (fun parameter -> NestedMetadataInput.nonEmptyOr "Unnamed formal parameter" parameter.Name),
                        (ProcessCoreEntityValue.FormalParameter >> navigate),
                        imports = (fun catalog -> catalog.FormalParameters)
                    ))
                ]
            )
        ]

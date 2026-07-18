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
type AnnotationMetadata =

    [<ReactComponent(true)>]
    static member AnnotationView
        (
            annotation: ProcessCore.Annotation,
            setAnnotation: ProcessCore.Annotation -> unit,
            ?onNavigate: ProcessCoreEntityValue -> unit
        ) =

        let navigate = defaultArg onNavigate ignore

        let updateAnnotation (updateFn: ProcessCore.Annotation -> ProcessCore.Annotation) =
            let copy =
                ProcessCore.Annotation(
                    annotation.Name,
                    ?value = annotation.Value,
                    ?unit = annotation.Unit,
                    ?nameTAN = annotation.NameTAN,
                    ?valueTAN = annotation.ValueTAN,
                    ?unitTAN = annotation.UnitTAN,
                    ?additionalType = annotation.AdditionalType,
                    ?instanceOf = annotation.InstanceOf
                )

            let updatedAnnotation = updateFn copy
            setAnnotation updatedAnnotation

        let nameTerm = Term(name = annotation.Name, ?id = annotation.NameTAN)

        let unitTerm =
            annotation.Unit
            |> Option.map (fun unitName -> Term(name = unitName, ?id = annotation.UnitTAN))

        let updateNameTerm (selectedTerm: Term) =
            updateAnnotation (fun updatedAnnotation ->
                updatedAnnotation.Name <- selectedTerm.name |> Option.defaultValue annotation.Name
                updatedAnnotation.NameTAN <- selectedTerm.id
                updatedAnnotation
            )

        let updateUnit (selectedTerm: Term option) =
            updateAnnotation (fun updatedAnnotation ->
                updatedAnnotation.Unit <- selectedTerm |> Option.bind (fun term -> term.name)
                updatedAnnotation.UnitTAN <- selectedTerm |> Option.bind (fun term -> term.id)
                updatedAnnotation
            )

        LayoutComponents.Section [
            LayoutComponents.BoxedField(
                "Annotation Metadata",
                content = [
                    Helpers.RequiredTermInput(nameTerm, updateNameTerm, "Name")
                    FormComponents.TextInput.TextInput(
                        annotation.Value |> Option.defaultValue "",
                        (fun value ->
                            updateAnnotation (fun updatedAnnotation ->
                                updatedAnnotation.Value <- Some value
                                updatedAnnotation
                            )
                        ),
                        label = "Value"
                    )
                    Helpers.TermInput(unitTerm, updateUnit, "Unit")
                    FormComponents.TextInput.TextInput(
                        annotation.NameTAN |> Option.defaultValue "",
                        (fun value ->
                            updateAnnotation (fun updatedAnnotation ->
                                updatedAnnotation.NameTAN <- Some value
                                updatedAnnotation
                            )
                        ),
                        label = "Name TAN"
                    )
                    FormComponents.TextInput.TextInput(
                        annotation.ValueTAN |> Option.defaultValue "",
                        (fun value ->
                            updateAnnotation (fun updatedAnnotation ->
                                updatedAnnotation.ValueTAN <- Some value
                                updatedAnnotation
                            )
                        ),
                        label = "Value TAN"
                    )
                    FormComponents.TextInput.TextInput(
                        annotation.UnitTAN |> Option.defaultValue "",
                        (fun value ->
                            updateAnnotation (fun updatedAnnotation ->
                                updatedAnnotation.UnitTAN <- Some value
                                updatedAnnotation
                            )
                        ),
                        label = "Unit TAN"
                    )
                    FormComponents.TextInput.TextInput(
                        annotation.AdditionalType |> Option.defaultValue "",
                        (fun value ->
                            updateAnnotation (fun updatedAnnotation ->
                                updatedAnnotation.AdditionalType <- Some value
                                updatedAnnotation
                            )
                        ),
                        label = "Additional Type"
                    )
                    (NestedMetadataInput.OptionalRow(
                        "Instance Of",
                        annotation.InstanceOf,
                        (fun () -> FormalParameter("")),
                        (fun instanceOf ->
                            Annotation(
                                annotation.Name,
                                ?value = annotation.Value,
                                ?unit = annotation.Unit,
                                ?nameTAN = annotation.NameTAN,
                                ?valueTAN = annotation.ValueTAN,
                                ?unitTAN = annotation.UnitTAN,
                                ?additionalType = annotation.AdditionalType,
                                ?instanceOf = instanceOf
                            )
                            |> setAnnotation
                        ),
                        "swt:iconify swt:fluent--options-20-regular",
                        (fun parameter -> NestedMetadataInput.nonEmptyOr "Unnamed formal parameter" parameter.Name),
                        (ProcessCoreEntityValue.FormalParameter >> navigate),
                        imports = (fun catalog -> catalog.FormalParameters)
                    ))
                ]
            )
        ]

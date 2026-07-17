namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
open Swate.Components.Page.Metadata
open Swate.Components.Primitive.LayoutComponents

[<Erase; Mangle(false)>]
type AnnotationMetadata =

    [<ReactComponent(true)>]
    static member AnnotationMetadata
        (annotation: ProcessCore.Annotation, setAnnotation: ProcessCore.Annotation -> unit)
        =

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

        LayoutComponents.Section [
            LayoutComponents.BoxedField(
                "Dataset Metadata",
                content = [
                    FormComponents.TextInput.TextInput(
                        annotation.Name,
                        (fun value ->
                            updateAnnotation (fun updatedAnnotation ->
                                updatedAnnotation.Name <- value
                                updatedAnnotation
                            )
                        ),
                        label = "Name"
                    )
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
                    FormComponents.TextInput.TextInput(
                        annotation.Unit |> Option.defaultValue "",
                        (fun value ->
                            updateAnnotation (fun updatedAnnotation ->
                                updatedAnnotation.Unit <- Some value
                                updatedAnnotation
                            )
                        ),
                        label = "Unit"
                    )
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
                    // TODO InstanceOf is a FormalParameter, which is a complex type. We need a way to select or create a FormalParameter.
                    Html.div
                        "Placeholder for InstanceOf (FormalParameter) input. This should be a dropdown or a search field to select an existing FormalParameter or create a new one."
                    FormComponents.TextInput.TextInput(
                        annotation.Name,
                        (fun input ->
                            updateAnnotation (fun updatedAnnotation ->
                                updatedAnnotation.Name <- input
                                updatedAnnotation
                            )
                        ),
                        label = "Instance Of"
                    )
                ]
            )
        ]

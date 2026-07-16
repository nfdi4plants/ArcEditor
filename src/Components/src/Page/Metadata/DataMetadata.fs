namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
open Swate.Components.Page.Metadata
open Swate.Components.Primitive.LayoutComponents

[<Erase; Mangle(false)>]
type DataMetadata =

    [<ReactComponent(true)>]
    static member DataMetadata(data: ProcessCore.Data, setData: ProcessCore.Data -> unit) =

        let updateData (updateFn: ProcessCore.Data -> ProcessCore.Data) =
            let copy =
                ProcessCore.Data(
                    data.Path,
                    ?selector = data.Selector,
                    ?selectorFormat = data.SelectorFormat,
                    ?encodingFormat = data.EncodingFormat,
                    ?additionalType = data.AdditionalType,
                    hasPart = data.HasPart,
                    additionalProperty = data.AdditionalProperty
                )

            let updatedData = updateFn copy
            setData updatedData

        LayoutComponents.Section [
            LayoutComponents.BoxedField(
                "Data Metadata",
                content = [
                    FormComponents.TextInput.TextInput(
                        data.Path,
                        (fun value ->
                            updateData (fun updatedData ->
                                updatedData.Path <- value
                                updatedData
                            )
                        ),
                        label = "Path"
                    )
                    FormComponents.TextInput.TextInput(
                        data.SelectorFormat |> Option.defaultValue "",
                        (fun value ->
                            updateData (fun updatedData ->
                                updatedData.SelectorFormat <- Some value
                                updatedData
                            )
                        ),
                        label = "Selector Format"
                    )
                    FormComponents.TextInput.TextInput(
                        data.EncodingFormat |> Option.defaultValue "",
                        (fun value ->
                            updateData (fun updatedData ->
                                updatedData.EncodingFormat <- Some value
                                updatedData
                            )
                        ),
                        label = "Encoding Format"
                    )
                    FormComponents.TextInput.TextInput(
                        data.AdditionalType |> Option.defaultValue "",
                        (fun value ->
                            updateData (fun updatedData ->
                                updatedData.AdditionalType <- Some value
                                updatedData
                            )
                        ),
                        label = "Additional Type"
                    )
                    // TODO AdditionalProperty is a Annotation seq, which is a complex type. We need a way to select or create an Annotation.
                    Html.div
                        "Placeholder for AdditionalProperty (Annotation seq) input. This should be a dropdown or a search field to select an existing Annotation or create a new one."
                    FormComponents.TextInput.TextInput(
                        data.Path,
                        (fun input ->
                            updateData (fun updatedData ->
                                updatedData.Path <- input
                                updatedData
                            )
                        ),
                        label = "Processes"
                    )
                    // TODO HasPart is a Data seq, which is a complex type. We need a way to select or create a Data.
                    Html.div
                        "Placeholder for HasPart (Data seq) input. This should be a dropdown or a search field to select an existing Data or create a new one."
                    FormComponents.TextInput.TextInput(
                        data.Path,
                        (fun input ->
                            updateData (fun updatedData ->
                                updatedData.Path <- input
                                updatedData
                            )
                        ),
                        label = "Has Part"
                    )
                    // TODO AdditionalProperty is an Annotation seq, which is a complex type. We need a way to select or create an Annotation.
                    Html.div
                        "Placeholder for AdditionalProperty (Annotation seq) input. This should be a dropdown or a search field to select an existing Annotation or create a new one."
                    FormComponents.TextInput.TextInput(
                        data.Path,
                        (fun input ->
                            updateData (fun updatedData ->
                                updatedData.Path <- input
                                updatedData
                            )
                        ),
                        label = "Additional Property"
                    )
                ]
            )
        ]

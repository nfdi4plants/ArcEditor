namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
open Swate.Components.Page.Metadata
open Swate.Components.Primitive.LayoutComponents

[<Erase; Mangle(false)>]
type SampleMetadata =

    [<ReactComponent(true)>]
    static member SampleMetadata(sample: ProcessCore.Sample, setSample: ProcessCore.Sample -> unit) =

        let updateSample (updateFn: ProcessCore.Sample -> ProcessCore.Sample) =
            let copy =
                ProcessCore.Sample(
                    sample.Name,
                    ?additionalType = sample.AdditionalType,
                    additionalProperty = sample.AdditionalProperty
                )

            let updatedSample = updateFn copy
            setSample updatedSample

        LayoutComponents.Section [
            LayoutComponents.BoxedField(
                "Sample Metadata",
                content = [
                    FormComponents.TextInput.TextInput(
                        sample.Name,
                        (fun value ->
                            updateSample (fun updatedSample ->
                                updatedSample.Name <- value
                                updatedSample
                            )
                        ),
                        label = "Name"
                    )
                    FormComponents.TextInput.TextInput(
                        sample.AdditionalType |> Option.defaultValue "",
                        (fun value ->
                            updateSample (fun updatedSample ->
                                updatedSample.AdditionalType <- Some value
                                updatedSample
                            )
                        ),
                        label = "Additional Type"
                    )
                    // TODO AdditionalProperty is a Annotation seq, which is a complex type. We need a way to select or create an Annotation.
                    Html.div
                        "Placeholder for AdditionalProperty (Annotation seq) input. This should be a dropdown or a search field to select an existing Annotation or create a new one."
                    FormComponents.TextInput.TextInput(
                        sample.Name,
                        (fun input ->
                            updateSample (fun updatedSample ->
                                updatedSample.Name <- input
                                updatedSample
                            )
                        ),
                        label = "Additional Property"
                    )
                ]
            )
        ]

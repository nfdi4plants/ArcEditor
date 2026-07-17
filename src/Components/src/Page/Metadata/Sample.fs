namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
open ProcessCore
open Swate.Components.Page.Metadata
open Swate.Components.Page.ObjectBrowser.Types
open Swate.Components.Primitive.LayoutComponents
open Swate.Components.Page.Metadata.FormComponents

[<Erase; Mangle(false)>]
type SampleMetadata =

    [<ReactComponent(true)>]
    static member SampleView
        (sample: ProcessCore.Sample, setSample: ProcessCore.Sample -> unit, ?onNavigate: ProcessCoreEntityValue -> unit)
        =

        let navigate = defaultArg onNavigate ignore

        let copySample (additionalProperties: ResizeArray<Annotation>) =
            ProcessCore.Sample(
                sample.Name,
                ?additionalType = sample.AdditionalType,
                additionalProperty = additionalProperties
            )

        let updateSample (updateFn: ProcessCore.Sample -> ProcessCore.Sample) =
            let copy = copySample sample.AdditionalProperty

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
                    NestedMetadataInput.CreatePCInputSequence(
                        (ResizeArray sample.AdditionalProperty),
                        (fun () -> Annotation("")),
                        (copySample >> setSample),
                        "Additional Properties",
                        NestedMetadataInput.Annotation,
                        (ProcessCoreEntityValue.Annotation >> navigate)
                    )
                ]
            )
        ]

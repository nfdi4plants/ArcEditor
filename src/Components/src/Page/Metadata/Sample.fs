namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
open ProcessCore
open Swate.Components.Shared
open Swate.Components.Primitive.LayoutComponents
open Swate.Components.Page.Metadata
open Swate.Components.Page.ObjectBrowser.Types
open Swate.Components.Page.Metadata.FormComponents


[<Erase; Mangle(false)>]
type SampleMetadata =

    [<ReactComponent(true)>]
    static member SampleView
        (sample: ProcessCore.Sample, mutate: (ARC -> unit) -> unit, ?onNavigate: ProcessCoreEntityValue -> unit)
        =

        let navigate = defaultArg onNavigate ignore

        let additionalProperties =
            MetadataRelationship.create
                mutate
                sample.AdditionalProperty
                sample.AddAdditionalProperty
                sample.RemoveAdditionalProperty

        LayoutComponents.Section [
            LayoutComponents.BoxedField(
                "Sample Metadata",
                content = [
                    FormComponents.TextInput.TextInput(
                        sample.Name,
                        (fun value -> mutate (fun _ -> sample.Name <- value)),
                        label = "Name",
                        // ProcessCore hotfix: prevent clearing this mandatory primary field.
                        validator = Swate.Components.ProcessCoreHotfixes.required "Name"
                    )
                    FormComponents.TextInput.TextInput(
                        sample.AdditionalType |> Option.defaultValue "",
                        (fun value ->
                            mutate (fun _ ->
                                sample.AdditionalType <- Option.whereNot System.String.IsNullOrWhiteSpace value
                            )
                        ),
                        label = "Additional Type"
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        sample.AdditionalProperty,
                        (fun () -> Annotation("New Annotation")),
                        "Additional Properties",
                        NestedMetadataInput.Annotation,
                        (ProcessCoreEntityValue.Annotation >> navigate),
                        imports = (fun catalog -> catalog.Annotations),
                        duplicateCandidates = (fun catalog -> catalog.Annotations),
                        addItem = additionalProperties.Add,
                        removeItem = additionalProperties.Remove
                    )
                ]
            )
        ]

    [<ReactComponent>]
    static member Inputs(inputs: ResizeArray<IONode>, mutate: (ARC -> unit) -> unit) =
        Html.div [
            prop.className "swt:space-y-4"
            prop.children [
                for input in inputs do
                    match input with
                    | SampleNode sample -> SampleMetadata.SampleView(sample, mutate)
                    | DataNode _ -> ()
            ]
        ]

    [<ReactComponent>]
    static member Outputs(outputs: ResizeArray<IONode>, mutate: (ARC -> unit) -> unit) =
        Html.div [
            prop.className "swt:space-y-4"
            prop.children [
                for output in outputs do
                    match output with
                    | SampleNode sample -> SampleMetadata.SampleView(sample, mutate)
                    | DataNode _ -> ()
            ]
        ]

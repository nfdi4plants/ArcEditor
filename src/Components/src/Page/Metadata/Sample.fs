namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
open ProcessCore
open Swate.Components.Page.Metadata
open Swate.Components.Page.ObjectBrowser.Types
open Swate.Components.Primitive.LayoutComponents
open Swate.Components.Page.Metadata.FormComponents
open Swate.Components.Shared

[<Erase; Mangle(false)>]
type SampleMetadata =

    [<ReactComponent(true)>]
    static member SampleView
        (sample: ProcessCore.Sample, setSample: ProcessCore.Sample -> unit, ?onNavigate: ProcessCoreEntityValue -> unit)
        =

        let navigate = defaultArg onNavigate ignore

        LayoutComponents.Section [
            LayoutComponents.BoxedField(
                "Sample Metadata",
                content = [
                    FormComponents.TextInput.TextInput(
                        sample.Name,
                        (fun value -> sample.Copy(name = value) |> setSample),
                        label = "Name",
                        // ProcessCore hotfix: prevent clearing this mandatory primary field.
                        validator = Swate.Components.ProcessCoreHotfixes.required "Name"
                    )
                    FormComponents.TextInput.TextInput(
                        sample.AdditionalType |> Option.defaultValue "",
                        (fun value ->
                            sample.Copy(additionalType = Option.whereNot System.String.IsNullOrWhiteSpace value)
                            |> setSample
                        ),
                        label = "Additional Type"
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        (ResizeArray sample.AdditionalProperty),
                        (fun () -> Annotation("New Annotation")),
                        (fun value -> sample.Copy(additionalProperty = value) |> setSample),
                        "Additional Properties",
                        NestedMetadataInput.Annotation,
                        (ProcessCoreEntityValue.Annotation >> navigate),
                        imports = (fun catalog -> catalog.Annotations)
                    )
                ]
            )
        ]

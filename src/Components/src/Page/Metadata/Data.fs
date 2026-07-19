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
type DataMetadata =

    [<ReactComponent(true)>]
    static member DataView
        (data: ProcessCore.Data, setData: ProcessCore.Data -> unit, ?onNavigate: ProcessCoreEntityValue -> unit)
        =

        let navigate = defaultArg onNavigate ignore

        let rec containsData (target: ProcessCore.Data) (candidate: ProcessCore.Data) =
            obj.ReferenceEquals(target, candidate)
            || (candidate.HasPart |> Seq.exists (containsData target))

        let importableParts (catalog: ImportCatalogContext.ImportCatalog) =
            catalog.Data |> Array.filter (containsData data >> not)

        // let copyData (parts: ResizeArray<Data>) (additionalProperties: ResizeArray<Annotation>) =
        //     ProcessCore.Data(
        //         data.Path,
        //         ?selector = data.Selector,
        //         ?selectorFormat = data.SelectorFormat,
        //         ?encodingFormat = data.EncodingFormat,
        //         ?additionalType = data.AdditionalType,
        //         hasPart = parts,
        //         additionalProperty = additionalProperties
        //     )

        // let updateData (updateFn: ProcessCore.Data -> ProcessCore.Data) =
        //     let copy = copyData data.HasPart data.AdditionalProperty

        //     let updatedData = updateFn copy
        //     setData updatedData

        LayoutComponents.Section [
            LayoutComponents.BoxedField(
                "Data Metadata",
                content = [
                    FormComponents.TextInput.TextInput(
                        data.Path,
                        (fun value -> data.Copy(path = value) |> setData),
                        label = "Path",
                        // ProcessCore hotfix: prevent clearing this mandatory primary field.
                        validator = Swate.Components.ProcessCoreHotfixes.required "Path"
                    )
                    FormComponents.TextInput.TextInput(
                        data.SelectorFormat |> Option.defaultValue "",
                        (fun value ->
                            data.Copy(selectorFormat = Option.whereNot System.String.IsNullOrWhiteSpace value)
                            |> setData
                        ),
                        label = "Selector Format"
                    )
                    FormComponents.TextInput.TextInput(
                        data.EncodingFormat |> Option.defaultValue "",
                        (fun value ->
                            data.Copy(encodingFormat = Option.whereNot System.String.IsNullOrWhiteSpace value)
                            |> setData
                        ),
                        label = "Encoding Format"
                    )
                    FormComponents.TextInput.TextInput(
                        data.AdditionalType |> Option.defaultValue "",
                        (fun value ->
                            data.Copy(additionalType = Option.whereNot System.String.IsNullOrWhiteSpace value)
                            |> setData
                        ),
                        label = "Additional Type"
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        (ResizeArray data.HasPart),
                        (fun () -> Data("")),
                        (fun parts -> data.Copy(hasPart = parts) |> setData),
                        "Has Part",
                        NestedMetadataInput.Data,
                        (ProcessCoreEntityValue.Data >> navigate),
                        imports = importableParts
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        (ResizeArray data.AdditionalProperty),
                        (fun () -> Annotation("")),
                        (fun properties -> data.Copy(additionalProperty = properties) |> setData),
                        "Additional Properties",
                        NestedMetadataInput.Annotation,
                        (ProcessCoreEntityValue.Annotation >> navigate),
                        imports = (fun catalog -> catalog.Annotations)
                    )
                ]
            )
        ]

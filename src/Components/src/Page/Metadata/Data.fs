namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
open ProcessCore
open Swate.Components.Page.Metadata
open Swate.Components.Page.ObjectBrowser.Types
open Swate.Components.Primitive.LayoutComponents
open Swate.Components.Page.Metadata.FormComponents

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

        let copyData (parts: ResizeArray<Data>) (additionalProperties: ResizeArray<Annotation>) =
            ProcessCore.Data(
                data.Path,
                ?selector = data.Selector,
                ?selectorFormat = data.SelectorFormat,
                ?encodingFormat = data.EncodingFormat,
                ?additionalType = data.AdditionalType,
                hasPart = parts,
                additionalProperty = additionalProperties
            )

        let updateData (updateFn: ProcessCore.Data -> ProcessCore.Data) =
            let copy = copyData data.HasPart data.AdditionalProperty

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
                        label = "Path",
                        // ProcessCore hotfix: prevent clearing this mandatory primary field.
                        validator = Swate.Components.ProcessCoreHotfixes.required "Path"
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
                    NestedMetadataInput.CreatePCInputSequence(
                        (ResizeArray data.HasPart),
                        (fun () -> Data("")),
                        (fun parts -> copyData parts data.AdditionalProperty |> setData),
                        "Has Part",
                        NestedMetadataInput.Data,
                        (ProcessCoreEntityValue.Data >> navigate),
                        imports = importableParts
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        (ResizeArray data.AdditionalProperty),
                        (fun () -> Annotation("")),
                        (fun properties -> copyData data.HasPart properties |> setData),
                        "Additional Properties",
                        NestedMetadataInput.Annotation,
                        (ProcessCoreEntityValue.Annotation >> navigate),
                        imports = (fun catalog -> catalog.Annotations)
                    )
                ]
            )
        ]

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
type DataMetadata =

    [<ReactComponent(true)>]
    static member DataView
        (data: ProcessCore.Data, mutate: (ARC -> unit) -> unit, ?onNavigate: ProcessCoreEntityValue -> unit)
        =

        let navigate = defaultArg onNavigate ignore

        let rec containsData (target: ProcessCore.Data) (candidate: ProcessCore.Data) =
            obj.ReferenceEquals(target, candidate)
            || (candidate.HasPart |> Seq.exists (containsData target))

        let importableParts (catalog: ImportCatalogContext.ImportCatalog) =
            catalog.Data |> Array.filter (containsData data >> not)

        LayoutComponents.Section [
            LayoutComponents.BoxedField(
                "Data Metadata",
                content = [
                    FormComponents.TextInput.TextInput(
                        data.Path,
                        (fun value -> mutate (fun _ -> data.Path <- value)),
                        label = "Path",
                        // ProcessCore hotfix: prevent clearing this mandatory primary field.
                        validator = Swate.Components.ProcessCoreHotfixes.required "Path"
                    )
                    FormComponents.TextInput.TextInput(
                        data.SelectorFormat |> Option.defaultValue "",
                        (fun value ->
                            mutate (fun _ ->
                                data.SelectorFormat <- Option.whereNot System.String.IsNullOrWhiteSpace value
                            )
                        ),
                        label = "Selector Format"
                    )
                    FormComponents.TextInput.TextInput(
                        data.EncodingFormat |> Option.defaultValue "",
                        (fun value ->
                            mutate (fun _ ->
                                data.EncodingFormat <- Option.whereNot System.String.IsNullOrWhiteSpace value
                            )
                        ),
                        label = "Encoding Format"
                    )
                    FormComponents.TextInput.TextInput(
                        data.AdditionalType |> Option.defaultValue "",
                        (fun value ->
                            mutate (fun _ ->
                                data.AdditionalType <- Option.whereNot System.String.IsNullOrWhiteSpace value
                            )
                        ),
                        label = "Additional Type"
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        data.HasPart,
                        (fun () -> Data("New Data")),
                        ignore,
                        "Has Part",
                        NestedMetadataInput.Data,
                        (ProcessCoreEntityValue.Data >> navigate),
                        imports = importableParts,
                        addItem = (fun item -> mutate (fun _ -> data.AddPart item)),
                        removeItem = (fun item -> mutate (fun _ -> data.RemovePart item))
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        data.AdditionalProperty,
                        (fun () -> Annotation("New Annotation")),
                        ignore,
                        "Additional Properties",
                        NestedMetadataInput.Annotation,
                        (ProcessCoreEntityValue.Annotation >> navigate),
                        imports = (fun catalog -> catalog.Annotations),
                        addItem = (fun item -> mutate (fun _ -> data.AddAdditionalProperty item)),
                        removeItem = (fun item -> mutate (fun _ -> data.RemoveAdditionalProperty item))
                    )
                ]
            )
        ]

    [<ReactComponent>]
    static member DataItems(dataItems: ResizeArray<Data>, mutate: (ARC -> unit) -> unit) =
        Html.div [
            prop.className "swt:space-y-4"
            prop.children [
                for data in dataItems do
                    DataMetadata.DataView(data, mutate)
            ]
        ]

    [<ReactComponent>]
    static member Inputs(inputs: ResizeArray<IONode>, mutate: (ARC -> unit) -> unit) =
        Html.div [
            prop.className "swt:space-y-4"
            prop.children [
                for input in inputs do
                    match input with
                    | DataNode data -> DataMetadata.DataView(data, mutate)
                    | SampleNode _ -> ()
            ]
        ]

    [<ReactComponent>]
    static member Outputs(outputs: ResizeArray<IONode>, mutate: (ARC -> unit) -> unit) =
        Html.div [
            prop.className "swt:space-y-4"
            prop.children [
                for output in outputs do
                    match output with
                    | DataNode data -> DataMetadata.DataView(data, mutate)
                    | SampleNode _ -> ()
            ]
        ]

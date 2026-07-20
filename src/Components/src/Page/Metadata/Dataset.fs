namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
open ProcessCore
open Swate.Components.Shared
open Swate.Components.Primitive.LayoutComponents
open Swate.Components.Page.ObjectBrowser.Types
open Swate.Components.Page.Metadata.FormComponents

[<Erase; Mangle(false)>]
type DatasetMetadata =

    [<ReactComponent(true)>]
    static member DatasetView
        (dataset: ProcessCore.Dataset, mutate: (ARC -> unit) -> unit, ?onNavigate: ProcessCoreEntityValue -> unit)
        =

        let navigate = defaultArg onNavigate ignore

        let rec containsDataset (target: ProcessCore.Dataset) (candidate: ProcessCore.Dataset) =
            obj.ReferenceEquals(target, candidate)
            || (candidate.HasPart |> Seq.exists (containsDataset target))

        let importableDatasets (catalog: ImportCatalogContext.ImportCatalog) =
            catalog.Datasets |> Array.filter (containsDataset dataset >> not)

        LayoutComponents.Section [
            LayoutComponents.BoxedField(
                "Dataset Metadata",
                content = [
                    TextInput.TextInput(
                        dataset.Identifier,
                        (fun value -> mutate (fun _ -> dataset.Identifier <- value)),
                        label = "Identifier",
                        // ProcessCore hotfix: prevent clearing this mandatory primary field.
                        validator = Swate.Components.ProcessCoreHotfixes.required "Identifier"
                    )
                    TextInput.TextInput(
                        dataset.Title |> Option.defaultValue "",
                        (fun value ->
                            mutate (fun _ -> dataset.Title <- Option.whereNot System.String.IsNullOrWhiteSpace value)
                        ),
                        label = "Title"
                    )
                    TextInput.TextInput(
                        dataset.Description |> Option.defaultValue "",
                        (fun value ->
                            mutate (fun _ ->
                                dataset.Description <- Option.whereNot System.String.IsNullOrWhiteSpace value
                            )
                        ),
                        label = "Description",
                        isArea = true
                    )
                    TextInput.TextInput(
                        dataset.AdditionalType |> Option.defaultValue "",
                        (fun value ->
                            mutate (fun _ ->
                                dataset.AdditionalType <- Option.whereNot System.String.IsNullOrWhiteSpace value
                            )
                        ),
                        label = "Additional Type"
                    )
                    TextInput.TextInput(
                        dataset.License |> Option.defaultValue "",
                        (fun value ->
                            mutate (fun _ -> dataset.License <- Option.whereNot System.String.IsNullOrWhiteSpace value)
                        ),
                        label = "License"
                    )
                    DateTimeInput.DateTimeInput(
                        dataset.DatePublished |> Option.defaultValue "",
                        (fun value ->
                            mutate (fun _ ->
                                dataset.DatePublished <- Option.whereNot System.String.IsNullOrWhiteSpace value
                            )
                        ),
                        label = "Date Published"
                    )
                    DateTimeInput.DateTimeInput(
                        dataset.DateCreated |> Option.defaultValue "",
                        (fun value ->
                            mutate (fun _ ->
                                dataset.DateCreated <- Option.whereNot System.String.IsNullOrWhiteSpace value
                            )
                        ),
                        label = "Date Created"
                    )
                    DateTimeInput.DateTimeInput(
                        dataset.DateModified |> Option.defaultValue "",
                        (fun value ->
                            mutate (fun _ ->
                                dataset.DateModified <- Option.whereNot System.String.IsNullOrWhiteSpace value
                            )
                        ),
                        label = "Date Modified"
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        dataset.Processes,
                        (fun () -> ProcessCore.Process("New Process")),
                        ignore,
                        "Processes",
                        (fun item ->
                            "swt:iconify-color swt:fluent-color--arrow-clockwise-dashes-settings-20",
                            NestedMetadataInput.nonEmptyOr "Unnamed process" item.Name
                        ),
                        (ProcessCoreEntityValue.Process >> navigate),
                        imports = (fun catalog -> catalog.Processes),
                        addItem = (fun item -> mutate (fun _ -> dataset.AddProcess item)),
                        removeItem = (fun item -> mutate (fun _ -> dataset.RemoveProcess item))
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        dataset.HasPart,
                        (fun () -> ProcessCore.Dataset(System.Guid.NewGuid().ToString())),
                        ignore,
                        "Has Part",
                        (fun item ->
                            "swt:iconify-color swt:fluent-color--database-20",
                            NestedMetadataInput.optionOr
                                (NestedMetadataInput.nonEmptyOr "Unnamed dataset" item.Identifier)
                                item.Title
                        ),
                        (ProcessCoreEntityValue.Dataset >> navigate),
                        imports = importableDatasets,
                        addItem = (fun item -> mutate (fun _ -> dataset.AddPart item)),
                        removeItem = (fun item -> mutate (fun _ -> dataset.RemovePart item))
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        dataset.DataFiles,
                        (fun () -> ProcessCore.Data("New Data")),
                        ignore,
                        "Data Files",
                        NestedMetadataInput.Data,
                        (ProcessCoreEntityValue.Data >> navigate),
                        imports = (fun catalog -> catalog.Data),
                        addItem = (fun item -> mutate (fun _ -> dataset.AddDataFile item)),
                        removeItem = (fun item -> mutate (fun _ -> dataset.RemoveDataFile item))
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        dataset.Agents,
                        (fun () -> ProcessCore.Agent("New Agent")),
                        ignore,
                        "Agents",
                        NestedMetadataInput.agent,
                        (ProcessCoreEntityValue.Agent >> navigate),
                        imports = (fun catalog -> catalog.Agents),
                        addItem = (fun item -> mutate (fun _ -> dataset.AddAgent item)),
                        removeItem = (fun item -> mutate (fun _ -> dataset.RemoveAgent item))
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        dataset.Citations,
                        (fun () -> ProcessCore.ScholarlyArticle("New Scholarly Article")),
                        ignore,
                        "Citations",
                        (fun item ->
                            "swt:iconify-color swt:fluent-color--document-text-20",
                            NestedMetadataInput.nonEmptyOr "Unnamed scholarly article" item.Headline
                        ),
                        (ProcessCoreEntityValue.ScholarlyArticle >> navigate),
                        imports = (fun catalog -> catalog.ScholarlyArticles),
                        addItem = (fun item -> mutate (fun _ -> dataset.AddCitation item)),
                        removeItem = (fun item -> mutate (fun _ -> dataset.RemoveCitation item))
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        dataset.DataContexts,
                        (fun () -> ProcessCore.DataContext(ProcessCore.Data("New Data"))),
                        ignore,
                        "Data Contexts",
                        (fun item ->
                            "swt:iconify-color swt:fluent-color--content-view-20",
                            NestedMetadataInput.optionOr
                                (NestedMetadataInput.nonEmptyOr "Unnamed data context" item.Data.Name)
                                item.Label
                        ),
                        (ProcessCoreEntityValue.DataContext >> navigate),
                        imports = (fun catalog -> catalog.DataContexts),
                        addItem = (fun item -> mutate (fun _ -> dataset.AddDataContext item)),
                        removeItem = (fun item -> mutate (fun _ -> dataset.RemoveDataContext item))
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        dataset.AdditionalProperty,
                        (fun () -> ProcessCore.Annotation("New Annotation")),
                        ignore,
                        "Additional Properties",
                        NestedMetadataInput.Annotation,
                        (ProcessCoreEntityValue.Annotation >> navigate),
                        imports = (fun catalog -> catalog.Annotations),
                        addItem = (fun item -> mutate (fun _ -> dataset.AddAdditionalProperty item)),
                        removeItem = (fun item -> mutate (fun _ -> dataset.RemoveAdditionalProperty item))
                    )
                ]
            )
        ]

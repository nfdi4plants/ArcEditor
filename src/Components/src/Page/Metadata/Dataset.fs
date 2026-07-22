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

        let rec rootDataset (current: ProcessCore.Dataset) =
            current.PartOf |> Option.map rootDataset |> Option.defaultValue current

        let root = rootDataset dataset

        let importableDatasets (_catalog: ImportCatalogContext.ImportCatalog) =
            root.HasPart |> Seq.filter (containsDataset dataset >> not) |> Seq.toArray

        let importableProcesses (_catalog: ImportCatalogContext.ImportCatalog) = root.Processes |> Seq.toArray

        let dataFiles =
            MetadataRelationship.create mutate dataset.DataFiles dataset.AddDataFile dataset.RemoveDataFile

        let agents =
            MetadataRelationship.create mutate dataset.Agents dataset.AddAgent dataset.RemoveAgent

        let citations =
            MetadataRelationship.create mutate dataset.Citations dataset.AddCitation dataset.RemoveCitation

        let dataContexts =
            MetadataRelationship.create mutate dataset.DataContexts dataset.AddDataContext dataset.RemoveDataContext

        let additionalProperties =
            MetadataRelationship.create
                mutate
                dataset.AdditionalProperty
                dataset.AddAdditionalProperty
                dataset.RemoveAdditionalProperty

        let processOrder =
            MetadataRelationship.create mutate dataset.Processes dataset.AddProcess dataset.RemoveProcess

        let datasetOrder =
            MetadataRelationship.create mutate dataset.HasPart dataset.AddPart dataset.RemovePart

        let addProcess (processObject: ProcessCore.Process) =
            mutate (fun _ ->
                match processObject.ProcessOf with
                | Some owner when not (obj.ReferenceEquals(owner, dataset)) -> owner.RemoveProcess processObject
                | _ -> ()

                dataset.AddProcess processObject
            )

        let addDataset (child: ProcessCore.Dataset) =
            mutate (fun _ ->
                child.PartOf |> Option.iter (fun owner -> owner.RemovePart child)
                dataset.AddPart child
            )

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
                        "Processes",
                        (fun item ->
                            "swt:iconify-color swt:fluent-color--arrow-clockwise-dashes-settings-20",
                            NestedMetadataInput.nonEmptyOr "Unnamed process" item.Name
                        ),
                        (ProcessCoreEntityValue.Process >> navigate),
                        imports = importableProcesses,
                        duplicateCandidates = (fun catalog -> catalog.Processes),
                        addItem = addProcess,
                        removeItem = processOrder.Remove
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        dataset.HasPart,
                        (fun () -> ProcessCore.Dataset(System.Guid.NewGuid().ToString())),
                        "Has Part",
                        (fun item ->
                            "swt:iconify-color swt:fluent-color--database-20",
                            NestedMetadataInput.optionOr
                                (NestedMetadataInput.nonEmptyOr "Unnamed dataset" item.Identifier)
                                item.Title
                        ),
                        (ProcessCoreEntityValue.Dataset >> navigate),
                        imports = importableDatasets,
                        duplicateCandidates = (fun catalog -> catalog.Datasets),
                        addItem = addDataset,
                        removeItem = datasetOrder.Remove
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        dataset.DataFiles,
                        (fun () -> ProcessCore.Data("New Data")),
                        "Data Files",
                        NestedMetadataInput.Data,
                        (ProcessCoreEntityValue.Data >> navigate),
                        imports = (fun catalog -> catalog.Data),
                        duplicateCandidates = (fun catalog -> catalog.Data),
                        addItem = dataFiles.Add,
                        removeItem = dataFiles.Remove
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        dataset.Agents,
                        (fun () -> ProcessCore.Agent("New Agent")),
                        "Agents",
                        NestedMetadataInput.agent,
                        (ProcessCoreEntityValue.Agent >> navigate),
                        imports = (fun catalog -> catalog.Agents),
                        duplicateCandidates = (fun catalog -> catalog.Agents),
                        addItem = agents.Add,
                        removeItem = agents.Remove
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        dataset.Citations,
                        (fun () -> ProcessCore.ScholarlyArticle("New Scholarly Article")),
                        "Citations",
                        (fun item ->
                            "swt:iconify-color swt:fluent-color--document-text-20",
                            NestedMetadataInput.nonEmptyOr "Unnamed scholarly article" item.Headline
                        ),
                        (ProcessCoreEntityValue.ScholarlyArticle >> navigate),
                        imports = (fun catalog -> catalog.ScholarlyArticles),
                        duplicateCandidates = (fun catalog -> catalog.ScholarlyArticles),
                        addItem = citations.Add,
                        removeItem = citations.Remove
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        dataset.DataContexts,
                        (fun () -> ProcessCore.DataContext(ProcessCore.Data("New Data"))),
                        "Data Contexts",
                        (fun item ->
                            "swt:iconify-color swt:fluent-color--content-view-20",
                            NestedMetadataInput.optionOr
                                (NestedMetadataInput.nonEmptyOr "Unnamed data context" item.Data.Name)
                                item.Label
                        ),
                        (ProcessCoreEntityValue.DataContext >> navigate),
                        imports = (fun catalog -> catalog.DataContexts),
                        duplicateCandidates = (fun catalog -> catalog.DataContexts),
                        addItem = dataContexts.Add,
                        removeItem = dataContexts.Remove
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        dataset.AdditionalProperty,
                        (fun () -> ProcessCore.Annotation("New Annotation")),
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

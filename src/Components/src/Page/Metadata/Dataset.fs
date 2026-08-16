namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
open ProcessCore
open Swate.Components.Shared
open Swate.Components.ProcessCore
open Swate.Components.Primitive.LayoutComponents
open Swate.Components.Page.ObjectBrowser.Types
open Swate.Components.Page.Metadata.FormComponents

[<Erase; Mangle(false)>]
type DatasetMetadata =

    [<ReactComponent(true)>]
    static member DatasetView
        (
            dataset: ProcessCore.Dataset,
            arcView: Swate.Components.ProcessCore.Types.ArcView,
            mutate: (ARC -> unit) -> unit,
            ?onNavigate: ProcessCoreEntityValue -> unit
        ) =

        let navigate = defaultArg onNavigate ignore

        let root = EntityCatalog.rootDataset dataset

        let processes =
            RendererModel.forDataset dataset arcView
            |> Array.map _.Representative
            |> ResizeArray

        let createRelationshipMutations items add remove =
            MetadataRelationship.create mutate items add remove

        let dataFiles =
            createRelationshipMutations dataset.DataFiles dataset.AddDataFile dataset.RemoveDataFile

        let agents =
            createRelationshipMutations dataset.Agents dataset.AddAgent dataset.RemoveAgent

        let citations =
            createRelationshipMutations dataset.Citations dataset.AddCitation dataset.RemoveCitation

        let dataContexts =
            createRelationshipMutations dataset.DataContexts dataset.AddDataContext dataset.RemoveDataContext

        let additionalProperties =
            createRelationshipMutations
                dataset.AdditionalProperty
                dataset.AddAdditionalProperty
                dataset.RemoveAdditionalProperty

        let datasetOrder =
            createRelationshipMutations dataset.HasPart dataset.AddPart dataset.RemovePart

        LayoutComponents.Section(
            [
                LayoutComponents.BoxedField(
                    "Dataset Metadata",
                    content = [
                        TextInput.TextInput(
                            dataset.Identifier,
                            (fun value -> mutate (fun _ -> dataset.Identifier <- value)),
                            label = "Identifier",
                            // ProcessCore hotfix: prevent clearing this mandatory primary field.
                            validator = Swate.Components.ProcessCore.Hotfixes.required "Identifier"
                        )
                        TextInput.TextInput(
                            dataset.Title |> Option.defaultValue "",
                            (fun value ->
                                mutate (fun _ ->
                                    dataset.Title <- Option.whereNot System.String.IsNullOrWhiteSpace value
                                )
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
                                mutate (fun _ ->
                                    dataset.License <- Option.whereNot System.String.IsNullOrWhiteSpace value
                                )
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
                        CollectionCollapse.Main(
                            "Processes",
                            "Logical processes contained in this dataset",
                            processes.Count,
                            NestedMetadataInput.CreatePCInputSequence(
                                processes,
                                (fun () -> ProcessCore.Process("New Process")),
                                "Processes",
                                (fun item ->
                                    Icons.processIcon, NestedMetadataInput.nonEmptyOr "Unnamed process" item.Name
                                ),
                                (ProcessCoreEntityValue.Process >> navigate),
                                imports = (fun _ -> RendererModel.forDataset root arcView |> Array.map _.Representative),
                                duplicateCandidates = (fun catalog -> catalog.Processes),
                                showLabel = false,
                                stickyFooter = true,
                                addItem =
                                    (fun processObject ->
                                        mutate (fun _ -> RendererModel.moveProcess dataset processObject arcView)
                                    ),
                                removeItem =
                                    (fun processObject ->
                                        mutate (fun _ -> RendererModel.removeProcess processObject arcView)
                                    )
                            ),
                            iconClass = Icons.processIcon
                        )
                        NestedMetadataInput.CreatePCInputSequence(
                            dataset.HasPart,
                            (fun () -> ProcessCore.Dataset(System.Guid.NewGuid().ToString())),
                            "Has Part",
                            (fun item ->
                                Icons.datasetIcon,
                                NestedMetadataInput.optionOr
                                    (NestedMetadataInput.nonEmptyOr "Unnamed dataset" item.Identifier)
                                    item.Title
                            ),
                            (ProcessCoreEntityValue.Dataset >> navigate),
                            imports =
                                (fun catalog ->
                                    ignore catalog

                                    root.HasPart
                                    |> Seq.filter (EntityCatalog.containsDataset dataset >> not)
                                    |> Seq.toArray
                                ),
                            duplicateCandidates = (fun catalog -> catalog.Datasets),
                            addItem =
                                (fun child ->
                                    mutate (fun _ ->
                                        child.PartOf |> Option.iter (fun owner -> owner.RemovePart child)
                                        dataset.AddPart child
                                    )
                                ),
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
                                Icons.scholarlyArticleIcon,
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
                                Icons.dataContextIcon,
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
            ],
            overflowVisible = true
        )

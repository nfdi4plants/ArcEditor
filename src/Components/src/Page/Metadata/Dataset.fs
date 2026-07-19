namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
open Swate.Components.Primitive.LayoutComponents
open Swate.Components.Page.ObjectBrowser.Types
open Swate.Components.Page.Metadata.FormComponents
open ProcessCore
open Swate.Components.Shared

module private DatasetMetadataTypes =
    type DatasetChildren = {
        Processes: ResizeArray<ProcessCore.Process>
        Parts: ResizeArray<ProcessCore.Dataset>
        DataFiles: ResizeArray<ProcessCore.Data>
        Agents: ResizeArray<ProcessCore.Agent>
        Citations: ResizeArray<ProcessCore.ScholarlyArticle>
        DataContexts: ResizeArray<ProcessCore.DataContext>
        Properties: ResizeArray<ProcessCore.Annotation>
    }

open DatasetMetadataTypes

[<Erase; Mangle(false)>]
type DatasetMetadata =

    [<ReactComponent(true)>]
    static member DatasetView
        (
            dataset: ProcessCore.Dataset,
            setDataset: ProcessCore.Dataset -> unit,
            ?onNavigate: ProcessCoreEntityValue -> unit
        ) =

        let navigate = defaultArg onNavigate ignore

        let rec containsDataset (target: ProcessCore.Dataset) (candidate: ProcessCore.Dataset) =
            obj.ReferenceEquals(target, candidate)
            || (candidate.HasPart |> Seq.exists (containsDataset target))

        let importableDatasets (catalog: ImportCatalogContext.ImportCatalog) =
            catalog.Datasets |> Array.filter (containsDataset dataset >> not)

        // let copyDataset
        //     (processes: ResizeArray<ProcessCore.Process>)
        //     (parts: ResizeArray<ProcessCore.Dataset>)
        //     (dataFiles: ResizeArray<ProcessCore.Data>)
        //     (agents: ResizeArray<ProcessCore.Agent>)
        //     (citations: ResizeArray<ProcessCore.ScholarlyArticle>)
        //     (dataContexts: ResizeArray<ProcessCore.DataContext>)
        //     (additionalProperties: ResizeArray<ProcessCore.Annotation>)
        //     =
        //     let requestedProcesses = ResizeArray processes
        //     let requestedParts = ResizeArray parts
        //     let requestedDataFiles = ResizeArray dataFiles
        //     let requestedAgents = ResizeArray agents
        //     let requestedCitations = ResizeArray citations
        //     let requestedDataContexts = ResizeArray dataContexts
        //     let requestedAdditionalProperties = ResizeArray additionalProperties

        //     // ProcessCore relationships carry parent back-references. Detach them from
        //     // the old dataset before attaching the requested collection to its copy.
        //     dataset.Processes |> Seq.toArray |> Array.iter dataset.RemoveProcess
        //     dataset.HasPart |> Seq.toArray |> Array.iter dataset.RemovePart

        //     ProcessCore.Dataset(
        //         dataset.Identifier,
        //         ?title = dataset.Title,
        //         ?description = dataset.Description,
        //         ?additionalType = dataset.AdditionalType,
        //         ?license = dataset.License,
        //         ?datePublished = dataset.DatePublished,
        //         ?dateCreated = dataset.DateCreated,
        //         ?dateModified = dataset.DateModified,
        //         processes = requestedProcesses,
        //         hasPart = requestedParts,
        //         dataFiles = requestedDataFiles,
        //         agents = requestedAgents,
        //         citations = requestedCitations,
        //         dataContexts = requestedDataContexts,
        //         additionalProperty = requestedAdditionalProperties
        //     )

        // let updateDataset (updateFn: ProcessCore.Dataset -> ProcessCore.Dataset) =
        //     let copy =
        //         copyDataset
        //             dataset.Processes
        //             dataset.HasPart
        //             dataset.DataFiles
        //             dataset.Agents
        //             dataset.Citations
        //             dataset.DataContexts
        //             dataset.AdditionalProperty

        //     let updateDataset = updateFn copy
        //     setDataset updateDataset

        // let children = {
        //     Processes = dataset.Processes
        //     Parts = dataset.HasPart
        //     DataFiles = dataset.DataFiles
        //     Agents = dataset.Agents
        //     Citations = dataset.Citations
        //     DataContexts = dataset.DataContexts
        //     Properties = dataset.AdditionalProperty
        // }

        // let setChildren children =
        //     copyDataset
        //         children.Processes
        //         children.Parts
        //         children.DataFiles
        //         children.Agents
        //         children.Citations
        //         children.DataContexts
        //         children.Properties
        //     |> setDataset

        LayoutComponents.Section [
            LayoutComponents.BoxedField(
                "Dataset Metadata",
                content = [
                    TextInput.TextInput(
                        dataset.Identifier,
                        (fun value -> dataset.Copy(identifier = value) |> setDataset),
                        label = "Identifier",
                        // ProcessCore hotfix: prevent clearing this mandatory primary field.
                        validator = Swate.Components.ProcessCoreHotfixes.required "Identifier"
                    )
                    TextInput.TextInput(
                        dataset.Title |> Option.defaultValue "",
                        (fun value ->
                            dataset.Copy(title = Option.whereNot System.String.IsNullOrWhiteSpace value)
                            |> setDataset
                        ),
                        label = "Title"
                    )
                    TextInput.TextInput(
                        dataset.Description |> Option.defaultValue "",
                        (fun value ->
                            dataset.Copy(description = Option.whereNot System.String.IsNullOrWhiteSpace value)
                            |> setDataset
                        ),
                        label = "Description",
                        isArea = true
                    )
                    TextInput.TextInput(
                        dataset.AdditionalType |> Option.defaultValue "",
                        (fun value ->
                            dataset.Copy(additionalType = Option.whereNot System.String.IsNullOrWhiteSpace value)
                            |> setDataset
                        ),
                        label = "Additional Type"
                    )
                    TextInput.TextInput(
                        dataset.License |> Option.defaultValue "",
                        (fun value ->
                            dataset.Copy(license = Option.whereNot System.String.IsNullOrWhiteSpace value)
                            |> setDataset
                        ),
                        label = "License"
                    )
                    DateTimeInput.DateTimeInput(
                        dataset.DatePublished |> Option.defaultValue "",
                        (fun value ->
                            dataset.Copy(datePublished = Option.whereNot System.String.IsNullOrWhiteSpace value)
                            |> setDataset
                        ),
                        label = "Date Published"
                    )
                    DateTimeInput.DateTimeInput(
                        dataset.DateCreated |> Option.defaultValue "",
                        (fun value ->
                            dataset.Copy(dateCreated = Option.whereNot System.String.IsNullOrWhiteSpace value)
                            |> setDataset
                        ),
                        label = "Date Created"
                    )
                    DateTimeInput.DateTimeInput(
                        dataset.DateModified |> Option.defaultValue "",
                        (fun value ->
                            dataset.Copy(dateModified = Option.whereNot System.String.IsNullOrWhiteSpace value)
                            |> setDataset
                        ),
                        label = "Date Modified"
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        (ResizeArray dataset.Processes),
                        (fun () -> ProcessCore.Process("New Process")),
                        (fun processes -> dataset.Copy(processes = processes) |> setDataset),
                        "Processes",
                        (fun item ->
                            "swt:iconify-color swt:fluent-color--arrow-clockwise-dashes-settings-20",
                            NestedMetadataInput.nonEmptyOr "Unnamed process" item.Name
                        ),
                        (ProcessCoreEntityValue.Process >> navigate),
                        imports = (fun catalog -> catalog.Processes)
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        (ResizeArray dataset.HasPart),
                        (fun () -> ProcessCore.Dataset(System.Guid.NewGuid().ToString())),
                        (fun parts -> dataset.Copy(hasPart = parts) |> setDataset),
                        "Has Part",
                        (fun item ->
                            "swt:iconify-color swt:fluent-color--database-20",
                            NestedMetadataInput.optionOr
                                (NestedMetadataInput.nonEmptyOr "Unnamed dataset" item.Identifier)
                                item.Title
                        ),
                        (ProcessCoreEntityValue.Dataset >> navigate),
                        imports = importableDatasets
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        (ResizeArray dataset.DataFiles),
                        (fun () -> ProcessCore.Data("New Data")),
                        (fun dataFiles -> dataset.Copy(dataFiles = dataFiles) |> setDataset),
                        "Data Files",
                        NestedMetadataInput.Data,
                        (ProcessCoreEntityValue.Data >> navigate),
                        imports = (fun catalog -> catalog.Data)
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        (ResizeArray dataset.Agents),
                        (fun () -> ProcessCore.Agent("New Agent")),
                        (fun agents -> dataset.Copy(agents = agents) |> setDataset),
                        "Agents",
                        NestedMetadataInput.agent,
                        (ProcessCoreEntityValue.Agent >> navigate),
                        imports = (fun catalog -> catalog.Agents)
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        (ResizeArray dataset.Citations),
                        (fun () -> ProcessCore.ScholarlyArticle("New Scholarly Article")),
                        (fun citations -> dataset.Copy(citations = citations) |> setDataset),
                        "Citations",
                        (fun item ->
                            "swt:iconify-color swt:fluent-color--document-text-20",
                            NestedMetadataInput.nonEmptyOr "Unnamed scholarly article" item.Headline
                        ),
                        (ProcessCoreEntityValue.ScholarlyArticle >> navigate),
                        imports = (fun catalog -> catalog.ScholarlyArticles)
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        (ResizeArray dataset.DataContexts),
                        (fun () -> ProcessCore.DataContext(ProcessCore.Data("New Data"))),
                        (fun dataContexts -> dataset.Copy(dataContexts = dataContexts) |> setDataset),
                        "Data Contexts",
                        (fun item ->
                            "swt:iconify-color swt:fluent-color--content-view-20",
                            NestedMetadataInput.optionOr
                                (NestedMetadataInput.nonEmptyOr "Unnamed data context" item.Data.Name)
                                item.Label
                        ),
                        (ProcessCoreEntityValue.DataContext >> navigate),
                        imports = (fun catalog -> catalog.DataContexts)
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        (ResizeArray dataset.AdditionalProperty),
                        (fun () -> ProcessCore.Annotation("New Annotation")),
                        (fun properties -> dataset.Copy(additionalProperty = properties) |> setDataset),
                        "Additional Properties",
                        NestedMetadataInput.Annotation,
                        (ProcessCoreEntityValue.Annotation >> navigate),
                        imports = (fun catalog -> catalog.Annotations)
                    )
                ]
            )
        ]

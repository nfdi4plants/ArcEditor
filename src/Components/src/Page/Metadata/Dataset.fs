namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
open Swate.Components.Primitive.LayoutComponents
open Swate.Components.Page.ObjectBrowser.Types
open Swate.Components.Page.Metadata.FormComponents

type private DatasetChildren = {
    processes: ResizeArray<ProcessCore.Process>
    parts: ResizeArray<ProcessCore.Dataset>
    dataFiles: ResizeArray<ProcessCore.Data>
    agents: ResizeArray<ProcessCore.Agent>
    citations: ResizeArray<ProcessCore.ScholarlyArticle>
    dataContexts: ResizeArray<ProcessCore.DataContext>
    properties: ResizeArray<ProcessCore.Annotation>
}

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

        let copyDataset
            (processes: ResizeArray<ProcessCore.Process>)
            (parts: ResizeArray<ProcessCore.Dataset>)
            (dataFiles: ResizeArray<ProcessCore.Data>)
            (agents: ResizeArray<ProcessCore.Agent>)
            (citations: ResizeArray<ProcessCore.ScholarlyArticle>)
            (dataContexts: ResizeArray<ProcessCore.DataContext>)
            (additionalProperties: ResizeArray<ProcessCore.Annotation>)
            =
            let requestedProcesses = ResizeArray processes
            let requestedParts = ResizeArray parts
            let requestedDataFiles = ResizeArray dataFiles
            let requestedAgents = ResizeArray agents
            let requestedCitations = ResizeArray citations
            let requestedDataContexts = ResizeArray dataContexts
            let requestedAdditionalProperties = ResizeArray additionalProperties

            // ProcessCore relationships carry parent back-references. Detach them from
            // the old dataset before attaching the requested collection to its copy.
            dataset.Processes |> Seq.toArray |> Array.iter dataset.RemoveProcess
            dataset.HasPart |> Seq.toArray |> Array.iter dataset.RemovePart

            ProcessCore.Dataset(
                dataset.Identifier,
                ?title = dataset.Title,
                ?description = dataset.Description,
                ?additionalType = dataset.AdditionalType,
                ?license = dataset.License,
                ?datePublished = dataset.DatePublished,
                ?dateCreated = dataset.DateCreated,
                ?dateModified = dataset.DateModified,
                processes = requestedProcesses,
                hasPart = requestedParts,
                dataFiles = requestedDataFiles,
                agents = requestedAgents,
                citations = requestedCitations,
                dataContexts = requestedDataContexts,
                additionalProperty = requestedAdditionalProperties
            )

        let updateDataset (updateFn: ProcessCore.Dataset -> ProcessCore.Dataset) =
            let copy =
                copyDataset
                    dataset.Processes
                    dataset.HasPart
                    dataset.DataFiles
                    dataset.Agents
                    dataset.Citations
                    dataset.DataContexts
                    dataset.AdditionalProperty

            let updateDataset = updateFn copy
            setDataset updateDataset

        let children = {
            processes = dataset.Processes
            parts = dataset.HasPart
            dataFiles = dataset.DataFiles
            agents = dataset.Agents
            citations = dataset.Citations
            dataContexts = dataset.DataContexts
            properties = dataset.AdditionalProperty
        }

        let setChildren children =
            copyDataset
                children.processes
                children.parts
                children.dataFiles
                children.agents
                children.citations
                children.dataContexts
                children.properties
            |> setDataset

        LayoutComponents.Section [
            LayoutComponents.BoxedField(
                "Dataset Metadata",
                content = [
                    TextInput.TextInput(
                        dataset.Identifier,
                        (fun value ->
                            updateDataset (fun ds ->
                                ds.Identifier <- value
                                ds
                            )
                        ),
                        label = "Identifier"
                    )
                    TextInput.TextInput(
                        dataset.Title |> Option.defaultValue "",
                        (fun value ->
                            updateDataset (fun ds ->
                                ds.Title <- Some value
                                ds
                            )
                        ),
                        label = "Title"
                    )
                    TextInput.TextInput(
                        dataset.Description |> Option.defaultValue "",
                        (fun value ->
                            updateDataset (fun ds ->
                                ds.Description <- Some value
                                ds
                            )
                        ),
                        label = "Description",
                        isArea = true
                    )
                    TextInput.TextInput(
                        dataset.AdditionalType |> Option.defaultValue "",
                        (fun value ->
                            updateDataset (fun ds ->
                                ds.AdditionalType <- Some value
                                ds
                            )
                        ),
                        label = "Additional Type"
                    )
                    TextInput.TextInput(
                        dataset.License |> Option.defaultValue "",
                        (fun value ->
                            updateDataset (fun ds ->
                                ds.License <- Some value
                                ds
                            )
                        ),
                        label = "License"
                    )
                    DateTimeInput.DateTimeInput(
                        dataset.DatePublished |> Option.defaultValue "",
                        (fun value ->
                            updateDataset (fun ds ->
                                ds.DatePublished <- Some value
                                ds
                            )
                        ),
                        label = "Date Published"
                    )
                    DateTimeInput.DateTimeInput(
                        dataset.DateCreated |> Option.defaultValue "",
                        (fun value ->
                            updateDataset (fun ds ->
                                ds.DateCreated <- Some value
                                ds
                            )
                        ),
                        label = "Date Created"
                    )
                    DateTimeInput.DateTimeInput(
                        dataset.DateModified |> Option.defaultValue "",
                        (fun value ->
                            updateDataset (fun ds ->
                                ds.DateModified <- Some value
                                ds
                            )
                        ),
                        label = "Date Modified"
                    )
                    NestedMetadataInput.sequence
                        (ResizeArray dataset.Processes)
                        (fun () -> ProcessCore.Process(""))
                        (fun processes -> setChildren { children with processes = processes })
                        "Processes"
                        (fun item ->
                            "swt:iconify-color swt:fluent-color--arrow-clockwise-dashes-settings-20",
                            NestedMetadataInput.nonEmptyOr "Unnamed process" item.Name
                        )
                        (ProcessCoreEntityValue.Process >> navigate)
                    NestedMetadataInput.sequence
                        (ResizeArray dataset.HasPart)
                        (fun () -> ProcessCore.Dataset(""))
                        (fun parts -> setChildren { children with parts = parts })
                        "Has Part"
                        (fun item ->
                            "swt:iconify-color swt:fluent-color--database-20",
                            NestedMetadataInput.optionOr
                                (NestedMetadataInput.nonEmptyOr "Unnamed dataset" item.Identifier)
                                item.Title
                        )
                        (ProcessCoreEntityValue.Dataset >> navigate)
                    NestedMetadataInput.sequence
                        (ResizeArray dataset.DataFiles)
                        (fun () -> ProcessCore.Data(""))
                        (fun dataFiles -> setChildren { children with dataFiles = dataFiles })
                        "Data Files"
                        NestedMetadataInput.data
                        (ProcessCoreEntityValue.Data >> navigate)
                    NestedMetadataInput.sequence
                        (ResizeArray dataset.Agents)
                        (fun () -> ProcessCore.Agent(""))
                        (fun agents -> setChildren { children with agents = agents })
                        "Agents"
                        NestedMetadataInput.agent
                        (ProcessCoreEntityValue.Agent >> navigate)
                    NestedMetadataInput.sequence
                        (ResizeArray dataset.Citations)
                        (fun () -> ProcessCore.ScholarlyArticle(""))
                        (fun citations -> setChildren { children with citations = citations })
                        "Citations"
                        (fun item ->
                            "swt:iconify-color swt:fluent-color--document-text-20",
                            NestedMetadataInput.nonEmptyOr "Unnamed scholarly article" item.Headline
                        )
                        (ProcessCoreEntityValue.ScholarlyArticle >> navigate)
                    NestedMetadataInput.sequence
                        (ResizeArray dataset.DataContexts)
                        (fun () -> ProcessCore.DataContext(ProcessCore.Data("")))
                        (fun dataContexts ->
                            setChildren {
                                children with
                                    dataContexts = dataContexts
                            }
                        )
                        "Data Contexts"
                        (fun item ->
                            "swt:iconify-color swt:fluent-color--content-view-20",
                            NestedMetadataInput.optionOr
                                (NestedMetadataInput.nonEmptyOr "Unnamed data context" item.Data.Name)
                                item.Label
                        )
                        (ProcessCoreEntityValue.DataContext >> navigate)
                    NestedMetadataInput.sequence
                        (ResizeArray dataset.AdditionalProperty)
                        (fun () -> ProcessCore.Annotation(""))
                        (fun properties ->
                            setChildren {
                                children with
                                    properties = properties
                            }
                        )
                        "Additional Properties"
                        NestedMetadataInput.annotation
                        (ProcessCoreEntityValue.Annotation >> navigate)
                ]
            )
        ]

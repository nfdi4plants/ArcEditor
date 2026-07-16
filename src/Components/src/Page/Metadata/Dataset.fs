namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
open Swate.Components.Composite.ProcessCore
open Swate.Components.Primitive.LayoutComponents
open Swate.Components.Page.Metadata.FormComponents

[<Erase; Mangle(false)>]
type DatasetMetadata =

    [<ReactComponent(true)>]
    static member DatasetMetadata
        (
            dataset: ProcessCore.Dataset,
            setDataset: ProcessCore.Dataset -> unit,
            ?onNavigate: ProcessCoreEntityValue -> unit
        ) =

        let navigate = defaultArg onNavigate ignore

        let nonEmptyOr fallback (value: string) =
            if System.String.IsNullOrWhiteSpace value then
                fallback
            else
                value

        let optionOr fallback value =
            value |> Option.defaultValue "" |> nonEmptyOr fallback

        let navigationInput
            (icon: string)
            (label: string)
            (navigateTo: unit -> unit)
            (remove: Browser.Types.MouseEvent -> unit)
            =
            Html.div [
                prop.className "swt:flex swt:w-full swt:items-center swt:gap-2"
                prop.children [
                    Html.button [
                        prop.className
                            "swt:btn swt:btn-ghost swt:h-auto swt:min-h-10 swt:flex-1 swt:justify-start swt:px-3"
                        prop.ariaLabel $"Open {label} metadata"
                        prop.onClick (fun _ -> navigateTo ())
                        prop.children [
                            Html.i [ prop.className [ icon; "swt:size-6" ] ]
                            Html.span label
                        ]
                    ]
                    Helpers.deleteButton remove
                ]
            ]

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
                    InputSequence.InputSequence(
                        ResizeArray dataset.Processes,
                        constructor = (fun () -> ProcessCore.Process("")),
                        setter =
                            (fun processes ->
                                copyDataset
                                    processes
                                    dataset.HasPart
                                    dataset.DataFiles
                                    dataset.Agents
                                    dataset.Citations
                                    dataset.DataContexts
                                    dataset.AdditionalProperty
                                |> setDataset
                            ),
                        inputComponent =
                            (fun (processObject, _, remove) ->
                                let label = nonEmptyOr "Unnamed process" processObject.Name

                                navigationInput
                                    "swt:iconify-color swt:fluent-color--arrow-clockwise-dashes-settings-20"
                                    label
                                    (fun () -> navigate (ProcessCoreEntityValue.Process processObject))
                                    remove
                            ),
                        label = "Processes"
                    )
                    InputSequence.InputSequence(
                        ResizeArray dataset.HasPart,
                        constructor = (fun () -> ProcessCore.Dataset("")),
                        setter =
                            (fun parts ->
                                copyDataset
                                    dataset.Processes
                                    parts
                                    dataset.DataFiles
                                    dataset.Agents
                                    dataset.Citations
                                    dataset.DataContexts
                                    dataset.AdditionalProperty
                                |> setDataset
                            ),
                        inputComponent =
                            (fun (child, _, remove) ->
                                let label = optionOr (nonEmptyOr "Unnamed dataset" child.Identifier) child.Title

                                navigationInput
                                    "swt:iconify-color swt:fluent-color--database-20"
                                    label
                                    (fun () -> navigate (ProcessCoreEntityValue.Dataset child))
                                    remove
                            ),
                        label = "Has Part"
                    )
                    InputSequence.InputSequence(
                        ResizeArray dataset.DataFiles,
                        constructor = (fun () -> ProcessCore.Data("")),
                        setter =
                            (fun dataFiles ->
                                copyDataset
                                    dataset.Processes
                                    dataset.HasPart
                                    dataFiles
                                    dataset.Agents
                                    dataset.Citations
                                    dataset.DataContexts
                                    dataset.AdditionalProperty
                                |> setDataset
                            ),
                        inputComponent =
                            (fun (data, _, remove) ->
                                let label = nonEmptyOr "Unnamed data" data.Name

                                navigationInput
                                    "swt:iconify-color swt:fluent-color--data-line-20"
                                    label
                                    (fun () -> navigate (ProcessCoreEntityValue.Data data))
                                    remove
                            ),
                        label = "Data Files"
                    )
                    InputSequence.InputSequence(
                        ResizeArray dataset.Agents,
                        constructor = (fun () -> ProcessCore.Agent("")),
                        setter =
                            (fun agents ->
                                copyDataset
                                    dataset.Processes
                                    dataset.HasPart
                                    dataset.DataFiles
                                    agents
                                    dataset.Citations
                                    dataset.DataContexts
                                    dataset.AdditionalProperty
                                |> setDataset
                            ),
                        inputComponent =
                            (fun (agent, _, remove) ->
                                let label =
                                    [
                                        agent.GivenName
                                        agent.FamilyName |> Option.defaultValue ""
                                    ]
                                    |> List.filter (System.String.IsNullOrWhiteSpace >> not)
                                    |> String.concat " "
                                    |> nonEmptyOr "Unnamed agent"

                                navigationInput
                                    "swt:iconify-color swt:fluent-color--person-20"
                                    label
                                    (fun () -> navigate (ProcessCoreEntityValue.Agent agent))
                                    remove
                            ),
                        label = "Agents"
                    )
                    InputSequence.InputSequence(
                        ResizeArray dataset.Citations,
                        constructor = (fun () -> ProcessCore.ScholarlyArticle("")),
                        setter =
                            (fun citations ->
                                copyDataset
                                    dataset.Processes
                                    dataset.HasPart
                                    dataset.DataFiles
                                    dataset.Agents
                                    citations
                                    dataset.DataContexts
                                    dataset.AdditionalProperty
                                |> setDataset
                            ),
                        inputComponent =
                            (fun (article, _, remove) ->
                                let label = nonEmptyOr "Unnamed scholarly article" article.Headline

                                navigationInput
                                    "swt:iconify-color swt:fluent-color--document-text-20"
                                    label
                                    (fun () -> navigate (ProcessCoreEntityValue.ScholarlyArticle article))
                                    remove
                            ),
                        label = "Citations"
                    )
                    InputSequence.InputSequence(
                        ResizeArray dataset.DataContexts,
                        constructor = (fun () -> ProcessCore.DataContext(ProcessCore.Data(""))),
                        setter =
                            (fun dataContexts ->
                                copyDataset
                                    dataset.Processes
                                    dataset.HasPart
                                    dataset.DataFiles
                                    dataset.Agents
                                    dataset.Citations
                                    dataContexts
                                    dataset.AdditionalProperty
                                |> setDataset
                            ),
                        inputComponent =
                            (fun (dataContext, _, remove) ->
                                let label =
                                    optionOr
                                        (nonEmptyOr "Unnamed data context" dataContext.Data.Name)
                                        dataContext.Label

                                navigationInput
                                    "swt:iconify-color swt:fluent-color--content-view-20"
                                    label
                                    (fun () -> navigate (ProcessCoreEntityValue.DataContext dataContext))
                                    remove
                            ),
                        label = "Data Contexts"
                    )
                    InputSequence.InputSequence(
                        ResizeArray dataset.AdditionalProperty,
                        constructor = (fun () -> ProcessCore.Annotation("")),
                        setter =
                            (fun additionalProperties ->
                                copyDataset
                                    dataset.Processes
                                    dataset.HasPart
                                    dataset.DataFiles
                                    dataset.Agents
                                    dataset.Citations
                                    dataset.DataContexts
                                    additionalProperties
                                |> setDataset
                            ),
                        inputComponent =
                            (fun (annotation, _, remove) ->
                                let label = nonEmptyOr "Unnamed annotation" annotation.Name

                                navigationInput
                                    "swt:iconify-color swt:fluent-color--comment-multiple-20"
                                    label
                                    (fun () -> navigate (ProcessCoreEntityValue.Annotation annotation))
                                    remove
                            ),
                        label = "Additional Properties"
                    )
                ]
            )
        ]

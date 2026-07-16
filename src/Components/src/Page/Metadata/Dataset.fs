namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
open Swate.Components.Primitive.LayoutComponents
open Swate.Components.Page.Metadata.FormComponents

[<Erase; Mangle(false)>]
type DatasetMetadata =

    [<ReactComponent(true)>]
    static member DatasetMetadata(dataset: ProcessCore.Dataset, setDataset: ProcessCore.Dataset -> unit) =

        let updateDataset (updateFn: ProcessCore.Dataset -> ProcessCore.Dataset) =
            let copy =
                ProcessCore.Dataset(
                    dataset.Identifier,
                    ?title = dataset.Title,
                    ?description = dataset.Description,
                    ?additionalType = dataset.AdditionalType,
                    ?license = dataset.License,
                    ?datePublished = dataset.DatePublished,
                    ?dateCreated = dataset.DateCreated,
                    ?dateModified = dataset.DateModified,
                    processes = dataset.Processes,
                    hasPart = dataset.HasPart,
                    dataFiles = dataset.DataFiles,
                    agents = dataset.Agents,
                    citations = dataset.Citations,
                    dataContexts = dataset.DataContexts,
                    additionalProperty = dataset.AdditionalProperty
                )

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
                    // TODO Processes is a Process seq, which is a complex type. We need a way to select or create a Process.
                    Html.div
                        "Placeholder for Processes (Process seq) input. This should be a dropdown or a search field to select an existing Process or create a new one."
                    FormComponents.TextInput.TextInput(
                        dataset.Identifier,
                        (fun input ->
                            updateDataset (fun updatedDataset ->
                                updatedDataset.Identifier <- input
                                updatedDataset
                            )
                        ),
                        label = "Processes"
                    )
                    // TODO HasPart is a Dataset seq, which is a complex type. We need a way to select or create a Dataset.
                    Html.div
                        "Placeholder for HasPart (Dataset seq) input. This should be a dropdown or a search field to select an existing Dataset or create a new one."
                    FormComponents.TextInput.TextInput(
                        dataset.Identifier,
                        (fun input ->
                            updateDataset (fun updatedDataset ->
                                updatedDataset.Identifier <- input
                                updatedDataset
                            )
                        ),
                        label = "HasPart"
                    )
                    // TODO DataFiles is a Data seq, which is a complex type. We need a way to select or create a DataFiles.
                    Html.div
                        "Placeholder for DataFiles (Data seq) input. This should be a dropdown or a search field to select an existing Data or create a new one."
                    FormComponents.TextInput.TextInput(
                        dataset.Identifier,
                        (fun input ->
                            updateDataset (fun updatedDataset ->
                                updatedDataset.Identifier <- input
                                updatedDataset
                            )
                        ),
                        label = "DataFiles"
                    )
                    // TODO Agents is an Agent seq, which is a complex type. We need a way to select or create an Agent.
                    Html.div
                        "Placeholder for Agents (Agent seq) input. This should be a dropdown or a search field to select an existing Agent or create a new one."
                    FormComponents.TextInput.TextInput(
                        dataset.Identifier,
                        (fun input ->
                            updateDataset (fun updatedDataset ->
                                updatedDataset.Identifier <- input
                                updatedDataset
                            )
                        ),
                        label = "Agents"
                    )
                    // TODO Citations is an ScholarlyArticle seq, which is a complex type. We need a way to select or create a ScholarlyArticle.
                    Html.div
                        "Placeholder for Citations (ScholarlyArticle seq) input. This should be a dropdown or a search field to select an existing ScholarlyArticle or create a new one."
                    FormComponents.TextInput.TextInput(
                        dataset.Identifier,
                        (fun input ->
                            updateDataset (fun updatedDataset ->
                                updatedDataset.Identifier <- input
                                updatedDataset
                            )
                        ),
                        label = "Citations"
                    )
                    // TODO DataContexts is an DataContext seq, which is a complex type. We need a way to select or create a DataContext.
                    Html.div
                        "Placeholder for DataContexts (DataContext seq) input. This should be a dropdown or a search field to select an existing DataContext or create a new one."
                    FormComponents.TextInput.TextInput(
                        dataset.Identifier,
                        (fun input ->
                            updateDataset (fun updatedDataset ->
                                updatedDataset.Identifier <- input
                                updatedDataset
                            )
                        ),
                        label = "DataContexts"
                    )
                    // TODO AdditionalProperty is an Annotation seq, which is a complex type. We need a way to select or create an Annotation.
                    Html.div
                        "Placeholder for AdditionalProperty (Annotation seq) input. This should be a dropdown or a search field to select an existing Annotation or create a new one."
                    FormComponents.TextInput.TextInput(
                        dataset.Identifier,
                        (fun input ->
                            updateDataset (fun updatedDataset ->
                                updatedDataset.Identifier <- input
                                updatedDataset
                            )
                        ),
                        label = "AdditionalProperty"
                    )
                ]
            )
        ]

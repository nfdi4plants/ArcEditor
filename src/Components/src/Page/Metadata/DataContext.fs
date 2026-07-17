namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
open Swate.Components.Page.Metadata
open Swate.Components.Primitive.LayoutComponents

[<Erase; Mangle(false)>]
type DataContextMetadata =

    [<ReactComponent(true)>]
    static member DataContextMetadata
        (dataContext: ProcessCore.DataContext, setDataContext: ProcessCore.DataContext -> unit)
        =

        let updateDataContext (updateFn: ProcessCore.DataContext -> ProcessCore.DataContext) =
            let copy =
                ProcessCore.DataContext(
                    dataContext.Data,
                    ?explication = dataContext.Explication,
                    ?objectType = dataContext.ObjectType,
                    ?unit = dataContext.Unit,
                    ?label = dataContext.Label,
                    ?description = dataContext.Description,
                    ?generatedBy = dataContext.GeneratedBy
                )

            let updatedDataContext = updateFn copy
            setDataContext updatedDataContext

        LayoutComponents.Section [
            LayoutComponents.BoxedField(
                "Data Context Metadata",
                content = [
                    // TODO Data is a Data, which is a complex type. We need a way to select or create a Data.
                    Html.div
                        "Placeholder for AdditionalProperty (Annotation seq) input. This should be a dropdown or a search field to select an existing Annotation or create a new one."
                    FormComponents.TextInput.TextInput(
                        dataContext.Label |> Option.defaultValue "",
                        (fun input ->
                            updateDataContext (fun updatedDataContext ->
                                updatedDataContext.Label <- Some input
                                updatedDataContext
                            )
                        ),
                        label = "Data"
                    )
                    // TODO Explication is a DefinedTerm, which is a complex type. We need a way to select or create a DefinedTerm.
                    Html.div
                        "Placeholder for Explication (DefinedTerm) input. This should be a dropdown or a search field to select an existing DefinedTerm or create a new one."
                    FormComponents.TextInput.TextInput(
                        dataContext.Label |> Option.defaultValue "",
                        (fun input ->
                            updateDataContext (fun updatedDataContext ->
                                updatedDataContext.Label <- Some input
                                updatedDataContext
                            )
                        ),
                        label = "Explication"
                    )
                    // TODO ObjectType is a DefinedTerm, which is a complex type. We need a way to select or create a DefinedTerm.
                    Html.div
                        "Placeholder for ObjectType (DefinedTerm) input. This should be a dropdown or a search field to select an existing DefinedTerm or create a new one."
                    FormComponents.TextInput.TextInput(
                        dataContext.Label |> Option.defaultValue "",
                        (fun input ->
                            updateDataContext (fun updatedDataContext ->
                                updatedDataContext.Label <- Some input
                                updatedDataContext
                            )
                        ),
                        label = "Object Type"
                    )
                    // TODO Unit is a DefinedTerm, which is a complex type. We need a way to select or create a DefinedTerm.
                    Html.div
                        "Placeholder for Unit (DefinedTerm) input. This should be a dropdown or a search field to select an existing DefinedTerm or create a new one."
                    FormComponents.TextInput.TextInput(
                        dataContext.Label |> Option.defaultValue "",
                        (fun input ->
                            updateDataContext (fun updatedDataContext ->
                                updatedDataContext.Label <- Some input
                                updatedDataContext
                            )
                        ),
                        label = "Unit"
                    )
                    FormComponents.TextInput.TextInput(
                        dataContext.Label |> Option.defaultValue "",
                        (fun value ->
                            updateDataContext (fun updatedDataContext ->
                                updatedDataContext.Label <- Some value
                                updatedDataContext
                            )
                        ),
                        label = "Label"
                    )
                    FormComponents.TextInput.TextInput(
                        dataContext.Description |> Option.defaultValue "",
                        (fun value ->
                            updateDataContext (fun updatedDataContext ->
                                updatedDataContext.Label <- Some value
                                updatedDataContext
                            )
                        ),
                        label = "Description"
                    )
                    FormComponents.TextInput.TextInput(
                        dataContext.GeneratedBy |> Option.defaultValue "",
                        (fun value ->
                            updateDataContext (fun updatedDataContext ->
                                updatedDataContext.Label <- Some value
                                updatedDataContext
                            )
                        ),
                        label = "Generated By"
                    )
                ]
            )
        ]

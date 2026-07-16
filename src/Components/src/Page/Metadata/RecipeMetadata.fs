namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
open Swate.Components.Page.Metadata
open Swate.Components.Primitive.LayoutComponents

[<Erase; Mangle(false)>]
type RecipeMetadata =

    [<ReactComponent(true)>]
    static member RecipeMetadata(recipe: ProcessCore.Recipe, setData: ProcessCore.Recipe -> unit) =

        let updateRecipe (updateFn: ProcessCore.Recipe -> ProcessCore.Recipe) =
            let copy =
                ProcessCore.Recipe(
                    ?name = recipe.Name,
                    ?description = recipe.Description,
                    ?version = recipe.Version,
                    ?url = recipe.Url,
                    ?intendedUse = recipe.IntendedUse,
                    ?additionalType = recipe.AdditionalType,
                    parameters = recipe.Parameters,
                    components = recipe.Components,
                    additionalProperty = recipe.AdditionalProperty
                )

            let updatedData = updateFn copy
            setData updatedData

        LayoutComponents.Section [
            LayoutComponents.BoxedField(
                "Recipe Metadata",
                content = [
                    FormComponents.TextInput.TextInput(
                        recipe.Name |> Option.defaultValue "",
                        (fun value ->
                            updateRecipe (fun updatedRecipe ->
                                updatedRecipe.Name <- Some value
                                updatedRecipe
                            )
                        ),
                        label = "Name"
                    )
                    FormComponents.TextInput.TextInput(
                        recipe.Description |> Option.defaultValue "",
                        (fun value ->
                            updateRecipe (fun updatedRecipe ->
                                updatedRecipe.Description <- Some value
                                updatedRecipe
                            )
                        ),
                        label = "Description"
                    )
                    FormComponents.TextInput.TextInput(
                        recipe.Version |> Option.defaultValue "",
                        (fun value ->
                            updateRecipe (fun updatedRecipe ->
                                updatedRecipe.Version <- Some value
                                updatedRecipe
                            )
                        ),
                        label = "Version"
                    )
                    FormComponents.TextInput.TextInput(
                        recipe.Url |> Option.defaultValue "",
                        (fun value ->
                            updateRecipe (fun updatedRecipe ->
                                updatedRecipe.Url <- Some value
                                updatedRecipe
                            )
                        ),
                        label = "URL"
                    )
                    // TODO IntendedUse is a DefinedTerm, which is a complex type. We need a way to select or create a DefinedTerm.
                    Html.div
                        "Placeholder for IntendedUse (DefinedTerm) input. This should be a dropdown or a search field to select an existing DefinedTerm or create a new one."
                    FormComponents.TextInput.TextInput(
                        recipe.Name |> Option.defaultValue "",
                        (fun input ->
                            updateRecipe (fun updatedRecipe ->
                                updatedRecipe.Name <- Some input
                                updatedRecipe
                            )
                        ),
                        label = "Intended Use"
                    )
                    FormComponents.TextInput.TextInput(
                        recipe.AdditionalType |> Option.defaultValue "",
                        (fun value ->
                            updateRecipe (fun updatedRecipe ->
                                updatedRecipe.AdditionalType <- Some value
                                updatedRecipe
                            )
                        ),
                        label = "Additional Type"
                    )
                    // TODO Parameters is a FormalParameter seq, which is a complex type. We need a way to select or create a FormalParameter.
                    Html.div
                        "Placeholder for Parameters (FormalParameter seq) input. This should be a dropdown or a search field to select an existing DefinedTerm or create a new one."
                    FormComponents.TextInput.TextInput(
                        recipe.Name |> Option.defaultValue "",
                        (fun input ->
                            updateRecipe (fun updatedRecipe ->
                                updatedRecipe.Name <- Some input
                                updatedRecipe
                            )
                        ),
                        label = "Parameters"
                    )
                    // TODO Components is a Annotation seq, which is a complex type. We need a way to select or create an Annotation.
                    Html.div
                        "Placeholder for Components (Annotation seq) input. This should be a dropdown or a search field to select an existing Annotation or create a new one."
                    FormComponents.TextInput.TextInput(
                        recipe.Name |> Option.defaultValue "",
                        (fun input ->
                            updateRecipe (fun updatedRecipe ->
                                updatedRecipe.Name <- Some input
                                updatedRecipe
                            )
                        ),
                        label = "Components"
                    )
                    // TODO AdditionalProperty is an Annotation seq, which is a complex type. We need a way to select or create an Annotation.
                    Html.div
                        "Placeholder for AdditionalProperty (Annotation seq) input. This should be a dropdown or a search field to select an existing Annotation or create a new one."
                    FormComponents.TextInput.TextInput(
                        recipe.Name |> Option.defaultValue "",
                        (fun input ->
                            updateRecipe (fun updatedRecipe ->
                                updatedRecipe.Name <- Some input
                                updatedRecipe
                            )
                        ),
                        label = "Additional Property"
                    )
                ]
            )
        ]

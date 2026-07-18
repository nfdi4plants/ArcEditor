namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
open ProcessCore
open Swate.Components.Page.Metadata
open Swate.Components.Page.ObjectBrowser.Types
open Swate.Components.Primitive.LayoutComponents
open Swate.Components.Page.Metadata.FormComponents

module private RecipeMetadataTypes =
    type RecipeChildren = {
        Parameters: ResizeArray<FormalParameter>
        Components: ResizeArray<Annotation>
        Properties: ResizeArray<Annotation>
    }

open RecipeMetadataTypes

[<Erase; Mangle(false)>]
type RecipeMetadata =

    [<ReactComponent(true)>]
    static member RecipeView
        (recipe: ProcessCore.Recipe, setData: ProcessCore.Recipe -> unit, ?onNavigate: ProcessCoreEntityValue -> unit)
        =

        let navigate = defaultArg onNavigate ignore

        let copyRecipe
            (parameters: ResizeArray<FormalParameter>)
            (components: ResizeArray<Annotation>)
            (additionalProperties: ResizeArray<Annotation>)
            =
            ProcessCore.Recipe(
                ?name = recipe.Name,
                ?description = recipe.Description,
                ?version = recipe.Version,
                ?url = recipe.Url,
                ?intendedUse = recipe.IntendedUse,
                ?additionalType = recipe.AdditionalType,
                parameters = parameters,
                components = components,
                additionalProperty = additionalProperties
            )

        let updateRecipe (updateFn: ProcessCore.Recipe -> ProcessCore.Recipe) =
            let copy = copyRecipe recipe.Parameters recipe.Components recipe.AdditionalProperty

            let updatedData = updateFn copy
            setData updatedData

        let children = {
            Parameters = recipe.Parameters
            Components = recipe.Components
            Properties = recipe.AdditionalProperty
        }

        let setChildren children =
            copyRecipe children.Parameters children.Components children.Properties
            |> setData

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
                    (NestedMetadataInput.OptionalDefinedTerm(
                        "Intended Use",
                        recipe.IntendedUse,
                        (fun intendedUse ->
                            let copy = copyRecipe children.Parameters children.Components children.Properties
                            copy.IntendedUse <- intendedUse
                            setData copy
                        ),
                        (ProcessCoreEntityValue.DefinedTerm >> navigate),
                        imports = (fun catalog -> catalog.DefinedTerms)
                    ))
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
                    NestedMetadataInput.CreatePCInputSequence(
                        (ResizeArray recipe.Parameters),
                        (fun () -> FormalParameter("")),
                        (fun parameters ->
                            setChildren {
                                children with
                                    Parameters = parameters
                            }
                        ),
                        "Parameters",
                        NestedMetadataInput.FormalParameter,
                        (ProcessCoreEntityValue.FormalParameter >> navigate),
                        imports = (fun catalog -> catalog.FormalParameters)
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        (ResizeArray recipe.Components),
                        (fun () -> Annotation("")),
                        (fun components ->
                            setChildren {
                                children with
                                    Components = components
                            }
                        ),
                        "Components",
                        NestedMetadataInput.Annotation,
                        (ProcessCoreEntityValue.Annotation >> navigate),
                        imports = (fun catalog -> catalog.Annotations)
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        (ResizeArray recipe.AdditionalProperty),
                        (fun () -> Annotation("")),
                        (fun properties ->
                            setChildren {
                                children with
                                    Properties = properties
                            }
                        ),
                        "Additional Properties",
                        NestedMetadataInput.Annotation,
                        (ProcessCoreEntityValue.Annotation >> navigate),
                        imports = (fun catalog -> catalog.Annotations)
                    )
                ]
            )
        ]

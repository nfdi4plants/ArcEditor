namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
open ProcessCore
open Swate.Components.Page.Metadata
open Swate.Components.Page.ObjectBrowser.Types
open Swate.Components.Primitive.LayoutComponents
open Swate.Components.Page.Metadata.FormComponents

type private RecipeChildren = {
    parameters: ResizeArray<FormalParameter>
    components: ResizeArray<Annotation>
    properties: ResizeArray<Annotation>
}

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
            parameters = recipe.Parameters
            components = recipe.Components
            properties = recipe.AdditionalProperty
        }

        let setChildren children =
            copyRecipe children.parameters children.components children.properties
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
                    (NestedMetadataInput.optionalDefinedTerm
                        "Intended Use"
                        recipe.IntendedUse
                        (fun intendedUse ->
                            let copy = copyRecipe children.parameters children.components children.properties
                            copy.IntendedUse <- intendedUse
                            setData copy
                        )
                        (ProcessCoreEntityValue.DefinedTerm >> navigate))
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
                    NestedMetadataInput.sequence
                        (ResizeArray recipe.Parameters)
                        (fun () -> FormalParameter(""))
                        (fun parameters ->
                            setChildren {
                                children with
                                    parameters = parameters
                            }
                        )
                        "Parameters"
                        NestedMetadataInput.formalParameter
                        (ProcessCoreEntityValue.FormalParameter >> navigate)
                    NestedMetadataInput.sequence
                        (ResizeArray recipe.Components)
                        (fun () -> Annotation(""))
                        (fun components ->
                            setChildren {
                                children with
                                    components = components
                            }
                        )
                        "Components"
                        NestedMetadataInput.annotation
                        (ProcessCoreEntityValue.Annotation >> navigate)
                    NestedMetadataInput.sequence
                        (ResizeArray recipe.AdditionalProperty)
                        (fun () -> Annotation(""))
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

namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
open ProcessCore
open Swate.Components.Shared
open Swate.Components.Composite.TermSearch.Types
open Swate.Components.Primitive.LayoutComponents
open Swate.Components.Page.ObjectBrowser.Types
open Swate.Components.Page.Metadata.FormComponents

[<Erase; Mangle(false)>]
type RecipeMetadata =

    [<ReactComponent(true)>]
    static member RecipeView
        (recipe: ProcessCore.Recipe, mutate: (ARC -> unit) -> unit, ?onNavigate: ProcessCoreEntityValue -> unit)
        =

        let navigate = defaultArg onNavigate ignore

        let components =
            MetadataRelationship.create mutate recipe.Components recipe.AddComponent recipe.RemoveComponent

        let additionalProperties =
            MetadataRelationship.create
                mutate
                recipe.AdditionalProperty
                recipe.AddAdditionalProperty
                recipe.RemoveAdditionalProperty

        LayoutComponents.Section [
            LayoutComponents.BoxedField(
                "Recipe Metadata",
                content = [
                    FormComponents.TextInput.TextInput(
                        recipe.Name |> Option.defaultValue "",
                        (fun value ->
                            mutate (fun _ -> recipe.Name <- Option.whereNot System.String.IsNullOrWhiteSpace value)
                        ),
                        label = "Name"
                    )
                    FormComponents.TextInput.TextInput(
                        recipe.Description |> Option.defaultValue "",
                        (fun value ->
                            mutate (fun _ ->
                                recipe.Description <- Option.whereNot System.String.IsNullOrWhiteSpace value
                            )
                        ),
                        label = "Description"
                    )
                    FormComponents.TextInput.TextInput(
                        recipe.Version |> Option.defaultValue "",
                        (fun value ->
                            mutate (fun _ -> recipe.Version <- Option.whereNot System.String.IsNullOrWhiteSpace value)
                        ),
                        label = "Version"
                    )
                    FormComponents.TextInput.TextInput(
                        recipe.Url |> Option.defaultValue "",
                        (fun value ->
                            mutate (fun _ -> recipe.Url <- Option.whereNot System.String.IsNullOrWhiteSpace value)
                        ),
                        label = "URL"
                    )
                    (NestedMetadataInput.OptionalDefinedTerm(
                        "Intended Use",
                        recipe.IntendedUse,
                        (fun intendedUse -> mutate (fun _ -> recipe.IntendedUse <- intendedUse)),
                        (ProcessCoreEntityValue.DefinedTerm >> navigate)
                    ))
                    FormComponents.TextInput.TextInput(
                        recipe.AdditionalType |> Option.defaultValue "",
                        (fun value ->
                            mutate (fun _ ->
                                recipe.AdditionalType <- Option.whereNot System.String.IsNullOrWhiteSpace value
                            )
                        ),
                        label = "Additional Type"
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        recipe.Parameters,
                        (fun () -> FormalParameter("New Formal Parameter")),
                        "Parameters",
                        NestedMetadataInput.FormalParameter,
                        (ProcessCoreEntityValue.FormalParameter >> navigate),
                        addItem = (fun item -> mutate (fun _ -> recipe.AddParameter item)),
                        removeItem = (fun item -> mutate (fun _ -> recipe.RemoveParameter item))
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        recipe.Components,
                        (fun () -> Annotation("New Annotation")),
                        "Components",
                        NestedMetadataInput.Annotation,
                        (ProcessCoreEntityValue.Annotation >> navigate),
                        imports = (fun catalog -> catalog.Annotations),
                        duplicateCandidates = (fun catalog -> catalog.Annotations),
                        addItem = components.Add,
                        removeItem = components.Remove
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        recipe.AdditionalProperty,
                        (fun () -> Annotation("New Annotation")),
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

type RecipeMetadata with

    [<ReactComponent>]
    static member Recipes(recipes: ResizeArray<Recipe>, mutate: (ARC -> unit) -> unit) =
        Html.div [
            prop.className "swt:space-y-4"
            prop.children [
                for recipe in recipes do
                    RecipeMetadata.RecipeView(recipe, mutate)
            ]
        ]

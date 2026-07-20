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
                        (ProcessCoreEntityValue.DefinedTerm >> navigate),
                        imports = (fun catalog -> catalog.DefinedTerms)
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
                        ignore,
                        "Parameters",
                        NestedMetadataInput.FormalParameter,
                        (ProcessCoreEntityValue.FormalParameter >> navigate),
                        imports = (fun catalog -> catalog.FormalParameters),
                        addItem = (fun item -> mutate (fun _ -> recipe.AddParameter item)),
                        removeItem = (fun item -> mutate (fun _ -> recipe.RemoveParameter item)),
                        updateItems = (fun items -> FormalParameterMetadata.FormalParameters(items, mutate))
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        recipe.Components,
                        (fun () -> Annotation("New Annotation")),
                        ignore,
                        "Components",
                        NestedMetadataInput.Annotation,
                        (ProcessCoreEntityValue.Annotation >> navigate),
                        imports = (fun catalog -> catalog.Annotations),
                        addItem = (fun item -> mutate (fun _ -> recipe.AddComponent item)),
                        removeItem = (fun item -> mutate (fun _ -> recipe.RemoveComponent item)),
                        updateItems = (fun items -> AnnotationMetadata.Annotations(items, mutate))
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        recipe.AdditionalProperty,
                        (fun () -> Annotation("New Annotation")),
                        ignore,
                        "Additional Properties",
                        NestedMetadataInput.Annotation,
                        (ProcessCoreEntityValue.Annotation >> navigate),
                        imports = (fun catalog -> catalog.Annotations),
                        addItem = (fun item -> mutate (fun _ -> recipe.AddAdditionalProperty item)),
                        removeItem = (fun item -> mutate (fun _ -> recipe.RemoveAdditionalProperty item)),
                        updateItems = (fun items -> AnnotationMetadata.Annotations(items, mutate))
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

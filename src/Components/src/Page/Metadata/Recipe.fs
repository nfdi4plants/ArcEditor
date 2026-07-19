namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
open ProcessCore
open Swate.Components.Page.Metadata
open Swate.Components.Page.ObjectBrowser.Types
open Swate.Components.Primitive.LayoutComponents
open Swate.Components.Page.Metadata.FormComponents
open Swate.Components.Shared

[<Erase; Mangle(false)>]
type RecipeMetadata =

    [<ReactComponent(true)>]
    static member RecipeView
        (recipe: ProcessCore.Recipe, setData: ProcessCore.Recipe -> unit, ?onNavigate: ProcessCoreEntityValue -> unit)
        =

        let navigate = defaultArg onNavigate ignore

        LayoutComponents.Section [
            LayoutComponents.BoxedField(
                "Recipe Metadata",
                content = [
                    FormComponents.TextInput.TextInput(
                        recipe.Name |> Option.defaultValue "",
                        (fun value ->
                            recipe.Copy(name = Option.whereNot System.String.IsNullOrWhiteSpace value)
                            |> setData
                        ),
                        label = "Name"
                    )
                    FormComponents.TextInput.TextInput(
                        recipe.Description |> Option.defaultValue "",
                        (fun value ->
                            recipe.Copy(description = Option.whereNot System.String.IsNullOrWhiteSpace value)
                            |> setData
                        ),
                        label = "Description"
                    )
                    FormComponents.TextInput.TextInput(
                        recipe.Version |> Option.defaultValue "",
                        (fun value ->
                            recipe.Copy(version = Option.whereNot System.String.IsNullOrWhiteSpace value)
                            |> setData
                        ),
                        label = "Version"
                    )
                    FormComponents.TextInput.TextInput(
                        recipe.Url |> Option.defaultValue "",
                        (fun value ->
                            recipe.Copy(url = Option.whereNot System.String.IsNullOrWhiteSpace value)
                            |> setData
                        ),
                        label = "URL"
                    )
                    (NestedMetadataInput.OptionalDefinedTerm(
                        "Intended Use",
                        recipe.IntendedUse,
                        (fun intendedUse -> recipe.Copy(intendedUse = intendedUse) |> setData),
                        (ProcessCoreEntityValue.DefinedTerm >> navigate),
                        imports = (fun catalog -> catalog.DefinedTerms)
                    ))
                    FormComponents.TextInput.TextInput(
                        recipe.AdditionalType |> Option.defaultValue "",
                        (fun value ->
                            recipe.Copy(additionalType = Option.whereNot System.String.IsNullOrWhiteSpace value)
                            |> setData
                        ),
                        label = "Additional Type"
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        (ResizeArray recipe.Parameters),
                        (fun () -> FormalParameter("New Formal Parameter")),
                        (fun parameters -> recipe.Copy(parameters = parameters) |> setData),
                        "Parameters",
                        NestedMetadataInput.FormalParameter,
                        (ProcessCoreEntityValue.FormalParameter >> navigate),
                        imports = (fun catalog -> catalog.FormalParameters)
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        (ResizeArray recipe.Components),
                        (fun () -> Annotation("New Annotation")),
                        (fun components -> recipe.Copy(components = components) |> setData),
                        "Components",
                        NestedMetadataInput.Annotation,
                        (ProcessCoreEntityValue.Annotation >> navigate),
                        imports = (fun catalog -> catalog.Annotations)
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        (ResizeArray recipe.AdditionalProperty),
                        (fun () -> Annotation("New Annotation")),
                        (fun properties -> recipe.Copy(additionalProperty = properties) |> setData),
                        "Additional Properties",
                        NestedMetadataInput.Annotation,
                        (ProcessCoreEntityValue.Annotation >> navigate),
                        imports = (fun catalog -> catalog.Annotations)
                    )
                ]
            )
        ]

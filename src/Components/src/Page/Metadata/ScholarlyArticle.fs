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
type ScholarlyArticleMetadata =

    [<ReactComponent(true)>]
    static member ScholarlyArticleView
        (
            sample: ProcessCore.ScholarlyArticle,
            setSample: ProcessCore.ScholarlyArticle -> unit,
            ?onNavigate: ProcessCoreEntityValue -> unit
        ) =

        let navigate = defaultArg onNavigate ignore

        LayoutComponents.Section [
            LayoutComponents.BoxedField(
                "Scholarly Article Metadata",
                content = [
                    FormComponents.TextInput.TextInput(
                        sample.Headline,
                        (fun value -> sample.Copy(headline = value) |> setSample),
                        label = "Headline",
                        // ProcessCore hotfix: prevent clearing this mandatory primary field.
                        validator = Swate.Components.ProcessCoreHotfixes.required "Headline"
                    )
                    FormComponents.TextInput.TextInput(
                        sample.Id |> Option.defaultValue "",
                        (fun value ->
                            sample.Copy(id = Option.whereNot System.String.IsNullOrWhiteSpace value)
                            |> setSample
                        ),
                        label = "Id"
                    )
                    FormComponents.TextInput.TextInput(
                        sample.Identifier |> Option.defaultValue "",
                        (fun value ->
                            sample.Copy(identifier = Option.whereNot System.String.IsNullOrWhiteSpace value)
                            |> setSample
                        ),
                        label = "Identifier"
                    )
                    (NestedMetadataInput.OptionalDefinedTerm(
                        "Creative Work Status",
                        sample.CreativeWorkStatus,
                        (fun status -> sample.Copy(creativeWorkStatus = status) |> setSample),
                        (ProcessCoreEntityValue.DefinedTerm >> navigate),
                        imports = (fun catalog -> catalog.DefinedTerms)
                    ))
                    NestedMetadataInput.CreatePCInputSequence(
                        (ResizeArray sample.Authors),
                        (fun () -> Agent("New Agent")),
                        (fun authors -> sample.Copy(authors = authors) |> setSample),
                        "Authors",
                        NestedMetadataInput.agent,
                        (ProcessCoreEntityValue.Agent >> navigate),
                        imports = (fun catalog -> catalog.Agents)
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        (ResizeArray sample.AdditionalProperty),
                        (fun () -> Annotation("New Annotation")),
                        (fun properties -> sample.Copy(additionalProperty = properties) |> setSample),
                        "Additional Properties",
                        NestedMetadataInput.Annotation,
                        (ProcessCoreEntityValue.Annotation >> navigate),
                        imports = (fun catalog -> catalog.Annotations)
                    )
                ]
            )
        ]

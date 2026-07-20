namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
open ProcessCore
open Swate.Components.Shared
open Swate.Components.Primitive.LayoutComponents
open Swate.Components.Page.Metadata
open Swate.Components.Page.ObjectBrowser.Types
open Swate.Components.Page.Metadata.FormComponents

[<Erase; Mangle(false)>]
type ScholarlyArticleMetadata =

    [<ReactComponent(true)>]
    static member ScholarlyArticleView
        (
            article: ProcessCore.ScholarlyArticle,
            mutate: (ARC -> unit) -> unit,
            ?onNavigate: ProcessCoreEntityValue -> unit
        ) =

        let navigate = defaultArg onNavigate ignore

        LayoutComponents.Section [
            LayoutComponents.BoxedField(
                "Scholarly Article Metadata",
                content = [
                    FormComponents.TextInput.TextInput(
                        article.Headline,
                        (fun value -> mutate (fun _ -> article.Headline <- value)),
                        label = "Headline",
                        // ProcessCore hotfix: prevent clearing this mandatory primary field.
                        validator = Swate.Components.ProcessCoreHotfixes.required "Headline"
                    )
                    FormComponents.TextInput.TextInput(
                        article.Id |> Option.defaultValue "",
                        (fun value ->
                            mutate (fun _ -> article.Id <- Option.whereNot System.String.IsNullOrWhiteSpace value)
                        ),
                        label = "Id"
                    )
                    FormComponents.TextInput.TextInput(
                        article.Identifier |> Option.defaultValue "",
                        (fun value ->
                            mutate (fun _ ->
                                article.Identifier <- Option.whereNot System.String.IsNullOrWhiteSpace value
                            )
                        ),
                        label = "Identifier"
                    )
                    (NestedMetadataInput.OptionalDefinedTerm(
                        "Creative Work Status",
                        article.CreativeWorkStatus,
                        (fun status -> mutate (fun _ -> article.CreativeWorkStatus <- status)),
                        (ProcessCoreEntityValue.DefinedTerm >> navigate),
                        imports = (fun catalog -> catalog.DefinedTerms)
                    ))
                    NestedMetadataInput.CreatePCInputSequence(
                        article.Authors,
                        (fun () -> Agent("New Agent")),
                        ignore,
                        "Authors",
                        NestedMetadataInput.agent,
                        (ProcessCoreEntityValue.Agent >> navigate),
                        imports = (fun catalog -> catalog.Agents),
                        addItem = (fun item -> mutate (fun _ -> article.AddAuthor item)),
                        removeItem = (fun item -> mutate (fun _ -> article.RemoveAuthor item)),
                        updateItems = (fun items -> AgentMetadata.Agents(items, mutate))
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        article.AdditionalProperty,
                        (fun () -> Annotation("New Annotation")),
                        ignore,
                        "Additional Properties",
                        NestedMetadataInput.Annotation,
                        (ProcessCoreEntityValue.Annotation >> navigate),
                        imports = (fun catalog -> catalog.Annotations),
                        addItem = (fun item -> mutate (fun _ -> article.AddAdditionalProperty item)),
                        removeItem = (fun item -> mutate (fun _ -> article.RemoveAdditionalProperty item)),
                        updateItems = (fun items -> AnnotationMetadata.Annotations(items, mutate))
                    )
                ]
            )
        ]

type ScholarlyArticleMetadata with

    [<ReactComponent>]
    static member ScholarlyArticles(articles: ResizeArray<ScholarlyArticle>, mutate: (ARC -> unit) -> unit) =
        Html.div [
            prop.className "swt:space-y-4"
            prop.children [
                for article in articles do
                    ScholarlyArticleMetadata.ScholarlyArticleView(article, mutate)
            ]
        ]

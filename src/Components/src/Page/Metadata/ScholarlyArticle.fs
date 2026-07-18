namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
open ProcessCore
open Swate.Components.Page.Metadata
open Swate.Components.Page.ObjectBrowser.Types
open Swate.Components.Primitive.LayoutComponents
open Swate.Components.Page.Metadata.FormComponents

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

        let copyArticle (authors: ResizeArray<Agent>) (additionalProperties: ResizeArray<Annotation>) =
            ProcessCore.ScholarlyArticle(
                sample.Headline,
                ?id = sample.Id,
                ?identifier = sample.Identifier,
                ?creativeWorkStatus = sample.CreativeWorkStatus,
                authors = authors,
                additionalProperty = additionalProperties
            )

        let updateSample (updateFn: ProcessCore.ScholarlyArticle -> ProcessCore.ScholarlyArticle) =
            let copy = copyArticle sample.Authors sample.AdditionalProperty

            let updatedSample = updateFn copy
            setSample updatedSample

        LayoutComponents.Section [
            LayoutComponents.BoxedField(
                "Scholarly Article Metadata",
                content = [
                    FormComponents.TextInput.TextInput(
                        sample.Headline,
                        (fun value ->
                            updateSample (fun updatedSample ->
                                updatedSample.Headline <- value
                                updatedSample
                            )
                        ),
                        label = "Headline",
                        // ProcessCore hotfix: prevent clearing this mandatory primary field.
                        validator = Swate.Components.ProcessCoreHotfixes.required "Headline"
                    )
                    FormComponents.TextInput.TextInput(
                        sample.Id |> Option.defaultValue "",
                        (fun value ->
                            updateSample (fun updatedSample ->
                                updatedSample.Id <- Some value
                                updatedSample
                            )
                        ),
                        label = "Id"
                    )
                    FormComponents.TextInput.TextInput(
                        sample.Identifier |> Option.defaultValue "",
                        (fun value ->
                            updateSample (fun updatedSample ->
                                updatedSample.Identifier <- Some value
                                updatedSample
                            )
                        ),
                        label = "Identifier"
                    )
                    (NestedMetadataInput.OptionalDefinedTerm(
                        "Creative Work Status",
                        sample.CreativeWorkStatus,
                        (fun status ->
                            let copy = copyArticle sample.Authors sample.AdditionalProperty
                            copy.CreativeWorkStatus <- status
                            setSample copy
                        ),
                        (ProcessCoreEntityValue.DefinedTerm >> navigate)
                    ))
                    NestedMetadataInput.CreatePCInputSequence(
                        (ResizeArray sample.Authors),
                        (fun () -> Agent("")),
                        (fun authors -> copyArticle authors sample.AdditionalProperty |> setSample),
                        "Authors",
                        NestedMetadataInput.agent,
                        (ProcessCoreEntityValue.Agent >> navigate)
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        (ResizeArray sample.AdditionalProperty),
                        (fun () -> Annotation("")),
                        (fun properties -> copyArticle sample.Authors properties |> setSample),
                        "Additional Properties",
                        NestedMetadataInput.Annotation,
                        (ProcessCoreEntityValue.Annotation >> navigate)
                    )
                ]
            )
        ]

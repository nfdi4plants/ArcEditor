namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
open Swate.Components.Page.Metadata
open Swate.Components.Primitive.LayoutComponents

[<Erase; Mangle(false)>]
type ScholarlyArticleMetadata =

    [<ReactComponent(true)>]
    static member ScholarlyArticleMetadata
        (sample: ProcessCore.ScholarlyArticle, setSample: ProcessCore.ScholarlyArticle -> unit)
        =

        let updateSample (updateFn: ProcessCore.ScholarlyArticle -> ProcessCore.ScholarlyArticle) =
            let copy =
                ProcessCore.ScholarlyArticle(
                    sample.Headline,
                    ?id = sample.Id,
                    ?identifier = sample.Identifier,
                    ?creativeWorkStatus = sample.CreativeWorkStatus,
                    authors = sample.Authors,
                    additionalProperty = sample.AdditionalProperty
                )

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
                        label = "Headline"
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
                    // TODO CreativeWorkStatus is a DefinedTerm, which is a complex type. We need a way to select or create an DefinedTerm.
                    Html.div
                        "Placeholder for CreativeWorkStatus (DefinedTerm) input. This should be a dropdown or a search field to select an existing DefinedTerm or create a new one."
                    FormComponents.TextInput.TextInput(
                        sample.Headline,
                        (fun input ->
                            updateSample (fun updatedSample ->
                                updatedSample.Headline <- input
                                updatedSample
                            )
                        ),
                        label = "Creative WorkStatus"
                    )
                    // TODO Authors is an Agent seq, which is a complex type. We need a way to select or create an Annotation.
                    Html.div
                        "Placeholder for Authors (Agent seq) input. This should be a dropdown or a search field to select an existing Agent or create a new one."
                    FormComponents.TextInput.TextInput(
                        sample.Headline,
                        (fun input ->
                            updateSample (fun updatedSample ->
                                updatedSample.Headline <- input
                                updatedSample
                            )
                        ),
                        label = "Authors"
                    )
                    // TODO AdditionalProperty is an Annotation seq, which is a complex type. We need a way to select or create an Annotation.
                    Html.div
                        "Placeholder for AdditionalProperty (Annotation seq) input. This should be a dropdown or a search field to select an existing Annotation or create a new one."
                    FormComponents.TextInput.TextInput(
                        sample.Headline,
                        (fun input ->
                            updateSample (fun updatedSample ->
                                updatedSample.Headline <- input
                                updatedSample
                            )
                        ),
                        label = "Additional Property"
                    )
                ]
            )
        ]

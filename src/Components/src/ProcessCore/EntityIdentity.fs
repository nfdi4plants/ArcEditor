module Swate.Components.ProcessCore.EntityIdentity

open System
open ProcessCore

let nonEmpty (value: string) =
    if String.IsNullOrWhiteSpace value then
        None
    else
        Some(value.Trim())

let nameOr fallback values =
    values |> Seq.choose id |> Seq.tryPick nonEmpty |> Option.defaultValue fallback

let datasetName (dataset: Dataset) =
    nameOr "Unnamed dataset" [ dataset.Title; Some dataset.Identifier ]

let dataContextName (dataContext: DataContext) =
    nameOr "Unnamed data context" [ dataContext.Label; Some dataContext.Data.Name ]

let agentName (agent: Agent) =
    let fullName =
        [|
            nonEmpty agent.GivenName
            agent.FamilyName |> Option.bind nonEmpty
        |]
        |> Array.choose id
        |> String.concat " "
        |> nonEmpty

    nameOr "Unnamed agent" [ fullName; agent.Identifier; agent.Email ]

let private valueKey (value: string) = $"{value.Length}:{value}"

let private optionKey value =
    value
    |> Option.map (fun value -> "S" + valueKey value)
    |> Option.defaultValue "N"

let private fieldsKey values =
    values |> Seq.map valueKey |> String.concat ""

let definedTermKey (term: DefinedTerm) =
    fieldsKey [
        term.Name
        optionKey term.TAN
        optionKey term.InDefinedTermSet
    ]

let annotationKey (annotation: Annotation) =
    // NameTAN is presentation metadata; occurrences with the same semantic
    // name/value pair must be treated as the same ARC annotation.
    fieldsKey [ annotation.Name; optionKey annotation.Value ]

let dataKey (data: Data) =
    fieldsKey [ data.Path; optionKey data.Selector ]

let dataContextKey (dataContext: DataContext) =
    let termKey prefix term =
        term |> Option.map (definedTermKey >> (+) prefix) |> Option.defaultValue "N"

    fieldsKey [
        dataContext.Data.Path
        optionKey dataContext.Data.Selector
        termKey "E" dataContext.Explication
        termKey "O" dataContext.ObjectType
        termKey "U" dataContext.Unit
        optionKey dataContext.Label
        optionKey dataContext.Description
        optionKey dataContext.GeneratedBy
    ]

let recipeKey (recipe: Recipe) =
    fieldsKey [ optionKey recipe.Name; optionKey recipe.Version ]

let agentKey (agent: Agent) =
    agent.Id
    |> Option.defaultValue (
        fieldsKey [
            agent.GivenName
            optionKey agent.FamilyName
            optionKey agent.Email
        ]
    )

let organizationKey (organization: Organization) =
    organization.Id |> Option.defaultValue organization.Name

let articleKey (article: ScholarlyArticle) =
    article.Id
    |> Option.defaultValue (fieldsKey [ article.Headline; optionKey article.Identifier ])

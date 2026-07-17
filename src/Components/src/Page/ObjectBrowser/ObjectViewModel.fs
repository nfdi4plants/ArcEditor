module Swate.Components.Page.ObjectBrowser.ObjectViewModel

open System
open ProcessCore
open Swate.Components.Page.ObjectBrowser.Types

    let private nonEmpty (value: string) =
        if String.IsNullOrWhiteSpace value then
            None
        else
            Some(value.Trim())

    let private nameOr fallback values =
        values |> Seq.choose id |> Seq.tryPick nonEmpty |> Option.defaultValue fallback

    let rec private descendantDatasets (dataset: Dataset) = seq {
        for child in dataset.HasPart do
            yield child
            yield! descendantDatasets child
    }

    let private datasetsIncludingRoot (arc: ARC) : seq<Dataset> = seq {
        yield arc :> Dataset
        yield! descendantDatasets arc
    }

    let private datasetName (dataset: Dataset) =
        nameOr "Unnamed dataset" [ dataset.Title; Some dataset.Identifier ]

    let private dataContextName (dataContext: DataContext) =
        nameOr "Unnamed data context" [ dataContext.Label; Some dataContext.Data.Name ]

    let private agentName (agent: Agent) =
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

    let private definedTermKey (term: DefinedTerm) =
        fieldsKey [
            term.Name
            optionKey term.TAN
            optionKey term.InDefinedTermSet
        ]

    let private annotationKey (annotation: Annotation) =
        fieldsKey [
            annotation.Name
            optionKey annotation.Value
            optionKey annotation.NameTAN
        ]

    let private dataKey (data: Data) =
        fieldsKey [ data.Path; optionKey data.Selector ]

    let private dataContextKey (dataContext: DataContext) =
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

    let private recipeKey (recipe: Recipe) =
        fieldsKey [ optionKey recipe.Name; optionKey recipe.Version ]

    let private agentKey (agent: Agent) =
        agent.Id
        |> Option.defaultValue (
            fieldsKey [
                agent.GivenName
                optionKey agent.FamilyName
                optionKey agent.Email
            ]
        )

    let private organizationKey (organization: Organization) =
        organization.Id |> Option.defaultValue organization.Name

    let private articleKey (article: ScholarlyArticle) =
        article.Id
        |> Option.defaultValue (fieldsKey [ article.Headline; optionKey article.Identifier ])

    let private articleOccurrences (datasets: Dataset array) = seq {
        for dataset in datasets do
            yield! dataset.Citations
    }

    let private agentOccurrences (datasets: Dataset array) = seq {
        for dataset in datasets do
            yield! dataset.Agents

            for article in dataset.Citations do
                yield! article.Authors
    }

    let private allAgents (arc: ARC) =
        datasetsIncludingRoot arc
        |> Seq.toArray
        |> agentOccurrences
        |> Seq.distinctBy agentKey

    let rec private dataAndParts (data: Data) = seq {
        yield data

        for child in data.HasPart do
            yield! dataAndParts child
    }

    let private dataOccurrences (datasets: Dataset array) (processes: Process array) = seq {
        for dataset in datasets do
            for data in dataset.DataFiles do
                yield! dataAndParts data

        for processObject in processes do
            for node in Seq.append processObject.Inputs processObject.Outputs do
                match node with
                | DataNode data -> yield! dataAndParts data
                | SampleNode _ -> ()
    }

    let private getEntityKeyAndName entityValue =
        match entityValue with
        | ProcessCoreEntityValue.Dataset dataset -> dataset.Identifier, datasetName dataset
        | ProcessCoreEntityValue.Process processObject ->
            processObject.Name, nameOr "Unnamed process" [ Some processObject.Name ]
        | ProcessCoreEntityValue.Sample sample -> sample.Name, nameOr "Unnamed sample" [ Some sample.Name ]
        | ProcessCoreEntityValue.Data data -> dataKey data, nameOr "Unnamed data" [ Some data.Name ]
        | ProcessCoreEntityValue.Recipe recipe -> recipeKey recipe, nameOr "Unnamed recipe" [ recipe.Name ]
        | ProcessCoreEntityValue.Annotation annotation ->
            annotationKey annotation, nameOr "Unnamed annotation" [ Some annotation.Name ]
        | ProcessCoreEntityValue.DataContext dataContext -> dataContextKey dataContext, dataContextName dataContext
        | ProcessCoreEntityValue.Agent agent -> agentKey agent, agentName agent
        | ProcessCoreEntityValue.Organization organization ->
            organizationKey organization, nameOr "Unnamed organization" [ Some organization.Name ]
        | ProcessCoreEntityValue.ScholarlyArticle article ->
            articleKey article, nameOr "Unnamed scholarly article" [ Some article.Headline ]

    let getEntities (arc: ARC) (kind: MemberKind) =
        let entityValues =
            match kind with
            | MemberKind.Dataset ->
                descendantDatasets arc
                |> Seq.distinctBy (fun dataset -> dataset.Identifier)
                |> Seq.map ProcessCoreEntityValue.Dataset
            | MemberKind.Process -> arc.AllProcesses() |> Seq.map ProcessCoreEntityValue.Process
            | MemberKind.Sample -> arc.AllSamples() |> Seq.map ProcessCoreEntityValue.Sample
            | MemberKind.Data -> arc.AllData() |> Seq.map ProcessCoreEntityValue.Data
            | MemberKind.Recipe ->
                arc.AllProcesses()
                |> Seq.choose (fun processObject -> processObject.ExecutesProtocol)
                |> Seq.distinctBy recipeKey
                |> Seq.map ProcessCoreEntityValue.Recipe
            | MemberKind.Annotation -> arc.AllAnnotations() |> Seq.map ProcessCoreEntityValue.Annotation
            | MemberKind.DataContext -> arc.AllDataContexts() |> Seq.map ProcessCoreEntityValue.DataContext
            | MemberKind.Agent -> allAgents arc |> Seq.map ProcessCoreEntityValue.Agent
            | MemberKind.Organization -> arc.AllOrganizations() |> Seq.map ProcessCoreEntityValue.Organization
            | MemberKind.ScholarlyArticle -> arc.AllCitations() |> Seq.map ProcessCoreEntityValue.ScholarlyArticle

        entityValues
        |> Seq.map (fun entityValue ->
            let key, displayName = getEntityKeyAndName entityValue

            {
                memberKind = kind
                key = key
                displayName = displayName
                value = entityValue
            }
        )
        |> Array.ofSeq

    let getNames arc kind =
        getEntities arc kind |> Array.map _.displayName

    let private removeMatching key getKey remove (items: seq<'T>) =
        items |> Seq.filter (getKey >> (=) key) |> Seq.toArray |> Array.iter remove

    let private removeNodeFromProcesses predicate (processes: Process array) =
        for processObject in processes do
            processObject.Inputs
            |> Seq.filter predicate
            |> Seq.toArray
            |> Array.iter processObject.RemoveInput

            processObject.Outputs
            |> Seq.filter predicate
            |> Seq.toArray
            |> Array.iter processObject.RemoveOutput

    let private removeAnnotationFromArc (arc: ARC) (annotation: Annotation) =
        let key = annotationKey annotation

        let removeFrom items remove =
            removeMatching key annotationKey remove items

        let datasets = datasetsIncludingRoot arc |> Seq.toArray
        let processes = arc.AllProcesses() |> Seq.toArray

        for dataset in datasets do
            removeFrom dataset.AdditionalProperty dataset.RemoveAdditionalProperty

        for processObject in processes do
            removeFrom processObject.ParameterValue processObject.RemoveParameterValue

            for node in Seq.append processObject.Inputs processObject.Outputs |> Seq.toArray do
                match node with
                | SampleNode sample -> removeFrom sample.AdditionalProperty sample.RemoveAdditionalProperty
                | DataNode _ -> ()

            processObject.ExecutesProtocol
            |> Option.iter (fun recipe ->
                removeFrom recipe.Components recipe.RemoveComponent
                removeFrom recipe.AdditionalProperty recipe.RemoveAdditionalProperty
            )

        for data in dataOccurrences datasets processes |> Seq.toArray do
            removeFrom data.AdditionalProperty data.RemoveAdditionalProperty

        for agent in agentOccurrences datasets |> Seq.toArray do
            removeFrom agent.AdditionalProperty agent.RemoveAdditionalProperty

        for article in articleOccurrences datasets |> Seq.toArray do
            removeFrom article.AdditionalProperty article.RemoveAdditionalProperty

    let removeEntity (arc: ARC) (entity: ProcessCoreEntity) =
        let datasets = datasetsIncludingRoot arc |> Seq.toArray
        let processes = arc.AllProcesses() |> Seq.toArray

        match entity.value with
        | ProcessCoreEntityValue.Dataset dataset ->
            dataset.PartOf |> Option.iter (fun parent -> parent.RemovePart dataset)
        | ProcessCoreEntityValue.Process processObject ->
            let owner = processObject.ProcessOf

            processObject.Inputs |> Seq.toArray |> Array.iter processObject.RemoveInput

            processObject.Outputs |> Seq.toArray |> Array.iter processObject.RemoveOutput

            owner |> Option.iter (fun dataset -> dataset.RemoveProcess processObject)
        | ProcessCoreEntityValue.Sample sample ->
            removeNodeFromProcesses
                (function
                | SampleNode candidate -> candidate.Name = sample.Name
                | _ -> false)
                processes
        | ProcessCoreEntityValue.Data data ->
            let key = dataKey data
            let allData = dataOccurrences datasets processes |> Seq.toArray

            removeNodeFromProcesses
                (function
                | DataNode candidate -> dataKey candidate = key
                | _ -> false)
                processes

            for dataset in datasets do
                removeMatching key dataKey dataset.RemoveDataFile dataset.DataFiles

                removeMatching
                    key
                    (fun (context: DataContext) -> dataKey context.Data)
                    dataset.RemoveDataContext
                    dataset.DataContexts

            for parent in allData do
                removeMatching key dataKey parent.RemovePart parent.HasPart
        | ProcessCoreEntityValue.Recipe recipe ->
            let key = recipeKey recipe

            for processObject in processes do
                match processObject.ExecutesProtocol with
                | Some candidate when recipeKey candidate = key -> processObject.ExecutesProtocol <- None
                | _ -> ()
        | ProcessCoreEntityValue.Annotation annotation -> removeAnnotationFromArc arc annotation
        | ProcessCoreEntityValue.DataContext dataContext ->
            let key = dataContextKey dataContext

            for dataset in datasets do
                removeMatching key dataContextKey dataset.RemoveDataContext dataset.DataContexts
        | ProcessCoreEntityValue.Agent agent ->
            let key = agentKey agent

            for dataset in datasets do
                removeMatching key agentKey dataset.RemoveAgent dataset.Agents

                for article in dataset.Citations |> Seq.toArray do
                    removeMatching key agentKey article.RemoveAuthor article.Authors
        | ProcessCoreEntityValue.Organization organization ->
            let key = organizationKey organization

            for agent in agentOccurrences datasets |> Seq.toArray do
                match agent.Affiliation with
                | Some affiliation when organizationKey affiliation = key -> agent.Affiliation <- None
                | _ -> ()
        | ProcessCoreEntityValue.ScholarlyArticle article ->
            let key = articleKey article

            for dataset in datasets do
                removeMatching key articleKey dataset.RemoveCitation dataset.Citations

    let removeEntities arc entities = entities |> Seq.iter (removeEntity arc)

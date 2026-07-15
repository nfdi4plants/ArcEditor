namespace Swate.Components.Composite.ProcessCore

open System
open ProcessCore

module ObjectViewModel =

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

    let private recipeKey (recipe: Recipe) =
        let name = recipe.Name |> Option.defaultValue ""
        let version = recipe.Version |> Option.defaultValue ""
        $"{name}|{version}"

    let private agentKey (agent: Agent) =
        let familyName = agent.FamilyName |> Option.defaultValue ""
        let email = agent.Email |> Option.defaultValue ""

        agent.Id |> Option.defaultValue $"{agent.GivenName}|{familyName}|{email}"

    let private allAgents (arc: ARC) =
        seq {
            yield! arc.AllAgents()

            for citation in arc.AllCitations() do
                yield! citation.Authors
        }
        |> Seq.distinctBy agentKey

    let getNames (arc: ARC) (kind: MemberKind) =
        match kind with
        | MemberKind.Dataset ->
            descendantDatasets arc
            |> Seq.distinctBy (fun dataset -> dataset.Identifier)
            |> Seq.map datasetName
        | MemberKind.Process ->
            arc.AllProcesses()
            |> Seq.map (fun processObject -> nameOr "Unnamed process" [ Some processObject.Name ])
        | MemberKind.Sample ->
            arc.AllSamples()
            |> Seq.map (fun sample -> nameOr "Unnamed sample" [ Some sample.Name ])
        | MemberKind.Data -> arc.AllData() |> Seq.map (fun data -> nameOr "Unnamed data" [ Some data.Name ])
        | MemberKind.Recipe ->
            arc.AllProcesses()
            |> Seq.choose (fun processObject -> processObject.ExecutesProtocol)
            |> Seq.distinctBy recipeKey
            |> Seq.map (fun recipe -> nameOr "Unnamed recipe" [ recipe.Name ])
        | MemberKind.Annotation ->
            arc.AllAnnotations()
            |> Seq.map (fun annotation -> nameOr "Unnamed annotation" [ Some annotation.Name ])
        | MemberKind.DataContext -> arc.AllDataContexts() |> Seq.map dataContextName
        | MemberKind.Agent -> allAgents arc |> Seq.map agentName
        | MemberKind.Organization ->
            arc.AllOrganizations()
            |> Seq.map (fun organization -> nameOr "Unnamed organization" [ Some organization.Name ])
        | MemberKind.ScholarlyArticle ->
            arc.AllCitations()
            |> Seq.map (fun article -> nameOr "Unnamed scholarly article" [ Some article.Headline ])
        |> Array.ofSeq

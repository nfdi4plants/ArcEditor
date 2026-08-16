module Swate.Components.Page.ObjectBrowser.ObjectViewModel

open ProcessCore
open Swate.Components.Page.ObjectBrowser.Types
open Swate.Components.ProcessCore.ObjectGraph
open Swate.Components.ProcessCore

let private getEntityKeyAndName entityValue =
    match entityValue with
    | ProcessCoreEntityValue.Dataset dataset -> dataset.Identifier, EntityCatalog.datasetName dataset
    | ProcessCoreEntityValue.Process processObject ->
        processObject.Name, EntityCatalog.nameOr "Unnamed process" [ Some processObject.Name ]
    | ProcessCoreEntityValue.Sample sample -> sample.Name, EntityCatalog.nameOr "Unnamed sample" [ Some sample.Name ]
    | ProcessCoreEntityValue.Data data ->
        EntityCatalog.dataKey data, EntityCatalog.nameOr "Unnamed data" [ Some data.Name ]
    | ProcessCoreEntityValue.Recipe recipe ->
        EntityCatalog.recipeKey recipe, EntityCatalog.nameOr "Unnamed recipe" [ recipe.Name ]
    | ProcessCoreEntityValue.FormalParameter parameter ->
        parameter.Name, EntityCatalog.nameOr "Unnamed formal parameter" [ Some parameter.Name ]
    | ProcessCoreEntityValue.DefinedTerm term ->
        term.Name, EntityCatalog.nameOr "Unnamed defined term" [ Some term.Name ]
    | ProcessCoreEntityValue.Annotation annotation ->
        EntityCatalog.annotationKey annotation, EntityCatalog.nameOr "Unnamed annotation" [ Some annotation.Name ]
    | ProcessCoreEntityValue.DataContext dataContext ->
        EntityCatalog.dataContextKey dataContext, EntityCatalog.dataContextName dataContext
    | ProcessCoreEntityValue.Agent agent -> EntityCatalog.agentKey agent, EntityCatalog.agentName agent
    | ProcessCoreEntityValue.Organization organization ->
        EntityCatalog.organizationKey organization,
        EntityCatalog.nameOr "Unnamed organization" [ Some organization.Name ]
    | ProcessCoreEntityValue.ScholarlyArticle article ->
        EntityCatalog.articleKey article, EntityCatalog.nameOr "Unnamed scholarly article" [ Some article.Headline ]

/// Returns the user-facing fallback-aware name for a ProcessCore entity.
let displayName entityValue = getEntityKeyAndName entityValue |> snd

/// Removes duplicate browser entries while retaining entities of different kinds.
let distinctEntities entities =
    entities |> Array.distinctBy (fun entity -> entity.memberKind, entity.key)

/// Applies the Object Browser's optional kind filter and case-insensitive name search.
let filterEntities (searchQuery: string) (memberKind: MemberKind option) (entities: ProcessCoreEntity array) =
    let searchTerm = searchQuery.Trim()
    let normalizedSearchTerm = searchTerm.ToUpperInvariant()

    entities
    |> Array.filter (fun entity ->
        memberKind |> Option.forall ((=) entity.memberKind)
        && (searchTerm = ""
            || entity.displayName.ToUpperInvariant().Contains(normalizedSearchTerm))
    )

/// Adapts a ProcessCore value to the UI-facing Object Browser model.
let createEntity kind entityValue =
    let key, displayName = getEntityKeyAndName entityValue

    {
        memberKind = kind
        key = key
        displayName = displayName
        value = entityValue
    }

/// Enumerates Object Browser entries of the requested kind from the ARC projection.
let getEntities (arcView: Swate.Components.ProcessCore.Types.ArcView) (arc: ARC) (kind: MemberKind) =
    let entityValues =
        match kind with
        | MemberKind.Dataset ->
            descendantDatasets arc
            |> Seq.distinctBy _.Identifier
            |> Seq.map ProcessCoreEntityValue.Dataset
        | MemberKind.Process ->
            arcView.Processes
            |> Seq.map (_.Representative >> ProcessCoreEntityValue.Process)
        | MemberKind.Sample -> arcView.Samples |> Seq.map ProcessCoreEntityValue.Sample
        | MemberKind.Data -> arcView.Data |> Seq.map ProcessCoreEntityValue.Data
        | MemberKind.Recipe ->
            recipes arc
            |> Seq.distinctBy EntityCatalog.recipeKey
            |> Seq.map ProcessCoreEntityValue.Recipe
        | MemberKind.Annotation -> arc.AllAnnotations() |> Seq.map ProcessCoreEntityValue.Annotation
        | MemberKind.DataContext -> arc.AllDataContexts() |> Seq.map ProcessCoreEntityValue.DataContext
        | MemberKind.Agent -> EntityCatalog.agents arc |> Seq.map ProcessCoreEntityValue.Agent
        | MemberKind.Organization -> EntityCatalog.organizations arc |> Seq.map ProcessCoreEntityValue.Organization
        | MemberKind.ScholarlyArticle -> arc.AllCitations() |> Seq.map ProcessCoreEntityValue.ScholarlyArticle

    entityValues |> Seq.map (createEntity kind) |> Array.ofSeq

/// Dispatches removal to the reference-aware command for the selected entity kind.
let removeEntity (arcView: Swate.Components.ProcessCore.Types.ArcView) (arc: ARC) (entity: ProcessCoreEntity) =
    match entity.value with
    | ProcessCoreEntityValue.Dataset value -> value.PartOf |> Option.iter (fun parent -> parent.RemovePart value)
    | ProcessCoreEntityValue.Process value -> RendererModel.removeProcess value arcView
    | ProcessCoreEntityValue.Sample value -> EntityCommands.removeSample arc value
    | ProcessCoreEntityValue.Data value -> EntityCommands.removeData arc value
    | ProcessCoreEntityValue.Recipe value -> EntityCommands.removeRecipe arc value
    | ProcessCoreEntityValue.FormalParameter _
    | ProcessCoreEntityValue.DefinedTerm _ -> ()
    | ProcessCoreEntityValue.Annotation value -> EntityCommands.removeAnnotation arc value
    | ProcessCoreEntityValue.DataContext value -> EntityCommands.removeDataContext arc value
    | ProcessCoreEntityValue.Agent value -> EntityCommands.removeAgent arc value
    | ProcessCoreEntityValue.Organization value -> EntityCommands.removeOrganization arc value
    | ProcessCoreEntityValue.ScholarlyArticle value -> EntityCommands.removeScholarlyArticle arc value

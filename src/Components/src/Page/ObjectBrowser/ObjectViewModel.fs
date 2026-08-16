module Swate.Components.Page.ObjectBrowser.ObjectViewModel

open ProcessCore
open Swate.Components.Page.ObjectBrowser.Types
open Swate.Components.ProcessCore.ObjectGraph
open Swate.Components.ProcessCore

let private getEntityKeyAndName entityValue =
    match entityValue with
    | ProcessCoreEntityValue.Dataset dataset -> dataset.Identifier, EntityIdentity.datasetName dataset
    | ProcessCoreEntityValue.Process processObject ->
        processObject.Name, EntityIdentity.nameOr "Unnamed process" [ Some processObject.Name ]
    | ProcessCoreEntityValue.Sample sample -> sample.Name, EntityIdentity.nameOr "Unnamed sample" [ Some sample.Name ]
    | ProcessCoreEntityValue.Data data ->
        EntityIdentity.dataKey data, EntityIdentity.nameOr "Unnamed data" [ Some data.Name ]
    | ProcessCoreEntityValue.Recipe recipe ->
        EntityIdentity.recipeKey recipe, EntityIdentity.nameOr "Unnamed recipe" [ recipe.Name ]
    | ProcessCoreEntityValue.FormalParameter parameter ->
        parameter.Name, EntityIdentity.nameOr "Unnamed formal parameter" [ Some parameter.Name ]
    | ProcessCoreEntityValue.DefinedTerm term ->
        term.Name, EntityIdentity.nameOr "Unnamed defined term" [ Some term.Name ]
    | ProcessCoreEntityValue.Annotation annotation ->
        EntityIdentity.annotationKey annotation, EntityIdentity.nameOr "Unnamed annotation" [ Some annotation.Name ]
    | ProcessCoreEntityValue.DataContext dataContext ->
        EntityIdentity.dataContextKey dataContext, EntityIdentity.dataContextName dataContext
    | ProcessCoreEntityValue.Agent agent -> EntityIdentity.agentKey agent, EntityIdentity.agentName agent
    | ProcessCoreEntityValue.Organization organization ->
        EntityIdentity.organizationKey organization,
        EntityIdentity.nameOr "Unnamed organization" [ Some organization.Name ]
    | ProcessCoreEntityValue.ScholarlyArticle article ->
        EntityIdentity.articleKey article, EntityIdentity.nameOr "Unnamed scholarly article" [ Some article.Headline ]

let createEntity kind entityValue =
    let key, displayName = getEntityKeyAndName entityValue

    {
        memberKind = kind
        key = key
        displayName = displayName
        value = entityValue
    }

let getEntitiesWithView (arcView: Swate.Components.ProcessCore.Types.ArcView) (arc: ARC) (kind: MemberKind) =
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
            |> Seq.distinctBy EntityIdentity.recipeKey
            |> Seq.map ProcessCoreEntityValue.Recipe
        | MemberKind.Annotation -> arc.AllAnnotations() |> Seq.map ProcessCoreEntityValue.Annotation
        | MemberKind.DataContext -> arc.AllDataContexts() |> Seq.map ProcessCoreEntityValue.DataContext
        | MemberKind.Agent -> EntityCatalog.agents arc |> Seq.map ProcessCoreEntityValue.Agent
        | MemberKind.Organization -> EntityCatalog.organizations arc |> Seq.map ProcessCoreEntityValue.Organization
        | MemberKind.ScholarlyArticle -> arc.AllCitations() |> Seq.map ProcessCoreEntityValue.ScholarlyArticle

    entityValues |> Seq.map (createEntity kind) |> Array.ofSeq

let getEntities (arc: ARC) kind =
    getEntitiesWithView (RendererModel.create arc) arc kind

let getNames arc kind =
    getEntities arc kind |> Array.map _.displayName

let removeEntityWithView (arcView: Swate.Components.ProcessCore.Types.ArcView) (arc: ARC) (entity: ProcessCoreEntity) =
    match entity.value with
    | ProcessCoreEntityValue.Dataset value -> EntityCommands.removeDataset value
    | ProcessCoreEntityValue.Process value -> EntityCommands.removeProcess value arcView
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

let removeEntity (arc: ARC) entity =
    removeEntityWithView (RendererModel.create arc) arc entity

let removeEntities (arc: ARC) entities =
    let arcView = RendererModel.create arc
    entities |> Seq.iter (removeEntityWithView arcView arc)

module Swate.Components.Page.ObjectBrowser.MemberTree

open ProcessCore
open Swate.Components.ProcessCore
open Swate.Components.Primitive.Tree.Types
open Swate.Components.Page.ObjectBrowser.Types

module private MemberTreeTypes =
    type EntityInfo = { icon: string; reference: obj }

open MemberTreeTypes

/// Associates a ProcessCore value with its visual icon and reference identity.
let private createEntityInfo icon value = { icon = icon; reference = box value }

/// Returns rendering and cycle-detection information for a ProcessCore entity value.
let private createEntityInfoForValue =
    function
    | ProcessCoreEntityValue.Dataset value -> createEntityInfo Icons.datasetIcon value
    | ProcessCoreEntityValue.Process value -> createEntityInfo Icons.processIcon value
    | ProcessCoreEntityValue.Sample value -> createEntityInfo Icons.sampleIcon value
    | ProcessCoreEntityValue.Data value -> createEntityInfo Icons.dataIcon value
    | ProcessCoreEntityValue.Recipe value -> createEntityInfo Icons.recipeIcon value
    | ProcessCoreEntityValue.FormalParameter value -> createEntityInfo Icons.formalParameterIcon value
    | ProcessCoreEntityValue.DefinedTerm value -> createEntityInfo Icons.definedTermIcon value
    | ProcessCoreEntityValue.Annotation value -> createEntityInfo Icons.annotationIcon value
    | ProcessCoreEntityValue.DataContext value -> createEntityInfo Icons.dataContextIcon value
    | ProcessCoreEntityValue.Agent value -> createEntityInfo Icons.agentIcon value
    | ProcessCoreEntityValue.Organization value -> createEntityInfo Icons.organizationIcon value
    | ProcessCoreEntityValue.ScholarlyArticle value -> createEntityInfo Icons.scholarlyArticleIcon value

/// Creates a browser entity, inheriting the parent kind for values without their own object category.
let private createEntity fallbackKind value =
    value
    |> ProcessCoreEntityValue.tryGetProcessCoreObjectKind
    |> Option.defaultValue fallbackKind
    |> fun kind -> ObjectViewModel.createEntity kind value

/// Creates the shared browser/explorer representation of a dataset.
let createDatasetEntity (dataset: Dataset) =
    createEntity MemberKind.Dataset (ProcessCoreEntityValue.Dataset dataset)

/// Converts the ProcessCore input/output union into the browser entity union.
let private createIOEntityValue =
    function
    | ProcessCore.SampleNode sample -> ProcessCoreEntityValue.Sample sample
    | ProcessCore.DataNode data -> ProcessCoreEntityValue.Data data

/// Recursively creates an entity node while stopping branches that revisit an ancestor reference.
let rec private createEntityNode arcView ancestors parentKey index (item: ProcessCoreEntity) =
    let info = createEntityInfoForValue item.value

    let isCycle =
        ancestors
        |> List.exists (fun ancestor -> obj.ReferenceEquals(ancestor, info.reference))

    let nodeKey = $"{parentKey}/{index}/{item.key}"

    {
        key = nodeKey
        label = item.displayName
        icon = Some info.icon
        data = Some item
        children =
            if isCycle then
                [||]
            else
                entityCollections arcView item
                |> Array.filter (fun collection -> not (Array.isEmpty collection.members))
                |> Array.map (createCollectionNode arcView (info.reference :: ancestors) nodeKey)
    }

/// Creates a structural folder for one named ProcessCore relationship.
and private createCollectionNode arcView ancestors parentKey (collection: EntityCollection) =
    let nodeKey = $"{parentKey}/{collection.key}"

    {
        key = nodeKey
        label = collection.label
        icon = Some collection.icon
        data = None
        children = collection.members |> Array.mapi (createEntityNode arcView ancestors nodeKey)
    }

/// Returns the immediate named relationships supported by a ProcessCore entity.
and entityCollections arcView (item: ProcessCoreEntity) : EntityCollection array =
    let allowedMemberKinds relationshipKey =
        match item.value, relationshipKey with
        | ProcessCoreEntityValue.Dataset _, "processes" -> [| MemberKind.Process |]
        | ProcessCoreEntityValue.Dataset _, "has-part" -> [||]
        | ProcessCoreEntityValue.Dataset _, "data-files" -> [| MemberKind.Data |]
        | ProcessCoreEntityValue.Dataset _, "agents" -> [| MemberKind.Agent |]
        | ProcessCoreEntityValue.Dataset _, "citations" -> [| MemberKind.ScholarlyArticle |]
        | ProcessCoreEntityValue.Dataset _, "data-contexts" -> [| MemberKind.DataContext |]
        | ProcessCoreEntityValue.Process _, ("inputs" | "outputs") -> [| MemberKind.Sample; MemberKind.Data |]
        | ProcessCoreEntityValue.Process _, "executes-protocol" -> [| MemberKind.Recipe |]
        | ProcessCoreEntityValue.Data _, "has-part" -> [| MemberKind.Data |]
        | ProcessCoreEntityValue.DataContext _, "data" -> [| MemberKind.Data |]
        | ProcessCoreEntityValue.Agent _, "affiliation" -> [| MemberKind.Organization |]
        | ProcessCoreEntityValue.ScholarlyArticle _, "authors" -> [| MemberKind.Agent |]
        | _, ("additional-properties" | "parameter-values" | "components") -> [| MemberKind.Annotation |]
        | _ -> [||]

    let createCollection relationshipKey label icon items = {
        key = relationshipKey
        label = label
        icon = icon
        members = items
        allowedMemberKinds = allowedMemberKinds relationshipKey
    }

    let createCollectionFromMany relationshipKey label icon wrap values =
        values
        |> Seq.map (wrap >> createEntity item.memberKind)
        |> Seq.toArray
        |> createCollection relationshipKey label icon

    let createCollectionFromOptional relationshipKey label icon wrap value =
        value
        |> Option.toArray
        |> createCollectionFromMany relationshipKey label icon wrap

    let createAdditionalPropertiesCollection values =
        createCollectionFromMany
            "additional-properties"
            "Additional Properties"
            Icons.annotationIcon
            ProcessCoreEntityValue.Annotation
            values

    let createDefinedTermCollection relationshipKey label value =
        createCollectionFromOptional
            relationshipKey
            label
            Icons.definedTermIcon
            ProcessCoreEntityValue.DefinedTerm
            value

    [|
        match item.value with
        | ProcessCoreEntityValue.Dataset dataset ->
            yield
                RendererModel.forDataset dataset arcView
                |> Array.map _.Representative
                |> createCollectionFromMany "processes" "Processes" Icons.processIcon ProcessCoreEntityValue.Process

            yield
                createCollectionFromMany
                    "has-part"
                    "Has Part"
                    Icons.datasetIcon
                    ProcessCoreEntityValue.Dataset
                    dataset.HasPart

            yield
                createCollectionFromMany
                    "data-files"
                    "Data Files"
                    Icons.dataIcon
                    ProcessCoreEntityValue.Data
                    dataset.DataFiles

            yield createCollectionFromMany "agents" "Agents" Icons.agentIcon ProcessCoreEntityValue.Agent dataset.Agents

            yield
                createCollectionFromMany
                    "citations"
                    "Citations"
                    Icons.scholarlyArticleIcon
                    ProcessCoreEntityValue.ScholarlyArticle
                    dataset.Citations

            yield
                createCollectionFromMany
                    "data-contexts"
                    "Data Contexts"
                    Icons.dataContextIcon
                    ProcessCoreEntityValue.DataContext
                    dataset.DataContexts

            yield createAdditionalPropertiesCollection dataset.AdditionalProperty

        | ProcessCoreEntityValue.Process processObject ->
            let processView = RendererModel.forProcess processObject arcView

            yield
                createCollectionFromOptional
                    "executes-protocol"
                    "Executes Protocol"
                    Icons.recipeIcon
                    ProcessCoreEntityValue.Recipe
                    processObject.ExecutesProtocol

            yield
                processView.Inputs.Values
                |> Seq.toArray
                |> createCollectionFromMany "inputs" "Inputs" Icons.inputIcon createIOEntityValue

            yield
                processView.Outputs.Values
                |> Seq.toArray
                |> createCollectionFromMany "outputs" "Outputs" Icons.outputIcon createIOEntityValue

            yield
                createCollectionFromMany
                    "parameter-values"
                    "Parameter Values"
                    Icons.annotationIcon
                    ProcessCoreEntityValue.Annotation
                    processObject.ParameterValue

        | ProcessCoreEntityValue.Sample sample -> yield createAdditionalPropertiesCollection sample.AdditionalProperty

        | ProcessCoreEntityValue.Data data ->
            yield createCollectionFromMany "has-part" "Has Part" Icons.dataIcon ProcessCoreEntityValue.Data data.HasPart

            yield createAdditionalPropertiesCollection data.AdditionalProperty

        | ProcessCoreEntityValue.Recipe recipe ->
            yield createDefinedTermCollection "intended-use" "Intended Use" recipe.IntendedUse

            yield
                createCollectionFromMany
                    "parameters"
                    "Parameters"
                    Icons.formalParameterIcon
                    ProcessCoreEntityValue.FormalParameter
                    recipe.Parameters

            yield
                createCollectionFromMany
                    "components"
                    "Components"
                    Icons.annotationIcon
                    ProcessCoreEntityValue.Annotation
                    recipe.Components

            yield createAdditionalPropertiesCollection recipe.AdditionalProperty

        | ProcessCoreEntityValue.FormalParameter parameter ->
            yield createDefinedTermCollection "default-value" "Default Value" parameter.DefaultValue

        | ProcessCoreEntityValue.Annotation annotation ->
            yield
                createCollectionFromOptional
                    "instance-of"
                    "Instance Of"
                    Icons.formalParameterIcon
                    ProcessCoreEntityValue.FormalParameter
                    annotation.InstanceOf

        | ProcessCoreEntityValue.DataContext dataContext ->
            yield
                [|
                    createEntity item.memberKind (ProcessCoreEntityValue.Data dataContext.Data)
                |]
                |> createCollection "data" "Data" Icons.dataIcon

            yield createDefinedTermCollection "explication" "Explication" dataContext.Explication
            yield createDefinedTermCollection "object-type" "Object Type" dataContext.ObjectType
            yield createDefinedTermCollection "unit" "Unit" dataContext.Unit

        | ProcessCoreEntityValue.Agent agent ->
            yield
                createCollectionFromOptional
                    "affiliation"
                    "Affiliation"
                    Icons.organizationIcon
                    ProcessCoreEntityValue.Organization
                    agent.Affiliation

            yield createAdditionalPropertiesCollection agent.AdditionalProperty

            yield
                createCollectionFromMany
                    "job-titles"
                    "Job Titles"
                    Icons.jobTitleIcon
                    ProcessCoreEntityValue.DefinedTerm
                    agent.JobTitles

        | ProcessCoreEntityValue.ScholarlyArticle article ->
            yield createDefinedTermCollection "creative-work-status" "Creative Work Status" article.CreativeWorkStatus

            yield
                createCollectionFromMany
                    "authors"
                    "Authors"
                    Icons.agentIcon
                    ProcessCoreEntityValue.Agent
                    article.Authors

            yield createAdditionalPropertiesCollection article.AdditionalProperty

        | ProcessCoreEntityValue.DefinedTerm _
        | ProcessCoreEntityValue.Organization _ -> ()
    |]

/// Creates root tree nodes for the ARC's immediate child datasets.
let createDatasetNodes arcView (arc: ProcessCore.ARC) : TreeNode<ProcessCoreEntity> array =
    arc.HasPart
    |> Seq.map createDatasetEntity
    |> Seq.toArray
    |> Array.mapi (createEntityNode arcView [] "datasets")

/// Returns direct children that belong to a top-level object category for scoped sidebar counts.
let directMembers arcView (item: ProcessCoreEntity) : ProcessCoreEntity array =
    entityCollections arcView item
    |> Array.collect _.members
    |> Array.filter (fun entity -> ProcessCoreEntityValue.tryGetProcessCoreObjectKind entity.value |> Option.isSome)

/// Counts reference-unique direct members by their top-level object category.
let directMemberCounts arcView item =
    item
    |> directMembers arcView
    |> ObjectViewModel.distinctEntities
    |> Array.countBy _.memberKind
    |> Map.ofArray

/// Returns the distinct object kinds accepted by any relationship of an entity.
let allowedChildKinds arcView (item: ProcessCoreEntity) =
    entityCollections arcView item
    |> Array.collect _.allowedMemberKinds
    |> Array.distinct

/// Creates the Process relationship actions available at an optional collection level.
let createProcessRelationshipActions processObject relationshipKey =
    let create relationship =
        ContextMenuRequest.AddProcessRelationship(processObject, relationship)

    match relationshipKey with
    | Some "inputs" -> [| create ProcessRelationship.Input |]
    | Some "outputs" -> [| create ProcessRelationship.Output |]
    | Some "parameter-values" -> [| create ProcessRelationship.ParameterValue |]
    | Some _ -> [||]
    | None -> [|
        create ProcessRelationship.Input
        create ProcessRelationship.Output
        create ProcessRelationship.ParameterValue
      |]

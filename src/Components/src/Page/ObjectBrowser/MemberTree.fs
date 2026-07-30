module Swate.Components.Page.ObjectBrowser.MemberTree

open ProcessCore
open Swate.Components.ProcessCore
open Swate.Components.Primitive.Tree.Types
open Swate.Components.Page.ObjectBrowser.Types

// Converts the ProcessCore object graph into a navigable dataset hierarchy. Entity
// rows carry metadata-browser values; relationship folders organize nested objects.

let private datasetIcon = MemberCatalog.iconForKind MemberKind.Dataset
let private processIcon = MemberCatalog.iconForKind MemberKind.Process
let private sampleIcon = MemberCatalog.iconForKind MemberKind.Sample
let private dataIcon = MemberCatalog.iconForKind MemberKind.Data
let private recipeIcon = MemberCatalog.iconForKind MemberKind.Recipe
let private parameterIcon = "swt:iconify swt:fluent--options-20-regular"
let private termIcon = "swt:iconify swt:fluent--tag-20-regular"
let private annotationIcon = MemberCatalog.iconForKind MemberKind.Annotation
let private dataContextIcon = MemberCatalog.iconForKind MemberKind.DataContext
let private agentIcon = MemberCatalog.iconForKind MemberKind.Agent
let private organizationIcon = MemberCatalog.iconForKind MemberKind.Organization
let private articleIcon = MemberCatalog.iconForKind MemberKind.ScholarlyArticle
let private jobTitleIcon = "swt:iconify swt:fluent--briefcase-20-regular"

type private EntityInfo = { icon: string; reference: obj }

/// Associates a ProcessCore value with its visual icon and reference identity.
let private info icon value = { icon = icon; reference = box value }

/// Returns rendering and cycle-detection information for a ProcessCore entity value.
let private entityInfo =
    function
    | ProcessCoreEntityValue.Dataset value -> info datasetIcon value
    | ProcessCoreEntityValue.Process value -> info processIcon value
    | ProcessCoreEntityValue.Sample value -> info sampleIcon value
    | ProcessCoreEntityValue.Data value -> info dataIcon value
    | ProcessCoreEntityValue.Recipe value -> info recipeIcon value
    | ProcessCoreEntityValue.FormalParameter value -> info parameterIcon value
    | ProcessCoreEntityValue.DefinedTerm value -> info termIcon value
    | ProcessCoreEntityValue.Annotation value -> info annotationIcon value
    | ProcessCoreEntityValue.DataContext value -> info dataContextIcon value
    | ProcessCoreEntityValue.Agent value -> info agentIcon value
    | ProcessCoreEntityValue.Organization value -> info organizationIcon value
    | ProcessCoreEntityValue.ScholarlyArticle value -> info articleIcon value

/// Creates a browser entity, inheriting the parent kind for values without their own object category.
let private entity fallbackKind value =
    value
    |> ProcessCoreEntityValue.tryGetProcessCoreObjectKind
    |> Option.defaultValue fallbackKind
    |> fun kind -> ObjectViewModel.createEntity kind value

/// Converts a relationship collection into browser entities.
let private entities fallbackKind wrap values =
    values |> Seq.map (wrap >> entity fallbackKind) |> Seq.toArray

/// Converts the ProcessCore input/output union into the browser entity union.
let private ioValue =
    function
    | ProcessCore.SampleNode sample -> ProcessCoreEntityValue.Sample sample
    | ProcessCore.DataNode data -> ProcessCoreEntityValue.Data data

/// Recursively creates an entity node while stopping branches that revisit an ancestor reference.
let rec private entityNode arcView ancestors parentKey index (item: ProcessCoreEntity) =
    let info = entityInfo item.value

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
                entityCollections arcView (info.reference :: ancestors) nodeKey item
    }

/// Creates a structural folder for one named ProcessCore relationship.
and private collectionNode arcView ancestors parentKey relationshipKey label icon items =
    let nodeKey = $"{parentKey}/{relationshipKey}"

    {
        key = nodeKey
        label = label
        icon = Some icon
        data = None
        children = items |> Array.mapi (entityNode arcView ancestors nodeKey)
    }

/// Maps the relationships supported by each ProcessCore type to tree folders.
and private entityCollections arcView ancestors parentKey (item: ProcessCoreEntity) =
    let collection relationshipKey label icon items =
        collectionNode arcView ancestors parentKey relationshipKey label icon items

    let many relationshipKey label icon wrap values =
        values |> entities item.memberKind wrap |> collection relationshipKey label icon

    let optional relationshipKey label icon wrap value =
        value |> Option.toArray |> many relationshipKey label icon wrap

    let additionalProperties values =
        many "additional-properties" "Additional Properties" annotationIcon ProcessCoreEntityValue.Annotation values

    let definedTerm relationshipKey label value =
        optional relationshipKey label termIcon ProcessCoreEntityValue.DefinedTerm value

    [|
        match item.value with
        | ProcessCoreEntityValue.Dataset dataset ->
            yield
                RendererModel.forDataset dataset arcView
                |> Array.map _.Representative
                |> many "processes" "Processes" processIcon ProcessCoreEntityValue.Process

            yield many "has-part" "Has Part" datasetIcon ProcessCoreEntityValue.Dataset dataset.HasPart
            yield many "data-files" "Data Files" dataIcon ProcessCoreEntityValue.Data dataset.DataFiles
            yield many "agents" "Agents" agentIcon ProcessCoreEntityValue.Agent dataset.Agents
            yield many "citations" "Citations" articleIcon ProcessCoreEntityValue.ScholarlyArticle dataset.Citations

            yield
                many
                    "data-contexts"
                    "Data Contexts"
                    dataContextIcon
                    ProcessCoreEntityValue.DataContext
                    dataset.DataContexts

            yield additionalProperties dataset.AdditionalProperty

        | ProcessCoreEntityValue.Process processObject ->
            let processView = RendererModel.forProcess processObject arcView

            yield
                optional
                    "executes-protocol"
                    "Executes Protocol"
                    recipeIcon
                    ProcessCoreEntityValue.Recipe
                    processObject.ExecutesProtocol

            yield processView.Inputs.Values |> Seq.toArray |> many "inputs" "Inputs" sampleIcon ioValue

            yield processView.Outputs.Values |> Seq.toArray |> many "outputs" "Outputs" dataIcon ioValue

            yield
                many
                    "parameter-values"
                    "Parameter Values"
                    annotationIcon
                    ProcessCoreEntityValue.Annotation
                    processObject.ParameterValue

        | ProcessCoreEntityValue.Sample sample -> yield additionalProperties sample.AdditionalProperty

        | ProcessCoreEntityValue.Data data ->
            yield many "has-part" "Has Part" dataIcon ProcessCoreEntityValue.Data data.HasPart
            yield additionalProperties data.AdditionalProperty

        | ProcessCoreEntityValue.Recipe recipe ->
            yield definedTerm "intended-use" "Intended Use" recipe.IntendedUse

            yield many "parameters" "Parameters" parameterIcon ProcessCoreEntityValue.FormalParameter recipe.Parameters

            yield many "components" "Components" annotationIcon ProcessCoreEntityValue.Annotation recipe.Components
            yield additionalProperties recipe.AdditionalProperty

        | ProcessCoreEntityValue.FormalParameter parameter ->
            yield definedTerm "default-value" "Default Value" parameter.DefaultValue

        | ProcessCoreEntityValue.Annotation annotation ->
            yield
                optional
                    "instance-of"
                    "Instance Of"
                    parameterIcon
                    ProcessCoreEntityValue.FormalParameter
                    annotation.InstanceOf

        | ProcessCoreEntityValue.DataContext dataContext ->
            yield
                [|
                    entity item.memberKind (ProcessCoreEntityValue.Data dataContext.Data)
                |]
                |> collection "data" "Data" dataIcon

            yield definedTerm "explication" "Explication" dataContext.Explication
            yield definedTerm "object-type" "Object Type" dataContext.ObjectType
            yield definedTerm "unit" "Unit" dataContext.Unit

        | ProcessCoreEntityValue.Agent agent ->
            yield
                optional
                    "affiliation"
                    "Affiliation"
                    organizationIcon
                    ProcessCoreEntityValue.Organization
                    agent.Affiliation

            yield additionalProperties agent.AdditionalProperty

            yield many "job-titles" "Job Titles" jobTitleIcon ProcessCoreEntityValue.DefinedTerm agent.JobTitles

        | ProcessCoreEntityValue.ScholarlyArticle article ->
            yield definedTerm "creative-work-status" "Creative Work Status" article.CreativeWorkStatus

            yield many "authors" "Authors" agentIcon ProcessCoreEntityValue.Agent article.Authors
            yield additionalProperties article.AdditionalProperty

        | ProcessCoreEntityValue.DefinedTerm _
        | ProcessCoreEntityValue.Organization _ -> ()
    |]
    |> Array.filter (fun folder -> not (Array.isEmpty folder.children))

/// Creates root tree nodes for the ARC's immediate child datasets.
let datasetNodes arcView (arc: ProcessCore.ARC) : TreeNode<ProcessCoreEntity> array =
    arc.HasPart
    |> Seq.map (fun dataset -> entity MemberKind.Dataset (ProcessCoreEntityValue.Dataset dataset))
    |> Seq.toArray
    |> Array.mapi (entityNode arcView [] "datasets")

/// Returns direct children that belong to a top-level object category for scoped sidebar counts.
let directMembers arcView (item: ProcessCoreEntity) : ProcessCoreEntity array =
    let info = entityInfo item.value

    entityCollections arcView [ info.reference ] "scope" item
    |> Array.collect _.children
    |> Array.choose (fun node ->
        node.data
        |> Option.filter (fun entity ->
            ProcessCoreEntityValue.tryGetProcessCoreObjectKind entity.value |> Option.isSome
        )
    )

/// Counts reference-unique direct members by their top-level object category.
let directMemberCounts arcView item =
    item
    |> directMembers arcView
    |> Array.distinctBy (fun entity -> entity.memberKind, entity.key)
    |> Array.countBy _.memberKind
    |> Map.ofArray

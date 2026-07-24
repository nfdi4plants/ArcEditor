module Swate.Components.Page.ObjectBrowser.MemberTree

open Swate.Components.Primitive.Tree.Types
open Swate.Components.Page.ObjectBrowser.Types

let private datasetIcon = "swt:iconify-color swt:fluent-color--database-20"

let private processIcon =
    "swt:iconify-color swt:fluent-color--arrow-clockwise-dashes-settings-20"

let private sampleIcon = "swt:iconify-color swt:fluent-color--molecule-20"
let private dataIcon = "swt:iconify-color swt:fluent-color--data-line-20"

let private recipeIcon =
    "swt:iconify-color swt:fluent-color--clipboard-text-edit-20"

let private parameterIcon = "swt:iconify swt:fluent--options-20-regular"
let private termIcon = "swt:iconify swt:fluent--tag-20-regular"

let private annotationIcon =
    "swt:iconify-color swt:fluent-color--comment-multiple-20"

let private dataContextIcon = "swt:iconify-color swt:fluent-color--content-view-20"
let private agentIcon = "swt:iconify-color swt:fluent-color--person-20"
let private organizationIcon = "swt:iconify-color swt:fluent-color--org-20"
let private articleIcon = "swt:iconify-color swt:fluent-color--document-text-20"
let private jobTitleIcon = "swt:iconify swt:fluent--briefcase-20-regular"

type private EntityInfo = {
    memberKind: MemberKind option
    icon: string
    reference: obj
}

let private info memberKind icon value = {
    memberKind = memberKind
    icon = icon
    reference = box value
}

let private entityInfo =
    function
    | ProcessCoreEntityValue.Dataset value -> info (Some MemberKind.Dataset) datasetIcon value
    | ProcessCoreEntityValue.Process value -> info (Some MemberKind.Process) processIcon value
    | ProcessCoreEntityValue.Sample value -> info (Some MemberKind.Sample) sampleIcon value
    | ProcessCoreEntityValue.Data value -> info (Some MemberKind.Data) dataIcon value
    | ProcessCoreEntityValue.Recipe value -> info (Some MemberKind.Recipe) recipeIcon value
    | ProcessCoreEntityValue.FormalParameter value -> info None parameterIcon value
    | ProcessCoreEntityValue.DefinedTerm value -> info None termIcon value
    | ProcessCoreEntityValue.Annotation value -> info (Some MemberKind.Annotation) annotationIcon value
    | ProcessCoreEntityValue.DataContext value -> info (Some MemberKind.DataContext) dataContextIcon value
    | ProcessCoreEntityValue.Agent value -> info (Some MemberKind.Agent) agentIcon value
    | ProcessCoreEntityValue.Organization value -> info (Some MemberKind.Organization) organizationIcon value
    | ProcessCoreEntityValue.ScholarlyArticle value -> info (Some MemberKind.ScholarlyArticle) articleIcon value

let private entity fallbackKind value =
    let info = entityInfo value

    ObjectViewModel.createEntity (Option.defaultValue fallbackKind info.memberKind) value

let private entities fallbackKind wrap values =
    values |> Seq.map (wrap >> entity fallbackKind) |> Seq.toArray

let private ioValue =
    function
    | ProcessCore.SampleNode sample -> ProcessCoreEntityValue.Sample sample
    | ProcessCore.DataNode data -> ProcessCoreEntityValue.Data data

let rec private entityNode ancestors parentKey index (item: ProcessCoreEntity) =
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
                entityCollections (info.reference :: ancestors) nodeKey item
    }

and private collectionNode ancestors parentKey relationshipKey label icon items =
    let nodeKey = $"{parentKey}/{relationshipKey}"

    {
        key = nodeKey
        label = label
        icon = Some icon
        data = None
        children = items |> Array.mapi (entityNode ancestors nodeKey)
    }

and private entityCollections ancestors parentKey (item: ProcessCoreEntity) =
    let collection relationshipKey label icon items =
        collectionNode ancestors parentKey relationshipKey label icon items

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
            yield many "processes" "Processes" processIcon ProcessCoreEntityValue.Process dataset.Processes
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
            yield
                optional
                    "executes-protocol"
                    "Executes Protocol"
                    recipeIcon
                    ProcessCoreEntityValue.Recipe
                    processObject.ExecutesProtocol

            yield many "inputs" "Inputs" sampleIcon ioValue processObject.Inputs
            yield many "outputs" "Outputs" dataIcon ioValue processObject.Outputs

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

let datasetNodes (datasets: ProcessCoreEntity array) : TreeNode<ProcessCoreEntity> array =
    datasets |> Array.mapi (entityNode [] "datasets")

let directMembers (item: ProcessCoreEntity) : ProcessCoreEntity array =
    let info = entityInfo item.value

    entityCollections [ info.reference ] "scope" item
    |> Array.collect _.children
    |> Array.choose (fun node ->
        node.data
        |> Option.filter (fun entity -> (entityInfo entity.value).memberKind |> Option.isSome)
    )

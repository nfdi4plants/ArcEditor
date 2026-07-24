module Swate.Components.Page.ObjectBrowser.MemberTree

open Swate.Components.Primitive.Tree.Types
open Swate.Components.Page.ObjectBrowser.Types

let private entityIcon =
    function
    | ProcessCoreEntityValue.Dataset _ -> "swt:iconify-color swt:fluent-color--database-20"
    | ProcessCoreEntityValue.Process _ -> "swt:iconify-color swt:fluent-color--arrow-clockwise-dashes-settings-20"
    | ProcessCoreEntityValue.Sample _ -> "swt:iconify-color swt:fluent-color--molecule-20"
    | ProcessCoreEntityValue.Data _ -> "swt:iconify-color swt:fluent-color--data-line-20"
    | ProcessCoreEntityValue.Recipe _ -> "swt:iconify-color swt:fluent-color--clipboard-text-edit-20"
    | ProcessCoreEntityValue.FormalParameter _ -> "swt:iconify swt:fluent--options-20-regular"
    | ProcessCoreEntityValue.DefinedTerm _ -> "swt:iconify swt:fluent--tag-20-regular"
    | ProcessCoreEntityValue.Annotation _ -> "swt:iconify-color swt:fluent-color--comment-multiple-20"
    | ProcessCoreEntityValue.DataContext _ -> "swt:iconify-color swt:fluent-color--content-view-20"
    | ProcessCoreEntityValue.Agent _ -> "swt:iconify-color swt:fluent-color--person-20"
    | ProcessCoreEntityValue.Organization _ -> "swt:iconify-color swt:fluent-color--organization-20"
    | ProcessCoreEntityValue.ScholarlyArticle _ -> "swt:iconify-color swt:fluent-color--document-text-20"

let private referenceObject =
    function
    | ProcessCoreEntityValue.Dataset value -> box value
    | ProcessCoreEntityValue.Process value -> box value
    | ProcessCoreEntityValue.Sample value -> box value
    | ProcessCoreEntityValue.Data value -> box value
    | ProcessCoreEntityValue.Recipe value -> box value
    | ProcessCoreEntityValue.FormalParameter value -> box value
    | ProcessCoreEntityValue.DefinedTerm value -> box value
    | ProcessCoreEntityValue.Annotation value -> box value
    | ProcessCoreEntityValue.DataContext value -> box value
    | ProcessCoreEntityValue.Agent value -> box value
    | ProcessCoreEntityValue.Organization value -> box value
    | ProcessCoreEntityValue.ScholarlyArticle value -> box value

let private entity fallbackKind value =
    let kind =
        match value with
        | ProcessCoreEntityValue.Dataset _ -> MemberKind.Dataset
        | ProcessCoreEntityValue.Process _ -> MemberKind.Process
        | ProcessCoreEntityValue.Sample _ -> MemberKind.Sample
        | ProcessCoreEntityValue.Data _ -> MemberKind.Data
        | ProcessCoreEntityValue.Recipe _ -> MemberKind.Recipe
        | ProcessCoreEntityValue.Annotation _ -> MemberKind.Annotation
        | ProcessCoreEntityValue.DataContext _ -> MemberKind.DataContext
        | ProcessCoreEntityValue.Agent _ -> MemberKind.Agent
        | ProcessCoreEntityValue.Organization _ -> MemberKind.Organization
        | ProcessCoreEntityValue.ScholarlyArticle _ -> MemberKind.ScholarlyArticle
        // These metadata-only types do not have their own top-level member category.
        | ProcessCoreEntityValue.FormalParameter _
        | ProcessCoreEntityValue.DefinedTerm _ -> fallbackKind

    ObjectViewModel.createEntity kind value

let private optionEntity fallbackKind wrap value =
    value |> Option.map (wrap >> entity fallbackKind) |> Option.toArray

let private entities fallbackKind wrap values =
    values |> Seq.map (wrap >> entity fallbackKind) |> Seq.toArray

let private ioEntities fallbackKind values =
    values
    |> Seq.map (
        function
        | ProcessCore.SampleNode sample -> ProcessCoreEntityValue.Sample sample
        | ProcessCore.DataNode data -> ProcessCoreEntityValue.Data data
    )
    |> Seq.map (entity fallbackKind)
    |> Seq.toArray

let rec private entityNode ancestors parentKey index (item: ProcessCoreEntity) =
    let itemReference = referenceObject item.value

    let isCycle =
        ancestors
        |> List.exists (fun ancestor -> obj.ReferenceEquals(ancestor, itemReference))

    let nodeKey = $"{parentKey}/{index}/{item.key}"

    {
        key = nodeKey
        label = item.displayName
        icon = Some(entityIcon item.value)
        data = Some item
        children =
            if isCycle then
                [||]
            else
                entityCollections (itemReference :: ancestors) nodeKey item
    }

and private collectionNode ancestors parentKey relationshipKey label icon items = {
    key = $"{parentKey}/{relationshipKey}"
    label = label
    icon = Some icon
    data = None
    children = items |> Array.mapi (entityNode ancestors $"{parentKey}/{relationshipKey}")
}

and private entityCollections ancestors parentKey (item: ProcessCoreEntity) =
    let collection relationshipKey label icon items =
        collectionNode ancestors parentKey relationshipKey label icon items

    [|
        match item.value with
        | ProcessCoreEntityValue.Dataset dataset ->
            yield
                dataset.Processes
                |> entities item.memberKind ProcessCoreEntityValue.Process
                |> collection
                    "processes"
                    "Processes"
                    "swt:iconify-color swt:fluent-color--arrow-clockwise-dashes-settings-20"

            yield
                dataset.HasPart
                |> entities item.memberKind ProcessCoreEntityValue.Dataset
                |> collection "has-part" "Has Part" "swt:iconify-color swt:fluent-color--database-20"

            yield
                dataset.DataFiles
                |> entities item.memberKind ProcessCoreEntityValue.Data
                |> collection "data-files" "Data Files" "swt:iconify-color swt:fluent-color--data-line-20"

            yield
                dataset.Agents
                |> entities item.memberKind ProcessCoreEntityValue.Agent
                |> collection "agents" "Agents" "swt:iconify-color swt:fluent-color--person-20"

            yield
                dataset.Citations
                |> entities item.memberKind ProcessCoreEntityValue.ScholarlyArticle
                |> collection "citations" "Citations" "swt:iconify-color swt:fluent-color--document-text-20"

            yield
                dataset.DataContexts
                |> entities item.memberKind ProcessCoreEntityValue.DataContext
                |> collection "data-contexts" "Data Contexts" "swt:iconify-color swt:fluent-color--content-view-20"

            yield
                dataset.AdditionalProperty
                |> entities item.memberKind ProcessCoreEntityValue.Annotation
                |> collection
                    "additional-properties"
                    "Additional Properties"
                    "swt:iconify-color swt:fluent-color--comment-multiple-20"

        | ProcessCoreEntityValue.Process processObject ->
            yield
                processObject.ExecutesProtocol
                |> optionEntity item.memberKind ProcessCoreEntityValue.Recipe
                |> collection
                    "executes-protocol"
                    "Executes Protocol"
                    "swt:iconify-color swt:fluent-color--clipboard-text-edit-20"

            yield
                processObject.Inputs
                |> ioEntities item.memberKind
                |> collection "inputs" "Inputs" "swt:iconify-color swt:fluent-color--molecule-20"

            yield
                processObject.Outputs
                |> ioEntities item.memberKind
                |> collection "outputs" "Outputs" "swt:iconify-color swt:fluent-color--data-line-20"

            yield
                processObject.ParameterValue
                |> entities item.memberKind ProcessCoreEntityValue.Annotation
                |> collection
                    "parameter-values"
                    "Parameter Values"
                    "swt:iconify-color swt:fluent-color--comment-multiple-20"

        | ProcessCoreEntityValue.Sample sample ->
            yield
                sample.AdditionalProperty
                |> entities item.memberKind ProcessCoreEntityValue.Annotation
                |> collection
                    "additional-properties"
                    "Additional Properties"
                    "swt:iconify-color swt:fluent-color--comment-multiple-20"

        | ProcessCoreEntityValue.Data data ->
            yield
                data.HasPart
                |> entities item.memberKind ProcessCoreEntityValue.Data
                |> collection "has-part" "Has Part" "swt:iconify-color swt:fluent-color--data-line-20"

            yield
                data.AdditionalProperty
                |> entities item.memberKind ProcessCoreEntityValue.Annotation
                |> collection
                    "additional-properties"
                    "Additional Properties"
                    "swt:iconify-color swt:fluent-color--comment-multiple-20"

        | ProcessCoreEntityValue.Recipe recipe ->
            yield
                recipe.IntendedUse
                |> optionEntity item.memberKind ProcessCoreEntityValue.DefinedTerm
                |> collection "intended-use" "Intended Use" "swt:iconify swt:fluent--tag-20-regular"

            yield
                recipe.Parameters
                |> entities item.memberKind ProcessCoreEntityValue.FormalParameter
                |> collection "parameters" "Parameters" "swt:iconify swt:fluent--options-20-regular"

            yield
                recipe.Components
                |> entities item.memberKind ProcessCoreEntityValue.Annotation
                |> collection "components" "Components" "swt:iconify-color swt:fluent-color--comment-multiple-20"

            yield
                recipe.AdditionalProperty
                |> entities item.memberKind ProcessCoreEntityValue.Annotation
                |> collection
                    "additional-properties"
                    "Additional Properties"
                    "swt:iconify-color swt:fluent-color--comment-multiple-20"

        | ProcessCoreEntityValue.FormalParameter parameter ->
            yield
                parameter.DefaultValue
                |> optionEntity item.memberKind ProcessCoreEntityValue.DefinedTerm
                |> collection "default-value" "Default Value" "swt:iconify swt:fluent--tag-20-regular"

        | ProcessCoreEntityValue.Annotation annotation ->
            yield
                annotation.InstanceOf
                |> optionEntity item.memberKind ProcessCoreEntityValue.FormalParameter
                |> collection "instance-of" "Instance Of" "swt:iconify swt:fluent--options-20-regular"

        | ProcessCoreEntityValue.DataContext dataContext ->
            yield
                [|
                    entity item.memberKind (ProcessCoreEntityValue.Data dataContext.Data)
                |]
                |> collection "data" "Data" "swt:iconify-color swt:fluent-color--data-line-20"

            yield
                dataContext.Explication
                |> optionEntity item.memberKind ProcessCoreEntityValue.DefinedTerm
                |> collection "explication" "Explication" "swt:iconify swt:fluent--tag-20-regular"

            yield
                dataContext.ObjectType
                |> optionEntity item.memberKind ProcessCoreEntityValue.DefinedTerm
                |> collection "object-type" "Object Type" "swt:iconify swt:fluent--tag-20-regular"

            yield
                dataContext.Unit
                |> optionEntity item.memberKind ProcessCoreEntityValue.DefinedTerm
                |> collection "unit" "Unit" "swt:iconify swt:fluent--tag-20-regular"

        | ProcessCoreEntityValue.Agent agent ->
            yield
                agent.Affiliation
                |> optionEntity item.memberKind ProcessCoreEntityValue.Organization
                |> collection "affiliation" "Affiliation" "swt:iconify-color swt:fluent-color--organization-20"

            yield
                agent.AdditionalProperty
                |> entities item.memberKind ProcessCoreEntityValue.Annotation
                |> collection
                    "additional-properties"
                    "Additional Properties"
                    "swt:iconify-color swt:fluent-color--comment-multiple-20"

            yield
                agent.JobTitles
                |> entities item.memberKind ProcessCoreEntityValue.DefinedTerm
                |> collection "job-titles" "Job Titles" "swt:iconify swt:fluent--briefcase-20-regular"

        | ProcessCoreEntityValue.ScholarlyArticle article ->
            yield
                article.CreativeWorkStatus
                |> optionEntity item.memberKind ProcessCoreEntityValue.DefinedTerm
                |> collection "creative-work-status" "Creative Work Status" "swt:iconify swt:fluent--tag-20-regular"

            yield
                article.Authors
                |> entities item.memberKind ProcessCoreEntityValue.Agent
                |> collection "authors" "Authors" "swt:iconify-color swt:fluent-color--person-20"

            yield
                article.AdditionalProperty
                |> entities item.memberKind ProcessCoreEntityValue.Annotation
                |> collection
                    "additional-properties"
                    "Additional Properties"
                    "swt:iconify-color swt:fluent-color--comment-multiple-20"

        | ProcessCoreEntityValue.DefinedTerm _
        | ProcessCoreEntityValue.Organization _ -> ()
    |]
    |> Array.filter (fun folder -> not (Array.isEmpty folder.children))

let datasetNodes (datasets: ProcessCoreEntity array) : TreeNode<ProcessCoreEntity> array =
    datasets |> Array.mapi (entityNode [] "datasets")

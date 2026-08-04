namespace Swate.Components.Page.ArcObjectEditor

open Fable.Core
open Feliz
open ProcessCore
open Swate.Components.ProcessCore.UseProcessCore
open Swate.Components.ProcessCore
open Swate.Components
open Swate.Components.Page.Metadata
open Swate.Components.ProcessCore.ObjectGraph
open Swate.Components.Page.Metadata.FormComponents.ImportCatalogContext
open Swate.Components.Page.ObjectBrowser
open Swate.Components.Page.ObjectBrowser.Types
open Swate.Components.Primitive.ErrorModal.Context

module private ArcObjectEditorTypes =

    type ArcParent =
        | DatasetParent of Dataset
        | ProcessParent of Process
        | SampleParent of Sample
        | DataParent of Data
        | RecipeParent of Recipe
        | ArticleParent of ScholarlyArticle
        | AgentParent of Agent
        | DataContextParent of DataContext

open ArcObjectEditorTypes

module private ArcObjectEditorHelper =

    let private replaceMatchingAtOriginalIndices
        (items: ResizeArray<'T>, remove: 'T -> unit, add: 'T -> unit)
        isCurrent
        (updated: 'T)
        =
        let matches = items |> Seq.indexed |> Seq.filter (snd >> isCurrent) |> Seq.toArray

        for originalIndex, current in matches do
            remove current
            add updated

            let appendedIndex = items.Count - 1
            let appended = items.[appendedIndex]
            items.RemoveAt appendedIndex
            items.Insert(originalIndex, appended)

        matches.Length

    let private replaceAtOriginalIndex operations current updated =
        let replaced =
            replaceMatchingAtOriginalIndices operations (fun item -> obj.ReferenceEquals(item, current)) updated

        if replaced = 0 then
            invalidOp "The selected metadata object is no longer part of its parent."

    let replaceDatasetChild
        (parent: Dataset)
        (currentValue: ProcessCoreEntityValue)
        (updatedValue: ProcessCoreEntityValue)
        =
        match currentValue, updatedValue with
        | ProcessCoreEntityValue.Dataset current, ProcessCoreEntityValue.Dataset updated ->
            updated.PartOf <- None

            replaceAtOriginalIndex (parent.HasPart, parent.RemovePart, parent.AddPart) current updated
        | ProcessCoreEntityValue.Process current, ProcessCoreEntityValue.Process updated ->
            updated.ProcessOf <- None

            replaceAtOriginalIndex (parent.Processes, parent.RemoveProcess, parent.AddProcess) current updated
        | ProcessCoreEntityValue.Data current, ProcessCoreEntityValue.Data updated ->
            replaceAtOriginalIndex (parent.DataFiles, parent.RemoveDataFile, parent.AddDataFile) current updated
        | ProcessCoreEntityValue.Agent current, ProcessCoreEntityValue.Agent updated ->
            replaceAtOriginalIndex (parent.Agents, parent.RemoveAgent, parent.AddAgent) current updated
        | ProcessCoreEntityValue.ScholarlyArticle current, ProcessCoreEntityValue.ScholarlyArticle updated ->
            replaceAtOriginalIndex (parent.Citations, parent.RemoveCitation, parent.AddCitation) current updated
        | ProcessCoreEntityValue.DataContext current, ProcessCoreEntityValue.DataContext updated ->
            replaceAtOriginalIndex
                (parent.DataContexts, parent.RemoveDataContext, parent.AddDataContext)
                current
                updated
        | ProcessCoreEntityValue.Annotation current, ProcessCoreEntityValue.Annotation updated ->
            replaceAtOriginalIndex
                (parent.AdditionalProperty, parent.RemoveAdditionalProperty, parent.AddAdditionalProperty)
                current
                updated
        | _ -> invalidOp "The selected metadata object is not a direct member of this dataset."

    let replaceProcessChild
        (parent: Process)
        (currentValue: ProcessCoreEntityValue)
        (updatedValue: ProcessCoreEntityValue)
        =
        match currentValue, updatedValue with
        | ProcessCoreEntityValue.Recipe current, ProcessCoreEntityValue.Recipe updated when
            parent.ExecutesProtocol
            |> Option.exists (fun item -> obj.ReferenceEquals(item, current))
            ->
            parent.ExecutesProtocol <- Some updated
        | ProcessCoreEntityValue.Annotation current, ProcessCoreEntityValue.Annotation updated ->
            replaceAtOriginalIndex
                (parent.ParameterValue, parent.RemoveParameterValue, parent.AddParameterValue)
                current
                updated
        | _ ->
            let currentNode, updatedNode =
                match currentValue, updatedValue with
                | ProcessCoreEntityValue.Sample current, ProcessCoreEntityValue.Sample updated ->
                    SampleNode current, SampleNode updated
                | ProcessCoreEntityValue.Data current, ProcessCoreEntityValue.Data updated ->
                    DataNode current, DataNode updated
                | _ -> invalidOp "Only sample, data, or annotation children can be updated from process metadata."

            let isCurrentNode node =
                match node, currentNode with
                | SampleNode candidate, SampleNode current -> obj.ReferenceEquals(candidate, current)
                | DataNode candidate, DataNode current -> obj.ReferenceEquals(candidate, current)
                | _ -> false

            let replaceIn (nodes: ResizeArray<IONode>) remove add =
                replaceMatchingAtOriginalIndices (nodes, remove, add) isCurrentNode updatedNode

            let replacedInputs = replaceIn parent.Inputs parent.RemoveInput parent.AddInput
            let replacedOutputs = replaceIn parent.Outputs parent.RemoveOutput parent.AddOutput

            if replacedInputs + replacedOutputs = 0 then
                invalidOp "The selected metadata object is no longer an input or output of this process."

    let replaceSampleChild (parent: Sample) currentValue updatedValue =
        match currentValue, updatedValue with
        | ProcessCoreEntityValue.Annotation current, ProcessCoreEntityValue.Annotation updated ->
            replaceAtOriginalIndex
                (parent.AdditionalProperty, parent.RemoveAdditionalProperty, parent.AddAdditionalProperty)
                current
                updated
        | _ -> invalidOp "Only annotations can be nested in sample metadata."

    let replaceDataChild (parent: Data) currentValue updatedValue =
        match currentValue, updatedValue with
        | ProcessCoreEntityValue.Data current, ProcessCoreEntityValue.Data updated ->
            replaceAtOriginalIndex (parent.HasPart, parent.RemovePart, parent.AddPart) current updated
        | ProcessCoreEntityValue.Annotation current, ProcessCoreEntityValue.Annotation updated ->
            replaceAtOriginalIndex
                (parent.AdditionalProperty, parent.RemoveAdditionalProperty, parent.AddAdditionalProperty)
                current
                updated
        | _ -> invalidOp "Only data or annotation children can be nested in data metadata."

    let replaceRecipeChild (parent: Recipe) currentValue updatedValue =
        match currentValue, updatedValue with
        | ProcessCoreEntityValue.DefinedTerm current, ProcessCoreEntityValue.DefinedTerm updated when
            parent.IntendedUse
            |> Option.exists (fun item -> obj.ReferenceEquals(item, current))
            ->
            parent.IntendedUse <- Some updated
        | ProcessCoreEntityValue.FormalParameter current, ProcessCoreEntityValue.FormalParameter updated ->
            replaceAtOriginalIndex (parent.Parameters, parent.RemoveParameter, parent.AddParameter) current updated
        | ProcessCoreEntityValue.Annotation current, ProcessCoreEntityValue.Annotation updated ->
            let componentCount =
                replaceMatchingAtOriginalIndices
                    (parent.Components, parent.RemoveComponent, parent.AddComponent)
                    (fun item -> obj.ReferenceEquals(item, current))
                    updated

            let propertyCount =
                replaceMatchingAtOriginalIndices
                    (parent.AdditionalProperty, parent.RemoveAdditionalProperty, parent.AddAdditionalProperty)
                    (fun item -> obj.ReferenceEquals(item, current))
                    updated

            if componentCount + propertyCount = 0 then
                invalidOp "The annotation is no longer part of this recipe."
        | _ -> invalidOp "The selected value cannot be nested in recipe metadata."

    let replaceArticleChild (parent: ScholarlyArticle) currentValue updatedValue =
        match currentValue, updatedValue with
        | ProcessCoreEntityValue.DefinedTerm current, ProcessCoreEntityValue.DefinedTerm updated when
            parent.CreativeWorkStatus
            |> Option.exists (fun item -> obj.ReferenceEquals(item, current))
            ->
            parent.CreativeWorkStatus <- Some updated
        | ProcessCoreEntityValue.Agent current, ProcessCoreEntityValue.Agent updated ->
            replaceAtOriginalIndex (parent.Authors, parent.RemoveAuthor, parent.AddAuthor) current updated
        | ProcessCoreEntityValue.Annotation current, ProcessCoreEntityValue.Annotation updated ->
            replaceAtOriginalIndex
                (parent.AdditionalProperty, parent.RemoveAdditionalProperty, parent.AddAdditionalProperty)
                current
                updated
        | _ -> invalidOp "Only authors or annotations can be nested in article metadata."

    let replaceAgentChild (parent: Agent) currentValue updatedValue =
        match currentValue, updatedValue with
        | ProcessCoreEntityValue.Annotation current, ProcessCoreEntityValue.Annotation updated ->
            replaceAtOriginalIndex
                (parent.AdditionalProperty, parent.RemoveAdditionalProperty, parent.AddAdditionalProperty)
                current
                updated
        | ProcessCoreEntityValue.DefinedTerm current, ProcessCoreEntityValue.DefinedTerm updated ->
            replaceAtOriginalIndex (parent.JobTitles, parent.RemoveJobTitle, parent.AddJobTitle) current updated
        | ProcessCoreEntityValue.Organization current, ProcessCoreEntityValue.Organization updated when
            parent.Affiliation
            |> Option.exists (fun item -> obj.ReferenceEquals(item, current))
            ->
            parent.Affiliation <- Some updated
        | _ -> invalidOp "The selected value cannot be nested in agent metadata."

    let replaceDataContextChild (parent: DataContext) currentValue updatedValue =
        match currentValue, updatedValue with
        | ProcessCoreEntityValue.Data current, ProcessCoreEntityValue.Data updated when
            obj.ReferenceEquals(parent.Data, current)
            ->
            parent.Data <- updated
        | ProcessCoreEntityValue.DefinedTerm current, ProcessCoreEntityValue.DefinedTerm updated ->
            if
                parent.Explication
                |> Option.exists (fun item -> obj.ReferenceEquals(item, current))
            then
                parent.Explication <- Some updated
            elif
                parent.ObjectType
                |> Option.exists (fun item -> obj.ReferenceEquals(item, current))
            then
                parent.ObjectType <- Some updated
            elif parent.Unit |> Option.exists (fun item -> obj.ReferenceEquals(item, current)) then
                parent.Unit <- Some updated
            else
                invalidOp "The defined term is no longer part of this data context."
        | _ -> invalidOp "The selected value cannot be nested in data-context metadata."

    let replaceFormalParameterChild (parent: FormalParameter) currentValue updatedValue =
        match currentValue, updatedValue with
        | ProcessCoreEntityValue.DefinedTerm current, ProcessCoreEntityValue.DefinedTerm updated when
            parent.DefaultValue
            |> Option.exists (fun item -> obj.ReferenceEquals(item, current))
            ->
            parent.DefaultValue <- Some updated
        | _ -> invalidOp "Only a default defined term can be nested in formal-parameter metadata."

    let replaceAnnotationChild (parent: Annotation) currentValue updatedValue =
        match currentValue, updatedValue with
        | ProcessCoreEntityValue.FormalParameter current, ProcessCoreEntityValue.FormalParameter updated when
            parent.InstanceOf
            |> Option.exists (fun item -> obj.ReferenceEquals(item, current))
            ->
            parent.InstanceOf <- Some updated
        | _ -> invalidOp "Only a formal parameter can be nested in annotation metadata."

    let replaceChild parentValue currentValue updatedValue =
        match parentValue with
        | ProcessCoreEntityValue.Dataset parent -> replaceDatasetChild parent currentValue updatedValue
        | ProcessCoreEntityValue.Process parent -> replaceProcessChild parent currentValue updatedValue
        | ProcessCoreEntityValue.Sample parent -> replaceSampleChild parent currentValue updatedValue
        | ProcessCoreEntityValue.Data parent -> replaceDataChild parent currentValue updatedValue
        | ProcessCoreEntityValue.Recipe parent -> replaceRecipeChild parent currentValue updatedValue
        | ProcessCoreEntityValue.ScholarlyArticle parent -> replaceArticleChild parent currentValue updatedValue
        | ProcessCoreEntityValue.Agent parent -> replaceAgentChild parent currentValue updatedValue
        | ProcessCoreEntityValue.DataContext parent -> replaceDataContextChild parent currentValue updatedValue
        | ProcessCoreEntityValue.FormalParameter parent -> replaceFormalParameterChild parent currentValue updatedValue
        | ProcessCoreEntityValue.Annotation parent -> replaceAnnotationChild parent currentValue updatedValue
        | _ -> invalidOp "The selected metadata object cannot contain nested metadata."

    let private containsReference item items =
        items |> Seq.exists (fun candidate -> obj.ReferenceEquals(candidate, item))

    let private parentsInArc (arc: ARC) =
        let datasets = datasetsIncludingRoot arc
        let processes = arc.AllProcesses()
        let data = arc.AllData()
        let articles = arc.AllCitations()
        let recipes = recipes arc
        let agents = arc.AllAgents()

        seq {
            yield! datasets |> Seq.map DatasetParent
            yield! processes |> Seq.map ProcessParent
            yield! arc.AllSamples() |> Seq.map SampleParent
            yield! data |> Seq.map DataParent
            yield! recipes |> Seq.map RecipeParent
            yield! articles |> Seq.map ArticleParent
            yield! agents |> Seq.map AgentParent
            yield! arc.AllDataContexts() |> Seq.map DataContextParent
        }

    let private parentContains parent child =
        match parent, child with
        | DatasetParent parent, ProcessCoreEntityValue.Dataset current -> containsReference current parent.HasPart
        | DatasetParent parent, ProcessCoreEntityValue.Process current -> containsReference current parent.Processes
        | DatasetParent parent, ProcessCoreEntityValue.Data current -> containsReference current parent.DataFiles
        | DatasetParent parent, ProcessCoreEntityValue.Agent current -> containsReference current parent.Agents
        | DatasetParent parent, ProcessCoreEntityValue.ScholarlyArticle current ->
            containsReference current parent.Citations
        | DatasetParent parent, ProcessCoreEntityValue.DataContext current ->
            containsReference current parent.DataContexts
        | DatasetParent parent, ProcessCoreEntityValue.Annotation current ->
            containsReference current parent.AdditionalProperty
        | ProcessParent parent, ProcessCoreEntityValue.Recipe current ->
            parent.ExecutesProtocol
            |> Option.exists (fun item -> obj.ReferenceEquals(item, current))
        | ProcessParent parent, ProcessCoreEntityValue.Annotation current ->
            containsReference current parent.ParameterValue
        | ProcessParent parent, ProcessCoreEntityValue.Sample current ->
            Seq.append parent.Inputs parent.Outputs
            |> Seq.exists (
                function
                | SampleNode item -> obj.ReferenceEquals(item, current)
                | DataNode _ -> false
            )
        | ProcessParent parent, ProcessCoreEntityValue.Data current ->
            Seq.append parent.Inputs parent.Outputs
            |> Seq.exists (
                function
                | DataNode item -> obj.ReferenceEquals(item, current)
                | SampleNode _ -> false
            )
        | SampleParent parent, ProcessCoreEntityValue.Annotation current ->
            containsReference current parent.AdditionalProperty
        | DataParent parent, ProcessCoreEntityValue.Data current -> containsReference current parent.HasPart
        | DataParent parent, ProcessCoreEntityValue.Annotation current ->
            containsReference current parent.AdditionalProperty
        | RecipeParent parent, ProcessCoreEntityValue.Annotation current ->
            containsReference current parent.Components
            || containsReference current parent.AdditionalProperty
        | ArticleParent parent, ProcessCoreEntityValue.Agent current -> containsReference current parent.Authors
        | ArticleParent parent, ProcessCoreEntityValue.Annotation current ->
            containsReference current parent.AdditionalProperty
        | AgentParent parent, ProcessCoreEntityValue.Organization current ->
            parent.Affiliation
            |> Option.exists (fun item -> obj.ReferenceEquals(item, current))
        | AgentParent parent, ProcessCoreEntityValue.Annotation current ->
            containsReference current parent.AdditionalProperty
        | DataContextParent parent, ProcessCoreEntityValue.Data current -> obj.ReferenceEquals(parent.Data, current)
        | _ -> false

    let private replaceParentChild parent currentValue updatedValue =
        match parent with
        | DatasetParent parent -> replaceDatasetChild parent currentValue updatedValue
        | ProcessParent parent -> replaceProcessChild parent currentValue updatedValue
        | SampleParent parent -> replaceSampleChild parent currentValue updatedValue
        | DataParent parent -> replaceDataChild parent currentValue updatedValue
        | RecipeParent parent -> replaceRecipeChild parent currentValue updatedValue
        | ArticleParent parent -> replaceArticleChild parent currentValue updatedValue
        | AgentParent parent -> replaceAgentChild parent currentValue updatedValue
        | DataContextParent parent -> replaceDataContextChild parent currentValue updatedValue

    let private sameEntityType left right =
        match left, right with
        | ProcessCoreEntityValue.Dataset _, ProcessCoreEntityValue.Dataset _
        | ProcessCoreEntityValue.Process _, ProcessCoreEntityValue.Process _
        | ProcessCoreEntityValue.Sample _, ProcessCoreEntityValue.Sample _
        | ProcessCoreEntityValue.Data _, ProcessCoreEntityValue.Data _
        | ProcessCoreEntityValue.Recipe _, ProcessCoreEntityValue.Recipe _
        | ProcessCoreEntityValue.FormalParameter _, ProcessCoreEntityValue.FormalParameter _
        | ProcessCoreEntityValue.DefinedTerm _, ProcessCoreEntityValue.DefinedTerm _
        | ProcessCoreEntityValue.Annotation _, ProcessCoreEntityValue.Annotation _
        | ProcessCoreEntityValue.DataContext _, ProcessCoreEntityValue.DataContext _
        | ProcessCoreEntityValue.Agent _, ProcessCoreEntityValue.Agent _
        | ProcessCoreEntityValue.Organization _, ProcessCoreEntityValue.Organization _
        | ProcessCoreEntityValue.ScholarlyArticle _, ProcessCoreEntityValue.ScholarlyArticle _ -> true
        | _ -> false

    let replaceRootEntity (arc: ARC) currentValue updatedValue =
        if not (sameEntityType currentValue updatedValue) then
            invalidOp "The updated metadata type does not match the selected object."

        let parents =
            parentsInArc arc
            |> Seq.filter (fun parent -> parentContains parent currentValue)
            |> Seq.toArray

        if parents.Length = 0 then
            invalidOp "The selected metadata object is no longer part of this ARC."

        parents
        |> Array.iter (fun parent -> replaceParentChild parent currentValue updatedValue)

[<Erase; Mangle(false)>]
type ArcObjectEditor =

    [<ReactComponent(true)>]
    static member ArcObjectEditor
        (
            arc: ARC,
            arcView: Swate.Components.ProcessCore.Types.ArcView,
            mutate: (ARC -> unit) -> unit,
            kind: MemberKind,
            ?initialEntity: ProcessCoreEntity,
            ?scopedEntities: ProcessCoreEntity array,
            ?onOpenInTableEditor: ProcessCoreEntity -> unit,
            ?runAsyncMutation: (unit -> unit) -> Fable.Core.JS.Promise<unit>
        ) =
        let initialEntityKey =
            initialEntity |> Option.map _.key |> Option.defaultValue "root"

        Html.div [
            prop.key $"{kind}/{initialEntityKey}"
            prop.className "swt:size-full"
            prop.children [
                ArcObjectEditorContent.ArcObjectEditorContent(
                    arc,
                    arcView,
                    mutate,
                    kind,
                    initialEntity,
                    scopedEntities,
                    onOpenInTableEditor,
                    runAsyncMutation
                )
            ]
        ]

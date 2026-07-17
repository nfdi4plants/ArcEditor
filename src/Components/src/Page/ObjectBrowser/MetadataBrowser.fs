namespace Swate.Components.Page.ObjectBrowser

open Fable.Core
open Feliz
open ProcessCore
open Swate.Components
open Swate.Components.Page.Metadata
open Swate.Components.Page.ObjectBrowser.Types
open Swate.Components.Primitive.ErrorModal.Context

module private MetadataBrowserHelper =

    let private nonEmptyOr fallback value =
        if System.String.IsNullOrWhiteSpace value then
            fallback
        else
            value

    let valueLabel value =
        match value with
        | ProcessCoreEntityValue.Dataset dataset ->
            dataset.Title
            |> Option.filter (System.String.IsNullOrWhiteSpace >> not)
            |> Option.defaultValue dataset.Identifier
        | ProcessCoreEntityValue.Process processObject -> nonEmptyOr "Unnamed process" processObject.Name
        | ProcessCoreEntityValue.Sample sample -> nonEmptyOr "Unnamed sample" sample.Name
        | ProcessCoreEntityValue.Data data -> nonEmptyOr "Unnamed data" data.Name
        | ProcessCoreEntityValue.Recipe recipe -> recipe.Name |> Option.defaultValue "Recipe"
        | ProcessCoreEntityValue.FormalParameter parameter -> nonEmptyOr "Unnamed formal parameter" parameter.Name
        | ProcessCoreEntityValue.DefinedTerm term -> nonEmptyOr "Unnamed defined term" term.Name
        | ProcessCoreEntityValue.Annotation annotation -> nonEmptyOr "Unnamed annotation" annotation.Name
        | ProcessCoreEntityValue.DataContext dataContext ->
            dataContext.Label |> Option.defaultValue dataContext.Data.Name
        | ProcessCoreEntityValue.Agent agent ->
            [
                agent.GivenName
                agent.FamilyName |> Option.defaultValue ""
            ]
            |> List.filter (System.String.IsNullOrWhiteSpace >> not)
            |> String.concat " "
        | ProcessCoreEntityValue.Organization organization -> organization.Name
        | ProcessCoreEntityValue.ScholarlyArticle article -> article.Headline

    let replaceDatasetChild
        (parent: Dataset)
        (currentValue: ProcessCoreEntityValue)
        (updatedValue: ProcessCoreEntityValue)
        =
        match currentValue, updatedValue with
        | ProcessCoreEntityValue.Dataset current, ProcessCoreEntityValue.Dataset updated ->
            parent.RemovePart current
            updated.PartOf <- None
            parent.AddPart updated
        | ProcessCoreEntityValue.Process current, ProcessCoreEntityValue.Process updated ->
            parent.RemoveProcess current
            updated.ProcessOf <- None
            parent.AddProcess updated
        | ProcessCoreEntityValue.Data current, ProcessCoreEntityValue.Data updated ->
            parent.RemoveDataFile current
            parent.AddDataFile updated
        | ProcessCoreEntityValue.Agent current, ProcessCoreEntityValue.Agent updated ->
            parent.RemoveAgent current
            parent.AddAgent updated
        | ProcessCoreEntityValue.ScholarlyArticle current, ProcessCoreEntityValue.ScholarlyArticle updated ->
            parent.RemoveCitation current
            parent.AddCitation updated
        | ProcessCoreEntityValue.DataContext current, ProcessCoreEntityValue.DataContext updated ->
            parent.RemoveDataContext current
            parent.AddDataContext updated
        | ProcessCoreEntityValue.Annotation current, ProcessCoreEntityValue.Annotation updated ->
            parent.RemoveAdditionalProperty current
            parent.AddAdditionalProperty updated
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
            parent.RemoveParameterValue current
            parent.AddParameterValue updated
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
                let matches = nodes |> Seq.filter isCurrentNode |> Seq.toArray

                for node in matches do
                    remove node
                    add updatedNode

                matches.Length

            let replacedInputs = replaceIn parent.Inputs parent.RemoveInput parent.AddInput
            let replacedOutputs = replaceIn parent.Outputs parent.RemoveOutput parent.AddOutput

            if replacedInputs + replacedOutputs = 0 then
                invalidOp "The selected metadata object is no longer an input or output of this process."

    let replaceSampleChild (parent: Sample) currentValue updatedValue =
        match currentValue, updatedValue with
        | ProcessCoreEntityValue.Annotation current, ProcessCoreEntityValue.Annotation updated ->
            parent.RemoveAdditionalProperty current
            parent.AddAdditionalProperty updated
        | _ -> invalidOp "Only annotations can be nested in sample metadata."

    let replaceDataChild (parent: Data) currentValue updatedValue =
        match currentValue, updatedValue with
        | ProcessCoreEntityValue.Data current, ProcessCoreEntityValue.Data updated ->
            parent.RemovePart current
            parent.AddPart updated
        | ProcessCoreEntityValue.Annotation current, ProcessCoreEntityValue.Annotation updated ->
            parent.RemoveAdditionalProperty current
            parent.AddAdditionalProperty updated
        | _ -> invalidOp "Only data or annotation children can be nested in data metadata."

    let replaceRecipeChild (parent: Recipe) currentValue updatedValue =
        match currentValue, updatedValue with
        | ProcessCoreEntityValue.DefinedTerm current, ProcessCoreEntityValue.DefinedTerm updated when
            parent.IntendedUse
            |> Option.exists (fun item -> obj.ReferenceEquals(item, current))
            ->
            parent.IntendedUse <- Some updated
        | ProcessCoreEntityValue.FormalParameter current, ProcessCoreEntityValue.FormalParameter updated ->
            parent.RemoveParameter current
            parent.AddParameter updated
        | ProcessCoreEntityValue.Annotation current, ProcessCoreEntityValue.Annotation updated ->
            let mutable replaced = false

            if parent.Components |> Seq.exists (fun item -> obj.ReferenceEquals(item, current)) then
                parent.RemoveComponent current
                parent.AddComponent updated
                replaced <- true

            if
                parent.AdditionalProperty
                |> Seq.exists (fun item -> obj.ReferenceEquals(item, current))
            then
                parent.RemoveAdditionalProperty current
                parent.AddAdditionalProperty updated
                replaced <- true

            if not replaced then
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
            parent.RemoveAuthor current
            parent.AddAuthor updated
        | ProcessCoreEntityValue.Annotation current, ProcessCoreEntityValue.Annotation updated ->
            parent.RemoveAdditionalProperty current
            parent.AddAdditionalProperty updated
        | _ -> invalidOp "Only authors or annotations can be nested in article metadata."

    let replaceAgentChild (parent: Agent) currentValue updatedValue =
        match currentValue, updatedValue with
        | ProcessCoreEntityValue.Annotation current, ProcessCoreEntityValue.Annotation updated ->
            parent.RemoveAdditionalProperty current
            parent.AddAdditionalProperty updated
        | ProcessCoreEntityValue.DefinedTerm current, ProcessCoreEntityValue.DefinedTerm updated ->
            parent.RemoveJobTitle current
            parent.AddJobTitle updated
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

    let replaceRootDataset (currentValue: ProcessCoreEntityValue) (updatedValue: ProcessCoreEntityValue) =
        match currentValue, updatedValue with
        | ProcessCoreEntityValue.Dataset current, ProcessCoreEntityValue.Dataset updated ->
            match current.PartOf with
            | Some parent ->
                parent.RemovePart current
                updated.PartOf <- None
                parent.AddPart updated
            | None -> invalidOp "The ARC root dataset cannot be replaced from the dataset browser."
        | _ -> invalidOp "Only datasets can be opened from the dataset browser."

[<Erase; Mangle(false)>]
type MetadataBrowser =

    [<ReactComponent(true)>]
    static member Main(arcStateCtx: StateUpdaterContext<ARC option>, kind: MemberKind) =
        let navigationPath, setNavigationPath =
            React.useState<ProcessCoreEntityValue list> []

        let errorModal = useErrorModalCtx ()

        React.useEffect ((fun () -> setNavigationPath []), [| box kind |])

        let openRoot entity = setNavigationPath [ entity.value ]

        let navigate value =
            setNavigationPath (navigationPath @ [ value ])

        let goBack () =
            match navigationPath with
            | [] -> ()
            | [ _ ] -> setNavigationPath []
            | path -> setNavigationPath (path |> List.take (path.Length - 1))

        let updateCurrent updatedValue =
            match arcStateCtx.state, navigationPath with
            | Some arc, _ :: _ ->
                try
                    let currentValue = List.last navigationPath
                    let parentPath = navigationPath |> List.take (navigationPath.Length - 1)

                    match List.tryLast parentPath with
                    | Some(ProcessCoreEntityValue.Dataset parent) ->
                        MetadataBrowserHelper.replaceDatasetChild parent currentValue updatedValue
                    | Some(ProcessCoreEntityValue.Process parent) ->
                        MetadataBrowserHelper.replaceProcessChild parent currentValue updatedValue
                    | Some(ProcessCoreEntityValue.Sample parent) ->
                        MetadataBrowserHelper.replaceSampleChild parent currentValue updatedValue
                    | Some(ProcessCoreEntityValue.Data parent) ->
                        MetadataBrowserHelper.replaceDataChild parent currentValue updatedValue
                    | Some(ProcessCoreEntityValue.Recipe parent) ->
                        MetadataBrowserHelper.replaceRecipeChild parent currentValue updatedValue
                    | Some(ProcessCoreEntityValue.ScholarlyArticle parent) ->
                        MetadataBrowserHelper.replaceArticleChild parent currentValue updatedValue
                    | Some(ProcessCoreEntityValue.Agent parent) ->
                        MetadataBrowserHelper.replaceAgentChild parent currentValue updatedValue
                    | Some(ProcessCoreEntityValue.DataContext parent) ->
                        MetadataBrowserHelper.replaceDataContextChild parent currentValue updatedValue
                    | Some(ProcessCoreEntityValue.FormalParameter parent) ->
                        MetadataBrowserHelper.replaceFormalParameterChild parent currentValue updatedValue
                    | Some(ProcessCoreEntityValue.Annotation parent) ->
                        MetadataBrowserHelper.replaceAnnotationChild parent currentValue updatedValue
                    | Some _ -> invalidOp "The selected metadata object cannot contain nested metadata."
                    | None -> MetadataBrowserHelper.replaceRootDataset currentValue updatedValue

                    setNavigationPath (parentPath @ [ updatedValue ])
                    arcStateCtx.setStateUpdater (fun _ -> Some arc)
                with error ->
                    errorModal.report error.Message
            | _ -> ()

        let metadataView value =
            match value with
            | ProcessCoreEntityValue.Dataset dataset ->
                DatasetMetadata.DatasetView(
                    dataset,
                    (ProcessCoreEntityValue.Dataset >> updateCurrent),
                    onNavigate = navigate
                )
            | ProcessCoreEntityValue.Process processObject ->
                ProcessMetadata.ProcessView(
                    processObject,
                    (ProcessCoreEntityValue.Process >> updateCurrent),
                    onNavigate = navigate
                )
            | ProcessCoreEntityValue.Sample sample ->
                SampleMetadata.SampleView(
                    sample,
                    (ProcessCoreEntityValue.Sample >> updateCurrent),
                    onNavigate = navigate
                )
            | ProcessCoreEntityValue.Data data ->
                DataMetadata.DataView(data, (ProcessCoreEntityValue.Data >> updateCurrent), onNavigate = navigate)
            | ProcessCoreEntityValue.Recipe recipe ->
                RecipeMetadata.RecipeView(
                    recipe,
                    (ProcessCoreEntityValue.Recipe >> updateCurrent),
                    onNavigate = navigate
                )
            | ProcessCoreEntityValue.FormalParameter parameter ->
                FormalParameterMetadata.FormalParameterView(
                    parameter,
                    (ProcessCoreEntityValue.FormalParameter >> updateCurrent),
                    onNavigate = navigate
                )
            | ProcessCoreEntityValue.DefinedTerm term ->
                DefinedTermMetadata.DefinedTermView(term, ProcessCoreEntityValue.DefinedTerm >> updateCurrent)
            | ProcessCoreEntityValue.Agent agent ->
                AgentMetadata.AgentView(agent, (ProcessCoreEntityValue.Agent >> updateCurrent), onNavigate = navigate)
            | ProcessCoreEntityValue.Organization organization ->
                OrganizationMetadata.OrganizationView(
                    organization,
                    ProcessCoreEntityValue.Organization >> updateCurrent
                )
            | ProcessCoreEntityValue.ScholarlyArticle article ->
                ScholarlyArticleMetadata.ScholarlyArticleView(
                    article,
                    (ProcessCoreEntityValue.ScholarlyArticle >> updateCurrent),
                    onNavigate = navigate
                )
            | ProcessCoreEntityValue.DataContext dataContext ->
                DataContextMetadata.DataContextView(
                    dataContext,
                    (ProcessCoreEntityValue.DataContext >> updateCurrent),
                    onNavigate = navigate
                )
            | ProcessCoreEntityValue.Annotation annotation ->
                AnnotationMetadata.AnnotationView(
                    annotation,
                    (ProcessCoreEntityValue.Annotation >> updateCurrent),
                    onNavigate = navigate
                )

        match List.tryLast navigationPath with
        | None -> ObjectBrowser.Main(arcStateCtx, kind, onOpen = openRoot)
        | Some currentValue ->
            let backLabel =
                match navigationPath |> List.rev |> List.tryItem 1 with
                | Some parent -> $"Back to {MetadataBrowserHelper.valueLabel parent}"
                | None -> $"Back to {(MemberCatalog.find kind).label}"

            Html.section [
                prop.testId "process-core-metadata-browser"
                prop.className "swt:size-full swt:min-h-0 swt:overflow-y-auto swt:bg-base-200"
                prop.children [
                    Html.div [
                        prop.className "swt:sticky swt:top-0 swt:z-10 swt:bg-base-200 swt:px-6 swt:pt-4"
                        prop.children [
                            Html.button [
                                prop.testId "process-core-metadata-back"
                                prop.className "swt:btn swt:btn-ghost swt:btn-sm"
                                prop.ariaLabel backLabel
                                prop.onClick (fun _ -> goBack ())
                                prop.children [
                                    Html.i [
                                        prop.className "swt:iconify swt:fluent--arrow-left-20-regular swt:size-5"
                                    ]
                                    Html.span backLabel
                                ]
                            ]
                        ]
                    ]
                    metadataView currentValue
                ]
            ]

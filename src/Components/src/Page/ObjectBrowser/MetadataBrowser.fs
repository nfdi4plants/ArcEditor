namespace Swate.Components.Page.ObjectBrowser

open Fable.Core
open Feliz
open ProcessCore
open Swate.Components
open Swate.Components.Page.Metadata
open Swate.Components.Page.ObjectBrowser.Types
open Swate.Components.Primitive.ErrorModal.Context

module private MetadataBrowserHelper =

    let valueLabel value =
        match value with
        | ProcessCoreEntityValue.Dataset dataset ->
            dataset.Title
            |> Option.filter (System.String.IsNullOrWhiteSpace >> not)
            |> Option.defaultValue dataset.Identifier
        | ProcessCoreEntityValue.Process processObject -> processObject.Name
        | ProcessCoreEntityValue.Sample sample -> sample.Name
        | ProcessCoreEntityValue.Data data -> data.Name
        | ProcessCoreEntityValue.Recipe recipe -> recipe.Name |> Option.defaultValue "Recipe"
        | ProcessCoreEntityValue.Annotation annotation -> annotation.Name
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

        let openRoot entity =
            match entity.value with
            | ProcessCoreEntityValue.Dataset _ as dataset -> setNavigationPath [ dataset ]
            | _ -> ()

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
                    | Some _ -> invalidOp "Nested metadata navigation is only supported for dataset members."
                    | None -> MetadataBrowserHelper.replaceRootDataset currentValue updatedValue

                    setNavigationPath (parentPath @ [ updatedValue ])
                    arcStateCtx.setStateUpdater (fun _ -> Some arc)
                with error ->
                    errorModal.report error.Message
            | _ -> ()

        let metadataView value =
            match value with
            | ProcessCoreEntityValue.Dataset dataset ->
                DatasetMetadata.DatasetMetadata(
                    dataset,
                    (ProcessCoreEntityValue.Dataset >> updateCurrent),
                    onNavigate = navigate
                )
            | ProcessCoreEntityValue.Process processObject ->
                ProcessMetadata.ProcessMetadata(processObject, ProcessCoreEntityValue.Process >> updateCurrent)
            | ProcessCoreEntityValue.Data data ->
                DataMetadata.DataMetadata(data, ProcessCoreEntityValue.Data >> updateCurrent)
            | ProcessCoreEntityValue.Agent agent ->
                AgentMetadata.AgentMetadata(agent, ProcessCoreEntityValue.Agent >> updateCurrent)
            | ProcessCoreEntityValue.ScholarlyArticle article ->
                ScholarlyArticleMetadata.ScholarlyArticleMetadata(
                    article,
                    ProcessCoreEntityValue.ScholarlyArticle >> updateCurrent
                )
            | ProcessCoreEntityValue.DataContext dataContext ->
                DataContextMetadata.DataContextMetadata(
                    dataContext,
                    ProcessCoreEntityValue.DataContext >> updateCurrent
                )
            | ProcessCoreEntityValue.Annotation annotation ->
                AnnotationMetadata.AnnotationMetadata(annotation, ProcessCoreEntityValue.Annotation >> updateCurrent)
            | _ -> Html.none

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

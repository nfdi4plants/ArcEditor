namespace Swate.Components.Page.ArcObjectEditor

open Fable.Core
open Feliz
open ProcessCore
open Swate.Components
open Swate.Components.Page.Metadata
open Swate.Components.Page.Metadata.FormComponents.ImportCatalogContext
open Swate.Components.Page.ObjectBrowser
open Swate.Components.Page.ObjectBrowser.Types
open Swate.Components.ProcessCore
open Swate.Components.Primitive.ErrorModal.Context
open Swate.Components.Primitive.Navbar

module private ArcObjectEditorContentHelper =

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
        | ProcessCoreEntityValue.Process value -> nonEmptyOr "Unnamed process" value.Name
        | ProcessCoreEntityValue.Sample value -> nonEmptyOr "Unnamed sample" value.Name
        | ProcessCoreEntityValue.Data value -> nonEmptyOr "Unnamed data" value.Name
        | ProcessCoreEntityValue.Recipe value -> value.Name |> Option.defaultValue "Recipe"
        | ProcessCoreEntityValue.FormalParameter value -> nonEmptyOr "Unnamed formal parameter" value.Name
        | ProcessCoreEntityValue.DefinedTerm value -> nonEmptyOr "Unnamed defined term" value.Name
        | ProcessCoreEntityValue.Annotation value -> nonEmptyOr "Unnamed annotation" value.Name
        | ProcessCoreEntityValue.DataContext value -> value.Label |> Option.defaultValue value.Data.Name
        | ProcessCoreEntityValue.Agent value ->
            [
                value.GivenName
                value.FamilyName |> Option.defaultValue ""
            ]
            |> List.filter (System.String.IsNullOrWhiteSpace >> not)
            |> String.concat " "
        | ProcessCoreEntityValue.Organization value -> value.Name
        | ProcessCoreEntityValue.ScholarlyArticle value -> value.Headline

[<Erase; Mangle(false)>]
type ArcObjectEditorContent =

    [<ReactComponent>]
    static member ArcObjectEditorContent
        (
            arc: ARC,
            arcView: Swate.Components.ProcessCore.Types.ArcView,
            mutate: (ARC -> unit) -> unit,
            kind: MemberKind,
            initialEntity: ProcessCoreEntity option,
            scopedEntities: ProcessCoreEntity array option,
            onOpenInTableEditor: (ProcessCoreEntity -> unit) option,
            runAsyncMutation: ((unit -> unit) -> Fable.Core.JS.Promise<unit>) option
        ) =
        let arcStateCtx: StateUpdaterContext<ARC option> = {
            state = Some arc
            setStateUpdater = fun update -> mutate (fun currentArc -> update (Some currentArc) |> ignore)
        }

        let navigationPath, setNavigationPath =
            React.useState<ProcessCoreEntityValue list> (
                initialEntity
                |> Option.map (fun entity -> [ entity.value ])
                |> Option.defaultValue []
            )

        let errorModal = useErrorModalCtx ()
        let revision, setRevision = React.useState 0
        let searchQuery, setSearchQuery = React.useState ""

        let importCatalog =
            React.useMemo ((fun () -> ImportCatalogContextHelper.create arc), [| box arc; box arcView |])

        let navigate value =
            setNavigationPath (navigationPath @ [ value ])

        let mutateWithErrorHandling mutation =
            try
                mutate mutation
                setRevision (revision + 1)
            with error ->
                errorModal.report error.Message

        let metadataView value =
            match value with
            | ProcessCoreEntityValue.Dataset dataset ->
                DatasetMetadata.DatasetView(dataset, arcView, mutateWithErrorHandling, onNavigate = navigate)
            | ProcessCoreEntityValue.Process processObject ->
                ProcessMetadata.ProcessView(
                    RendererModel.forProcess processObject arcView,
                    mutateWithErrorHandling,
                    onNavigate = navigate
                )
            | ProcessCoreEntityValue.Sample value ->
                SampleMetadata.SampleView(value, mutateWithErrorHandling, onNavigate = navigate)
            | ProcessCoreEntityValue.Data value ->
                DataMetadata.DataView(value, mutateWithErrorHandling, onNavigate = navigate)
            | ProcessCoreEntityValue.Recipe value ->
                RecipeMetadata.RecipeView(value, mutateWithErrorHandling, onNavigate = navigate)
            | ProcessCoreEntityValue.FormalParameter value ->
                FormalParameterMetadata.FormalParameterView(value, mutateWithErrorHandling, onNavigate = navigate)
            | ProcessCoreEntityValue.DefinedTerm value ->
                DefinedTermMetadata.DefinedTermView(value, mutateWithErrorHandling)
            | ProcessCoreEntityValue.Agent value ->
                AgentMetadata.AgentView(value, mutateWithErrorHandling, onNavigate = navigate)
            | ProcessCoreEntityValue.Organization value ->
                OrganizationMetadata.OrganizationView(value, mutateWithErrorHandling)
            | ProcessCoreEntityValue.ScholarlyArticle value ->
                ScholarlyArticleMetadata.ScholarlyArticleView(value, mutateWithErrorHandling, onNavigate = navigate)
            | ProcessCoreEntityValue.DataContext value ->
                DataContextMetadata.DataContextView(value, mutateWithErrorHandling, onNavigate = navigate)
            | ProcessCoreEntityValue.Annotation value ->
                AnnotationMetadata.AnnotationView(value, mutateWithErrorHandling, onNavigate = navigate)

        let currentValue = List.tryLast navigationPath
        let isMetadataActive = currentValue.IsSome
        let rootLabel = (MemberCatalog.find kind).label

        let backButton =
            match currentValue with
            | None -> Html.none
            | Some _ ->
                let backLabel =
                    navigationPath
                    |> List.rev
                    |> List.tryItem 1
                    |> Option.map (ArcObjectEditorContentHelper.valueLabel >> sprintf "Back to %s")
                    |> Option.defaultValue $"Back to {rootLabel}"

                Html.button [
                    prop.testId "process-core-metadata-back"
                    prop.className "swt:btn swt:btn-ghost swt:btn-sm"
                    prop.ariaLabel backLabel
                    prop.onClick (fun _ -> setNavigationPath (ListHelpers.removeLast navigationPath))
                    prop.children [
                        Html.i [
                            prop.className "swt:iconify swt:fluent--arrow-left-20-regular swt:size-5"
                        ]
                        Html.span backLabel
                    ]
                ]

        let breadcrumb =
            Html.nav [
                prop.ariaLabel "Breadcrumb"
                prop.className "swt:flex swt:min-w-0 swt:flex-1 swt:items-center swt:overflow-hidden swt:text-sm"
                prop.children [
                    Breadcrumb.item
                        rootLabel
                        true
                        (if isMetadataActive then
                             Some(fun () -> setNavigationPath [])
                         else
                             None)

                    for index, value in List.indexed navigationPath do
                        Breadcrumb.separator ()

                        let label = ArcObjectEditorContentHelper.valueLabel value
                        let isCurrent = index = navigationPath.Length - 1

                        Breadcrumb.item
                            label
                            false
                            (if isCurrent then
                                 None
                             else
                                 Some(fun () -> setNavigationPath (navigationPath |> List.take (index + 1))))
                ]
            ]

        let content =
            match currentValue with
            | None ->
                ObjectBrowser.Main(
                    arcStateCtx,
                    arcView,
                    kind,
                    onOpen = (fun entity -> setNavigationPath [ entity.value ]),
                    ?onOpenInTableEditor = onOpenInTableEditor,
                    searchQuery = searchQuery,
                    ?scopedEntities = scopedEntities
                )
            | Some value ->
                ImportCtx.Provider(
                    Some {
                        Catalog = importCatalog
                        RunAsyncMutation = runAsyncMutation
                    },
                    Html.div [ prop.key revision; prop.children [ metadataView value ] ]
                )

        Html.section [
            if isMetadataActive then
                prop.testId "process-core-metadata-editor"
            prop.className "swt:flex swt:size-full swt:min-h-0 swt:flex-col swt:bg-base-200"
            prop.children [
                Html.div [
                    prop.className "swt:z-10 swt:h-12 swt:shrink-0 swt:bg-base-200"
                    prop.children [
                        Navbar.Main(
                            left = backButton,
                            middle = breadcrumb,
                            right = SearchBar.SearchBar(searchQuery, setSearchQuery, isMetadataActive)
                        )
                    ]
                ]
                Html.div [
                    prop.className "swt:min-h-0 swt:flex-1 swt:overflow-y-auto"
                    prop.children [ content ]
                ]
            ]
        ]

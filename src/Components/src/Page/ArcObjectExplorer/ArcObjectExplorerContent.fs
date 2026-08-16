namespace Swate.Components.Page.ArcObjectExplorer

open Fable.Core
open Feliz
open ProcessCore
open Swate.Components
open Swate.Components.Composite.InteractiveList
open Swate.Components.Page.ArcObjectExplorer.Types
open Swate.Components.Page.ObjectBrowser
open Swate.Components.Page.ObjectBrowser.Types
open Swate.Components.ProcessCore
open Swate.Components.Primitive.Navbar

[<Erase; Mangle(false)>]
type ArcObjectExplorerContent =

    [<ReactComponent>]
    static member private Tile(index: int, entity: ProcessCoreEntity, onActivate: ProcessCoreEntity -> unit) =
        Html.button [
            prop.custom (Attributes.RowIndex, index)
            prop.type'.button
            prop.className
                "swt:group swt:flex swt:min-w-0 swt:items-center swt:gap-3 swt:rounded-md swt:border swt:border-transparent swt:p-3 swt:text-left swt:hover:border-base-300 swt:hover:bg-base-300 swt:focus:outline-none swt:focus-visible:ring-2 swt:focus-visible:ring-primary"
            prop.title entity.displayName
            prop.onDoubleClick (fun _ -> onActivate entity)
            prop.onKeyDown (fun event ->
                if event.key = "Enter" then
                    event.preventDefault ()
                    onActivate entity
            )
            prop.children [
                Html.i [
                    prop.ariaHidden true
                    prop.className [
                        MemberCatalog.iconForKind entity.memberKind
                        "swt:size-10 swt:shrink-0"
                    ]
                ]
                Html.span [
                    prop.className "swt:min-w-0 swt:truncate swt:text-sm"
                    prop.text entity.displayName
                ]
            ]
        ]

    [<ReactComponent>]
    static member ArcObjectExplorerContent
        (
            arcStateCtx: StateUpdaterContext<ARC option>,
            arcView: Swate.Components.ProcessCore.Types.ArcView,
            selectedTarget: ExplorerTreeTarget option,
            onOpenInMetadataEditor: (ProcessCoreEntity -> unit) option,
            onOpenInTableEditor: (ProcessCoreEntity -> unit) option
        ) =
        let containerRef = React.useElementRef ()
        let searchQuery, setSearchQuery = React.useState ""

        let selectedMemberKind, setSelectedMemberKind =
            React.useState<MemberKind option> None

        let initialPath = selectedTarget |> Option.map _.Dataset |> Option.toList
        let navigationPath, setNavigationPath = React.useState initialPath

        let activeCollection, setActiveCollection =
            React.useState (selectedTarget |> Option.filter (fun target -> not target.Levels.IsEmpty))

        let currentEntity = List.tryLast navigationPath

        let title, entities =
            match arcStateCtx.state, activeCollection, currentEntity with
            | None, _, _ -> "Datasets", [||]
            | Some _, Some collection, _ ->
                let labels = collection.Levels |> List.map _.Label
                let members = collection.Levels |> List.last |> _.Members
                String.concat " / " (collection.Dataset.displayName :: labels), members
            | Some arc, None, None -> "Datasets", ObjectViewModel.getEntitiesWithView arcView arc MemberKind.Dataset
            | Some _, None, Some current ->
                navigationPath |> List.map _.displayName |> String.concat " / ",
                MemberTree.directMembers arcView current

        let searchTerm = searchQuery.Trim()
        let normalizedSearchTerm = searchTerm.ToUpperInvariant()

        let availableMemberKinds = entities |> Array.map _.memberKind |> Array.distinct

        let selectedFilterIndex =
            selectedMemberKind
            |> Option.bind (fun selected -> availableMemberKinds |> Array.tryFindIndex ((=) selected))

        let visibleEntities =
            entities
            |> Array.filter (fun entity ->
                selectedMemberKind |> Option.forall ((=) entity.memberKind)
                && (normalizedSearchTerm = ""
                    || entity.displayName.ToUpperInvariant().Contains(normalizedSearchTerm))
            )

        let contextMenuKinds =
            match activeCollection, currentEntity with
            | Some collection, _ -> collection.Levels |> List.last |> _.AllowedMemberKinds
            | None, None -> [| MemberKind.Dataset |]
            | None, Some current -> MemberTree.allowedChildKinds arcView current

        let contextMenuAddActions =
            match currentEntity, activeCollection with
            | Some {
                       value = ProcessCoreEntityValue.Process processObject
                   },
              collection ->
                collection
                |> Option.bind (fun target -> target.Levels |> List.tryLast)
                |> Option.map _.RelationshipKey
                |> MemberTree.createProcessRelationshipActions processObject
            | _ -> [||]

        Html.section [
            prop.ref containerRef
            prop.testId "arc-object-explorer"
            prop.ariaLabel "ARC object explorer"
            prop.className "swt:absolute swt:inset-0 swt:flex swt:min-h-0 swt:min-w-0 swt:flex-col swt:bg-base-200"
            prop.children [
                Html.header [
                    prop.className "swt:h-12 swt:shrink-0 swt:border-b swt:border-base-300 swt:bg-base-100"
                    prop.children [
                        Navbar.Main(
                            left =
                                (if navigationPath.IsEmpty then
                                     Html.div []
                                 else
                                     Html.button [
                                         prop.type'.button
                                         prop.className "swt:btn swt:btn-ghost swt:btn-square swt:btn-sm"
                                         prop.ariaLabel "Back one level"
                                         prop.title "Back one level"
                                         prop.onClick (fun _ ->
                                             match activeCollection with
                                             | Some _ -> setActiveCollection None
                                             | None -> setNavigationPath (ListHelpers.removeLast navigationPath)

                                             setSelectedMemberKind None
                                         )
                                         prop.children [
                                             Html.i [
                                                 prop.className
                                                     "swt:iconify swt:fluent--arrow-left-20-regular swt:size-5"
                                             ]
                                         ]
                                     ]),
                            middle =
                                Html.nav [
                                    prop.ariaLabel "Breadcrumb"
                                    prop.className
                                        "swt:flex swt:min-w-0 swt:flex-1 swt:items-center swt:overflow-hidden swt:text-sm"
                                    prop.children [
                                        for index, entity in List.indexed navigationPath do
                                            if index > 0 then
                                                Breadcrumb.separator ()

                                            Breadcrumb.item
                                                entity.displayName
                                                true
                                                (Some(fun () ->
                                                    setNavigationPath (navigationPath |> List.take (index + 1))
                                                    setActiveCollection None
                                                    setSelectedMemberKind None
                                                ))

                                        match activeCollection with
                                        | Some collection ->
                                            for index, level in List.indexed collection.Levels do
                                                Breadcrumb.separator ()

                                                Breadcrumb.item
                                                    level.Label
                                                    false
                                                    (Some(fun () ->
                                                        let levels = collection.Levels |> List.take (index + 1)

                                                        setActiveCollection (Some { collection with Levels = levels })

                                                        setSelectedMemberKind None
                                                    ))
                                        | None -> ()
                                    ]
                                ],
                            right =
                                Html.div [
                                    prop.className "swt:flex swt:items-center swt:gap-2"
                                    prop.children [
                                        Filter.Filter(
                                            availableMemberKinds
                                            |> Array.map (fun kind -> (MemberCatalog.find kind).label),
                                            selectedFilterIndex,
                                            (fun index ->
                                                setSelectedMemberKind (
                                                    index
                                                    |> Option.bind (fun i -> availableMemberKinds |> Array.tryItem i)
                                                )
                                            ),
                                            false
                                        )
                                        SearchBar.SearchBar(searchQuery, setSearchQuery, false)
                                    ]
                                ]
                        )
                    ]
                ]
                Html.div [
                    prop.className
                        "swt:grid swt:min-h-0 swt:w-full swt:flex-1 swt:auto-rows-min swt:grid-cols-[repeat(auto-fill,minmax(12rem,1fr))] swt:content-start swt:gap-1 swt:overflow-y-auto swt:p-4"
                    prop.children [
                        if Array.isEmpty visibleEntities then
                            Html.p [
                                prop.role.status
                                prop.className "swt:col-span-full swt:p-8 swt:text-center swt:text-base-content/60"
                                prop.text (
                                    if normalizedSearchTerm = "" then
                                        $"No objects available in {title}."
                                    else
                                        $"No objects match \"{searchTerm}\"."
                                )
                            ]
                        else
                            for index, entity in Array.indexed visibleEntities do
                                ArcObjectExplorerContent.Tile(
                                    index,
                                    entity,
                                    (fun selected ->
                                        setNavigationPath (navigationPath @ [ selected ])
                                        setActiveCollection None
                                        setSelectedMemberKind None
                                    )
                                )
                    ]
                ]
                ContextMenu.ContextMenu(
                    containerRef,
                    arcStateCtx,
                    arcView,
                    None,
                    ignore,
                    contextMenuMemberKinds = contextMenuKinds,
                    tryGetContextMenuEntity = (fun index -> visibleEntities |> Array.tryItem index),
                    contextMenuAddActions = contextMenuAddActions,
                    allowDeleteMembers = false,
                    ?onOpenInMetadataEditor = onOpenInMetadataEditor,
                    ?onOpenInTableEditor = onOpenInTableEditor
                )
            ]
        ]

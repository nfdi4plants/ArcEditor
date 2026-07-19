namespace Swate.Components.Page.ObjectBrowser

open Fable.Core
open Feliz
open ProcessCore
open Swate.Components
open Swate.Components.Page.ObjectBrowser.Types
open Swate.Components.Composite.InteractiveList
open Swate.Components.Composite.InteractiveList.Types

[<Erase; Mangle(false)>]
type ObjectBrowser =

    [<ReactComponent>]
    static member private EntityRow
        (
            entry: InteractiveListData<int>,
            entity: ProcessCoreEntity,
            onClick: InteractiveListData<int> -> unit,
            onDelete: ProcessCoreEntity -> unit,
            isSelected: bool
        ) =
        Html.tr [
            prop.custom (Attributes.RowIndex, entry.data)
            prop.className [
                "swt:cursor-pointer swt:align-middle swt:hover:bg-base-300 swt:focus:bg-base-300 swt:focus:outline-none"

                if isSelected then
                    "swt:bg-base-300"
            ]
            prop.ariaSelected isSelected
            prop.tabIndex 0
            prop.onClick (fun _ -> onClick entry)
            prop.onKeyDown (fun event ->
                if event.key = "Enter" || event.key = " " then
                    event.preventDefault ()
                    onClick entry
            )
            prop.children [
                Html.td [
                    prop.className "swt:w-px"
                    prop.children [
                        Html.div [
                            prop.className "swt:flex swt:items-center"
                            prop.children [ Html.i [ prop.className [ entry.icon; "swt:size-6" ] ] ]
                        ]
                    ]
                ]
                Html.td [
                    prop.className "swt:min-w-0 swt:max-w-0 swt:px-4 swt:py-2"
                    prop.children [
                        Html.div [ prop.className "swt:truncate"; prop.text entry.label ]
                        match entity.value with
                        | ProcessCoreEntityValue.Dataset dataset ->
                            Html.div [
                                prop.className "swt:truncate swt:text-xs swt:text-base-content/50"
                                prop.title dataset.Identifier
                                prop.text dataset.Identifier
                            ]
                        | _ -> Html.none
                    ]
                ]
                Html.td [
                    prop.className "swt:w-max swt:whitespace-nowrap swt:py-2 swt:pr-2 swt:text-right"
                    prop.children [
                        Html.button [
                            prop.type'.button
                            prop.className "swt:btn swt:btn-sm swt:btn-error swt:whitespace-nowrap"
                            prop.ariaLabel $"Delete {entity.displayName}"
                            prop.text "Delete"
                            prop.onClick (fun event ->
                                event.stopPropagation ()
                                onDelete entity
                            )
                        ]
                    ]
                ]
            ]
        ]

    [<ReactComponent(true)>]
    static member Main
        (
            arcStateCtx: StateUpdaterContext<ARC option>,
            kind: MemberKind,
            ?onOpen: ProcessCoreEntity -> unit,
            ?onOpenInTableEditor: ProcessCoreEntity -> unit
        ) =
        let containerRef = React.useElementRef ()
        let _, refreshBrowser = React.useStateWithUpdater 0

        let selectedObject, setSelectedObject =
            React.useState<(MemberKind * int) option> None

        let actionRequest, setActionRequest = React.useState<ContextMenuRequest option> None

        React.useEffectOnce (fun () ->
            let refreshAfterArcChange (_event: Browser.Types.Event) =
                setSelectedObject None
                refreshBrowser ((+) 1)

            Browser.Dom.window.addEventListener (ChangeNotification.ArcChangedEvent, refreshAfterArcChange)

            FsReact.createDisposable (fun () ->
                Browser.Dom.window.removeEventListener (ChangeNotification.ArcChangedEvent, refreshAfterArcChange)
            )
        )

        match arcStateCtx.state with
        | None -> Html.none
        | Some arc ->
            let descriptor = MemberCatalog.find kind
            let entities = ObjectViewModel.getEntities arc kind

            let objectEntries: InteractiveListData<int>[] =
                entities
                |> Array.mapi (fun index entity -> {
                    icon = descriptor.icon
                    label = entity.displayName
                    data = index
                })

            let selectEntry entry =
                setSelectedObject (Some(kind, entry.data))
                onOpen |> Option.iter (fun openEntity -> openEntity entities.[entry.data])

            let rowRender =
                fun entry ->
                    let entity = entities.[entry.data]

                    ObjectBrowser.EntityRow(
                        entry,
                        entity,
                        selectEntry,
                        (ContextMenuRequest.DeleteEntity >> Some >> setActionRequest),
                        (selectedObject = Some(kind, entry.data))
                    )

            Html.section [
                prop.ref containerRef
                prop.testId "process-core-object-browser"
                prop.ariaLabel descriptor.label
                prop.className "swt:size-full swt:min-h-0 swt:overflow-y-auto swt:bg-base-200 swt:p-6"
                prop.children [
                    Html.h1 [
                        prop.className "swt:mb-6 swt:text-xl swt:font-semibold"
                        prop.text descriptor.label
                    ]

                    if Array.isEmpty entities then
                        Html.div [
                            prop.testId "process-core-object-browser-empty"
                            prop.role.status
                            prop.className
                                "swt:flex swt:min-h-48 swt:items-center swt:justify-center swt:text-base-content/60"
                            prop.text $"No {descriptor.label} available in this ARC."
                        ]
                    else
                        Html.div [
                            prop.testId "process-core-object-list"
                            prop.children [
                                InteractiveList.InteractiveList(
                                    objectEntries,
                                    selectEntry,
                                    rowRender = rowRender,
                                    isSelected = (fun entry -> selectedObject = Some(kind, entry.data)),
                                    styles = InteractiveListStyles(tableClassName = "swt:table-fixed swt:w-full")
                                )
                            ]
                        ]

                    ContextMenu.ContextMenu(
                        containerRef,
                        arcStateCtx,
                        Some kind,
                        ignore,
                        ?onOpenInTableEditor = onOpenInTableEditor,
                        ?actionRequest = actionRequest,
                        onActionRequestClosed = (fun () -> setActionRequest None)
                    )
                ]
            ]

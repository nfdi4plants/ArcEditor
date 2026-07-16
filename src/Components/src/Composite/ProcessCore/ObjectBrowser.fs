namespace Swate.Components.Composite.ProcessCore

open Fable.Core
open Feliz
open ProcessCore
open Swate.Components
open Swate.Components.Composite.InteractiveList
open Swate.Components.Composite.InteractiveList.Types

[<Erase; Mangle(false)>]
type ObjectBrowser =

    [<ReactComponent(true)>]
    static member Main
        (arcStateCtx: StateUpdaterContext<ARC option>, kind: MemberKind, ?onOpen: ProcessCoreEntity -> unit)
        =
        let containerRef = React.useElementRef ()
        let _, refreshBrowser = React.useStateWithUpdater 0

        let selectedObject, setSelectedObject =
            React.useState<(MemberKind * int) option> None

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
                                    (fun entry ->
                                        setSelectedObject (Some(kind, entry.data))
                                        onOpen |> Option.iter (fun openEntity -> openEntity entities.[entry.data])
                                    ),
                                    isSelected = (fun entry -> selectedObject = Some(kind, entry.data))
                                )
                            ]
                        ]

                    ContextMenu.ContextMenu(containerRef, arcStateCtx, Some kind, ignore)
                ]
            ]

namespace Swate.Components.Page.ProcessCoreSidebar

open Fable.Core
open Feliz
open ProcessCore
open Swate.Components
open Swate.Components.Page.ObjectBrowser
open Swate.Components.Page.ObjectBrowser.Types
open Swate.Components.Page.ProcessCoreSidebar.ArcSidebarHelper
open Swate.Components.Primitive.ContextMenu

[<Erase; Mangle(false)>]
type ArcSidebar =

    [<ReactComponent(true)>]
    static member Main
        (
            arcStateCtx: StateUpdaterContext<ARC option>,
            onSelect: MemberKind -> unit,
            ?selectedKind: MemberKind,
            ?arcRootPath: string,
            ?onCopyPath: string -> unit,
            ?onOpenFolder: string -> unit,
            ?onRevealFolder: string -> unit
        ) =
        let headerRef = React.useRef None

        let contextMenuItems (_: obj) =
            match arcRootPath with
            | None -> []
            | Some path -> createContextMenuItems path onOpenFolder onRevealFolder onCopyPath

        match arcStateCtx.state with
        | None -> Html.none
        | Some arc ->
            let arcName = displayName arc.Title arc.Identifier
            let headerTitle = arcRootPath |> Option.defaultValue arcName

            Html.aside [
                prop.testId "arc-sidebar"
                prop.ariaLabel "ARC sidebar"
                prop.className "swt:flex swt:size-full swt:min-h-0 swt:flex-col"
                prop.children [
                    Html.header [
                        prop.ref headerRef
                        prop.testId "arc-sidebar-header"
                        prop.title headerTitle
                        prop.className
                            "swt:flex swt:shrink-0 swt:items-center swt:gap-2 swt:border-b swt:border-base-300 swt:pb-3"
                        prop.children [
                            Html.i [
                                prop.className "swt:iconify swt:fluent--archive-20-filled swt:size-5 swt:shrink-0"
                            ]
                            Html.h2 [
                                prop.testId "arc-sidebar-title"
                                prop.className "swt:min-w-0 swt:truncate swt:text-sm swt:font-semibold"
                                prop.title headerTitle
                                prop.text arcName
                            ]
                        ]
                    ]
                    ContextMenu.ContextMenu(contextMenuItems, headerRef)
                    Html.div [
                        prop.testId "arc-sidebar-body"
                        prop.className "swt:min-h-0 swt:grow swt:overflow-y-auto swt:pt-2"
                        prop.children [
                            MemberList.Main(arcStateCtx, onSelect, ?selectedKind = selectedKind)
                        ]
                    ]
                ]
            ]

namespace Swate.Components.Composite.Sidebars

open Fable.Core
open Feliz
open ProcessCore
open Swate.Components
open Swate.Components.Composite.ProcessCore
open Swate.Components.Composite.Sidebars.ArcSidebarHelper

[<Erase; Mangle(false)>]
type ArcSidebar =

    [<ReactComponent(true)>]
    static member Main(arcStateCtx: StateUpdaterContext<ARC option>) =
        let title, identifier =
            match arcStateCtx.state with
            | Some arc -> arc.Title, arc.Identifier
            | None -> None, ""

        let arcName =
            React.useMemo ((fun () -> displayName title identifier), [| box title; box identifier |])

        match arcStateCtx.state with
        | None -> Html.none
        | Some _ ->
            Html.aside [
                prop.testId "arc-sidebar"
                prop.ariaLabel "ARC sidebar"
                prop.className "swt:flex swt:size-full swt:min-h-0 swt:flex-col"
                prop.children [
                    Html.header [
                        prop.testId "arc-sidebar-header"
                        prop.className
                            "swt:flex swt:shrink-0 swt:items-center swt:gap-2 swt:border-b swt:border-base-300 swt:pb-3"
                        prop.children [
                            Html.i [
                                prop.className "swt:iconify swt:fluent--archive-20-filled swt:size-5 swt:shrink-0"
                            ]
                            Html.h2 [
                                prop.testId "arc-sidebar-title"
                                prop.className "swt:min-w-0 swt:truncate swt:text-sm swt:font-semibold"
                                prop.title arcName
                                prop.text arcName
                            ]
                        ]
                    ]
                    Html.div [
                        prop.testId "arc-sidebar-body"
                        prop.className "swt:min-h-0 swt:grow swt:overflow-y-auto swt:pt-2"
                        prop.children [ ProcessCoreMemberList.Main() ]
                    ]
                ]
            ]

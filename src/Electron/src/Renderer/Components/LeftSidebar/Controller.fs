module Renderer.Components.LeftSidebar.Controller

open Feliz
open Renderer.Types
open Swate.Components
open Swate.Components.Composite.Layout
open Swate.Components.Composite.Layout.LeftSidebarContext

[<Hook>]
let useController () =
    let isOpen, setIsOpen = React.useState false

    React.useMemo ((fun _ -> { state = isOpen; setState = setIsOpen }), [| box isOpen |])

[<ReactComponent>]
let ActionButtons (leftSidebarTarget: LeftSidebarPage, setLeftSidebarTarget: LeftSidebarPage -> unit) =
    let leftSidebarCtx = useLeftSidebarCtx ()
    let pageStateCtx = Renderer.Context.PageStateContext.usePageStateCtx ()

    let toggleTarget target onTargetChanged =
        if leftSidebarTarget = target then
            leftSidebarCtx.setState (not leftSidebarCtx.state)
        else
            setLeftSidebarTarget target
            leftSidebarCtx.setState true
            onTargetChanged ()

    React.Fragment [
        Layout.LayoutBtn(
            iconClassName = "swt:fluent--folder-open-24-regular",
            tooltip = "Explorer",
            isActive = (leftSidebarTarget = LeftSidebarPage.Explorer),
            onClick =
                fun () ->
                    toggleTarget
                        LeftSidebarPage.Explorer
                        (fun () -> pageStateCtx.setState (Some(PageState.ArcObjectExplorerPage None)))
        )
        Layout.LayoutBtn(
            iconClassName = "swt:fluent--document-edit-24-regular",
            tooltip = "Editor",
            isActive = (leftSidebarTarget = LeftSidebarPage.Editor),
            onClick = fun () -> toggleTarget LeftSidebarPage.Editor (fun () -> pageStateCtx.setState None)
        )
        Layout.LayoutBtn(
            iconClassName = "swt:fluent--branch-fork-24-regular",
            tooltip = "Git",
            isActive = (leftSidebarTarget = LeftSidebarPage.Git),
            onClick = fun () -> toggleTarget LeftSidebarPage.Git (fun () -> pageStateCtx.setState None)
        )
    ]

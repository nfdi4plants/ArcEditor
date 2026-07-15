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

    let toggleTarget target =
        if leftSidebarTarget = target then
            leftSidebarCtx.setState (not leftSidebarCtx.state)
        else
            setLeftSidebarTarget target
            leftSidebarCtx.setState true

    React.Fragment [
        Layout.LayoutBtn(
            iconClassName = "swt:fluent--archive-20-filled",
            tooltip = "ARC",
            isActive = (leftSidebarTarget = LeftSidebarPage.Arc),
            onClick = fun () -> toggleTarget LeftSidebarPage.Arc
        )
        Layout.LayoutBtn(
            iconClassName = "swt:fluent--branch-fork-24-regular",
            tooltip = "Git",
            isActive = (leftSidebarTarget = LeftSidebarPage.Git),
            onClick = fun () -> toggleTarget LeftSidebarPage.Git
        )
    ]

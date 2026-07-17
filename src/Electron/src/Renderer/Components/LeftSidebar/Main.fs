module Renderer.Components.LeftSidebar.Main

open Feliz
open Renderer.Types
open Swate.Components.Page.ProcessCoreSidebar

/// This can be further reduced by using the actual contexts instead of passing down the states and setters as props, but this is good enough for now
[<ReactComponent>]
let Main (leftSidebarTarget: LeftSidebarPage) =
    let arcStateCtx = Renderer.Context.ArcStateContext.useArcStateCtx ()
    let pageStateCtx = Renderer.Context.PageStateContext.usePageStateCtx ()

    let selectedProcessCoreKind =
        match pageStateCtx.state with
        | Some(PageState.ProcessCoreObjectsPage kind) -> Some kind
        | _ -> None

    Html.div [
        prop.className [
            "swt:box-border"
            "swt:flex"
            "swt:h-full"
            "swt:min-h-0"
            "swt:min-w-0"
            "swt:max-w-full"
            "swt:flex-col"
            "swt:overflow-hidden"
            "swt:p-4"
        ]
        prop.children [|
            match leftSidebarTarget with
            | LeftSidebarPage.Arc ->
                ArcSidebar.Main(
                    arcStateCtx,
                    (fun kind -> pageStateCtx.setState (Some(PageState.ProcessCoreObjectsPage kind))),
                    ?selectedKind = selectedProcessCoreKind
                )
            | LeftSidebarPage.Git -> Git.GitSidebarPanel.Main()
        |]
    ]

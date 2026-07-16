module Renderer.Components.MainContent.Main

open Feliz
open Renderer.Types
open Swate.Electron.Shared
open Swate.Components.Page.ProcessCore
open Renderer.Components.MainContent.DataHubBrowserTarget
open Renderer.Components.MainContent.EmptySelectionTarget

module private LazyComponents =

    open Swate.Components.Primitive

    [<ReactComponent>]
    let FullPageLoadingSpinner (text: string) =
        Html.div [
            prop.className "swt:flex-1 swt:flex swt:min-w-0 swt:min-h-0 swt:grow swt:justify-center swt:items-center"
            prop.children [
                Swate.Components.Primitive.LoadingSpinner.LoadingSpinner.LoadingSpinner(
                    size = DaisyuiSize.XL,
                    color = DaisyuiColors.Primary,
                    text = text
                )
            ]
        ]

    [<ReactLazyComponent>]
    let LazySettingPage () =
        Renderer.Components.MainContent.SettingsPageTarget.SettingsPage()

    [<ReactLazyComponent>]
    let ProvenanceGroupingTarget () =
        Renderer.Components.MainContent.ProvenanceGroupingTarget.ProvenanceGroupingTarget()

/// This can be further reduced by using the actual contexts instead of passing down the states and setters as props, but this is good enough for now
[<ReactMemoComponent>]
let Main (appRootPath: ArcRootPath, pageState: PageState option) =
    let arcStateCtx = Renderer.Context.ArcStateContext.useArcStateCtx ()

    Html.div [
        prop.className "swt:size-full swt:min-w-0 swt:min-h-0 swt:flex swt:justify-center swt:overflow-hidden"
        prop.children [
            match appRootPath, pageState with
            | _, Some PageState.DataHubBrowser -> DataHubBrowserTarget()
            | _, Some PageState.SettingsPage ->
                React.Suspense(
                    [ LazyComponents.LazySettingPage() ],
                    fallback = LazyComponents.FullPageLoadingSpinner("Loading settings...")
                )
            | None, _ ->
                Html.div [
                    prop.className "swt:flex-1 swt:min-w-0 swt:min-h-0 swt:flex swt:justify-center swt:items-center"
                    prop.children [ Renderer.Components.InitState.InitState() ]
                ]
            | Some _, Some PageState.ProvenanceGroupingPage ->
                React.Suspense(
                    [ LazyComponents.ProvenanceGroupingTarget() ],
                    fallback = LazyComponents.FullPageLoadingSpinner("Loading Table Editor...")
                )
            | Some _, Some(PageState.ProcessCoreObjectsPage kind) ->
                match arcStateCtx.state with
                | Some _ -> ObjectBrowser.Main(arcStateCtx, kind)
                | None -> LazyComponents.FullPageLoadingSpinner("Loading ARC...")
            | Some _, Some(PageState.GitDiffPage diffData) -> GitDiffTarget.Main diffData
            | Some _, Some(PageState.GitMergeConflictPage mergeData) -> GitMergeConflictTarget.Main mergeData
            | Some _, Some(PageState.GitUnsupportedPage unsupportedPage) -> GitUnsupportedTarget.Main unsupportedPage
            | Some _, None -> EmptySelectionTarget()
        ]
    ]

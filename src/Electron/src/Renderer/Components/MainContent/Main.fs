module Renderer.Components.MainContent.Main

open Feliz
open Renderer.Types
open Swate.Electron.Shared
open Swate.Components.Page.ObjectBrowser
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

/// This can be further reduced by using the actual contexts instead of passing down the states and setters as props, but this is good enough for now
[<ReactMemoComponent>]
let Main (appRootPath: ArcRootPath, pageState: PageState option) =
    let arcStateCtx = Renderer.Context.ArcStateContext.useArcStateCtx ()
    let pageStateCtx = Renderer.Context.PageStateContext.usePageStateCtx ()

    let sessionCtx =
        Renderer.Context.ProvenanceSessionContext.useProvenanceSessionCtx ()

    let errorModal = Swate.Components.Primitive.ErrorModal.Context.useErrorModalCtx ()

    let openInTableEditor (entity: Swate.Components.Page.ObjectBrowser.Types.ProcessCoreEntity) =
        match arcStateCtx.state with
        | None -> ()
        | Some arc ->
            let locations =
                match entity.value with
                | Swate.Components.Page.ObjectBrowser.Types.ProcessCoreEntityValue.Process proc ->
                    ProvenanceGrouping.ProcessCoreSessionLoader.tryLocationForProcess proc arc
                    |> Option.toList
                | Swate.Components.Page.ObjectBrowser.Types.ProcessCoreEntityValue.Dataset dataset ->
                    ProvenanceGrouping.ProcessCoreSessionLoader.locationsForDataset dataset arc
                | _ -> []

            if locations.IsEmpty then
                errorModal.report $"'{entity.displayName}' contains no processes to open in the table editor."
            else
                match ProvenanceGrouping.ProcessCoreSessionLoader.load locations arc with
                | Ok loaded ->
                    sessionCtx.setStateUpdater (fun _ ->
                        Some {
                            Renderer.Context.ProvenanceSessionContext.Loaded = loaded
                            Renderer.Context.ProvenanceSessionContext.IsStale = false
                        }
                    )

                    pageStateCtx.setState (Some PageState.ProvenanceGroupingPage)
                | Error errors ->
                    let details = errors |> List.map (sprintf "%A") |> String.concat "\n"
                    errorModal.report $"Loading the provenance tables failed:\n{details}"

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
            // Not lazily loaded: in vite dev mode a lazy chunk whose module
            // graph fails to load (dep discovery, outdated optimize hashes)
            // rejects the page with an opaque error and caches the rejection.
            // The editor is local desktop code, so eager loading costs nothing.
            | Some _, Some PageState.ProvenanceGroupingPage ->
                Renderer.Components.MainContent.ProvenanceGroupingTarget.ProvenanceGroupingTarget()
            | Some _, Some(PageState.ProcessCoreObjectsPage kind) ->
                match arcStateCtx.state with
                | Some _ -> MetadataBrowser.Main(arcStateCtx, kind, onOpenInTableEditor = openInTableEditor)
                | None -> LazyComponents.FullPageLoadingSpinner("Loading ARC...")
            | Some _, Some(PageState.GitDiffPage diffData) -> GitDiffTarget.Main diffData
            | Some _, Some(PageState.GitMergeConflictPage mergeData) -> GitMergeConflictTarget.Main mergeData
            | Some _, Some(PageState.GitUnsupportedPage unsupportedPage) -> GitUnsupportedTarget.Main unsupportedPage
            | Some _, None -> EmptySelectionTarget()
        ]
    ]

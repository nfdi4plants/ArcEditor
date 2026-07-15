module Renderer.App

open Elmish
open Feliz
open Feliz.UseElmish
open Renderer.Components
open Renderer.Types
open Swate.Components
open Swate.Components.Composite.Layout
open Swate.Components.Primitive.ErrorModal
open Swate.Electron.Shared
open Swate.Electron.Shared.IPCTypes.MainToRendererIpc

type private Model = {
    // Current ARC root shared with renderer contexts.
    ArcRootPath: ArcRootPath
    PageState: PageState option
    LeftSidebarTarget: LeftSidebarPage option
} with

    static member Init = {
        ArcRootPath = None
        PageState = None
        LeftSidebarTarget = Some LeftSidebarPage.Arc
    }

type private Msg =
    | ArcRootPathChanged of ArcRootPath
    | PageStateChanged of PageState option
    | SetLeftSidebarTarget of LeftSidebarPage option

let private init () : Model * Cmd<Msg> = Model.Init, Cmd.none

let private update (msg: Msg) (model: Model) : Model * Cmd<Msg> =

    match msg with
    | ArcRootPathChanged arcRootPath ->
        let nextModel =
            match arcRootPath with
            | Some _ -> {
                model with
                    ArcRootPath = arcRootPath
                    LeftSidebarTarget = Some LeftSidebarPage.Arc
              }
            | None -> Model.Init

        nextModel, Cmd.none
    | PageStateChanged pageStateOption ->
        {
            model with
                PageState = pageStateOption
        },
        Cmd.none
    | SetLeftSidebarTarget leftSidebarTarget ->
        {
            model with
                LeftSidebarTarget = leftSidebarTarget
        },
        Cmd.none

let private subscribe (setLeftSidebarIsOpen: bool -> unit) (_model: Model) : Sub<Msg> = [
    [ "appPathChange" ],
    fun dispatch ->
        let dispose =
            Renderer.IpcReceiver.subscribeProxyReceiver<IPathChangeRendererApi> {
                pathChange =
                    fun arcRootPath ->
                        setLeftSidebarIsOpen arcRootPath.IsSome
                        dispatch (ArcRootPathChanged arcRootPath)
            }

        { new System.IDisposable with
            member _.Dispose() = dispose ()
        }
]

[<ReactComponent>]
let Main () =
    let leftSidebarState = Renderer.Components.LeftSidebar.Controller.useController ()

    let model, dispatch =
        React.useElmish (init, update, subscribe leftSidebarState.setState, [||])

    let setPageState (pageState: PageState option) = dispatch (PageStateChanged pageState)

    let pageCtx: StateContext<PageState option> =
        React.useMemo (
            (fun _ -> {
                state = model.PageState
                setState = setPageState
            }),
            [| box model.PageState |]
        )

    let children =
        Renderer.Components.MainContent.Main.Main(model.ArcRootPath, model.PageState)

    let setLeftSidebarTarget =
        React.useCallback ((fun leftSidebarTarget -> dispatch (SetLeftSidebarTarget leftSidebarTarget)), [||])

    let isInitializedArcVault = Option.isSome model.ArcRootPath

    let currentArcScopeId =
        model.ArcRootPath
        |> Option.map Swate.Components.Shared.PathHelpers.normalizePath
        |> Option.bind (fun path ->
            if System.String.IsNullOrWhiteSpace path then
                None
            else
                Some path
        )

    let leftSidebar, leftActions =
        match model.LeftSidebarTarget with
        | Some target when isInitializedArcVault ->
            Some(Renderer.Components.LeftSidebar.Main.Main(target)),
            Some(
                Renderer.Components.LeftSidebar.Controller.ActionButtons(
                    target,
                    fun nextTarget -> setLeftSidebarTarget (Some nextTarget)
                )
            )
        | _ -> None, None

    Swate.Components.Composite.ThemeSelector.ThemeProvider.ThemeProvider(
        Swate.Components.Composite.TermSearch.TermSearchConfigProvider.TIBQueryProvider(
            ErrorModalProvider.ErrorModalProvider(
                Context.AppStateContext.AppStateCtx.Provider(
                    model.ArcRootPath,
                    Renderer.Context.PageStateContext.PageStateCtx.Provider(
                        pageCtx,
                        Renderer.Context.ArcStateContext.Provider(
                            Renderer.Context.AuthStateContext.Provider(
                                Renderer.Context.GitStateContext.GitStateCtxProvider(
                                    Layout.Main(
                                        children = React.Fragment [| children |],
                                        navbar = Renderer.Components.Navbar.Main(),
                                        ?leftSidebar = leftSidebar,
                                        ?leftActions = leftActions,
                                        leftSidebarState = leftSidebarState
                                    )
                                )
                            )
                        )
                    )
                ),
                ?scopeId = currentArcScopeId
            )
        )
    )

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
        LeftSidebarTarget = None
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
            | Some _ -> { model with ArcRootPath = arcRootPath }
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

let private subscribe (_model: Model) : Sub<Msg> = [
    [ "appPathChange" ],
    fun dispatch ->
        let dispose =
            Renderer.IpcReceiver.subscribeProxyReceiver<IPathChangeRendererApi> {
                pathChange = ArcRootPathChanged >> dispatch
            }

        { new System.IDisposable with
            member _.Dispose() = dispose ()
        }
]

[<ReactComponent>]
let private LeftActionButtons (leftSidebarTarget: LeftSidebarPage, setLeftSidebarTarget) =
    let leftSidebarCtx =
        Swate.Components.Composite.Layout.LeftSidebarContext.useLeftSidebarCtx ()

    let toggleTarget target =
        if leftSidebarTarget = target then
            leftSidebarCtx.setState (not leftSidebarCtx.state)
        else
            leftSidebarCtx.setState true
            setLeftSidebarTarget target

    React.Fragment [
        Layout.LayoutBtn(
            iconClassName = "swt:fluent--branch-fork-24-regular",
            tooltip = "Git",
            isActive = (leftSidebarTarget = LeftSidebarPage.Git),
            onClick = fun () -> toggleTarget LeftSidebarPage.Git
        )
    ]

[<ReactComponent>]
let Main () =
    let model, dispatch = React.useElmish (init, update, subscribe, [||])

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

    let leftSidebar =
        match model.LeftSidebarTarget with
        | Some target when isInitializedArcVault ->
            Renderer.Components.LeftSidebar.Main.Main(target)
        | _ -> Html.none
        

    let leftActions =
        match model.LeftSidebarTarget with
        | Some target when isInitializedArcVault ->
            LeftActionButtons(target, fun x -> setLeftSidebarTarget (Some x))
        | _ -> Html.none

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
                                        children =
                                            React.Fragment [|
                                                children
                                            |],
                                        navbar = Renderer.Components.Navbar.Main(),
                                        leftSidebar = leftSidebar,
                                        leftActions = leftActions
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

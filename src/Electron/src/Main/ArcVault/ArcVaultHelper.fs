module Main.ArcVaultHelper


open System
open Swate.Components.Shared
open Swate.Electron.Shared.FileIOHelper
open Swate.Electron.Shared.FileIOTypes
open Fable.Electron
open Fable.Core
open Fable.Core.JsInterop
open Main
open Main.Bindings
open Node.Api
open Swate.Electron.Shared.RenamePathRules

let private fsPromisesDynamic: obj = importAll "fs/promises"
let private pathDynamic: obj = importAll "path"

let createWindow () = promise {
    let screenSize = screen.getPrimaryDisplay().workAreaSize

    let windowIconPath = Helper.Assets.getIcon ()

    let mainWindowOptions =
        BrowserWindowConstructorOptions(
            title = "ArcEditor",
            icon = (windowIconPath |> U2.Case2),
            width = int screenSize.width,
            height = int screenSize.height,
            webPreferences = WebPreferences(preload = path.join (__dirname, "preload.fs.js"))
        )

    let window = BrowserWindow(mainWindowOptions)

    if isNullOrUndefined MAIN_WINDOW_VITE_DEV_SERVER_URL then
        do! window.loadFile (path.join (__dirname, $"../renderer/{MAIN_WINDOW_VITE_NAME}/index.html"))
    else
        window.webContents.openDevTools Enums.WebContents.OpenDevTools.Options.Mode.Right
        do! window.loadURL MAIN_WINDOW_VITE_DEV_SERVER_URL

    // Prevent links from opening new Electron windows
    window.webContents.setWindowOpenHandler (fun details ->
        Fable.Electron.Main.shell.openExternal details.url |> Promise.start
        WindowOpenHandlerResponse(Enums.Types.WindowOpenHandlerResponse.Action.Deny)
    )

    // Prevent navigation inside current Electron window
    window.webContents.onWillNavigate (fun event url _ _ _ _ ->
        let currentUrl = window.webContents.getURL ()

        if url <> currentUrl then
            event.preventDefault ()
            Fable.Electron.Main.shell.openExternal url |> Promise.start
    )

    return window
}



let shouldUsePollingByDefault (platform: string) =
    System.String.Equals(platform, "win32", System.StringComparison.OrdinalIgnoreCase)

let private currentNodePlatform () : string =
    emitJsExpr () "process.platform" |> unbox<string>

let createFileWatcher (path: string) =

    let ignoreFn =
        fun (path: string) ->
            let normalizedPath = PathHelpers.normalizeSeparators path
            let tempXlsxPattern = """\.~\$.*\.xlsx$"""

            System.Text.RegularExpressions.Regex.IsMatch(normalizedPath, tempXlsxPattern)
            || Swate.Electron.Shared.FileIOHelper.isGitMetadataPath normalizedPath

    // Native Windows file events can keep handles that block app-initiated folder renames.
    let usePolling =
        shouldUsePollingByDefault (currentNodePlatform ())

    let watcherOptions =
        if usePolling then
            Chokidar.WatchOptions(
                cwd = path,
                awaitWriteFinish = true,
                ignored = !^ignoreFn,
                ignoreInitial = true,
                usePolling = true,
                interval = 200,
                binaryInterval = 400
            )
        else
            Chokidar.WatchOptions(cwd = path, awaitWriteFinish = true, ignored = !^ignoreFn, ignoreInitial = true)

    let watcher = Chokidar.Chokidar.watch (path, watcherOptions)

    watcher

open Fable.Electron.Remoting.Main

let sendArcHasUnsavedChangesUpdate (hasUnsavedChanges: bool) (window: BrowserWindow) =
    let sendMsg =
        Remoting.createIpc ()
        |> Remoting.withWindow window
        |> Remoting.buildProxySender<Swate.Electron.Shared.IPCTypes.MainToRendererIpc.IHasUnsavedArcChangesRendererApi>

    sendMsg.arcUnsavedChangesUpdate hasUnsavedChanges

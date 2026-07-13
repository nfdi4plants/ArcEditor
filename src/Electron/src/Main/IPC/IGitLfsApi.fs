module Main.IPC.IGitLfsApi

open Fable.Electron
open Fable.Electron.Main
open Fable.Electron.Remoting.Main
open Swate.Electron.Shared.GitTypes
open Swate.Electron.Shared.IPCTypes.MainToRendererIpc
open Swate.Electron.Shared.IPCTypes
open Main
open Main.Git.GitLfsService

// Cancellation tracking - in-memory store for cancellation flags keyed by request ID
let cancellations = System.Collections.Generic.Dictionary<string, bool>()

let private createProgressReporter (window: BrowserWindow) (requestId: string) =
    let rendererApi =
        Remoting.createIpc ()
        |> Remoting.withWindow window
        |> Remoting.buildProxySender<IGitLfsProgressRendererApi>

    fun msg -> rendererApi.gitLfsProgressUpdate { RequestId = requestId; Message = msg }

let runChannel (window: BrowserWindow) (request: GitLfsRequest) = promise {
    cancellations.[request.RequestId] <- false

    try
        let onProgress = createProgressReporter window request.RequestId

        let cancelCheck () =
            match cancellations.TryGetValue(request.RequestId) with
            | true, value -> value
            | _ -> false

        let! result = run request onProgress cancelCheck
        return result
    finally
        cancellations.Remove(request.RequestId) |> ignore
}

let cancelChannel (requestId: string) = promise {
    if cancellations.ContainsKey requestId then
        cancellations.[requestId] <- true

    return Ok "Cancellation requested"
}

let api (event: IpcMainInvokeEvent) : IGitLfsApi = {
    runGitLfs =
        fun (request: GitLfsRequest) -> promise {
            let windowId = windowIdFromIpcEvent event

            match ARC_VAULTS.TryGetVault(windowId) with
            | None -> return Error(exn $"The ARC for window id {windowId} should exist")
            | Some vault ->
                match vault.path with
                | None -> return Error(exn "ARC is not loaded.")
                | Some arcPath ->
                    // Always enforce the active ARC root to avoid running against arbitrary repos.
                    let enforcedRequest = { request with RepoPath = arcPath }
                    let! result = runChannel vault.window enforcedRequest

                    match result with
                    | Error e ->
                        Swate.Components.console.log ($"Error: {e.Message}")
                        return Error e
                    | Ok successResult ->
                        match enforcedRequest.Command with
                        | Track
                        | Untrack -> Helper.AppLogging.printf windowId "Git LFS command '%s' executed successfully for ARC at '%s'" (unbox<string> enforcedRequest.Command) arcPath
                        | _ -> ()

                        return Ok successResult
        }
    cancelGitLfs = fun (requestId: string) -> cancelChannel requestId
}

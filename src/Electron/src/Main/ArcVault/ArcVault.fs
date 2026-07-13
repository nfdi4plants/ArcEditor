[<AutoOpen>]
module Main.ArcVault

open System.Collections.Generic
open Fable.Electron
open Fable.Electron.Remoting.Main
open Main
open Main.Bindings
open Main.ArcVaultHelper
open Swate.Components.Shared
open Swate.Electron.Shared.IPCTypes
open Swate.Electron.Shared.IPCTypes.IPCTypesHelper
open Swate.Electron.Shared.IPCTypes.MainToRendererIpc
open Swate.Electron.Shared.FileIOTypes

/// <summary>
/// Represents a vault window in the application, optionally associated with a file path.
/// </summary>
/// <param name="path">Can be None if not opened ARC.</param>
type ArcVault(window: BrowserWindow) =

    let fileWatcherOwnWriteArcMergeSuppressionMs = 500

    member val window: BrowserWindow = window with get
    member val path: string option = None with get, set
    member val arc: bool option = None with get, private set
    member val isBusyWriting: bool = false with get, set
    member val watcher: Chokidar.IWatcher option = None with get, set

    /// This function mutably sets the active ARC in memory without persisting to disk.
    member this.SetArc(arc: bool) =
            this.arc <- Some arc
            this.window.title <- if arc then System.Guid.NewGuid().ToString() else "No ARC Loaded"

[<AutoOpen>]
module ArcVaultExtensions =

    type ArcVault with

        /// Writes the active in-memory ARC scaffold to disk without touching unmanaged files such as notes.
        member this.WriteArc() : Fable.Core.JS.Promise<Result<unit, exn>> = promise {
            match this.path, this.arc with
            | Some arcPath, Some arc ->
                this.isBusyWriting <- true

                try
                    try
                        Helper.AppLogging.printf this.window.id "Persisting ARC to disk at '%s'..." arcPath
                        return Error (exn "Not implemented") 
                    with e ->
                        return Error(exn $"Failed to persist ARC to disk: {e.Message}")
                finally
                    this.isBusyWriting <- false
            | _ -> return Error(exn "ARC is not loaded.")
        }

        member this.LoadArc() = promise {
            if this.path.IsSome then
                this.SetArc(true)
                this.StartFileWatcher()
                Helper.AppLogging.printf this.window.id "Loading ARC from disk at '%s'..." this.path.Value
                failwith "LoadArc: Not implemented"
                // match! ARC.LoadAsyncSwateZeroByteRepair this.path.Value with
                // | Error e -> Helper.AppLogging.failfn this.window.id "Unable to load ARC: %s" (PathHelpers.formatContractErrors e)
                // | Ok arc ->
                //     this.SetArc(arc)
                //     this.RefreshHasUnsavedArcChangesFlag()
            else
                Helper.AppLogging.failfn this.window.id "No path set for StartFileWatcher."
        }

        member this.StartFileWatcher() =
            if this.path.IsSome then
                match this.watcher with
                | Some _ -> ()
                | None ->
                    let watcher = createFileWatcher this.path.Value

                    let callback = fun (eventType: string) (filePath: string) ->
                        Helper.AppLogging.printf this.window.id "File watcher event: %A for file: %s" eventType filePath

                    watcher.on (Chokidar.Events.All, callback) |> ignore
                    this.watcher <- Some watcher
            else
                Helper.AppLogging.failfn this.window.id "No path set for StartFileWatcher."

        member this.StopFileWatcher() = promise {
            match this.watcher with
            | None -> ()
            | Some watcher ->
                try
                    do! watcher.close ()
                with _ ->
                    ()

            this.watcher <- None
        }


        member this.OpenARC(path: string) = promise {
            match this.path with
            | Some _ -> Helper.AppLogging.failfn this.window.id "Unable to open ARC in vault bound to ARC."
            | None ->
                let normalizedPath = PathHelpers.normalizePath path

                let sendMsg =
                    Remoting.createIpc ()
                    |> Remoting.withWindow this.window
                    |> Remoting.buildProxySender<IPathChangeRendererApi>

                Helper.AppLogging.printf this.window.id "path: %s" normalizedPath
                this.path <- Some normalizedPath
                do! this.LoadArc()
                sendMsg.pathChange (Some normalizedPath)
        }

        member this.CreateARC(path: string, identifier: string) = promise {
            match this.path, this.arc with
            | Some _, _ -> Helper.AppLogging.failfn this.window.id "Unable to create ARC in vault bound to path."
            | _, Some _ -> Helper.AppLogging.failfn this.window.id "Unable to create ARC in vault bound to ARC."
            | None, None ->
                let normalizedPath = PathHelpers.normalizePath path

                let sendMsg =
                    Remoting.createIpc ()
                    |> Remoting.withWindow this.window
                    |> Remoting.buildProxySender<IPathChangeRendererApi>

                let arc = true
                this.path <- Some normalizedPath
                this.SetArc(arc)
                this.isBusyWriting <- true

                try
                    Helper.AppLogging.printf this.window.id "Creating new ARC at '%s' with identifier '%s'..." normalizedPath identifier
                    failwith "CreateARC: Not implemented"
                    // match! arc.TryWriteAsyncSwate(normalizedPath) with
                    // | Ok _ -> ()
                    // | Error errors ->
                    //     failwithf
                    //         "Could not write ARC, failed with the following errors %s"
                    //         (PathHelpers.formatContractErrors errors)
                finally
                    this.isBusyWriting <- false

                do! this.LoadArc()
                sendMsg.pathChange (Some normalizedPath)
        }

type ArcVaults() =
    /// Key is window.id
    member val Vaults = Dictionary<int, ArcVault>() with get

    member this.Paths = this.Vaults.Values |> Seq.choose (fun x -> x.path) |> Array.ofSeq

    member this.BroadcastRecentARCs() =
        let recentARCs = RECENT_ARCS.Get()

        this.Vaults.Values
        |> Array.ofSeq
        |> fun arr ->
            if arr.Length > 0 then
                arr
                |> Array.iter (fun vault ->
                    Remoting.createIpc ()
                    |> Remoting.withWindow vault.window
                    |> Remoting.buildProxySender<IRecentArcsRendererApi>
                    |> fun client -> client.recentARCsUpdate recentARCs
                )

    /// Centralized side-effect: update recent ARCs store and broadcast to all windows.
    member private this.TrackRecentAndBroadcast(arcPath: string) =
        let normalizedArcPath = PathHelpers.normalizePath arcPath
        RECENT_ARCS.Add(normalizedArcPath) |> ignore
        this.BroadcastRecentARCs()

    member this.DisposeVault(id: int) : unit =
        match this.Vaults.TryGetValue(id) with
        | false, _ -> Helper.AppLogging.failfn id "Failed to remove vault %i" id
        | true, vault ->
            vault.StopFileWatcher() |> Promise.start
            this.Vaults.Remove(id) |> ignore
            vault.path |> Option.iter (fun p -> RECENT_ARCS.Inactivate(p) |> ignore)
            this.BroadcastRecentARCs()
            Helper.AppLogging.printf id "Removed vault %i" id

    member this.OnCloseWindow(window: BrowserWindow, id: int) =

        window.onClosed (fun () ->
            this.DisposeVault(id)
        )

    member this.RegisterVault() : Fable.Core.JS.Promise<int> = promise {
        let! window = createWindow ()
        let id = window.id
        let vault = ArcVault(window)
        this.Vaults.Add(id, vault)

        this.OnCloseWindow(window, id)

        window.focus ()
        Helper.AppLogging.printf id "Register window"

        return id
    }

    member this.RegisterVaultWithArc(path: string) = promise {
        let! window = createWindow ()
        let id = window.id
        let vault = ArcVault(window)
        this.Vaults.Add(id, vault)
        do! vault.OpenARC(path)

        this.OnCloseWindow(window, id)

        window.focus ()
        Helper.AppLogging.printf id "Register window"

        return id
    }

    member this.RegisterVaultWithNewArc(path: string, newIdentifier: string) : Fable.Core.JS.Promise<int> = promise {
        let! window = createWindow ()
        let id = window.id
        let vault = ArcVault(window)
        this.Vaults.Add(id, vault)

        do! vault.CreateARC(path, newIdentifier)

        this.OnCloseWindow(window, id)

        window.focus ()
        Helper.AppLogging.printf id "Register window"

        return id
    }

    member this.OpenARCInVault(windowId: int, path: string) = promise {
        match this.Vaults.TryGetValue windowId with
        | false, _ -> failwith $"Vault with window-id '{windowId}' not found."
        | true, vault -> do! vault.OpenARC path

        return ()
    }

    member this.CreateARCInVault(windowId: int, path: string, identifier: string) = promise {
        match this.Vaults.TryGetValue windowId with
        | false, _ -> failwith $"Vault with window-id '{windowId}' not found."
        | true, vault -> do! vault.CreateARC(path, identifier)

        return ()
    }

    member this.TryGetVault(windowId: int) =
        match this.Vaults.TryGetValue windowId with
        | true, vault -> Some vault
        | false, _ -> None

    member this.TryGetVaultByPath(path: string) =
        this.Vaults.Values
        |> Seq.tryFind (fun v -> v.path |> Option.exists (fun vaultPath -> PathHelpers.pathsEqual vaultPath path))

    // ── ARC Lifecycle Controller ──────────────────────────────────────────
    // All open/create/focus decisions are made here.
    // IPC handlers should delegate to these methods.

    /// Open an existing ARC at the given path.
    /// Decision: already-open → focus, calling window empty → open there, else → new window.
    member this.OpenOrFocusArc(callingWindowId: int, arcPath: string) = promise {
        let normalizedArcPath = PathHelpers.normalizePath arcPath

        match this.TryGetVaultByPath normalizedArcPath with
        | Some vault ->
            vault.window.focus ()
            this.TrackRecentAndBroadcast(normalizedArcPath)
            return ArcOpenDisposition.FocusedExisting normalizedArcPath
        | None ->
            match this.TryGetVault callingWindowId with
            | Some vault when vault.path.IsNone ->
                do! vault.OpenARC(normalizedArcPath)
                this.TrackRecentAndBroadcast(normalizedArcPath)
                return ArcOpenDisposition.OpenedInCurrent normalizedArcPath
            | _ ->
                // let! newWindowId = this.RegisterVaultWithArc(normalizedArcPath)

                this.TrackRecentAndBroadcast(normalizedArcPath)
                return ArcOpenDisposition.OpenedInNewWindow normalizedArcPath
    }

    /// Create a new ARC at the given path with the given identifier.
    /// Decision: path already open → focus, calling window empty → create there, else → new window.
    member this.CreateOrFocusArc(callingWindowId: int, arcPath: string, identifier: string) = promise {
        let normalizedArcPath = PathHelpers.normalizePath arcPath

        match this.TryGetVaultByPath normalizedArcPath with
        | Some vault ->
            vault.window.focus ()
            this.TrackRecentAndBroadcast(normalizedArcPath)
            return ArcOpenDisposition.FocusedExisting normalizedArcPath
        | None ->
            match this.TryGetVault callingWindowId with
            | Some vault when vault.path.IsNone ->
                do! vault.CreateARC(normalizedArcPath, identifier)
                this.TrackRecentAndBroadcast(normalizedArcPath)
                return ArcOpenDisposition.CreatedInCurrent normalizedArcPath
            | _ ->
                // let! newWindowId = this.RegisterVaultWithNewArc(normalizedArcPath, identifier)

                // match this.TryGetVault newWindowId with
                // | Some newVault -> do! newVault.RefreshFileTree()
                // | None -> ()

                this.TrackRecentAndBroadcast(normalizedArcPath)
                return ArcOpenDisposition.CreatedInNewWindow normalizedArcPath
    }


let ARC_VAULTS: ArcVaults = ArcVaults()

module Main.IPC.IProcessCoreApi

open Main
open Swate.Electron.Shared.IPCTypes
open System
open Fable.Core
open Fable.Electron
open Fable.Electron.Main
open Fable.Electron.Remoting.Main
open Swate.Components.Shared
open Swate.Electron.Shared
open Swate.Electron.Shared.IPCTypes
open Swate.Electron.Shared.IPCTypes.MainToRendererIpc
open Swate.Electron.Shared.GitTypes
open Swate.Electron.Shared.FileIOTypes
open Swate.Electron.Shared.FileIOHelper
open Node.Api
open Main
open Swate.Electron.Shared.DTOs.ProvenanceGroupingDto
open Swate.Electron.Shared.DTOs.ArcDto
open ProcessCore
open Main.IPC.FileSystemIO

let api (event: IpcMainInvokeEvent) : IProcessCoreApi = {
    getArc = fun () -> promise {
        try
            return!
                withLoadedArcVault
                    event
                    (fun vault -> promise {
                        match vault.arc with
                        | Some arc -> 
                            let dto = ARC.toDTO arc
                            return Ok dto
                        | None -> return Error(exn "ARC is not loaded.")
                    })
        with e ->
            return Error e
    }
    setArc = fun arcDto -> promise {
        try
            return!
                withLoadedArcVault
                    event
                    (fun vault -> promise {
                        let arc = ARC.fromDTO arcDto
                        vault.SetArc arc
                        match! vault.WriteArc() with
                        | Ok () -> return Ok ()
                        | Error e -> return Error e
                    })
        with e ->
            return Error e
    }
}

module Main.IPC.IFileSystemIOApi

open Main
open Swate.Electron.Shared.IPCTypes
open Fable.Electron.Main
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
open Main.IPC.FileSystemIO

let api (event: IpcMainInvokeEvent) : IFileSystemIOApi = {
    pickDirectory =
        fun _ -> promise {
            try
                let properties = [|
                    Enums.Dialog.ShowOpenDialog.Options.Properties.OpenDirectory
                |]

                let window = dialogParentFromIpcEvent event

                let! result = dialog.showOpenDialog (?window = window, properties = properties)

                if result.canceled then
                    return Error(exn "Cancelled")
                elif result.filePaths.Length <> 1 then
                    return Error(exn "Not exactly one path")
                else
                    return Ok(result.filePaths |> Array.exactlyOne)
            with e ->
                return Error(exn $"Could not pick directory: {e.Message}")
        }
}

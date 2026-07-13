module Main.IPC.IRecentArcsApi

open Main
open Swate.Electron.Shared.IPCTypes

let api: IRecentArcsApi = {
    getRecentARCs = fun _ -> promise { return RECENT_ARCS.Get() }
    removeRecentARC =
        fun arcpointer -> promise {
            try
                RECENT_ARCS.Remove(arcpointer.path) |> ignore
                ARC_VAULTS.BroadcastRecentARCs()
                return Ok()
            with e ->
                return Error e
        }
}

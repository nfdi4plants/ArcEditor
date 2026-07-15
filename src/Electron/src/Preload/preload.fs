module Preload

open Fable.Electron.Remoting.Preload
open Swate.Electron.Shared.IPCTypes

Remoting.createIpc () |> Remoting.buildTwoWayBridge<IArcVaultsApi>
Remoting.createIpc () |> Remoting.buildTwoWayBridge<IProcessCoreApi>
Remoting.createIpc () |> Remoting.buildTwoWayBridge<IRecentArcsApi>
Remoting.createIpc () |> Remoting.buildTwoWayBridge<IFileSystemIOApi>
Remoting.createIpc () |> Remoting.buildTwoWayBridge<IGitLfsApi>
Remoting.createIpc () |> Remoting.buildTwoWayBridge<IGitApi>
Remoting.createIpc () |> Remoting.buildTwoWayBridge<IGitLabApi>
Remoting.createIpc () |> Remoting.buildTwoWayBridge<IAuthApi>
//
Remoting.createIpc () |> Remoting.buildBridge<IArcFileWatcherApi>
Remoting.createIpc () |> Remoting.buildBridge<IMainSaveBeforeQuitApi>
//
Remoting.createIpc ()
|> Remoting.buildBridge<MainToRendererIpc.IPathChangeRendererApi>

Remoting.createIpc ()
|> Remoting.buildBridge<MainToRendererIpc.IRecentArcsRendererApi>

Remoting.createIpc ()
|> Remoting.buildBridge<MainToRendererIpc.IAuthAccountsRendererApi>

Remoting.createIpc ()
|> Remoting.buildBridge<MainToRendererIpc.IFileTreeRendererApi>

Remoting.createIpc ()
|> Remoting.buildBridge<MainToRendererIpc.IGitProgressRendererApi>

Remoting.createIpc ()
|> Remoting.buildBridge<MainToRendererIpc.IGitRepositoryRendererApi>

Remoting.createIpc ()
|> Remoting.buildBridge<MainToRendererIpc.IGitLfsProgressRendererApi>

Remoting.createIpc ()
|> Remoting.buildBridge<MainToRendererIpc.IHasUnsavedArcChangesRendererApi>

Remoting.createIpc ()
|> Remoting.buildBridge<MainToRendererIpc.IArcLoadedRendererApi>

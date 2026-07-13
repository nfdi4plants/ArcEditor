module Main.WatcherHelpers

open System
open Fable.Electron
open Main.Bindings
open Main.ArcVaultTypes
open Swate.Components.Shared
open Swate.Electron.Shared.FileIOHelper

let eventNameEquals (expected: Chokidar.Events) (actual: string) =
    String.Equals(actual, expected.ToString(), StringComparison.OrdinalIgnoreCase)

/// Builds an ARC-root-relative watcher event from the raw chokidar payload.
let buildWatcherEvent (arcPath: string) (eventName: string) (path: string) =
    let normalizedPath = PathHelpers.normalizePath path
    let normalizedArcPath = PathHelpers.normalizePath arcPath

    let relativePath =
        match tryGetRepoRelativePath arcPath normalizedPath with
        | Some path -> PathHelpers.normalizePath path
        | None ->
            if PathHelpers.isSameOrDescendantPath normalizedPath normalizedArcPath then
                let prefix = normalizedArcPath + "/"

                if normalizedPath.StartsWith(prefix, StringComparison.OrdinalIgnoreCase) then
                    normalizedPath.Substring(prefix.Length)
                else
                    ""
            else
                normalizedPath

    let absolutePath =
        if PathHelpers.isSameOrDescendantPath normalizedPath normalizedArcPath then
            normalizedPath
        else
            $"{normalizedArcPath}/{relativePath}" |> PathHelpers.normalizePath

    {
        EventName = eventName
        RelativePath = relativePath
        AbsolutePath = absolutePath
    }

[<AutoOpenAttribute>]
module Renderer.Types

open Swate.Components.Shared
open Swate.Electron.Shared.FileIOTypes
open Swate.Electron.Shared.FileIOHelper
open Swate.Electron.Shared.GitTypes

[<RequireQualifiedAccess>]
type LeftSidebarPage =
    | Arc
    | Git

[<RequireQualifiedAccess>]
type PageState =
    | ProvenanceGroupingPage
    | GitDiffPage of GitDiffViewDataDto
    | GitMergeConflictPage of GitMergeConflictViewDataDto
    | GitUnsupportedPage of GitUnsupportedContentDto
    | DataHubBrowser
    | SettingsPage

[<AutoOpenAttribute>]
module Renderer.Types

open Swate.Components.Shared
open Swate.Electron.Shared.FileIOTypes
open Swate.Electron.Shared.FileIOHelper
open Swate.Electron.Shared.GitTypes
open Swate.Components.Composite.ProcessCore

[<RequireQualifiedAccess>]
type LeftSidebarPage =
    | Arc
    | Git

[<RequireQualifiedAccess>]
type PageState =
    | ProvenanceGroupingPage
    | ProcessCoreObjectsPage of MemberKind
    | GitDiffPage of GitDiffViewDataDto
    | GitMergeConflictPage of GitMergeConflictViewDataDto
    | GitUnsupportedPage of GitUnsupportedContentDto
    | DataHubBrowser
    | SettingsPage

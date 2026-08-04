[<AutoOpenAttribute>]
module Renderer.Types

open Swate.Electron.Shared.GitTypes
open Swate.Components.Page.ArcObjectExplorer.Types
open Swate.Components.Page.ObjectBrowser.Types

[<RequireQualifiedAccess>]
type LeftSidebarPage =
    | Explorer
    | Editor
    | Git

[<RequireQualifiedAccess>]
type PageState =
    | ArcObjectExplorerPage of ExplorerTreeTarget option
    | ProvenanceGroupingPage
    | ProcessCoreObjectsPage of MemberKind * ProcessCoreEntity option * ProcessCoreEntity array option
    | GitDiffPage of GitDiffViewDataDto
    | GitMergeConflictPage of GitMergeConflictViewDataDto
    | GitUnsupportedPage of GitUnsupportedContentDto
    | DataHubBrowser
    | SettingsPage

[<AutoOpenAttribute>]
module Renderer.Types

open Swate.Electron.Shared.GitTypes
open Swate.Components.Page.ObjectBrowser.Types

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

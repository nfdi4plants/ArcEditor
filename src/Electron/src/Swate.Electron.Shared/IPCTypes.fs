/// This module SHOULD only contain the exact IPC communication types.
module Swate.Electron.Shared.IPCTypes

open Fable.Core
open Swate.Components.Api.GitLabApi
open Swate.Components.Composite.Authentication.Types
open Swate.Components.Page.DataHub.DataHubTypes
open Swate.Components.Shared
open Swate.Electron.Shared.DTOs.ProvenanceGroupingDto
open Swate.Electron.Shared.DTOs.ArcDto
open AuthTypes
open FileIOTypes
open GitTypes
open Swate.Components.Composite.ArcSelector.Types

module IPCTypesHelper =
    /// Not used at the moment
    [<RequireQualifiedAccess>]
    type SaveBeforeQuitDecision =
        | SaveAndClose
        | CloseWithoutSaving
        | CancelClose

open IPCTypesHelper

type CreateArcRequest = { identifier: string; initGit: bool }

/// Two Way Bridge: Renderer <-> Main
type IArcVaultsApi = {
    /// Open ARC via folder dialog. Main decides: current window / new window / focus existing.
    openARC: unit -> JS.Promise<Result<string option, exn>>
    /// Open ARC at a known path (e.g. recent-ARC click). Main decides disposition.
    openARCByPath: string -> JS.Promise<Result<string, exn>>
    /// Create ARC via folder dialog. Main decides disposition.
    createARC: CreateArcRequest -> JS.Promise<Result<string, exn>>
    closeARC: unit -> JS.Promise<Result<unit, exn>>
    getOpenPath: unit -> JS.Promise<string option>
    /// Persists the active in-memory ARC scaffold to disk.
    saveArc: unit -> JS.Promise<Result<unit, exn>>
}

type IProcessCoreApi = {
    /// Get the currently loaded ARC, if any.
    /// The ``string`` is the ProcessCore.Yaml format of the ARC.
    getArc: unit -> JS.Promise<Result<string, exn>>
    /// Set the currently loaded ARC AND write to disc.
    /// The ``string`` is the ProcessCore.Yaml format of the ARC.
    setArc: string -> JS.Promise<Result<unit, exn>>
}

type IRecentArcsApi = {
    getRecentARCs: unit -> JS.Promise<ARCPointer[]>
    removeRecentARC: ARCPointer -> JS.Promise<Result<unit, exn>>
}

type IFileSystemIOApi = {
    pickDirectory: unit -> JS.Promise<Result<string, exn>>
// movePath: MovePathRequest -> JS.Promise<Result<unit, exn>>
// pickArcPaths: unit -> JS.Promise<Result<string[], exn>>
// pickAbsolutePaths: unit -> JS.Promise<Result<string[], exn>>
// pathExists: string -> JS.Promise<Result<bool, exn>>
// openFile: string -> JS.Promise<Result<FileContentDTO, exn>>
// openPathWithDefaultApplication: string -> JS.Promise<Result<unit, exn>>
// openArcFolderInFileExplorer: unit -> JS.Promise<Result<unit, exn>>
// showPathInFileExplorer: string -> JS.Promise<Result<unit, exn>>
// writeFile: FileContentDTO -> JS.Promise<Result<unit, exn>>
// renamePath: RenamePathRequest -> JS.Promise<Result<unit, exn>>
// deletePath: string -> JS.Promise<Result<unit, exn>>
}

// type IProvenanceGroupingApi = {
//     listProvenanceTables: unit -> JS.Promise<Result<ProvenanceTableSelectionDto[], exn>>
//     loadProvenanceTable: ProvenanceTableSelectionDto -> JS.Promise<Result<ProvenanceLoadResultDto, exn>>
// }

type IGitLfsApi = {
    runGitLfs: GitLfsRequest -> JS.Promise<Result<GitLfsResult, exn>>
    cancelGitLfs: string -> JS.Promise<Result<string, exn>>
}

/// Two Way Bridge: Renderer <-> Main
type IGitApi = {
    checkGitVersions: unit -> JS.Promise<Result<unit, exn>>
    getGitStatus: unit -> JS.Promise<Result<GitStatusDto, exn>>
    getGitBranches: unit -> JS.Promise<Result<GitBranchRefDto[], exn>>
    getOriginRepositoryWebUrl: unit -> JS.Promise<Result<string option, exn>>
    getGitLfsSettings: unit -> JS.Promise<Result<GitLfsSettingsDto, exn>>
    previewGitPull: GitRemoteOperationRequest -> JS.Promise<Result<GitPullPreflightResult, exn>>
    getGitDiffSummary: unit -> JS.Promise<Result<GitDiffSummaryDto, exn>>
    getGitWordDiff: GitPathspecRequest -> JS.Promise<Result<string, exn>>
    getGitDiffViewData: string -> JS.Promise<Result<GitPageLoadResultDto<GitDiffViewDataDto>, exn>>
    getGitMergeConflictViewData: string -> JS.Promise<Result<GitPageLoadResultDto<GitMergeConflictViewDataDto>, exn>>
    installGitLfs: unit -> JS.Promise<Result<GitOperationResult, exn>>
    gitFetch: GitRemoteOperationRequest -> JS.Promise<Result<GitOperationResult, exn>>
    gitPull: GitRemoteOperationRequest -> JS.Promise<Result<GitOperationResult, exn>>
    gitPush: GitRemoteOperationRequest -> JS.Promise<Result<GitOperationResult, exn>>
    gitCancelPush: unit -> JS.Promise<Result<GitOperationResult, exn>>
    gitInitRepository: string -> JS.Promise<Result<GitOperationResult, exn>>
    gitAddRemote: GitRemoteConfigRequest -> JS.Promise<Result<GitOperationResult, exn>>
    gitCloneRepository: GitCloneRepositoryRequest -> JS.Promise<Result<GitOperationResult, exn>>
    gitStagePaths: GitPathspecRequest -> JS.Promise<Result<GitOperationResult, exn>>
    gitUnstagePaths: GitPathspecRequest -> JS.Promise<Result<GitOperationResult, exn>>
    gitDiscardPaths: GitPathspecRequest -> JS.Promise<Result<GitOperationResult, exn>>
    gitCommit: GitCommitRequest -> JS.Promise<Result<GitOperationResult, exn>>
    setGitLfsSettings: GitLfsSettingsDto -> JS.Promise<Result<GitOperationResult, exn>>
    gitLfsPrune: unit -> JS.Promise<Result<GitOperationResult, exn>>
    gitLfsDedup: unit -> JS.Promise<Result<GitOperationResult, exn>>
    gitLfsDownloadFile: GitLfsFileRequest -> JS.Promise<Result<GitOperationResult, exn>>
    gitLfsFreeLocalCopy: GitLfsFileRequest -> JS.Promise<Result<GitOperationResult, exn>>
    createBranch: GitCreateBranchRequest -> JS.Promise<Result<GitOperationResult, exn>>
    checkoutBranch: GitCheckoutBranchRequest -> JS.Promise<Result<GitOperationResult, exn>>
    confirmGitMergeResolution:
        GitConfirmMergeResolutionRequest -> JS.Promise<Result<GitConfirmMergeResolutionResult, exn>>
}

/// Two Way Bridge: Renderer <-> Main
type IGitLabApi = {
    loadAllRepos: ExploreRepoQuery -> JS.Promise<Result<PagedResponse<ExploreProjectDto>, GitLabError>>
    loadMostStarredRepos: ExploreMostStarredQuery -> JS.Promise<Result<PagedResponse<ExploreProjectDto>, GitLabError>>
    loadUserRepos: ExploreRepoQuery -> JS.Promise<Result<PagedResponse<ExploreProjectDto>, GitLabError>>
    loadOrganisationGroups: ExploreGroupsQuery -> JS.Promise<Result<PagedResponse<GroupDto>, GitLabError>>
    loadOrganisationRepos:
        ExploreGroupProjectsQuery -> JS.Promise<Result<PagedResponse<ExploreProjectDto>, GitLabError>>
    createProject: string -> JS.Promise<Result<ExploreProjectDto, GitLabError>>
}

/// Two Way Bridge: Renderer <-> Main
type IAuthApi = {
    signIn: AuthSignInRequest -> Fable.Core.JS.Promise<Result<AuthResult, exn>>
    getAuthState: unit -> Fable.Core.JS.Promise<Result<AuthStateDto, exn>>
    signOut: unit -> Fable.Core.JS.Promise<Result<unit, exn>>
    revalidate: unit -> Fable.Core.JS.Promise<Result<AuthResult, exn>>
    listAccounts: unit -> Fable.Core.JS.Promise<Result<AccountSummary array, exn>>
    rotatePersonalAccessToken: string -> Fable.Core.JS.Promise<Result<AuthStateDto, exn>>
    setActiveAccount: string -> Fable.Core.JS.Promise<Result<AuthStateDto, exn>>
    removeAccount: string -> Fable.Core.JS.Promise<Result<unit, exn>>
}

/// One Way Bridge: Main -> Renderer
module MainToRendererIpc =

    type IPathChangeRendererApi = { pathChange: string option -> unit }

    type IRecentArcsRendererApi = {
        recentARCsUpdate: ARCPointer[] -> unit
    }

    type IAuthAccountsRendererApi = {
        authAccountsUpdate: AuthStateDto -> unit
    }

    type IFileTreeRendererApi = {
        fileTreeUpdate: System.Collections.Generic.Dictionary<string, FileEntry> -> unit
    }

    type IGitProgressRendererApi = {
        gitProgressUpdate: GitProgressDto -> unit
    }

    type IGitRepositoryRendererApi = {
        gitRepositoryInitialized: string -> unit
    }

    type IGitLfsProgressRendererApi = {
        gitLfsProgressUpdate: GitLfsProgressDto -> unit
    }

    type IHasUnsavedArcChangesRendererApi = {
        arcUnsavedChangesUpdate: bool -> unit
    }

    type IArcLoadedRendererApi = {
        /// ``string`` is the ProcessCore.Yaml format of the loaded ARC. ``None`` if no ARC is loaded.
        arcLoaded: string option -> unit
    }

// TODO: What should filewatcher do when detecting changes?
/// One Way Bridge: Main -> Renderer
type IArcFileWatcherApi = {
    /// This function is called when ARC is reloaded due to local file changes.
    IsLoadingChanges: bool -> unit
}

type IMainSaveBeforeQuitApi = { requestSaveBeforeQuit: unit -> unit }

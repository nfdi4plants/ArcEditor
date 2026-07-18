module Renderer.Context.ProvenanceSessionContext

open Feliz
open Swate.Components
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreSessionLoader

/// The table editor's open session. Held app-wide so switching pages (git,
/// settings, object browser) and returning does not lose the edit state -
/// the editor page reads and writes this context instead of owning state.
type ProvenanceEditorState = {
    Loaded: LoadedProvenanceSession
    /// Set when the ARC changed underneath the loaded session. Writeback
    /// would fail its stale-graph check, so the editor offers a reload
    /// (discarding unsaved edits) instead of a save.
    IsStale: bool
}

let ProvenanceSessionCtx =
    React.createContext<StateUpdaterContext<ProvenanceEditorState option>> (
        {
            state = None
            setStateUpdater = ignore
        }
    )

[<Hook>]
let useProvenanceSessionCtx () = React.useContext ProvenanceSessionCtx

[<ReactComponent>]
let Provider (children: ReactElement) =
    let state, setState =
        React.useStateWithUpdater (None: ProvenanceEditorState option)

    let context =
        React.useMemo (
            (fun _ -> {
                state = state
                setStateUpdater = setState
            }),
            [| box state |]
        )

    ProvenanceSessionCtx.Provider(context, children)

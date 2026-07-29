module Renderer.Context.ArcStateContext

open ProcessCore
open Swate.Components.ProcessCore.UseProcessCore
open Feliz
open Swate.Components
open Swate.Electron.Shared.AuthTypes
open Fable.Electron.Remoting.Renderer
open Swate.Electron.Shared.IPCTypes.MainToRendererIpc
open Swate.Components.Primitive.ErrorModal.Context

type ArcState = {
    arc: ARC
    mutate: (ARC -> unit) -> unit
}

let ArcStateCtx = React.createContext<ArcState> ()

[<Hook>]
let useArcStateCtx () = React.useContext ArcStateCtx

// Defensive YAML hydration (disabled). IPC now uses ProcessCore's YAML parser directly.
//
// let private hydrateArc yaml =
//     Swate.Components.ProcessCore.Hotfixes.decodeWithEmptyPrimaryFields "" yaml

[<ReactComponent>]
let Provider (children: ReactElement) =

    let arcState, setArcState = React.useState (None: ARC option)

    let version, setVersion = React.useStateWithUpdater 0

    let setArcState =
        fun (arc: ARC option) ->
            setArcState arc
            setVersion (fun v -> v + 1)

    let arcMemo =
        React.useMemo ((fun () -> Option.defaultValue (new ARC("Temp ARC")) arcState), [| box version |])

    let arc, mutate, revision = useProcessCore arcMemo

    let errorCtx = useErrorModalCtx ()

    // A renderer reload wipes this state while the main process keeps the
    // window's vault loaded, and `arcLoaded` only fires on an actual open.
    // Pulling the current ARC on mount rehydrates a reloaded window; the raw
    // setter is used deliberately, since hydrating must not write back.
    React.useEffectOnce (fun () ->
        promise {
            match! Api.ipcProcessCoreApi.getArc () with
            | Ok yaml ->
                // Decoded outside the updater: React may invoke an updater
                // more than once, and a hydrate never overrides an `arcLoaded`
                // push that already landed.
                let hydrated = ARC.fromYamlString yaml
                setArcState (Some hydrated)
            // Having no ARC is the normal state during initial hydration.
            | Error error when error.Message = "ARC is not loaded." -> ()
            | Error _ -> errorCtx.report "Failed to get ARC from main process"
        }
        |> Promise.start
    )

    let setArcMain (arc: ARC) = promise {
        let yaml = arc.toYamlString ()

        match! Api.ipcProcessCoreApi.setArc yaml with
        | Ok _ -> return Some arc
        | Error ex ->
            errorCtx.report $"Failed to set ARC: {ex.Message}"
            return None
    }

    React.useEffectOnce (fun () ->
        let unsubscribe =
            Remoting.createIpc ()
            |> Remoting.buildProxyReceiverDisposable<
                Swate.Electron.Shared.IPCTypes.MainToRendererIpc.IArcLoadedRendererApi
                >
                {
                    arcLoaded =
                        fun arcYamlOpt ->
                            match arcYamlOpt with
                            | Some arcYaml ->
                                let arc = ARC.fromYamlString arcYaml
                                setArcState (Some arc)
                            | None -> setArcState None
                }

        FsReact.createDisposable unsubscribe
    )

    let mutateWithWrite =
        React.useCallback (
            (fun (arcFn: ARC -> unit) ->
                mutate (fun currentArc ->
                    //let newArcOpt = arcFn currentArc
                    arcFn currentArc
                    setArcMain currentArc |> Promise.start
                )
            ),
            [| box errorCtx; box version |]
        )

    let state =
        React.useMemo (
            (fun _ -> { arc = arc; mutate = mutateWithWrite }),
            [| box arc; box mutateWithWrite; box revision |]
        )

    React.Fragment [
        ArcStateCtx.Provider(state, children)
    // ProcessCore hotfix: block editing until all missing mandatory primary fields are repaired.
    // Must use mutate with write to ensure that the repaired ARC is written back to the main process.
    //Swate.Components.ProcessCore.MandatoryFieldRepair.MandatoryFieldRepair(
    //    arc,
    //    fun repairedArc -> setArc (fun _ -> Some repairedArc)
    //)
    ]

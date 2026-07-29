module Renderer.Context.ArcStateContext

open ProcessCore
open Swate.Components.ProcessCore
open Swate.Components.ProcessCore.UseProcessCore
open Feliz
open Swate.Components
open Swate.Electron.Shared.AuthTypes
open Fable.Electron.Remoting.Renderer
open Swate.Electron.Shared.IPCTypes.MainToRendererIpc
open Swate.Components.Primitive.ErrorModal.Context

type ArcState = {
    arc: ARC
    arcView: RendererModel.ArcView
    mutate: (ARC -> unit) -> unit
    runAsyncMutation: (unit -> unit) -> Fable.Core.JS.Promise<unit>
    isWorking: bool
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
    let activeMutationArc = React.useRef<ARC option> None
    let isWorking, setIsWorking = React.useState false

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
                match activeMutationArc.current with
                | Some currentArc -> arcFn currentArc
                | None ->
                    mutate (fun currentArc ->
                        arcFn currentArc
                        setArcMain currentArc |> Promise.start
                    )
            ),
            [| box errorCtx; box version |]
        )

    let runAsyncMutation =
        React.useCallback (
            (fun (action: unit -> unit) -> promise {
                setIsWorking true

                try
                    let mutable mutatedArc = None

                    mutate (fun currentArc ->
                        activeMutationArc.current <- Some currentArc

                        try
                            action ()
                            mutatedArc <- Some currentArc
                        finally
                            activeMutationArc.current <- None
                    )

                    match mutatedArc with
                    | Some currentArc ->
                        let! _ = setArcMain currentArc
                        ()
                    | None -> ()
                finally
                    setIsWorking false
            }),
            [| box version |]
        )

    let state =
        let arcView =
            React.useMemo ((fun () -> RendererModel.create arc), [| box arc; box revision |])

        React.useMemo (
            (fun _ -> {
                arc = arc
                arcView = arcView
                mutate = mutateWithWrite
                runAsyncMutation = runAsyncMutation
                isWorking = isWorking
            }),
            [|
                box arc
                box arcView
                box mutateWithWrite
                box runAsyncMutation
                box isWorking
            |]
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

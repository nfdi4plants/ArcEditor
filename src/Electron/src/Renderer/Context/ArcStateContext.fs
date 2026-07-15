module Renderer.Context.ArcStateContext

open ProcessCore
open Feliz
open Swate.Components
open Swate.Electron.Shared.AuthTypes
open Swate.Electron.Shared.IPCTypes.MainToRendererIpc
open Swate.Components.Primitive.ErrorModal.Context
open Swate.Electron.Shared.DTOs.ArcDto
open Fable.Electron.Remoting.Renderer

let ArcStateCtx =
    React.createContext<StateUpdaterContext<ARC option>> (
        {
            state = None
            setStateUpdater = (fun _ -> ())
        }
    )

[<Hook>]
let useArcStateCtx () = React.useContext ArcStateCtx

[<ReactComponent>]
let Provider (children: ReactElement) =

    let arc, setArc = React.useStateWithUpdater (None: ARC option)
    let errorCtx = useErrorModalCtx ()

    // not needed currently
    // let loadARC () = promise {
    //     match! Api.ipcProcessCoreApi.getArc () with
    //     | Ok response ->
    //         return Some (ARC.fromDTO response)
    //     | Error ex ->
    //         return None
    // }

    let setArcMain (arc: ARC) = promise {
        let dto = ARC.toDTO arc

        match! Api.ipcProcessCoreApi.setArc dto with
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
                        fun arcDtoOpt ->
                            match arcDtoOpt with
                            | Some arcDto ->
                                let arc = ARC.fromDTO arcDto
                                setArc (fun _ -> Some arc)
                            | None -> setArc (fun _ -> None)
                }

        FsReact.createDisposable unsubscribe
    )

    let setArc =
        React.useCallback (
            (fun (arcFn: ARC option -> ARC option) ->
                setArc (fun currentArc ->
                    let newArcOpt = arcFn currentArc

                    match newArcOpt with
                    | Some newArc ->
                        // Write changed ARC to disc
                        setArcMain newArc |> Promise.start
                        Some newArc
                    | None -> None
                )
            ),
            [| box errorCtx |]
        )

    let state =
        React.useMemo (
            (fun _ -> {
                state = arc
                setStateUpdater = setArc
            }),
            [| box arc; box setArc |]
        )

    ArcStateCtx.Provider(state, children)

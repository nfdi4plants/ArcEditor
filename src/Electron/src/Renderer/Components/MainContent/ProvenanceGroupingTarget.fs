module Renderer.Components.MainContent.ProvenanceGroupingTarget

open Feliz
open Swate.Components.Page.ProvenanceGrouping
open Swate.Components.Primitive.ErrorModal.Context
open Swate.Electron.Shared.ProvenanceGrouping
open Renderer.Context.ProvenanceSessionContext

let private writebackErrorsText (errors: ProcessCoreAdapterTypes.ProcessCoreWritebackError list) =
    errors
    |> List.map (sprintf "%A")
    |> String.concat "\n"
    |> sprintf "Saving the table editor changes failed:\n%s"

let private conversionErrorsText (errors: ProcessCoreAdapterTypes.ProcessCoreConversionError list) =
    errors
    |> List.map (sprintf "%A")
    |> String.concat "\n"
    |> sprintf "Loading the provenance tables failed:\n%s"

/// The endpoint kinds `nodeFromSet` can materialize on writeback. Passed to
/// the editor so its create dialogs never depend on which kinds the loaded
/// tables happen to contain (a sample-only table can still create Data, an
/// empty table doesn't fall back to unwritable catalog kinds).
let private processCoreEndpointKinds = [
    ProcessCoreAdapterTypes.ProcessCoreKinds.sampleEndpoint
    ProcessCoreAdapterTypes.ProcessCoreKinds.dataEndpoint
]

[<ReactComponent>]
let ProvenanceGroupingTarget () =
    let arcStateCtx = Renderer.Context.ArcStateContext.useArcStateCtx ()
    let sessionCtx = useProvenanceSessionCtx ()
    let errorModal = useErrorModalCtx ()

    // Every ARC persist replaces the context value, so this effect sees both
    // external reloads (arcLoaded pushes a new instance) and in-place edits
    // from the object browser (persisted through the same context). A session
    // whose conversion fingerprints no longer match is stale: saving would
    // fail the stale-graph check, so the toolbar offers a reload instead.
    React.useEffect (
        (fun () ->
            match sessionCtx.state with
            | Some state ->
                let arc = arcStateCtx.arc
                let isStale = not (ProcessCoreSessionLoader.isCurrent state.Loaded arc)

                if isStale <> state.IsStale then
                    sessionCtx.setStateUpdater (Option.map (fun current -> { current with IsStale = isStale }))
            | None -> ()
        ),
        [| box arcStateCtx.arc |]
    )

    let reload () =
        match sessionCtx.state with
        | Some state ->
            let arc = arcStateCtx.arc

            match ProcessCoreSessionLoader.load state.Loaded.Locations arc with
            | Ok reloaded -> sessionCtx.setStateUpdater (fun _ -> Some { Loaded = reloaded; IsStale = false })
            | Error errors ->
                sessionCtx.setStateUpdater (fun _ -> None)
                errorModal.report (conversionErrorsText errors)
        | None -> ()

    let save () =
        match sessionCtx.state with
        | Some state ->
            let arc = arcStateCtx.arc

            match ProcessCoreWriteback.prepareWriteBackMany state.Loaded.Indices state.Loaded.Session arc with
            | Ok writeBack ->
                arcStateCtx.mutate (writeBack >> ignore)

                // Reload from the mutated graph first so the session's
                // fingerprints match the ARC the persist below publishes.
                (match ProcessCoreSessionLoader.load state.Loaded.Locations arc with
                 | Ok reloaded -> sessionCtx.setStateUpdater (fun _ -> Some { Loaded = reloaded; IsStale = false })
                 | Error errors ->
                     sessionCtx.setStateUpdater (fun _ -> None)
                     errorModal.report (conversionErrorsText errors))

                // Persists to disk through the shared ARC path and refreshes
                // every other ARC consumer (object browser lists etc.).
                Swate.Components.Page.ObjectBrowser.ChangeNotification.dispatch ()
            | Error errors -> errorModal.report (writebackErrorsText errors)
        | None -> ()

    match sessionCtx.state with
    | None ->
        Html.div [
            prop.testId "provenance-target-empty"
            prop.className
                "swt:flex swt:flex-1 swt:min-w-0 swt:min-h-0 swt:items-center swt:justify-center swt:text-base-content/60"
            prop.children [
                Html.p "Right-click a process or dataset in the object browser and choose \"Open in table editor\"."
            ]
        ]
    | Some state ->
        let hasChanges = not state.Loaded.Session.PatchLog.IsEmpty

        let title =
            state.Loaded.Locations
            |> List.map (fun location -> location.TableName)
            |> String.concat ", "

        Html.div [
            prop.className "swt:flex swt:flex-1 swt:min-w-0 swt:min-h-0 swt:flex-col"
            prop.children [
                Html.div [
                    prop.testId "provenance-target-toolbar"
                    prop.className
                        "swt:flex swt:shrink-0 swt:items-center swt:gap-3 swt:border-b swt:border-base-300 swt:bg-base-100 swt:px-4 swt:py-2"
                    prop.children [
                        Html.h2 [
                            prop.className "swt:min-w-0 swt:truncate swt:text-sm swt:font-semibold"
                            prop.title title
                            prop.text title
                        ]

                        if not state.Loaded.Warnings.IsEmpty then
                            Html.span [
                                prop.testId "provenance-target-warnings"
                                prop.className "swt:badge swt:badge-warning swt:badge-sm"
                                prop.title (state.Loaded.Warnings |> List.map (sprintf "%A") |> String.concat "\n")
                                prop.text $"{state.Loaded.Warnings.Length} warnings"
                            ]

                        Html.div [ prop.className "swt:grow" ]

                        if state.IsStale then
                            Html.span [
                                prop.testId "provenance-target-stale"
                                prop.className "swt:text-sm swt:text-warning"
                                prop.text "The ARC changed - reload to continue editing."
                            ]

                            Html.button [
                                prop.testId "provenance-target-reload"
                                prop.className "swt:btn swt:btn-sm swt:btn-warning"
                                prop.text (if hasChanges then "Discard changes & reload" else "Reload")
                                prop.onClick (fun _ -> reload ())
                            ]
                        else
                            Html.button [
                                prop.testId "provenance-target-save"
                                prop.className "swt:btn swt:btn-sm swt:btn-primary"
                                prop.disabled (not hasChanges)
                                prop.title (
                                    if hasChanges then
                                        "Write the changes back to the ARC"
                                    else
                                        "No changes to save"
                                )
                                prop.text "Save"
                                prop.onClick (fun _ -> save ())
                            ]
                    ]
                ]

                Html.div [
                    prop.className "swt:min-h-0 swt:grow swt:overflow-hidden"
                    prop.children [
                        ProvenanceGrouping.Main(
                            state.Loaded.Session,
                            endpointKinds = processCoreEndpointKinds,
                            onChange =
                                (fun change ->
                                    sessionCtx.setStateUpdater (
                                        Option.map (fun current -> {
                                            current with
                                                Loaded = {
                                                    current.Loaded with
                                                        Session = change.Session
                                                }
                                        })
                                    )
                                )
                        )
                    ]
                ]
            ]
        ]

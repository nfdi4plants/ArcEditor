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

    // A layer with no process links has nothing writeback can materialise, so
    // the post-save reload - which rebuilds layers from processes - drops it.
    // The user is told before that happens rather than after (findings D4).
    let pendingUnpersistableSave, setPendingUnpersistableSave = React.useState false

    let save () =
        match sessionCtx.state with
        | Some state ->
            let arc = arcStateCtx.arc

            match Session.prepareForWriteback state.Loaded.Session with
            | Error error -> errorModal.report $"Preparing the session for writeback failed: {error}"
            | Ok prepared ->
                match ProcessCoreWriteback.prepareWriteBackMany state.Loaded.Index prepared arc with
                | Ok writeBack ->
                    arcStateCtx.mutate (writeBack >> ignore)

                    // Reload from the mutated graph first so the session's
                    // fingerprints match the ARC the persist below publishes.
                    // The load-time locations cannot name a process group the
                    // editor just created, so a layer added here would vanish
                    // from the surface despite having been written; reload the
                    // extended list, and fall back to exactly what was loaded if
                    // the derived location does not resolve.
                    let reloadLocations =
                        ProcessCoreSessionLoader.locationsAfterWriteback state.Loaded.Locations state.Loaded.Session

                    let reloadFrom locations =
                        ProcessCoreSessionLoader.load locations arc

                    (match reloadFrom reloadLocations with
                     | Ok reloaded -> sessionCtx.setStateUpdater (fun _ -> Some { Loaded = reloaded; IsStale = false })
                     | Error _ ->
                         match reloadFrom state.Loaded.Locations with
                         | Ok reloaded ->
                             sessionCtx.setStateUpdater (fun _ -> Some { Loaded = reloaded; IsStale = false })
                         | Error errors ->
                             sessionCtx.setStateUpdater (fun _ -> None)
                             errorModal.report (conversionErrorsText errors))

                // The shared ARC mutation path persists, advances the ProcessCore revision,
                // and refreshes renderer consumers, so no separate browser event is needed.
                | Error errors -> errorModal.report (writebackErrorsText errors)
        | None -> ()

    // Recomputed every render, so the prompt below reflects the session as it is
    // now: connecting the flagged layer, or the ARC going stale, retires the
    // prompt instead of leaving it to act on conditions that no longer hold.
    let savePlan =
        sessionCtx.state
        |> Option.map (fun state -> Session.planSave state.IsStale state.Loaded.Session)

    let requestSave () =
        match savePlan with
        | Some Session.ProceedWithSave -> save ()
        | Some(Session.ConfirmUnpersistableLayers _) -> setPendingUnpersistableSave true
        | Some Session.BlockedByStaleArc
        | None -> ()

    let pendingUnpersistableLayers =
        match pendingUnpersistableSave, savePlan with
        | true, Some(Session.ConfirmUnpersistableLayers layers) -> layers
        | _ -> []

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
        let hasChanges = not state.Loaded.Session.MutationJournal.IsEmpty

        let title =
            state.Loaded.Locations
            |> List.map (fun location -> location.ProcessGroupName)
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
                                prop.onClick (fun _ -> requestSave ())
                            ]
                    ]
                ]

                // Driven by the freshly computed plan, so it never renders with
                // an empty layer list (or over a stale ARC) after the session
                // moved on underneath it.
                if not pendingUnpersistableLayers.IsEmpty then
                    Html.div [
                        prop.testId "provenance-target-unpersistable-prompt"
                        prop.className "swt:alert swt:alert-warning swt:flex-wrap swt:items-start swt:m-4"
                        prop.children [
                            Html.div [
                                prop.className "swt:flex swt:flex-col swt:gap-1"
                                prop.children [
                                    Html.strong [
                                        prop.text (
                                            let names =
                                                pendingUnpersistableLayers
                                                |> List.map (fun layer -> layer.Label)
                                                |> String.concat ", "

                                            if pendingUnpersistableLayers.Length = 1 then
                                                $"Layer {names} has no connections and will not be saved."
                                            else
                                                $"Layers {names} have no connections and will not be saved."
                                        )
                                    ]
                                    Html.span [
                                        prop.className "swt:text-sm"
                                        prop.text "Draw a connection first, or continue and lose them."
                                    ]
                                ]
                            ]
                            Html.div [
                                prop.className "swt:ml-auto swt:flex swt:gap-2"
                                prop.children [
                                    Html.button [
                                        prop.testId "provenance-target-unpersistable-confirm"
                                        prop.className "swt:btn swt:btn-sm swt:btn-warning"
                                        prop.text "Save anyway"
                                        prop.onClick (fun _ ->
                                            setPendingUnpersistableSave false

                                            // Re-checked rather than assumed: the
                                            // plan is only still a confirmation
                                            // because this render says so, and a
                                            // stale ARC would fail the save with a
                                            // raw error instead of the toolbar's
                                            // guided reload.
                                            match savePlan with
                                            | Some Session.BlockedByStaleArc
                                            | None -> ()
                                            | Some _ -> save ()
                                        )
                                    ]
                                    Html.button [
                                        prop.testId "provenance-target-unpersistable-cancel"
                                        prop.className "swt:btn swt:btn-sm swt:btn-ghost"
                                        prop.text "Cancel"
                                        prop.onClick (fun _ -> setPendingUnpersistableSave false)
                                    ]
                                ]
                            ]
                        ]
                    ]

                Html.div [
                    prop.className "swt:min-h-0 swt:grow swt:overflow-hidden"
                    prop.children [
                        ProvenanceGrouping.Main(
                            state.Loaded.Session,
                            endpointKinds = processCoreEndpointKinds,
                            referenceCatalog = state.Loaded.ReferenceCatalog,
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

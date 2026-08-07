namespace Swate.Components.Page.ProvenanceGrouping

open Fable.Core
open Fable.Core.JsInterop
open Feliz
open Browser.Types
open Swate.Components.Page.ProvenanceGrouping.Identifiers
open Swate.Components.Page.ProvenanceGrouping.Values
open Swate.Components.Page.ProvenanceGrouping.AvailabilityTypes
open Swate.Components.Page.ProvenanceGrouping.ProjectionTypes
open Swate.Components.Primitive.ContextMenu
open Swate.Components.Primitive.ContextMenu.Types
open Swate.Components.Page.ProvenanceGrouping.Types

module private ConnectorAnnotationMenu =

    let private isPropagated (annotation: ProjectedAnnotation) =
        match annotation.Availability.Relation with
        | ForwardPropagated _
        | ReverseConnectionLocal _ -> true
        | OwnedNode
        | IncidentProcess _ -> false

    /// A displayed connector is a bulk-edit surface for the process
    /// annotations its pooled links own in this layer (intent §4), gated on
    /// unique resolvability of every entry and blocked whole otherwise, so
    /// only reverse-local stays permanently excluded here. Per the recorded
    /// decision on this menu, container-bound (Recipe Component) backings are
    /// not special-cased either — `Commands.editAvailableReferences` is the
    /// enforcement point and refuses the whole command when one is present.
    let private isEditable (annotation: ProjectedAnnotation) =
        match annotation.Availability.Relation with
        | ReverseConnectionLocal _ -> false
        | _ -> true

    let private annotationLabel (session: ProvenanceSession) (annotation: ProjectedAnnotation) =
        let header = PropertyRails.headerKeyOf annotation

        let valueText =
            let valueId =
                match annotation.Backing with
                | NodeAssignmentBacking(identity, _, _) -> identity.ValueId
                | ProcessAssignmentBacking(identity, _, _, _, _) -> identity.ValueId

            session.Values
            |> Map.tryFind valueId
            |> Option.map (fun definition -> Formatting.formatValue definition.Value definition.Unit)
            |> Option.defaultValue ""

        $"{header.Header.Name}: {valueText}"

    let items
        (session: ProvenanceSession)
        (remove: DisplayConnector -> unit)
        (removeAnnotation: (DisplayConnector -> ProjectedAnnotation list -> unit) option)
        (editAnnotation: (DisplayConnector -> ProjectedAnnotation list -> unit) option)
        (editGate: (DisplayConnector -> ProjectedAnnotation list -> string option) option)
        (removalGate: (DisplayConnector -> ProjectedAnnotation list -> string option) option)
        (data: obj)
        =
        let connector = data |> unbox<DisplayConnector>

        [
            yield
                ContextMenuItem(
                    text = Html.span "Delete connection",
                    icon =
                        Html.i [
                            prop.className "swt:iconify swt:fluent--delete-20-regular swt:size-4"
                        ],
                    onClick =
                        (fun event ->
                            event.buttonEvent.stopPropagation ()
                            remove connector
                        )
                )
            // One row per displayed value with both actions on it, the same
            // shape `GroupAnnotationMenu.items` builds on cards; an action this
            // connector cannot perform for any backing is greyed out with a
            // hint, and a value with no available action contributes no row.
            // Today a connector only ever carries its own process assignments
            // (`Projection.connectorAnnotations`), so nothing is disabled here
            // in practice - the guard keeps the two menus honest if that widens.
            if not connector.Annotations.IsEmpty then
                // Ordered alphabetically, like the card menu's partitions.
                let grouped =
                    connector.Annotations
                    |> Projection.groupProjectedAnnotations
                    |> List.sortBy (fun group -> (annotationLabel session group.Annotations.Head).ToLowerInvariant())

                for group in grouped do
                    let representative = group.Annotations.Head
                    let writableAnnotations = group.Annotations |> List.filter (isPropagated >> not)
                    let editableAnnotations = group.Annotations |> List.filter isEditable

                    let gateHint
                        (gate: (DisplayConnector -> ProjectedAnnotation list -> string option) option)
                        annotations
                        =
                        gate |> Option.bind (fun gate -> gate connector annotations)

                    let editAction =
                        match editAnnotation with
                        | Some onEdit when not editableAnnotations.IsEmpty ->
                            match gateHint editGate editableAnnotations with
                            | Some hint -> AnnotationMenuRow.ActionDisabled hint
                            | None ->
                                AnnotationMenuRow.ActionEnabled(fun (_: Browser.Types.MouseEvent) ->
                                    onEdit connector editableAnnotations
                                )
                        | _ -> AnnotationMenuRow.ActionDisabled AnnotationMenuRow.editDisabledHint

                    let removeAction =
                        match removeAnnotation with
                        | Some onRemove when not writableAnnotations.IsEmpty ->
                            match gateHint removalGate writableAnnotations with
                            | Some hint -> AnnotationMenuRow.ActionDisabled hint
                            | None ->
                                AnnotationMenuRow.ActionEnabled(fun (_: Browser.Types.MouseEvent) ->
                                    onRemove connector writableAnnotations
                                )
                        | _ -> AnnotationMenuRow.ActionDisabled AnnotationMenuRow.removeDisabledHint

                    let staticallyAvailable =
                        (editAnnotation.IsSome && not editableAnnotations.IsEmpty)
                        || (removeAnnotation.IsSome && not writableAnnotations.IsEmpty)

                    if staticallyAvailable then
                        yield
                            AnnotationMenuRow.item
                                (PropertyRails.headerKeyOf representative).Kind
                                (annotationLabel session representative)
                                None
                                editAction
                                removeAction
        ]

[<Erase; Mangle(false)>]
type ConnectorOverlay =

    [<ReactComponent>]
    static member private LiveConnectorLayer(store: LiveDrag.Store, ?debug: bool) =
        let _, bump = React.useStateWithUpdater 0

        React.useEffect (
            (fun () ->
                let unsubscribe =
                    store |> LiveDrag.subscribe (fun () -> bump (fun version -> version + 1))

                FsReact.createDisposable unsubscribe
            ),
            [| box store |]
        )

        match ConnectorPaths.liveConnection store.Current with
        | Some measured ->
            Svg.svg [
                svg.className "swt:absolute swt:inset-0 swt:pointer-events-none swt:size-full"
                svg.children [
                    yield!
                        ConnectorSvg.strokeElements
                            measured
                            measured.StrokeWidth
                            1.0
                            false
                            false
                            (defaultArg debug false)
                ]
            ]
        | None -> Html.none

    [<ReactComponent>]
    static member Main
        (
            containerRef: IRefValue<HTMLElement option>,
            layerId: ProvenanceLayerId,
            session: ProvenanceSession,
            inputGroups: DisplayGroup list,
            outputGroups: DisplayGroup list,
            connections: DisplayConnector list,
            inputRailProjection: PropertyRails.RailProjection,
            outputRailProjection: PropertyRails.RailProjection,
            overlayState: ConnectorOverlayState,
            layoutSignature: string,
            showPropertyHeaderConnectors: bool,
            liveDragStore: LiveDrag.Store,
            dragActivity: DragActivity.Store,
            onSelect: DisplayConnector -> unit,
            ?onRemove: DisplayConnector -> unit,
            ?onRemoveAnnotation: DisplayConnector -> ProjectedAnnotation list -> unit,
            ?onEditAnnotation: DisplayConnector -> ProjectedAnnotation list -> unit,
            // Menu-spawn gates: dry-run the exact command an action click
            // would issue and return the refusal reason to grey it out with,
            // or None when the action would go through.
            ?editAnnotationGate: DisplayConnector -> ProjectedAnnotation list -> string option,
            ?removeAnnotationGate: DisplayConnector -> ProjectedAnnotation list -> string option,
            ?activeDragOwnerKind: AnnotationOwnerKind option,
            ?debug: bool,
            ?railColorByHeader: Map<AnnotationHeaderKey, string option>
        ) =
        let measuredState, setMeasuredState =
            React.useStateWithUpdater ((([]: MeasuredConnector list), false))

        let hoveredKey, setHoveredKey = React.useState<string option> None
        let pendingFrame = React.useRef (None: float option)

        let hoverStore = React.useContext HoverHighlight.context
        let _, bumpHover = React.useStateWithUpdater 0

        React.useEffect (
            (fun () ->
                let unsubscribe =
                    hoverStore
                    |> HoverHighlight.subscribe (fun () -> bumpHover (fun version -> version + 1))

                FsReact.createDisposable unsubscribe
            ),
            [| box hoverStore |]
        )

        let hoveredGroup = hoverStore.Current

        let animateNextMeasure = React.useRef false
        let debugEnabled = defaultArg debug false

        let colorByHeader =
            React.useMemo ((fun () -> defaultArg railColorByHeader Map.empty), [| box railColorByHeader |])

        let specs =
            React.useMemo (
                (fun () ->
                    ConnectorPaths.specs
                        layerId
                        session
                        inputGroups
                        outputGroups
                        connections
                        inputRailProjection
                        outputRailProjection
                        colorByHeader
                        overlayState
                        showPropertyHeaderConnectors
                ),
                [|
                    box layerId
                    box session
                    box inputGroups
                    box outputGroups
                    box connections
                    box inputRailProjection
                    box outputRailProjection
                    box colorByHeader
                    box overlayState.ExpandedGroups
                    box overlayState.ExpandedProperties
                    box showPropertyHeaderConnectors
                |]
            )

        let latestSpecs = React.useRef specs

        React.useLayoutEffect ((fun () -> latestSpecs.current <- specs), [| box specs |])

        let setMeasuredPaths animate next =
            setMeasuredState (fun (current, currentAnimate) ->
                if current = next then
                    (current, currentAnimate)
                else
                    (next, animate)
            )

        let measureNow () =
            pendingFrame.current <- None
            let animate = animateNextMeasure.current
            animateNextMeasure.current <- false

            match containerRef.current with
            | None -> setMeasuredPaths false []
            | Some container ->
                let context = ConnectorMeasure.createContext container

                ConnectorPaths.measure context latestSpecs.current |> setMeasuredPaths animate

        // While a drag is in flight nothing the overlay draws moves (the drag
        // preview lives outside the container), so layout-driven remeasuring is
        // suspended and a single catch-up runs when the drag ends.
        let scheduleMeasure () =
            if not dragActivity.Active then
                match pendingFrame.current with
                | Some _ -> ()
                | None -> pendingFrame.current <- Some(AnimationFrame.request measureNow)

        let cancelPendingFrame () =
            match pendingFrame.current with
            | Some handle ->
                AnimationFrame.cancel handle
                pendingFrame.current <- None
            | None -> ()

        React.useEffectOnce (fun () ->
            match containerRef.current with
            | None -> FsReact.createDisposable cancelPendingFrame
            | Some container ->
                let onLayout = fun (_: Event) -> scheduleMeasure ()
                container.addEventListener ("scroll", onLayout)
                Browser.Dom.window.addEventListener ("resize", onLayout)

                let observer = ConnectorObserver.create scheduleMeasure

                let observeCurrentNodes () =
                    ConnectorObserver.observeNode observer container

                    ConnectorObserver.observeMatching
                        container
                        "[data-provenance-group-node],[data-provenance-member-node],[data-provenance-connection-node],[data-provenance-resize-node]"
                        observer

                let mutationFrame = ref (None: float option)
                let mutationNeedsObserve = ref false

                let cancelMutationFrame () =
                    match mutationFrame.Value with
                    | Some handle ->
                        AnimationFrame.cancel handle
                        mutationFrame.Value <- None
                    | None -> ()

                // Re-collecting and re-observing every anchor node is only
                // needed when nodes were added or removed; attribute-only
                // batches just remeasure the nodes already known.
                let scheduleMutationMeasure needsObserve =
                    if dragActivity.Active then
                        ()
                    else
                        mutationNeedsObserve.Value <- mutationNeedsObserve.Value || needsObserve

                        match mutationFrame.Value with
                        | Some _ -> ()
                        | None ->
                            mutationFrame.Value <-
                                Some(
                                    AnimationFrame.request (fun () ->
                                        mutationFrame.Value <- None

                                        if mutationNeedsObserve.Value then
                                            mutationNeedsObserve.Value <- false
                                            observeCurrentNodes ()

                                        measureNow ()
                                    )
                                )

                let mutationObserver = ConnectorMutationObserver.create scheduleMutationMeasure

                observeCurrentNodes ()
                ConnectorMutationObserver.observe mutationObserver container

                // The catch-up for everything skipped during the drag: one
                // full re-observe + remeasure once the store deactivates.
                let unsubscribeDragActivity =
                    dragActivity
                    |> DragActivity.subscribe (fun () ->
                        if not dragActivity.Active then
                            scheduleMutationMeasure true
                    )

                FsReact.createDisposable (fun () ->
                    unsubscribeDragActivity ()
                    container.removeEventListener ("scroll", onLayout)
                    Browser.Dom.window.removeEventListener ("resize", onLayout)
                    ConnectorObserver.disconnect observer
                    ConnectorMutationObserver.disconnect mutationObserver
                    cancelMutationFrame ()
                    cancelPendingFrame ()
                )
        )

        React.useEffect (
            (fun () ->
                animateNextMeasure.current <- true
                measureNow ()
            ),
            [| box specs |]
        )

        React.useEffect (
            (fun () ->
                animateNextMeasure.current <- true
                scheduleMeasure ()
            ),
            [| box layoutSignature |]
        )

        let selectedConnectionId = overlayState.SelectedConnectionId
        let paths, measuredWithAnimation = measuredState
        let animatePaths = measuredWithAnimation && not (Motion.prefersReduced ())

        let renderedKeys = React.useRef (Set.empty: Set<string>)
        let previousKeys = renderedKeys.current

        React.useEffect (fun () -> renderedKeys.current <- paths |> List.map (fun m -> m.Key) |> Set.ofList)

        let valueDragKind = activeDragOwnerKind |> Option.bind id

        React.Fragment [
            Svg.svg [
                svg.className "swt:absolute swt:inset-0 swt:pointer-events-none swt:size-full"
                svg.children [
                    for measured in paths do
                        let activateFromKeyboard (event: KeyboardEvent) =
                            match measured.InteractiveConnector, event.key with
                            | Some connector, "Enter"
                            | Some connector, " "
                            | Some connector, "Spacebar" ->
                                event.preventDefault ()
                                onSelect connector
                            | Some connector, "Delete"
                            | Some connector, "Backspace" ->
                                match onRemove with
                                | Some remove ->
                                    event.preventDefault ()
                                    remove connector
                                | None -> ()
                            | _ -> ()

                        let isSelected =
                            match measured.InteractiveConnector, selectedConnectionId with
                            | Some connector, Some selectedId -> connector.Id = selectedId
                            | _ -> false

                        let isHoverRelated =
                            match measured.InteractiveConnector, hoveredGroup with
                            | Some connector, Some target ->
                                (target.Side = ProvenanceSide.Input && connector.InputGroupId = target.GroupId)
                                || (target.Side = ProvenanceSide.Output && connector.OutputGroupId = target.GroupId)
                            | _ -> false

                        let isEmphasized = isSelected || hoveredKey = Some measured.Key || isHoverRelated

                        // While a process value is in flight every existing edge is
                        // a legal target, so they all announce themselves - the same
                        // move the group cards make with their faint ring. A node
                        // value can never land on an edge, so those drags leave the
                        // edges inert rather than promising a drop that is refused.
                        let isDropCandidate =
                            measured.InteractiveConnector.IsSome
                            && valueDragKind = Some AnnotationOwnerKind.Process

                        let strokeWidth =
                            if isEmphasized then measured.StrokeWidth + 1.25
                            elif isDropCandidate then measured.StrokeWidth + 0.25
                            else measured.StrokeWidth

                        let strokeOpacity =
                            match measured.InteractiveConnector with
                            | Some _ when isEmphasized -> 1.0
                            | Some _ when isDropCandidate -> 1.0
                            | Some _ when selectedConnectionId.IsSome -> 0.3
                            | Some _ -> 0.85
                            | None -> 1.0

                        let debugAttributes = ConnectorSvg.debugAttributes debugEnabled measured

                        Svg.g [
                            svg.key measured.Key
                            // Hovering a candidate edge while a process value is in
                            // flight upgrades the whole edge with a primary glow: the
                            // drag handlers hit-test per move and mark the edge under
                            // the pointer with the drop-hover data attribute (mouse
                            // events cannot reach it - the drag preview intercepts
                            // them), mirroring the strong ring on a hovered card.
                            svg.className
                                "swt:data-[provenance-drop-hover=true]:[filter:drop-shadow(0_0_3px_var(--color-primary))]"
                            // The drop-candidate state is announced on the group so
                            // the whole edge - halo, stroke and pooled-count badge -
                            // brightens together, and so a story can assert it.
                            if isDropCandidate then
                                svg.custom ("data-provenance-drop-candidate", "true")
                            svg.children [
                                yield!
                                    ConnectorSvg.strokeElements
                                        measured
                                        strokeWidth
                                        strokeOpacity
                                        animatePaths
                                        (not (previousKeys.Contains measured.Key))
                                        debugEnabled
                                match measured.InteractiveConnector, measured.Midpoint with
                                | Some connector, Some midpoint when connector.LinkIds.Count > 1 ->
                                    let countText = string connector.LinkIds.Count
                                    let radius = if countText.Length > 2 then 12. else 9.

                                    Svg.g [
                                        svg.className "swt:pointer-events-none"
                                        svg.custom ("opacity", strokeOpacity)
                                        if debugEnabled then
                                            svg.custom ("data-testid", "provenance-connection-count")
                                            svg.custom ("data-provenance-connection-key", measured.Key)
                                        svg.children [
                                            Svg.circle [
                                                svg.cx midpoint.X
                                                svg.cy midpoint.Y
                                                svg.r radius
                                                svg.custom ("fill", "var(--color-base-100)")
                                                svg.custom ("stroke", "var(--color-primary)")
                                                svg.custom ("strokeWidth", 1.5)
                                            ]
                                            Svg.text [
                                                svg.x midpoint.X
                                                svg.y midpoint.Y
                                                svg.custom ("textAnchor", "middle")
                                                svg.custom ("dominantBaseline", "central")
                                                svg.custom ("fill", "var(--color-primary)")
                                                svg.custom ("fontSize", 10)
                                                svg.custom ("fontWeight", 600)
                                                svg.text countText
                                            ]
                                        ]
                                    ]
                                | _ -> ()
                                match measured.InteractiveConnector with
                                | Some connector ->
                                    let hitPath = measured.RibbonPath |> Option.defaultValue measured.Path

                                    let cursorClass =
                                        match valueDragKind with
                                        | Some AnnotationOwnerKind.Process ->
                                            "swt:pointer-events-auto swt:cursor-copy swt:outline-none swt:shadow-none"
                                        | Some AnnotationOwnerKind.Node ->
                                            "swt:pointer-events-auto swt:cursor-not-allowed swt:outline-none swt:shadow-none"
                                        | _ ->
                                            "swt:pointer-events-auto swt:cursor-pointer swt:outline-none swt:shadow-none"

                                    Svg.path [
                                        svg.d hitPath
                                        svg.custom ("style", ConnectorSvg.pathStyle hitPath animatePaths)

                                        match measured.RibbonPath with
                                        | Some _ ->
                                            svg.fill "transparent"
                                            svg.stroke "none"
                                        | None ->
                                            svg.fill "none"
                                            svg.stroke "transparent"
                                            svg.strokeWidth 14
                                        svg.className cursorClass
                                        svg.custom ("tabIndex", "0")
                                        svg.custom ("role", "button")
                                        svg.custom (
                                            "aria-label",
                                            measured.AriaLabel
                                            |> Option.defaultValue $"Select connection {connector.Id}"
                                        )
                                        svg.custom (ConnectorContextMenu.connectionKeyAttribute, measured.Key)
                                        svg.custom ("data-provenance-connector-edge-id", connector.Id)
                                        yield! debugAttributes
                                        svg.onClick (fun _ -> onSelect connector)
                                        svg.onKeyDown activateFromKeyboard
                                        svg.onMouseEnter (fun _ -> setHoveredKey (Some measured.Key))
                                        svg.onMouseLeave (fun _ -> setHoveredKey None)
                                        svg.onFocus (fun _ -> setHoveredKey (Some measured.Key))
                                        svg.onBlur (fun _ -> setHoveredKey None)
                                    ]
                                | None -> ()
                            ]
                        ]
                ]
            ]
            ConnectorOverlay.LiveConnectorLayer(liveDragStore, ?debug = debug)
            match onRemove with
            | Some remove ->
                ContextMenu.ContextMenu(
                    ConnectorAnnotationMenu.items
                        session
                        remove
                        onRemoveAnnotation
                        onEditAnnotation
                        editAnnotationGate
                        removeAnnotationGate,
                    ref = containerRef,
                    onSpawn = ConnectorContextMenu.spawnData paths,
                    debug = debugEnabled
                )
            | None -> Html.none
        ]

namespace Swate.Components.Page.ProvenanceGrouping

open System
open Fable.Core
open Fable.Core.JsInterop
open Feliz
open Swate.Components.Composite.FolderedDraggableList
open Swate.Components.Composite.FolderedDraggableList.Types
open Swate.Components.JsBindings
open Swate.Components.Page.ProvenanceGrouping.Identifiers
open Swate.Components.Page.ProvenanceGrouping.Values
open Swate.Components.Page.ProvenanceGrouping.Domain
open Swate.Components.Page.ProvenanceGrouping.MutationTypes
open Swate.Components.Page.ProvenanceGrouping.AvailabilityTypes
open Swate.Components.Page.ProvenanceGrouping.ProjectionTypes
open Swate.Components.Page.ProvenanceGrouping.Types

type EditorLookups = {
    FindGroup: ProvenanceSide -> string -> DisplayGroup option
    FindProperty: string -> AnnotationHeaderKey option
    FindValueDefinition: PropertyValueDefinitionId -> PropertyValueDefinition option
}

type DragContext = {
    Session: ProvenanceSession
    ReferenceCatalog: ReferenceCatalog
    Layer: ProvenanceLayer
    Projection: CachedLayerProjection
    /// The connectors the editor is currently *showing*. They are re-pooled from
    /// the grouping selection, so they are not `Projection.Connectors` and a drop
    /// must resolve its connector id against these.
    Connectors: DisplayConnector list
    UiState: UiState
    GetUiState: unit -> UiState
    Publish: Result<ProvenanceSession, ProvenanceCommandError> -> unit
    SetUiState: UiState -> unit
    Lookups: EditorLookups
    ConnectNodePairs: (CanonicalNodeId * CanonicalNodeId) list -> unit
}

type ActiveDrag = {
    Payload: DragDrop.Payload
    Label: string option
}

type PropertyShelfItemPayload = {
    Property: AnnotationHeaderKey
    SourceSide: ProvenanceSide
    ShelfPayload: PropertyShelfPayload
}

module EditorLookups =

    let create
        (session: ProvenanceSession)
        (projection: CachedLayerProjection)
        (layer: ProvenanceLayer)
        uiState
        inputGroups
        outputGroups
        =
        let findGroup side groupId =
            let groups: DisplayGroup list =
                if side = ProvenanceSide.Input then
                    inputGroups
                else
                    outputGroups

            groups |> List.tryFind (fun (group: DisplayGroup) -> group.Id = groupId)

        let knownProperties =
            lazy
                ([
                    yield! PropertyRails.headersForSide ProvenanceSide.Input projection
                    yield! PropertyRails.headersForSide ProvenanceSide.Output projection
                    yield! State.Drafts.propertiesForSide layer.Id ProvenanceSide.Input uiState
                    yield! State.Drafts.propertiesForSide layer.Id ProvenanceSide.Output uiState
                 ]
                 |> List.distinct)

        let findProperty propertyId =
            knownProperties.Value
            |> List.tryFind (fun property -> DragDrop.propertyKeyIdentity property = propertyId)

        let findValueDefinition valueId = session.Values |> Map.tryFind valueId

        {
            FindGroup = findGroup
            FindProperty = findProperty
            FindValueDefinition = findValueDefinition
        }

module SessionErrors =

    let text error =
        match error with
        | ProvenanceCommandError.LayerNotFound layerId -> $"The layer '{layerId}' no longer exists."
        | NodeNotFound nodeId -> $"The entity '{nodeId}' no longer exists in this layer."
        | ProcessNotFound processId -> $"The process '{processId}' no longer exists."
        | LinkNotFound linkId -> $"The link '{linkId}' no longer exists."
        | AssignmentNotFound(_, assignmentId) -> $"The assignment '{assignmentId}' no longer exists."
        | PropertyNotFound propertyId -> $"The property '{propertyId}' no longer exists."
        | ValueNotFound valueId -> $"The value '{valueId}' no longer exists."
        | DuplicateEndpointAppearance key -> $"An endpoint for '{key.NodeId}' already exists on this side."
        | EmptyTarget -> "Drop a value onto a group with at least one entity."
        | AmbiguousPooledEdit _ ->
            "This annotation does not resolve to a unique owning assignment for every entry, so nothing was changed."
        | ReadOnlyReverseLocalEdit _ ->
            "This annotation is read-only because it is propagated through a reverse connection."
        | ReadOnlyReverseLocalRemoval _ ->
            "This annotation cannot be removed because it is propagated through a reverse connection."
        | PropagatedRemovalAtReceiver _ -> "This annotation is propagated and can only be removed at its origin."
        | OverwriteConfirmationRequired _ ->
            "This change would replace an existing value. Please confirm the overwrite."
        | MultiplePropertyValues _ ->
            "Cannot overwrite: multiple distinct values exist for this annotation, so no single value can be replaced."
        | MixedPropertyValueCounts _ ->
            "Cannot assign: every target must either have no value or exactly one value for this annotation."
        | MissingReferenceContainer _ -> "This value requires a reference container that is not present on the target."
        | ReferenceSlotOccupied _ -> "This reference slot is already occupied on the target."
        | ReadOnlyAdapterResourceMutation -> "This resource is managed externally and cannot be modified here."
        | InconsistentCanonicalState details -> $"Internal error: {details}"
        | InconsistentLayerProjection(layerId, details) -> $"Internal error in layer '{layerId}': {details}"

module EditorActions =

    let addLayer session inputGroups outputGroups uiState name publish =
        let request =
            Display.layerRequest name session.ActiveLayerId inputGroups outputGroups uiState

        Session.addLayer request.Name request.SelectedNodes session |> publish

    let createEndpoint session publish layerId side kind header name layerOrderPosition =
        Session.addEndpoint layerId side kind header name layerOrderPosition session
        |> publish

    let requestEffectWithSource
        (source: ValueAssignmentSource option)
        (session: ProvenanceSession)
        (request: ValueAssignmentRequest)
        =
        let content: Commands.NodeValueContent = {
            Category = request.Category
            Value = request.Value
            Unit = request.Unit
        }

        match request.Target with
        | NodeTargets nodeIds ->
            match source |> Option.bind _.CopiedFromAssignmentId with
            | Some sourceAssignmentId ->
                session.Nodes
                |> Map.toList
                |> List.tryPick (fun (nodeId, node) ->
                    if node.Assignments |> Map.containsKey sourceAssignmentId then
                        Some nodeId
                    else
                        None
                )
                |> function
                    | Some sourceOwnerId ->
                        Commands.copyLoadedNodeValue sourceOwnerId sourceAssignmentId nodeIds None session
                    | None -> Error(AssignmentNotFound(None, sourceAssignmentId))
            | None ->
                let draft: Commands.NodeAssignmentDraft = {
                    Content = content
                    OwnerKind = request.OwnerKind
                    PropertyKind = request.PropertyKind
                }

                Commands.assignNodeValue nodeIds draft Commands.NoOverwrite session
        | ProcessTargets linkIds ->
            let draft: Commands.ProcessAssignmentDraft = {
                Content = content
                OwnerKind = request.OwnerKind
                PropertyKind = request.PropertyKind
                ContainerReferenceValueId = source |> Option.bind _.ContainerReferenceValueId
                ReferenceSlotId = source |> Option.bind _.ReferenceSlotId
                Lineage =
                    source
                    |> Option.bind _.CopiedFromAssignmentId
                    |> Option.map AssignmentLineage.DerivedFrom
                    |> Option.defaultValue AssignmentLineage.Created
            }

            Commands.assignProcessValue linkIds draft session

    let private removalReferences (visibleLinkIds: Set<ProcessLinkId>) (annotations: ProjectedAnnotation list) =
        annotations
        |> List.map (fun annotation ->
            let reference = Projection.availableReferenceOfAnnotation annotation

            match reference.Owner with
            | ProcessOwner _ ->
                let selectedLinks =
                    visibleLinkIds |> Set.intersect annotation.Availability.OriginatingLinkIds

                {
                    reference with
                        OriginatingLinkIds = selectedLinks
                        VisibleThroughLinkIds = selectedLinks
                }
            | NodeOwner _ -> reference
        )

    let removeProjectedAnnotations
        (receiverId: CanonicalNodeId)
        (visibleLinkIds: Set<ProcessLinkId>)
        (session: ProvenanceSession)
        (annotations: ProjectedAnnotation list)
        : Result<ProvenanceSession, ProvenanceCommandError> =
        Commands.removeAvailableReferences receiverId (removalReferences visibleLinkIds annotations) session
        |> Result.map (fun effect -> Session.commit effect session)

    /// Dry-runs the exact removal `removeProjectedAnnotations` would issue,
    /// without committing. Commands are pure - they only describe an effect -
    /// so a surface can ask whether the removal would be refused, and why,
    /// before offering it.
    let precheckRemoveProjectedAnnotations
        (receiverId: CanonicalNodeId)
        (visibleLinkIds: Set<ProcessLinkId>)
        (session: ProvenanceSession)
        (annotations: ProjectedAnnotation list)
        : Result<unit, ProvenanceCommandError> =
        Commands.removeAvailableReferences receiverId (removalReferences visibleLinkIds annotations) session
        |> Result.map ignore

    let private editReferences (visibleLinkIds: Set<ProcessLinkId>) (annotations: ProjectedAnnotation list) =
        annotations
        |> List.map (fun annotation ->
            let reference = Projection.availableReferenceOfAnnotation annotation

            match reference.Owner with
            | ProcessOwner _ ->
                let selectedLinks =
                    visibleLinkIds |> Set.intersect annotation.Availability.OriginatingLinkIds

                // A propagated process annotation originates on links the
                // receiving surface does not itself show, so narrowing to the
                // visible links would leave nothing to edit. Fall back to the
                // annotation's own originating links, which is what design §4
                // means by resolving a downstream reference to its origin.
                let resolvedLinks =
                    if selectedLinks.IsEmpty then
                        annotation.Availability.OriginatingLinkIds
                    else
                        selectedLinks

                {
                    reference with
                        OriginatingLinkIds = resolvedLinks
                        VisibleThroughLinkIds = resolvedLinks
                }
            | NodeOwner _ -> reference
        )

    /// Downstream editing of a displayed annotation (design §4/§7.2): every
    /// surface bulk-edits the assignments behind the displayed value when each
    /// entry resolves uniquely; a reverse-local reference or a container-bound
    /// backing blocks the whole command in
    /// `Commands.editAvailableReferences` itself.
    let editProjectedAnnotations
        (receiverId: CanonicalNodeId)
        (visibleLinkIds: Set<ProcessLinkId>)
        (session: ProvenanceSession)
        (annotations: ProjectedAnnotation list)
        (content: Commands.NodeValueContent)
        : Result<ProvenanceSession, ProvenanceCommandError> =
        Commands.editAvailableReferences receiverId (editReferences visibleLinkIds annotations) content session
        |> Result.map (fun effect -> Session.commit effect session)

    /// Dry-runs the exact edit `editProjectedAnnotations` would issue, without
    /// committing. Every gate in `Commands.editAvailableReferences` - empty
    /// target, reverse-local, the resolvability gate, a container-bound
    /// backing inside `atomic` - is value-independent, so the entry's current
    /// value stands in for whatever the user would type into the edit form.
    let precheckEditProjectedAnnotations
        (receiverId: CanonicalNodeId)
        (visibleLinkIds: Set<ProcessLinkId>)
        (session: ProvenanceSession)
        (annotations: ProjectedAnnotation list)
        : Result<unit, ProvenanceCommandError> =
        match annotations with
        | [] -> Error EmptyTarget
        | representative :: _ ->
            let header = PropertyRails.headerKeyOf representative

            let valueId =
                match representative.Backing with
                | NodeAssignmentBacking(identity, _, _) -> identity.ValueId
                | ProcessAssignmentBacking(identity, _, _, _, _) -> identity.ValueId

            match session.Values |> Map.tryFind valueId with
            | None -> Error(ValueNotFound valueId)
            | Some definition ->
                let content: Commands.NodeValueContent = {
                    Category = header.Header
                    Value = definition.Value
                    Unit = definition.Unit
                }

                Commands.editAvailableReferences receiverId (editReferences visibleLinkIds annotations) content session
                |> Result.map ignore

    let private overwriteEffectWithSource
        (_source: ValueAssignmentSource option)
        (session: ProvenanceSession)
        (warning: ValueAssignmentWarning)
        =
        let content: Commands.NodeValueContent = {
            Category = warning.Header
            Value = warning.Value
            Unit = warning.Unit
        }

        let targetLinks =
            match warning.Target with
            | ProcessTargets linkIds -> linkIds
            | NodeTargets _ -> Set.empty

        let editOperations =
            warning.ExistingAssignmentIds
            |> Set.toList
            |> List.map (fun assignmentId ->
                fun current ->
                    let nodeOwner =
                        current.Nodes
                        |> Map.tryPick (fun nodeId node ->
                            if node.Assignments |> Map.containsKey assignmentId then
                                Some nodeId
                            else
                                None
                        )

                    match nodeOwner with
                    | Some nodeId -> Commands.editNodeAssignment nodeId assignmentId content current
                    | None ->
                        let processOwner =
                            current.Processes
                            |> Map.tryPick (fun processId proc ->
                                if proc.Assignments |> Map.containsKey assignmentId then
                                    Some(processId, proc.Assignments.[assignmentId])
                                else
                                    None
                            )

                        match processOwner with
                        | Some(processId, assignment) ->
                            Commands.editProcessAssignmentSubset
                                processId
                                assignmentId
                                (Set.intersect targetLinks assignment.CoveredLinkIds)
                                content
                                current
                        | None -> Commands.atomic [] current
            )

        Commands.atomic editOperations session

    let applyAssignmentBatchWithSource
        session
        publish
        (source: ValueAssignmentSource option)
        (batch: PropertyAssignmentBatch)
        =
        let overwriteOperations =
            batch.Overwrites
            |> List.map (fun warning current -> overwriteEffectWithSource source current warning)

        let addOperations =
            batch.Adds
            |> List.map (fun request -> fun current -> requestEffectWithSource source current request)

        Commands.atomic (overwriteOperations @ addOperations) session
        |> Result.map (fun effect -> Session.commit effect session)
        |> publish

    let connectNodePairs session (layer: ProvenanceLayer) publish pairs =
        Session.connectNodes layer.Id (pairs |> List.distinct) session |> publish

    let orderedMemberPairs (layer: ProvenanceLayer) (inputGroup: DisplayGroup) (outputGroup: DisplayGroup) =
        let orderByPosition (endpoints: Map<CanonicalNodeId, LayerEndpoint>) (nodeIds: Set<CanonicalNodeId>) =
            nodeIds
            |> Set.toList
            |> List.choose (fun nodeId ->
                endpoints
                |> Map.tryFind nodeId
                |> Option.map (fun ep -> nodeId, ep.LayerOrderPosition)
            )
            |> List.sortBy snd
            |> List.map fst

        let inputNodes = orderByPosition layer.InputEndpoints inputGroup.CanonicalNodeIds
        let outputNodes = orderByPosition layer.OutputEndpoints outputGroup.CanonicalNodeIds

        if inputNodes.Length = outputNodes.Length then
            List.zip inputNodes outputNodes |> Some
        else
            None

    let allMemberPairs (inputGroup: DisplayGroup) (outputGroup: DisplayGroup) = [
        for inputNodeId in inputGroup.CanonicalNodeIds do
            for outputNodeId in outputGroup.CanonicalNodeIds do
                inputNodeId, outputNodeId
    ]

module HandleMeasure =

    let private tryDocumentNode (handle: ConnectionHandleRef) =
        let node: Browser.Types.HTMLElement =
            !!Browser.Dom.document.querySelector($"[data-provenance-connection-node='{DragDrop.connectionHandleNodeId handle}']")

        if isNull node then None else Some node

    let tryCenter (surface: Browser.Types.HTMLElement) (handle: ConnectionHandleRef) =
        match tryDocumentNode handle with
        | Some node ->
            let origin = surface.getBoundingClientRect ()
            let rect = node.getBoundingClientRect ()

            Some {
                X = rect.left - origin.left + float surface.scrollLeft + rect.width / 2.
                Y = rect.top - origin.top + float surface.scrollTop + rect.height / 2.
            }
        | None -> None

    let tryViewportCenter (handle: ConnectionHandleRef) =
        tryDocumentNode handle
        |> Option.map (fun node ->
            let rect = node.getBoundingClientRect ()

            {
                X = rect.left + rect.width / 2.
                Y = rect.top + rect.height / 2.
            }
        )

module DropHitTesting =

    [<Emit("document.elementsFromPoint($0, $1)")>]
    let private elementsFromPoint (_x: float) (_y: float) : Browser.Types.HTMLElement[] = jsNative

    [<Emit("Array.from(document.querySelectorAll($0))")>]
    let private queryAll (_selector: string) : Browser.Types.HTMLElement[] = jsNative

    /// `activatorEvent + delta` is a point in the drag-start viewport frame:
    /// dnd-kit folds window scrolling that happens mid-drag (autoscroll) into
    /// the delta. Recorded at drag start, this converts such points back to
    /// the live viewport frame that `elementsFromPoint` and client rects use.
    /// One drag runs at a time, so a single shared record is enough.
    let mutable private dragStartScroll = {| X = 0.0; Y = 0.0 |}

    let recordDragStartScroll () =
        dragStartScroll <- {|
            X = Browser.Dom.window.scrollX
            Y = Browser.Dom.window.scrollY
        |}

    let private toViewportFrame (point: ConnectionPoint) = {
        X = point.X - (Browser.Dom.window.scrollX - dragStartScroll.X)
        Y = point.Y - (Browser.Dom.window.scrollY - dragStartScroll.Y)
    }

    /// `elementsFromPoint` sees nothing outside the viewport, but a drop can
    /// legitimately land there (dnd-kit's own collision math is pure rect
    /// arithmetic and resolved such drops before cards and handles stopped
    /// being droppables). An empty hit list - within the viewport it always
    /// contains at least the root element - falls back to a geometric scan of
    /// the selector's candidates.
    let private candidatesAtPoint startFramePoint (selector: string) =
        let point = toViewportFrame startFramePoint
        let hit = elementsFromPoint point.X point.Y

        if hit.Length > 0 then
            hit
        else
            queryAll selector
            |> Array.filter (fun element ->
                let rect = element.getBoundingClientRect ()

                point.X >= rect.left
                && point.X <= rect.right
                && point.Y >= rect.top
                && point.Y <= rect.bottom
            )

    [<Emit("$0.closest($1)")>]
    let private closest (_element: Browser.Types.HTMLElement) (_selector: string) : Browser.Types.HTMLElement = jsNative

    let private attribute name (element: Browser.Types.HTMLElement) =
        let value = element.getAttribute name
        if isNull value then None else Some value

    let private closestAttribute selector attributeName (element: Browser.Types.HTMLElement) =
        let node = closest element selector
        if isNull node then None else attribute attributeName node

    let private endpoint source (event: DndKit.IDndKitEvent) =
        let activator = event.activatorEvent

        if isNull (box activator) then
            HandleMeasure.tryViewportCenter source
            |> Option.map (fun start -> {
                X = start.X + event.delta.x
                Y = start.Y + event.delta.y
            })
        else
            // The draggable handle itself already carries dnd-kit's transform at
            // drag end. Measuring its current centre and then adding `delta`
            // applies the movement twice. The activator coordinates are the
            // untransformed pointer origin, so origin + delta is the true final
            // pointer used by the fallback hit test.
            Some {
                X = activator.clientX + event.delta.x
                Y = activator.clientY + event.delta.y
            }

    let private targetHandleAt point source =
        let elements = candidatesAtPoint point "[data-provenance-connection-drop-id]"

        elements
        |> Array.tryPick (fun element ->
            closestAttribute "[data-provenance-connection-drop-id]" "data-provenance-connection-drop-id" element
            |> Option.bind DragDrop.tryConnectionDropId
            |> Option.bind (fun target -> if target = source then None else Some target)
        )

    let connectionTarget source event =
        endpoint source event |> Option.bind (fun point -> targetHandleAt point source)

    /// The connector is an SVG path, not a dnd-kit droppable, so a value dropped
    /// on an edge has to be resolved by hit-testing the pointer instead of by
    /// `event.over`. The overlay already tags each interactive path with its
    /// connector id for exactly this.
    let connectorEdgeAt (event: DndKit.IDndKitEvent) =
        let activatorPoint =
            let activator = event.activatorEvent

            if isNull (box activator) then
                None
            else
                Some {
                    X = activator.clientX + event.delta.x
                    Y = activator.clientY + event.delta.y
                }

        activatorPoint
        |> Option.bind (fun point ->
            elementsFromPoint point.X point.Y
            |> Array.tryPick (
                closestAttribute "[data-provenance-connector-edge-id]" "data-provenance-connector-edge-id"
            )
        )

    /// Same technique for member rows: one droppable per row would need a hook
    /// per row, which React forbids inside the card's member loop.
    let memberDropAt (event: DndKit.IDndKitEvent) =
        let activator = event.activatorEvent

        if isNull (box activator) then
            None
        else
            elementsFromPoint (activator.clientX + event.delta.x) (activator.clientY + event.delta.y)
            |> Array.tryPick (closestAttribute "[data-provenance-member-drop-id]" "data-provenance-member-drop-id")
            |> Option.bind DragDrop.tryMemberDropId

    /// And for whole cards: group cards are not dnd-kit droppables (a droppable
    /// per card re-renders every card on each over change), so a value dropped
    /// on one resolves through the card's advertised drop id instead.
    let groupDropAt (event: DndKit.IDndKitEvent) =
        let activator = event.activatorEvent

        if isNull (box activator) then
            None
        else
            candidatesAtPoint
                {
                    X = activator.clientX + event.delta.x
                    Y = activator.clientY + event.delta.y
                }
                "[data-provenance-group-drop-id]"
            |> Array.tryPick (closestAttribute "[data-provenance-group-drop-id]" "data-provenance-group-drop-id")
            |> Option.bind DragDrop.tryDropId

    /// The element (not id) under the pointer matching a drop selector, for the
    /// per-move drop-hover marking.
    let targetNodeAt (event: DndKit.IDndKitEvent) (selector: string) =
        let activator = event.activatorEvent

        if isNull (box activator) then
            None
        else
            elementsFromPoint (activator.clientX + event.delta.x) (activator.clientY + event.delta.y)
            |> Array.tryPick (fun element ->
                let node = closest element selector
                if isNull node then None else Some node
            )

/// Marks the drop target under the pointer with a data attribute while a drag
/// runs. This replaces the per-card and per-handle dnd-kit droppables whose only
/// remaining job was the `isOver` ring: the attribute drives the same ring via
/// CSS without any per-target hook subscriptions, so crossing a target no longer
/// re-renders every draggable and droppable in the editor.
module DropHover =

    let attributeName = "data-provenance-drop-hover"

    type Target =
        /// A value chip in flight. Process values can also land on connector
        /// edges, so those drags mark the hovered edge too; a node value only
        /// ever targets cards (and their member rows).
        | ValueTargets of includeConnectors: bool
        | ConnectionHandles of excludeDropId: string

    type Store = {
        mutable Active: Target option
        mutable Current: Browser.Types.HTMLElement option
    }

    let create () : Store = { Active = None; Current = None }

    let private mark (next: Browser.Types.HTMLElement option) (store: Store) =
        match store.Current, next with
        | Some current, Some nextNode when System.Object.ReferenceEquals(current, nextNode) -> ()
        | current, next ->
            current |> Option.iter (fun node -> node.removeAttribute attributeName)
            next |> Option.iter (fun node -> node.setAttribute (attributeName, "true"))
            store.Current <- next

    let start payload (valueKind: AnnotationOwnerKind option) (store: Store) =
        store.Active <-
            match payload with
            | Some(DragDrop.Payload.PropertyValue _)
            | Some(DragDrop.Payload.CatalogValue _) ->
                Some(Target.ValueTargets(valueKind = Some AnnotationOwnerKind.Process))
            | Some(DragDrop.Payload.ConnectionHandle handle) ->
                Some(Target.ConnectionHandles(DragDrop.connectionHandleDropId handle))
            | _ -> None

        mark None store

    let clear (store: Store) =
        mark None store
        store.Active <- None

    let update (event: DndKit.IDndKitEvent) (store: Store) =
        match store.Active with
        | None -> ()
        | Some(Target.ValueTargets includeConnectors) ->
            // The edge is tried first to mirror the drop routing, where a
            // connector under the pointer wins over the card beneath it.
            let edge =
                if includeConnectors then
                    DropHitTesting.targetNodeAt event "[data-provenance-drop-candidate]"
                else
                    None

            let next =
                edge
                |> Option.orElseWith (fun () -> DropHitTesting.targetNodeAt event "[data-provenance-group-drop-id]")

            mark next store
        | Some(Target.ConnectionHandles excludeDropId) ->
            let next =
                DropHitTesting.targetNodeAt event "[data-provenance-connection-drop-id]"
                |> Option.filter (fun node -> node.getAttribute "data-provenance-connection-drop-id" <> excludeDropId)

            mark next store

module DragHandlers =

    let private activeLabel (event: DndKit.IDndKitEvent) =
        if
            isNull event.active
            || isNull event.active.data
            || isNull event.active.data.current
        then
            None
        else
            let labelObj: obj = event.active.data.current?label

            if isNull labelObj then
                None
            else
                let label = string labelObj

                if String.IsNullOrWhiteSpace label || label = "undefined" then
                    None
                else
                    Some label

    let handleStart
        (surfaceRef: IRefValue<Browser.Types.HTMLElement option>)
        setActiveDrag
        (liveDragStore: LiveDrag.Store)
        (event: DndKit.IDndKitEvent)
        =
        let payload = DragDrop.tryDragId (string event.active.id)

        DropHitTesting.recordDragStartScroll ()

        setActiveDrag (
            payload
            |> Option.map (fun payload -> {
                Payload = payload
                Label = activeLabel event
            })
        )

        match payload, surfaceRef.current with
        | Some(DragDrop.Payload.ConnectionHandle handle), Some surface ->
            HandleMeasure.tryCenter surface handle
            |> Option.iter (fun point -> LiveDrag.start handle point liveDragStore)
        | _ -> ()

    let handleMove (liveDragStore: LiveDrag.Store) (dropHoverStore: DropHover.Store) (event: DndKit.IDndKitMoveEvent) =
        match liveDragStore.Current with
        | Some live ->
            LiveDrag.moveTo
                {
                    X = live.Start.X + event.delta.x
                    Y = live.Start.Y + event.delta.y
                }
                liveDragStore
        | None -> ()

        DropHover.update event dropHoverStore

    let private pulseDropTarget side groupId (source: ValueAssignmentSource) =
        Motion.requestFrame (fun () ->
            let cardNode: Browser.Types.HTMLElement =
                !!
                    Browser.Dom.document.querySelector
                    ($"[data-provenance-group-node='{DragDrop.groupNodeId side groupId}']")

            let mutable pulsedCard = false

            if not (isNull cardNode) then
                Motion.pulse cardNode
                pulsedCard <- true

            let identity = DragDrop.groupingValueIdentity source.Key source.Value source.Unit

            let sidePrefix = $"provenance-node::{side}::"

            let tabs =
                Motion.queryAll Browser.Dom.document.body ($"[data-provenance-grouping-value='{identity}']")

            for tab in tabs do
                let card = Motion.closest tab "[data-provenance-group-node]"

                if
                    not (isNull card)
                    && (card.getAttribute "data-provenance-group-node").StartsWith sidePrefix
                then
                    Motion.flash tab

                    if not pulsedCard then
                        Motion.pulse (unbox card)
                        pulsedCard <- true
        )
        |> ignore

    let private tryResolvePropertyValueDrag context (drag: PropertyValueDrag) =
        match drag.DraftId with
        | Some draftId ->
            State.Drafts.tryFind draftId (context.GetUiState())
            |> Option.map (fun draft -> {
                Key = {
                    Kind = draft.OwnerKind
                    Header = draft.Category
                }
                // A draft under an existing category reuses that kind-bearing
                // entry, so it must carry the entry's established kind to
                // conflict-match its assignments (intent §1, §3).
                PropertyKind = Model.establishedPropertyKind draft.OwnerKind draft.Category context.Session
                Value = draft.Value
                Unit = draft.Unit
                ContainerReferenceValueId = None
                ReferenceSlotId = None
                CopiedFromAssignmentId = None
            })
        | None ->
            drag.DefinitionId
            |> Option.bind (fun definitionId ->
                context.Session.Values
                |> Map.tryFind definitionId
                |> Option.bind (fun definition ->
                    if definition.Value = drag.Source.Value && definition.Unit = drag.Source.Unit then
                        Some drag.Source
                    else
                        None
                )
            )

    let private propertyIdForDrag context (drag: PropertyValueDrag) source =
        match drag.DefinitionId with
        | Some definitionId -> context.Session.Values |> Map.tryFind definitionId |> Option.map _.PropertyId
        | None ->
            context.Session.Properties
            |> Map.tryPick (fun id definition ->
                if definition.Category = source.Key.Header then
                    Some id
                else
                    None
            )

    let private publishAssignmentResult context draftId result =
        context.Publish result

        match result, draftId with
        | Ok _, Some id -> context.GetUiState() |> State.Drafts.remove id |> context.SetUiState
        | _ -> ()

    let private applyPropertyValueToGroups
        context
        (drag: PropertyValueDrag)
        (source: ValueAssignmentSource)
        (targetGroups: DisplayGroup list)
        (pulseTarget: (ProvenanceSide * string) option)
        =
        let uiState = context.GetUiState()

        let propertyId = propertyIdForDrag context drag source |> Option.defaultValue ""
        let identified = source.CopiedFromAssignmentId

        let planResult =
            match source.Key.Kind with
            | AnnotationOwnerKind.Node ->
                ValueAssignment.planNodeValueDropToGroups source propertyId identified targetGroups context.Session
            | AnnotationOwnerKind.Process ->
                let linkIds =
                    targetGroups
                    |> List.collect (fun g -> g.ProcessLinkIds |> Set.toList)
                    |> Set.ofList

                let annotations = targetGroups |> List.collect _.Annotations

                ValueAssignment.planProcessValueDropToLinks
                    source
                    propertyId
                    identified
                    linkIds
                    annotations
                    context.Session

        match planResult with
        | Ok batch ->
            let affectedValueCount =
                batch.Overwrites |> List.sumBy (fun w -> w.ExistingAssignmentIds.Count)

            let targetCount target =
                match target with
                | NodeTargets ids -> ids.Count
                | ProcessTargets ids -> ids.Count

            let affectedSideCount =
                targetGroups |> List.map _.Side |> List.distinct |> List.length

            let affectedEntityCount =
                [
                    yield! batch.Adds |> List.map _.Target
                    yield! batch.Overwrites |> List.map _.Target
                ]
                |> List.sumBy targetCount

            let pendingBatch = {
                Batch = batch
                AffectedSideCount = affectedSideCount
                AffectedValueCount = affectedValueCount
                AffectedGroupCount = targetGroups.Length
                AffectedEntityCount = affectedEntityCount
                DraftId = drag.DraftId
                Source = Some source
            }

            if batch.Overwrites.IsEmpty && targetGroups.Length <= 1 then
                if not batch.Adds.IsEmpty then
                    let result =
                        batch.Adds
                        |> List.map (fun request ->
                            fun current -> EditorActions.requestEffectWithSource (Some source) current request
                        )
                        |> fun operations -> Commands.atomic operations context.Session
                        |> Result.map (fun effect -> Session.commit effect context.Session)

                    publishAssignmentResult context drag.DraftId result

                    match result, pulseTarget with
                    | Ok _, Some(side, groupId) -> pulseDropTarget side groupId source
                    | _ -> ()
            else
                State.AssignmentBatch.set pendingBatch uiState |> context.SetUiState
        | Error error ->
            context.SetUiState {
                uiState with
                    Error = Some(SessionErrors.text error)
            }

    let private routePropertyValueDrop context side groupId (drag: PropertyValueDrag) =
        match tryResolvePropertyValueDrag context drag with
        | Some source ->
            let targetGroups =
                ValueAssignment.selectedTargetGroupsForDrop
                    context.Layer.Id
                    side
                    groupId
                    (context.GetUiState()).SelectedInputs
                    (context.GetUiState()).SelectedOutputs
                    context.Lookups.FindGroup

            applyPropertyValueToGroups context drag source targetGroups (Some(side, groupId))
        | _ -> ()

    let private routeCatalogValueDrop context side groupId scheme durableId =
        match context.ReferenceCatalog |> Map.tryFind (scheme, durableId) with
        | Some entry ->
            let groups =
                ValueAssignment.selectedTargetGroupsForDrop
                    context.Layer.Id
                    side
                    groupId
                    (context.GetUiState()).SelectedInputs
                    (context.GetUiState()).SelectedOutputs
                    context.Lookups.FindGroup

            let result =
                match entry.AssignmentKind with
                | AnnotationOwnerKind.Node ->
                    let nodeIds =
                        groups
                        |> List.collect (fun group -> group.CanonicalNodeIds |> Set.toList)
                        |> Set.ofList

                    Commands.assignCatalogNodeValue
                        nodeIds
                        context.ReferenceCatalog
                        entry
                        Commands.NoOverwrite
                        context.Session
                | AnnotationOwnerKind.Process ->
                    let linkIds =
                        groups
                        |> List.collect (fun group -> group.ProcessLinkIds |> Set.toList)
                        |> Set.ofList

                    Commands.assignCatalogProcessValue linkIds context.ReferenceCatalog entry context.Session

            result
            |> Result.map (fun effect -> Session.commit effect context.Session)
            |> context.Publish
        | _ -> ()

    /// A value dropped on a connector. Only a process-kind value can land on an
    /// edge: an edge *is* the link a process assignment covers, whereas a node
    /// value has no node to own it there. A node-kind drop is refused with
    /// visible feedback rather than silently ignored (design §14.1, item 12).
    let private routePropertyValueDropOnConnector context connectorId (drag: PropertyValueDrag) =
        match tryResolvePropertyValueDrag context drag with
        | None -> ()
        | Some source ->
            match context.Connectors |> List.tryFind (fun connector -> connector.Id = connectorId) with
            | None -> ()
            | Some connector ->
                match source.Key.Kind with
                | AnnotationOwnerKind.Node ->
                    context.SetUiState {
                        context.GetUiState() with
                            Error =
                                Some
                                    $"'{source.Key.Header.Name}' is a node annotation, so it cannot be assigned to a connection. Drop it on an entity instead."
                    }
                | AnnotationOwnerKind.Process ->
                    let propertyId = propertyIdForDrag context drag source |> Option.defaultValue ""

                    // Pooled connectors carry every link they represent, so one
                    // drop covers them all and the command partitions per process.
                    ValueAssignment.planProcessValueDropToLinks
                        source
                        propertyId
                        source.CopiedFromAssignmentId
                        connector.LinkIds
                        connector.Annotations
                        context.Session
                    |> function
                        | Ok batch ->
                            // One connector is one target, so this never needs
                            // the multi-target confirmation the group-card path
                            // raises; an overwrite still does.
                            if batch.Overwrites.IsEmpty then
                                batch.Adds
                                |> List.map (fun request ->
                                    fun current -> EditorActions.requestEffectWithSource (Some source) current request
                                )
                                |> fun operations -> Commands.atomic operations context.Session
                                |> Result.map (fun effect -> Session.commit effect context.Session)
                                |> publishAssignmentResult context drag.DraftId
                            else
                                State.AssignmentBatch.set
                                    {
                                        Batch = batch
                                        AffectedSideCount = 1
                                        AffectedValueCount =
                                            batch.Overwrites |> List.sumBy (fun w -> w.ExistingAssignmentIds.Count)
                                        AffectedGroupCount = 1
                                        AffectedEntityCount = connector.LinkIds.Count
                                        DraftId = drag.DraftId
                                        Source = Some source
                                    }
                                    (context.GetUiState())
                                |> context.SetUiState
                        | Error error ->
                            context.SetUiState {
                                context.GetUiState() with
                                    Error = Some(SessionErrors.text error)
                            }

    let private routeCatalogValueDropOnConnector context connectorId scheme durableId =
        match context.ReferenceCatalog |> Map.tryFind (scheme, durableId) with
        | Some entry when entry.AssignmentKind = AnnotationOwnerKind.Process ->
            match context.Connectors |> List.tryFind (fun connector -> connector.Id = connectorId) with
            | Some connector ->
                Commands.assignCatalogProcessValue connector.LinkIds context.ReferenceCatalog entry context.Session
                |> Result.map (fun effect -> Session.commit effect context.Session)
                |> context.Publish
            | None -> ()
        | Some entry ->
            context.SetUiState {
                context.GetUiState() with
                    Error =
                        Some
                            $"'{entry.Category.Name}' is a node annotation, so it cannot be assigned to a connection. Drop it on an entity instead."
            }
        | None -> ()

    let private routeCatalogValueDropOnMember context side memberNodeId scheme durableId =
        match context.ReferenceCatalog |> Map.tryFind (scheme, durableId) with
        | Some entry ->
            let result =
                match entry.AssignmentKind with
                | AnnotationOwnerKind.Node ->
                    Commands.assignCatalogNodeValue
                        (Set.singleton memberNodeId)
                        context.ReferenceCatalog
                        entry
                        Commands.NoOverwrite
                        context.Session
                | AnnotationOwnerKind.Process ->
                    Commands.assignCatalogProcessValue
                        (Projection.processLinkIdsForNodes
                            context.Layer
                            side
                            (Set.singleton memberNodeId)
                            context.Session)
                        context.ReferenceCatalog
                        entry
                        context.Session

            result
            |> Result.map (fun effect -> Session.commit effect context.Session)
            |> context.Publish
        | None -> ()

    let private routePropertyValueDropOnProcessOnly context processId linkId (drag: PropertyValueDrag) =
        match tryResolvePropertyValueDrag context drag with
        | Some source when source.Key.Kind = AnnotationOwnerKind.Process ->
            match
                context.Projection.ProcessOnlyEntries
                |> List.tryFind (fun entry -> entry.StructuralProcessId = processId && entry.LinkId = linkId)
            with
            | Some entry ->
                let propertyId = propertyIdForDrag context drag source |> Option.defaultValue ""

                ValueAssignment.planProcessValueDropToLinks
                    source
                    propertyId
                    source.CopiedFromAssignmentId
                    (Set.singleton linkId)
                    entry.Annotations
                    context.Session
                |> function
                    | Error error ->
                        context.SetUiState {
                            context.GetUiState() with
                                Error = Some(SessionErrors.text error)
                        }
                    | Ok batch when batch.Overwrites.IsEmpty ->
                        batch.Adds
                        |> List.map (fun request ->
                            fun current -> EditorActions.requestEffectWithSource (Some source) current request
                        )
                        |> fun operations -> Commands.atomic operations context.Session
                        |> Result.map (fun effect -> Session.commit effect context.Session)
                        |> publishAssignmentResult context drag.DraftId
                    | Ok batch ->
                        State.AssignmentBatch.set
                            {
                                Batch = batch
                                DraftId = drag.DraftId
                                Source = Some source
                                AffectedSideCount = 1
                                AffectedValueCount =
                                    batch.Overwrites
                                    |> List.sumBy (fun warning -> warning.ExistingAssignmentIds.Count)
                                AffectedGroupCount = 1
                                AffectedEntityCount = 1
                            }
                            (context.GetUiState())
                        |> context.SetUiState
            | None -> ()
        | Some _ ->
            context.SetUiState {
                context.GetUiState() with
                    Error = Some "Only process annotations can be assigned to an endpointless process."
            }
        | None -> ()

    let private routeCatalogValueDropOnProcessOnly context processId linkId scheme durableId =
        match context.ReferenceCatalog |> Map.tryFind (scheme, durableId) with
        | Some entry when entry.AssignmentKind = AnnotationOwnerKind.Process ->
            match
                context.Projection.ProcessOnlyEntries
                |> List.exists (fun current -> current.StructuralProcessId = processId && current.LinkId = linkId)
            with
            | true ->
                Commands.assignCatalogProcessValue (Set.singleton linkId) context.ReferenceCatalog entry context.Session
                |> Result.map (fun effect -> Session.commit effect context.Session)
                |> context.Publish
            | false -> ()
        | Some _ ->
            context.SetUiState {
                context.GetUiState() with
                    Error = Some "Only process annotations can be assigned to an endpointless process."
            }
        | None -> ()

    let applyPropertyValueToSelection context (drag: PropertyValueDrag) =
        match tryResolvePropertyValueDrag context drag with
        | Some source ->
            let uiState = context.GetUiState()
            let layerId = context.Layer.Id

            let groupsFor side (selected: Set<ProvenanceLayerId * string>) =
                selected
                |> Set.toList
                |> List.choose (fun (currentLayerId, id) ->
                    if currentLayerId = layerId then
                        context.Lookups.FindGroup side id
                    else
                        None
                )

            let inputGroups = groupsFor ProvenanceSide.Input uiState.SelectedInputs
            let outputGroups = groupsFor ProvenanceSide.Output uiState.SelectedOutputs

            match inputGroups.IsEmpty, outputGroups.IsEmpty with
            | false, false ->
                context.SetUiState {
                    uiState with
                        Error = Some "Select groups on one side at a time before assigning a value."
                }
            | false, true -> applyPropertyValueToGroups context drag source inputGroups None
            | true, false -> applyPropertyValueToGroups context drag source outputGroups None
            | true, true -> ()
        | None -> ()

    let applyCatalogValueToSelection context (entry: ReferenceCatalogEntry) =
        let uiState = context.GetUiState()
        let layerId = context.Layer.Id

        let groupsFor side (selected: Set<ProvenanceLayerId * string>) =
            selected
            |> Set.toList
            |> List.choose (fun (currentLayerId, id) ->
                if currentLayerId = layerId then
                    context.Lookups.FindGroup side id
                else
                    None
            )

        let inputGroups = groupsFor ProvenanceSide.Input uiState.SelectedInputs
        let outputGroups = groupsFor ProvenanceSide.Output uiState.SelectedOutputs

        match inputGroups.IsEmpty, outputGroups.IsEmpty with
        | false, false ->
            context.SetUiState {
                uiState with
                    Error = Some "Select groups on one side at a time before assigning a value."
            }
        | true, true -> ()
        | _ ->
            let groups = if inputGroups.IsEmpty then outputGroups else inputGroups

            let result =
                match entry.AssignmentKind with
                | AnnotationOwnerKind.Node ->
                    let nodeIds =
                        groups
                        |> List.collect (fun group -> group.CanonicalNodeIds |> Set.toList)
                        |> Set.ofList

                    Commands.assignCatalogNodeValue
                        nodeIds
                        context.ReferenceCatalog
                        entry
                        Commands.NoOverwrite
                        context.Session
                | AnnotationOwnerKind.Process ->
                    let linkIds =
                        groups
                        |> List.collect (fun group -> group.ProcessLinkIds |> Set.toList)
                        |> Set.ofList

                    Commands.assignCatalogProcessValue linkIds context.ReferenceCatalog entry context.Session

            result
            |> Result.map (fun effect -> Session.commit effect context.Session)
            |> context.Publish

    let private routeGroupConnection context inputGroupId outputGroupId =
        match
            context.Lookups.FindGroup ProvenanceSide.Input inputGroupId,
            context.Lookups.FindGroup ProvenanceSide.Output outputGroupId
        with
        | Some inputGroup, Some outputGroup ->
            if inputGroup.CanonicalNodeIds.Count = 1 && outputGroup.CanonicalNodeIds.Count = 1 then
                match EditorActions.orderedMemberPairs context.Layer inputGroup outputGroup with
                | Some pairs -> context.ConnectNodePairs pairs
                | None -> ()
            else
                State.MemberResolution.request
                    {
                        LayerId = context.Layer.Id
                        InputGroupId = inputGroup.Id
                        OutputGroupId = outputGroup.Id
                        InputMemberCount = inputGroup.CanonicalNodeIds.Count
                        OutputMemberCount = outputGroup.CanonicalNodeIds.Count
                    }
                    (context.GetUiState())
                |> context.SetUiState
        | _ -> ()

    let private routeMemberToGroupConnection context inputGroupId outputGroupId memberNodeId memberSide =
        match
            context.Lookups.FindGroup ProvenanceSide.Input inputGroupId,
            context.Lookups.FindGroup ProvenanceSide.Output outputGroupId
        with
        | Some inputGroup, Some outputGroup ->
            let pairs =
                match memberSide with
                | ProvenanceSide.Input ->
                    outputGroup.CanonicalNodeIds
                    |> Set.toList
                    |> List.map (fun outputNodeId -> memberNodeId, outputNodeId)
                | ProvenanceSide.Output ->
                    inputGroup.CanonicalNodeIds
                    |> Set.toList
                    |> List.map (fun inputNodeId -> inputNodeId, memberNodeId)

            context.ConnectNodePairs pairs
        | _ -> ()

    let routeConnectionHandle context source target =
        match ConnectionRouting.action source target with
        | Some(ConnectionRouting.ConnectionAction.ConnectGroups(inputGroupId, outputGroupId)) ->
            routeGroupConnection context inputGroupId outputGroupId
        | Some(ConnectionRouting.ConnectionAction.ConnectMembers(_, _, inputNodeId, outputNodeId)) ->
            context.ConnectNodePairs [ inputNodeId, outputNodeId ]
        | Some(ConnectionRouting.ConnectionAction.ConnectMemberToGroup(inputGroupId,
                                                                       outputGroupId,
                                                                       memberNodeId,
                                                                       memberSide)) ->
            routeMemberToGroupConnection context inputGroupId outputGroupId memberNodeId memberSide
        | None -> ()

    /// Item 23's "single member drop treated as a group of one": the member row
    /// narrows its own card to exactly the node under the pointer, so the whole
    /// existing planning path applies unchanged.
    let private routePropertyValueDropOnMember context side groupId memberNodeId (drag: PropertyValueDrag) =
        match tryResolvePropertyValueDrag context drag, context.Lookups.FindGroup side groupId with
        | Some source, Some group ->

            let singleMember = {
                group with
                    CanonicalNodeIds = Set.singleton memberNodeId
                    EndpointKeys = group.EndpointKeys |> Set.filter (fun key -> key.NodeId = memberNodeId)
                    AnnotationsByNodeId =
                        group.AnnotationsByNodeId |> Map.filter (fun nodeId _ -> nodeId = memberNodeId)
                    ProcessLinkIds =
                        Projection.processLinkIdsForNodes
                            context.Layer
                            side
                            (Set.singleton memberNodeId)
                            context.Session
            }

            applyPropertyValueToGroups context drag source [ singleMember ] (Some(side, groupId))
        | _ -> ()

    let private routeExistingValueAndPropertyDrags
        context
        dragPayload
        groupDrop
        propertyDrop
        connectorDrop
        memberDrop
        processOnlyDrop
        =
        match dragPayload, groupDrop, propertyDrop, processOnlyDrop with
        | Some(DragDrop.Payload.FolderCatalogValue(_, scheme, durableId)), _, Some targetSide, _ ->
            match context.ReferenceCatalog |> Map.tryFind (scheme, durableId) with
            | Some entry ->
                let property = {
                    Kind = entry.AssignmentKind
                    Header = entry.Category
                }

                context.GetUiState()
                |> State.PropertyPlacement.place context.Layer.Id targetSide property
                |> context.SetUiState
            | None -> ()
        | Some(DragDrop.Payload.CatalogValue(_, scheme, durableId)), _, _, Some(processId, linkId) ->
            routeCatalogValueDropOnProcessOnly context processId linkId scheme durableId
        | Some(DragDrop.Payload.PropertyValue drag), _, _, Some(processId, linkId) ->
            routePropertyValueDropOnProcessOnly context processId linkId drag
        | Some(DragDrop.Payload.CatalogValue(_, scheme, durableId)), _, _, _ when Option.isSome connectorDrop ->
            routeCatalogValueDropOnConnector context connectorDrop.Value scheme durableId
        | Some(DragDrop.Payload.CatalogValue(_, scheme, durableId)), _, _, _ when Option.isSome memberDrop ->
            let side, _, memberNodeId = memberDrop.Value
            routeCatalogValueDropOnMember context side memberNodeId scheme durableId
        | Some(DragDrop.Payload.CatalogValue(_, scheme, durableId)), Some(side, groupId), _, _ ->
            routeCatalogValueDrop context side groupId scheme durableId
        | Some(DragDrop.Payload.PropertyValue propertyValueId), _, _, _ when Option.isSome connectorDrop ->
            // An edge wins over whatever droppable happens to sit under it: the
            // connector is drawn on top and is what the user aimed at.
            routePropertyValueDropOnConnector context connectorDrop.Value propertyValueId
        | Some(DragDrop.Payload.PropertyValue propertyValueId), _, _, _ when Option.isSome memberDrop ->
            let side, groupId, memberNodeId = memberDrop.Value
            routePropertyValueDropOnMember context side groupId memberNodeId propertyValueId
        | Some(DragDrop.Payload.PropertyValue propertyValueId), Some(side, groupId), _, _ ->
            routePropertyValueDrop context side groupId propertyValueId
        | Some(DragDrop.Payload.FolderPropertyHeader(sourceSide, headerId)), _, Some targetSide, _ ->
            match context.Lookups.FindProperty headerId with
            | Some property when
                sourceSide = targetSide
                || PropertyRails.canSwitchHeader property context.Projection
                ->
                context.GetUiState()
                |> State.PropertyPlacement.place context.Layer.Id targetSide property
                |> context.SetUiState
            | _ -> ()
        | Some(DragDrop.Payload.PropertyHeader(sourceSide, headerId)), _, Some targetSide, _ when
            sourceSide <> targetSide
            ->
            match context.Lookups.FindProperty headerId with
            | Some property when PropertyRails.canSwitchHeader property context.Projection ->
                State.GroupingAssignments.move
                    context.Layer.Id
                    (context.Layer.Id, sourceSide)
                    (context.Layer.Id, targetSide)
                    targetSide
                    property
                    (context.GetUiState())
                |> context.SetUiState
            | _ -> ()
        | _ -> ()

    let handleEnd context (event: DndKit.IDndKitEvent) =
        let dragPayload = DragDrop.tryDragId (string event.active.id)

        // Rails and process-only entries stay dnd-kit droppables, so those two
        // still resolve through `event.over`. Cards and connection handles are
        // not droppables and resolve by hit-testing the pointer instead.
        let propertyDrop, processOnlyDrop =
            if isNull event.over then
                None, None
            else
                DragDrop.tryPropertyDropId (string event.over.id), DragDrop.tryProcessOnlyDropId (string event.over.id)

        match dragPayload with
        | Some(DragDrop.Payload.ConnectionHandle source) ->
            DropHitTesting.connectionTarget source event
            |> Option.iter (routeConnectionHandle context source)
        | _ ->
            let connectorDrop, memberDrop, groupDrop =
                match dragPayload with
                | Some(DragDrop.Payload.PropertyValue _)
                | Some(DragDrop.Payload.CatalogValue _) ->
                    DropHitTesting.connectorEdgeAt event,
                    DropHitTesting.memberDropAt event,
                    DropHitTesting.groupDropAt event
                | _ -> None, None, None

            routeExistingValueAndPropertyDrags
                context
                dragPayload
                groupDrop
                propertyDrop
                connectorDrop
                memberDrop
                processOnlyDrop

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
open Swate.Components.Page.ProvenanceGrouping.ProjectionTypes
open Swate.Components.Page.ProvenanceGrouping.Types

type EditorLookups = {
    FindGroup: ProvenanceSide -> string -> DisplayGroup option
    FindProperty: string -> AnnotationHeaderKey option
    FindValueDefinition: PropertyValueDefinitionId -> PropertyValueDefinition option
    SourceForValue: PropertyValueDefinitionId -> PropertyValueDefinition -> ValueAssignmentSource
}

type DragContext = {
    Session: ProvenanceSession
    Layer: ProvenanceLayer
    Projection: CachedLayerProjection
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

        let sourceForValue (valueId: PropertyValueDefinitionId) (definition: PropertyValueDefinition) =
            let property = session.Properties |> Map.tryFind definition.PropertyId

            let category =
                property
                |> Option.map _.Category
                |> Option.defaultValue {
                    Name = ""
                    TermSource = None
                    TermAccession = None
                }

            let annotation =
                projection.Groups
                |> List.tryPick (fun group ->
                    group.Annotations
                    |> List.tryFind (fun a ->
                        match a.Backing with
                        | NodeAssignmentBacking(identity, _, _) -> identity.ValueId = valueId
                        | ProcessAssignmentBacking(identity, _, _, _, _) -> identity.ValueId = valueId
                    )
                )
                |> Option.orElseWith (fun () ->
                    projection.Connectors
                    |> List.tryPick (fun conn ->
                        conn.Annotations
                        |> List.tryFind (fun a ->
                            match a.Backing with
                            | NodeAssignmentBacking(identity, _, _) -> identity.ValueId = valueId
                            | ProcessAssignmentBacking(identity, _, _, _, _) -> identity.ValueId = valueId
                        )
                    )
                )

            match annotation with
            | Some a ->
                let key = PropertyRails.headerKeyOf a

                match a.Backing with
                | NodeAssignmentBacking(identity, _, _) -> {
                    Key = key
                    PropertyKind = identity.PropertyKind
                    Value = definition.Value
                    Unit = definition.Unit
                    ContainerReferenceValueId = None
                    ReferenceSlotId = None
                    CopiedFromAssignmentId = Some identity.AssignmentId
                  }
                | ProcessAssignmentBacking(identity, _, _, containerRef, slotRef) -> {
                    Key = key
                    PropertyKind = identity.PropertyKind
                    Value = definition.Value
                    Unit = definition.Unit
                    ContainerReferenceValueId = containerRef
                    ReferenceSlotId = slotRef
                    CopiedFromAssignmentId = Some identity.AssignmentId
                  }
            | None -> {
                Key = {
                    Kind = AnnotationOwnerKind.Node
                    Header = category
                }
                PropertyKind = AssignmentPropertyKind.Generic
                Value = definition.Value
                Unit = definition.Unit
                ContainerReferenceValueId = None
                ReferenceSlotId = None
                CopiedFromAssignmentId = None
              }

        {
            FindGroup = findGroup
            FindProperty = findProperty
            FindValueDefinition = findValueDefinition
            SourceForValue = sourceForValue
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
        | AmbiguousPooledEdit _ -> "Multiple links cover this annotation. Edit the individual links instead."
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

        CanonicalSession.addLayer request.Name request.SelectedNodes session |> publish

    let createEndpoint session publish layerId side kind header name layerOrderPosition =
        CanonicalSession.addEndpoint layerId side kind header name layerOrderPosition session
        |> publish

    let applyRequest (session: ProvenanceSession) (request: ValueAssignmentRequest) =
        let content: Commands.NodeValueContent = {
            Category = request.Category
            Value = request.Value
            Unit = request.Unit
        }

        match request.Target with
        | NodeTargets nodeIds ->
            let draft: Commands.NodeAssignmentDraft = {
                Content = content
                OwnerKind = request.OwnerKind
                PropertyKind = request.PropertyKind
            }

            Commands.assignNodeValue nodeIds draft Commands.NoOverwrite session
            |> Result.map (fun effect -> CanonicalSession.commit effect session)
        | ProcessTargets linkIds ->
            let draft: Commands.ProcessAssignmentDraft = {
                Content = content
                OwnerKind = request.OwnerKind
                PropertyKind = request.PropertyKind
                ContainerReferenceValueId = None
                ReferenceSlotId = None
                Lineage = AssignmentLineage.Created
            }

            Commands.assignProcessValue linkIds draft session
            |> Result.map (fun effect -> CanonicalSession.commit effect session)

    let private applyOverwrite (session: ProvenanceSession) (warning: ValueAssignmentWarning) =
        let removeResult =
            warning.ExistingAssignmentIds
            |> Set.fold
                (fun result assignmentId ->
                    result
                    |> Result.bind (fun current ->
                        let nodeOwner =
                            current.Nodes
                            |> Map.tryPick (fun nodeId node ->
                                if node.Assignments |> Map.containsKey assignmentId then
                                    Some nodeId
                                else
                                    None
                            )

                        match nodeOwner with
                        | Some nodeId ->
                            Commands.removeNodeAssignment nodeId assignmentId current
                            |> Result.map (fun effect -> CanonicalSession.commit effect current)
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
                                Commands.removeProcessAssignmentLinks
                                    processId
                                    assignmentId
                                    assignment.CoveredLinkIds
                                    current
                                |> Result.map (fun effect -> CanonicalSession.commit effect current)
                            | None -> Ok current
                    )
                )
                (Ok session)

        removeResult
        |> Result.bind (fun afterRemoval ->
            let request: ValueAssignmentRequest = {
                Target = warning.Target
                OwnerKind =
                    match warning.Target with
                    | NodeTargets _ -> AnnotationOwnerKind.Node
                    | ProcessTargets _ -> AnnotationOwnerKind.Process
                PropertyKind = AssignmentPropertyKind.Generic
                Category = warning.Header
                Value = warning.Value
                Unit = warning.Unit
            }

            applyRequest afterRemoval request
        )

    let applyAssignmentBatch session publish (batch: PropertyAssignmentBatch) =
        batch.Overwrites
        |> List.fold
            (fun result warning -> result |> Result.bind (fun current -> applyOverwrite current warning))
            (Ok session)
        |> Result.bind (fun afterOverwrites ->
            batch.Adds
            |> List.fold
                (fun result request -> result |> Result.bind (fun current -> applyRequest current request))
                (Ok afterOverwrites)
        )
        |> publish

    let connectNodePairs session (layer: ProvenanceLayer) publish pairs =
        CanonicalSession.connectNodes layer.Id (pairs |> List.distinct) session
        |> publish

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

    [<Emit("$0.closest($1)")>]
    let private closest (_element: Browser.Types.HTMLElement) (_selector: string) : Browser.Types.HTMLElement = jsNative

    let private attribute name (element: Browser.Types.HTMLElement) =
        let value = element.getAttribute name
        if isNull value then None else Some value

    let private closestAttribute selector attributeName (element: Browser.Types.HTMLElement) =
        let node = closest element selector
        if isNull node then None else attribute attributeName node

    let private endpoint source (event: DndKit.IDndKitEvent) =
        HandleMeasure.tryViewportCenter source
        |> Option.map (fun start -> {
            X = start.X + event.delta.x
            Y = start.Y + event.delta.y
        })

    let private targetHandleAt point source =
        elementsFromPoint point.X point.Y
        |> Array.tryPick (fun element ->
            closestAttribute "[data-provenance-connection-drop-id]" "data-provenance-connection-drop-id" element
            |> Option.bind DragDrop.tryConnectionDropId
            |> Option.bind (fun target -> if target = source then None else Some target)
        )

    let connectionTarget source event =
        endpoint source event |> Option.bind (fun point -> targetHandleAt point source)

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

    let handleMove (liveDragStore: LiveDrag.Store) (event: DndKit.IDndKitMoveEvent) =
        match liveDragStore.Current with
        | Some live ->
            LiveDrag.moveTo
                {
                    X = live.Start.X + event.delta.x
                    Y = live.Start.Y + event.delta.y
                }
                liveDragStore
        | None -> ()

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

    let private applyPropertyValueToGroups
        context
        (source: ValueAssignmentSource)
        (targetGroups: DisplayGroup list)
        (pulseTarget: (ProvenanceSide * string) option)
        =
        let uiState = context.GetUiState()

        let propertyId =
            context.Session.Properties
            |> Map.tryPick (fun id def -> if def.Category = source.Key.Header then Some id else None)
            |> Option.defaultValue ""

        let planResult =
            match source.Key.Kind with
            | AnnotationOwnerKind.Node ->
                ValueAssignment.planNodeValueDropToGroups source propertyId None targetGroups context.Session
            | AnnotationOwnerKind.Process ->
                let linkIds =
                    targetGroups
                    |> List.collect (fun g -> g.ProcessLinkIds |> Set.toList)
                    |> Set.ofList

                let annotations = targetGroups |> List.collect _.Annotations

                ValueAssignment.planProcessValueDropToLinks source propertyId None linkIds annotations context.Session

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
            }

            if batch.Overwrites.IsEmpty && targetGroups.Length <= 1 then
                if not batch.Adds.IsEmpty then
                    let result =
                        batch.Adds
                        |> List.fold
                            (fun result request ->
                                result
                                |> Result.bind (fun current -> EditorActions.applyRequest current request)
                            )
                            (Ok context.Session)

                    context.Publish result

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

    let private routePropertyValueDrop context side groupId propertyValueId =
        match context.Lookups.FindValueDefinition propertyValueId with
        | Some definition ->
            let source = context.Lookups.SourceForValue propertyValueId definition

            let targetGroups =
                ValueAssignment.selectedTargetGroupsForDrop
                    context.Layer.Id
                    side
                    groupId
                    (context.GetUiState()).SelectedInputs
                    (context.GetUiState()).SelectedOutputs
                    context.Lookups.FindGroup

            applyPropertyValueToGroups context source targetGroups (Some(side, groupId))
        | _ -> ()

    let applyPropertyValueToSelection context propertyValueId =
        match context.Lookups.FindValueDefinition propertyValueId with
        | Some definition ->
            let source = context.Lookups.SourceForValue propertyValueId definition
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

            let targetGroups = [
                yield! groupsFor ProvenanceSide.Input uiState.SelectedInputs
                yield! groupsFor ProvenanceSide.Output uiState.SelectedOutputs
            ]

            if not targetGroups.IsEmpty then
                applyPropertyValueToGroups context source targetGroups None
        | None -> ()

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

    let private routeExistingValueAndPropertyDrags context dragPayload groupDrop propertyDrop =
        match dragPayload, groupDrop, propertyDrop with
        | Some(DragDrop.Payload.PropertyValue propertyValueId), Some(side, groupId), _ ->
            routePropertyValueDrop context side groupId propertyValueId
        | Some(DragDrop.Payload.FolderPropertyHeader(sourceSide, headerId)), _, Some targetSide ->
            match context.Lookups.FindProperty headerId with
            | Some property when
                sourceSide = targetSide
                || PropertyRails.canSwitchHeader property context.Projection
                ->
                context.GetUiState()
                |> State.PropertyPlacement.place context.Layer.Id targetSide property
                |> context.SetUiState
            | _ -> ()
        | Some(DragDrop.Payload.PropertyHeader(sourceSide, headerId)), _, Some targetSide when sourceSide <> targetSide ->
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

        let groupDrop, propertyDrop, connectionDrop =
            if isNull event.over then
                None, None, None
            else
                DragDrop.tryDropId (string event.over.id),
                DragDrop.tryPropertyDropId (string event.over.id),
                DragDrop.tryConnectionDropId (string event.over.id)

        match dragPayload, connectionDrop with
        | Some(DragDrop.Payload.ConnectionHandle source), Some target ->
            let resolvedTarget =
                if target = source then
                    DropHitTesting.connectionTarget source event
                else
                    Some target

            resolvedTarget |> Option.iter (routeConnectionHandle context source)
        | Some(DragDrop.Payload.ConnectionHandle source), None ->
            DropHitTesting.connectionTarget source event
            |> Option.iter (routeConnectionHandle context source)
        | _ -> routeExistingValueAndPropertyDrags context dragPayload groupDrop propertyDrop

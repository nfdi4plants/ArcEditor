module Swate.Components.Page.ProvenanceGrouping.State

open Swate.Components.Page.ProvenanceGrouping.Identifiers
open Swate.Components.Page.ProvenanceGrouping.Values
open Swate.Components.Page.ProvenanceGrouping.Domain
open Swate.Components.Page.ProvenanceGrouping.ProjectionTypes
open Swate.Components.Page.ProvenanceGrouping.Types

/// Shared key helpers used by the UI state maps and sets.
module Keys =

    /// A UI grouping/colour/rail key is `(owner kind, normalized header)`. There
    /// is no origin source in it: node grouping ignores origin entirely, and a
    /// process value's origin is part of its *value* key, not of the header the
    /// user chooses to group by.
    let groupingKey (kind: AnnotationOwnerKind) (header: ProvenanceTerm) : GroupingKey = {
        Kind = kind
        Header = header
    }

    let propertySlot layerId side (property: GroupingKey) = layerId, side, property

    let draftKey layerId side : LayerSideId = layerId, side

    let selectedGroup layerId groupId = layerId, groupId

/// Stores and clamps the three-panel editor layout ratios.
module PanelLayout =

    let minimumSide = 15
    let minimumMiddle = 30

    let defaultRatios = { Left = 20; Middle = 60; Right = 20 }

    let private clamp left right =
        let left = left |> max minimumSide |> min (100 - minimumMiddle - minimumSide)

        let right = right |> max minimumSide |> min (100 - minimumMiddle - left)

        {
            Left = left
            Middle = 100 - left - right
            Right = right
        }

    /// Clamps raw percentages with the same rules the stored ratios use, so live
    /// drag previews land exactly where the committed state will.
    let clamped left right = clamp left right

    let get layerId state =
        state.PanelRatios |> Map.tryFind layerId |> Option.defaultValue defaultRatios

    let set layerId ratios state = {
        state with
            PanelRatios = state.PanelRatios |> Map.add layerId (clamp ratios.Left ratios.Right)
    }

    let setLeft layerId left state =
        let current = get layerId state
        set layerId { current with Left = left } state

    let setRight layerId right state =
        let current = get layerId state
        set layerId { current with Right = right } state

/// Tracks the in-app prompt and expansion state for mismatched group connections.
module MemberResolution =

    let request (pending: PendingMemberResolution) state = {
        state with
            PendingMemberResolution = Some pending
            Error = None
    }

    let clearPending state = {
        state with
            PendingMemberResolution = None
    }

    let chooseManual (pending: PendingMemberResolution) state = {
        state with
            PendingMemberResolution = None
            // Exactly the two cards that were about to be connected open, so the
            // member handles the user needs next are the ones on screen.
            ExpandedGroups =
                Set.ofList [
                    ProvenanceSide.Input, pending.InputGroupId
                    ProvenanceSide.Output, pending.OutputGroupId
                ]
            Detail = None
            Hint =
                Some
                    "Drag from a member's connection handle to a member or group on the other side (or tap both handles) to connect them individually."
    }

/// One-line follow-up guidance shown after actions that need a next step.
module Hint =

    let clear state = { state with Hint = None }

module PropertyColors =

    let palette = [|
        "#2563eb"
        "#16a34a"
        "#d97706"
        "#dc2626"
        "#7c3aed"
        "#0891b2"
        "#be185d"
    |]

    let empty = {
        ManualPropertyColors = Map.empty
        SourceColors = Map.empty
        SourceColorSetOrder = Map.empty
        NextSourceColorSetOrder = 0
    }

    let private automaticColorForLayer layerIndex = palette.[layerIndex % palette.Length]

    /// The fallback when no source colour applies - including for a catalog-only
    /// reference, which has no source at all. Shared so `ControlsSupport`'s
    /// colour picker cannot drift to its own first-palette guess.
    let defaultColor: ProvenanceColor = "#64748b"

    let colorKey (kind: AnnotationOwnerKind) (header: ProvenanceTerm) : PropertyColorKey = {
        Kind = kind
        Header = header
    }

    /// Design §3.5. Colour is not keyed by component, layer, or viewing shelf:
    /// a manual override wins, else the origin source with the greatest
    /// `SourceColorSetOrder`, else one fixed default. The caller supplies the
    /// item's origin sources; `Projection` derives those (a node annotation's
    /// from its owning node's appearances, a process annotation's from its
    /// owning process's layer).
    let resolveColor
        (settings: PropertyColorSettings)
        (key: PropertyColorKey)
        (originSourceIds: Set<ProvenanceSourceId>)
        : ProvenanceColor =
        match settings.ManualPropertyColors |> Map.tryFind key with
        | Some color -> color
        | None ->
            originSourceIds
            |> Seq.choose (fun sourceId ->
                match
                    settings.SourceColors |> Map.tryFind sourceId, settings.SourceColorSetOrder |> Map.tryFind sourceId
                with
                | Some color, Some setOrder -> Some(setOrder, sourceId, color)
                | _ -> None
            )
            // Ties break on source ID so the result cannot depend on map order.
            |> Seq.sortByDescending (fun (setOrder, sourceId, _) -> setOrder, sourceId)
            |> Seq.tryHead
            |> Option.map (fun (_, _, color) -> color)
            |> Option.defaultValue defaultColor

    let setColor (kind: AnnotationOwnerKind) (header: ProvenanceTerm) color state = {
        state with
            PropertyColors = {
                state.PropertyColors with
                    ManualPropertyColors =
                        state.PropertyColors.ManualPropertyColors
                        |> Map.add (colorKey kind header) color
            }
    }

    let clearColor (kind: AnnotationOwnerKind) (header: ProvenanceTerm) state = {
        state with
            PropertyColors = {
                state.PropertyColors with
                    ManualPropertyColors =
                        state.PropertyColors.ManualPropertyColors |> Map.remove (colorKey kind header)
            }
    }

    let setSourceColor (sourceId: ProvenanceSourceId) color state =
        let setOrder = state.PropertyColors.NextSourceColorSetOrder

        {
            state with
                PropertyColors = {
                    state.PropertyColors with
                        SourceColors = state.PropertyColors.SourceColors |> Map.add sourceId color
                        SourceColorSetOrder = state.PropertyColors.SourceColorSetOrder |> Map.add sourceId setOrder
                        NextSourceColorSetOrder = setOrder + 1
                }
        }

    let clearSourceColor (sourceId: ProvenanceSourceId) state = {
        state with
            PropertyColors = {
                state.PropertyColors with
                    SourceColors = state.PropertyColors.SourceColors |> Map.remove sourceId
                    SourceColorSetOrder = state.PropertyColors.SourceColorSetOrder |> Map.remove sourceId
            }
    }

    /// Every source the session can currently colour. Canonically a source is a
    /// layer's, and nothing else carries one: node assignments store no origin,
    /// process assignments derive theirs from their owning process's layer, and
    /// an unassigned sidebar draft has no source at all. Layer order therefore
    /// fixes the automatic palette assignment.
    let ensureSourceColors (session: ProvenanceSession) (state: UiState) : PropertyColorSettings =
        let orderedSourceIds =
            session.LayerOrder
            |> List.choose (fun layerId -> session.Layers |> Map.tryFind layerId)
            |> List.map (fun layer -> layer.Source.Id)
            |> List.distinct

        let liveSources = orderedSourceIds |> Set.ofList

        let retained =
            state.PropertyColors.SourceColors
            |> Map.filter (fun sourceId _ -> liveSources.Contains sourceId)

        let retainedSetOrder =
            state.PropertyColors.SourceColorSetOrder
            |> Map.filter (fun sourceId _ -> liveSources.Contains sourceId)

        let nextAvailableSetOrder =
            retainedSetOrder
            |> Map.values
            |> Seq.fold (fun next order -> max next (order + 1)) state.PropertyColors.NextSourceColorSetOrder

        let withMissingDefaults, withMissingSetOrder, nextSetOrder =
            orderedSourceIds
            |> List.mapi (fun index sourceId -> sourceId, automaticColorForLayer index)
            |> List.fold
                (fun (colors, setOrders, nextOrder) (sourceId, color) ->
                    let colors =
                        if colors |> Map.containsKey sourceId then
                            colors
                        else
                            colors |> Map.add sourceId color

                    if setOrders |> Map.containsKey sourceId then
                        colors, setOrders, nextOrder
                    else
                        colors, setOrders |> Map.add sourceId nextOrder, nextOrder + 1
                )
                (retained, retainedSetOrder, nextAvailableSetOrder)

        {
            state.PropertyColors with
                SourceColors = withMissingDefaults
                SourceColorSetOrder = withMissingSetOrder
                NextSourceColorSetOrder = nextSetOrder
        }

module Filters =

    let defaultState = {
        SearchText = ""
        PropertySort = PropertySort.ValueCountDesc
        GroupSort = GroupSort.NameAsc
        ValueCountFilter = PropertyValueCountFilter.Any
        OriginFilter = PropertyOriginFilter.AnyOrigin
    }

    let setSearch text state = {
        state with
            Filters = { state.Filters with SearchText = text }
    }

    let setValueCountFilter filter state = {
        state with
            Filters = {
                state.Filters with
                    ValueCountFilter = filter
            }
    }

    let setOriginFilter filter state = {
        state with
            Filters = {
                state.Filters with
                    OriginFilter = filter
            }
    }

    let setPropertySort sort state = {
        state with
            Filters = {
                state.Filters with
                    PropertySort = sort
            }
    }

    let setGroupSort sort state = {
        state with
            Filters = { state.Filters with GroupSort = sort }
    }

/// Creates, finds, and synchronizes side-local state with the current session.
module Sides =

    let empty = { GroupingAssignments = [] }

    /// A canonical side is just `(layer, side)`. The old model stored two
    /// pre-built side IDs on each layer; both sides always exist, so they are
    /// derived here instead.
    let private sideIdsForSession (session: ProvenanceSession) : LayerSideId list =
        session.LayerOrder
        |> List.collect (fun layerId -> [
            layerId, ProvenanceSide.Input
            layerId, ProvenanceSide.Output
        ])

    let get sideId state =
        state.SideStates |> Map.tryFind sideId |> Option.defaultValue empty

    let private retainCurrentSides session state =
        let currentIds = sideIdsForSession session |> Set.ofList

        let retained: Map<LayerSideId, SideViewState> =
            state.SideStates |> Map.filter (fun id _ -> currentIds.Contains id)

        sideIdsForSession session
        |> List.fold
            (fun (map: Map<LayerSideId, SideViewState>) sideId ->
                if map.ContainsKey sideId then
                    map
                else
                    map |> Map.add sideId empty
            )
            retained

    let ensure (session: ProvenanceSession) state =
        let currentLayerIds = session.LayerOrder |> Set.ofList
        let sideStates = retainCurrentSides session state

        let propertyRailPlacements =
            state.PropertyRailPlacements
            |> Map.filter (fun (layerId, _) _ -> currentLayerIds.Contains layerId)

        let propertyRailOrders =
            state.PropertyRailOrders
            |> Map.filter (fun (layerId, _) _ -> currentLayerIds.Contains layerId)

        let panelRatios =
            state.PanelRatios
            |> Map.filter (fun layerId _ -> currentLayerIds.Contains layerId)

        let expandedProperties =
            state.ExpandedProperties
            |> Set.filter (fun (layerId, _, _) -> currentLayerIds.Contains layerId)

        let drafts =
            state.Drafts
            |> Map.filter (fun (layerId, _) _ -> currentLayerIds.Contains layerId)

        let pendingMemberResolution =
            state.PendingMemberResolution
            |> Option.filter (fun pending -> currentLayerIds.Contains pending.LayerId)

        let propertyColors = PropertyColors.ensureSourceColors session state

        if
            sideStates = state.SideStates
            && propertyRailPlacements = state.PropertyRailPlacements
            && propertyRailOrders = state.PropertyRailOrders
            && panelRatios = state.PanelRatios
            && expandedProperties = state.ExpandedProperties
            && drafts = state.Drafts
            && pendingMemberResolution = state.PendingMemberResolution
            && propertyColors = state.PropertyColors
        then
            state
        else
            {
                state with
                    SideStates = sideStates
                    PropertyRailPlacements = propertyRailPlacements
                    PropertyRailOrders = propertyRailOrders
                    PanelRatios = panelRatios
                    ExpandedProperties = expandedProperties
                    Drafts = drafts
                    PendingMemberResolution = pendingMemberResolution
                    PropertyColors = propertyColors
            }

    let update sideId updateSide state =
        let current = get sideId state
        let next = updateSide current

        {
            state with
                SideStates = state.SideStates |> Map.add sideId next
        }

/// Pure success/error state transitions shared by every session-publishing
/// action (publishResult, removeDisplayConnection, undoLast). Extracted so the
/// error path is unit-testable and guaranteed to clear pending prompts exactly
/// like the success path does - previously the error branch only set `Error`,
/// leaving a stale confirmation prompt (e.g. an overwrite batch) mounted after
/// a failed publish.
module Publish =

    let onSuccess (nextSession: ProvenanceSession) state = {
        Sides.ensure nextSession state with
            Error = None
            Hint = None
            PendingAssignmentBatch = None
            PendingMemberResolution = None
            PendingAnnotationEdit = None
    }

    let onError (message: string) state = {
        state with
            Error = Some message
            PendingAssignmentBatch = None
            PendingMemberResolution = None
            PendingAnnotationEdit = None
    }

/// Stores visual rail order separately from filtering and writeback state.
module RailOrder =

    let private key layerId side = layerId, side

    let private distinctHeaders headers = headers |> List.distinct

    let tryGet layerId side state =
        state.PropertyRailOrders |> Map.tryFind (key layerId side)

    let get layerId side state =
        tryGet layerId side state |> Option.defaultValue []

    let apply (order: GroupingKey list) (headers: GroupingKey list) =
        let headerSet = headers |> Set.ofList
        let ordered = order |> List.filter (fun header -> headerSet.Contains header)
        let orderedSet = ordered |> Set.ofList

        let appended =
            headers |> List.filter (fun header -> not (orderedSet.Contains header))

        ordered @ appended

    let set layerId side headers state =
        let next = distinctHeaders headers

        {
            state with
                PropertyRailOrders = state.PropertyRailOrders |> Map.add (key layerId side) next
        }

    let ensure layerId side headers state =
        let next =
            match tryGet layerId side state with
            | Some current -> apply current headers
            | None -> distinctHeaders headers

        if tryGet layerId side state = Some next then
            state
        else
            set layerId side next state

    let reorderVisible layerId side sortedVisibleHeaders state =
        let visibleSet = sortedVisibleHeaders |> Set.ofList

        let retainedHidden =
            get layerId side state
            |> List.filter (fun header -> not (visibleSet.Contains header))

        set layerId side (sortedVisibleHeaders @ retainedHidden) state

    let removeHeader layerId side header state =
        let next = get layerId side state |> List.filter ((<>) header)

        set layerId side next state

    let appendHeader layerId side header state =
        let next =
            get layerId side state
            |> List.filter ((<>) header)
            |> fun headers -> headers @ [ header ]

        set layerId side next state

/// Tracks explicit side drop-zone placement without changing grouping selection.
module PropertyPlacement =

    let place layerId side property state =
        let key = property

        {
            state with
                PropertyRailPlacements = state.PropertyRailPlacements |> Map.add (layerId, key) side
                Error = None
        }
        |> RailOrder.appendHeader layerId side property

/// Consolidates a hidden side's switchable annotations onto the still-visible rail.
module SideVisibility =

    /// Real, permanent move (revealing the side does not send them back): every
    /// annotation on the hidden side that the model lets switch sides is relocated
    /// to the visible rail, following the cards that stay on screen. A solo grouping
    /// the header held on the now-hidden side is dropped, since it can no longer be
    /// seen or managed; group-both assignments are cross-side and left intact.
    let consolidateToVisible
        layerId
        (hiddenSide: ProvenanceSide)
        (hiddenSideId: LayerSideId)
        (canSwitch: GroupingKey -> bool)
        state
        =
        let visibleSide =
            match hiddenSide with
            | ProvenanceSide.Input -> ProvenanceSide.Output
            | ProvenanceSide.Output -> ProvenanceSide.Input

        let hiddenScope = scopeForSide hiddenSide

        let placedHeaders =
            state.PropertyRailPlacements
            |> Map.toList
            |> List.choose (fun ((placementLayerId, key), side) ->
                if placementLayerId = layerId && side = hiddenSide then
                    Some key
                else
                    None
            )

        let soloGroupedHeaders =
            (Sides.get hiddenSideId state).GroupingAssignments
            |> List.filter (fun assignment -> assignment.Scope = hiddenScope)
            |> List.map (fun assignment -> assignment.Key)

        let headers =
            [ yield! placedHeaders; yield! soloGroupedHeaders ]
            |> List.distinct
            |> List.filter canSwitch

        headers
        |> List.fold
            (fun state property ->
                let key = property

                let withoutSolo =
                    Sides.update
                        hiddenSideId
                        (fun current -> {
                            current with
                                GroupingAssignments =
                                    current.GroupingAssignments
                                    |> List.filter (fun assignment ->
                                        not (assignment.Key = key && assignment.Scope = hiddenScope)
                                    )
                        })
                        state

                {
                    withoutSolo with
                        PropertyRailPlacements =
                            withoutSolo.PropertyRailPlacements |> Map.add (layerId, key) visibleSide
                }
                |> RailOrder.removeHeader layerId hiddenSide property
                |> RailOrder.appendHeader layerId visibleSide property
            )
            state

/// Tracks expanded property value panels on the side rails.
module PropertyExpansion =

    let isExpanded layerId side property state =
        state.ExpandedProperties
        |> Set.contains (Keys.propertySlot layerId side property)

    let toggle layerId side property state =
        let slot = Keys.propertySlot layerId side property

        let expanded =
            if state.ExpandedProperties.Contains slot then
                state.ExpandedProperties.Remove slot
            else
                state.ExpandedProperties.Add slot

        {
            state with
                ExpandedProperties = expanded
        }

/// Unassigned sidebar drafts: values that live only in the rail until their
/// first assignment promotes them to a session value definition (intent §3).
/// A draft carries the owner kind chosen at creation, so no scope is asked for
/// after a drop or on reuse.
module Drafts =

    let forSide layerId side state =
        state.Drafts
        |> Map.tryFind (Keys.draftKey layerId side)
        |> Option.defaultValue []

    let private matchesKey (key: GroupingKey) (draft: SidebarDraft) =
        draft.OwnerKind = key.Kind && draft.Category = key.Header

    let forProperty layerId side (key: GroupingKey) state =
        forSide layerId side state |> List.filter (matchesKey key)

    let propertiesForSide layerId side state : GroupingKey list =
        forSide layerId side state
        |> List.map (fun draft -> Keys.groupingKey draft.OwnerKind draft.Category)
        |> List.distinct
        |> List.sortBy (fun key -> key.Header.Name, key.Header.TermAccession)

    let tryFind draftId state =
        state.Drafts
        |> Map.toList
        |> List.collect snd
        |> List.tryFind (fun draft -> draft.Id = draftId)

    let private nextDraftId layerId side state =
        let sideText =
            match side with
            | ProvenanceSide.Input -> "input"
            | ProvenanceSide.Output -> "output"

        let existing =
            state.Drafts
            |> Map.toList
            |> List.collect snd
            |> List.map (fun draft -> draft.Id)
            |> Set.ofList

        let rec loop index =
            let id = $"draft-{layerId}-{sideText}-{index}"
            if existing.Contains id then loop (index + 1) else id

        loop (existing.Count + 1)

    let add layerId side (key: GroupingKey) value unit state =
        let draft = {
            Id = nextDraftId layerId side state
            OwnerKind = key.Kind
            Category = key.Header
            Value = value
            Unit = unit
        }

        {
            state with
                Drafts =
                    state.Drafts
                    |> Map.add (Keys.draftKey layerId side) (forSide layerId side state @ [ draft ])
                PropertyRailPlacements = state.PropertyRailPlacements |> Map.add (layerId, key) side
                ExpandedProperties = state.ExpandedProperties |> Set.add (Keys.propertySlot layerId side key)
                Error = None
        }
        |> RailOrder.appendHeader layerId side key

    /// Promotion is a session mutation, so the UI only drops the draft once its
    /// assignment has been committed.
    let remove draftId state = {
        state with
            Drafts =
                state.Drafts
                |> Map.map (fun _ drafts -> drafts |> List.filter (fun draft -> draft.Id <> draftId))
    }

    /// Drops every draft of one property on one layer, both sides. Rail property
    /// removal uses this: a draft-only header has no session property, so the
    /// draft cleanup is the whole removal.
    let removeForProperty layerId (key: GroupingKey) state = {
        state with
            Drafts =
                state.Drafts
                |> Map.map (fun (draftLayerId, _) drafts ->
                    if draftLayerId = layerId then
                        drafts |> List.filter (matchesKey key >> not)
                    else
                        drafts
                )
    }

/// Manages batch assignment confirmation state for dropped property values.
module AssignmentBatch =

    let set (batch: PendingAssignmentBatch) state = {
        state with
            PendingAssignmentBatch = Some batch
            Error = None
    }

    let clear state = {
        state with
            PendingAssignmentBatch = None
    }

/// Manages the pending downstream annotation edit prompt (design §4/§7.2).
module AnnotationEdit =

    let set (pending: PendingAnnotationEdit) state = {
        state with
            PendingAnnotationEdit = Some pending
            Error = None
    }

    let clear state = {
        state with
            PendingAnnotationEdit = None
    }

/// Updates grouping assignments and side placement for properties.
module GroupingAssignments =

    let private removeProperty property (assignments: GroupingAssignment list) : GroupingAssignment list =
        let key = property
        assignments |> List.filter (fun assignment -> assignment.Key <> key)

    let private removePropertyScope property scope (assignments: GroupingAssignment list) : GroupingAssignment list =
        let key = property

        assignments
        |> List.filter (fun assignment -> assignment.Key <> key || assignment.Scope <> scope)

    let private upsert
        (assignment: GroupingAssignment)
        (assignments: GroupingAssignment list)
        : GroupingAssignment list =
        assignments
        |> List.filter (fun current -> current.Key <> assignment.Key)
        |> fun retained -> retained @ [ assignment ]

    let toggleSide sideId side property state =
        Sides.update
            sideId
            (fun current ->
                let key = property
                let scope = scopeForSide side
                let assignment: GroupingAssignment = { Key = key; Scope = scope }

                let isSelected =
                    current.GroupingAssignments
                    |> List.exists (fun current -> current.Key = key && current.Scope = scope)

                let nextAssignments =
                    if isSelected then
                        removePropertyScope property scope current.GroupingAssignments
                    else
                        upsert assignment current.GroupingAssignments

                {
                    current with
                        GroupingAssignments = nextAssignments
                }
            )
            state

    let toggleBoth inputSideId outputSideId property state =
        let key = property

        let isSelected =
            [ inputSideId; outputSideId ]
            |> List.exists (fun sideId ->
                (Sides.get sideId state).GroupingAssignments
                |> List.exists (fun assignment -> assignment.Key = key && assignment.Scope = GroupingScope.Both)
            )

        let setSide state sideId =
            Sides.update
                sideId
                (fun current ->
                    let nextAssignments =
                        if isSelected then
                            removePropertyScope property GroupingScope.Both current.GroupingAssignments
                        else
                            upsert
                                ({
                                    Key = key
                                    Scope = GroupingScope.Both
                                }
                                : GroupingAssignment)
                                current.GroupingAssignments

                    {
                        current with
                            GroupingAssignments = nextAssignments
                    }
                )
                state

        let withInput = setSide state inputSideId
        setSide withInput outputSideId

    let move layerId sourceSideId targetSideId targetSide property state =
        let key = property

        let sourceSide =
            match targetSide with
            | ProvenanceSide.Input -> ProvenanceSide.Output
            | ProvenanceSide.Output -> ProvenanceSide.Input

        // Switching sides moves the rail control; grouping state travels with the
        // header instead of being force-enabled on the target side.
        let wasGrouped =
            (Sides.get sourceSideId state).GroupingAssignments
            |> List.exists (fun assignment -> assignment.Key = key)

        let withoutSource =
            Sides.update
                sourceSideId
                (fun current -> {
                    current with
                        GroupingAssignments = removeProperty property current.GroupingAssignments
                })
                state

        let withTarget =
            if wasGrouped then
                let targetAssignment: GroupingAssignment = {
                    Key = key
                    Scope = scopeForSide targetSide
                }

                Sides.update
                    targetSideId
                    (fun current -> {
                        current with
                            GroupingAssignments = upsert targetAssignment current.GroupingAssignments
                    })
                    withoutSource
            else
                withoutSource

        {
            withTarget with
                PropertyRailPlacements = withTarget.PropertyRailPlacements |> Map.add (layerId, key) targetSide
        }
        |> RailOrder.removeHeader layerId sourceSide property
        |> RailOrder.appendHeader layerId targetSide property

/// Tracks selected input/output groups for layer creation.
module Selection =

    let clearLayer layerId state =
        let retain (selected: Set<ProvenanceLayerId * string>) =
            selected |> Set.filter (fun (currentLayerId, _) -> currentLayerId <> layerId)

        {
            state with
                SelectedInputs = retain state.SelectedInputs
                SelectedOutputs = retain state.SelectedOutputs
        }

    let contains layerId side groupId state =
        let identity = Keys.selectedGroup layerId groupId

        match side with
        | ProvenanceSide.Input -> state.SelectedInputs.Contains identity
        | ProvenanceSide.Output -> state.SelectedOutputs.Contains identity

    let toggle layerId side groupId state =
        let identity = Keys.selectedGroup layerId groupId

        match side with
        | ProvenanceSide.Input ->
            let selected =
                if state.SelectedInputs.Contains identity then
                    state.SelectedInputs.Remove identity
                else
                    state.SelectedInputs.Add identity

            { state with SelectedInputs = selected }
        | ProvenanceSide.Output ->
            let selected =
                if state.SelectedOutputs.Contains identity then
                    state.SelectedOutputs.Remove identity
                else
                    state.SelectedOutputs.Add identity

            {
                state with
                    SelectedOutputs = selected
            }

/// Tracks the details panel target shown beneath the editor surface.
module Detail =

    let isGroupExpanded side groupId state =
        state.ExpandedGroups |> Set.contains (side, groupId)

    let toggleGroup side groupId state =
        // Collapsing removes just this card; expanding replaces the set so manual
        // toggling keeps the familiar one-open-card behavior.
        let next =
            if isGroupExpanded side groupId state then
                state.ExpandedGroups |> Set.remove (side, groupId)
            else
                Set.singleton (side, groupId)

        {
            state with
                ExpandedGroups = next
                Detail = None
        }

    let showConnection connectionId state = {
        state with
            Detail = Some(ProvenanceDetail.Connection connectionId)
    }

/// Creates the complete UI state for a provenance session.
let init (session: ProvenanceSession) = {
    SideStates =
        session.LayerOrder
        |> List.collect (fun layerId -> [
            (layerId, ProvenanceSide.Input), Sides.empty
            (layerId, ProvenanceSide.Output), Sides.empty
        ])
        |> Map.ofList
    PropertyRailPlacements = Map.empty
    PropertyRailOrders = Map.empty
    ExpandedProperties = Set.empty
    Drafts = Map.empty
    PendingAssignmentBatch = None
    PanelRatios = Map.empty
    PendingMemberResolution = None
    PendingAnnotationEdit = None
    SelectedInputs = Set.empty
    SelectedOutputs = Set.empty
    ExpandedGroups = Set.empty
    Detail = None
    Error = None
    Hint = None
    PropertyColors = PropertyColors.empty
    Filters = Filters.defaultState
}

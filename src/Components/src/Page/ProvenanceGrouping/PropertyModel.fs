namespace Swate.Components.Page.ProvenanceGrouping

/// Stable keys for the property shelf folders.
module PropertyFolders =

    open System
    open Swate.Components.Page.ProvenanceGrouping.Identifiers

    let private slug (value: string) =
        let text = if isNull value then "" else value.Trim()

        let chars =
            text
            |> Seq.map (fun character ->
                if Char.IsLetterOrDigit character || character = '-' || character = '_' then
                    Char.ToLowerInvariant character
                else
                    '-'
            )
            |> Seq.toArray

        let compact =
            String(chars).Split([| '-' |], StringSplitOptions.RemoveEmptyEntries)
            |> String.concat "-"

        if String.IsNullOrWhiteSpace compact then
            "unknown"
        else
            compact

    /// Folders are keyed by origin *source id*. A node annotation's sources come
    /// from its owning node's appearances, so one annotation can legitimately
    /// belong to more than one source folder (design §3.5).
    let sourceFolderId (sourceId: ProvenanceSourceId) = $"source-{slug sourceId}"

    let unknownFolderId = "unknown-origin"

/// Builds property rail headers and values from the layer projection plus
/// UI-only draft state.
module PropertyRails =

    open Swate.Components.Page.ProvenanceGrouping.Identifiers
    open Swate.Components.Page.ProvenanceGrouping.Values
    open Swate.Components.Page.ProvenanceGrouping.AvailabilityTypes
    open Swate.Components.Page.ProvenanceGrouping.Domain
    open Swate.Components.Page.ProvenanceGrouping.ProjectionTypes
    open Swate.Components.Page.ProvenanceGrouping.Types
    open Swate.Components.Util.DurableIdDisambiguation

    /// The per-header UI key for a grouping value. Node and process annotations
    /// of the same header stay distinct, matching that they never group together
    /// (intent §7).
    let headerKeyOfGroupingValue (key: GroupingValueKey) : GroupingKey =
        match key with
        | NodeValue(header, _, _) -> {
            Kind = AnnotationOwnerKind.Node
            Header = header
          }
        | ProcessValue(header, _, _, _) -> {
            Kind = AnnotationOwnerKind.Process
            Header = header
          }

    /// The per-header UI key for a projected annotation.
    let headerKeyOf (annotation: ProjectedAnnotation) : GroupingKey = headerKeyOfGroupingValue annotation.Key

    /// One rail chip. An assigned value keeps its backing annotations so an edit
    /// or removal can resolve the exact owner; a draft has no assignment yet.
    type RailValue =
        | AssignedValue of definition: PropertyValueDefinition * backing: ProjectedAnnotation list
        | DraftValue of SidebarDraft
        | CatalogValue of entry: ReferenceCatalogEntry * displayLabel: string

    module RailValue =

        let value =
            function
            | AssignedValue(definition, _) -> definition.Value
            | DraftValue draft -> draft.Value
            | CatalogValue(entry, displayLabel) ->
                ProvenanceValue.Reference {
                    entry.Reference with
                        Label = displayLabel
                }

        let unit' =
            function
            | AssignedValue(definition, _) -> definition.Unit
            | DraftValue draft -> draft.Unit
            | CatalogValue(entry, _) -> entry.Unit

        let dragId =
            function
            | AssignedValue(_, annotation :: _) ->
                let assignmentId =
                    match annotation.Backing with
                    | NodeAssignmentBacking(identity, _, _)
                    | ProcessAssignmentBacking(identity, _, _, _, _) -> identity.AssignmentId

                $"assignment:{assignmentId}"
            | AssignedValue(definition, []) -> $"value:{definition.Id}"
            | DraftValue draft -> draft.Id
            | CatalogValue(entry, _) -> $"catalog:{entry.Reference.Scheme}:{entry.Reference.Id}"

        let tryDragPayload (header: GroupingKey) =
            function
            | AssignedValue(definition, backing) ->
                // A merged chip can span concrete kinds (intent §7 merges
                // `Characteristic: X` and `Factor: X` into one grouping value),
                // so the head backing's kind is only authoritative when every
                // backing agrees. On disagreement the payload degrades to a
                // Generic draft with no copy source: copying from the head would
                // materialize its kind and lineage silently. The tradeoff is
                // deliberate - `None` also forfeits the identified
                // overwrite-narrowing, so such a drop onto a target holding
                // several same-header assignments refuses instead of narrowing,
                // which is the safe direction.
                let backingKinds =
                    backing
                    |> List.map (fun annotation ->
                        match annotation.Backing with
                        | NodeAssignmentBacking(identity, _, _)
                        | ProcessAssignmentBacking(identity, _, _, _, _) -> identity.PropertyKind
                    )
                    |> List.distinct

                let source =
                    match backing |> List.tryHead, backingKinds with
                    | Some annotation, [ sharedKind ] ->
                        match annotation.Backing with
                        | NodeAssignmentBacking(identity, _, _) -> {
                            Key = header
                            PropertyKind = sharedKind
                            Value = definition.Value
                            Unit = definition.Unit
                            ContainerReferenceValueId = None
                            ReferenceSlotId = None
                            CopiedFromAssignmentId = Some identity.AssignmentId
                          }
                        | ProcessAssignmentBacking(identity, _, _, containerReferenceValueId, referenceSlotId) -> {
                            Key = header
                            PropertyKind = sharedKind
                            Value = definition.Value
                            Unit = definition.Unit
                            ContainerReferenceValueId = containerReferenceValueId
                            ReferenceSlotId = referenceSlotId
                            CopiedFromAssignmentId = Some identity.AssignmentId
                          }
                    | _ -> {
                        Key = header
                        PropertyKind = AssignmentPropertyKind.Generic
                        Value = definition.Value
                        Unit = definition.Unit
                        ContainerReferenceValueId = None
                        ReferenceSlotId = None
                        CopiedFromAssignmentId = None
                      }

                Some {
                    DefinitionId = Some definition.Id
                    DraftId = None
                    Source = source
                }
            | DraftValue draft ->
                Some {
                    DefinitionId = None
                    DraftId = Some draft.Id
                    Source = {
                        Key = header
                        PropertyKind = AssignmentPropertyKind.Generic
                        Value = draft.Value
                        Unit = draft.Unit
                        ContainerReferenceValueId = None
                        ReferenceSlotId = None
                        CopiedFromAssignmentId = None
                    }
                }
            | CatalogValue _ -> None

        let isDraft =
            function
            | AssignedValue _ -> false
            | DraftValue _ -> true
            | CatalogValue _ -> false

        /// Every distinct session value ID represented by this entry. One
        /// displayed value may aggregate several backing definitions: a global
        /// removal applies to every backing value ID and assignment
        /// represented by the entry.
        let backingValueIds =
            function
            | AssignedValue(definition, backing) ->
                [
                    yield definition.Id
                    for annotation in backing do
                        match annotation.Backing with
                        | NodeAssignmentBacking(identity, _, _) -> yield identity.ValueId
                        | ProcessAssignmentBacking(identity, _, _, _, _) -> yield identity.ValueId
                ]
                |> Set.ofList
            | DraftValue _
            | CatalogValue _ -> Set.empty

    /// Where a header's values reach the viewed layer from. Layer-awareness is
    /// resolved once, where the annotations are still in hand, so no consumer
    /// has to re-derive it from a layer-independent availability relation.
    type PropertyOrigin =
        | CurrentLayer
        | Upstream

    type RailProjection = {
        Headers: GroupingKey list
        ValuesByHeader: Map<GroupingKey, RailValue list>
        ExpandedHeaders: Set<GroupingKey>
        CanSwitchHeaders: Set<GroupingKey>
        StatsByHeader: Map<GroupingKey, PropertyStats>
        ConnectionCountByHeader: Map<GroupingKey, int>
        BadgeByHeader: Map<GroupingKey, PropertyCountBadge>
        ColorByHeader: Map<GroupingKey, ProvenanceColor>
        /// Whether each header is carried by the viewed layer, reaches it from
        /// upstream, or both. Origin filtering derives from the availability
        /// relation (design §14.2's node assignment rules: "filtering derives
        /// from the availability relation"), classified per annotation against
        /// the viewed layer so an incident value from another layer's process
        /// counts as upstream.
        OriginsByHeader: Map<GroupingKey, Set<PropertyOrigin>>
        SourcesByHeader: Map<GroupingKey, Set<ProvenanceSourceId>>
        OriginFilterOptions: PropertyOriginFilter list
    }

    let groupsForSide side (projection: CachedLayerProjection) =
        projection.Groups |> List.filter (fun group -> group.Side = side)

    /// One-pass caches over a side of the projection. Rail projection asks
    /// per-header questions for every header, so the shared passes are hoisted
    /// here instead of re-walking the groups per header.
    type SideIndex = {
        /// Distinct canonical nodes on the side; the denominator for coverage.
        ItemCount: int
        Annotations: ProjectedAnnotation list
        AnnotationsByHeader: Map<GroupingKey, ProjectedAnnotation list>
        HeaderSet: Set<GroupingKey>
        DistinctValueCountByHeader: Map<GroupingKey, int>
        ItemsWithValueCountByHeader: Map<GroupingKey, int>
        HeadersByNodeId: Map<CanonicalNodeId, Set<GroupingKey>>
    }

    let buildSideIndex side (projection: CachedLayerProjection) : SideIndex =
        let groups = groupsForSide side projection

        let nodeIds =
            groups
            |> List.collect (fun group -> group.CanonicalNodeIds |> Set.toList)
            |> List.distinct

        // Annotations are deduplicated per (node, header, value) so a node
        // appearing in several groups is not counted twice.
        let perNode =
            nodeIds
            |> List.map (fun nodeId ->
                let annotations =
                    groups
                    |> List.filter (fun group -> group.CanonicalNodeIds.Contains nodeId)
                    |> List.collect _.Annotations
                    |> List.distinct

                nodeId, annotations
            )

        let annotations = perNode |> List.collect snd |> List.distinct

        let annotationsByHeader = annotations |> List.groupBy headerKeyOf |> Map.ofList

        {
            ItemCount = nodeIds.Length
            Annotations = annotations
            AnnotationsByHeader = annotationsByHeader
            HeaderSet = annotationsByHeader |> Map.keys |> Set.ofSeq
            DistinctValueCountByHeader =
                annotationsByHeader
                |> Map.map (fun _ grouped -> grouped |> List.map _.Key |> List.distinct |> List.length)
            ItemsWithValueCountByHeader =
                annotationsByHeader
                |> Map.map (fun header _ ->
                    perNode
                    |> List.filter (fun (_, nodeAnnotations) ->
                        nodeAnnotations
                        |> List.exists (fun annotation -> headerKeyOf annotation = header)
                    )
                    |> List.length
                )
            HeadersByNodeId =
                perNode
                |> List.map (fun (nodeId, nodeAnnotations) ->
                    nodeId, nodeAnnotations |> List.map headerKeyOf |> Set.ofList
                )
                |> Map.ofList
        }

    let private headerSortKey (key: GroupingKey) =
        key.Header.Name.Trim().ToLowerInvariant(),
        key.Header.Name,
        (key.Header.TermAccession |> Option.defaultValue ""),
        (match key.Kind with
         | AnnotationOwnerKind.Node -> "0"
         | AnnotationOwnerKind.Process -> "1")

    let headersFromIndex (index: SideIndex) =
        index.HeaderSet |> Set.toList |> List.sortBy headerSortKey

    let private placedHeadersForSide layerId side (uiState: UiState) =
        uiState.PropertyRailPlacements
        |> Map.toList
        |> List.choose (fun ((currentLayerId, key), targetSide) ->
            if currentLayerId = layerId && targetSide = side then
                Some key
            else
                None
        )

    let private railPlacement layerId property (uiState: UiState) =
        uiState.PropertyRailPlacements |> Map.tryFind (layerId, property)

    /// Which headers belong on this side's rail: everything the projection puts
    /// on either side, plus this layer's drafts, filtered by explicit placement
    /// and otherwise by the side the annotation actually appears on.
    let propertyRailHeadersFromIndexes
        (inputIndex: SideIndex)
        (outputIndex: SideIndex)
        (catalogHeaders: Set<GroupingKey>)
        layerId
        side
        (uiState: UiState)
        =
        let projectedHeaders =
            [
                yield! headersFromIndex inputIndex
                yield! headersFromIndex outputIndex
            ]
            |> List.distinct
            |> List.sortBy headerSortKey

        let projectedHeaderSet = Set.ofList projectedHeaders

        let draftInputSet =
            State.Drafts.propertiesForSide layerId ProvenanceSide.Input uiState
            |> Set.ofList

        let draftOutputSet =
            State.Drafts.propertiesForSide layerId ProvenanceSide.Output uiState
            |> Set.ofList

        let draftHeaders = State.Drafts.propertiesForSide layerId side uiState

        let draftSetFor draftSide =
            if draftSide = ProvenanceSide.Input then
                draftInputSet
            else
                draftOutputSet

        let hasDraft property =
            draftInputSet.Contains property || draftOutputSet.Contains property

        let defaultSideForHeader property =
            if outputIndex.HeaderSet.Contains property then
                Some ProvenanceSide.Output
            elif inputIndex.HeaderSet.Contains property then
                Some ProvenanceSide.Input
            else
                None

        let knownHeaders =
            [
                yield! projectedHeaders
                yield! draftHeaders
                yield! catalogHeaders
            ]
            |> List.distinct
            |> Set.ofList

        let isValidRailSide header =
            projectedHeaderSet.Contains header
            || hasDraft header
            || catalogHeaders.Contains header

        [
            yield! projectedHeaders
            yield! placedHeadersForSide layerId side uiState
            yield! draftHeaders
        ]
        |> List.distinct
        |> List.filter knownHeaders.Contains
        |> List.filter (fun header ->
            match railPlacement layerId header uiState with
            | Some targetSide when isValidRailSide header -> targetSide = side
            | Some _ -> defaultSideForHeader header = Some side || (draftSetFor side).Contains header
            | None ->
                defaultSideForHeader header = Some side
                || (defaultSideForHeader header).IsNone && (draftSetFor side).Contains header
        )
        |> List.sortBy headerSortKey

    let headersForSide side (projection: CachedLayerProjection) =
        buildSideIndex side projection |> headersFromIndex

    let hasHeaderForSide side property (projection: CachedLayerProjection) =
        (buildSideIndex side projection).HeaderSet.Contains property

    let canSwitchHeader property (projection: CachedLayerProjection) =
        hasHeaderForSide ProvenanceSide.Input property projection
        && hasHeaderForSide ProvenanceSide.Output property projection

module Search =

    let contains (needle: string) (haystack: string) =
        let needle = if isNull needle then "" else needle.Trim()

        if System.String.IsNullOrWhiteSpace needle then
            true
        elif isNull haystack then
            false
        else
            haystack.ToLowerInvariant().Contains(needle.ToLowerInvariant())

module PropertyProjection =

    open Swate.Components.Page.ProvenanceGrouping.Identifiers
    open Swate.Components.Page.ProvenanceGrouping.Values
    open Swate.Components.Page.ProvenanceGrouping.AvailabilityTypes
    open Swate.Components.Page.ProvenanceGrouping.ProjectionTypes
    open Swate.Components.Page.ProvenanceGrouping.Types
    open PropertyRails

    let badgeForStats (stats: PropertyStats) : PropertyCountBadge =
        if stats.DistinctValueCount = 1 && stats.ItemsWithValueCount = stats.TotalItemCount then
            Hide
        elif stats.DistinctValueCount = 1 && stats.ItemsWithValueCount < stats.TotalItemCount then
            Coverage(stats.ItemsWithValueCount, stats.TotalItemCount)
        else
            DistinctValues stats.DistinctValueCount

    let propertyStatsForSide
        (side: ProvenanceSide)
        (property: GroupingKey)
        (projection: CachedLayerProjection)
        : PropertyStats =
        let index = buildSideIndex side projection

        {
            Property = property
            DistinctValueCount =
                index.DistinctValueCountByHeader
                |> Map.tryFind property
                |> Option.defaultValue 0
            ItemsWithValueCount =
                index.ItemsWithValueCountByHeader
                |> Map.tryFind property
                |> Option.defaultValue 0
            TotalItemCount = index.ItemCount
        }

    let headerMatchesProjectedValues (searchText: string) (property: GroupingKey) (values: RailValue list) =
        Search.contains searchText property.Header.Name
        || Search.contains searchText (property.Header.TermAccession |> Option.defaultValue "")
        || values
           |> List.exists (fun railValue ->
               Search.contains
                   searchText
                   (Formatting.formatValue (RailValue.value railValue) (RailValue.unit' railValue))
           )

    let valueCountFilterMatches (filter: PropertyValueCountFilter) (badge: PropertyCountBadge) =
        match filter, badge with
        | PropertyValueCountFilter.Any, _ -> true
        | PropertyValueCountFilter.Singleton, PropertyCountBadge.Hide -> true
        | PropertyValueCountFilter.Singleton, PropertyCountBadge.Coverage _ -> true
        | PropertyValueCountFilter.Multiple, PropertyCountBadge.DistinctValues n -> n > 1
        | PropertyValueCountFilter.CoverageGap, PropertyCountBadge.Coverage _ -> true
        | _ -> false

    let originFilterMatches
        (filter: PropertyOriginFilter)
        (origins: Set<PropertyOrigin>)
        (sourceIds: Set<ProvenanceSourceId>)
        =
        match filter with
        | PropertyOriginFilter.AnyOrigin -> true
        | PropertyOriginFilter.CurrentOnly -> origins.Contains CurrentLayer
        | PropertyOriginFilter.AnyUpstream -> origins.Contains Upstream
        | PropertyOriginFilter.Source sourceId -> sourceIds.Contains sourceId

    let sortHeaders
        (sort: PropertySort)
        (statsByHeader: Map<GroupingKey, PropertyStats>)
        (connectionCountsByHeader: Map<GroupingKey, int>)
        (headers: GroupingKey list)
        =
        let nameKey (key: GroupingKey) =
            key.Header.Name.Trim().ToLowerInvariant(),
            key.Header.Name,
            (key.Header.TermAccession |> Option.defaultValue ""),
            (match key.Kind with
             | AnnotationOwnerKind.Node -> "0"
             | AnnotationOwnerKind.Process -> "1")

        match sort with
        | PropertySort.ValueCountDesc ->
            headers
            |> List.sortBy (fun header ->
                let count =
                    statsByHeader.TryFind header
                    |> Option.map (fun stats -> stats.DistinctValueCount)
                    |> Option.defaultValue 0

                let name, rawName, accession, kind = nameKey header
                -count, name, rawName, accession, kind
            )
        | PropertySort.NameAsc -> headers |> List.sortBy nameKey
        | PropertySort.ConnectionCountDesc ->
            headers
            |> List.sortBy (fun header ->
                let count = connectionCountsByHeader.TryFind header |> Option.defaultValue 0
                let name, rawName, accession, kind = nameKey header
                -count, name, rawName, accession, kind
            )

    /// The filter offers every source actually present on the rail, so a
    /// boundary node's upstream source is selectable where it exists.
    let originFilterOptions (sourcesByHeader: Map<GroupingKey, Set<ProvenanceSourceId>>) : PropertyOriginFilter list =
        let sources =
            sourcesByHeader
            |> Map.toList
            |> List.collect (snd >> Set.toList)
            |> List.distinct
            |> List.sort

        [
            PropertyOriginFilter.AnyOrigin
            PropertyOriginFilter.CurrentOnly
            PropertyOriginFilter.AnyUpstream
            yield! sources |> List.map PropertyOriginFilter.Source
        ]

    let railProjectionWithFilters
        (session: ProvenanceSession)
        (layerId: ProvenanceLayerId)
        (side: ProvenanceSide)
        (projection: CachedLayerProjection)
        (uiState: UiState)
        : RailProjection =
        let filters = uiState.Filters
        let inputIndex = buildSideIndex ProvenanceSide.Input projection
        let outputIndex = buildSideIndex ProvenanceSide.Output projection

        let sideIndex =
            if side = ProvenanceSide.Input then
                inputIndex
            else
                outputIndex

        let catalogEntries =
            projection.ShelfEntries
            |> List.choose (fun shelfEntry ->
                match shelfEntry.Payload with
                | CatalogBacked payload -> Some payload.Entry
                | AssignmentBacked _ -> None
            )

        let catalogHeaders =
            catalogEntries
            |> List.map (fun entry -> {
                Kind = entry.AssignmentKind
                Header = entry.Category
            })
            |> Set.ofList

        let catalogDisplayNames =
            catalogEntries
            |> List.map (fun entry ->
                {
                    DisplayLabel = entry.Reference.Label
                    Scheme = entry.Reference.Scheme
                    DurableId = entry.Reference.Id
                }
                : Swate.Components.Util.DurableIdDisambiguation.DurableLabel
            )
            |> Swate.Components.Util.DurableIdDisambiguation.disambiguate

        let headers =
            propertyRailHeadersFromIndexes inputIndex outputIndex catalogHeaders layerId side uiState

        let annotationsForHeader header =
            sideIndex.AnnotationsByHeader |> Map.tryFind header |> Option.defaultValue []

        let valuesByHeader =
            headers
            |> List.map (fun header ->
                let catalog =
                    catalogEntries
                    |> List.filter (fun entry -> entry.AssignmentKind = header.Kind && entry.Category = header.Header)
                    |> List.map (fun entry ->
                        CatalogValue(
                            entry,
                            catalogDisplayNames
                            |> Map.tryFind (entry.Reference.Scheme, entry.Reference.Id)
                            |> Option.defaultValue entry.Reference.Label
                        )
                    )

                // One chip per grouping value (intent §6/§7): equal header,
                // typed value, and unit — plus origin source for process
                // values — collapse into one entry. Assignment and writeback
                // identity (owners, assignment IDs, value IDs) stays in the
                // chip's backing list and never splits the display.
                let assigned =
                    annotationsForHeader header
                    |> List.groupBy _.Key
                    |> List.choose (fun (_, backing) ->
                        backing
                        |> List.tryPick (fun annotation ->
                            let valueId =
                                match annotation.Backing with
                                | NodeAssignmentBacking(identity, _, _) -> identity.ValueId
                                | ProcessAssignmentBacking(identity, _, _, _, _) -> identity.ValueId

                            session.Values |> Map.tryFind valueId
                        )
                        |> Option.map (fun definition -> AssignedValue(definition, backing))
                    )
                    |> List.filter (fun railValue ->
                        match RailValue.value railValue with
                        | ProvenanceValue.Reference reference ->
                            catalog
                            |> List.exists (fun catalogValue ->
                                match RailValue.value catalogValue with
                                | ProvenanceValue.Reference candidate ->
                                    candidate.Scheme = reference.Scheme && candidate.Id = reference.Id
                                | _ -> false
                            )
                            |> not
                        | _ -> true
                    )

                let drafts =
                    State.Drafts.forProperty layerId side header uiState |> List.map DraftValue

                header,
                [ yield! catalog; yield! assigned; yield! drafts ]
                |> List.sortBy (fun railValue ->
                    Formatting.formatValue (RailValue.value railValue) (RailValue.unit' railValue)
                )
            )
            |> Map.ofList

        let statsByHeader =
            headers
            |> List.map (fun header ->
                header,
                {
                    Property = header
                    DistinctValueCount =
                        sideIndex.DistinctValueCountByHeader
                        |> Map.tryFind header
                        |> Option.defaultValue 0
                    ItemsWithValueCount =
                        sideIndex.ItemsWithValueCountByHeader
                        |> Map.tryFind header
                        |> Option.defaultValue 0
                    TotalItemCount = sideIndex.ItemCount
                }
            )
            |> Map.ofList

        // A header's connector count is how many of this layer's connectors carry
        // one of the side's nodes that holds it.
        let connectionCountsByHeader =
            let countedHeaders =
                projection.Connectors
                |> List.collect (fun connector ->
                    let endpointKeys =
                        if side = ProvenanceSide.Input then
                            connector.InputEndpointKeys
                        else
                            connector.OutputEndpointKeys

                    endpointKeys
                    |> Set.toList
                    |> List.collect (fun key ->
                        sideIndex.HeadersByNodeId
                        |> Map.tryFind key.NodeId
                        |> Option.map Set.toList
                        |> Option.defaultValue []
                    )
                    |> List.distinct
                )
                |> List.countBy id
                |> Map.ofList

            headers
            |> List.map (fun header -> header, countedHeaders |> Map.tryFind header |> Option.defaultValue 0)
            |> Map.ofList

        let originsByHeader =
            headers
            |> List.map (fun header ->
                let assignedOrigins =
                    annotationsForHeader header
                    |> List.map (fun annotation ->
                        if Projection.annotationIsCurrentForLayer session layerId annotation then
                            CurrentLayer
                        else
                            Upstream
                    )
                    |> Set.ofList

                let origins =
                    if
                        assignedOrigins.IsEmpty
                        && (State.Drafts.forProperty layerId side header uiState |> List.isEmpty |> not)
                    then
                        // Draft values live only in this layer/side palette. They
                        // have no assignment relation yet, but are current for
                        // filtering and origin display until first promotion.
                        Set.singleton CurrentLayer
                    else
                        assignedOrigins

                header, origins
            )
            |> Map.ofList

        let sourcesByHeader =
            headers
            |> List.map (fun header ->
                header,
                annotationsForHeader header
                |> List.fold
                    (fun sources annotation ->
                        Set.union sources (Projection.originSourceIdsForAnnotation session annotation)
                    )
                    Set.empty
            )
            |> Map.ofList

        let badgeByHeader = statsByHeader |> Map.map (fun _ stats -> badgeForStats stats)

        // Design §3.5: this is the UI colour site. Gather the item's origin
        // sources and hand them to the shared resolver; no local fallback.
        let colorByHeader =
            headers
            |> List.map (fun header ->
                header,
                State.PropertyColors.resolveColor
                    uiState.PropertyColors
                    (State.PropertyColors.colorKey header.Kind header.Header)
                    (sourcesByHeader |> Map.tryFind header |> Option.defaultValue Set.empty)
            )
            |> Map.ofList

        let filtered =
            headers
            |> List.filter (fun header ->
                headerMatchesProjectedValues filters.SearchText header valuesByHeader.[header]
                && valueCountFilterMatches filters.ValueCountFilter badgeByHeader.[header]
                && originFilterMatches filters.OriginFilter originsByHeader.[header] sourcesByHeader.[header]
            )

        let defaultSorted =
            sortHeaders filters.PropertySort statsByHeader connectionCountsByHeader filtered

        let sorted =
            match State.RailOrder.tryGet layerId side uiState with
            | Some order -> State.RailOrder.apply order defaultSorted
            | None -> defaultSorted

        {
            Headers = sorted
            ValuesByHeader = sorted |> List.map (fun header -> header, valuesByHeader.[header]) |> Map.ofList
            StatsByHeader = statsByHeader
            ConnectionCountByHeader = connectionCountsByHeader
            BadgeByHeader = badgeByHeader
            ColorByHeader = sorted |> List.map (fun header -> header, colorByHeader.[header]) |> Map.ofList
            OriginsByHeader = originsByHeader
            SourcesByHeader = sourcesByHeader
            OriginFilterOptions = originFilterOptions sourcesByHeader
            ExpandedHeaders =
                sorted
                |> List.filter (fun header -> State.PropertyExpansion.isExpanded layerId side header uiState)
                |> Set.ofList
            CanSwitchHeaders =
                sorted
                |> List.filter (fun header ->
                    inputIndex.HeaderSet.Contains header && outputIndex.HeaderSet.Contains header
                )
                |> Set.ofList
        }

/// Projects the active session layer into renderable groups, connectors, and
/// layer commands.
module Display =

    open Swate.Components.Page.ProvenanceGrouping.Identifiers
    open Swate.Components.Page.ProvenanceGrouping.Domain
    open Swate.Components.Page.ProvenanceGrouping.ProjectionTypes
    open Swate.Components.Page.ProvenanceGrouping.Types

    /// The cached projection for a layer. Refresh is the session's job, so a
    /// missing or stale entry is reported rather than silently recomputed here.
    let tryLayerProjection layerId (session: ProvenanceSession) =
        session.LayerProjections
        |> Map.tryFind layerId
        |> Option.filter (fun projection -> not projection.Stale)

    let activeLayer (session: ProvenanceSession) : ProvenanceLayer = session.Layers.[session.ActiveLayerId]

    /// The headers one side currently groups by. A `Both`-scoped assignment
    /// applies to either side; a side-scoped one only to its own.
    let activeGroupingHeaders layerId side (uiState: UiState) =
        (State.Sides.get (layerId, side) uiState).GroupingAssignments
        |> List.filter (fun assignment -> scopeAppliesToSide side assignment.Scope)
        |> List.map _.Key
        |> Set.ofList

    /// The cards and connectors the editor renders. The session caches the
    /// finest partition (one card per endpoint); the grouping selection is UI
    /// state, so the coarsening happens here (intent §6, §7). With nothing
    /// selected every item keeps its own card and its endpoint name.
    let displayLayer (session: ProvenanceSession) (uiState: UiState) =
        let layer = activeLayer session

        match tryLayerProjection layer.Id session with
        | Some projection ->
            let headersBySide =
                [ ProvenanceSide.Input; ProvenanceSide.Output ]
                |> List.map (fun side -> side, activeGroupingHeaders layer.Id side uiState)
                |> Map.ofList

            let isActive side key =
                headersBySide
                |> Map.tryFind side
                |> Option.map (Set.contains (PropertyRails.headerKeyOfGroupingValue key))
                |> Option.defaultValue false

            // Regrouping only re-partitions already-projected annotations, so
            // the only failure it can report is one the cached projection would
            // already have failed on. Falling back to the cache keeps a card
            // visible rather than blanking the editor.
            let groups, connectors =
                match Projection.regroupLayer isActive layer session projection with
                | Ok(groups, connectors) -> groups, connectors
                | Error _ -> projection.Groups, projection.Connectors

            layer,
            groups |> List.filter (fun group -> group.Side = ProvenanceSide.Input),
            groups |> List.filter (fun group -> group.Side = ProvenanceSide.Output),
            connectors
        | None -> layer, [], [], []

    let nodesInGroups layerId (groups: DisplayGroup list) selectedIds =
        groups
        |> List.filter (fun group -> selectedIds |> Set.contains (layerId, group.Id))
        |> List.collect (fun group -> group.CanonicalNodeIds |> Set.toList)
        |> List.distinct

    let layerRequest name layerId inputGroups outputGroups uiState : AddLayerRequest = {
        Name = name
        SelectedNodes = [
            yield!
                nodesInGroups layerId inputGroups uiState.SelectedInputs
                |> List.map (fun nodeId -> ProvenanceSide.Input, nodeId)
            yield!
                nodesInGroups layerId outputGroups uiState.SelectedOutputs
                |> List.map (fun nodeId -> ProvenanceSide.Output, nodeId)
        ]
    }

    let sortGroups (sort: GroupSort) (connectors: DisplayConnector list) (groups: DisplayGroup list) =
        match sort with
        | GroupSort.NameAsc -> groups |> List.sortBy (fun group -> group.Id)
        | GroupSort.MemberCountDesc -> groups |> List.sortByDescending (fun group -> group.CanonicalNodeIds.Count)
        | GroupSort.ConnectionCountDesc ->
            groups
            |> List.sortByDescending (fun group ->
                connectors
                |> List.sumBy (fun connector ->
                    if connector.InputGroupId = group.Id || connector.OutputGroupId = group.Id then
                        connector.LinkIds.Count
                    else
                        0
                )
            )

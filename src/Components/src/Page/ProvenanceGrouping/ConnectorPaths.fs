namespace Swate.Components.Page.ProvenanceGrouping

open Fable.Core
open Fable.Core.JsInterop
open Feliz
open Browser.Types
open Swate.Components.Page.ProvenanceGrouping.Identifiers
open Swate.Components.Page.ProvenanceGrouping.Values
open Swate.Components.Page.ProvenanceGrouping.Domain
open Swate.Components.Page.ProvenanceGrouping.ProjectionTypes
open Swate.Components.Primitive.ContextMenu
open Swate.Components.Primitive.ContextMenu.Types
open Swate.Components.Page.ProvenanceGrouping.Types

/// Projects model/UI state into logical connector specs, and measures specs into
/// concrete SVG paths. Spec derivation is pure and memoizable; only the measure
/// step reads the DOM, so layout observers can remeasure cheaply.
module ConnectorPaths =

    let private spec
        key
        testId
        className
        strokeWidth
        strokeDasharray
        interactiveConnection
        ariaLabel
        color
        skipWhenClose
        source
        target
        : ConnectorSpec =
        {
            Key = key
            TestId = testId
            ClassName = className
            StrokeWidth = strokeWidth
            StrokeDasharray = strokeDasharray
            InteractiveConnector = interactiveConnection
            AriaLabel = ariaLabel
            Color = color
            Source = source
            Target = target
            SkipWhenClose = skipWhenClose
            SankeyWeight = None
        }

    let private groupById (inputGroups: DisplayGroup list) (outputGroups: DisplayGroup list) side groupId =
        let groups =
            match side with
            | ProvenanceSide.Input -> inputGroups
            | ProvenanceSide.Output -> outputGroups

        groups |> List.tryFind (fun group -> group.Id = groupId)

    let private isGroupedCard (inputGroups: DisplayGroup list) (outputGroups: DisplayGroup list) side groupId =
        groupById inputGroups outputGroups side groupId
        |> Option.exists (fun group -> group.Annotations |> List.isEmpty |> not)

    let private isConnectedToExpanded
        (inputGroups: DisplayGroup list)
        (outputGroups: DisplayGroup list)
        (connections: DisplayConnector list)
        side
        groupId
        overlayState
        =
        ConnectorOverlayState.followsExpandedNeighbors overlayState
        && isGroupedCard inputGroups outputGroups side groupId
        && (connections
            |> List.exists (fun connection ->
                match side with
                | ProvenanceSide.Input ->
                    connection.InputGroupId = groupId
                    && ConnectorOverlayState.isGroupExpanded
                        ProvenanceSide.Output
                        connection.OutputGroupId
                        overlayState
                | ProvenanceSide.Output ->
                    connection.OutputGroupId = groupId
                    && ConnectorOverlayState.isGroupExpanded ProvenanceSide.Input connection.InputGroupId overlayState
            ))

    let private isGroupExpanded
        (inputGroups: DisplayGroup list)
        (outputGroups: DisplayGroup list)
        (connections: DisplayConnector list)
        side
        groupId
        overlayState
        =
        ConnectorOverlayState.isGroupExpanded side groupId overlayState
        || isConnectedToExpanded inputGroups outputGroups connections side groupId overlayState

    let groupConnectionSpecs
        (inputGroups: DisplayGroup list)
        (outputGroups: DisplayGroup list)
        (connections: DisplayConnector list)
        overlayState
        =
        (connections: DisplayConnector list)
        // Expanded endpoints swap the aggregate group connector for the
        // member-level connectors, so the group line disappears instead of
        // doubling up underneath them.
        |> List.filter (fun connection ->
            not (
                isGroupExpanded
                    inputGroups
                    outputGroups
                    connections
                    ProvenanceSide.Input
                    connection.InputGroupId
                    overlayState
            )
            && not (
                isGroupExpanded
                    inputGroups
                    outputGroups
                    connections
                    ProvenanceSide.Output
                    connection.OutputGroupId
                    overlayState
            )
        )
        |> List.map (fun connection ->
            // Group connectors paint as sankey ribbons instead of graph edges;
            // weighting by the underlying connection count makes heavy bundles
            // claim a wider share of their cards' edges.
            {
                spec
                    $"connection:{connection.Id}"
                    "provenance-connection"
                    "swt:text-primary"
                    2.25
                    None
                    (Some connection)
                    (Some $"Select connection {connection.Id}")
                    None
                    false
                    (ConnectorHandles.group ProvenanceSide.Input connection.InputGroupId)
                    (ConnectorHandles.group ProvenanceSide.Output connection.OutputGroupId) with
                    SankeyWeight = Some(float connection.LinkIds.Count)
            })

    let memberConnectionSpecs
        (session: ProvenanceSession)
        (inputGroups: DisplayGroup list)
        (outputGroups: DisplayGroup list)
        (connections: DisplayConnector list)
        overlayState
        =
        // Link id -> its exact endpoint pair. One process may own several links,
        // so this is built from the links themselves rather than from processes.
        let linkShapes =
            session.Processes
            |> Map.toList
            |> List.collect (fun (_, structuralProcess) ->
                structuralProcess.Links
                |> Map.toList
                |> List.map (fun (linkId, link) -> linkId, link.Shape)
            )
            |> Map.ofList

        (connections: DisplayConnector list)
        |> List.collect (fun displayConnection ->
            let inputExpanded =
                isGroupExpanded
                    inputGroups
                    outputGroups
                    connections
                    ProvenanceSide.Input
                    displayConnection.InputGroupId
                    overlayState

            let outputExpanded =
                isGroupExpanded
                    inputGroups
                    outputGroups
                    connections
                    ProvenanceSide.Output
                    displayConnection.OutputGroupId
                    overlayState

            if not inputExpanded && not outputExpanded then
                []
            else
                displayConnection.LinkIds
                |> Set.toList
                |> List.choose (fun connectionId ->
                    linkShapes
                    |> Map.tryFind connectionId
                    |> Option.bind (fun shape ->
                        match shape with
                        | ProcessLinkShape.Between(inputNodeId, outputNodeId) -> Some(inputNodeId, outputNodeId)
                        // A one-sided or endpointless link draws no member-level
                        // connector, because it has no pair to join.
                        | ProcessLinkShape.InputOnly _
                        | ProcessLinkShape.OutputOnly _
                        | ProcessLinkShape.Endpointless -> None
                    )
                    |> Option.map (fun (inputNodeId, outputNodeId) ->
                        let source =
                            if inputExpanded then
                                ConnectorHandles.member'
                                    ProvenanceSide.Input
                                    displayConnection.InputGroupId
                                    inputNodeId
                            else
                                ConnectorHandles.group ProvenanceSide.Input displayConnection.InputGroupId

                        let target =
                            if outputExpanded then
                                ConnectorHandles.member'
                                    ProvenanceSide.Output
                                    displayConnection.OutputGroupId
                                    outputNodeId
                            else
                                ConnectorHandles.group ProvenanceSide.Output displayConnection.OutputGroupId

                        let singleConnection = {
                            displayConnection with
                                LinkIds = Set.singleton connectionId
                        }

                        // Member connectors are ribbons too, so expanding a card
                        // fans its group ribbon out into per-member ribbons instead
                        // of falling back to plain lines. Each stands for exactly
                        // one underlying connection, hence the unit weight. The
                        // visual path inherits pointer-events:none from the SVG
                        // root; only the separate hit path opts back in.
                        {
                            spec
                                $"member:{displayConnection.Id}:{connectionId}"
                                "provenance-member-connection"
                                "swt:text-primary/70"
                                2.0
                                None
                                (Some singleConnection)
                                (Some $"Select connection {displayConnection.Id}")
                                None
                                false
                                source
                                target with
                                SankeyWeight = Some 1.
                        }
                    )
                )
        )

    let private memberHasMatchingValue
        (group: DisplayGroup)
        (predicate: ProjectedAnnotation -> bool)
        (nodeId: CanonicalNodeId)
        =
        GroupCardData.memberAnnotations nodeId group |> List.exists predicate

    type private RailConnectionTarget = {
        KeySuffix: string
        Handle: ConnectionHandleRef
    }

    let private matchingMembers (predicate: ProjectedAnnotation -> bool) (group: DisplayGroup) =
        GroupCardData.memberIds group
        |> List.filter (memberHasMatchingValue group predicate)

    let private railConnectionTargets
        inputGroups
        outputGroups
        (connections: DisplayConnector list)
        predicate
        side
        overlayState
        (group: DisplayGroup)
        =
        let members = matchingMembers predicate group

        if members.IsEmpty then
            []
        elif isGroupExpanded inputGroups outputGroups connections side group.Id overlayState then
            members
            |> List.map (fun memberId -> {
                KeySuffix = $"{group.Id}:{memberId}"
                Handle = ConnectorHandles.memberPropertyAnchor side group.Id memberId
            })
        else
            [
                {
                    KeySuffix = group.Id
                    Handle = ConnectorHandles.propertyAnchor side group.Id
                }
            ]

    /// Dashed rail connectors derived from model data only: collapsed properties
    /// draw one line per same-side group containing any value for that property.
    let private railConnectionSpecsForSide
        layerId
        inputGroups
        outputGroups
        (connections: DisplayConnector list)
        side
        groups
        (railProjection: PropertyRails.RailProjection)
        (colorByHeader: Map<AnnotationHeaderKey, string option>)
        overlayState
        =
        railProjection.Headers
        |> List.filter (fun property ->
            not (ConnectorOverlayState.isPropertyExpanded layerId side property overlayState)
        )
        |> List.collect (fun property ->
            let color =
                railProjection.ColorByHeader
                |> Map.tryFind property
                |> Option.orElseWith (fun () -> colorByHeader |> Map.tryFind property |> Option.bind id)

            groups
            |> List.collect (
                railConnectionTargets
                    inputGroups
                    outputGroups
                    connections
                    (fun annotation -> PropertyRails.headerKeyOf annotation = property)
                    side
                    overlayState
            )
            |> List.map (fun target ->
                spec
                    $"property:{side}:{DragDrop.propertyKeyIdentity property}:{target.KeySuffix}"
                    "provenance-property-connection"
                    "swt:text-secondary swt:pointer-events-none"
                    1.75
                    (Some "4 4")
                    None
                    None
                    color
                    true
                    (ConnectorHandles.propertyHeader side property)
                    target.Handle
            )
        )

    let private railValueKey (property: AnnotationHeaderKey) (railValue: PropertyRails.RailValue) =
        let identity =
            Projection.toGroupingValueIdentity (PropertyRails.RailValue.value railValue)

        let unit' = PropertyRails.RailValue.unit' railValue

        match property.Kind with
        | AnnotationOwnerKind.Node -> NodeValue(property.Header, identity, unit')
        // A process value's key also carries its origin source, which a rail
        // value does not know; matching therefore compares the node-shaped key
        // and falls back to header equality for process values.
        | AnnotationOwnerKind.Process -> NodeValue(property.Header, identity, unit')

    /// Value-level rail connectors match on the grouping key, so equal values
    /// under one header line up without consulting stored value ids.
    let private propertyValueMatches property (key: GroupingValueKey) (annotation: ProjectedAnnotation) =
        let sameValue =
            match annotation.Key, key with
            | NodeValue(_, left, leftUnit), NodeValue(_, right, rightUnit)
            | ProcessValue(_, left, leftUnit, _), NodeValue(_, right, rightUnit) -> left = right && leftUnit = rightUnit
            | _ -> annotation.Key = key

        PropertyRails.headerKeyOf annotation = property && sameValue

    let private valueRailConnectionSpecsForSide
        layerId
        inputGroups
        outputGroups
        (connections: DisplayConnector list)
        side
        groups
        (railProjection: PropertyRails.RailProjection)
        (colorByHeader: Map<AnnotationHeaderKey, string option>)
        overlayState
        =
        railProjection.Headers
        |> List.filter (fun property -> ConnectorOverlayState.isPropertyExpanded layerId side property overlayState)
        |> List.collect (fun property ->
            let color =
                railProjection.ColorByHeader
                |> Map.tryFind property
                |> Option.orElseWith (fun () -> colorByHeader |> Map.tryFind property |> Option.bind id)

            railProjection.ValuesByHeader
            |> Map.tryFind property
            |> Option.defaultValue []
            |> List.collect (fun propertyValue ->
                groups
                |> List.collect (
                    railConnectionTargets
                        inputGroups
                        outputGroups
                        connections
                        (propertyValueMatches property (railValueKey property propertyValue))
                        side
                        overlayState
                )
                |> List.map (fun target ->
                    spec
                        $"value:{side}:{DragDrop.propertyKeyIdentity property}:{Formatting.formatValue (PropertyRails.RailValue.value propertyValue) (PropertyRails.RailValue.unit' propertyValue)}:{target.KeySuffix}"
                        "provenance-value-connection"
                        "swt:text-accent swt:pointer-events-none"
                        2.0
                        (Some "4 4")
                        None
                        None
                        color
                        true
                        (ConnectorHandles.propertyValue side (PropertyRails.RailValue.dragId propertyValue))
                        target.Handle
                )
            )
        )

    let railConnectionSpecs
        layerId
        inputGroups
        outputGroups
        (connections: DisplayConnector list)
        inputRailProjection
        outputRailProjection
        (colorByHeader: Map<AnnotationHeaderKey, string option>)
        overlayState
        showPropertyHeaderConnectors
        =
        [
            if showPropertyHeaderConnectors then
                yield!
                    railConnectionSpecsForSide
                        layerId
                        inputGroups
                        outputGroups
                        connections
                        ProvenanceSide.Input
                        inputGroups
                        inputRailProjection
                        colorByHeader
                        overlayState

                yield!
                    railConnectionSpecsForSide
                        layerId
                        inputGroups
                        outputGroups
                        connections
                        ProvenanceSide.Output
                        outputGroups
                        outputRailProjection
                        colorByHeader
                        overlayState
            yield!
                valueRailConnectionSpecsForSide
                    layerId
                    inputGroups
                    outputGroups
                    connections
                    ProvenanceSide.Input
                    inputGroups
                    inputRailProjection
                    colorByHeader
                    overlayState
            yield!
                valueRailConnectionSpecsForSide
                    layerId
                    inputGroups
                    outputGroups
                    connections
                    ProvenanceSide.Output
                    outputGroups
                    outputRailProjection
                    colorByHeader
                    overlayState
        ]

    let liveConnection liveConnectionDrag =
        liveConnectionDrag
        |> Option.bind (fun live ->
            ConnectorMeasure.pathBetweenPoints live.Start live.Current
            |> Option.map (fun path -> {
                Key = "live"
                Path = path
                TestId = "provenance-live-connection"
                // connector-flow marches the dashes toward the pointer while aiming.
                ClassName = "swt:text-primary swt:pointer-events-none swt:opacity-80 swt:connector-flow"
                StrokeWidth = 2.25
                StrokeDasharray = Some "6 4"
                InteractiveConnector = None
                AriaLabel = None
                Color = None
                Midpoint = None
                // Aiming keeps the classic line; only committed group
                // connections render as ribbons.
                RibbonPath = None
            })
        )

    /// All logical connectors for the current editor state, in paint order.
    let specs
        layerId
        (session: ProvenanceSession)
        inputGroups
        outputGroups
        (connections: DisplayConnector list)
        inputRailProjection
        outputRailProjection
        (colorByHeader: Map<AnnotationHeaderKey, string option>)
        overlayState
        showPropertyHeaderConnectors
        : ConnectorSpec list =
        [
            yield!
                railConnectionSpecs
                    layerId
                    inputGroups
                    outputGroups
                    connections
                    inputRailProjection
                    outputRailProjection
                    colorByHeader
                    overlayState
                    showPropertyHeaderConnectors
            yield! groupConnectionSpecs inputGroups outputGroups connections overlayState
            yield! memberConnectionSpecs session inputGroups outputGroups connections overlayState
        ]

    /// Resolves specs against the measured DOM; specs whose handles are missing or
    /// (for rail connectors) too close together are dropped.
    let measure context (specs: ConnectorSpec list) : MeasuredConnector list =
        // Sankey ribbons cannot be measured one by one: every group connection
        // claims a weighted share of its cards' facing edges, so they are laid
        // out together first and looked up per spec below.
        let sankeyRibbons =
            specs
            |> List.choose (fun spec ->
                spec.SankeyWeight
                |> Option.map (fun weight ->
                    ({
                        Key = spec.Key
                        Source = spec.Source
                        Target = spec.Target
                        Weight = weight
                    }
                    : ConnectorMeasure.SankeyRibbonRequest)
                )
            )
            |> ConnectorMeasure.measureSankeyRibbons context

        specs
        |> List.choose (fun spec ->
            let measured =
                match spec.SankeyWeight with
                | Some _ ->
                    sankeyRibbons.TryFind spec.Key
                    |> Option.map (fun ribbon -> ribbon.Path, Some ribbon.RibbonPath, Some ribbon.Midpoint)
                | None when spec.SkipWhenClose ->
                    ConnectorMeasure.pathBetweenDistantHandles context spec.Source spec.Target
                    |> Option.map (fun path -> path, None, None)
                | None ->
                    ConnectorMeasure.pathWithMidpointBetweenHandles context spec.Source spec.Target
                    |> Option.map (fun (path, midpoint) -> path, None, Some midpoint)

            measured
            |> Option.map (fun (path, ribbonPath, midpoint) -> {
                Key = spec.Key
                Path = path
                TestId = spec.TestId
                ClassName = spec.ClassName
                StrokeWidth = spec.StrokeWidth
                StrokeDasharray = spec.StrokeDasharray
                InteractiveConnector = spec.InteractiveConnector
                AriaLabel = spec.AriaLabel
                Color = spec.Color
                Midpoint = midpoint
                RibbonPath = ribbonPath
            })
        )

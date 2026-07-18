namespace Swate.Components.Page.ProvenanceGrouping

open Fable.Core
open Fable.Core.JsInterop
open Feliz
open Browser.Types
open Swate.Components.Page.ProvenanceGrouping.ProvenanceTypes
open Swate.Components.Page.ProvenanceGrouping.Grouping
open Swate.Components.Page.ProvenanceGrouping.Session
open Swate.Components.Primitive.ContextMenu
open Swate.Components.Primitive.ContextMenu.Types
open Swate.Components.Page.ProvenanceGrouping.Types

/// A rail connector's removal target: the property (or one exact value of it)
/// on the rail end, and the endpoints its group end covers. Right-clicking the
/// connector retracts that property from exactly those endpoints.
type ConnectorPropertyTarget = {
    Property: ProvenancePropertyKey
    /// `None` targets the whole header - every value of it on those endpoints.
    PropertyValueId: ProvenancePropertyValueId option
    Side: ProvenanceSide
    GroupLabel: string
    SetIds: ProvenanceSetId list
}

type MeasuredConnector = {
    Key: string
    Path: string
    TestId: string
    ClassName: string
    StrokeWidth: float
    StrokeDasharray: string option
    InteractiveConnection: DisplayConnection option
    /// Set for rail connectors, which are not selectable but do offer removal.
    PropertyTarget: ConnectorPropertyTarget option
    AriaLabel: string option
    Color: string option
    /// Curve midpoint, measured so summary badges (e.g. underlying-connection
    /// counts) can sit on the line without re-deriving the bezier.
    Midpoint: ConnectionPoint option
    /// Closed sankey-ribbon outline. When set, the connector is painted as a
    /// translucent filled band spanning its share of both card edges instead
    /// of a stroked line; `Path` stays the centerline for the midpoint badge,
    /// while the ribbon itself is the pointer target.
    RibbonPath: string option
}

/// Logical connector between two handles, derived purely from model/UI state.
/// Measuring specs against the DOM produces MeasuredConnector values; keeping the
/// derivation out of the measure path lets observers remeasure without re-scanning
/// the model.
type ConnectorSpec = {
    Key: string
    TestId: string
    ClassName: string
    StrokeWidth: float
    StrokeDasharray: string option
    InteractiveConnection: DisplayConnection option
    PropertyTarget: ConnectorPropertyTarget option
    AriaLabel: string option
    Color: string option
    Source: ConnectionHandleRef
    Target: ConnectionHandleRef
    /// Rail connectors drop out entirely when their endpoints are too close.
    SkipWhenClose: bool
    /// Weight for connectors drawn as sankey ribbons (group and member
    /// connections): the ribbon claims this share of each endpoint edge (card
    /// side or member-row side) relative to the other ribbons attached there,
    /// so together they cover the edge completely. None keeps the
    /// stroked-line rendering.
    SankeyWeight: float option
}

type ConnectorOverlayState = {
    ExpandedGroups: Set<ProvenanceSide * string>
    SelectedConnectionId: string option
    ExpandedProperties: Set<ProvenanceLayerId * ProvenanceSide * GroupingKey>
}

module ConnectorOverlayState =

    let fromUiState (uiState: UiState) =
        let selectedConnectionId =
            match uiState.Detail with
            | Some(ProvenanceDetail.Connection connectionId) -> Some connectionId
            | _ -> None

        {
            ExpandedGroups = uiState.ExpandedGroups
            SelectedConnectionId = selectedConnectionId
            ExpandedProperties = uiState.ExpandedProperties
        }

    let isGroupExpanded side groupId state =
        state.ExpandedGroups |> Set.contains (side, groupId)

    /// Connected cards only follow a single manually expanded card. When several
    /// cards are expanded explicitly (manual connection resolution), only those
    /// exact cards open.
    let followsExpandedNeighbors state = state.ExpandedGroups.Count = 1

    let isPropertyExpanded layerId side property state =
        state.ExpandedProperties.Contains(layerId, side, property)

type ConnectorMeasureContext = {
    Container: HTMLElement
    Origin: ClientRect
    Nodes: Map<string, HTMLElement>
    /// Group-card elements by their `data-provenance-group-node` id; sankey
    /// ribbons measure whole card edges instead of the small handle circles.
    GroupNodes: Map<string, HTMLElement>
    /// Member-row elements of expanded cards by their
    /// `data-provenance-member-node` id; member-level sankey ribbons measure
    /// whole row edges the way group ribbons measure card edges.
    MemberNodes: Map<string, HTMLElement>
}

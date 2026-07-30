namespace Swate.Components.Page.ProvenanceGrouping

/// Stable identity strings for React keys, DOM lookup attributes, and DnD payload/drop parsing.
module DragDrop =

    open Swate.Components.Page.ProvenanceGrouping.Identifiers
    open Swate.Components.Page.ProvenanceGrouping.Values
    open Swate.Components.Page.ProvenanceGrouping.Types

    let private encode (value: string) = System.Uri.EscapeDataString value
    let private decode (value: string) = System.Uri.UnescapeDataString value

    let private termIdentity (term: ProvenanceTerm) =
        let source = term.TermSource |> Option.defaultValue ""
        let accession = term.TermAccession |> Option.defaultValue ""
        $"{encode term.Name}|{encode source}|{encode accession}"

    let private ownerKindText kind =
        match kind with
        | AnnotationOwnerKind.Node -> "Node"
        | AnnotationOwnerKind.Process -> "Process"

    let private valueIdentity value =
        match value with
        | ProvenanceValue.Text text -> $"Text:{encode text}"
        | ProvenanceValue.Integer integer -> $"Integer:{integer}"
        | ProvenanceValue.Float float -> $"Float:{float}"
        | ProvenanceValue.Term term -> $"Term:{termIdentity term}"
        // Identity is the scheme and durable id; the label is display metadata
        // only and must never enter an identity string (intent §2).
        | ProvenanceValue.Reference reference -> $"Reference:{encode reference.Scheme}|{encode reference.Id}"

    /// A per-header UI identity. The owner kind is part of it because node and
    /// process annotations of the same header are always distinct, and there is
    /// no origin source in it: node grouping ignores origin, and a process
    /// value's origin belongs to its value key rather than its header.
    let propertyKeyIdentity (property: AnnotationHeaderKey) =
        $"{ownerKindText property.Kind}:{termIdentity property.Header}"

    let propertyHeaderIdentity = propertyKeyIdentity

    let draftIdentity (draft: SidebarDraft) =
        let unit = draft.Unit |> Option.map termIdentity |> Option.defaultValue ""
        $"{draft.Id}:{valueIdentity draft.Value}:Unit:{unit}"

    /// Id-free identity for one grouping value, so a freshly dropped value can be
    /// found again on the card it regrouped into (the model assigns it a new id
    /// there). Quote-safe for use inside quoted attribute selectors.
    let groupingValueIdentity (property: AnnotationHeaderKey) (value: ProvenanceValue) (unit: ProvenanceTerm option) =
        let unitText = unit |> Option.map termIdentity |> Option.defaultValue ""

        // encode maps to encodeURIComponent, which leaves apostrophes alone.
        $"{propertyKeyIdentity property}:{valueIdentity value}:Unit:{unitText}".Replace("'", "%27")

    let valueDragId valueId = $"provenance-value|{encode valueId}"

    let propertyDragId side property =
        $"provenance-property|{side}|{encode (propertyKeyIdentity property)}"

    let folderPropertyDragId side property =
        $"provenance-folder-property|{side}|{encode (propertyKeyIdentity property)}"

    let propertyRailDropId side = $"provenance-property-drop|{side}"

    let groupDragId side groupId =
        $"provenance-group|{side}|{encode groupId}"

    let groupDropId side groupId =
        $"provenance-drop|{side}|{encode groupId}"

    let groupNodeId side groupId =
        $"provenance-node::{side}::{encode groupId}"

    let memberNodeId side groupId nodeId =
        $"provenance-member-node::{side}::{encode groupId}::{encode nodeId}"

    let private handleKindText kind =
        match kind with
        | ConnectionHandleKind.GroupCard -> "GroupCard"
        | ConnectionHandleKind.GroupMember -> "GroupMember"
        | ConnectionHandleKind.GroupMemberPropertyAnchor -> "GroupMemberPropertyAnchor"
        | ConnectionHandleKind.PropertyHeader -> "PropertyHeader"
        | ConnectionHandleKind.PropertyValue -> "PropertyValue"
        | ConnectionHandleKind.GroupPropertyAnchor -> "GroupPropertyAnchor"

    let private tryHandleKind value =
        match value with
        | "GroupCard" -> Some ConnectionHandleKind.GroupCard
        | "GroupMember" -> Some ConnectionHandleKind.GroupMember
        | "GroupMemberPropertyAnchor" -> Some ConnectionHandleKind.GroupMemberPropertyAnchor
        | "PropertyHeader" -> Some ConnectionHandleKind.PropertyHeader
        | "PropertyValue" -> Some ConnectionHandleKind.PropertyValue
        | "GroupPropertyAnchor" -> Some ConnectionHandleKind.GroupPropertyAnchor
        | _ -> None

    let connectionHandleIdentity (handle: ConnectionHandleRef) =
        let parent = handle.ParentGroupId |> Option.defaultValue ""
        $"{handleKindText handle.Kind}|{handle.Side}|{encode handle.Id}|{encode parent}"

    let connectionHandleDragId handle =
        $"provenance-connection-drag|{connectionHandleIdentity handle}"

    let connectionHandleDropId handle =
        $"provenance-connection-drop|{connectionHandleIdentity handle}"

    let connectionHandleNodeId handle =
        $"provenance-connection-node::{connectionHandleIdentity handle}"

    type Payload =
        | PropertyValue of PropertyValueDefinitionId
        | PropertyHeader of ProvenanceSide * string
        | FolderPropertyHeader of ProvenanceSide * string
        | Group of ProvenanceSide * string
        | ConnectionHandle of ConnectionHandleRef

    let private tryParseHandleParts kind side sourceId parent =
        match tryHandleKind kind, side with
        | Some kind, "Input" ->
            Some {
                Kind = kind
                Side = ProvenanceSide.Input
                Id = decode sourceId
                ParentGroupId = if parent = "" then None else Some(decode parent)
            }
        | Some kind, "Output" ->
            Some {
                Kind = kind
                Side = ProvenanceSide.Output
                Id = decode sourceId
                ParentGroupId = if parent = "" then None else Some(decode parent)
            }
        | _ -> None

    let tryDragId (id: string) =
        match id.Split('|') with
        | [| "provenance-value"; valueId |] -> Some(Payload.PropertyValue(decode valueId))
        | [| "provenance-property"; "Input"; headerId |] ->
            Some(Payload.PropertyHeader(ProvenanceSide.Input, decode headerId))
        | [| "provenance-property"; "Output"; headerId |] ->
            Some(Payload.PropertyHeader(ProvenanceSide.Output, decode headerId))
        | [| "provenance-folder-property"; "Input"; headerId |] ->
            Some(Payload.FolderPropertyHeader(ProvenanceSide.Input, decode headerId))
        | [| "provenance-folder-property"; "Output"; headerId |] ->
            Some(Payload.FolderPropertyHeader(ProvenanceSide.Output, decode headerId))
        | [| "provenance-group"; "Input"; groupId |] -> Some(Payload.Group(ProvenanceSide.Input, decode groupId))
        | [| "provenance-group"; "Output"; groupId |] -> Some(Payload.Group(ProvenanceSide.Output, decode groupId))
        | [| "provenance-connection-drag"; kind; side; sourceId; parent |] ->
            tryParseHandleParts kind side sourceId parent
            |> Option.map Payload.ConnectionHandle
        | _ -> None

    let tryDropId (id: string) =
        match id.Split('|') with
        | [| "provenance-drop"; "Input"; groupId |] -> Some(ProvenanceSide.Input, decode groupId)
        | [| "provenance-drop"; "Output"; groupId |] -> Some(ProvenanceSide.Output, decode groupId)
        | _ -> None

    let tryPropertyDropId (id: string) =
        match id.Split('|') with
        | [| "provenance-property-drop"; "Input" |] -> Some ProvenanceSide.Input
        | [| "provenance-property-drop"; "Output" |] -> Some ProvenanceSide.Output
        | _ -> None

    let tryConnectionDropId (id: string) =
        match id.Split('|') with
        | [| "provenance-connection-drop"; kind; side; sourceId; parent |] ->
            tryParseHandleParts kind side sourceId parent
        | _ -> None

/// Keeps transient connector dragging outside editor state so pointer moves only
/// repaint the overlay layer that needs the live path.
module LiveDrag =

    open Swate.Components.Page.ProvenanceGrouping.Types

    type Store = {
        mutable Current: LiveConnectionDrag option
        mutable Listeners: (unit -> unit) list
    }

    let create () : Store = { Current = None; Listeners = [] }

    let private notify store =
        for listener in store.Listeners do
            listener ()

    let subscribe listener store =
        store.Listeners <- listener :: store.Listeners

        fun () ->
            store.Listeners <-
                store.Listeners
                |> List.filter (fun current -> not (System.Object.ReferenceEquals(current, listener)))

    let start source point store =
        store.Current <-
            Some {
                Source = source
                Start = point
                Current = point
            }

        notify store

    let moveTo point store =
        store.Current <- store.Current |> Option.map (fun current -> { current with Current = point })
        notify store

    let clear store =
        if store.Current.IsSome then
            store.Current <- None
            notify store

/// Tracks the hovered group card outside editor state, LiveDrag-style: hovering a
/// card emphasizes its connectors and marks the connected opposite cards without
/// re-rendering the editor tree.
module HoverHighlight =

    open Fable.Core
    open Fable.Core.JsInterop
    open Feliz
    open Swate.Components.Page.ProvenanceGrouping.Identifiers

    type Target = {
        Side: ProvenanceSide
        GroupId: string
    }

    type Store = {
        mutable Current: Target option
        mutable Listeners: (unit -> unit) list
    }

    let create () : Store = { Current = None; Listeners = [] }

    let private notify store =
        for listener in store.Listeners do
            listener ()

    let subscribe listener store =
        store.Listeners <- listener :: store.Listeners

        fun () ->
            store.Listeners <-
                store.Listeners
                |> List.filter (fun current -> not (System.Object.ReferenceEquals(current, listener)))

    let set target store =
        if store.Current <> Some target then
            store.Current <- Some target
            notify store

    let clear store =
        if store.Current.IsSome then
            store.Current <- None
            notify store

    /// The store instance itself is the (stable) context value; consumers subscribe
    /// for changes instead of re-rendering through context updates.
    let context = React.createContext (defaultValue = create ())

    [<Fable.Core.ImportMember("react")>]
    let private createElement (comp: obj) (props: obj) (children: ReactElement) : ReactElement = jsNative

    let provider (store: Store) (children: ReactElement) : ReactElement =
        createElement !!context?Provider {| value = store |} children

/// Validates edge-handle drag/drop pairs and returns the editor action they imply.
module ConnectionRouting =

    open Swate.Components.Page.ProvenanceGrouping.Identifiers
    open Swate.Components.Page.ProvenanceGrouping.Values
    open Swate.Components.Page.ProvenanceGrouping.Types

    type ConnectionAction =
        | ConnectGroups of inputGroupId: string * outputGroupId: string
        | ConnectMembers of
            inputGroupId: string *
            outputGroupId: string *
            inputNodeId: CanonicalNodeId *
            outputNodeId: CanonicalNodeId
        | ConnectMemberToGroup of
            inputGroupId: string *
            outputGroupId: string *
            memberNodeId: CanonicalNodeId *
            memberSide: ProvenanceSide

    let private oppositeSides left right = left.Side <> right.Side

    let private orderedGroups left right =
        match left.Side, right.Side with
        | ProvenanceSide.Input, ProvenanceSide.Output -> left.Id, right.Id
        | ProvenanceSide.Output, ProvenanceSide.Input -> right.Id, left.Id
        | _ -> left.Id, right.Id

    let private orderedMembers left right =
        match left.Side, right.Side, left.ParentGroupId, right.ParentGroupId with
        | ProvenanceSide.Input, ProvenanceSide.Output, Some inputGroupId, Some outputGroupId ->
            Some(inputGroupId, outputGroupId, left.Id, right.Id)
        | ProvenanceSide.Output, ProvenanceSide.Input, Some outputGroupId, Some inputGroupId ->
            Some(inputGroupId, outputGroupId, right.Id, left.Id)
        | _ -> None

    let action source target =
        match source.Kind, target.Kind with
        | ConnectionHandleKind.GroupCard, ConnectionHandleKind.GroupCard when oppositeSides source target ->
            let inputGroupId, outputGroupId = orderedGroups source target
            Some(ConnectionAction.ConnectGroups(inputGroupId, outputGroupId))
        | ConnectionHandleKind.GroupMember, ConnectionHandleKind.GroupMember when oppositeSides source target ->
            orderedMembers source target |> Option.map ConnectionAction.ConnectMembers
        | ConnectionHandleKind.GroupMember, ConnectionHandleKind.GroupCard when oppositeSides source target ->
            source.ParentGroupId
            |> Option.map (fun sourceParent ->
                let inputGroupId, outputGroupId =
                    if source.Side = ProvenanceSide.Input then
                        sourceParent, target.Id
                    else
                        target.Id, sourceParent

                ConnectionAction.ConnectMemberToGroup(inputGroupId, outputGroupId, source.Id, source.Side)
            )
        | ConnectionHandleKind.GroupCard, ConnectionHandleKind.GroupMember when oppositeSides source target ->
            target.ParentGroupId
            |> Option.map (fun targetParent ->
                let inputGroupId, outputGroupId =
                    if target.Side = ProvenanceSide.Input then
                        targetParent, source.Id
                    else
                        source.Id, targetParent

                ConnectionAction.ConnectMemberToGroup(inputGroupId, outputGroupId, target.Id, target.Side)
            )
        | _ -> None

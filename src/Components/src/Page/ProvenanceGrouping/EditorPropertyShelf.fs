namespace Swate.Components.Page.ProvenanceGrouping

open System
open System.Globalization
open Fable.Core
open Fable.Core.JsInterop
open Feliz
open Swate.Components.Composite.FolderedDraggableList
open Swate.Components.Composite.FolderedDraggableList.Types
open Swate.Components.JsBindings
open Swate.Components.Page.ProvenanceGrouping.Identifiers
open Swate.Components.Page.ProvenanceGrouping.Values
open Swate.Components.Page.ProvenanceGrouping.Domain
open Swate.Components.Page.ProvenanceGrouping.ProjectionTypes
open Swate.Components.Page.ProvenanceGrouping.Types
open Swate.Components.Util.DurableIdDisambiguation

module PropertyShelf =

    type private FolderKey =
        | SourceFolder of ProvenanceSourceRef
        | ResourceFolder
        | UnknownFolder

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

        let slug = String(chars).Trim('-')

        if String.IsNullOrWhiteSpace slug then "item" else slug

    let private badgeText (badge: PropertyCountBadge option) : string option =
        match badge with
        | Some PropertyCountBadge.Hide
        | None -> None
        | Some(PropertyCountBadge.DistinctValues count) -> Some(string count)
        | Some(PropertyCountBadge.Coverage(withValue, total)) -> Some($"{withValue}/{total}")

    let private headerIdentity (property: GroupingKey) = DragDrop.propertyKeyIdentity property

    let private headerId (property: GroupingKey) = headerIdentity property |> slug

    let private sourceSideForHeader
        (layerId: ProvenanceLayerId)
        (inputProjection: PropertyRails.RailProjection)
        (outputProjection: PropertyRails.RailProjection)
        (uiState: UiState)
        (property: GroupingKey)
        =
        match uiState.PropertyRailPlacements |> Map.tryFind (layerId, property) with
        | Some side -> side
        | None when outputProjection.Headers |> List.contains property -> ProvenanceSide.Output
        | _ -> ProvenanceSide.Input

    let private sourceRefsForSession (session: ProvenanceSession) =
        session.Layers
        |> Map.toList
        |> List.map (fun (_, layer) -> layer.Source)
        |> List.distinctBy _.Id

    let private sourcesForHeader
        (inputProjection: PropertyRails.RailProjection)
        (outputProjection: PropertyRails.RailProjection)
        (property: GroupingKey)
        =
        [
            inputProjection.SourcesByHeader |> Map.tryFind property
            outputProjection.SourcesByHeader |> Map.tryFind property
        ]
        |> List.choose id
        |> List.fold Set.union Set.empty

    let private folderKeyId =
        function
        | SourceFolder source -> PropertyFolders.sourceFolderId source.Id
        | ResourceFolder -> "provenance-resource-folder"
        | UnknownFolder -> PropertyFolders.unknownFolderId

    let private folderName =
        function
        | SourceFolder source -> source.Name
        | ResourceFolder -> "Resources"
        | UnknownFolder -> "Unknown origin"

    let private folderColor (uiState: UiState) =
        function
        | SourceFolder source -> uiState.PropertyColors.SourceColors |> Map.tryFind source.Id
        | ResourceFolder -> None
        | UnknownFolder -> None

    let setFolderColor (session: ProvenanceSession) folderId color state =
        let sourceRefs = sourceRefsForSession session

        let source =
            sourceRefs
            |> List.tryFind (fun source -> folderKeyId (SourceFolder source) = folderId)

        match source, color with
        | Some source, Some selectedColor -> State.PropertyColors.setSourceColor source.Id selectedColor state
        | Some source, None -> State.PropertyColors.clearSourceColor source.Id state
        | None, _ -> state

    let private folderSort (session: ProvenanceSession) activeLayerId key =
        let activeLayer = session.Layers |> Map.tryFind activeLayerId

        match key, activeLayer with
        | SourceFolder source, Some layer when source.Id = layer.Source.Id -> 0, 0, folderName key
        | SourceFolder source, _ ->
            let layerIndex =
                session.LayerOrder
                |> List.tryFindIndex (fun layerId ->
                    session.Layers
                    |> Map.tryFind layerId
                    |> Option.map (fun l -> l.Source.Id = source.Id)
                    |> Option.defaultValue false
                )
                |> Option.defaultValue Int32.MaxValue

            1, layerIndex, folderName key
        | ResourceFolder, _ -> 2, Int32.MaxValue, folderName key
        | UnknownFolder, _ -> 3, Int32.MaxValue, folderName key

    let private manualColor (uiState: UiState) (property: GroupingKey) =
        uiState.PropertyColors.ManualPropertyColors |> Map.tryFind property

    let private isPlacedInCurrentLayer (layer: ProvenanceLayer) (uiState: UiState) (property: GroupingKey) =
        let placedInRail =
            uiState.PropertyRailPlacements |> Map.containsKey (layer.Id, property)

        let groupedOnSide sideId =
            (State.Sides.get sideId uiState).GroupingAssignments
            |> List.exists (fun assignment -> assignment.Key = property)

        placedInRail
        || groupedOnSide (layer.Id, ProvenanceSide.Input)
        || groupedOnSide (layer.Id, ProvenanceSide.Output)

    let folders
        session
        (layer: ProvenanceLayer)
        uiState
        (inputProjection: PropertyRails.RailProjection)
        (outputProjection: PropertyRails.RailProjection)
        : FolderedDraggableFolder<PropertyShelfItemPayload> list =
        let sourceRefById =
            sourceRefsForSession session
            |> List.map (fun source -> source.Id, source)
            |> Map.ofList

        let projection = session.LayerProjections |> Map.tryFind layer.Id

        let headerOfBacking backing =
            let ownerKind, propertyId =
                match backing with
                | NodeAssignmentBacking(identity, _, _) -> AnnotationOwnerKind.Node, identity.PropertyId
                | ProcessAssignmentBacking(identity, _, _, _, _) -> AnnotationOwnerKind.Process, identity.PropertyId

            session.Properties
            |> Map.tryFind propertyId
            |> Option.map (fun property -> {
                Kind = ownerKind
                Header = property.Category
            })

        let sourceFoldersForBacking backing =
            match backing with
            | NodeAssignmentBacking(_, ownerId, _) ->
                // Node assignments do not own provenance origin metadata. Their
                // shelf folders are derived from every layer in which the owner
                // node appears, preserving LayerOrder for deterministic display.
                session.LayerOrder
                |> List.choose (fun layerId -> session.Layers |> Map.tryFind layerId)
                |> List.filter (fun ownerLayer ->
                    ownerLayer.InputEndpoints |> Map.containsKey ownerId
                    || ownerLayer.OutputEndpoints |> Map.containsKey ownerId
                )
                |> List.choose (fun ownerLayer -> sourceRefById |> Map.tryFind ownerLayer.Source.Id)
                |> List.distinctBy _.Id
                |> List.map SourceFolder
            | ProcessAssignmentBacking(_, processId, _, _, _) ->
                session.Processes
                |> Map.tryFind processId
                |> Option.bind (fun structuralProcess ->
                    session.Layers
                    |> Map.tryFind structuralProcess.OriginLayerId
                    |> Option.bind (fun ownerLayer -> sourceRefById |> Map.tryFind ownerLayer.Source.Id)
                    |> Option.map SourceFolder
                )
                |> Option.toList

        let sourceSideForEntry property =
            sourceSideForHeader layer.Id inputProjection outputProjection uiState property

        let catalogDisplayNames =
            projection
            |> Option.map (fun current ->
                current.ShelfEntries
                |> List.choose (fun entry ->
                    match entry.Payload with
                    | CatalogBacked payload ->
                        Some {
                            DisplayLabel = payload.Entry.Reference.Label
                            Scheme = payload.Entry.Reference.Scheme
                            DurableId = payload.Entry.Reference.Id
                        }
                    | AssignmentBacked _ -> None
                )
                |> disambiguate
            )
            |> Option.defaultValue Map.empty

        let itemForEntry folderKey (entry: PropertyShelfEntry) =
            match entry.Payload with
            | AssignmentBacked payload ->
                match headerOfBacking payload.Backing with
                | None -> None
                | Some property when isPlacedInCurrentLayer layer uiState property -> None
                | Some property ->
                    // One shelf row per property per folder: the
                    // shelf drag payload only carries the property, so the many
                    // assignments backing one property are writeback detail
                    // that must not multiply the row. The per-Id dedupe below
                    // collapses the duplicates this shared Id produces.
                    Some(
                        {
                            Id = $"{folderKeyId folderKey}-property-{headerId property}"
                            Label = property.Header.Name
                            Payload = {
                                Property = property
                                SourceSide = sourceSideForEntry property
                                ShelfPayload = entry.Payload
                            }
                            Color = manualColor uiState property
                            Badge = None
                            Tooltip = Some(folderName folderKey)
                            Disabled = false
                        }
                    )
            | CatalogBacked payload ->
                let property = {
                    Kind = payload.Entry.AssignmentKind
                    Header = payload.Entry.Category
                }

                Some(
                    {
                        Id =
                            $"{folderKeyId folderKey}-catalog-{slug payload.Entry.Reference.Scheme}-{slug payload.Entry.Reference.Id}"
                        Label =
                            catalogDisplayNames
                            |> Map.tryFind (payload.Entry.Reference.Scheme, payload.Entry.Reference.Id)
                            |> Option.defaultValue payload.Entry.Reference.Label
                        Payload = {
                            Property = property
                            SourceSide = sourceSideForEntry property
                            ShelfPayload = entry.Payload
                        }
                        Color = manualColor uiState property
                        Badge = None
                        Tooltip = Some($"{payload.Entry.Reference.Scheme}: {payload.Entry.Reference.Id}")
                        Disabled = false
                    }
                )

        let itemEntries =
            projection
            |> Option.map (fun current ->
                current.ShelfEntries
                |> List.collect (fun entry ->
                    let folderKeys =
                        match entry.Payload with
                        | CatalogBacked _ -> [ ResourceFolder ]
                        | AssignmentBacked payload ->
                            match sourceFoldersForBacking payload.Backing with
                            | [] -> [ UnknownFolder ]
                            | sourceFolders -> sourceFolders

                    folderKeys
                    |> List.choose (fun folderKey ->
                        itemForEntry folderKey entry |> Option.map (fun item -> folderKey, item)
                    )
                )
            )
            |> Option.defaultValue []

        let shelfHeaderOrder =
            [
                yield! outputProjection.Headers
                yield! inputProjection.Headers
            ]
            |> List.distinct

        let itemEntries =
            itemEntries
            |> List.sortBy (fun (_, item) ->
                let rank =
                    shelfHeaderOrder
                    |> List.tryFindIndex ((=) item.Payload.Property)
                    |> Option.defaultValue Int32.MaxValue

                rank, item.Label, item.Id
            )

        let folderKeys =
            [
                yield SourceFolder layer.Source
                yield! itemEntries |> List.map fst
            ]
            |> List.distinct
            |> List.sortBy (folderSort session layer.Id)

        folderKeys
        |> List.map (fun key ->
            let items =
                itemEntries
                |> List.choose (fun (itemFolderKey, item) -> if itemFolderKey = key then Some item else None)
                |> List.groupBy (fun item -> item.Id)
                |> List.map (fun (_, matching) -> matching.Head)

            {
                Id = folderKeyId key
                Name = folderName key
                Color = folderColor uiState key
                Items = items
            }
        )

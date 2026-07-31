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

module PropertyShelf =

    type private FolderKey =
        | SourceFolder of ProvenanceSourceRef
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
        | UnknownFolder -> PropertyFolders.unknownFolderId

    let private folderName =
        function
        | SourceFolder source -> source.Name
        | UnknownFolder -> "Unknown origin"

    let private folderColor (uiState: UiState) =
        function
        | SourceFolder source -> uiState.PropertyColors.SourceColors |> Map.tryFind source.Id
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
        let activeLayer =
            session.Layers |> Map.tryFind activeLayerId

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
        | UnknownFolder, _ -> 2, Int32.MaxValue, folderName key

    let private manualColor (uiState: UiState) (property: GroupingKey) =
        uiState.PropertyColors.ManualPropertyColors |> Map.tryFind property

    let private isPlacedInCurrentLayer (layer: ProvenanceLayer) (uiState: UiState) (property: GroupingKey) =
        let placedInRail = uiState.PropertyRailPlacements |> Map.containsKey (layer.Id, property)

        let groupedOnSide sideId =
            (State.Sides.get sideId uiState).GroupingAssignments
            |> List.exists (fun assignment -> assignment.Key = property)

        placedInRail
        || groupedOnSide (layer.Id, ProvenanceSide.Input)
        || groupedOnSide (layer.Id, ProvenanceSide.Output)

    let private itemForHeader
        (sourceSide: ProvenanceSide)
        (folderKey: FolderKey)
        (inputProjection: PropertyRails.RailProjection)
        (outputProjection: PropertyRails.RailProjection)
        (uiState: UiState)
        (property: GroupingKey)
        : FolderedDraggableItem<PropertyShelfItemPayload> =
        let badge =
            outputProjection.BadgeByHeader
            |> Map.tryFind property
            |> Option.orElseWith (fun () -> inputProjection.BadgeByHeader |> Map.tryFind property)
            |> badgeText

        let tooltip = folderName folderKey

        {
            Id = $"{folderKeyId folderKey}-{headerId property}"
            Label = property.Header.Name
            Payload = {
                Property = property
                SourceSide = sourceSide
            }
            Color = manualColor uiState property
            Badge = badge
            Tooltip =
                if String.IsNullOrWhiteSpace tooltip then
                    None
                else
                    Some tooltip
            Disabled = false
        }

    let private dedupeItems (items: FolderedDraggableItem<PropertyShelfItemPayload> list) =
        items
        |> List.groupBy (fun item -> item.Id)
        |> List.map (fun (_, matching) -> matching.Head)

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

        let headers =
            [
                yield! inputProjection.Headers
                yield! outputProjection.Headers
            ]
            |> List.distinct
            |> List.filter (isPlacedInCurrentLayer layer uiState >> not)

        let itemEntries =
            headers
            |> List.collect (fun property ->
                let sourceSide =
                    sourceSideForHeader layer.Id inputProjection outputProjection uiState property

                let sourceIds = sourcesForHeader inputProjection outputProjection property

                let folderKeys =
                    if sourceIds.IsEmpty then
                        [ UnknownFolder ]
                    else
                        sourceIds
                        |> Set.toList
                        |> List.choose (fun sourceId ->
                            sourceRefById |> Map.tryFind sourceId |> Option.map SourceFolder
                        )
                        |> (fun keys -> if keys.IsEmpty then [ UnknownFolder ] else keys)

                folderKeys
                |> List.map (fun folderKey ->
                    folderKey,
                    itemForHeader sourceSide folderKey inputProjection outputProjection uiState property
                )
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
                |> dedupeItems

            {
                Id = folderKeyId key
                Name = folderName key
                Color = folderColor uiState key
                Items = items
            }
        )

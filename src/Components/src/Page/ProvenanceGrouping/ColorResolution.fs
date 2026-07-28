module Swate.Components.Page.ProvenanceGrouping.ColorResolution

open Swate.Components.Page.ProvenanceGrouping.Identifiers
open Swate.Components.Page.ProvenanceGrouping.Values
open Swate.Components.Page.ProvenanceGrouping.Domain
open Swate.Components.Page.ProvenanceGrouping.ProjectionTypes

type PropertyColorKey = {
    Kind: AnnotationOwnerKind
    Header: ProvenanceTerm
}

type ColorSettings = {
    Palette: ProvenanceColor array
    SourceColors: Map<ProvenanceSourceId, ProvenanceColor>
    SourceColorSetOrder: Map<ProvenanceSourceId, int>
    ManualPropertyColors: Map<PropertyColorKey, ProvenanceColor>
}

let defaultColor: ProvenanceColor = "#64748b"

let resolveColor
    (settings: ColorSettings)
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
        |> Seq.sortByDescending (fun (setOrder, sourceId, _) -> setOrder, sourceId)
        |> Seq.tryHead
        |> Option.map (fun (_, _, color) -> color)
        |> Option.defaultValue defaultColor

let private appearanceSourceIds nodeId (session: ProvenanceSession) =
    session.Layers
    |> Map.toSeq
    |> Seq.choose (fun (_, layer) ->
        if
            layer.InputEndpoints.ContainsKey nodeId
            || layer.OutputEndpoints.ContainsKey nodeId
        then
            Some layer.Source.Id
        else
            None
    )
    |> Set.ofSeq

let private processSourceIds processId (session: ProvenanceSession) =
    session.Processes
    |> Map.tryFind processId
    |> Option.bind (fun structuralProcess -> session.Layers |> Map.tryFind structuralProcess.OriginLayerId)
    |> Option.map (fun layer -> Set.singleton layer.Source.Id)
    |> Option.defaultValue Set.empty

let originSourceIdsForAnnotation (session: ProvenanceSession) (annotation: ProjectedAnnotation) =
    match annotation.Backing with
    | NodeAssignmentBacking(_, ownerId, _) -> appearanceSourceIds ownerId session
    | ProcessAssignmentBacking(_, ownerId, _, _, _) -> processSourceIds ownerId session

let originSourceIdsForGroupingValue
    (session: ProvenanceSession)
    (key: GroupingValueKey)
    (annotations: ProjectedAnnotation list)
    =
    annotations
    |> List.filter (fun annotation -> annotation.Key = key)
    |> List.fold
        (fun sourceIds annotation -> Set.union sourceIds (originSourceIdsForAnnotation session annotation))
        Set.empty

let originSourceIdsForShelfEntry (session: ProvenanceSession) (entry: PropertyShelfEntry) =
    match entry.Payload with
    | CatalogBacked _ -> Set.empty
    | AssignmentBacked payload ->
        match payload.Backing with
        | NodeAssignmentBacking(_, ownerId, _) -> appearanceSourceIds ownerId session
        | ProcessAssignmentBacking(_, ownerId, _, _, _) -> processSourceIds ownerId session

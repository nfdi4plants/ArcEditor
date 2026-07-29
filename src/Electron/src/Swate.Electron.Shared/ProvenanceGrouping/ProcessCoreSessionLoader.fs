/// Host-facing loading surface for the provenance table editor: resolves
/// ProcessCore entities to table locations, converts them into one
/// multi-layer session, and checks whether a loaded session still matches
/// the ARC graph it came from. Lives in this assembly so it can use the
/// internal graph helpers; hosts (the Electron renderer) only ever need
/// this module plus `ProcessCoreWriteback.writeBackMany`.
module Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreSessionLoader

open ProcessCore
open Swate.Components.Page.ProvenanceGrouping.ProvenanceTypes
open Swate.Components.Page.ProvenanceGrouping.Session
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreAdapterTypes
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreConverter
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreGraph

/// One loaded editor session with everything a host needs to write it back
/// (`Indices`) and to reload it after a save or an external ARC change
/// (`Locations`).
type LoadedProvenanceSession = {
    Session: ProvenanceSession
    Indices: Map<ProvenanceSourceId, ProcessCoreWritebackIndex>
    Warnings: ProcessCoreConversionWarning list
    Locations: ProcessCoreTableLocation list
}

/// The table location of the process group a process belongs to, or `None`
/// when the process is not part of the ARC's dataset tree.
let tryLocationForProcess (proc: Process) (arc: ARC) : ProcessCoreTableLocation option =
    datasetEntries arc
    |> List.tryFind (fun entry ->
        entry.Dataset.Processes
        |> Seq.exists (fun candidate -> obj.ReferenceEquals(candidate, proc))
    )
    |> Option.map (fun entry -> {
        DatasetPath = entry.Path
        TableName = proc.Name
    })

/// One table location per distinct process-group name in the dataset, in
/// first-occurrence order - the dataset-open flow loads each group as its
/// own layer.
let locationsForDataset (dataset: Dataset) (arc: ARC) : ProcessCoreTableLocation list =
    match tryDatasetPath dataset arc with
    | None -> []
    | Some path ->
        dataset.Processes
        |> Seq.map (fun proc -> proc.Name)
        |> Seq.distinct
        |> Seq.map (fun name -> { DatasetPath = path; TableName = name })
        |> Seq.toList

/// Converts every location and builds one session with one layer per table.
let load
    (locations: ProcessCoreTableLocation list)
    (arc: ARC)
    : Result<LoadedProvenanceSession, ProcessCoreConversionError list> =
    if locations.IsEmpty then
        invalidArg (nameof locations) "load requires at least one table location."

    let results = locations |> List.map (fun location -> fromArc location arc)

    let errors =
        results
        |> List.collect (
            function
            | Error error -> [ error ]
            | Ok _ -> []
        )

    if not errors.IsEmpty then
        Error(errors |> List.distinct)
    else
        let converted =
            results
            |> List.choose (
                function
                | Ok result -> Some result
                | Error _ -> None
            )

        Ok {
            Session = Session.initMany (converted |> List.map (fun result -> result.Model))
            Indices =
                converted
                |> List.map (fun result -> result.Model.Source.Id, result.Index)
                |> Map.ofList
            Warnings = converted |> List.collect (fun result -> result.Warnings)
            Locations = locations
        }

/// True while the loaded session still matches the ARC graph it was
/// converted from; false once the ARC changed underneath it, in which case
/// writeback would fail its stale-graph check and the host should offer a
/// reload instead.
let isCurrent (loaded: LoadedProvenanceSession) (arc: ARC) : bool =
    let fingerprint = graphFingerprint arc

    loaded.Indices |> Map.forall (fun _ index -> index.ArcFingerprint = fingerprint)

/// One canonical editor session with the single multi-location index and
/// reference catalog produced by canonical conversion.
type LoadedCanonicalSession = {
    Session: Swate.Components.Page.ProvenanceGrouping.ProjectionTypes.ProvenanceSession
    Index: ProcessCoreCanonicalIndex
    ReferenceCatalog: Swate.Components.Page.ProvenanceGrouping.Domain.ReferenceCatalog
    Warnings: ProcessCoreCanonicalWarning list
    Locations: ProcessCoreProcessGroupLocation list
}

/// The canonical process-group location of a process, or `None` when the
/// exact process object is not part of the ARC's dataset tree.
let tryCanonicalLocationForProcess (proc: Process) (arc: ARC) : ProcessCoreProcessGroupLocation option =
    datasetEntries arc
    |> List.tryFind (fun entry ->
        entry.Dataset.Processes
        |> Seq.exists (fun candidate -> obj.ReferenceEquals(candidate, proc))
    )
    |> Option.map (fun entry -> {
        DatasetPath = entry.Path
        ProcessGroupName = proc.Name
    })

/// One canonical location per distinct process-group name in the dataset, in
/// first-occurrence order.
let canonicalLocationsForDataset (dataset: Dataset) (arc: ARC) : ProcessCoreProcessGroupLocation list =
    match tryDatasetPath dataset arc with
    | None -> []
    | Some path ->
        dataset.Processes
        |> Seq.map (fun proc -> proc.Name)
        |> Seq.distinct
        |> Seq.map (fun name -> {
            DatasetPath = path
            ProcessGroupName = name
        })
        |> Seq.toList

/// Converts all selected process groups directly into one canonical session
/// and its one multi-location index.
let loadCanonical
    (locations: ProcessCoreProcessGroupLocation list)
    (arc: ARC)
    : Result<LoadedCanonicalSession, ProcessCoreCanonicalConversionError list> =
    if locations.IsEmpty then
        invalidArg (nameof locations) "loadCanonical requires at least one process-group location."

    fromArcMany locations arc
    |> Result.map (fun converted -> {
        Session = converted.Session
        Index = converted.Index
        ReferenceCatalog = converted.ReferenceCatalog
        Warnings = converted.Warnings
        Locations = converted.Locations
    })

/// True while the canonical session's single captured graph fingerprint
/// matches the current ARC.
let isCanonicalCurrent (loaded: LoadedCanonicalSession) (arc: ARC) : bool =
    loaded.Index.ArcFingerprint = graphFingerprint arc

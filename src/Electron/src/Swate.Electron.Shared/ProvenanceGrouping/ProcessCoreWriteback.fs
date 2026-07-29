module Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreWriteback

open ProcessCore
open Swate.Components.Page.ProvenanceGrouping.ProvenanceTypes
open Swate.Components.Page.ProvenanceGrouping.Edit
open Swate.Components.Page.ProvenanceGrouping.Session
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreAdapterTypes
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreGraph

type private ExistingAnnotationUpdate = {
    PropertyValueId: ProvenancePropertyValueId
    Annotations: Annotation list
    Value: ProvenanceValue
    Unit: ProvenanceTerm option
}

/// One materialized node reference, resolved and validated during preflight
/// so `apply` only ever touches already-validated `IONode` instances.
type private PlannedNode = {
    SetId: ProvenanceSetId
    Header: ProvenanceIOHeader
    Node: IONode
}

/// One internal mutation command for a direct ProcessCore process row.
/// Structure is never derived from a tabular/scaffold representation.
/// `ConnectionId` is set only for rows that represent a final connection, so
/// property placement can later map a connection target to the exact
/// process that materializes it.
type private PlannedRow = {
    Input: PlannedNode option
    Output: PlannedNode option
    ConnectionId: ProvenanceConnectionId option
}

/// One property value resolved to its final (post-edit) value/unit and the
/// exact editor target that must receive it.
type private PropertyPlacement = {
    Target: ProvenancePropertyTarget
    Header: ProvenancePropertyHeader
    Value: ProvenanceValue
    Unit: ProvenanceTerm option
}

type private PropertyMutationOwner =
    | NodeOwner of IONode
    | ProcessParameterOwner of Process

type private PropertyMutation = {
    Owner: PropertyMutationOwner
    Annotation: Annotation
}

/// One session-created layer's materialization: every row for its final
/// sets/connections, plus an empty-process sentinel row (`Input = Output =
/// None`) when the layer has neither.
type private NewLayerPlan = {
    LayerName: string
    Rows: PlannedRow list
}

/// One loaded table's structural materialization: process replacements and
/// new rows target this table's dataset under this table's process name.
type private TablePlan = {
    Dataset: Dataset option
    LoadedTableName: string
    ReplacedProcesses: (Process * PlannedRow list) list
    NewRows: PlannedRow list
}

type private Plan = {
    Updates: ExistingAnnotationUpdate list
    Tables: TablePlan list
    /// Session-created layers materialize once, into the dataset of the last
    /// loaded table in `LayerOrder` - the chain end that `addLayer` extends.
    NewLayersDataset: Dataset option
    NewLayers: NewLayerPlan list
    PropertyMutations: PropertyMutation list
    DeferredPropertyPlacements: PropertyPlacement list
}

let private anchorOfOrigin =
    function
    | ProvenancePropertyOrigin.Real anchor
    | ProvenancePropertyOrigin.Virtual anchor -> anchor

let private findPropertyValue (session: ProvenanceSession) (propertyValueId: ProvenancePropertyValueId) =
    session.Layers
    |> List.tryPick (fun layer -> layer.Model.PropertyValues.TryFind propertyValueId)

/// Finds the source ID for a table name that belongs to neither the initial
/// layer nor any session-created layer - i.e. previous/upstream context -
/// by locating any property value whose real origin anchor names that table.
let private findPreviousSourceId (session: ProvenanceSession) (tableName: string) : ProvenanceSourceId option =
    session.Layers
    |> List.tryPick (fun layer ->
        layer.Model.PropertyValues
        |> Map.toList
        |> List.tryPick (fun (_, value) ->
            let anchor = anchorOfOrigin value.Origin

            if anchor.Source.Name = tableName then
                Some anchor.Source.Id
            else
                None
        )
    )

let private collectErrors
    (results: Result<'a, ProcessCoreWritebackError list> list)
    : Result<'a list, ProcessCoreWritebackError list> =
    let errors =
        results
        |> List.collect (
            function
            | Error e -> e
            | Ok _ -> []
        )

    if not errors.IsEmpty then
        Error(errors |> List.distinct)
    else
        Ok(
            results
            |> List.choose (
                function
                | Ok v -> Some v
                | Error _ -> None
            )
        )

let private validateGraph (index: ProcessCoreWritebackIndex) (arc: ARC) : ProcessCoreWritebackError list =
    if graphFingerprint arc <> index.ArcFingerprint then
        [ ProcessCoreWritebackError.StaleArc ]
    else
        []

let private validateLayers
    (indices: ProcessCoreWritebackIndex list)
    (session: ProvenanceSession)
    : ProcessCoreWritebackError list =
    let layerIds = session.Layers |> List.map (fun layer -> layer.Id) |> List.sort
    let orderIds = session.LayerOrder |> List.sort

    let loadedNames =
        indices
        |> List.choose (fun index ->
            session.Layers
            |> List.tryFind (fun layer -> layer.Model.Source.Id = index.InitialSourceId)
            |> Option.map (fun layer -> layer.Model.Source.Name)
        )

    [
        for index in indices do
            if
                session.Layers
                |> List.exists (fun layer -> layer.Model.Source.Id = index.InitialSourceId)
                |> not
            then
                yield ProcessCoreWritebackError.InitialLayerNotFound index.InitialSourceId
        // Structural patches route by table name, so two loaded layers may
        // not share one - a name collision would make routing ambiguous.
        for name in loadedNames |> List.countBy id |> List.filter (snd >> (<) 1) |> List.map fst do
            yield ProcessCoreWritebackError.DuplicateLayerName name
        if
            layerIds <> orderIds
            || (session.LayerOrder |> List.distinct |> List.length)
               <> session.LayerOrder.Length
        then
            yield ProcessCoreWritebackError.InvalidLayerOrder session.LayerOrder
    ]

/// All location maps of every loaded table's index, merged into one lookup
/// view. Safe because every set/connection/property id is prefixed with its
/// loaded table's source id, so ids never collide across indices. The
/// carried `LoadedTable`/`InitialSourceId` are those of the first index and
/// must not be read through the merged view.
let private mergeIndices (indices: ProcessCoreWritebackIndex list) : ProcessCoreWritebackIndex =
    match indices with
    | [] -> invalidArg (nameof indices) "mergeIndices requires at least one index."
    | [ single ] -> single
    | primary :: _ ->
        let mergeMaps maps =
            maps
            |> List.fold (fun merged map -> Map.fold (fun acc key value -> Map.add key value acc) merged map) Map.empty

        {
            primary with
                EndpointLocations = indices |> List.map (fun index -> index.EndpointLocations) |> mergeMaps
                PropertyValueLocations = indices |> List.map (fun index -> index.PropertyValueLocations) |> mergeMaps
                ConnectionLocations = indices |> List.map (fun index -> index.ConnectionLocations) |> mergeMaps
        }

/// Resolves one `UpdatePropertyValue` patch. A property absent from the
/// conversion index but present in the final session is editor-created
/// (`Virtual`) in this session; its value update is absorbed here because
/// its owning `AddLoadedPropertyValue` materialization always writes the
/// property's final session value, not the add-patch payload.
let private resolveUpdatePatch
    (index: ProcessCoreWritebackIndex)
    (session: ProvenanceSession)
    (arc: ARC)
    (propertyValueId: ProvenancePropertyValueId)
    (patchAnchor: ProvenanceWritebackAnchor)
    : Result<ExistingAnnotationUpdate option, ProcessCoreWritebackError list> =
    match index.PropertyValueLocations.TryFind propertyValueId with
    | None ->
        match findPropertyValue session propertyValueId with
        | None ->
            Error [
                ProcessCoreWritebackError.PropertyNotFound propertyValueId
            ]
        | Some _ -> Ok None
    | Some locations ->
        match findPropertyValue session propertyValueId with
        | None ->
            Error [
                ProcessCoreWritebackError.PropertyNotFound propertyValueId
            ]
        | Some _ when
            locations
            |> List.exists (fun location ->
                match location.Owner with
                | ProcessCoreAnnotationOwner.RecipeComponent _ -> true
                | _ -> false
            )
            ->
            Error [
                ProcessCoreWritebackError.ReadOnlyRecipeComponentMutation
            ]
        | Some finalValue ->
            let finalAnchor = anchorOfOrigin finalValue.Origin

            if finalAnchor.Source.Id <> patchAnchor.Source.Id then
                Error [
                    ProcessCoreWritebackError.SourceLocationNotFound propertyValueId
                ]
            else
                let resolutions =
                    locations
                    |> List.map (fun location ->
                        match tryResolveAnnotation location arc with
                        | Some annotation when annotationFingerprint annotation = location.Fingerprint -> Ok annotation
                        | Some _ -> Error(ProcessCoreWritebackError.SourceLocationNotFound propertyValueId)
                        | None -> Error(ProcessCoreWritebackError.SourceLocationNotFound propertyValueId)
                    )

                let errors =
                    resolutions
                    |> List.choose (
                        function
                        | Error e -> Some e
                        | Ok _ -> None
                    )

                if not errors.IsEmpty then
                    Error(errors |> List.distinct)
                else
                    let annotations =
                        resolutions
                        |> List.choose (
                            function
                            | Ok a -> Some a
                            | Error _ -> None
                        )

                    Ok(
                        Some {
                            PropertyValueId = propertyValueId
                            Annotations = annotations
                            Value = finalValue.Value
                            Unit = finalValue.Unit
                        }
                    )

let private processLocationKey (location: ProcessCoreProcessLocation) =
    let path = String.concat "/" location.DatasetPath
    $"{path}:{location.ProcessIndex}"

let private processLocationKeyOfConnection (index: ProcessCoreWritebackIndex) (connectionId: ProvenanceConnectionId) =
    processLocationKey index.ConnectionLocations.[connectionId].Process

/// Resolves the `IONode` for one set. `nodeFromSet` always constructs a
/// fresh object; if a node with the same canonical key already exists
/// anywhere in the ARC, `SetInput`/`SetOutput` silently swaps in that
/// existing object during apply (ProcessCore's own registry
/// canonicalization), which would orphan any annotation already written to
/// the fresh preflight-time reference. Resolving the real existing object
/// up front keeps the reference in the plan identical to the one that ends
/// up linked into the graph.
let private resolvePlannedNode
    (arc: ARC)
    (sets: Map<ProvenanceSetId, ProvenanceSet>)
    (setId: ProvenanceSetId)
    : Result<PlannedNode, ProcessCoreWritebackError list> =
    match sets.TryFind setId with
    | None -> Error [ ProcessCoreWritebackError.SetNotFound setId ]
    | Some set ->
        match nodeFromSet set with
        | Error e -> Error [ e ]
        | Ok freshNode ->
            let node =
                tryResolveNode (nodeLocation freshNode) arc |> Option.defaultValue freshNode

            Ok {
                SetId = setId
                Header = set.Header
                Node = node
            }

/// AddLoadedSet/AddLoadedConnection patches for the loaded table. Every
/// final connection materializes exactly one two-sided row; every final
/// added set not represented by a connection materializes one one-sided
/// row. A connection added and later removed is consumed without a row -
/// its endpoint set(s) fall back to the one-sided rule.
let private planAdditions
    (arc: ARC)
    (index: ProcessCoreWritebackIndex)
    (finalInputSets: Map<ProvenanceSetId, ProvenanceSet>)
    (finalOutputSets: Map<ProvenanceSetId, ProvenanceSet>)
    (finalConnections: Map<ProvenanceConnectionId, ProvenanceConnection>)
    (addSetPatches: (ProvenanceSide * ProvenanceIOHeader * string) list)
    (addConnectionPatches: (ProvenanceSetId * ProvenanceSetId) list)
    : Result<PlannedRow list, ProcessCoreWritebackError list> =

    let addedSetIds (sets: Map<ProvenanceSetId, ProvenanceSet>) =
        sets
        |> Map.toList
        |> List.map fst
        |> List.filter (fun id -> not (index.EndpointLocations.ContainsKey id))
        |> Set.ofList

    let addedInputSetIds = addedSetIds finalInputSets
    let addedOutputSetIds = addedSetIds finalOutputSets

    let isConnectedInFinal setId =
        finalConnections
        |> Map.exists (fun _ connection -> connection.InputSetId = setId || connection.OutputSetId = setId)

    let claimedConnectionIds =
        System.Collections.Generic.HashSet<ProvenanceConnectionId>()

    let connectionResults =
        addConnectionPatches
        |> List.choose (fun (inputSetId, outputSetId) ->
            finalConnections
            |> Map.toList
            |> List.tryFind (fun (connectionId, connection) ->
                not (claimedConnectionIds.Contains connectionId)
                && connection.InputSetId = inputSetId
                && connection.OutputSetId = outputSetId
            )
            |> Option.map (fun (connectionId, connection) ->
                claimedConnectionIds.Add connectionId |> ignore

                match
                    resolvePlannedNode arc finalInputSets connection.InputSetId,
                    resolvePlannedNode arc finalOutputSets connection.OutputSetId
                with
                | Ok inputNode, Ok outputNode ->
                    Ok(
                        Some {
                            Input = Some inputNode
                            Output = Some outputNode
                            ConnectionId = Some connectionId
                        }
                    )
                | Error e, _
                | _, Error e -> Error e
            )
        )

    let claimedSetIds = System.Collections.Generic.HashSet<ProvenanceSetId>()

    let setResults =
        addSetPatches
        |> List.choose (fun (side, header, name) ->
            let candidates, sets =
                match side with
                | ProvenanceSide.Input -> addedInputSetIds, finalInputSets
                | ProvenanceSide.Output -> addedOutputSetIds, finalOutputSets

            candidates
            |> Set.toList
            |> List.tryFind (fun id ->
                not (claimedSetIds.Contains id)
                && sets.[id].Header = header
                && sets.[id].Name = name
            )
            |> Option.map (fun id ->
                claimedSetIds.Add id |> ignore

                if isConnectedInFinal id then
                    Ok None
                else
                    resolvePlannedNode arc sets id
                    |> Result.map (fun node ->
                        Some(
                            match side with
                            | ProvenanceSide.Input -> {
                                Input = Some node
                                Output = None
                                ConnectionId = None
                              }
                            | ProvenanceSide.Output -> {
                                Input = None
                                Output = Some node
                                ConnectionId = None
                              }
                        )
                    )
            )
        )

    let allResults = connectionResults @ setResults

    let errors =
        allResults
        |> List.collect (
            function
            | Error e -> e
            | Ok _ -> []
        )

    if not errors.IsEmpty then
        Error(errors |> List.distinct)
    else
        Ok(
            allResults
            |> List.choose (
                function
                | Ok row -> row
                | Error _ -> None
            )
        )

/// RemoveLoadedConnection patches for the loaded table. Groups removals by
/// their indexed original process, replays every indexed connection from
/// that process against the final model, and plans one exact row per
/// surviving edge plus one-sided rows for endpoints left disconnected
/// everywhere in the final model. A removal matching no indexed connection
/// location is an editor-created connection and is consumed as a no-op.
let private planRemovals
    (arc: ARC)
    (index: ProcessCoreWritebackIndex)
    (finalInputSets: Map<ProvenanceSetId, ProvenanceSet>)
    (finalOutputSets: Map<ProvenanceSetId, ProvenanceSet>)
    (finalConnections: Map<ProvenanceConnectionId, ProvenanceConnection>)
    (removalPairs: (ProvenanceSetId * ProvenanceSetId) list)
    : Result<(Process * PlannedRow list) list, ProcessCoreWritebackError list> =

    let matchedConnectionIds =
        removalPairs
        |> List.choose (fun (inputSetId, outputSetId) ->
            index.ConnectionLocations
            |> Map.toList
            |> List.tryFind (fun (_, location) ->
                location.InputSetId = inputSetId && location.OutputSetId = outputSetId
            )
            |> Option.map fst
        )
        |> List.distinct

    if matchedConnectionIds.IsEmpty then
        Ok []
    else
        let byProcess =
            matchedConnectionIds |> List.groupBy (processLocationKeyOfConnection index)

        let results =
            byProcess
            |> List.map (fun (processKey, connectionIdsForProcess) ->
                let anyLocation = index.ConnectionLocations.[connectionIdsForProcess.Head]
                let procLocation = anyLocation.Process

                match tryResolveProcess procLocation arc with
                | None ->
                    Error [
                        ProcessCoreWritebackError.SourceLocationNotFound processKey
                    ]
                | Some originalProcess ->
                    let allForProcess =
                        index.ConnectionLocations
                        |> Map.toList
                        |> List.filter (fun (connectionId, _) ->
                            processLocationKeyOfConnection index connectionId = processKey
                        )
                        |> List.map fst

                    let surviving = allForProcess |> List.filter finalConnections.ContainsKey
                    let removed = allForProcess |> List.filter (finalConnections.ContainsKey >> not)

                    if removed.IsEmpty then
                        Ok None
                    else
                        let survivingRowResults =
                            surviving
                            |> List.map (fun connectionId ->
                                let connection = finalConnections.[connectionId]

                                match
                                    resolvePlannedNode arc finalInputSets connection.InputSetId,
                                    resolvePlannedNode arc finalOutputSets connection.OutputSetId
                                with
                                | Ok inputNode, Ok outputNode ->
                                    Ok {
                                        Input = Some inputNode
                                        Output = Some outputNode
                                        ConnectionId = Some connectionId
                                    }
                                | Error e, _
                                | _, Error e -> Error e
                            )

                        let disconnectedSetRefs =
                            removed
                            |> List.collect (fun connectionId ->
                                let location = index.ConnectionLocations.[connectionId]

                                [
                                    ProvenanceSide.Input, location.InputSetId
                                    ProvenanceSide.Output, location.OutputSetId
                                ]
                            )
                            |> List.distinct
                            |> List.filter (fun (_, setId) ->
                                not (
                                    finalConnections
                                    |> Map.exists (fun _ connection ->
                                        connection.InputSetId = setId || connection.OutputSetId = setId
                                    )
                                )
                            )

                        let oneSidedRowResults =
                            disconnectedSetRefs
                            |> List.map (fun (side, setId) ->
                                let sets =
                                    match side with
                                    | ProvenanceSide.Input -> finalInputSets
                                    | ProvenanceSide.Output -> finalOutputSets

                                resolvePlannedNode arc sets setId
                                |> Result.map (fun node ->
                                    match side with
                                    | ProvenanceSide.Input -> {
                                        Input = Some node
                                        Output = None
                                        ConnectionId = None
                                      }
                                    | ProvenanceSide.Output -> {
                                        Input = None
                                        Output = Some node
                                        ConnectionId = None
                                      }
                                )
                            )

                        let combined = survivingRowResults @ oneSidedRowResults

                        let errors =
                            combined
                            |> List.collect (
                                function
                                | Error e -> e
                                | Ok _ -> []
                            )

                        if not errors.IsEmpty then
                            Error(errors |> List.distinct)
                        else
                            Ok(
                                Some(
                                    originalProcess,
                                    combined
                                    |> List.choose (
                                        function
                                        | Ok row -> Some row
                                        | Error _ -> None
                                    )
                                )
                            )
            )

        let errors =
            results
            |> List.collect (
                function
                | Error e -> e
                | Ok _ -> []
            )

        if not errors.IsEmpty then
            Error(errors |> List.distinct)
        else
            Ok(
                results
                |> List.choose (
                    function
                    | Ok x -> x
                    | Error _ -> None
                )
            )

/// Indexed processes that materialize exactly one endpoint set and no
/// connection - the shape a saved disconnected endpoint leaves behind -
/// keyed by that set. Processes the conversion derived a connection from are
/// excluded, so this never overlaps the processes `planRemovals` replaces.
let private reusableOneSidedProcesses
    (index: ProcessCoreWritebackIndex)
    : Map<ProvenanceSetId, ProcessCoreProcessLocation> =
    let connectedProcessKeys =
        index.ConnectionLocations
        |> Map.toList
        |> List.map (fun (_, location) -> processLocationKey location.Process)
        |> Set.ofList

    let occurrences =
        index.EndpointLocations
        |> Map.toList
        |> List.collect (fun (setId, location) ->
            location.Occurrences |> List.map (fun occurrence -> occurrence.Process, setId)
        )

    let setsByProcessKey =
        occurrences
        |> List.groupBy (fun (procLocation, _) -> processLocationKey procLocation)
        |> List.map (fun (key, items) -> key, items |> List.map snd |> List.distinct)
        |> Map.ofList

    occurrences
    |> List.choose (fun (procLocation, setId) ->
        let key = processLocationKey procLocation

        if connectedProcessKeys.Contains key then
            None
        else
            match setsByProcessKey.TryFind key with
            | Some [ only ] when only = setId -> Some(setId, procLocation)
            | _ -> None
    )
    |> List.distinctBy fst
    |> Map.ofList

/// Retargets an added-connection row onto the disconnected process that
/// already materializes one of its endpoints. Saving a disconnected endpoint
/// writes a one-sided process; connecting that endpoint in a later session
/// would otherwise append a second process and leave the first behind as a
/// redundant disconnected row for the same node. Reusing it also keeps the
/// process's position and any annotations it carries.
let private supersedeOneSidedProcesses (arc: ARC) (index: ProcessCoreWritebackIndex) (table: TablePlan) : TablePlan =
    let reusable = reusableOneSidedProcesses index

    if reusable.IsEmpty then
        table
    else
        let claimedProcessKeys = System.Collections.Generic.HashSet<string>()

        let tryReuse (row: PlannedRow) =
            if row.ConnectionId.IsNone then
                None
            else
                [ row.Input; row.Output ]
                |> List.choose id
                |> List.tryPick (fun planned ->
                    reusable.TryFind planned.SetId
                    |> Option.filter (fun location -> not (claimedProcessKeys.Contains(processLocationKey location)))
                    |> Option.bind (fun location ->
                        tryResolveProcess location arc
                        |> Option.map (fun proc -> processLocationKey location, proc)
                    )
                )

        let remainingRows, replacements =
            table.NewRows
            |> List.fold
                (fun (rows, replacements) row ->
                    match tryReuse row with
                    | Some(processKey, proc) ->
                        claimedProcessKeys.Add processKey |> ignore
                        rows, replacements @ [ proc, [ row ] ]
                    | None -> rows @ [ row ], replacements
                )
                ([], [])

        {
            table with
                NewRows = remainingRows
                ReplacedProcesses = table.ReplacedProcesses @ replacements
        }

/// Every planned node materialization, across replacement and new rows,
/// grouped by ProcessCore node key. Two distinct editor sets that would
/// materialize to the same node with differing header identity are a
/// conflict; reuse through matching header identity remains valid.
let private validateNodeIdentity (rows: PlannedRow list) : ProcessCoreWritebackError list =
    let allNodes =
        rows |> List.collect (fun row -> [ row.Input; row.Output ] |> List.choose id)

    allNodes
    |> List.groupBy (fun planned -> planned.Node.Key())
    |> List.choose (fun (nodeKey, planned) ->
        // Distinct set IDs alone are not a conflict: a reference link legitimately
        // reuses one canonical node under two different editor set IDs across
        // layers. Only differing header identity (kind or text) is a genuine
        // conflict between two distinct editor sets.
        let distinctHeaders =
            planned |> List.map (fun p -> p.Header.Kind.Id, p.Header.Text) |> List.distinct

        if distinctHeaders.Length > 1 then
            let distinctSetIds = planned |> List.map (fun p -> p.SetId) |> List.distinct
            Some(ProcessCoreWritebackError.ConflictingNodeIdentity(nodeKey, distinctSetIds))
        else
            None
    )

let private addSetPatchesFor (tableName: string) (patchLog: ProvenanceTablePatch list) =
    patchLog
    |> List.choose (
        function
        | ProvenanceTablePatch.AddLoadedSet(side, patchTableName, header, name) when patchTableName = tableName ->
            Some(side, header, name)
        | _ -> None
    )

let private addConnectionPatchesFor (tableName: string) (patchLog: ProvenanceTablePatch list) =
    patchLog
    |> List.choose (
        function
        | ProvenanceTablePatch.AddLoadedConnection(patchTableName, _, _, inputSetId, outputSetId) when
            patchTableName = tableName
            ->
            Some(inputSetId, outputSetId)
        | _ -> None
    )

let private removeConnectionPatchesFor (tableName: string) (patchLog: ProvenanceTablePatch list) =
    patchLog
    |> List.choose (
        function
        | ProvenanceTablePatch.RemoveLoadedConnection(patchTableName, _, _, inputSetId, outputSetId) when
            patchTableName = tableName
            ->
            Some(inputSetId, outputSetId)
        | _ -> None
    )

// ── Property placement (AddLoadedPropertyValue) ─────────────────────────────

let private parseOrdinal (prefix: string) (id: string) : Result<int, ProcessCoreWritebackError> =
    if not (id.StartsWith(prefix, System.StringComparison.Ordinal)) then
        Error(ProcessCoreWritebackError.GeneratedIdFormatChanged(id, prefix))
    else
        let suffix = id.Substring(prefix.Length)

        match
            System.Int32.TryParse(
                suffix,
                System.Globalization.NumberStyles.None,
                System.Globalization.CultureInfo.InvariantCulture
            )
        with
        | true, ordinal when ordinal >= 0 -> Ok ordinal
        | _ -> Error(ProcessCoreWritebackError.GeneratedIdFormatChanged(id, prefix))

let private resolvedTargetSetIds (layerModel: ProvenanceModel) target =
    match target with
    | ProvenancePropertyTarget.InputSets ids -> ids |> List.sort |> List.distinct
    | ProvenancePropertyTarget.OutputSets ids -> ids |> List.sort |> List.distinct
    | ProvenancePropertyTarget.Connections connectionIds ->
        connectionIds
        |> List.collect (fun id ->
            match layerModel.Connections.TryFind id with
            | Some connection -> [ connection.InputSetId; connection.OutputSetId ]
            | None -> []
        )
        |> List.sort
        |> List.distinct

/// Pairs `AddLoadedPropertyValue` patches to layer-owned `Virtual` final
/// values by replaying the patch log against candidates grouped by
/// (header, resolved target), claiming ascending numeric ID ordinals so
/// duplicate-value adds under one shared header/target never collide.
let private resolveAddPropertyPatches
    (initialLayer: ProvenanceLayer)
    (addPatches: (ProvenancePropertyTarget * ProvenancePropertyHeader) list)
    : Result<PropertyPlacement list, ProcessCoreWritebackError list> =

    let ownVirtual =
        initialLayer.Model.PropertyValues
        |> Map.toList
        |> List.choose (fun (id, value) ->
            match value.Origin with
            | ProvenancePropertyOrigin.Virtual anchor when anchor.Source.Id = initialLayer.Model.Source.Id ->
                Some(id, value)
            | _ -> None
        )

    let attachedSetIds id =
        let ownIds (sets: Map<ProvenanceSetId, ProvenanceSet>) =
            sets
            |> Map.toList
            |> List.filter (fun (_, set) -> set.PropertyValueIds |> List.contains id)
            |> List.map fst

        (ownIds initialLayer.Model.InputSets @ ownIds initialLayer.Model.OutputSets)
        |> List.sort
        |> List.distinct

    let prefix = $"{initialLayer.Model.Source.Id}::property-value-"
    let mutable ordinalErrors: ProcessCoreWritebackError list = []

    let candidatesByKey =
        ownVirtual
        |> List.choose (fun (id, value) ->
            match parseOrdinal prefix id with
            | Ok ordinal -> Some((value.Header, attachedSetIds id), (ordinal, id))
            | Error e ->
                ordinalErrors <- e :: ordinalErrors
                None
        )
        |> List.groupBy fst
        |> List.map (fun (key, items) -> key, items |> List.map snd |> List.sortBy fst |> List.map snd)
        |> Map.ofList

    if not ordinalErrors.IsEmpty then
        Error(ordinalErrors |> List.distinct)
    else
        let claimed = System.Collections.Generic.HashSet<ProvenancePropertyValueId>()

        let results =
            addPatches
            |> List.map (fun (target, header) ->
                let key = header, resolvedTargetSetIds initialLayer.Model target

                match candidatesByKey.TryFind key with
                | None ->
                    Error [
                        ProcessCoreWritebackError.PropertyNotFound(sprintf "%A" key)
                    ]
                | Some candidates ->
                    let chosen =
                        candidates
                        |> List.tryFind (fun id -> not (claimed.Contains id))
                        |> Option.orElse (List.tryHead candidates)

                    match chosen with
                    | None ->
                        Error [
                            ProcessCoreWritebackError.PropertyNotFound(sprintf "%A" key)
                        ]
                    | Some id ->
                        claimed.Add id |> ignore

                        match initialLayer.Model.PropertyValues.TryFind id with
                        | Some finalValue ->
                            Ok {
                                Target = target
                                Header = header
                                Value = finalValue.Value
                                Unit = finalValue.Unit
                            }
                        | None -> Error [ ProcessCoreWritebackError.PropertyNotFound id ]
            )

        collectErrors results

let private additionalTypeForKind (kind: ProvenanceKind) =
    if kind.Id = ProcessCoreKinds.characteristic.Id then
        Some "CharacteristicValue"
    elif kind.Id = ProcessCoreKinds.factor.Id then
        Some "FactorValue"
    elif kind.Id = ProcessCoreKinds.parameter.Id then
        Some "ParameterValue"
    elif kind.Id = ProcessCoreKinds.componentKind.Id then
        Some "Component"
    else
        None

let private resolveNodeForSet
    (structuralNodesById: Map<ProvenanceSetId, IONode>)
    (arc: ARC)
    (index: ProcessCoreWritebackIndex)
    (setId: ProvenanceSetId)
    : Result<IONode, ProcessCoreWritebackError list> =
    match structuralNodesById.TryFind setId with
    | Some node -> Ok node
    | None ->
        match index.EndpointLocations.TryFind setId with
        | Some location ->
            match location.Occurrences with
            | occurrence :: _ ->
                match tryResolveNode occurrence.Node arc with
                | Some node -> Ok node
                | None -> Error [ ProcessCoreWritebackError.SetNotFound setId ]
            | [] -> Error [ ProcessCoreWritebackError.SetNotFound setId ]
        | None -> Error [ ProcessCoreWritebackError.SetNotFound setId ]

/// `None` when the connection is genuinely new in this session (its process
/// does not exist until structural apply runs); such placements are
/// deferred and applied without a collision check, since a brand-new
/// process/recipe can never collide with anything.
let private resolveExistingConnectionProcess
    (arc: ARC)
    (index: ProcessCoreWritebackIndex)
    (connectionId: ProvenanceConnectionId)
    : Result<Process, ProcessCoreWritebackError list> option =
    match index.ConnectionLocations.TryFind connectionId with
    | None -> None
    | Some location ->
        match tryResolveProcess location.Process arc with
        | Some proc -> Some(Ok proc)
        | None ->
            Some(
                Error [
                    ProcessCoreWritebackError.ConnectionNotFound connectionId
                ]
            )

/// Splits connection-targeted parameter/component placements per connection:
/// ids whose process already exists are validated and planned immediately,
/// while ids whose process only materializes during structural apply are
/// deferred (a brand-new process/recipe can never collide with anything).
/// Splitting per id keeps a mixed existing+created target from bypassing
/// validation - and delivery - on its existing-connection part.
let private splitPlacements
    (index: ProcessCoreWritebackIndex)
    (placements: PropertyPlacement list)
    : PropertyPlacement list * PropertyPlacement list =
    let mutable immediate: PropertyPlacement list = []
    let mutable deferred: PropertyPlacement list = []

    for placement in placements do
        match placement.Target with
        | ProvenancePropertyTarget.Connections connectionIds when
            placement.Header.Kind.Id = ProcessCoreKinds.parameter.Id
            ->
            let existing, created =
                connectionIds |> List.partition index.ConnectionLocations.ContainsKey

            if not existing.IsEmpty then
                immediate <-
                    {
                        placement with
                            Target = ProvenancePropertyTarget.Connections existing
                    }
                    :: immediate

            if not created.IsEmpty then
                deferred <-
                    {
                        placement with
                            Target = ProvenancePropertyTarget.Connections created
                    }
                    :: deferred
        | _ -> immediate <- placement :: immediate

    List.rev immediate, List.rev deferred

let private resolveOwnersForPlacement
    (arc: ARC)
    (index: ProcessCoreWritebackIndex)
    (allConnections: Map<ProvenanceConnectionId, ProvenanceConnection>)
    (structuralNodesById: Map<ProvenanceSetId, IONode>)
    (placement: PropertyPlacement)
    : Result<PropertyMutationOwner list, ProcessCoreWritebackError list> =

    let resolveNode = resolveNodeForSet structuralNodesById arc index

    match placement.Target with
    | ProvenancePropertyTarget.InputSets ids
    | ProvenancePropertyTarget.OutputSets ids ->
        ids |> List.map resolveNode |> collectErrors |> Result.map (List.map NodeOwner)
    | ProvenancePropertyTarget.Connections connectionIds ->
        if placement.Header.Kind.Id = ProcessCoreKinds.parameter.Id then
            connectionIds
            |> List.map (fun id ->
                match resolveExistingConnectionProcess arc index id with
                | Some result -> result
                | None -> Error [ ProcessCoreWritebackError.ConnectionNotFound id ]
            )
            |> collectErrors
            |> Result.map (List.map ProcessParameterOwner)
        else
            connectionIds
            |> List.collect (fun id ->
                match allConnections.TryFind id with
                | Some connection -> [ connection.InputSetId; connection.OutputSetId ]
                | None -> []
            )
            |> List.distinct
            |> List.map resolveNode
            |> collectErrors
            |> Result.map (List.map NodeOwner)

let private existingAnnotations (owner: PropertyMutationOwner) : Annotation list =
    match owner with
    | NodeOwner node -> nodeAdditionalProperties node |> Seq.toList
    | ProcessParameterOwner proc -> proc.ParameterValue |> Seq.toList

let private ownerKey (owner: PropertyMutationOwner) =
    match owner with
    | NodeOwner node -> "node:" + node.Key()
    | ProcessParameterOwner proc ->
        "param:"
        + string (System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode proc)

let private narrowerMatch (existing: ProcessCoreAnnotationFingerprint) (requested: ProcessCoreAnnotationFingerprint) =
    existing.Name = requested.Name
    && existing.Value = requested.Value
    && existing.Unit = requested.Unit
    && existing.NameTAN = requested.NameTAN

/// Validates and plans every immediate (non-deferred) property placement
/// against a symbolic per-owner snapshot seeded from current annotations,
/// updated as each placement is planned so later placements in the same
/// batch see earlier ones. A full-fingerprint match is a genuine no-op; a
/// narrower ProcessCore-equality match with a different fingerprint is a
/// conflicting-identity error that leaves the plan (and graph) untouched.
let private planPropertyMutations
    (arc: ARC)
    (index: ProcessCoreWritebackIndex)
    (allConnections: Map<ProvenanceConnectionId, ProvenanceConnection>)
    (structuralNodesById: Map<ProvenanceSetId, IONode>)
    (placements: PropertyPlacement list)
    : Result<PropertyMutation list, ProcessCoreWritebackError list> =

    let pending =
        System.Collections.Generic.Dictionary<string, ResizeArray<ProcessCoreAnnotationFingerprint>>()

    let mutable errors: ProcessCoreWritebackError list = []
    let mutations = ResizeArray<PropertyMutation>()

    for placement in placements do
        match resolveOwnersForPlacement arc index allConnections structuralNodesById placement with
        | Error e -> errors <- errors @ e
        | Ok owners ->
            for owner in owners do
                let key = ownerKey owner

                if not (pending.ContainsKey key) then
                    pending.[key] <- ResizeArray(existingAnnotations owner |> List.map annotationFingerprint)

                let additionalType =
                    match owner with
                    | ProcessParameterOwner _ -> Some "ParameterValue"
                    | NodeOwner _ -> additionalTypeForKind placement.Header.Kind

                let annotation =
                    annotationFromValue additionalType placement.Header placement.Value placement.Unit

                let requested = annotationFingerprint annotation
                let list = pending.[key]

                if list |> Seq.exists ((=) requested) then
                    ()
                else
                    match owner, list |> Seq.tryFind (fun fp -> narrowerMatch fp requested) with
                    | ProcessParameterOwner _, _
                    | _, None ->
                        list.Add requested

                        mutations.Add {
                            Owner = owner
                            Annotation = annotation
                        }
                    | NodeOwner _, Some existingFp ->
                        errors <-
                            errors
                            @ [
                                ProcessCoreWritebackError.ConflictingAnnotationIdentity(key, existingFp, requested)
                            ]

    if not errors.IsEmpty then
        Error(errors |> List.distinct)
    else
        Ok(mutations |> List.ofSeq)

// ── New editor layers ────────────────────────────────────────────────────

let private validateNewLayerNames
    (datasets: Dataset list)
    (loadedTableNames: string list)
    (newLayers: ProvenanceLayer list)
    : ProcessCoreWritebackError list =
    let existingNames =
        datasets
        |> List.collect (fun ds -> ds.Processes |> Seq.map (fun proc -> proc.Name) |> List.ofSeq)
        |> Set.ofList
        |> Set.union (Set.ofList loadedTableNames)

    let mutable seen: Set<string> = Set.empty

    [
        for layer in newLayers do
            let trimmed = layer.Model.Source.Name.Trim()

            if System.String.IsNullOrWhiteSpace trimmed then
                yield ProcessCoreWritebackError.BlankLayerName layer.Id
            elif existingNames.Contains trimmed || seen.Contains trimmed then
                yield ProcessCoreWritebackError.DuplicateLayerName trimmed
            else
                seen <- seen.Add trimmed
    ]

let private validateReferenceLinkShape (session: ProvenanceSession) (link: ProvenanceReferenceLink) =
    let checkRef (reference: ProvenanceSetReference) =
        match session.Layers |> List.tryFind (fun layer -> layer.Id = reference.LayerId) with
        | None -> false
        | Some layer ->
            match reference.Side with
            | ProvenanceSide.Input -> layer.Model.InputSets.ContainsKey reference.SetId
            | ProvenanceSide.Output -> layer.Model.OutputSets.ContainsKey reference.SetId

    if checkRef link.Source && checkRef link.Target then
        []
    else
        [ ProcessCoreWritebackError.InvalidReferenceLink link ]

/// Resolves the final `IONode` for one new-layer set: zero incoming
/// reference links means create fresh from the set; one or more means reuse
/// the already-resolved source node(s), which is valid only when every
/// linked source resolves to the same canonical node key.
let private resolveNewLayerSetNode
    (arc: ARC)
    (resolvedNodesBySetId: System.Collections.Generic.Dictionary<ProvenanceSetId, IONode>)
    (referenceLinksByTarget: Map<ProvenanceSetReference, ProvenanceReferenceLink list>)
    (target: ProvenanceSetReference)
    (set: ProvenanceSet)
    : Result<IONode, ProcessCoreWritebackError list> =
    match resolvedNodesBySetId.TryGetValue target.SetId with
    | true, node -> Ok node
    | false, _ ->
        match referenceLinksByTarget.TryFind target with
        | None
        | Some [] ->
            match nodeFromSet set with
            | Ok freshNode ->
                // See resolvePlannedNode: reuse the real existing node if this set's
                // name coincides with one already in the graph, so a directly-added
                // annotation is not orphaned by ProcessCore's own canonicalization.
                let node =
                    tryResolveNode (nodeLocation freshNode) arc |> Option.defaultValue freshNode

                resolvedNodesBySetId.[target.SetId] <- node
                Ok node
            | Error e -> Error [ e ]
        | Some links ->
            let sourceResolutions =
                links
                |> List.map (fun link ->
                    match resolvedNodesBySetId.TryGetValue link.Source.SetId with
                    | true, node -> Ok node
                    | false, _ -> Error [ ProcessCoreWritebackError.InvalidReferenceLink link ]
                )

            let errors =
                sourceResolutions
                |> List.collect (
                    function
                    | Error e -> e
                    | Ok _ -> []
                )

            if not errors.IsEmpty then
                Error(errors |> List.distinct)
            else
                let nodes =
                    sourceResolutions
                    |> List.choose (
                        function
                        | Ok n -> Some n
                        | Error _ -> None
                    )

                let distinctKeys = nodes |> List.map (fun node -> node.Key()) |> List.distinct

                if distinctKeys.Length > 1 then
                    Error(links |> List.map ProcessCoreWritebackError.InvalidReferenceLink)
                else
                    resolvedNodesBySetId.[target.SetId] <- nodes.Head
                    Ok nodes.Head

/// Materializes one new layer from its final model alone - connection
/// add/remove patch history never controls new-layer structure. One row per
/// final connection, one one-sided row per disconnected final set, or one
/// empty-process sentinel row when the layer has neither.
let private planNewLayer
    (arc: ARC)
    (resolvedNodesBySetId: System.Collections.Generic.Dictionary<ProvenanceSetId, IONode>)
    (referenceLinksByTarget: Map<ProvenanceSetReference, ProvenanceReferenceLink list>)
    (layer: ProvenanceLayer)
    : Result<PlannedRow list, ProcessCoreWritebackError list> =

    let inputResults =
        layer.Model.InputSets
        |> Map.toList
        |> List.map (fun (id, set) ->
            resolveNewLayerSetNode
                arc
                resolvedNodesBySetId
                referenceLinksByTarget
                {
                    LayerId = layer.Id
                    Side = ProvenanceSide.Input
                    SetId = id
                }
                set
            |> Result.map (fun node ->
                id,
                {
                    SetId = id
                    Header = set.Header
                    Node = node
                }
            )
        )

    let outputResults =
        layer.Model.OutputSets
        |> Map.toList
        |> List.map (fun (id, set) ->
            resolveNewLayerSetNode
                arc
                resolvedNodesBySetId
                referenceLinksByTarget
                {
                    LayerId = layer.Id
                    Side = ProvenanceSide.Output
                    SetId = id
                }
                set
            |> Result.map (fun node ->
                id,
                {
                    SetId = id
                    Header = set.Header
                    Node = node
                }
            )
        )

    let errors =
        (inputResults @ outputResults)
        |> List.collect (
            function
            | Error e -> e
            | Ok _ -> []
        )

    if not errors.IsEmpty then
        Error(errors |> List.distinct)
    else
        let inputNodes =
            inputResults
            |> List.choose (
                function
                | Ok pair -> Some pair
                | Error _ -> None
            )
            |> Map.ofList

        let outputNodes =
            outputResults
            |> List.choose (
                function
                | Ok pair -> Some pair
                | Error _ -> None
            )
            |> Map.ofList

        let isConnected setId =
            layer.Model.Connections
            |> Map.exists (fun _ connection -> connection.InputSetId = setId || connection.OutputSetId = setId)

        let connectionRows =
            layer.Model.Connections
            |> Map.toList
            |> List.map (fun (connectionId, connection) -> {
                Input = inputNodes.TryFind connection.InputSetId
                Output = outputNodes.TryFind connection.OutputSetId
                ConnectionId = Some connectionId
            })

        let oneSidedInputRows =
            inputNodes
            |> Map.toList
            |> List.filter (fun (id, _) -> not (isConnected id))
            |> List.map (fun (_, node) -> {
                Input = Some node
                Output = None
                ConnectionId = None
            })

        let oneSidedOutputRows =
            outputNodes
            |> Map.toList
            |> List.filter (fun (id, _) -> not (isConnected id))
            |> List.map (fun (_, node) -> {
                Input = None
                Output = Some node
                ConnectionId = None
            })

        let rows = connectionRows @ oneSidedInputRows @ oneSidedOutputRows

        Ok(
            if rows.IsEmpty then
                [
                    {
                        Input = None
                        Output = None
                        ConnectionId = None
                    }
                ]
            else
                rows
        )

/// Preflight over one or more loaded tables. Callers guarantee via
/// `validateLayers` that every index's initial layer is present. All lookup
/// helpers receive the merged index view; per-table structural planning
/// receives its own layer's final pools and its own table-name-filtered
/// patches, so patches route to exactly one loaded table.
let private preflight
    (indexList: ProcessCoreWritebackIndex list)
    (session: ProvenanceSession)
    (arc: ARC)
    : Result<Plan, ProcessCoreWritebackError list> =
    let mergedIndex = mergeIndices indexList

    let indexBySourceId =
        indexList |> List.map (fun index -> index.InitialSourceId, index) |> Map.ofList

    let layersInOrder =
        session.LayerOrder
        |> List.map (fun id -> session.Layers |> List.find (fun layer -> layer.Id = id))

    let loadedPairs =
        layersInOrder
        |> List.choose (fun layer ->
            indexBySourceId.TryFind layer.Model.Source.Id
            |> Option.map (fun index -> index, layer)
        )

    let loadedSourceIds =
        loadedPairs |> List.map (fun (_, layer) -> layer.Model.Source.Id) |> Set.ofList

    let loadedTableNames =
        loadedPairs |> List.map (fun (_, layer) -> layer.Model.Source.Name)

    let newLayers =
        layersInOrder
        |> List.filter (fun layer -> not (loadedSourceIds.Contains layer.Model.Source.Id))

    let datasetsByPair =
        loadedPairs
        |> List.map (fun (index, layer) -> layer, tryResolveDataset index.LoadedTable.DatasetPath arc)

    let nameErrors =
        validateNewLayerNames (datasetsByPair |> List.choose snd) loadedTableNames newLayers

    let linkShapeErrors =
        session.ReferenceLinks |> List.collect (validateReferenceLinkShape session)

    let updateResults =
        session.PatchLog
        |> List.choose (
            function
            | ProvenanceTablePatch.UpdatePropertyValue(propertyValueId, anchor, _, _, _) ->
                Some(resolveUpdatePatch mergedIndex session arc propertyValueId anchor)
            | _ -> None
        )

    let updateErrors =
        updateResults
        |> List.collect (
            function
            | Error e -> e
            | Ok _ -> []
        )

    let structureResult =
        List.zip datasetsByPair loadedPairs
        |> List.map (fun ((_, dataset), (_, layer)) ->
            let tableName = layer.Model.Source.Name

            planAdditions
                arc
                mergedIndex
                layer.Model.InputSets
                layer.Model.OutputSets
                layer.Model.Connections
                (addSetPatchesFor tableName session.PatchLog)
                (addConnectionPatchesFor tableName session.PatchLog)
            |> Result.bind (fun additionRows ->
                planRemovals
                    arc
                    mergedIndex
                    layer.Model.InputSets
                    layer.Model.OutputSets
                    layer.Model.Connections
                    (removeConnectionPatchesFor tableName session.PatchLog)
                |> Result.map (fun replacedProcesses ->
                    {
                        Dataset = dataset
                        LoadedTableName = tableName
                        ReplacedProcesses = replacedProcesses
                        NewRows = additionRows
                    }
                    |> supersedeOneSidedProcesses arc mergedIndex
                )
            )
        )
        |> collectErrors

    // Node-identity validation runs once, below, over the combined structure
    // and new-layer rows; `allRowsForIdentity` covers the structure-only rows
    // even when new-layer planning fails.
    let structureErrors, structureValue =
        match structureResult with
        | Error errors -> errors, None
        | Ok value -> [], Some value

    let structuralRowsOf (tables: TablePlan list) =
        tables
        |> List.collect (fun table -> table.NewRows @ (table.ReplacedProcesses |> List.collect snd))

    // New layers materialize from their final model alone (no patch replay for
    // structure); resolved nodes are shared globally so reference links can
    // reuse canonical nodes across every loaded layer and any earlier new layer.
    let newLayersResult =
        structureValue
        |> Option.map (fun tables ->
            let initialStructuralNodesById =
                structuralRowsOf tables
                |> List.collect (fun row -> [ row.Input; row.Output ] |> List.choose id)
                |> List.map (fun planned -> planned.SetId, planned.Node)
                |> Map.ofList

            let initialRemainingIds =
                loadedPairs
                |> List.collect (fun (_, layer) ->
                    (layer.Model.InputSets |> Map.toList |> List.map fst)
                    @ (layer.Model.OutputSets |> Map.toList |> List.map fst)
                )
                |> List.filter (fun id -> not (initialStructuralNodesById.ContainsKey id))
                |> List.distinct

            initialRemainingIds
            |> List.map (resolveNodeForSet initialStructuralNodesById arc mergedIndex)
            |> collectErrors
            |> Result.map (fun pairs -> List.zip initialRemainingIds pairs)
            |> Result.bind (fun originalPairs ->
                let resolvedNodesBySetId =
                    System.Collections.Generic.Dictionary<ProvenanceSetId, IONode>()

                for KeyValue(id, node) in initialStructuralNodesById do
                    resolvedNodesBySetId.[id] <- node

                for id, node in originalPairs do
                    resolvedNodesBySetId.[id] <- node

                let referenceLinksByTarget =
                    session.ReferenceLinks |> List.groupBy (fun link -> link.Target) |> Map.ofList

                let layerResults =
                    newLayers
                    |> List.map (fun layer ->
                        planNewLayer arc resolvedNodesBySetId referenceLinksByTarget layer
                        |> Result.map (fun rows -> {
                            LayerName = layer.Model.Source.Name
                            Rows = rows
                        })
                    )

                collectErrors layerResults
                |> Result.map (fun plans -> plans, resolvedNodesBySetId)
            )
        )

    let newLayerErrors, newLayerValue =
        match newLayersResult with
        | None -> [], None
        | Some(Error errors) -> errors, None
        | Some(Ok value) -> [], Some value

    let allRowsForIdentity =
        match structureValue, newLayerValue with
        | Some tables, Some(newLayerPlans, _) ->
            structuralRowsOf tables @ (newLayerPlans |> List.collect (fun p -> p.Rows))
        | Some tables, None -> structuralRowsOf tables
        | None, _ -> []

    let nodeIdentityErrors = validateNodeIdentity allRowsForIdentity

    let allFinalConnections =
        session.Layers
        |> List.collect (fun layer -> layer.Model.Connections |> Map.toList)
        |> Map.ofList

    // A connection removed later in the session retracts the assignment made
    // through it (the editor detaches the value from that edge's endpoints),
    // so an add patch survives filtered to its still-present connections and
    // is consumed as a no-op once none survive.
    let addPropertyPatches =
        session.PatchLog
        |> List.choose (
            function
            | ProvenanceTablePatch.AddLoadedPropertyValue(target, _, header, _, _) ->
                match target with
                | ProvenancePropertyTarget.Connections connectionIds ->
                    match connectionIds |> List.filter allFinalConnections.ContainsKey with
                    | [] -> None
                    | surviving -> Some(ProvenancePropertyTarget.Connections surviving, header)
                | _ -> Some(target, header)
            | _ -> None
        )

    let targetBelongsToLayer (layerModel: ProvenanceModel) target =
        match target with
        | ProvenancePropertyTarget.InputSets ids -> ids |> List.forall layerModel.InputSets.ContainsKey
        | ProvenancePropertyTarget.OutputSets ids -> ids |> List.forall layerModel.OutputSets.ContainsKey
        | ProvenancePropertyTarget.Connections ids -> ids |> List.forall layerModel.Connections.ContainsKey

    let propertyResult =
        newLayerValue
        |> Option.map (fun (_, resolvedNodesBySetId) ->
            let structuralNodesById =
                resolvedNodesBySetId |> Seq.map (fun kvp -> kvp.Key, kvp.Value) |> Map.ofSeq

            let allConnections = allFinalConnections
            let allLayers = (loadedPairs |> List.map snd) @ newLayers
            let allLayerModels = allLayers |> List.map (fun layer -> layer.Model)

            let placementsPerPatch =
                addPropertyPatches
                |> List.map (fun (target, header) ->
                    allLayerModels |> List.tryFind (fun model -> targetBelongsToLayer model target), target, header
                )

            let placementResults =
                allLayerModels
                |> List.map (fun model ->
                    let patchesForModel =
                        placementsPerPatch
                        |> List.choose (fun (owner, target, header) ->
                            match owner with
                            | Some m when System.Object.ReferenceEquals(m, model) -> Some(target, header)
                            | _ -> None
                        )

                    let layer =
                        allLayers |> List.find (fun l -> System.Object.ReferenceEquals(l.Model, model))

                    resolveAddPropertyPatches layer patchesForModel
                )

            collectErrors placementResults
            |> Result.map List.concat
            |> Result.bind (fun placements ->
                let attemptsRecipeComponentMutation =
                    placements
                    |> List.exists (fun placement -> placement.Header.Kind.Id = ProcessCoreKinds.componentKind.Id)

                if attemptsRecipeComponentMutation then
                    Error [
                        ProcessCoreWritebackError.ReadOnlyRecipeComponentMutation
                    ]
                else
                    let immediate, deferred = splitPlacements mergedIndex placements

                    planPropertyMutations arc mergedIndex allConnections structuralNodesById immediate
                    |> Result.map (fun mutations -> mutations, deferred)
            )
        )

    let propertyErrors, propertyValue =
        match propertyResult with
        | None -> [], None
        | Some(Error errors) -> errors, None
        | Some(Ok value) -> [], Some value

    // Structural patches are handled against their loaded layer's pools above,
    // or (for a session-created layer's table name) by that layer's own
    // final-model materialization, which needs no patch replay. A structural
    // patch naming any other table targets previous/upstream context, where
    // only value edits are allowed.
    let knownLayerNames =
        loadedTableNames
        @ (newLayers |> List.map (fun layer -> layer.Model.Source.Name))
        |> Set.ofList

    let structuralPatchTableName =
        function
        | ProvenanceTablePatch.AddLoadedSet(_, patchTableName, _, _)
        | ProvenanceTablePatch.AddLoadedConnection(patchTableName, _, _, _, _)
        | ProvenanceTablePatch.RemoveLoadedConnection(patchTableName, _, _, _, _) -> Some patchTableName
        | _ -> None

    let unhandledPatches =
        session.PatchLog
        |> List.choose (fun patch ->
            match structuralPatchTableName patch with
            | Some patchTableName when not (knownLayerNames.Contains patchTableName) ->
                let sourceId =
                    findPreviousSourceId session patchTableName
                    |> Option.defaultValue patchTableName

                Some(ProcessCoreWritebackError.StructuralPreviousContextEdit sourceId)
            | _ -> None
        )

    let allErrors =
        nameErrors
        @ linkShapeErrors
        @ updateErrors
        @ structureErrors
        @ newLayerErrors
        @ nodeIdentityErrors
        @ propertyErrors
        @ unhandledPatches

    if not allErrors.IsEmpty then
        Error(allErrors |> List.distinct)
    else
        let updates =
            updateResults
            |> List.choose (
                function
                | Ok(Some update) -> Some update
                | _ -> None
            )
            |> List.distinctBy (fun update -> update.PropertyValueId)

        let newLayerPlans, _ = newLayerValue.Value
        let mutations, deferredPlacements = propertyValue.Value

        Ok {
            Updates = updates
            Tables = structureValue.Value
            NewLayersDataset = datasetsByPair |> List.tryLast |> Option.bind snd
            NewLayers = newLayerPlans
            PropertyMutations = mutations
            DeferredPropertyPlacements = deferredPlacements
        }

let private ioOf (row: PlannedRow) =
    (row.Input |> Option.map (fun p -> p.Node) |> Option.toList),
    (row.Output |> Option.map (fun p -> p.Node) |> Option.toList)

let private nodesOf (row: PlannedRow) =
    [ row.Input; row.Output ] |> List.choose id |> List.map (fun p -> p.Node)

let private applyMutation (mutation: PropertyMutation) =
    match mutation.Owner with
    | NodeOwner node ->
        match node with
        | SampleNode sample -> sample.AddAdditionalProperty mutation.Annotation
        | DataNode data -> data.AddAdditionalProperty mutation.Annotation
    | ProcessParameterOwner proc -> proc.AddParameterValue mutation.Annotation

let private apply (arc: ARC) (plan: Plan) : ProcessCoreWritebackSummary =
    let touchedAnnotations =
        System.Collections.Generic.HashSet<Annotation>(HashIdentity.Reference)

    for update in plan.Updates do
        for annotation in update.Annotations do
            applyValue update.Value update.Unit annotation
            touchedAnnotations.Add annotation |> ignore

    let mutable addedProcesses = 0
    let mutable removedProcesses = 0
    let mutable addedNodes = 0
    let mutable connectionProcessMap: Map<ProvenanceConnectionId, Process> = Map.empty

    let existingNodeKeys =
        System.Collections.Generic.HashSet<string>(arc.AllNodes() |> Seq.map (fun node -> node.Key()))

    let countNewNodes (row: PlannedRow) =
        for node in nodesOf row do
            if existingNodeKeys.Add(node.Key()) then
                addedNodes <- addedNodes + 1

    let recordConnection (row: PlannedRow) (proc: Process) =
        match row.ConnectionId with
        | Some connectionId -> connectionProcessMap <- connectionProcessMap |> Map.add connectionId proc
        | None -> ()

    for table in plan.Tables do
        match table.Dataset with
        | None -> ()
        | Some dataset ->
            for original, rows in table.ReplacedProcesses do
                match rows with
                | [] ->
                    removeProcess dataset original
                    removedProcesses <- removedProcesses + 1
                | first :: rest ->
                    countNewNodes first
                    let inputs, outputs = ioOf first
                    replaceProcessIO inputs outputs original
                    recordConnection first original

                    for row in rest do
                        countNewNodes row
                        let clone = cloneProcessShell original
                        addProcess dataset clone
                        let inputs, outputs = ioOf row
                        replaceProcessIO inputs outputs clone
                        recordConnection row clone
                        addedProcesses <- addedProcesses + 1

            for row in table.NewRows do
                countNewNodes row
                let proc = Process(table.LoadedTableName)
                addProcess dataset proc
                let inputs, outputs = ioOf row
                replaceProcessIO inputs outputs proc
                recordConnection row proc
                addedProcesses <- addedProcesses + 1

    // New layers materialize in `LayerOrder`, appended after the loaded tables.
    match plan.NewLayersDataset with
    | None -> ()
    | Some dataset ->
        for newLayer in plan.NewLayers do
            for row in newLayer.Rows do
                countNewNodes row
                let proc = Process(newLayer.LayerName)
                addProcess dataset proc
                let inputs, outputs = ioOf row
                replaceProcessIO inputs outputs proc
                recordConnection row proc
                addedProcesses <- addedProcesses + 1

    let mutable addedAnnotations = 0

    for mutation in plan.PropertyMutations do
        applyMutation mutation
        addedAnnotations <- addedAnnotations + 1

    for placement in plan.DeferredPropertyPlacements do
        match placement.Target with
        | ProvenancePropertyTarget.Connections connectionIds ->
            for connectionId in connectionIds do
                match connectionProcessMap.TryFind connectionId with
                | None ->
                    // splitPlacements defers only editor-created connections, and every
                    // final editor-created connection materializes exactly one planned
                    // row that records its process above. Skipping here would silently
                    // drop a validated placement, so an unmet invariant fails loudly.
                    failwith
                        $"Deferred property placement targets connection '{connectionId}' but no planned row materialized a process for it."
                | Some proc ->
                    let annotation =
                        annotationFromValue (Some "ParameterValue") placement.Header placement.Value placement.Unit

                    applyMutation {
                        Owner = ProcessParameterOwner proc
                        Annotation = annotation
                    }

                    addedAnnotations <- addedAnnotations + 1
        | _ -> ()

    {
        UpdatedAnnotations = touchedAnnotations.Count
        AddedAnnotations = addedAnnotations
        AddedNodes = addedNodes
        AddedProcesses = addedProcesses
        RemovedProcesses = removedProcesses
    }

/// Writes a session holding several independently loaded tables back to the
/// ARC. `indices` is keyed by each conversion's `InitialSourceId`; patches
/// route to their loaded table via their anchor's `Source.Id` (value edits,
/// through the per-source-namespaced property ids) or via their table name
/// (structural edits). Session-created layers materialize into the dataset
/// of the last loaded table in `LayerOrder`.
let prepareWriteBackMany
    (indices: Map<ProvenanceSourceId, ProcessCoreWritebackIndex>)
    (session: ProvenanceSession)
    (arc: ARC)
    : Result<ARC -> ProcessCoreWritebackSummary, ProcessCoreWritebackError list> =
    if indices.IsEmpty then
        invalidArg (nameof indices) "writeBackMany requires at least one writeback index."

    for KeyValue(sourceId, index) in indices do
        if sourceId <> index.InitialSourceId then
            invalidArg
                (nameof indices)
                $"Index for source '{index.InitialSourceId}' is keyed under '{sourceId}'; keys must equal InitialSourceId."

    let indexList = indices |> Map.toList |> List.map snd

    let structuralErrors =
        (indexList |> List.collect (fun index -> validateGraph index arc))
        @ validateLayers indexList session
        |> List.distinct

    if not structuralErrors.IsEmpty then
        Error structuralErrors
    else
        preflight indexList session arc
        |> Result.map (fun plan -> fun targetArc -> apply targetArc plan)

let writeBackMany
    (indices: Map<ProvenanceSourceId, ProcessCoreWritebackIndex>)
    (session: ProvenanceSession)
    (arc: ARC)
    : Result<ProcessCoreWritebackSummary, ProcessCoreWritebackError list> =
    prepareWriteBackMany indices session arc
    |> Result.map (fun mutation -> mutation arc)

let writeBack
    (index: ProcessCoreWritebackIndex)
    (session: ProvenanceSession)
    (arc: ARC)
    : Result<ProcessCoreWritebackSummary, ProcessCoreWritebackError list> =
    writeBackMany (Map [ index.InitialSourceId, index ]) session arc

// ── Canonical writeback ─────────────────────────────────────────────────────
//
// Additive: everything above stays live for the renderer, which still calls the
// legacy entry points. The canonical surface below owns the single
// multi-location index, the canonical session, and the canonical error type.
// The canonical and legacy provenance vocabularies declare the same primitive
// names, so this file must not `open` a canonical provenance module; canonical
// types are reached through these abbreviations instead.

module CanonicalIdentifiers = Swate.Components.Page.ProvenanceGrouping.Identifiers
module CanonicalPlan = Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreWritebackPlan
module CanonicalProjectionTypes = Swate.Components.Page.ProvenanceGrouping.ProjectionTypes
module CanonicalValues = Swate.Components.Page.ProvenanceGrouping.Values

/// One indexed annotation occurrence resolved to the live ProcessCore object
/// and the collection holding it, captured before any mutation so later
/// additions and removals cannot shift the positions it was resolved from.
type private ResolvedCanonicalAnnotation = {
    Owner: ProcessCoreCanonicalAnnotationOwner
    Annotation: Annotation
    Collection: ResizeArray<Annotation>
}

/// One canonical node's live ProcessCore representation. `IsNewObject` is true
/// only when the node exists neither in the loaded index nor anywhere in the
/// ARC, so an equal-key node outside the loaded selection is attached to
/// instead of duplicated.
type private ResolvedCanonicalNode = {
    Node: IONode
    IsNewObject: bool
    Annotations: CanonicalPlan.PlannedAnnotation list
}

[<RequireQualifiedAccess>]
type private CanonicalProcessTarget =
    /// The exact indexed ProcessCore process, updated in place.
    | Indexed of Process
    /// A shell cloned from the indexed process. Annotations are written from
    /// the plan rather than copied, so two partitions of one original process
    /// can never share an annotation object.
    | Clone of source: Process * destination: Dataset
    | Created of destination: Dataset

type private ResolvedCanonicalProcess = {
    Planned: CanonicalPlan.PlannedProcess
    Target: CanonicalProcessTarget
    Inputs: IONode list
    Outputs: IONode list
    Annotations: CanonicalPlan.PlannedAnnotation list
}

type private ResolvedCanonicalPlan = {
    Plan: CanonicalPlan.ProcessCoreWritebackPlan
    Nodes: (CanonicalIdentifiers.CanonicalNodeId * ResolvedCanonicalNode) list
    Processes: ResolvedCanonicalProcess list
    Removals: (Dataset * Process) list
    Occurrences: Map<CanonicalIdentifiers.AnnotationAssignmentId, ResolvedCanonicalAnnotation list>
    Remintings: Map<CanonicalIdentifiers.AnnotationAssignmentId, CanonicalPlan.PlannedAnnotationReminting>
}

let private canonicalInvalidState message =
    ProcessCoreCanonicalWritebackError.InvalidPreparedState message

/// Both public entry points refuse a session whose layer projections were never
/// resolved, so no caller can bypass `Session.prepareForWriteback`.
let private validateCanonicalProjections (session: CanonicalProjectionTypes.ProvenanceSession) = [
    for KeyValue(layerId, _) in session.Layers do
        match session.LayerProjections |> Map.tryFind layerId with
        | None ->
            yield
                canonicalInvalidState
                    $"Layer '{layerId}' has no resolved projection; the session was not prepared for writeback."
        | Some projection ->
            if
                projection.Stale
                || projection.TopologyRevision <> session.AvailabilityTopologyRevision
                || projection.ValueRevision <> session.AnnotationValueRevision
            then
                yield
                    canonicalInvalidState
                        $"Layer '{layerId}' has an unresolved projection invalidation; the session was not prepared for writeback."
]

let private validateCanonicalGraph (index: ProcessCoreCanonicalIndex) (arc: ARC) =
    if graphFingerprint arc <> index.ArcFingerprint then
        [ ProcessCoreCanonicalWritebackError.StaleArc ]
    else
        []

let private validateCanonicalSources
    (index: ProcessCoreCanonicalIndex)
    (session: CanonicalProjectionTypes.ProvenanceSession)
    =
    [
        if index.LoadedProcessGroups.IsEmpty then
            yield canonicalInvalidState "The canonical index carries no loaded process group."

        let layerIds = session.Layers |> Map.keys |> Set.ofSeq
        let orderIds = session.LayerOrder |> Set.ofList

        if layerIds <> orderIds || session.LayerOrder.Length <> orderIds.Count then
            yield ProcessCoreCanonicalWritebackError.InvalidLayerOrder session.LayerOrder

        for KeyValue(_, layer) in session.Layers do
            if not (index.SourceLocations.ContainsKey layer.Source.Id) then
                yield ProcessCoreCanonicalWritebackError.SourceLocationNotFound layer.Source.Id

        for KeyValue(sourceId, _) in index.SourceLocations do
            if session.Layers |> Map.exists (fun _ layer -> layer.Source.Id = sourceId) |> not then
                yield ProcessCoreCanonicalWritebackError.InitialLayerNotFound sourceId
    ]

let private canonicalAssignmentIds (session: CanonicalProjectionTypes.ProvenanceSession) =
    Set.union
        (session.Nodes
         |> Map.toSeq
         |> Seq.collect (fun (_, node) -> node.Assignments |> Map.keys)
         |> Set.ofSeq)
        (session.Processes
         |> Map.toSeq
         |> Seq.collect (fun (_, structuralProcess) -> structuralProcess.Assignments |> Map.keys)
         |> Set.ofSeq)

let private projectedBackings (projection: CanonicalProjectionTypes.CachedLayerProjection) = [
    yield!
        projection.Groups
        |> List.collect (fun group -> group.Annotations |> List.map _.Backing)

    yield!
        projection.Connectors
        |> List.collect (fun connector -> connector.Annotations |> List.map _.Backing)

    yield!
        projection.ProcessOnlyEntries
        |> List.collect (fun entry -> entry.Annotations |> List.map _.Backing)

    yield!
        projection.ShelfEntries
        |> List.choose (fun entry ->
            match entry.Payload with
            | CanonicalProjectionTypes.AssignmentBacked payload -> Some payload.Backing
            | CanonicalProjectionTypes.CatalogBacked _ -> None
        )
]

/// Every projected availability reference a materialization depends on must
/// resolve to an originating assignment *and* to an adapter origin. Generic
/// preparation checks projections against canonical state only; this pass
/// additionally requires the indexed occurrence or indexed stored resource the
/// reference ultimately derives from to exist.
let private validateCanonicalAvailability
    (index: ProcessCoreCanonicalIndex)
    (session: CanonicalProjectionTypes.ProvenanceSession)
    =
    let errors = ResizeArray<ProcessCoreCanonicalWritebackError>()
    let knownAssignmentIds = canonicalAssignmentIds session

    let isReferenceValue valueId =
        session.Values
        |> Map.tryFind valueId
        |> Option.exists (fun definition ->
            match definition.Value with
            | CanonicalValues.ProvenanceValue.Reference _ -> true
            | _ -> false
        )

    let validateLineage assignmentId valueId (lineage: CanonicalValues.AssignmentLineage) =
        match lineage with
        | CanonicalValues.AssignmentLineage.Created -> ()
        | CanonicalValues.AssignmentLineage.Loaded ->
            // A loaded reference occupies a storage slot rather than an
            // annotation position, so Recipe resolution - not an indexed
            // annotation occurrence - is its adapter origin.
            if
                not (isReferenceValue valueId)
                && not (index.AssignmentLocations.ContainsKey assignmentId)
            then
                errors.Add(
                    canonicalInvalidState
                        $"Projected reference for loaded assignment '{assignmentId}' has no indexed annotation occurrence."
                )
        | CanonicalValues.AssignmentLineage.DerivedFrom parentId ->
            if
                not (index.AssignmentLocations.ContainsKey parentId)
                && not (knownAssignmentIds.Contains parentId)
            then
                errors.Add(
                    canonicalInvalidState
                        $"Projected reference for assignment '{assignmentId}' derives from unknown assignment '{parentId}'."
                )
        | CanonicalValues.AssignmentLineage.DerivedFromCatalog(scheme, resourceId, _) ->
            if not (index.RecipeResources.ContainsKey(scheme, resourceId)) then
                errors.Add(
                    ProcessCoreCanonicalWritebackError.RecipeResourceNotFound(
                        scheme,
                        Swate.Components.ProcessCore.Copy.RecipeResourceKey.ById resourceId
                    )
                )

    let validateIdentity
        (identity: CanonicalProjectionTypes.AssignmentProjectionIdentity)
        (valueId: CanonicalIdentifiers.PropertyValueDefinitionId)
        (propertyKind: CanonicalValues.AssignmentPropertyKind)
        (lineage: CanonicalValues.AssignmentLineage)
        =
        if identity.ValueId <> valueId || identity.PropertyKind <> propertyKind then
            errors.Add(
                canonicalInvalidState
                    $"Projected reference for assignment '{identity.AssignmentId}' disagrees with its canonical value identity."
            )

        match session.Values |> Map.tryFind valueId with
        | Some definition when definition.PropertyId = identity.PropertyId -> ()
        | _ -> errors.Add(ProcessCoreCanonicalWritebackError.ValueNotFound valueId)

        validateLineage identity.AssignmentId valueId lineage

    let validateBacking backing =
        match backing with
        | CanonicalProjectionTypes.NodeAssignmentBacking(identity, ownerId, targetSource) ->
            match session.Nodes |> Map.tryFind ownerId with
            | None -> errors.Add(ProcessCoreCanonicalWritebackError.NodeNotFound ownerId)
            | Some node ->
                match node.Assignments |> Map.tryFind identity.AssignmentId with
                | None -> errors.Add(ProcessCoreCanonicalWritebackError.AssignmentNotFound identity.AssignmentId)
                | Some assignment ->
                    validateIdentity identity assignment.ValueId assignment.PropertyKind assignment.Lineage

            targetSource
            |> Option.iter (fun source ->
                if not (index.SourceLocations.ContainsKey source.Id) then
                    errors.Add(ProcessCoreCanonicalWritebackError.SourceLocationNotFound source.Id)
            )
        | CanonicalProjectionTypes.ProcessAssignmentBacking(identity, ownerId, linkIds, _, _) ->
            match session.Processes |> Map.tryFind ownerId with
            | None -> errors.Add(ProcessCoreCanonicalWritebackError.ProcessNotFound ownerId)
            | Some structuralProcess ->
                match structuralProcess.Assignments |> Map.tryFind identity.AssignmentId with
                | None -> errors.Add(ProcessCoreCanonicalWritebackError.AssignmentNotFound identity.AssignmentId)
                | Some assignment ->
                    validateIdentity identity assignment.ValueId assignment.PropertyKind assignment.Lineage

                    for linkId in linkIds do
                        if
                            not (assignment.CoveredLinkIds.Contains linkId)
                            || not (structuralProcess.Links.ContainsKey linkId)
                        then
                            errors.Add(ProcessCoreCanonicalWritebackError.LinkNotFound linkId)

    for KeyValue(_, projection) in session.LayerProjections do
        for backing in projectedBackings projection do
            validateBacking backing

    errors |> Seq.distinct |> Seq.toList

let private nodeAnnotationCollection (node: IONode) =
    match node with
    | SampleNode sample -> sample.AdditionalProperty
    | DataNode data -> data.AdditionalProperty

/// Removes the exact object. `Annotation.Equals` compares only name, value,
/// unit and nameTAN, so the published `Remove*` members would drop the first
/// equal occurrence instead of this one.
let private removeAnnotationByReference (annotations: ResizeArray<Annotation>) (target: Annotation) =
    let mutable position = -1

    for index in 0 .. annotations.Count - 1 do
        if position < 0 && obj.ReferenceEquals(annotations.[index], target) then
            position <- index

    if position >= 0 then
        annotations.RemoveAt position
        true
    else
        false

let private canonicalAnnotationAtPosition (position: int) (annotations: Annotation seq) =
    let items = annotations |> Seq.toList

    if position >= 0 && position < items.Length then
        Some items.[position]
    else
        None

/// Resolves one writable indexed occurrence to its live object and the
/// collection holding it. Recipe Components are read-only projections whose
/// positions and payloads the planner already validated against the stored
/// resource, so they never resolve to a writable target.
let private tryResolveCanonicalOccurrence (arc: ARC) (location: ProcessCoreCanonicalAnnotationLocation) =
    let inCollection (collection: ResizeArray<Annotation>) =
        canonicalAnnotationAtPosition location.Position collection
        |> Option.map (fun annotation -> annotation, collection)

    match location.Owner with
    | ProcessCoreCanonicalAnnotationOwner.NodeAdditionalProperty nodeLocation ->
        tryResolveNode nodeLocation arc
        |> Option.bind (fun node -> inCollection (nodeAnnotationCollection node))
    | ProcessCoreCanonicalAnnotationOwner.ProcessParameterValue processLocation ->
        tryResolveProcess processLocation arc
        |> Option.bind (fun proc -> inCollection proc.ParameterValue)
    | ProcessCoreCanonicalAnnotationOwner.RecipeComponent _ -> None

let private isWritableCanonicalOwner (owner: ProcessCoreCanonicalAnnotationOwner) =
    match owner with
    | ProcessCoreCanonicalAnnotationOwner.RecipeComponent _ -> false
    | _ -> true

/// Binds a fully validated plan to the exact live ProcessCore objects it will
/// mutate. Every lookup happens here, so `apply` performs only in-memory
/// mutations that cannot fail part-way through.
let private resolveCanonicalPlan
    (index: ProcessCoreCanonicalIndex)
    (session: CanonicalProjectionTypes.ProvenanceSession)
    (plan: CanonicalPlan.ProcessCoreWritebackPlan)
    (arc: ARC)
    : Result<ResolvedCanonicalPlan, ProcessCoreCanonicalWritebackError list> =
    let errors = ResizeArray<ProcessCoreCanonicalWritebackError>()

    let occurrences =
        index.AssignmentLocations
        |> Map.map (fun assignmentId locations ->
            locations
            |> List.filter (fun location -> isWritableCanonicalOwner location.Owner)
            |> List.choose (fun location ->
                match tryResolveCanonicalOccurrence arc location with
                | None ->
                    errors.Add(ProcessCoreCanonicalWritebackError.SourceLocationNotFound $"annotation:{assignmentId}")

                    None
                | Some(annotation, collection) ->
                    if canonicalAnnotationFingerprint annotation <> location.Fingerprint then
                        errors.Add ProcessCoreCanonicalWritebackError.StaleArc

                    Some {
                        Owner = location.Owner
                        Annotation = annotation
                        Collection = collection
                    }
            )
        )

    let nodes =
        plan.Nodes
        |> List.choose (fun planned ->
            match
                planned.ExistingLocations
                |> List.tryPick (fun location -> tryResolveNode location.Node arc)
            with
            | Some node ->
                Some(
                    planned.NodeId,
                    {
                        Node = node
                        IsNewObject = false
                        Annotations = planned.Annotations
                    }
                )
            | None when not planned.ExistingLocations.IsEmpty ->
                errors.Add(
                    ProcessCoreCanonicalWritebackError.SourceLocationNotFound
                        $"node:{planned.Key.KindId}:{planned.Key.Name}"
                )

                None
            | None ->
                match session.Nodes |> Map.tryFind planned.NodeId with
                | None ->
                    errors.Add(ProcessCoreCanonicalWritebackError.NodeNotFound planned.NodeId)
                    None
                | Some canonicalNode ->
                    match nodeFromCanonicalNode canonicalNode with
                    | Error resolutionError ->
                        errors.Add resolutionError
                        None
                    | Ok materialized ->
                        // ProcessCore canonicalizes by key when the node is linked into a
                        // process, so a node equal to one already in the ARC must be
                        // resolved up front - otherwise annotations would be written to a
                        // reference the graph then discards.
                        match tryResolveNode (nodeLocation materialized) arc with
                        | Some existing ->
                            Some(
                                planned.NodeId,
                                {
                                    Node = existing
                                    IsNewObject = false
                                    Annotations = planned.Annotations
                                }
                            )
                        | None ->
                            Some(
                                planned.NodeId,
                                {
                                    Node = materialized
                                    IsNewObject = true
                                    Annotations = planned.Annotations
                                }
                            )
        )

    let nodeObjects =
        nodes
        |> List.map (fun (nodeId, resolved) -> nodeId, resolved.Node)
        |> Map.ofList

    let resolveEndpoint nodeId =
        match nodeObjects |> Map.tryFind nodeId with
        | Some node -> [ node ]
        | None ->
            errors.Add(ProcessCoreCanonicalWritebackError.NodeNotFound nodeId)
            []

    let partitionById =
        plan.Partitions
        |> List.map (fun partition -> partition.Id, partition)
        |> Map.ofList

    let processes =
        plan.Processes
        |> List.choose (fun planned ->
            let destination = tryResolveDataset planned.Destination.DatasetPath arc

            let indexed =
                planned.IndexedProcess
                |> Option.bind (fun location -> tryResolveProcess location arc)

            let annotations =
                match partitionById |> Map.tryFind planned.PartitionId with
                | Some partition -> partition.Assignments |> List.choose _.Annotation
                | None ->
                    errors.Add(canonicalInvalidState $"Planned partition '{planned.PartitionId}' is missing.")

                    []

            let inputs, outputs =
                match planned.Shape with
                | CanonicalValues.ProcessLinkShape.Between(inputId, outputId) ->
                    resolveEndpoint inputId, resolveEndpoint outputId
                | CanonicalValues.ProcessLinkShape.InputOnly inputId -> resolveEndpoint inputId, []
                | CanonicalValues.ProcessLinkShape.OutputOnly outputId -> [], resolveEndpoint outputId
                | CanonicalValues.ProcessLinkShape.Endpointless -> [], []

            let target =
                match destination with
                | None ->
                    errors.Add(
                        ProcessCoreCanonicalWritebackError.SourceLocationNotFound(
                            String.concat "/" planned.Destination.DatasetPath
                        )
                    )

                    None
                | Some dataset ->
                    match planned.Disposition with
                    | CanonicalPlan.PlannedProcessDisposition.ReuseIndexed ->
                        match indexed with
                        | Some proc -> Some(CanonicalProcessTarget.Indexed proc)
                        | None ->
                            errors.Add(ProcessCoreCanonicalWritebackError.ProcessNotFound planned.StructuralProcessId)

                            None
                    | CanonicalPlan.PlannedProcessDisposition.CloneIndexed ->
                        match indexed with
                        | Some proc -> Some(CanonicalProcessTarget.Clone(proc, dataset))
                        | None ->
                            errors.Add(ProcessCoreCanonicalWritebackError.ProcessNotFound planned.StructuralProcessId)

                            None
                    | CanonicalPlan.PlannedProcessDisposition.NewProcess ->
                        Some(CanonicalProcessTarget.Created dataset)

            target
            |> Option.map (fun target -> {
                Planned = planned
                Target = target
                Inputs = inputs
                Outputs = outputs
                Annotations = annotations
            })
        )

    let removals =
        plan.ProcessRemovals
        |> List.choose (fun removal ->
            match tryResolveDataset removal.Location.DatasetPath arc, tryResolveProcess removal.Location arc with
            | Some dataset, Some proc -> Some(dataset, proc)
            | _ ->
                errors.Add(ProcessCoreCanonicalWritebackError.ProcessNotFound removal.StructuralProcessId)

                None
        )

    if errors.Count > 0 then
        Error(errors |> Seq.distinct |> Seq.toList)
    else
        Ok {
            Plan = plan
            Nodes = nodes
            Processes = processes
            Removals = removals
            Occurrences = occurrences
            Remintings =
                plan.AnnotationRemintings
                |> List.map (fun reminting -> reminting.AssignmentId, reminting)
                |> Map.ofList
        }

let private canonicalPreflight
    (index: ProcessCoreCanonicalIndex)
    (session: CanonicalProjectionTypes.ProvenanceSession)
    (arc: ARC)
    : Result<ResolvedCanonicalPlan, ProcessCoreCanonicalWritebackError list> =
    let adapterErrors =
        validateCanonicalProjections session
        @ validateCanonicalGraph index arc
        @ validateCanonicalSources index session
        @ validateCanonicalAvailability index session
        |> List.distinct

    if not adapterErrors.IsEmpty then
        Error adapterErrors
    else
        // Planning fails closed on a malformed stored Recipe payload, so it runs
        // before any phase that dereferences one.
        CanonicalPlan.tryCreate index session
        |> Result.bind (fun plan -> resolveCanonicalPlan index session plan arc)

/// Materializes one planned annotation. The planned fingerprint is the
/// complete payload the plan settled on, including nested and overflow data
/// carried over from the indexed occurrence and any planned reminting.
let private canonicalAnnotationFromPlan (planned: CanonicalPlan.PlannedAnnotation) =
    ProcessCore.Yaml.Annotation.fromYamlString false planned.Fingerprint.Payload

let private applyCanonicalAnnotation
    (resolved: ResolvedCanonicalPlan)
    (planned: CanonicalPlan.PlannedAnnotation)
    (existing: Annotation)
    =
    if canonicalAnnotationFingerprint existing = planned.Fingerprint then
        false
    else
        let requested = canonicalAnnotationFromPlan planned
        existing.Name <- requested.Name
        existing.Value <- requested.Value
        existing.Unit <- requested.Unit
        existing.NameTAN <- requested.NameTAN
        existing.ValueTAN <- requested.ValueTAN
        existing.UnitTAN <- requested.UnitTAN
        existing.AdditionalType <- requested.AdditionalType

        match resolved.Remintings |> Map.tryFind planned.AssignmentId with
        | Some reminting -> existing.SetProperty("@id", reminting.PlannedRegistryId)
        | None -> ()

        true

/// Writes one owner's complete final annotation set: an indexed occurrence on
/// this owner is updated in place, an occurrence whose assignment no longer
/// belongs to this owner is removed, and an assignment without an occurrence
/// here is added.
let private reconcileCanonicalAnnotations
    (resolved: ResolvedCanonicalPlan)
    (ownedHere: ProcessCoreCanonicalAnnotationOwner -> bool)
    (annotations: ResizeArray<Annotation>)
    (final: CanonicalPlan.PlannedAnnotation list)
    =
    let owned =
        resolved.Occurrences
        |> Map.toList
        |> List.collect (fun (assignmentId, items) ->
            items
            |> List.filter (fun item -> ownedHere item.Owner)
            |> List.map (fun item -> assignmentId, item.Annotation)
        )

    let finalIds = final |> List.map _.AssignmentId |> Set.ofList

    for assignmentId, annotation in owned do
        if not (finalIds.Contains assignmentId) then
            removeAnnotationByReference annotations annotation |> ignore

    let mutable updated = 0
    let mutable added = 0

    for planned in final do
        match
            owned
            |> List.filter (fun (assignmentId, _) -> assignmentId = planned.AssignmentId)
            |> List.map snd
        with
        | [] ->
            annotations.Add(canonicalAnnotationFromPlan planned)
            added <- added + 1
        | existing ->
            for annotation in existing do
                if applyCanonicalAnnotation resolved planned annotation then
                    updated <- updated + 1

    updated, added

let private applyCanonicalRecipeAssociation
    (target: CanonicalProcessTarget)
    (association: CanonicalPlan.PlannedRecipeAssociation option)
    (proc: Process)
    =
    match association with
    | None ->
        // A clone starts without an inherited association, so "no planned
        // change" means the process genuinely holds no Recipe.
        match target with
        | CanonicalProcessTarget.Indexed _ -> ()
        | _ -> proc.ExecutesRecipe <- None
    | Some association ->
        match target, association.Change with
        | CanonicalProcessTarget.Indexed _, CanonicalPlan.RecipeAssociationChange.Keep -> ()
        | _, CanonicalPlan.RecipeAssociationChange.Clear -> proc.ExecutesRecipe <- None
        | _, _ ->
            // Assign the exact indexed resource; a Recipe label never decides
            // resolution and no resource is created, cloned, or edited.
            proc.ExecutesRecipe <- association.FinalResource |> Option.map _.Resource

let private applyCanonicalPlan (resolved: ResolvedCanonicalPlan) : ProcessCoreWritebackSummary =
    let mutable addedProcesses = 0
    let mutable removedProcesses = 0
    let mutable updatedAnnotations = 0
    let mutable addedAnnotations = 0

    // Assignments the final session no longer holds at all. Per-owner
    // reconciliation below covers occurrences that only moved owner.
    for removal in resolved.Plan.AnnotationRemovals do
        match resolved.Occurrences |> Map.tryFind removal.AssignmentId with
        | None -> ()
        | Some items ->
            for item in items do
                removeAnnotationByReference item.Collection item.Annotation |> ignore

    // Structural apply, in the plan's validated destination order.
    let processTargets =
        resolved.Processes
        |> List.map (fun planned ->
            let proc =
                match planned.Target with
                | CanonicalProcessTarget.Indexed proc -> proc
                | CanonicalProcessTarget.Clone(source, destination) ->
                    // A shell only: the plan owns this partition's complete
                    // annotation set, so copying the original's annotation
                    // objects would make two partitions share them.
                    let clone =
                        Process(planned.Planned.ProcessName, ?additionalType = source.AdditionalType)

                    addProcess destination clone
                    addedProcesses <- addedProcesses + 1
                    clone
                | CanonicalProcessTarget.Created destination ->
                    let created = Process(planned.Planned.ProcessName)
                    addProcess destination created
                    addedProcesses <- addedProcesses + 1
                    created

            replaceProcessIO planned.Inputs planned.Outputs proc
            applyCanonicalRecipeAssociation planned.Target planned.Planned.RecipeAssociation proc
            planned, proc
        )

    for _, node in resolved.Nodes do
        let expected = nodeLocation node.Node

        let updated, added =
            reconcileCanonicalAnnotations
                resolved
                (function
                | ProcessCoreCanonicalAnnotationOwner.NodeAdditionalProperty location -> location = expected
                | _ -> false)
                (nodeAnnotationCollection node.Node)
                node.Annotations

        updatedAnnotations <- updatedAnnotations + updated
        addedAnnotations <- addedAnnotations + added

    for planned, proc in processTargets do
        let ownedHere =
            match planned.Target with
            | CanonicalProcessTarget.Indexed _ ->
                (function
                | ProcessCoreCanonicalAnnotationOwner.ProcessParameterValue location ->
                    Some location = planned.Planned.IndexedProcess
                | _ -> false)
            // A clone or a new process owns no indexed occurrence: the indexed
            // occurrences of its original belong to whichever partition reuses
            // that process.
            | _ -> (fun _ -> false)

        let updated, added =
            reconcileCanonicalAnnotations resolved ownedHere proc.ParameterValue planned.Annotations

        updatedAnnotations <- updatedAnnotations + updated
        addedAnnotations <- addedAnnotations + added

    for dataset, proc in resolved.Removals do
        removeProcess dataset proc
        removedProcesses <- removedProcesses + 1

    {
        UpdatedAnnotations = updatedAnnotations
        AddedAnnotations = addedAnnotations
        AddedNodes = resolved.Nodes |> List.filter (fun (_, node) -> node.IsNewObject) |> List.length
        AddedProcesses = addedProcesses
        RemovedProcesses = removedProcesses
    }

/// Writes one canonical session covering every selected process group back to
/// the ARC it was loaded from. Preflight resolves and validates everything it
/// will touch, so a rejected save leaves the ARC and the session untouched and
/// an accepted one applies completely.
let prepareCanonicalWriteBackMany
    (index: ProcessCoreCanonicalIndex)
    (session: CanonicalProjectionTypes.ProvenanceSession)
    (arc: ARC)
    : Result<(ARC -> ProcessCoreWritebackSummary), ProcessCoreCanonicalWritebackError list> =
    canonicalPreflight index session arc
    |> Result.map (fun resolved ->
        // Every mutation target is already bound to the ARC preflight ran
        // against; the parameter keeps the established apply-function shape.
        fun (_: ARC) -> applyCanonicalPlan resolved
    )

let canonicalWriteBackMany
    (index: ProcessCoreCanonicalIndex)
    (session: CanonicalProjectionTypes.ProvenanceSession)
    (arc: ARC)
    : Result<ProcessCoreWritebackSummary, ProcessCoreCanonicalWritebackError list> =
    prepareCanonicalWriteBackMany index session arc
    |> Result.map (fun mutation -> mutation arc)

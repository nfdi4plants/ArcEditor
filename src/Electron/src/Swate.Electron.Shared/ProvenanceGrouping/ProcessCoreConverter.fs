module Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreConverter

open ProcessCore
open Swate.Components.Page.ProvenanceGrouping.ProvenanceTypes
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreAdapterTypes
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreGraph

let private endpointHeader (_side: ProvenanceSide) (node: IONode) : ProvenanceIOHeader =
    let kind =
        match node with
        | SampleNode _ -> ProcessCoreKinds.sampleEndpoint
        | DataNode _ -> ProcessCoreKinds.dataEndpoint

    let additionalType =
        match node with
        | SampleNode sample -> sample.AdditionalType
        | DataNode data -> data.AdditionalType

    let text =
        additionalType
        |> Option.map (fun value -> value.Trim())
        |> Option.filter (fun value -> value <> "")
        |> Option.defaultValue kind.Label

    { Kind = kind; Text = text }

let private sideText side =
    match side with
    | ProvenanceSide.Input -> "input"
    | ProvenanceSide.Output -> "output"

let private setId
    (source: ProvenanceSourceRef)
    (side: ProvenanceSide)
    (header: ProvenanceIOHeader)
    (node: IONode)
    : ProvenanceSetId =
    $"{source.Id}::set:{sideText side}:{header.Kind.Id}:{header.Text}:{node.Key()}"

let private isBlankEndpoint (node: IONode) =
    System.String.IsNullOrWhiteSpace(nodeDisplayName node)

let private collectEndpoints
    (source: ProvenanceSourceRef)
    (datasetPath: string list)
    (selected: (int * Process) list)
    : Map<ProvenanceSetId, ProvenanceSet> *
      Map<ProvenanceSetId, ProcessCoreEndpointLocation> *
      ProcessCoreConversionWarning list
    =

    let mutable sets: Map<ProvenanceSetId, ProvenanceSet> = Map.empty
    let mutable headers: Map<ProvenanceSetId, ProvenanceIOHeader> = Map.empty

    let mutable occurrences: Map<ProvenanceSetId, ProcessCoreEndpointOccurrence list> =
        Map.empty

    let mutable warnings: ProcessCoreConversionWarning list = []

    let visit (procLocation: ProcessCoreProcessLocation) (side: ProvenanceSide) (position: int) (node: IONode) =
        if isBlankEndpoint node then
            warnings <-
                ProcessCoreConversionWarning.BlankEndpoint(procLocation, side, position)
                :: warnings
        else
            let header = endpointHeader side node
            let id = setId source side header node

            let occurrence = {
                Process = procLocation
                Side = side
                Position = position
                Node = nodeLocation node
            }

            occurrences <-
                occurrences
                |> Map.add id (occurrence :: (occurrences |> Map.tryFind id |> Option.defaultValue []))

            headers <- headers |> Map.add id header

            if not (sets.ContainsKey id) then
                sets <-
                    sets
                    |> Map.add id {
                        Id = id
                        Source = source
                        Header = header
                        Name = nodeDisplayName node
                        PropertyValueIds = []
                        InheritedPropertyValueIds = Map.empty
                    }

    for processIndex, proc in selected do
        let procLocation = processLocation datasetPath processIndex proc

        for position, node in proc.Input |> Option.toList |> List.indexed do
            visit procLocation ProvenanceSide.Input position node

        for position, node in proc.Output |> Option.toList |> List.indexed do
            visit procLocation ProvenanceSide.Output position node

    let endpointLocations =
        occurrences
        |> Map.map (fun id occurrenceList -> {
            Header = headers.[id]
            Occurrences = List.rev occurrenceList
        })

    sets, endpointLocations, List.rev warnings

let private connectionPairs (proc: Process) : (int * IONode * int * IONode) list =
    let inputs = proc.Input |> Option.toList
    let outputs = proc.Output |> Option.toList

    if inputs.Length = outputs.Length then
        [
            for index in 0 .. inputs.Length - 1 -> index, inputs.[index], index, outputs.[index]
        ]
    else
        [
            for inputIndex in 0 .. inputs.Length - 1 do
                for outputIndex in 0 .. outputs.Length - 1 do
                    yield inputIndex, inputs.[inputIndex], outputIndex, outputs.[outputIndex]
        ]

let private collectConnections
    (source: ProvenanceSourceRef)
    (datasetPath: string list)
    (selected: (int * Process) list)
    : Map<ProvenanceConnectionId, ProvenanceConnection> * Map<ProvenanceConnectionId, ProcessCoreConnectionLocation> =

    let mutable connections: Map<ProvenanceConnectionId, ProvenanceConnection> =
        Map.empty

    let mutable locations: Map<ProvenanceConnectionId, ProcessCoreConnectionLocation> =
        Map.empty

    for processIndex, proc in selected do
        let procLocation = processLocation datasetPath processIndex proc

        for inputPosition, inputNode, outputPosition, outputNode in connectionPairs proc do
            if not (isBlankEndpoint inputNode) && not (isBlankEndpoint outputNode) then
                let inputSetId =
                    setId source ProvenanceSide.Input (endpointHeader ProvenanceSide.Input inputNode) inputNode

                let outputSetId =
                    setId source ProvenanceSide.Output (endpointHeader ProvenanceSide.Output outputNode) outputNode

                let connectionId =
                    $"{source.Id}::connection:{processIndex}:{inputPosition}:{outputPosition}"

                let connection = {
                    Id = connectionId
                    Source = source
                    ProcessId = Some(processId procLocation)
                    ProcessName = Some proc.Name
                    InputSetId = inputSetId
                    OutputSetId = outputSetId
                }

                let location: ProcessCoreConnectionLocation = {
                    Process = procLocation
                    InputPosition = inputPosition
                    OutputPosition = outputPosition
                    InputSetId = inputSetId
                    OutputSetId = outputSetId
                }

                connections <- connections |> Map.add connectionId connection
                locations <- locations |> Map.add connectionId location

    connections, locations

let private partitionBySide
    (sets: Map<ProvenanceSetId, ProvenanceSet>)
    (endpointLocations: Map<ProvenanceSetId, ProcessCoreEndpointLocation>)
    (side: ProvenanceSide)
    =
    sets
    |> Map.filter (fun id _ ->
        match endpointLocations.TryFind id with
        | Some location -> location.Occurrences |> List.exists (fun occurrence -> occurrence.Side = side)
        | None -> false
    )

// ── Annotation normalization ────────────────────────────────────────────────

let private blankAnnotationName (annotation: Annotation) =
    System.String.IsNullOrWhiteSpace annotation.Name

let private categoryFromAnnotation (annotation: Annotation) : ProvenanceTerm = {
    Name = annotation.Name
    TermSource = None
    TermAccession = annotation.NameTAN
}

let private kindForNodeAnnotation (annotation: Annotation) =
    match annotation.AdditionalType |> Option.map (fun value -> value.ToLowerInvariant()) with
    | Some "characteristicvalue" -> ProcessCoreKinds.characteristic
    | Some "factorvalue" -> ProcessCoreKinds.factor
    | Some "parametervalue" -> ProcessCoreKinds.parameter
    | Some "component" -> ProcessCoreKinds.componentKind
    | _ -> ProcessCoreKinds.additionalProperty

let private processLocationKey (location: ProcessCoreProcessLocation) =
    let path = String.concat "/" location.DatasetPath
    $"{path}:{location.ProcessIndex}"

let private nonBlankSetIds (source: ProvenanceSourceRef) (side: ProvenanceSide) (nodes: IONode seq) =
    nodes
    |> Seq.filter (fun node -> not (isBlankEndpoint node))
    |> Seq.map (fun node -> setId source side (endpointHeader side node) node)
    |> Seq.distinct
    |> Seq.toList

/// One not-yet-collapsed converted property occurrence. Exact-duplicate
/// candidates (same header/value/unit/anchor context/targets) collapse into
/// one `ProvenancePropertyValue` while every source `Location` is retained
/// in the writeback index.
type private PropertyCandidate = {
    Header: ProvenancePropertyHeader
    Value: ProvenanceValue
    Unit: ProvenanceTerm option
    Anchor: ProvenanceWritebackAnchor
    TargetInputSetIds: ProvenanceSetId list
    TargetOutputSetIds: ProvenanceSetId list
    Location: ProcessCoreAnnotationLocation
}

let private collectPropertyCandidates
    (source: ProvenanceSourceRef)
    (datasetPath: string list)
    (selected: (int * Process) list)
    : PropertyCandidate list * ProcessCoreConversionWarning list =

    let mutable candidates: PropertyCandidate list = []
    let mutable warnings: ProcessCoreConversionWarning list = []

    let addNodeAnnotations
        (procLocation: ProcessCoreProcessLocation)
        (side: ProvenanceSide)
        (node: IONode)
        (targetSetId: ProvenanceSetId)
        =
        let owner = ProcessCoreAnnotationOwner.NodeAdditionalProperty(nodeLocation node)

        node
        |> nodeAdditionalProperties
        |> Seq.iteri (fun position annotation ->
            if blankAnnotationName annotation then
                warnings <- ProcessCoreConversionWarning.BlankAnnotationName(owner, position) :: warnings
            else
                let header = {
                    Kind = kindForNodeAnnotation annotation
                    Category = categoryFromAnnotation annotation
                }

                let anchor = {
                    Source = source
                    ProcessId = Some(processId procLocation)
                    ProcessName = Some procLocation.ExpectedName
                    Header = header
                    InputNames =
                        if side = ProvenanceSide.Input then
                            [ nodeDisplayName node ]
                        else
                            []
                    OutputNames =
                        if side = ProvenanceSide.Output then
                            [ nodeDisplayName node ]
                        else
                            []
                }

                let location: ProcessCoreAnnotationLocation = {
                    Owner = owner
                    Position = position
                    Fingerprint = annotationFingerprint annotation
                }

                candidates <-
                    {
                        Header = header
                        Value = valueFromAnnotation annotation
                        Unit = unitFromAnnotation annotation
                        Anchor = anchor
                        TargetInputSetIds = if side = ProvenanceSide.Input then [ targetSetId ] else []
                        TargetOutputSetIds = if side = ProvenanceSide.Output then [ targetSetId ] else []
                        Location = location
                    }
                    :: candidates
        )

    let addProcessLevelAnnotations
        (owner: ProcessCoreAnnotationOwner)
        (procLocation: ProcessCoreProcessLocation)
        (kind: ProvenanceKind)
        (targetInputSetIds: ProvenanceSetId list)
        (targetOutputSetIds: ProvenanceSetId list)
        (annotations: Annotation seq)
        =
        annotations
        |> Seq.iteri (fun position annotation ->
            if blankAnnotationName annotation then
                warnings <- ProcessCoreConversionWarning.BlankAnnotationName(owner, position) :: warnings
            elif targetInputSetIds.IsEmpty && targetOutputSetIds.IsEmpty then
                warnings <-
                    ProcessCoreConversionWarning.PropertyWithoutEndpoint(procLocation, annotation.Name)
                    :: warnings
            else
                let header = {
                    Kind = kind
                    Category = categoryFromAnnotation annotation
                }

                let anchor = {
                    Source = source
                    ProcessId = Some(processId procLocation)
                    ProcessName = Some procLocation.ExpectedName
                    Header = header
                    InputNames = []
                    OutputNames = []
                }

                let location: ProcessCoreAnnotationLocation = {
                    Owner = owner
                    Position = position
                    Fingerprint = annotationFingerprint annotation
                }

                candidates <-
                    {
                        Header = header
                        Value = valueFromAnnotation annotation
                        Unit = unitFromAnnotation annotation
                        Anchor = anchor
                        TargetInputSetIds = targetInputSetIds
                        TargetOutputSetIds = targetOutputSetIds
                        Location = location
                    }
                    :: candidates
        )

    for processIndex, proc in selected do
        let procLocation = processLocation datasetPath processIndex proc
        let inputs = proc.Input |> Option.toList
        let outputs = proc.Output |> Option.toList

        for position, node in inputs |> List.indexed do
            if not (isBlankEndpoint node) then
                let id =
                    setId source ProvenanceSide.Input (endpointHeader ProvenanceSide.Input node) node

                addNodeAnnotations procLocation ProvenanceSide.Input node id

        for position, node in outputs |> List.indexed do
            if not (isBlankEndpoint node) then
                let id =
                    setId source ProvenanceSide.Output (endpointHeader ProvenanceSide.Output node) node

                addNodeAnnotations procLocation ProvenanceSide.Output node id

        let targetInputSetIds = nonBlankSetIds source ProvenanceSide.Input inputs
        let targetOutputSetIds = nonBlankSetIds source ProvenanceSide.Output outputs

        addProcessLevelAnnotations
            (ProcessCoreAnnotationOwner.ProcessParameterValue procLocation)
            procLocation
            ProcessCoreKinds.parameter
            targetInputSetIds
            targetOutputSetIds
            proc.ParameterValue

        match proc.ExecutesRecipe with
        | Some recipe ->
            addProcessLevelAnnotations
                (ProcessCoreAnnotationOwner.RecipeComponent procLocation)
                procLocation
                ProcessCoreKinds.componentKind
                targetInputSetIds
                targetOutputSetIds
                recipe.Components
        | None -> ()

    List.rev candidates, List.rev warnings

/// Walks upstream from one selected input node, gathering non-selected
/// producer processes' parameters/components and their own input nodes'
/// annotations. All collected values target the originally selected input
/// set - the boundary node's own annotations are already collected as part
/// of the loaded input itself and are never revisited here.
let private walkPreviousContext
    (source: ProvenanceSourceRef)
    (datasetEntriesList: DatasetEntry list)
    (selectedKeys: Set<string>)
    (inputSetId: ProvenanceSetId)
    (startNode: IONode)
    : PropertyCandidate list * ProcessCoreConversionWarning list =

    let mutable candidates: PropertyCandidate list = []
    let mutable warnings: ProcessCoreConversionWarning list = []
    let visited = System.Collections.Generic.HashSet<string>()

    let addAnnotations
        (owner: ProcessCoreAnnotationOwner)
        (previousSource: ProvenanceSourceRef)
        (procLocation: ProcessCoreProcessLocation)
        (producerName: string)
        (inputNames: string list)
        (kindSelector: Annotation -> ProvenanceKind)
        (annotations: Annotation seq)
        =
        annotations
        |> Seq.iteri (fun position annotation ->
            if blankAnnotationName annotation then
                warnings <- ProcessCoreConversionWarning.BlankAnnotationName(owner, position) :: warnings
            else
                let header = {
                    Kind = kindSelector annotation
                    Category = categoryFromAnnotation annotation
                }

                let anchor = {
                    Source = previousSource
                    ProcessId = Some(processId procLocation)
                    ProcessName = Some producerName
                    Header = header
                    InputNames = inputNames
                    OutputNames = []
                }

                let location: ProcessCoreAnnotationLocation = {
                    Owner = owner
                    Position = position
                    Fingerprint = annotationFingerprint annotation
                }

                candidates <-
                    {
                        Header = header
                        Value = valueFromAnnotation annotation
                        Unit = unitFromAnnotation annotation
                        Anchor = anchor
                        TargetInputSetIds = [ inputSetId ]
                        TargetOutputSetIds = []
                        Location = location
                    }
                    :: candidates
        )

    let rec walk (node: IONode) =
        for producer in node.GetOutputOf() |> Seq.toList do
            match producer.ProcessOf with
            | Some ownerDataset ->
                match
                    datasetEntriesList
                    |> List.tryFind (fun entry -> obj.ReferenceEquals(entry.Dataset, ownerDataset))
                with
                | Some entry ->
                    match
                        entry.Dataset.Processes
                        |> Seq.tryFindIndex (fun candidate -> obj.ReferenceEquals(candidate, producer))
                    with
                    | Some processIndex ->
                        let procLocation = processLocation entry.Path processIndex producer
                        let key = processLocationKey procLocation

                        if visited.Add key then
                            if not (selectedKeys.Contains key) then
                                let previousSource =
                                    sourceRef {
                                        DatasetPath = procLocation.DatasetPath
                                        TableName = producer.Name
                                    }

                                addAnnotations
                                    (ProcessCoreAnnotationOwner.ProcessParameterValue procLocation)
                                    previousSource
                                    procLocation
                                    producer.Name
                                    []
                                    (fun _ -> ProcessCoreKinds.parameter)
                                    producer.ParameterValue

                                match producer.ExecutesRecipe with
                                | Some recipe ->
                                    addAnnotations
                                        (ProcessCoreAnnotationOwner.RecipeComponent procLocation)
                                        previousSource
                                        procLocation
                                        producer.Name
                                        []
                                        (fun _ -> ProcessCoreKinds.componentKind)
                                        recipe.Components
                                | None -> ()

                                for upstreamInput in producer.Input |> Option.toList do
                                    addAnnotations
                                        (ProcessCoreAnnotationOwner.NodeAdditionalProperty(nodeLocation upstreamInput))
                                        previousSource
                                        procLocation
                                        producer.Name
                                        [ nodeDisplayName upstreamInput ]
                                        kindForNodeAnnotation
                                        (nodeAdditionalProperties upstreamInput)

                            for upstreamInput in producer.Input |> Option.toList do
                                walk upstreamInput
                    | None -> ()
                | None -> ()
            | None -> ()

    walk startNode
    List.rev candidates, List.rev warnings

let private collectPreviousContextCandidates
    (arc: ARC)
    (source: ProvenanceSourceRef)
    (datasetPath: string list)
    (selected: (int * Process) list)
    : PropertyCandidate list * ProcessCoreConversionWarning list =

    let entries = datasetEntries arc

    let selectedKeys =
        selected
        |> List.map (fun (index, proc) -> processLocationKey (processLocation datasetPath index proc))
        |> Set.ofList

    let mutable candidates: PropertyCandidate list = []
    let mutable warnings: ProcessCoreConversionWarning list = []

    for _, proc in selected do
        for position, node in proc.Input |> Option.toList |> List.indexed do
            if not (isBlankEndpoint node) then
                let inputSetId =
                    setId source ProvenanceSide.Input (endpointHeader ProvenanceSide.Input node) node

                let nodeCandidates, nodeWarnings =
                    walkPreviousContext source entries selectedKeys inputSetId node

                candidates <- candidates @ nodeCandidates
                warnings <- warnings @ nodeWarnings

    candidates, warnings

let private dedupKey (candidate: PropertyCandidate) =
    candidate.Header,
    candidate.Value,
    candidate.Unit,
    candidate.Anchor.Source.Id,
    candidate.Anchor.ProcessId,
    candidate.Anchor.ProcessName,
    List.sort candidate.TargetInputSetIds,
    List.sort candidate.TargetOutputSetIds

let private buildPropertyValues (source: ProvenanceSourceRef) (candidates: PropertyCandidate list) =
    let groups = candidates |> List.groupBy dedupKey

    let mutable propertyValues: Map<ProvenancePropertyValueId, ProvenancePropertyValue> =
        Map.empty

    let mutable inputAttachments: Map<ProvenanceSetId, ProvenancePropertyValueId list> =
        Map.empty

    let mutable outputAttachments: Map<ProvenanceSetId, ProvenancePropertyValueId list> =
        Map.empty

    let mutable locations: Map<ProvenancePropertyValueId, ProcessCoreAnnotationLocation list> =
        Map.empty

    groups
    |> List.iteri (fun ordinal (_, group) ->
        let first = List.head group
        let id = $"{source.Id}::property:{ordinal + 1}"

        let propertyValue = {
            Id = id
            Header = first.Header
            Value = first.Value
            Unit = first.Unit
            Origin = ProvenancePropertyOrigin.Real first.Anchor
        }

        propertyValues <- propertyValues |> Map.add id propertyValue

        locations <-
            locations
            |> Map.add id (group |> List.map (fun candidate -> candidate.Location))

        for targetSetId in first.TargetInputSetIds do
            inputAttachments <-
                inputAttachments
                |> Map.add targetSetId (id :: (inputAttachments |> Map.tryFind targetSetId |> Option.defaultValue []))

        for targetSetId in first.TargetOutputSetIds do
            outputAttachments <-
                outputAttachments
                |> Map.add targetSetId (id :: (outputAttachments |> Map.tryFind targetSetId |> Option.defaultValue []))
    )

    let attach
        (attachments: Map<ProvenanceSetId, ProvenancePropertyValueId list>)
        (sets: Map<ProvenanceSetId, ProvenanceSet>)
        =
        sets
        |> Map.map (fun id set -> {
            set with
                PropertyValueIds = attachments |> Map.tryFind id |> Option.defaultValue [] |> List.rev
        })

    propertyValues, locations, attach inputAttachments, attach outputAttachments

let fromArc
    (location: ProcessCoreTableLocation)
    (arc: ARC)
    : Result<ProcessCoreConversionResult, ProcessCoreConversionError> =
    if location.DatasetPath.IsEmpty then
        Error ProcessCoreConversionError.EmptyDatasetPath
    else
        match resolveDatasetMatches location.DatasetPath arc with
        | [] -> Error(ProcessCoreConversionError.DatasetNotFound location.DatasetPath)
        | _ :: _ :: _ -> Error(ProcessCoreConversionError.AmbiguousDatasetPath location.DatasetPath)
        | [ dataset ] ->
            let selected =
                dataset.Processes
                |> Seq.mapi (fun index proc -> index, proc)
                |> Seq.filter (fun (_, proc) -> proc.Name = location.TableName)
                |> Seq.toList

            if selected.IsEmpty then
                Error(ProcessCoreConversionError.ProcessGroupNotFound location)
            else
                let source = sourceRef location

                let sets, endpointLocations, endpointWarnings =
                    collectEndpoints source location.DatasetPath selected

                let connections, connectionLocations =
                    collectConnections source location.DatasetPath selected

                let propertyCandidates, propertyWarnings =
                    collectPropertyCandidates source location.DatasetPath selected

                let previousCandidates, previousWarnings =
                    collectPreviousContextCandidates arc source location.DatasetPath selected

                let propertyValues, propertyLocations, attachInputs, attachOutputs =
                    buildPropertyValues source (propertyCandidates @ previousCandidates)

                let inputSets =
                    partitionBySide sets endpointLocations ProvenanceSide.Input |> attachInputs

                let outputSets =
                    partitionBySide sets endpointLocations ProvenanceSide.Output |> attachOutputs

                let model =
                    {
                        Source = source
                        PropertyValues = propertyValues
                        InputSets = inputSets
                        OutputSets = outputSets
                        Connections = connections
                    }
                    |> ProvenanceModel.refreshInheritedProperties

                Ok {
                    Model = model
                    Index = {
                        LoadedTable = location
                        InitialSourceId = source.Id
                        ArcFingerprint = graphFingerprint arc
                        EndpointLocations = endpointLocations
                        PropertyValueLocations = propertyLocations
                        ConnectionLocations = connectionLocations
                    }
                    Warnings = endpointWarnings @ propertyWarnings @ previousWarnings
                }

// Canonical conversion is kept after the complete legacy converter so both
// implementations can coexist without mixing their equally named model types.
open Swate.Components.ProcessCore.Copy
open Swate.Components.Page.ProvenanceGrouping.Identifiers
open Swate.Components.Page.ProvenanceGrouping.Values
open Swate.Components.Page.ProvenanceGrouping.Domain
open Swate.Components.Page.ProvenanceGrouping.ProjectionTypes
open Swate.Components.Page.ProvenanceGrouping.Model
open Swate.Components.Page.ProvenanceGrouping.Projection

type private ResolvedCanonicalProcessGroup = {
    Location: ProcessCoreProcessGroupLocation
    DatasetPath: string list
    SelectedProcesses: (int * Process) list
    Source: ProvenanceSourceRef
    LayerId: ProvenanceLayerId
}

type private CanonicalEndpointVisit = {
    NodeId: CanonicalNodeId
    Node: IONode
    SourceLocation: ProcessCoreCanonicalNodeSourceLocation
}

type private CanonicalProcessVisit = {
    ProcessId: StructuralProcessId
    LinkId: ProcessLinkId
    SourceProcess: Process
    SourceLocation: ProcessCoreProcessLocation
}

type private CanonicalNodeAnnotationCandidate = {
    NodeId: CanonicalNodeId
    Annotation: Annotation
    Position: int
    Location: ProcessCoreCanonicalAnnotationLocation
}

[<RequireQualifiedAccess>]
type private CanonicalImportedValueIdentity =
    | Text of string
    | Integer of int
    | Float of string
    | Term of ProvenanceTerm
    | Reference of scheme: string * id: string

let private canonicalIdentitySegment (value: string) = $"{value.Length}:{value}"

let private canonicalSource (location: ProcessCoreProcessGroupLocation) : ProvenanceSourceRef =
    let id =
        location.DatasetPath @ [ location.ProcessGroupName ]
        |> List.map canonicalIdentitySegment
        |> String.concat ""

    {
        Id = id
        Name = location.ProcessGroupName
    }

let private canonicalLayerId (source: ProvenanceSourceRef) : ProvenanceLayerId = $"layer:{source.Id}"

let private canonicalStructuralProcessId
    (source: ProvenanceSourceRef)
    (sourceProcessIndex: int)
    (sourceProcessName: string)
    : StructuralProcessId =
    $"{source.Id}::process:{sourceProcessIndex}:{sourceProcessName}"

let private canonicalEndpointKind (node: IONode) : ProvenanceKind =
    match node with
    | SampleNode _ -> ProcessCoreCanonicalKinds.sampleEndpoint
    | DataNode _ -> ProcessCoreCanonicalKinds.dataEndpoint

let private canonicalEndpointHeader (node: IONode) : ProvenanceIOHeader =
    let kind = canonicalEndpointKind node

    let additionalType =
        match node with
        | SampleNode sample -> sample.AdditionalType
        | DataNode data -> data.AdditionalType

    let text =
        additionalType
        |> Option.filter (System.String.IsNullOrWhiteSpace >> not)
        |> Option.defaultValue kind.Label

    { Kind = kind; Text = text }

let private canonicalCategoryFromAnnotation (annotation: Annotation) : ProvenanceTerm = {
    Name = annotation.Name
    TermSource = None
    TermAccession = annotation.NameTAN
}

let private canonicalUnitFromAnnotation (annotation: Annotation) : ProvenanceTerm option =
    annotation.Unit
    |> Option.map (fun name -> {
        Name = name
        TermSource = None
        TermAccession = annotation.UnitTAN
    })

let private canonicalNodePropertyKind
    (mappings: ProcessCoreGenericPropertyMappings)
    (annotation: Annotation)
    : AssignmentPropertyKind =
    match annotation.AdditionalType with
    | Some additionalType when additionalType = mappings.Node.AdditionalType -> AssignmentPropertyKind.Generic
    | Some "CharacteristicValue" -> AssignmentPropertyKind.AdapterSpecific ProcessCoreCanonicalKinds.characteristic
    | Some "FactorValue" -> AssignmentPropertyKind.AdapterSpecific ProcessCoreCanonicalKinds.factor
    | Some "ParameterValue" -> AssignmentPropertyKind.AdapterSpecific ProcessCoreCanonicalKinds.parameter
    | Some "Component" -> AssignmentPropertyKind.AdapterSpecific ProcessCoreCanonicalKinds.componentKind
    | _ -> AssignmentPropertyKind.AdapterSpecific ProcessCoreCanonicalKinds.additionalProperty

let private canonicalProcessParameterKind
    (mappings: ProcessCoreGenericPropertyMappings)
    (annotation: Annotation)
    : AssignmentPropertyKind =
    match annotation.AdditionalType with
    | Some additionalType when additionalType = mappings.Process.AdditionalType -> AssignmentPropertyKind.Generic
    | _ -> AssignmentPropertyKind.AdapterSpecific ProcessCoreCanonicalKinds.parameter

let private canonicalImportedValueIdentity =
    function
    | ProvenanceValue.Text value -> CanonicalImportedValueIdentity.Text value
    | ProvenanceValue.Integer value -> CanonicalImportedValueIdentity.Integer value
    | ProvenanceValue.Float value when System.Double.IsNaN value -> CanonicalImportedValueIdentity.Float "nan"
    | ProvenanceValue.Float value when value = 0.0 -> CanonicalImportedValueIdentity.Float "zero"
    | ProvenanceValue.Float value ->
        CanonicalImportedValueIdentity.Float(
            System.BitConverter
                .DoubleToInt64Bits(value)
                .ToString("X16", System.Globalization.CultureInfo.InvariantCulture)
        )
    | ProvenanceValue.Term value -> CanonicalImportedValueIdentity.Term value
    | ProvenanceValue.Reference value -> CanonicalImportedValueIdentity.Reference(value.Scheme, value.Id)

let private prependMapValue key value items =
    items
    |> Map.change key (fun current -> Some(value :: (current |> Option.defaultValue [])))

let private addProcessAssignment
    (processId: StructuralProcessId)
    (assignment: ProcessAssignment)
    (session: ProvenanceSession)
    : ProvenanceSession =
    {
        session with
            Processes =
                session.Processes
                |> Map.change
                    processId
                    (Option.map (fun structuralProcess -> {
                        structuralProcess with
                            Assignments = structuralProcess.Assignments |> Map.add assignment.Id assignment
                    }))
    }

let private resolveCanonicalProcessGroup
    (location: ProcessCoreProcessGroupLocation)
    (arc: ARC)
    : Result<ResolvedCanonicalProcessGroup, ProcessCoreCanonicalConversionError> =
    if location.DatasetPath.IsEmpty then
        Error ProcessCoreCanonicalConversionError.EmptyDatasetPath
    else
        match resolveDatasetMatches location.DatasetPath arc with
        | [] -> Error(ProcessCoreCanonicalConversionError.DatasetNotFound location.DatasetPath)
        | _ :: _ :: _ -> Error(ProcessCoreCanonicalConversionError.AmbiguousDatasetPath location.DatasetPath)
        | [ dataset ] ->
            let selectedProcesses =
                dataset.Processes
                |> Seq.mapi (fun index sourceProcess -> index, sourceProcess)
                |> Seq.filter (fun (_, sourceProcess) -> sourceProcess.Name = location.ProcessGroupName)
                |> Seq.toList

            if selectedProcesses.IsEmpty then
                Error(ProcessCoreCanonicalConversionError.ProcessGroupNotFound location)
            else
                let source = canonicalSource location

                Ok {
                    Location = location
                    DatasetPath = location.DatasetPath
                    SelectedProcesses = selectedProcesses
                    Source = source
                    LayerId = canonicalLayerId source
                }

let private canonicalReferenceCatalog (index: ProcessCoreCanonicalIndex) : ReferenceCatalog =
    index.RecipeResources
    |> Map.toList
    |> List.map (fun ((scheme, resourceId), resource) ->
        let dependentProcessValues =
            resource.Resource.Components
            |> Seq.mapi (fun position recipeComponent ->
                let componentLocation = resource.Components[position]

                {
                    Key = componentLocation.ComponentKey
                    Category = canonicalCategoryFromAnnotation recipeComponent
                    Value = canonicalValueFromAnnotation recipeComponent
                    Unit = canonicalUnitFromAnnotation recipeComponent
                    PropertyKind = AssignmentPropertyKind.AdapterSpecific ProcessCoreCanonicalKinds.componentKind
                }
            )
            |> Seq.toList

        let reference = {
            Scheme = scheme
            Id = resourceId
            Label = resource.Resource.Name |> Option.defaultValue resourceId
        }

        (scheme, resourceId),
        {
            Category = {
                Name = "Recipe"
                TermSource = None
                TermAccession = None
            }
            Reference = reference
            Unit = None
            AssignmentKind = AnnotationOwnerKind.Process
            PropertyKind = AssignmentPropertyKind.AdapterSpecific ProcessCoreCanonicalKinds.processCoreRecipeKind
            Cardinality = ReferenceCardinality.AtMostOnePerLink ProcessCoreCanonicalKinds.processCoreExecutesRecipeSlot
            DependentProcessValues = dependentProcessValues
        }
    )
    |> Map.ofList

let private projectCanonicalLayers (catalog: ReferenceCatalog) (session: ProvenanceSession) : ProvenanceSession =
    session.LayerOrder
    |> List.fold
        (fun current layerId ->
            match projectLayer layerId catalog current with
            | Ok projection -> {
                current with
                    LayerProjections = current.LayerProjections |> Map.add layerId projection
              }
            | Error error ->
                invalidOp $"Canonical ProcessCore conversion produced an invalid layer projection: {error}"
        )
        session

let fromArcMany
    (locations: ProcessCoreProcessGroupLocation list)
    (arc: ARC)
    : Result<ProcessCoreCanonicalConversionResult, ProcessCoreCanonicalConversionError list> =
    let resolutions =
        locations
        |> List.map (fun location -> resolveCanonicalProcessGroup location arc)

    let resolutionErrors =
        resolutions
        |> List.choose (
            function
            | Error error -> Some error
            | Ok _ -> None
        )

    if not resolutionErrors.IsEmpty then
        Error resolutionErrors
    else
        let resolvedGroups =
            resolutions
            |> List.choose (
                function
                | Ok resolved -> Some resolved
                | Error _ -> None
            )

        let mappings = ProcessCoreGenericPropertyMappings.defaults
        let mutable session = empty
        let mutable warnings: ProcessCoreCanonicalWarning list = []
        let mutable nodeIdsByKey: Map<CanonicalNodeKey, CanonicalNodeId> = Map.empty
        let mutable nextNodeOrdinal = 0

        let mutable propertyIdsByCategory: Map<ProvenanceTerm, PropertyDefinitionId> =
            Map.empty

        let mutable valueIdsByIdentity
            : Map<ProvenanceTerm * CanonicalImportedValueIdentity * ProvenanceTerm option, PropertyValueDefinitionId> =
            Map.empty

        let mutable nextPropertyOrdinal = 0
        let mutable nextValueOrdinal = 0
        let mutable layerOrderRev: ProvenanceLayerId list = []
        let mutable activeLayerId: ProvenanceLayerId = ""

        let ensureImportedNode kind name =
            let key = canonicalKey kind name

            match nodeIdsByKey |> Map.tryFind key with
            | Some nodeId -> nodeId
            | None ->
                nextNodeOrdinal <- nextNodeOrdinal + 1
                let nodeId = $"canonical-node-{nextNodeOrdinal}"

                let node = {
                    Id = nodeId
                    Key = key
                    Kind = kind
                    Name = name
                    Assignments = Map.empty
                }

                nodeIdsByKey <- nodeIdsByKey |> Map.add key nodeId

                session <- {
                    session with
                        Nodes = session.Nodes |> Map.add nodeId node
                }

                nodeId

        let installImportedValueDefinition category value unitValue =
            let identity = category, canonicalImportedValueIdentity value, unitValue

            match valueIdsByIdentity |> Map.tryFind identity with
            | Some valueId -> valueId
            | None ->
                let propertyId =
                    match propertyIdsByCategory |> Map.tryFind category with
                    | Some propertyId -> propertyId
                    | None ->
                        nextPropertyOrdinal <- nextPropertyOrdinal + 1
                        let propertyId = $"processcore-property-{nextPropertyOrdinal}"

                        let property = { Id = propertyId; Category = category }

                        propertyIdsByCategory <- propertyIdsByCategory |> Map.add category propertyId

                        session <- {
                            session with
                                Properties = session.Properties |> Map.add propertyId property
                        }

                        propertyId

                nextValueOrdinal <- nextValueOrdinal + 1
                let valueId = $"processcore-value-{nextValueOrdinal}"

                let definition = {
                    Id = valueId
                    PropertyId = propertyId
                    Value = value
                    Unit = unitValue
                }

                valueIdsByIdentity <- valueIdsByIdentity |> Map.add identity valueId

                session <- {
                    session with
                        Values = session.Values |> Map.add valueId definition
                }

                valueId

        let mutable nodeLocations: Map<CanonicalNodeId, ProcessCoreCanonicalNodeSourceLocation list> =
            Map.empty

        let mutable processLocations: Map<StructuralProcessId, ProcessCoreProcessLocation> =
            Map.empty

        let mutable linkLocations: Map<ProcessLinkId, ProcessCoreCanonicalLinkLocation> =
            Map.empty

        let mutable assignmentLocations: Map<AnnotationAssignmentId, ProcessCoreCanonicalAnnotationLocation list> =
            Map.empty

        let mutable referencingProcessesByRecipe: Map<RecipeResourceKey, ProcessCoreProcessLocation list> =
            Map.empty

        let mutable endpointVisits: CanonicalEndpointVisit list = []
        let mutable processVisits: CanonicalProcessVisit list = []

        for resolvedGroup in resolvedGroups do
            let mutable inputEndpoints: Map<CanonicalNodeId, LayerEndpoint> = Map.empty
            let mutable outputEndpoints: Map<CanonicalNodeId, LayerEndpoint> = Map.empty
            let mutable inputOrder = 0
            let mutable outputOrder = 0
            let mutable structuralProcessIds: Set<StructuralProcessId> = Set.empty

            let visitEndpoint
                (sourceProcessLocation: ProcessCoreProcessLocation)
                (side: ProvenanceSide)
                (node: IONode)
                =
                let sourceOrderHint =
                    match side with
                    | ProvenanceSide.Input ->
                        let current = inputOrder
                        inputOrder <- inputOrder + 1
                        current
                    | ProvenanceSide.Output ->
                        let current = outputOrder
                        outputOrder <- outputOrder + 1
                        current

                if isBlankEndpoint node then
                    warnings <-
                        ProcessCoreCanonicalWarning.BlankEndpoint(sourceProcessLocation, side, sourceOrderHint)
                        :: warnings

                    None
                else
                    let kind = canonicalEndpointKind node
                    let name = nodeDisplayName node
                    let nodeId = ensureImportedNode kind name

                    let sourceLocation = {
                        ProcessGroup = resolvedGroup.Location
                        Process = sourceProcessLocation
                        Side = side
                        Node = nodeLocation node
                        SourceOrderHint = sourceOrderHint
                    }

                    nodeLocations <- prependMapValue nodeId sourceLocation nodeLocations

                    let endpoint = {
                        Key = {
                            LayerId = resolvedGroup.LayerId
                            Side = side
                            NodeId = nodeId
                        }
                        Header = canonicalEndpointHeader node
                        LayerOrderPosition = sourceOrderHint
                    }

                    match side with
                    | ProvenanceSide.Input ->
                        if not (inputEndpoints.ContainsKey nodeId) then
                            inputEndpoints <- inputEndpoints |> Map.add nodeId endpoint
                    | ProvenanceSide.Output ->
                        if not (outputEndpoints.ContainsKey nodeId) then
                            outputEndpoints <- outputEndpoints |> Map.add nodeId endpoint

                    endpointVisits <-
                        {
                            NodeId = nodeId
                            Node = node
                            SourceLocation = sourceLocation
                        }
                        :: endpointVisits

                    Some(nodeId, sourceLocation)

            for sourceProcessIndex, sourceProcess in resolvedGroup.SelectedProcesses do
                let sourceProcessLocation =
                    processLocation resolvedGroup.DatasetPath sourceProcessIndex sourceProcess

                let structuralProcessId =
                    canonicalStructuralProcessId resolvedGroup.Source sourceProcessIndex sourceProcess.Name

                let linkId = $"{structuralProcessId}::link"

                let input =
                    sourceProcess.Input
                    |> Option.bind (visitEndpoint sourceProcessLocation ProvenanceSide.Input)

                let output =
                    sourceProcess.Output
                    |> Option.bind (visitEndpoint sourceProcessLocation ProvenanceSide.Output)

                let linkShape =
                    match input |> Option.map fst, output |> Option.map fst with
                    | Some inputNodeId, Some outputNodeId -> ProcessLinkShape.Between(inputNodeId, outputNodeId)
                    | Some inputNodeId, None -> ProcessLinkShape.InputOnly inputNodeId
                    | None, Some outputNodeId -> ProcessLinkShape.OutputOnly outputNodeId
                    | None, None -> ProcessLinkShape.Endpointless

                let link = { Id = linkId; Shape = linkShape }

                let structuralProcess = {
                    Id = structuralProcessId
                    OriginLayerId = resolvedGroup.LayerId
                    Name = Some sourceProcess.Name
                    Links = Map.ofList [ linkId, link ]
                    Assignments = Map.empty
                }

                session <- {
                    session with
                        Processes = session.Processes |> Map.add structuralProcessId structuralProcess
                }

                structuralProcessIds <- structuralProcessIds |> Set.add structuralProcessId
                processLocations <- processLocations |> Map.add structuralProcessId sourceProcessLocation

                linkLocations <-
                    linkLocations
                    |> Map.add linkId {
                        Process = sourceProcessLocation
                        Input = input |> Option.map snd
                        Output = output |> Option.map snd
                    }

                processVisits <-
                    {
                        ProcessId = structuralProcessId
                        LinkId = linkId
                        SourceProcess = sourceProcess
                        SourceLocation = sourceProcessLocation
                    }
                    :: processVisits

            let layer = {
                Id = resolvedGroup.LayerId
                Label = resolvedGroup.Location.ProcessGroupName
                Source = resolvedGroup.Source
                InputEndpoints = inputEndpoints
                OutputEndpoints = outputEndpoints
                StructuralProcessIds = structuralProcessIds
            }

            session <- {
                session with
                    Layers = session.Layers |> Map.add layer.Id layer
            }

            layerOrderRev <- layer.Id :: layerOrderRev

            if System.String.IsNullOrEmpty activeLayerId then
                activeLayerId <- layer.Id

        session <- {
            session with
                LayerOrder = List.rev layerOrderRev
                ActiveLayerId = activeLayerId
        }

        let nodeAnnotationCandidates =
            endpointVisits
            |> List.rev
            |> List.collect (fun visit ->
                let owner =
                    ProcessCoreCanonicalAnnotationOwner.NodeAdditionalProperty visit.SourceLocation.Node

                visit.Node
                |> nodeAdditionalProperties
                |> Seq.mapi (fun position annotation ->
                    let location = {
                        Owner = owner
                        Position = position
                        Fingerprint = canonicalAnnotationFingerprint annotation
                    }

                    if blankAnnotationName annotation then
                        warnings <- ProcessCoreCanonicalWarning.BlankAnnotationName(owner, position) :: warnings

                        None
                    else
                        Some {
                            NodeId = visit.NodeId
                            Annotation = annotation
                            Position = position
                            Location = location
                        }
                )
                |> Seq.choose id
                |> Seq.toList
            )

        nodeAnnotationCandidates
        |> List.groupBy _.NodeId
        |> List.iter (fun (nodeId, nodeCandidates) ->
            nodeCandidates
            |> List.groupBy (fun candidate -> candidate.Position, candidate.Location.Fingerprint.Payload)
            |> List.iteri (fun assignmentOrdinal (_, occurrences) ->
                let first = List.head occurrences

                let valueId =
                    installImportedValueDefinition
                        (canonicalCategoryFromAnnotation first.Annotation)
                        (canonicalValueFromAnnotation first.Annotation)
                        (canonicalUnitFromAnnotation first.Annotation)

                let assignmentId = $"{nodeId}::annotation:{assignmentOrdinal}"

                let assignment = {
                    Id = assignmentId
                    ValueId = valueId
                    PropertyKind = canonicalNodePropertyKind mappings first.Annotation
                    TargetSource = None
                    Lineage = AssignmentLineage.Loaded
                }

                session <- {
                    session with
                        Nodes =
                            session.Nodes
                            |> Map.change
                                nodeId
                                (Option.map (fun node -> {
                                    node with
                                        Assignments = node.Assignments |> Map.add assignmentId assignment
                                }))
                }

                assignmentLocations <-
                    assignmentLocations |> Map.add assignmentId (occurrences |> List.map _.Location)
            )
        )

        processVisits
        |> List.rev
        |> List.iter (fun visit ->
            visit.SourceProcess.ParameterValue
            |> Seq.iteri (fun position annotation ->
                let owner =
                    ProcessCoreCanonicalAnnotationOwner.ProcessParameterValue visit.SourceLocation

                if blankAnnotationName annotation then
                    warnings <- ProcessCoreCanonicalWarning.BlankAnnotationName(owner, position) :: warnings
                else
                    let valueId =
                        installImportedValueDefinition
                            (canonicalCategoryFromAnnotation annotation)
                            (canonicalValueFromAnnotation annotation)
                            (canonicalUnitFromAnnotation annotation)

                    let assignmentId = $"{visit.ProcessId}::parameter:{position}"

                    let assignment = {
                        Id = assignmentId
                        ValueId = valueId
                        PropertyKind = canonicalProcessParameterKind mappings annotation
                        CoveredLinkIds = Set.singleton visit.LinkId
                        ContainerReferenceValueId = None
                        ReferenceSlotId = None
                        Lineage = AssignmentLineage.Loaded
                    }

                    session <- addProcessAssignment visit.ProcessId assignment session

                    assignmentLocations <-
                        assignmentLocations
                        |> Map.add assignmentId [
                            {
                                Owner = owner
                                Position = position
                                Fingerprint = canonicalAnnotationFingerprint annotation
                            }
                        ]
            )

            match visit.SourceProcess.ExecutesRecipe with
            | None -> ()
            | Some recipe ->
                let resourceKey = RecipeResourceKey.ofRecipe recipe
                let resourceId = RecipeResourceKey.toStableString resourceKey
                let scheme = ProcessCoreCanonicalKinds.processCoreRecipeScheme

                referencingProcessesByRecipe <-
                    prependMapValue resourceKey visit.SourceLocation referencingProcessesByRecipe

                let referenceValue =
                    ProvenanceValue.Reference {
                        Scheme = scheme
                        Id = resourceId
                        Label = recipe.Name |> Option.defaultValue resourceId
                    }

                let recipeValueId =
                    installImportedValueDefinition
                        {
                            Name = "Recipe"
                            TermSource = None
                            TermAccession = None
                        }
                        referenceValue
                        None

                let recipeAssignmentId = $"{visit.ProcessId}::recipe"

                let recipeAssignment = {
                    Id = recipeAssignmentId
                    ValueId = recipeValueId
                    PropertyKind =
                        AssignmentPropertyKind.AdapterSpecific ProcessCoreCanonicalKinds.processCoreRecipeKind
                    CoveredLinkIds = Set.singleton visit.LinkId
                    ContainerReferenceValueId = None
                    ReferenceSlotId = Some ProcessCoreCanonicalKinds.processCoreExecutesRecipeSlot
                    Lineage = AssignmentLineage.Loaded
                }

                session <- addProcessAssignment visit.ProcessId recipeAssignment session

                recipe.Components
                |> Seq.iteri (fun position recipeComponent ->
                    let owner = ProcessCoreCanonicalAnnotationOwner.RecipeComponent(scheme, resourceId)

                    if blankAnnotationName recipeComponent then
                        warnings <- ProcessCoreCanonicalWarning.BlankAnnotationName(owner, position) :: warnings
                    else
                        let componentValueId =
                            installImportedValueDefinition
                                (canonicalCategoryFromAnnotation recipeComponent)
                                (canonicalValueFromAnnotation recipeComponent)
                                (canonicalUnitFromAnnotation recipeComponent)

                        let assignmentId = $"{visit.ProcessId}::recipe-component:{position}"

                        let assignment = {
                            Id = assignmentId
                            ValueId = componentValueId
                            PropertyKind =
                                AssignmentPropertyKind.AdapterSpecific ProcessCoreCanonicalKinds.componentKind
                            CoveredLinkIds = Set.singleton visit.LinkId
                            ContainerReferenceValueId = Some recipeValueId
                            ReferenceSlotId = None
                            Lineage =
                                AssignmentLineage.DerivedFromCatalog(
                                    scheme,
                                    resourceId,
                                    $"{resourceId}/component/{position}"
                                )
                        }

                        session <- addProcessAssignment visit.ProcessId assignment session

                        assignmentLocations <-
                            assignmentLocations
                            |> Map.add assignmentId [
                                {
                                    Owner = owner
                                    Position = position
                                    Fingerprint = canonicalAnnotationFingerprint recipeComponent
                                }
                            ]
                )
        )

        let assignmentValueIds =
            [
                yield!
                    session.Nodes
                    |> Map.toList
                    |> List.collect (fun (_, node) ->
                        node.Assignments
                        |> Map.toList
                        |> List.map (fun (assignmentId, assignment) -> assignmentId, assignment.ValueId)
                    )

                yield!
                    session.Processes
                    |> Map.toList
                    |> List.collect (fun (_, structuralProcess) ->
                        structuralProcess.Assignments
                        |> Map.toList
                        |> List.map (fun (assignmentId, assignment) -> assignmentId, assignment.ValueId)
                    )
            ]
            |> Map.ofList

        let indexSeed = {
            LoadedProcessGroups = locations
            SourceLocations =
                resolvedGroups
                |> List.map (fun resolvedGroup -> resolvedGroup.Source.Id, resolvedGroup.Location)
            NodeLocations = nodeLocations |> Map.map (fun _ locations -> List.rev locations)
            ProcessLocations = processLocations
            LinkLocations = linkLocations
            AssignmentLocations = assignmentLocations
            AssignmentValueIds = assignmentValueIds
            ReferencingProcessesByRecipe =
                referencingProcessesByRecipe
                |> Map.map (fun _ processLocations -> List.rev processLocations)
            GenericPropertyMappings = mappings
        }

        match tryCreateCanonicalIndex indexSeed arc with
        | Error error -> Error [ error ]
        | Ok index ->
            let catalog = canonicalReferenceCatalog index
            let projectedSession = projectCanonicalLayers catalog session

            Ok {
                Session = projectedSession
                Index = index
                ReferenceCatalog = catalog
                Warnings = List.rev warnings
                Locations = locations
            }

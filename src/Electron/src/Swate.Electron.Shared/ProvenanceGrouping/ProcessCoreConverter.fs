module Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreConverter

open ProcessCore
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreAdapterTypes
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreGraph

let private isBlankEndpoint (node: IONode) =
    System.String.IsNullOrWhiteSpace(nodeDisplayName node)

let private blankAnnotationName (annotation: Annotation) =
    System.String.IsNullOrWhiteSpace annotation.Name

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
    | SampleNode _ -> ProcessCoreKinds.sampleEndpoint
    | DataNode _ -> ProcessCoreKinds.dataEndpoint

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
    | Some "CharacteristicValue" -> AssignmentPropertyKind.AdapterSpecific ProcessCoreKinds.characteristic
    | Some "FactorValue" -> AssignmentPropertyKind.AdapterSpecific ProcessCoreKinds.factor
    | Some "ParameterValue" -> AssignmentPropertyKind.AdapterSpecific ProcessCoreKinds.parameter
    | Some "Component" -> AssignmentPropertyKind.AdapterSpecific ProcessCoreKinds.componentKind
    | _ -> AssignmentPropertyKind.AdapterSpecific ProcessCoreKinds.additionalProperty

let private canonicalProcessParameterKind
    (mappings: ProcessCoreGenericPropertyMappings)
    (annotation: Annotation)
    : AssignmentPropertyKind =
    match annotation.AdditionalType with
    | Some additionalType when additionalType = mappings.Process.AdditionalType -> AssignmentPropertyKind.Generic
    | _ -> AssignmentPropertyKind.AdapterSpecific ProcessCoreKinds.parameter

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
    : Result<ResolvedCanonicalProcessGroup, ProcessCoreConversionError> =
    if location.DatasetPath.IsEmpty then
        Error ProcessCoreConversionError.EmptyDatasetPath
    else
        match resolveDatasetMatches location.DatasetPath arc with
        | [] -> Error(ProcessCoreConversionError.DatasetNotFound location.DatasetPath)
        | _ :: _ :: _ -> Error(ProcessCoreConversionError.AmbiguousDatasetPath location.DatasetPath)
        | [ dataset ] ->
            let selectedProcesses =
                dataset.Processes
                |> Seq.mapi (fun index sourceProcess -> index, sourceProcess)
                |> Seq.filter (fun (_, sourceProcess) -> sourceProcess.Name = location.ProcessGroupName)
                |> Seq.toList

            if selectedProcesses.IsEmpty then
                Error(ProcessCoreConversionError.ProcessGroupNotFound location)
            else
                let source = canonicalSource location

                Ok {
                    Location = location
                    DatasetPath = location.DatasetPath
                    SelectedProcesses = selectedProcesses
                    Source = source
                    LayerId = canonicalLayerId source
                }

let private canonicalReferenceCatalog (index: ProcessCoreWritebackIndex) : ReferenceCatalog =
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
                    Value = valueFromAnnotation recipeComponent
                    Unit = canonicalUnitFromAnnotation recipeComponent
                    PropertyKind = AssignmentPropertyKind.AdapterSpecific ProcessCoreKinds.componentKind
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
            PropertyKind = AssignmentPropertyKind.AdapterSpecific ProcessCoreKinds.processCoreRecipeKind
            Cardinality = ReferenceCardinality.AtMostOnePerLink ProcessCoreKinds.processCoreExecutesRecipeSlot
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
    : Result<ProcessCoreConversionResult, ProcessCoreConversionError list> =
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
        let mutable warnings: ProcessCoreConversionWarning list = []
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
                        ProcessCoreConversionWarning.BlankEndpoint(sourceProcessLocation, side, sourceOrderHint)
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
                    ProcessCoreCanonicalAnnotationOwner.NodeAdditionalProperty visit.SourceLocation

                visit.Node
                |> nodeAdditionalProperties
                |> Seq.mapi (fun position annotation ->
                    let location = {
                        Owner = owner
                        Position = position
                        Fingerprint = canonicalAnnotationFingerprint annotation
                    }

                    if blankAnnotationName annotation then
                        warnings <- ProcessCoreConversionWarning.BlankAnnotationName(owner, position) :: warnings

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
                        (valueFromAnnotation first.Annotation)
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
                    warnings <- ProcessCoreConversionWarning.BlankAnnotationName(owner, position) :: warnings
                else
                    let valueId =
                        installImportedValueDefinition
                            (canonicalCategoryFromAnnotation annotation)
                            (valueFromAnnotation annotation)
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
                let scheme = ProcessCoreKinds.processCoreRecipeScheme

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
                    PropertyKind = AssignmentPropertyKind.AdapterSpecific ProcessCoreKinds.processCoreRecipeKind
                    CoveredLinkIds = Set.singleton visit.LinkId
                    ContainerReferenceValueId = None
                    ReferenceSlotId = Some ProcessCoreKinds.processCoreExecutesRecipeSlot
                    Lineage = AssignmentLineage.Loaded
                }

                session <- addProcessAssignment visit.ProcessId recipeAssignment session

                recipe.Components
                |> Seq.iteri (fun position recipeComponent ->
                    let owner = ProcessCoreCanonicalAnnotationOwner.RecipeComponent(scheme, resourceId)

                    if blankAnnotationName recipeComponent then
                        warnings <- ProcessCoreConversionWarning.BlankAnnotationName(owner, position) :: warnings
                    else
                        let componentValueId =
                            installImportedValueDefinition
                                (canonicalCategoryFromAnnotation recipeComponent)
                                (valueFromAnnotation recipeComponent)
                                (canonicalUnitFromAnnotation recipeComponent)

                        let assignmentId = $"{visit.ProcessId}::recipe-component:{position}"

                        let assignment = {
                            Id = assignmentId
                            ValueId = componentValueId
                            PropertyKind = AssignmentPropertyKind.AdapterSpecific ProcessCoreKinds.componentKind
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

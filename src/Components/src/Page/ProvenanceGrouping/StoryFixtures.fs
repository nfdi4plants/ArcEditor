module Swate.Components.Page.ProvenanceGrouping.StoryFixtures

open Swate.Components.Page.ProvenanceGrouping.Identifiers
open Swate.Components.Page.ProvenanceGrouping.Values
open Swate.Components.Page.ProvenanceGrouping.Domain
open Swate.Components.Page.ProvenanceGrouping.ProjectionTypes
open Swate.Components.Page.ProvenanceGrouping.Model

let private sampleKind = {
    Id = "canonical:endpoint:sample"
    Label = "Sample"
}

let private term name accession = {
    Name = name
    TermSource = Some "FIXTURE"
    TermAccession = accession
}

let private source id name : ProvenanceSourceRef = { Id = id; Name = name }

let private property id category : PropertyDefinition = { Id = id; Category = category }

let private value id propertyId content : PropertyValueDefinition = {
    Id = id
    PropertyId = propertyId
    Value = content
    Unit = None
}

let private nodeAssignment id valueId propertyKind : NodeAssignment = {
    Id = id
    ValueId = valueId
    PropertyKind = propertyKind
    TargetSource = None
    Lineage = AssignmentLineage.Loaded
}

let private processAssignment id valueId linkIds containerValueId slotId lineage : ProcessAssignment = {
    Id = id
    ValueId = valueId
    PropertyKind = AssignmentPropertyKind.Generic
    CoveredLinkIds = Set.ofList linkIds
    ContainerReferenceValueId = containerValueId
    ReferenceSlotId = slotId
    Lineage = lineage
}

let private node id name (assignments: NodeAssignment list) : CanonicalNode = {
    Id = id
    Key = { KindId = sampleKind.Id; Name = name }
    Kind = sampleKind
    Name = name
    Assignments =
        assignments
        |> List.map (fun assignment -> assignment.Id, assignment)
        |> Map.ofList
}

let private endpoint layerId side nodeId text position : LayerEndpoint = {
    Key = {
        LayerId = layerId
        Side = side
        NodeId = nodeId
    }
    Header = { Kind = sampleKind; Text = text }
    LayerOrderPosition = position
}

let private layer
    id
    label
    (sourceRef: ProvenanceSourceRef)
    (inputs: (CanonicalNodeId * string) list)
    (outputs: (CanonicalNodeId * string) list)
    (processIds: StructuralProcessId list)
    : ProvenanceLayer =
    {
        Id = id
        Label = label
        Source = sourceRef
        InputEndpoints =
            inputs
            |> List.mapi (fun position (nodeId, text) -> nodeId, endpoint id ProvenanceSide.Input nodeId text position)
            |> Map.ofList
        OutputEndpoints =
            outputs
            |> List.mapi (fun position (nodeId, text) -> nodeId, endpoint id ProvenanceSide.Output nodeId text position)
            |> Map.ofList
        StructuralProcessIds = Set.ofList processIds
    }

let private processLink id shape : ProcessLink = { Id = id; Shape = shape }

let private structuralProcess
    id
    layerId
    (links: ProcessLink list)
    (assignments: ProcessAssignment list)
    : StructuralProcess =
    {
        Id = id
        OriginLayerId = layerId
        Name = Some id
        Links = links |> List.map (fun processLink -> processLink.Id, processLink) |> Map.ofList
        Assignments =
            assignments
            |> List.map (fun assignment -> assignment.Id, assignment)
            |> Map.ofList
    }

let private session
    (layers: ProvenanceLayer list)
    (nodes: CanonicalNode list)
    (processes: StructuralProcess list)
    (properties: PropertyDefinition list)
    (values: PropertyValueDefinition list)
    : ProvenanceSession =
    {
        empty with
            Nodes =
                nodes
                |> List.map (fun canonicalNode -> canonicalNode.Id, canonicalNode)
                |> Map.ofList
            Processes =
                processes
                |> List.map (fun structuralProcess -> structuralProcess.Id, structuralProcess)
                |> Map.ofList
            Properties =
                properties
                |> List.map (fun definition -> definition.Id, definition)
                |> Map.ofList
            Values = values |> List.map (fun definition -> definition.Id, definition) |> Map.ofList
            Layers =
                layers
                |> List.map (fun provenanceLayer -> provenanceLayer.Id, provenanceLayer)
                |> Map.ofList
            LayerOrder = layers |> List.map _.Id
            ActiveLayerId = layers.Head.Id
    }

let createSharedNodeSession () =
    let category =
        property "property-temperature" (term "Temperature" (Some "FIXTURE:temperature"))

    let definition = value "value-temperature" category.Id (ProvenanceValue.Text "20")

    let assignment =
        nodeAssignment
            "assignment-temperature"
            definition.Id
            (AdapterSpecific {
                Id = "processcore:characteristic"
                Label = "Characteristic"
            })

    let firstLayer =
        layer "shared-layer-one" "Shared source one" (source "shared-source-one" "Shared source one") [] [
            "node-shared", "Shared sample"
        ] []

    let secondLayer =
        layer
            "shared-layer-two"
            "Shared source two"
            (source "shared-source-two" "Shared source two")
            [ "node-shared", "Shared sample" ] [] []

    session [ firstLayer; secondLayer ] [ node "node-shared" "Shared sample" [ assignment ] ] [] [ category ] [
        definition
    ]

let createSiblingLeakSession () =
    let nodeCategory = property "property-node-marker" (term "Node marker" None)

    let processCategory =
        property "property-process-marker" (term "Process marker" None)

    let valueX = value "value-x" nodeCategory.Id (ProvenanceValue.Text "X")
    let valueY = value "value-y" nodeCategory.Id (ProvenanceValue.Text "Y")
    let valueP = value "value-p" processCategory.Id (ProvenanceValue.Text "P")

    let assignmentX =
        nodeAssignment "assignment-x" valueX.Id AssignmentPropertyKind.Generic

    let assignmentY =
        nodeAssignment "assignment-y" valueY.Id AssignmentPropertyKind.Generic

    let linkAB = processLink "link-ab" (ProcessLinkShape.Between("node-a", "node-b"))
    let linkAC = processLink "link-ac" (ProcessLinkShape.Between("node-a", "node-c"))
    let linkBD = processLink "link-bd" (ProcessLinkShape.Between("node-b", "node-d"))

    let processAB =
        structuralProcess "process-ab" "branch-layer" [ linkAB ] [
            processAssignment "assignment-p" valueP.Id [ linkAB.Id ] None None AssignmentLineage.Loaded
        ]

    let processAC = structuralProcess "process-ac" "branch-layer" [ linkAC ] []
    let processBD = structuralProcess "process-bd" "branch-layer" [ linkBD ] []

    let branchLayer =
        layer "branch-layer" "Sibling leak" (source "branch-source" "Sibling leak") [ "node-a", "A"; "node-b", "B" ] [
            "node-b", "B"
            "node-c", "C"
            "node-d", "D"
        ] [ processAB.Id; processAC.Id; processBD.Id ]

    session
        [ branchLayer ]
        [
            node "node-a" "A" [ assignmentY ]
            node "node-b" "B" [ assignmentX ]
            node "node-c" "C" []
            node "node-d" "D" []
        ]
        [ processAB; processAC; processBD ] [ nodeCategory; processCategory ] [ valueX; valueY; valueP ]

let createAllLinkShapesSession () =
    let category = property "property-endpointless" (term "Endpointless marker" None)

    let definition =
        value "value-endpointless" category.Id (ProvenanceValue.Text "loaded")

    let between =
        processLink "link-between" (ProcessLinkShape.Between("node-a", "node-b"))

    let inputOnly = processLink "link-input-only" (ProcessLinkShape.InputOnly "node-a")

    let outputOnly =
        processLink "link-output-only" (ProcessLinkShape.OutputOnly "node-c")

    let endpointless = processLink "link-endpointless" ProcessLinkShape.Endpointless

    let connected =
        structuralProcess "process-shaped" "shape-layer" [ between; inputOnly; outputOnly ] []

    let loadedEndpointless =
        structuralProcess "process-endpointless" "shape-layer" [ endpointless ] [
            processAssignment
                "assignment-endpointless"
                definition.Id
                [ endpointless.Id ]
                None
                None
                AssignmentLineage.Loaded
        ]

    let shapeLayer =
        layer "shape-layer" "All link shapes" (source "shape-source" "All link shapes") [ "node-a", "A" ] [
            "node-b", "B"
            "node-c", "C"
        ] [ connected.Id; loadedEndpointless.Id ]

    session
        [ shapeLayer ]
        [
            node "node-a" "A" []
            node "node-b" "B" []
            node "node-c" "C" []
        ]
        [ connected; loadedEndpointless ] [ category ] [ definition ]

let createReferenceCatalogSession () =
    let recipeCategory =
        property "property-recipe" (term "Recipe" (Some "FIXTURE:recipe"))

    let componentCategory =
        property "property-component" (term "Component" (Some "FIXTURE:component"))

    let firstReference = {
        Scheme = "fixture:recipe"
        Id = "stored/recipe/one"
        Label = "Extraction"
    }

    let secondReference = {
        Scheme = "fixture:recipe"
        Id = "stored/recipe/two"
        Label = "Extraction"
    }

    let referenceDefinition =
        value "value-recipe-one" recipeCategory.Id (ProvenanceValue.Reference firstReference)

    let componentDefinition =
        value "value-component-one" componentCategory.Id (ProvenanceValue.Text "Buffer")

    let link =
        processLink "link-reference" (ProcessLinkShape.Between("node-input", "node-output"))

    let referenceAssignment =
        processAssignment
            "assignment-recipe-one"
            referenceDefinition.Id
            [ link.Id ]
            None
            (Some "fixture:recipe-slot")
            AssignmentLineage.Loaded

    let dependentAssignment =
        processAssignment
            "assignment-component-one"
            componentDefinition.Id
            [ link.Id ]
            (Some referenceDefinition.Id)
            None
            (DerivedFromCatalog(firstReference.Scheme, firstReference.Id, "component:buffer"))

    let referenceProcess =
        structuralProcess "process-reference" "reference-layer" [ link ] [ referenceAssignment; dependentAssignment ]

    let referenceLayer =
        layer
            "reference-layer"
            "Reference catalog"
            (source "reference-source" "Reference catalog")
            [ "node-input", "Input" ] [ "node-output", "Output" ] [ referenceProcess.Id ]

    let fixtureSession =
        session
            [ referenceLayer ]
            [
                node "node-input" "Input" []
                node "node-output" "Output" []
            ]
            [ referenceProcess ] [ recipeCategory; componentCategory ] [ referenceDefinition; componentDefinition ]

    let entry (reference: ReferenceValue) (dependents: ReferenceDependentProcessValue list) : ReferenceCatalogEntry = {
        Category = recipeCategory.Category
        Reference = reference
        Unit = None
        AssignmentKind = AnnotationOwnerKind.Process
        PropertyKind =
            AdapterSpecific {
                Id = "processcore:recipe"
                Label = "Recipe"
            }
        Cardinality = AtMostOnePerLink "fixture:recipe-slot"
        DependentProcessValues = dependents
    }

    let firstEntry =
        entry firstReference [
            {
                Key = "component:buffer"
                Category = componentCategory.Category
                Value = ProvenanceValue.Text "Buffer"
                Unit = None
                PropertyKind =
                    AdapterSpecific {
                        Id = "processcore:component"
                        Label = "Component"
                    }
            }
        ]

    let secondEntry = entry secondReference []

    fixtureSession,
    Map.ofList [
        (firstReference.Scheme, firstReference.Id), firstEntry
        (secondReference.Scheme, secondReference.Id), secondEntry
    ]

let allCanonicalSessions () = [
    createSharedNodeSession ()
    createSiblingLeakSession ()
    createAllLinkShapesSession ()
    createReferenceCatalogSession () |> fst
]

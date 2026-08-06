module Swate.Components.Page.ProvenanceGrouping.StoryFixtures

open Swate.Components.Page.ProvenanceGrouping.Identifiers
open Swate.Components.Page.ProvenanceGrouping.Values
open Swate.Components.Page.ProvenanceGrouping.Domain
open Swate.Components.Page.ProvenanceGrouping.MutationTypes
open Swate.Components.Page.ProvenanceGrouping.ProjectionTypes
open Swate.Components.Page.ProvenanceGrouping.Model
open Swate.Components.Page.ProvenanceGrouping.Projection

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

/// Every layer in `LayerOrder`, projected with the given catalog. A session that
/// reaches the editor has always been through the load boundary, where
/// `fromArcMany` projects every layer before returning; a fixture that skipped
/// this would hand the UI a session no real load can produce, and the first
/// component to read its own layer's projection would fail on a missing key.
let private projectAllLayers (catalog: ReferenceCatalog) (built: ProvenanceSession) : ProvenanceSession =
    built.LayerOrder
    |> List.fold
        (fun current layerId ->
            match projectLayer layerId catalog current with
            | Ok projection -> {
                current with
                    LayerProjections = current.LayerProjections |> Map.add layerId projection
              }
            | Error error -> failwithf "Story fixture produced an invalid layer projection: %A" error
        )
        built

let private sessionWithCatalog
    (catalog: ReferenceCatalog)
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
    |> projectAllLayers catalog

let private session layers nodes processes properties values : ProvenanceSession =
    sessionWithCatalog Map.empty layers nodes processes properties values

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

    let boundCategory =
        property "property-endpointless-bound" (term "Bound marker" None)

    let boundDefinition =
        value "value-endpointless-bound" boundCategory.Id (ProvenanceValue.Text "bound")

    // Preparation demands a real container: a Reference value assigned on
    // every link its dependents cover, exactly once per link.
    let recipeCategory =
        property "property-endpointless-recipe" (term "Recipe marker" None)

    let containerDefinition =
        value
            "value-endpointless-container"
            recipeCategory.Id
            (ProvenanceValue.Reference {
                Scheme = "fixture:recipe"
                Id = "endpointless-recipe"
                Label = "Endpointless recipe"
            })

    let between =
        processLink "link-between" (ProcessLinkShape.Between("node-a", "node-b"))

    let inputOnly = processLink "link-input-only" (ProcessLinkShape.InputOnly "node-a")

    let outputOnly =
        processLink "link-output-only" (ProcessLinkShape.OutputOnly "node-c")

    let endpointless = processLink "link-endpointless" ProcessLinkShape.Endpointless

    let endpointlessBound =
        processLink "link-endpointless-bound" ProcessLinkShape.Endpointless

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
            // A second loaded assignment behind the same header, value and unit:
            // the entry shows one badge per grouping value, and removing that
            // badge removes every backing assignment at once.
            processAssignment
                "assignment-endpointless-duplicate"
                definition.Id
                [ endpointless.Id ]
                None
                None
                AssignmentLineage.Loaded
        ]

    // A container-bound (Recipe Component) annotation is read-only wherever it
    // is shown, so its entry offers no removal affordance for it.
    let boundEndpointless =
        structuralProcess "process-endpointless-bound" "shape-layer" [ endpointlessBound ] [
            processAssignment
                "assignment-endpointless-recipe"
                containerDefinition.Id
                [ endpointlessBound.Id ]
                None
                None
                AssignmentLineage.Loaded
            processAssignment
                "assignment-endpointless-bound"
                boundDefinition.Id
                [ endpointlessBound.Id ]
                (Some containerDefinition.Id)
                None
                AssignmentLineage.Loaded
        ]

    let shapeLayer =
        layer "shape-layer" "All link shapes" (source "shape-source" "All link shapes") [ "node-a", "A" ] [
            "node-b", "B"
            "node-c", "C"
        ] [
            connected.Id
            loadedEndpointless.Id
            boundEndpointless.Id
        ]

    session
        [ shapeLayer ]
        [
            node "node-a" "A" []
            node "node-b" "B" []
            node "node-c" "C" []
        ]
        [ connected; loadedEndpointless; boundEndpointless ] [ category; boundCategory; recipeCategory ] [
            definition
            boundDefinition
            containerDefinition
        ]

/// One input entity whose two outgoing links belong to two structural
/// processes - the shape ProcessCore actually produces, where every `Process`
/// is one directed edge. A process-value drop on the entity therefore creates
/// one assignment per structural process while the card shows one entry, which
/// is the shape the entity-scoped bulk edit resolves. The fixture also carries
/// the two bulk-edit boundary cases: a node grouping value backed by two
/// owning assignments, and a displayed process value merging an editable
/// Parameter with a read-only container-bound (Recipe Component) backing.
let createFanOutSession () =
    let species = property "property-fan-species" (term "Species" None)

    let arabidopsis =
        value "value-fan-species" species.Id (ProvenanceValue.Text "Arabidopsis")

    let setting = property "property-fan-setting" (term "Device setting" None)
    let setting37 = value "value-fan-setting" setting.Id (ProvenanceValue.Text "37")

    // Preparation demands a real container: a Reference value assigned on
    // every link its dependents cover, exactly once per link.
    let recipeCategory = property "property-fan-recipe" (term "Recipe" None)

    let containerDefinition =
        value
            "value-fan-container"
            recipeCategory.Id
            (ProvenanceValue.Reference {
                Scheme = "fixture:recipe"
                Id = "fan-recipe"
                Label = "Fan recipe"
            })

    let linkA =
        processLink "link-fan-a" (ProcessLinkShape.Between("node-fan-input", "node-fan-out-a"))

    let linkB =
        processLink "link-fan-b" (ProcessLinkShape.Between("node-fan-input", "node-fan-out-b"))

    let processA =
        structuralProcess "process-fan-a" "fan-layer" [ linkA ] [
            processAssignment "assignment-fan-parameter" setting37.Id [ linkA.Id ] None None AssignmentLineage.Loaded
        ]

    let processB =
        structuralProcess "process-fan-b" "fan-layer" [ linkB ] [
            processAssignment
                "assignment-fan-recipe"
                containerDefinition.Id
                [ linkB.Id ]
                None
                None
                AssignmentLineage.Loaded
            processAssignment
                "assignment-fan-component"
                setting37.Id
                [ linkB.Id ]
                (Some containerDefinition.Id)
                None
                AssignmentLineage.Loaded
        ]

    let fanLayer =
        layer "fan-layer" "Fan out" (source "fan-source" "fan-table") [ "node-fan-input", "Fan Input" ] [
            "node-fan-out-a", "Fan Output A"
            "node-fan-out-b", "Fan Output B"
        ] [ processA.Id; processB.Id ]

    session
        [ fanLayer ]
        [
            node "node-fan-input" "Fan Input" [
                // Two owning assignments behind one displayed grouping value:
                // the entity card shows Species: Arabidopsis once and an edit
                // there covers both.
                nodeAssignment "assignment-fan-species-one" arabidopsis.Id AssignmentPropertyKind.Generic
                nodeAssignment "assignment-fan-species-two" arabidopsis.Id AssignmentPropertyKind.Generic
            ]
            node "node-fan-out-a" "Fan Output A" []
            node "node-fan-out-b" "Fan Output B" []
        ]
        [ processA; processB ] [ species; setting; recipeCategory ] [ arabidopsis; setting37; containerDefinition ]

/// A minimal reverse-connection-local scenario: Output A owns a node
/// annotation and Input A is its direct upstream neighbour on a Between link,
/// so Input A sees that annotation as ReverseConnectionLocal grouping-only
/// evidence (design §4: "grouping-only... not editable there"). Input B is
/// connected to an unannotated Output B, so it stays a control proving the
/// reflection does not leak beyond the direct neighbour.
let createReverseLocalSession () =
    let outcome = property "property-outcome" (term "Outcome" None)

    let success =
        value "value-outcome-success" outcome.Id (ProvenanceValue.Text "Success")

    let assignment =
        nodeAssignment "assignment-outcome-output-a" success.Id AssignmentPropertyKind.Generic

    let linkA =
        processLink "link-reverse-a" (ProcessLinkShape.Between("node-reverse-input-a", "node-reverse-output-a"))

    let linkB =
        processLink "link-reverse-b" (ProcessLinkShape.Between("node-reverse-input-b", "node-reverse-output-b"))

    let reverseProcess =
        structuralProcess "process-reverse" "reverse-layer" [ linkA; linkB ] []

    let reverseLayer =
        layer
            "reverse-layer"
            "Reverse local"
            (source "reverse-source" "Reverse local")
            [
                "node-reverse-input-a", "Input A"
                "node-reverse-input-b", "Input B"
            ] [
                "node-reverse-output-a", "Output A"
                "node-reverse-output-b", "Output B"
            ] [ reverseProcess.Id ]

    session
        [ reverseLayer ]
        [
            node "node-reverse-input-a" "Input A" []
            node "node-reverse-input-b" "Input B" []
            node "node-reverse-output-a" "Output A" [ assignment ]
            node "node-reverse-output-b" "Output B" []
        ]
        [ reverseProcess ] [ outcome ] [ success ]

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

    let catalog =
        Map.ofList [
            (firstReference.Scheme, firstReference.Id), firstEntry
            (secondReference.Scheme, secondReference.Id), secondEntry
        ]

    // Projected with the catalog from the start, exactly as a real load
    // resolves it through the access boundary - unlike a bare `session []`,
    // which the "layer created after load" .NET suite deliberately uses to
    // pin the *other* half of the same design rule (a layer with no cached
    // shelf recovers the catalog only from what the host supplies on refresh).
    let fixtureSession =
        sessionWithCatalog
            catalog
            [ referenceLayer ]
            [
                node "node-input" "Input" []
                node "node-output" "Output" []
            ]
            [ referenceProcess ] [ recipeCategory; componentCategory ] [ referenceDefinition; componentDefinition ]

    fixtureSession, catalog

let allSessions () = [
    createSharedNodeSession ()
    createSiblingLeakSession ()
    createAllLinkShapesSession ()
    createFanOutSession ()
    createReverseLocalSession ()
    createReferenceCatalogSession () |> fst
]

// ── Story fixtures ──────────────────────────────────────────────────────────
//
// Canonical replacements for the ten `Types/Fixtures.fs` models that every
// ProvenanceGrouping story renders. Two rules fixed their shape, and both are
// load-bearing for the stories rather than cosmetic:
//
// 1. Endpoint kind IDs stay `fixture:endpoint:sample` / `fixture:endpoint:data`
//    and layer IDs stay `layer-1`, `layer-2`, … because stories assert on both
//    (`LayerEndpointAdded:fixture:endpoint:data:Data`,
//    `provenance-layer-layer-1`).
// 2. A property becomes a node assignment when its fixture kind is a
//    characteristic and a process assignment when it is a parameter, following
//    the canonical load rule. A process assignment is incident-process
//    availability on *every* endpoint of its covered links, which is what keeps
//    the existing grouping stories valid: `Temperature` still reaches Input A/B/C
//    and `Analysis` still reaches Output A/B/C, now as `ProcessValue` keys
//    carrying an origin source.

module FixtureKinds =

    let private endpointKind id label : ProvenanceKind = {
        Id = $"fixture:endpoint:{id}"
        Label = label
    }

    let private propertyKind id label : ProvenanceKind = {
        Id = $"fixture:property:{id}"
        Label = label
    }

    let sampleEndpoint = endpointKind "sample" "Sample"
    let dataEndpoint = endpointKind "data" "Data"
    let characteristic = propertyKind "characteristic" "Characteristic"
    let parameter = propertyKind "parameter" "Parameter"

let private plainTerm name : ProvenanceTerm = {
    Name = name
    TermSource = None
    TermAccession = None
}

let private storyProperty id name : PropertyDefinition = { Id = id; Category = plainTerm name }

let private storyValue id propertyId content unit : PropertyValueDefinition = {
    Id = id
    PropertyId = propertyId
    Value = content
    Unit = unit
}

let private storyNode (kind: ProvenanceKind) id name (assignments: NodeAssignment list) : CanonicalNode = {
    Id = id
    Key = { KindId = kind.Id; Name = name }
    Kind = kind
    Name = name
    Assignments =
        assignments
        |> List.map (fun assignment -> assignment.Id, assignment)
        |> Map.ofList
}

let private storyNodeAssignment id valueId concreteKind : NodeAssignment = {
    Id = id
    ValueId = valueId
    PropertyKind = AdapterSpecific concreteKind
    TargetSource = None
    Lineage = AssignmentLineage.Loaded
}

let private storyProcessAssignment id valueId concreteKind linkIds : ProcessAssignment = {
    Id = id
    ValueId = valueId
    PropertyKind = AdapterSpecific concreteKind
    CoveredLinkIds = Set.ofList linkIds
    ContainerReferenceValueId = None
    ReferenceSlotId = None
    Lineage = AssignmentLineage.Loaded
}

/// An appearance of a canonical node on one side of one layer. `kind` and
/// `text` are per-appearance, so the same node can be `Output [Sample Name]` in
/// one layer and `Input [Sample Name]` in the next.
let private storyEndpoint layerId side (kind: ProvenanceKind) text position nodeId : LayerEndpoint = {
    Key = {
        LayerId = layerId
        Side = side
        NodeId = nodeId
    }
    Header = { Kind = kind; Text = text }
    LayerOrderPosition = position
}

let private storyLayer
    id
    label
    (sourceRef: ProvenanceSourceRef)
    (inputs: (CanonicalNodeId * ProvenanceKind * string) list)
    (outputs: (CanonicalNodeId * ProvenanceKind * string) list)
    (processIds: StructuralProcessId list)
    : ProvenanceLayer =
    {
        Id = id
        Label = label
        Source = sourceRef
        InputEndpoints =
            inputs
            |> List.mapi (fun position (nodeId, kind, text) ->
                nodeId, storyEndpoint id ProvenanceSide.Input kind text position nodeId
            )
            |> Map.ofList
        OutputEndpoints =
            outputs
            |> List.mapi (fun position (nodeId, kind, text) ->
                nodeId, storyEndpoint id ProvenanceSide.Output kind text position nodeId
            )
            |> Map.ofList
        StructuralProcessIds = Set.ofList processIds
    }

let private inputHeaderText = "Input [Sample Name]"
let private outputHeaderText = "Output [Sample Name]"

let private sampleInput nodeId =
    nodeId, FixtureKinds.sampleEndpoint, inputHeaderText

let private sampleOutput nodeId =
    nodeId, FixtureKinds.sampleEndpoint, outputHeaderText

/// The main story fixture: one layer, `layer-1`, over the assay table.
///
/// The old model gave `Previous Treatment` a second source by attaching
/// previous-process context to Input A's own property IDs, which is one of the
/// defects this model removes. Adding a previous-study layer does not reproduce
/// it either: a node does not belong to a layer, it *appears* in layers, and
/// design §3.5 resolves a node shelf entry's colour from "its one owning node's
/// appearance sources". Making Input A appear in a second layer would therefore
/// add that layer's source to `Species` as well, because Input A owns it - a
/// test in `Projection.Tests.fs` caught exactly that. So `Previous Treatment`
/// stays a plain assay-sourced characteristic here, and the non-layer folder
/// colour it used to exercise moves to `createChainedSession`, where a boundary
/// node genuinely has two appearance sources.
let createSampleSession () : ProvenanceSession =
    let assaySource = source "fixture:assay-table" "assay-table"
    let species = storyProperty "property-species" "Species"
    let temperature = storyProperty "property-temperature" "Temperature"
    let analysis = storyProperty "property-analysis" "Analysis"
    let replicate = storyProperty "property-replicate" "Replicate"

    let previousTreatment =
        storyProperty "property-previous-treatment" "Previous Treatment"

    // Equal header, value and unit share one definition, so the three
    // Arabidopsis inputs reference a single value ID.
    let arabidopsis =
        storyValue "value-species-arabidopsis" species.Id (ProvenanceValue.Text "Arabidopsis") None

    let chlamydomonas =
        storyValue "value-species-chlamydomonas" species.Id (ProvenanceValue.Text "Chlamydomonas") None

    let temperature12 =
        storyValue "value-temperature-12" temperature.Id (ProvenanceValue.Text "12 C") None

    let temperature24 =
        storyValue "value-temperature-24" temperature.Id (ProvenanceValue.Text "24 C") None

    let analysisMs =
        storyValue "value-analysis-ms" analysis.Id (ProvenanceValue.Text "Mass Spectrometry") None

    let analysisLcms =
        storyValue "value-analysis-lcms" analysis.Id (ProvenanceValue.Text "LC-MS") None

    let replicateOne =
        storyValue "value-replicate-1" replicate.Id (ProvenanceValue.Text "1") None

    let replicateTwo =
        storyValue "value-replicate-2" replicate.Id (ProvenanceValue.Text "2") None

    let drought =
        storyValue "value-previous-treatment-drought" previousTreatment.Id (ProvenanceValue.Text "Drought") None

    let linkA =
        processLink "link-a" (ProcessLinkShape.Between("node-input-a", "node-output-a"))

    let linkB =
        processLink "link-b" (ProcessLinkShape.Between("node-input-a", "node-output-b"))

    let linkC =
        processLink "link-c" (ProcessLinkShape.Between("node-input-b", "node-output-b"))

    let linkD =
        processLink "link-d" (ProcessLinkShape.Between("node-input-c", "node-output-c"))

    let linkE =
        processLink "link-e" (ProcessLinkShape.Between("node-input-d", "node-output-d"))

    let assayProcess =
        structuralProcess "process-assay" "layer-1" [ linkA; linkB; linkC; linkD; linkE ] [
            // Temperature covers the links incident to the inputs that carried
            // it; Analysis covers those incident to its outputs.
            storyProcessAssignment "assignment-temperature-12" temperature12.Id FixtureKinds.parameter [
                linkA.Id
                linkB.Id
                linkC.Id
            ]
            storyProcessAssignment "assignment-temperature-24" temperature24.Id FixtureKinds.parameter [ linkD.Id ]
            storyProcessAssignment "assignment-analysis-ms" analysisMs.Id FixtureKinds.parameter [
                linkA.Id
                linkB.Id
                linkC.Id
            ]
            storyProcessAssignment "assignment-analysis-lcms" analysisLcms.Id FixtureKinds.parameter [ linkD.Id ]
            // Replicate is the one property the old fixture already modelled
            // link-wise, so it stays one assignment per link.
            storyProcessAssignment "assignment-replicate-1" replicateOne.Id FixtureKinds.parameter [ linkB.Id ]
            storyProcessAssignment "assignment-replicate-2" replicateTwo.Id FixtureKinds.parameter [ linkC.Id ]
        ]

    let assayLayer =
        storyLayer
            "layer-1"
            "assay-table"
            assaySource
            [
                sampleInput "node-input-a"
                sampleInput "node-input-b"
                sampleInput "node-input-c"
                sampleInput "node-input-d"
            ] [
                sampleOutput "node-output-a"
                sampleOutput "node-output-b"
                sampleOutput "node-output-c"
                sampleOutput "node-output-d"
                sampleOutput "node-output-e"
            ] [ assayProcess.Id ]

    session
        [ assayLayer ]
        [
            storyNode FixtureKinds.sampleEndpoint "node-input-a" "Input A" [
                storyNodeAssignment "assignment-species-input-a" arabidopsis.Id FixtureKinds.characteristic
                storyNodeAssignment "assignment-previous-treatment" drought.Id FixtureKinds.characteristic
            ]
            storyNode FixtureKinds.sampleEndpoint "node-input-b" "Input B" [
                storyNodeAssignment "assignment-species-input-b" arabidopsis.Id FixtureKinds.characteristic
            ]
            storyNode FixtureKinds.sampleEndpoint "node-input-c" "Input C" [
                storyNodeAssignment "assignment-species-input-c" arabidopsis.Id FixtureKinds.characteristic
            ]
            storyNode FixtureKinds.sampleEndpoint "node-input-d" "Input D" [
                storyNodeAssignment "assignment-species-input-d" chlamydomonas.Id FixtureKinds.characteristic
            ]
            storyNode FixtureKinds.sampleEndpoint "node-output-a" "Output A" []
            storyNode FixtureKinds.sampleEndpoint "node-output-b" "Output B" []
            storyNode FixtureKinds.sampleEndpoint "node-output-c" "Output C" []
            storyNode FixtureKinds.sampleEndpoint "node-output-d" "Output D" []
            storyNode FixtureKinds.sampleEndpoint "node-output-e" "Output E" []
        ]
        [ assayProcess ] [
            species
            temperature
            analysis
            replicate
            previousTreatment
        ] [
            arabidopsis
            chlamydomonas
            temperature12
            temperature24
            analysisMs
            analysisLcms
            replicateOne
            replicateTwo
            drought
        ]

/// A process-value drop is ambiguous only when several same-kind assignments
/// conflict on one exact target link (intent §3). The sample fixture's two
/// Replicate values live on different links, so this variant adds both to
/// `link-b` and provides a third, non-target source assignment on `link-d`.
let createAmbiguousProcessAssignmentSession () : ProvenanceSession =
    let current = createSampleSession ()
    let replicate = current.Properties["property-replicate"]

    let replicateThree =
        storyValue "value-replicate-3" replicate.Id (ProvenanceValue.Text "3") None

    let structuralProcess = current.Processes["process-assay"]

    let replicateTwo = {
        structuralProcess.Assignments["assignment-replicate-2"] with
            CoveredLinkIds = Set.ofList [ "link-b"; "link-c" ]
    }

    let replicateThreeAssignment =
        storyProcessAssignment "assignment-replicate-3" replicateThree.Id FixtureKinds.parameter [ "link-d" ]

    let structuralProcess = {
        structuralProcess with
            Assignments =
                structuralProcess.Assignments
                |> Map.add replicateTwo.Id replicateTwo
                |> Map.add replicateThreeAssignment.Id replicateThreeAssignment
    }

    {
        current with
            Processes = current.Processes |> Map.add structuralProcess.Id structuralProcess
            Values = current.Values |> Map.add replicateThree.Id replicateThree
            LayerProjections = Map.empty
    }
    |> projectAllLayers Map.empty

/// Two equally-sized grouped sides whose map and display-name order disagree
/// with LayerOrderPosition. The pair-by-order story can therefore observe the
/// only ordering signal the component is allowed to use.
let createLayerOrderSession () : ProvenanceSession =
    let orderSource = source "fixture:layer-order" "layer-order"
    let species = storyProperty "property-layer-order-species" "Species"

    let value =
        storyValue "value-layer-order-species" species.Id (ProvenanceValue.Text "Shared") None

    let assignment nodeId =
        storyNodeAssignment $"assignment-layer-order-{nodeId}" value.Id FixtureKinds.characteristic

    let orderedLayer =
        storyLayer
            "layer-1"
            "layer-order"
            orderSource
            [
                "node-input-z", FixtureKinds.sampleEndpoint, "Input A"
                "node-input-a", FixtureKinds.sampleEndpoint, "Input Z"
            ] [
                "node-output-z", FixtureKinds.sampleEndpoint, "Output A"
                "node-output-a", FixtureKinds.sampleEndpoint, "Output Z"
            ] []

    session
        [ orderedLayer ]
        [
            storyNode FixtureKinds.sampleEndpoint "node-input-z" "Input A" [ assignment "node-input-z" ]
            storyNode FixtureKinds.sampleEndpoint "node-input-a" "Input Z" [ assignment "node-input-a" ]
            storyNode FixtureKinds.sampleEndpoint "node-output-z" "Output A" [ assignment "node-output-z" ]
            storyNode FixtureKinds.sampleEndpoint "node-output-a" "Output Z" [ assignment "node-output-a" ]
        ]
        [] [ species ] [ value ]

/// Two independently loaded process groups whose boundary sample
/// `Culture Batch` is *one* canonical node appearing as the growth layer's
/// output and the measurement layer's input - the chaining the old model needed
/// reference links for.
let createChainedSession () : ProvenanceSession =
    let growthSource = source "fixture:growth-table" "growth-table"

    let measurementSource = source "fixture:measurement-table" "measurement-table"

    let temperature = storyProperty "property-temperature" "Temperature"
    let analysis = storyProperty "property-analysis" "Analysis"

    // Owned by the boundary node, so it is the fixture's non-layer source case:
    // in the measurement layer's shelf this entry resolves to the union of
    // Culture Batch's appearance sources, which includes the growth table.
    let batchOrigin = storyProperty "property-batch-origin" "Batch Origin"

    let growthTemperature =
        storyValue "value-growth-temperature" temperature.Id (ProvenanceValue.Text "21 °C") None

    let measurementAnalysis =
        storyValue "value-measurement-analysis" analysis.Id (ProvenanceValue.Text "LC-MS") None

    let batchOriginValue =
        storyValue "value-batch-origin" batchOrigin.Id (ProvenanceValue.Text "Greenhouse") None

    let growthLink =
        processLink "link-growth" (ProcessLinkShape.Between("node-seed", "node-culture"))

    let measurementLink =
        processLink "link-measurement" (ProcessLinkShape.Between("node-culture", "node-extract"))

    let growthProcess =
        structuralProcess "process-growth" "layer-1" [ growthLink ] [
            storyProcessAssignment "assignment-growth-temperature" growthTemperature.Id FixtureKinds.parameter [
                growthLink.Id
            ]
        ]

    let measurementProcess =
        structuralProcess "process-measurement" "layer-2" [ measurementLink ] [
            storyProcessAssignment "assignment-measurement-analysis" measurementAnalysis.Id FixtureKinds.parameter [
                measurementLink.Id
            ]
        ]

    let growthLayer =
        storyLayer "layer-1" "growth-table" growthSource [ sampleInput "node-seed" ] [ sampleOutput "node-culture" ] [
            growthProcess.Id
        ]

    let measurementLayer =
        storyLayer "layer-2" "measurement-table" measurementSource [ sampleInput "node-culture" ] [
            sampleOutput "node-extract"
        ] [ measurementProcess.Id ]

    session
        [ growthLayer; measurementLayer ]
        [
            storyNode FixtureKinds.sampleEndpoint "node-seed" "Seed Stock" []
            storyNode FixtureKinds.sampleEndpoint "node-culture" "Culture Batch" [
                storyNodeAssignment "assignment-batch-origin" batchOriginValue.Id FixtureKinds.characteristic
            ]
            storyNode FixtureKinds.sampleEndpoint "node-extract" "Extract Batch" []
        ]
        [ growthProcess; measurementProcess ] [ temperature; analysis; batchOrigin ] [
            growthTemperature
            measurementAnalysis
            batchOriginValue
        ]

/// Provides a loaded Analysis value from the upstream process so the second
/// layer can copy an exact adapter-specific assignment onto its own process.
/// A newly authored Generic draft is intentionally a different kind-bearing
/// entry and therefore must not overwrite the loaded Parameter (intent §3).
let createChainedAlternateAnalysisSession () : ProvenanceSession =
    let current = createChainedSession ()
    let analysis = current.Properties["property-analysis"]

    let imaging =
        storyValue "value-growth-analysis-imaging" analysis.Id (ProvenanceValue.Text "Imaging") None

    let growthProcess = current.Processes["process-growth"]

    let imagingAssignment =
        storyProcessAssignment "assignment-growth-analysis-imaging" imaging.Id FixtureKinds.parameter [ "link-growth" ]

    let growthProcess = {
        growthProcess with
            Assignments = growthProcess.Assignments |> Map.add imagingAssignment.Id imagingAssignment
    }

    {
        current with
            Processes = current.Processes |> Map.add growthProcess.Id growthProcess
            Values = current.Values |> Map.add imaging.Id imaging
            LayerProjections = Map.empty
    }
    |> projectAllLayers Map.empty

let createInputOnlySession () : ProvenanceSession =
    let inputOnlySource = source "fixture:input-only-table" "input-only-table"
    let species = storyProperty "property-species" "Species"

    let arabidopsis =
        storyValue "value-input-only-species" species.Id (ProvenanceValue.Text "Arabidopsis") None

    let inputOnlyLayer =
        storyLayer "layer-1" "input-only-table" inputOnlySource [ sampleInput "node-input-only-a" ] [] []

    session
        [ inputOnlyLayer ]
        [
            storyNode FixtureKinds.sampleEndpoint "node-input-only-a" "Input Only A" [
                storyNodeAssignment "assignment-input-only-species" arabidopsis.Id FixtureKinds.characteristic
            ]
        ]
        [] [ species ] [ arabidopsis ]

/// The parameter has no connection to sit on, so it rides an `OutputOnly` link -
/// the canonical shape for a one-sided process, which the old model could not
/// express and approximated by attaching the value to a lone set.
let createOutputOnlySession () : ProvenanceSession =
    let outputOnlySource = source "fixture:output-only-table" "output-only-table"
    let analysis = storyProperty "property-analysis" "Analysis"

    let lcms =
        storyValue "value-output-only-analysis" analysis.Id (ProvenanceValue.Text "LC-MS") None

    let link =
        processLink "link-output-only" (ProcessLinkShape.OutputOnly "node-output-only-a")

    let outputOnlyProcess =
        structuralProcess "process-output-only" "layer-1" [ link ] [
            storyProcessAssignment "assignment-output-only-analysis" lcms.Id FixtureKinds.parameter [ link.Id ]
        ]

    let outputOnlyLayer =
        storyLayer "layer-1" "output-only-table" outputOnlySource [] [ sampleOutput "node-output-only-a" ] [
            outputOnlyProcess.Id
        ]

    session
        [ outputOnlyLayer ]
        [
            storyNode FixtureKinds.sampleEndpoint "node-output-only-a" "Output Only A" []
        ]
        [ outputOnlyProcess ] [ analysis ] [ lcms ]

let createDisconnectedPropertySession () : ProvenanceSession =
    let disconnectedSource =
        source "fixture:disconnected-property-table" "disconnected-property-table"

    let analysis = storyProperty "property-analysis" "Analysis"
    let replicate = storyProperty "property-replicate" "Replicate"

    let massSpectrometry =
        storyValue "value-disconnected-analysis" analysis.Id (ProvenanceValue.Text "Mass Spectrometry") None

    let replicateOne =
        storyValue "value-disconnected-replicate" replicate.Id (ProvenanceValue.Text "1") None

    let link =
        processLink "link-disconnected-output" (ProcessLinkShape.OutputOnly "node-disconnected-output")

    let disconnectedProcess =
        structuralProcess "process-disconnected" "layer-1" [ link ] [
            storyProcessAssignment "assignment-disconnected-analysis" massSpectrometry.Id FixtureKinds.parameter [
                link.Id
            ]
            storyProcessAssignment "assignment-disconnected-replicate" replicateOne.Id FixtureKinds.parameter [
                link.Id
            ]
        ]

    let disconnectedLayer =
        storyLayer "layer-1" "disconnected-property-table" disconnectedSource [ sampleInput "node-disconnected-input" ] [
            sampleOutput "node-disconnected-output"
        ] [ disconnectedProcess.Id ]

    session
        [ disconnectedLayer ]
        [
            storyNode FixtureKinds.sampleEndpoint "node-disconnected-input" "Disconnected Input" []
            storyNode FixtureKinds.sampleEndpoint "node-disconnected-output" "Disconnected Output" []
        ]
        [ disconnectedProcess ] [ analysis; replicate ] [ massSpectrometry; replicateOne ]

/// `Batch` is a parameter, so it is a process annotation and therefore incident
/// on both endpoints of its link. That incidence is exactly what makes it
/// "switchable": the same annotation can be grouped from either side.
let createSwitchablePropertySession () : ProvenanceSession =
    let switchableSource = source "fixture:switchable-table" "switchable-table"
    let batch = storyProperty "property-batch" "Batch"

    let batchA = storyValue "value-batch-a" batch.Id (ProvenanceValue.Text "A") None
    let batchB = storyValue "value-batch-b" batch.Id (ProvenanceValue.Text "B") None

    let linkA =
        processLink "link-switchable-a" (ProcessLinkShape.Between("node-input-a", "node-output-a"))

    let linkB =
        processLink "link-switchable-b" (ProcessLinkShape.Between("node-input-b", "node-output-b"))

    let switchableProcess =
        structuralProcess "process-switchable" "layer-1" [ linkA; linkB ] [
            storyProcessAssignment "assignment-batch-a" batchA.Id FixtureKinds.parameter [ linkA.Id ]
            storyProcessAssignment "assignment-batch-b" batchB.Id FixtureKinds.parameter [ linkB.Id ]
        ]

    let switchableLayer =
        storyLayer
            "layer-1"
            "switchable-table"
            switchableSource
            [ sampleInput "node-input-a"; sampleInput "node-input-b" ] [
                sampleOutput "node-output-a"
                sampleOutput "node-output-b"
            ] [ switchableProcess.Id ]

    session
        [ switchableLayer ]
        [
            storyNode FixtureKinds.sampleEndpoint "node-input-a" "Input A" []
            storyNode FixtureKinds.sampleEndpoint "node-input-b" "Input B" []
            storyNode FixtureKinds.sampleEndpoint "node-output-a" "Output A" []
            storyNode FixtureKinds.sampleEndpoint "node-output-b" "Output B" []
        ]
        [ switchableProcess ] [ batch ] [ batchA; batchB ]

let private degreeCelsius = {
    Name = "degree Celsius"
    TermSource = Some "UO"
    TermAccession = Some "UO:0000027"
}

let private instrumentTerm = {
    Name = "mass spectrometer"
    TermSource = Some "OBI"
    TermAccession = Some "OBI:0000049"
}

/// The sample fixture with typed values: Input A's temperature becomes a
/// `Float` with a unit, which splits it off the shared `12 C` definition that
/// still covers Input B, and Output A gains a `Term`-valued instrument.
let private typedSampleSessionWith (instrumentValue: ProvenanceTerm) : ProvenanceSession =
    let baseSession = createSampleSession ()
    let temperatureProperty = baseSession.Properties["property-temperature"]

    let instrumentProperty = storyProperty "property-instrument" "Instrument"

    let typedTemperature =
        storyValue "value-temperature-typed" temperatureProperty.Id (ProvenanceValue.Float 12.0) (Some degreeCelsius)

    let instrument =
        storyValue "value-instrument" instrumentProperty.Id (ProvenanceValue.Term instrumentValue) None

    let assayProcess = baseSession.Processes["process-assay"]

    let retypedAssignments =
        assayProcess.Assignments
        // Input A's two links carry the typed temperature; Input B's link keeps
        // the original textual definition.
        |> Map.add "assignment-temperature-12" {
            assayProcess.Assignments["assignment-temperature-12"] with
                CoveredLinkIds = Set.ofList [ "link-c" ]
        }
        |> Map.add
            "assignment-temperature-typed"
            (storyProcessAssignment "assignment-temperature-typed" typedTemperature.Id FixtureKinds.parameter [
                "link-a"
                "link-b"
            ])
        |> Map.add
            "assignment-instrument"
            (storyProcessAssignment "assignment-instrument" instrument.Id FixtureKinds.parameter [ "link-a" ])

    // Re-project: this rewrites `assignment-temperature-12`'s coverage on top of
    // an already-projected base session, so the inherited projections describe
    // the untyped shape and `prepareForWriteback` would reject them as
    // conflicting with canonical state.
    {
        baseSession with
            Processes =
                baseSession.Processes
                |> Map.add assayProcess.Id {
                    assayProcess with
                        Assignments = retypedAssignments
                }
            Properties = baseSession.Properties |> Map.add instrumentProperty.Id instrumentProperty
            Values =
                baseSession.Values
                |> Map.add typedTemperature.Id typedTemperature
                |> Map.add instrument.Id instrument
    }
    |> projectAllLayers Map.empty

let createTypedSampleSession () : ProvenanceSession = typedSampleSessionWith instrumentTerm

/// The controlled-replacement fixture: the same session with the instrument
/// term re-tagged, so a story can replace it from outside and assert the rail
/// refreshes.
let createRetaggedTypedSampleSession () : ProvenanceSession =
    typedSampleSessionWith {
        Name = "mass spectrometer"
        TermSource = Some "MS"
        TermAccession = Some "MS:1000031"
    }

let createDataOutputOnlySession () : ProvenanceSession =
    let dataSource = source "fixture:data-output-only-table" "data-output-only-table"

    let analysis = storyProperty "property-analysis" "Analysis"

    let lcms =
        storyValue "value-data-output-analysis" analysis.Id (ProvenanceValue.Text "LC-MS") None

    let link =
        processLink "link-data-output" (ProcessLinkShape.OutputOnly "node-data-output-a")

    let dataProcess =
        structuralProcess "process-data-output" "layer-1" [ link ] [
            storyProcessAssignment "assignment-data-output-analysis" lcms.Id FixtureKinds.parameter [ link.Id ]
        ]

    let dataLayer =
        storyLayer "layer-1" "data-output-only-table" dataSource [] [
            "node-data-output-a", FixtureKinds.dataEndpoint, "Output [Data]"
        ] [ dataProcess.Id ]

    session
        [ dataLayer ]
        [
            storyNode FixtureKinds.dataEndpoint "node-data-output-a" "Data Output A" []
        ]
        [ dataProcess ] [ analysis ] [ lcms ]

let private performanceInputId layerIndex i = sprintf "perf-input-%d-%d" layerIndex i

let private performanceOutputId layerIndex i =
    sprintf "perf-output-%d-%d" layerIndex i

let private performanceNodeValueId layerIndex i =
    sprintf "perf-value-node-%d-%d" layerIndex i

/// Step L.1's fan-in count: every connectable output in a performance layer
/// fans in to this many inputs.
let performanceFanIn (nodesPerSide: int) (edgeDensity: float) =
    max 1 (int (edgeDensity * float nodesPerSide + 0.5))

let private buildPerformanceLayer nodeCategoryId (processValue: PropertyValueDefinition) nodesPerSide fanIn layerIndex =
    let layerId = sprintf "perf-layer-%d" layerIndex
    let connectable = nodesPerSide - 1
    let inputIds = [ 0 .. nodesPerSide - 1 ] |> List.map (performanceInputId layerIndex)

    let outputIds =
        [ 0 .. nodesPerSide - 1 ] |> List.map (performanceOutputId layerIndex)

    let inputValues =
        [ 0 .. nodesPerSide - 1 ]
        |> List.map (fun i ->
            value
                (performanceNodeValueId layerIndex i)
                nodeCategoryId
                (ProvenanceValue.Text(sprintf "Batch-%d-%d" layerIndex i))
        )

    let inputAssignments =
        List.zip inputIds inputValues
        |> List.mapi (fun i (inputId, definition) ->
            inputId,
            nodeAssignment
                (sprintf "perf-assignment-node-%d-%d" layerIndex i)
                definition.Id
                AssignmentPropertyKind.Generic
        )
        |> Map.ofList

    let nodes =
        (inputIds
         |> List.map (fun inputId -> node inputId inputId [ inputAssignments[inputId] ]))
        @ (outputIds |> List.map (fun outputId -> node outputId outputId []))

    // The last input/output index in every layer is deliberately left out of
    // every fan-in below, so the active layer always has one genuinely
    // unconnected pair for the structural-edit benchmark scenario to connect.
    let processes =
        [ 0 .. connectable - 1 ]
        |> List.collect (fun o ->
            [ 0 .. fanIn - 1 ]
            |> List.map (fun k ->
                let inputId = inputIds[(o + k) % connectable]
                let outputId = outputIds[o]
                let linkId = sprintf "perf-link-%d-%d-%d" layerIndex o k

                let processLinkItem =
                    processLink linkId (ProcessLinkShape.Between(inputId, outputId))

                let assignment =
                    processAssignment
                        (sprintf "perf-assignment-link-%d-%d-%d" layerIndex o k)
                        processValue.Id
                        [ linkId ]
                        None
                        None
                        AssignmentLineage.Loaded

                structuralProcess (sprintf "perf-process-%d-%d-%d" layerIndex o k) layerId [ processLinkItem ] [
                    assignment
                ]
            )
        )

    let provenanceLayer =
        layer
            layerId
            (sprintf "Performance layer %d" layerIndex)
            (source (sprintf "perf-source-%d" layerIndex) (sprintf "Performance source %d" layerIndex))
            (inputIds |> List.map (fun inputId -> inputId, inputId))
            (outputIds |> List.map (fun outputId -> outputId, outputId))
            (processes |> List.map _.Id)

    provenanceLayer, nodes, processes, inputValues

/// Step L.1's benchmark workload: `layers` layers of `nodesPerSide` input and
/// output nodes each, with every connectable output fanned in to
/// `performanceFanIn nodesPerSide edgeDensity` inputs. Every input owns one
/// value; outputs own none, so their availability comes entirely from forward
/// propagation - the traversal the benchmark exists to measure. Only the
/// active (first) layer is projected: `Availability` resolution never reads
/// `LayerProjections`, and a session that has not switched to its other
/// layers would not have them projected either, so eagerly projecting every
/// layer here would only cost time the benchmark never measures.
let createPerformanceSession (layers: int) (nodesPerSide: int) (edgeDensity: float) : ProvenanceSession =
    let fanIn = performanceFanIn nodesPerSide edgeDensity
    let nodeCategory = property "perf-property-node" (term "Batch" None)
    let processCategory = property "perf-property-process" (term "Process marker" None)

    let processValue =
        value "perf-value-process" processCategory.Id (ProvenanceValue.Text "Process")

    let built =
        [ 0 .. layers - 1 ]
        |> List.map (buildPerformanceLayer nodeCategory.Id processValue nodesPerSide fanIn)

    let builtLayers = built |> List.map (fun (layerItem, _, _, _) -> layerItem)
    let nodes = built |> List.collect (fun (_, nodeItems, _, _) -> nodeItems)
    let processes = built |> List.collect (fun (_, _, processItems, _) -> processItems)
    let values = built |> List.collect (fun (_, _, _, valueItems) -> valueItems)

    let builtSession = {
        empty with
            Nodes = nodes |> List.map (fun item -> item.Id, item) |> Map.ofList
            Processes = processes |> List.map (fun item -> item.Id, item) |> Map.ofList
            Properties =
                Map.ofList [
                    nodeCategory.Id, nodeCategory
                    processCategory.Id, processCategory
                ]
            Values = (processValue :: values) |> List.map (fun item -> item.Id, item) |> Map.ofList
            Layers = builtLayers |> List.map (fun item -> item.Id, item) |> Map.ofList
            LayerOrder = builtLayers |> List.map _.Id
            ActiveLayerId = builtLayers.Head.Id
    }

    match projectLayer builtSession.ActiveLayerId Map.empty builtSession with
    | Ok projection -> {
        builtSession with
            LayerProjections = Map.ofList [ builtSession.ActiveLayerId, projection ]
      }
    | Error error -> failwithf "Performance workload produced an invalid layer projection: %A" error

/// The value ID owned by input 0 of the given layer - the value-only edit
/// scenario's target.
let performanceEditValueId (layerIndex: int) : PropertyValueDefinitionId = performanceNodeValueId layerIndex 0

/// The last output in the given layer, which `createPerformanceSession` never
/// assigns - the assignment-addition scenario's target.
let performanceUnassignedNodeId (layerIndex: int) (nodesPerSide: int) : CanonicalNodeId =
    performanceOutputId layerIndex (nodesPerSide - 1)

/// The last input/output pair in the given layer, which `createPerformanceSession`
/// never connects - the structural-edit scenario's target.
let performanceUnconnectedPair (layerIndex: int) (nodesPerSide: int) : CanonicalNodeId * CanonicalNodeId =
    performanceInputId layerIndex (nodesPerSide - 1), performanceOutputId layerIndex (nodesPerSide - 1)

/// A host-declared endpoint capability list, for the stories that create an
/// endpoint of a kind the loaded session does not already contain.
let sampleAndDataEndpointKinds () : ProvenanceKind list = [ FixtureKinds.sampleEndpoint; FixtureKinds.dataEndpoint ]

/// Renders the semantic mutation journal for Storybook assertions. This
/// replaces the old `PatchPreview`: the journal, not a patch list, is the
/// unsaved-change record, and it survives undo for free because undo restores a
/// whole prior session.
module JournalPreview =

    let private valueKind =
        function
        | ProvenanceValue.Text _ -> "Text"
        | ProvenanceValue.Integer _ -> "Integer"
        | ProvenanceValue.Float _ -> "Float"
        | ProvenanceValue.Term _ -> "Term"
        | ProvenanceValue.Reference _ -> "Reference"

    let private unitName (unit': ProvenanceTerm option) =
        unit' |> Option.map _.Name |> Option.defaultValue "none"

    let private valueMetadata =
        function
        | ProvenanceValue.Term term ->
            let source = term.TermSource |> Option.defaultValue "none"
            let accession = term.TermAccession |> Option.defaultValue "none"
            $":{source}:{accession}"
        | ProvenanceValue.Reference reference -> $":{reference.Scheme}:{reference.Id}"
        | _ -> ""

    let private definitionDetail (definition: PropertyValueDefinition) =
        $"{valueKind definition.Value}:{unitName definition.Unit}{valueMetadata definition.Value}"

    let private valueOf (session: ProvenanceSession) valueId =
        session.Values
        |> Map.tryFind valueId
        |> Option.map definitionDetail
        |> Option.defaultValue "unknown"

    let journalDetail (session: ProvenanceSession) =
        function
        | CanonicalNodeCreated node -> $"CanonicalNodeCreated:{node.Kind.Id}:{node.Kind.Label}"
        | LayerEndpointAdded endpoint -> $"LayerEndpointAdded:{endpoint.Header.Kind.Id}:{endpoint.Header.Kind.Label}"
        | StructuralProcessCreated _ -> "StructuralProcessCreated"
        | StructuralProcessReshaped _ -> "StructuralProcessReshaped"
        | ProcessLinkAdded(_, link) ->
            match link.Shape with
            | Between(inputId, outputId) -> $"ProcessLinkAdded:{inputId}->{outputId}"
            | InputOnly inputId -> $"ProcessLinkAdded:{inputId}->"
            | OutputOnly outputId -> $"ProcessLinkAdded:->{outputId}"
            | Endpointless -> "ProcessLinkAdded:endpointless"
        | ProcessLinkRemoved _ -> "ProcessLinkRemoved"
        | PropertyDefinitionCreated _ -> "PropertyDefinitionCreated"
        | PropertyDefinitionUpdated _ -> "PropertyDefinitionUpdated"
        | PropertyValueDefinitionCreated definition -> $"PropertyValueDefinitionCreated:{definitionDetail definition}"
        | PropertyValueDefinitionUpdated(_, after, _) -> $"PropertyValueDefinitionUpdated:{definitionDetail after}"
        | PropertyValueDefinitionDeleted _ -> "PropertyValueDefinitionDeleted"
        | PropertyDefinitionDeleted _ -> "PropertyDefinitionDeleted"
        | NodeAssignmentAdded(_, assignment, _) -> $"NodeAssignmentAdded:{valueOf session assignment.ValueId}"
        | NodeAssignmentValueChanged(_, _, after, _) -> $"NodeAssignmentValueChanged:{valueOf session after.ValueId}"
        | NodeAssignmentRemoved _ -> "NodeAssignmentRemoved"
        | ProcessAssignmentAdded(_, assignment, _) ->
            let coveredLinks = assignment.CoveredLinkIds |> Set.toList |> String.concat ","
            $"ProcessAssignmentAdded:{valueOf session assignment.ValueId}:links={coveredLinks}"
        | ProcessAssignmentCoverageChanged _ -> "ProcessAssignmentCoverageChanged"
        | ProcessAssignmentValueChanged(_, _, after, _) ->
            $"ProcessAssignmentValueChanged:{valueOf session after.ValueId}"
        | ProcessAssignmentSplit _ -> "ProcessAssignmentSplit"
        | ProcessAssignmentRemoved _ -> "ProcessAssignmentRemoved"
        | AdapterResourceReferenceReplaced _ -> "AdapterResourceReferenceReplaced"

    let journalDetails (session: ProvenanceSession) =
        session.MutationJournal |> List.map (journalDetail session) |> ResizeArray

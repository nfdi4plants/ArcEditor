module CanonicalModelTests

open Expecto
open Swate.Components.Page.ProvenanceGrouping.Identifiers
open Swate.Components.Page.ProvenanceGrouping.Values
open Swate.Components.Page.ProvenanceGrouping.Domain
open Swate.Components.Page.ProvenanceGrouping.MutationTypes
open Swate.Components.Page.ProvenanceGrouping.ProjectionTypes
open Swate.Components.Page.ProvenanceGrouping.Model
open Swate.Components.Page.ProvenanceGrouping.Session
open Swate.Components.Page.ProvenanceGrouping.StoryFixtures
open Swate.Components.Util.DurableIdDisambiguation

let private sampleKind = {
    Id = "canonical:endpoint:sample"
    Label = "Sample"
}

let private dataKind = {
    Id = "canonical:endpoint:data"
    Label = "Data"
}

let private source id = { Id = id; Name = id }

let private layer id sourceId = {
    Id = id
    Label = id
    Source = source sourceId
    InputEndpoints = Map.empty
    OutputEndpoints = Map.empty
    StructuralProcessIds = Set.empty
}

let private withLayers (layers: ProvenanceLayer list) (session: ProvenanceSession) = {
    session with
        Layers = layers |> List.map (fun item -> item.Id, item) |> Map.ofList
        LayerOrder = layers |> List.map _.Id
        ActiveLayerId = layers.Head.Id
}

let private endpoint layerId side nodeId headerText position = {
    Key = {
        LayerId = layerId
        Side = side
        NodeId = nodeId
    }
    Header = { Kind = sampleKind; Text = headerText }
    LayerOrderPosition = position
}

let private expectOk =
    function
    | Ok value -> value
    | Error error -> failtestf "Expected Ok but received %A" error

let private emptyProcess id layerId = {
    Id = id
    OriginLayerId = layerId
    Name = Some id
    Links = Map.empty
    Assignments = Map.empty
}

let private processLink id shape = { Id = id; Shape = shape }

let private processAssignment id coveredLinkIds = {
    Id = id
    ValueId = "value-one"
    PropertyKind = AssignmentPropertyKind.Generic
    CoveredLinkIds = coveredLinkIds
    ContainerReferenceValueId = None
    ReferenceSlotId = None
    Lineage = AssignmentLineage.Created
}

let private category name source accession = {
    Name = name
    TermSource = source
    TermAccession = accession
}

let private installPreparation (preparation: ValueDefinitionPreparation) (session: ProvenanceSession) = {
    session with
        Properties =
            session.Properties
            |> Map.add preparation.PropertyDefinition.Id preparation.PropertyDefinition
        Values =
            session.Values
            |> Map.add preparation.ValueDefinition.Id preparation.ValueDefinition
}

let private nodeAssignment id valueId = {
    Id = id
    ValueId = valueId
    PropertyKind = AssignmentPropertyKind.Generic
    TargetSource = None
    Lineage = AssignmentLineage.Created
}

let private catalogEntry scheme id label = {
    Category = category "Protocol reference" (Some "ARC") (Some "ARC:Protocol")
    Reference = {
        Scheme = scheme
        Id = id
        Label = label
    }
    Unit = None
    AssignmentKind = AnnotationOwnerKind.Process
    PropertyKind = AssignmentPropertyKind.Generic
    Cardinality = ReferenceCardinality.Many
    DependentProcessValues = []
}

let private expectInvariantError =
    function
    | Error(InconsistentCanonicalState details) ->
        Expect.isNotEmpty details "Invariant errors must explain the rejected state."
    | Error error -> failtestf "Expected InconsistentCanonicalState but received %A" error
    | Ok _ -> failtest "Expected InconsistentCanonicalState but operation succeeded."

let private identityTests =
    testList "canonical identity" [
        testCase "equal kind and name resolve to one canonical node across side, layer and source"
        <| fun _ ->
            let initial =
                empty
                |> withLayers [
                    layer "layer-one" "source-one"
                    layer "layer-two" "source-two"
                ]

            let nodeId, afterFirstEnsure = ensureNode sampleKind "S1" initial

            let assignment = {
                Id = "assignment-one"
                ValueId = "value-one"
                PropertyKind = AssignmentPropertyKind.Generic
                TargetSource = None
                Lineage = AssignmentLineage.Created
            }

            let assigned = {
                afterFirstEnsure with
                    Nodes =
                        afterFirstEnsure.Nodes
                        |> Map.change
                            nodeId
                            (Option.map (fun node -> {
                                node with
                                    Assignments = node.Assignments |> Map.add assignment.Id assignment
                            }))
            }

            let appearances = [
                "layer-one", ProvenanceSide.Input, "input-one", 0
                "layer-one", ProvenanceSide.Output, "output-one", 1
                "layer-two", ProvenanceSide.Input, "input-two", 7
                "layer-two", ProvenanceSide.Output, "output-two", 4
            ]

            let finalSession, resolvedIds =
                appearances
                |> List.fold
                    (fun (session, ids) (layerId, side, header, order) ->
                        let resolvedId, afterEnsure = ensureNode sampleKind "S1" session

                        let afterEndpoint =
                            addLayerEndpoint (endpoint layerId side resolvedId header order) afterEnsure
                            |> expectOk

                        afterEndpoint, resolvedId :: ids
                    )
                    (assigned, [])

            Expect.equal (resolvedIds |> Set.ofList) (Set.singleton nodeId) "All appearances must share one node."
            Expect.equal finalSession.Nodes.Count 1 "Only one canonical node must exist."
            Expect.equal finalSession.Nodes[nodeId].Assignments.Count 1 "Node assignments must remain stored once."

            Expect.equal
                (nodeAppearances finalSession nodeId |> List.length)
                4
                "All four appearances must be discoverable."

        testCase "names differing only in case are distinct nodes"
        <| fun _ ->
            let upperId, session = ensureNode sampleKind "S1" empty
            let lowerId, session = ensureNode sampleKind "s1" session
            Expect.notEqual upperId lowerId "Case must be significant."
            Expect.equal session.Nodes.Count 2 "Both nodes must be retained."

        testCase "names differing only in leading or trailing whitespace are distinct nodes"
        <| fun _ ->
            let plainId, session = ensureNode sampleKind "S1" empty
            let leadingId, session = ensureNode sampleKind " S1" session
            let trailingId, session = ensureNode sampleKind "S1 " session

            Expect.equal
                ([ plainId; leadingId; trailingId ] |> Set.ofList |> Set.count)
                3
                "Whitespace must be significant."

            Expect.equal session.Nodes.Count 3 "All exact names must be retained."

        testCase "different endpoint kinds with the same name are distinct nodes"
        <| fun _ ->
            let sampleId, session = ensureNode sampleKind "S1" empty
            let dataId, session = ensureNode dataKind "S1" session
            Expect.notEqual sampleId dataId "Kind ID must participate in canonical identity."
            Expect.equal session.Nodes.Count 2 "Both kinds must be retained."

        testCase "differently normalized Unicode names are distinct nodes"
        <| fun _ ->
            let precomposed = "\u00E9"
            let decomposed = "e\u0301"
            let precomposedId, session = ensureNode sampleKind precomposed empty
            let decomposedId, session = ensureNode sampleKind decomposed session
            Expect.notEqual precomposedId decomposedId "Unicode normalization must not be applied."
            Expect.equal session.Nodes.Count 2 "Both exact Unicode strings must be retained."

        testCase "names differing only in a data selector are distinct nodes"
        <| fun _ ->
            let fileId, session = ensureNode dataKind "f.csv" empty
            let rowId, session = ensureNode dataKind "f.csv#row=2" session
            Expect.notEqual fileId rowId "A selector is part of the exact name."
            Expect.equal session.Nodes.Count 2 "Both data identities must be retained."
    ]

let private endpointTests =
    testList "layer endpoints" [
        testCase "a layer endpoint is identified by layer, side and node only"
        <| fun _ ->
            let initial =
                empty
                |> withLayers [
                    layer "layer-one" "source-one"
                    layer "layer-two" "source-two"
                ]

            let nodeId, session = ensureNode sampleKind "S1" initial
            let first = endpoint "layer-one" ProvenanceSide.Input nodeId "first header" 2
            let afterFirst = addLayerEndpoint first session |> expectOk

            let duplicate = endpoint "layer-one" ProvenanceSide.Input nodeId "changed header" 99

            Expect.equal
                (addLayerEndpoint duplicate afterFirst)
                (Error(DuplicateEndpointAppearance duplicate.Key))
                "Header and order must not alter appearance identity."

            let otherSide = endpoint "layer-one" ProvenanceSide.Output nodeId "output" 3
            let otherLayer = endpoint "layer-two" ProvenanceSide.Input nodeId "other layer" 4

            let finalSession =
                afterFirst
                |> addLayerEndpoint otherSide
                |> expectOk
                |> addLayerEndpoint otherLayer
                |> expectOk

            Expect.equal (nodeAppearances finalSession nodeId |> List.length) 3 "Other sides and layers must succeed."

            let missingLayer = endpoint "missing-layer" ProvenanceSide.Input nodeId "missing" 0

            Expect.equal
                (addLayerEndpoint missingLayer finalSession)
                (Error(LayerNotFound "missing-layer"))
                "Missing layer is typed."

            let missingNode =
                endpoint "layer-one" ProvenanceSide.Input "missing-node" "missing" 0

            Expect.equal
                (addLayerEndpoint missingNode finalSession)
                (Error(NodeNotFound "missing-node"))
                "Missing node is typed."

        testCase "header and layer order position are independent per appearance"
        <| fun _ ->
            let initial =
                empty
                |> withLayers [
                    layer "layer-one" "source-one"
                    layer "layer-two" "source-two"
                ]

            let nodeId, session = ensureNode sampleKind "S1" initial
            let first = endpoint "layer-one" ProvenanceSide.Input nodeId "header one" 1
            let second = endpoint "layer-two" ProvenanceSide.Input nodeId "header two" 8

            let session =
                session
                |> addLayerEndpoint first
                |> expectOk
                |> addLayerEndpoint second
                |> expectOk

            let updatedFirst = {
                first with
                    Header = {
                        first.Header with
                            Text = "updated one"
                    }
                    LayerOrderPosition = 13
            }

            let updatedSession = {
                session with
                    Layers =
                        session.Layers
                        |> Map.change
                            "layer-one"
                            (Option.map (fun item -> {
                                item with
                                    InputEndpoints = item.InputEndpoints |> Map.add nodeId updatedFirst
                            }))
            }

            let appearances = nodeAppearances updatedSession nodeId

            let actualFirst =
                appearances |> List.find (fun item -> item.Key.LayerId = "layer-one")

            let actualSecond =
                appearances |> List.find (fun item -> item.Key.LayerId = "layer-two")

            Expect.equal
                (actualFirst.Header.Text, actualFirst.LayerOrderPosition)
                ("updated one", 13)
                "First appearance must update independently."

            Expect.equal
                (actualSecond.Header.Text, actualSecond.LayerOrderPosition)
                ("header two", 8)
                "Second appearance must remain unchanged."
    ]

let private structuralTests =
    testList "structural helpers" [
        testCase "addProcess rejects a link ID already owned by another process"
        <| fun _ ->
            let initial = empty |> withLayers [ layer "layer-one" "source-one" ]
            let nodeId, initial = ensureNode sampleKind "focus" initial

            let firstProcess = {
                emptyProcess "process-one" "layer-one" with
                    Links =
                        Map.ofList [
                            "shared-link", processLink "shared-link" ProcessLinkShape.Endpointless
                        ]
            }

            let beforeRejectedAdd = addProcess firstProcess initial |> expectOk

            let secondProcess = {
                emptyProcess "process-two" "layer-one" with
                    Links =
                        Map.ofList [
                            "shared-link", processLink "shared-link" (ProcessLinkShape.InputOnly nodeId)
                        ]
            }

            addProcess secondProcess beforeRejectedAdd |> expectInvariantError

            Expect.equal
                beforeRejectedAdd.Processes
                (Map.ofList [ "process-one", firstProcess ])
                "Rejected process must not change the existing process map."

            Expect.equal
                beforeRejectedAdd.Layers["layer-one"].StructuralProcessIds
                (Set.singleton "process-one")
                "Rejected process must not change layer ownership."

        testCase "addLink rejects a link ID already owned by another process"
        <| fun _ ->
            let initial = empty |> withLayers [ layer "layer-one" "source-one" ]
            let nodeId, initial = ensureNode sampleKind "focus" initial

            let initial =
                addProcess (emptyProcess "process-one" "layer-one") initial |> expectOk

            let initial =
                addProcess (emptyProcess "process-two" "layer-one") initial |> expectOk

            let existingLink = processLink "shared-link" ProcessLinkShape.Endpointless
            let beforeRejectedAdd = addLink "process-one" existingLink initial |> expectOk
            let duplicateLink = processLink "shared-link" (ProcessLinkShape.OutputOnly nodeId)
            addLink "process-two" duplicateLink beforeRejectedAdd |> expectInvariantError

            Expect.equal
                beforeRejectedAdd.Processes["process-one"].Links
                (Map.ofList [ "shared-link", existingLink ])
                "Original link owner must remain unchanged."

            Expect.isEmpty
                beforeRejectedAdd.Processes["process-two"].Links
                "Rejected link must not appear on the target process."

        testCase "addProcess rejects an assignment with empty link coverage"
        <| fun _ ->
            let initial = empty |> withLayers [ layer "layer-one" "source-one" ]
            let link = processLink "link-one" ProcessLinkShape.Endpointless

            let invalidProcess = {
                emptyProcess "process-one" "layer-one" with
                    Links = Map.ofList [ link.Id, link ]
                    Assignments =
                        Map.ofList [
                            "assignment-one", processAssignment "assignment-one" Set.empty
                        ]
            }

            addProcess invalidProcess initial |> expectInvariantError
            Expect.isEmpty initial.Processes "Rejected process must not be added."

            Expect.isEmpty
                initial.Layers["layer-one"].StructuralProcessIds
                "Rejected process must not gain layer ownership."

            let foreignCoverageProcess = {
                invalidProcess with
                    Assignments =
                        Map.ofList [
                            "assignment-one", processAssignment "assignment-one" (Set.singleton "foreign-link")
                        ]
            }

            Expect.equal
                (addProcess foreignCoverageProcess initial)
                (Error(LinkNotFound "foreign-link"))
                "Every covered link must belong to the submitted process."

            Expect.isEmpty initial.Processes "Rejected foreign coverage must not add the process."

        testCase "addProcess rejects a link map key that differs from the embedded link ID"
        <| fun _ ->
            let initial = empty |> withLayers [ layer "layer-one" "source-one" ]

            let invalidProcess = {
                emptyProcess "process-one" "layer-one" with
                    Links =
                        Map.ofList [
                            "map-key", processLink "embedded-link-id" ProcessLinkShape.Endpointless
                        ]
            }

            addProcess invalidProcess initial |> expectInvariantError
            Expect.isEmpty initial.Processes "Rejected process must not be added."

            Expect.isEmpty
                initial.Layers["layer-one"].StructuralProcessIds
                "Rejected process must not gain layer ownership."

        testCase "addProcess rejects an assignment map key that differs from the embedded assignment ID"
        <| fun _ ->
            let initial = empty |> withLayers [ layer "layer-one" "source-one" ]
            let link = processLink "link-one" ProcessLinkShape.Endpointless

            let invalidProcess = {
                emptyProcess "process-one" "layer-one" with
                    Links = Map.ofList [ link.Id, link ]
                    Assignments =
                        Map.ofList [
                            "map-key", processAssignment "embedded-assignment-id" (Set.singleton link.Id)
                        ]
            }

            addProcess invalidProcess initial |> expectInvariantError
            Expect.isEmpty initial.Processes "Rejected process must not be added."

            Expect.isEmpty
                initial.Layers["layer-one"].StructuralProcessIds
                "Rejected process must not gain layer ownership."

        testCase "shrinking coverage removes an assignment from the reverse index for the dropped link only"
        <| fun _ ->
            let assignment covered = {
                Id = "assignment-one"
                ValueId = "value-one"
                PropertyKind = AssignmentPropertyKind.Generic
                CoveredLinkIds = covered
                ContainerReferenceValueId = None
                ReferenceSlotId = None
                Lineage = AssignmentLineage.Created
            }

            let structuralProcess covered = {
                emptyProcess "process-one" "layer-one" with
                    Links =
                        [
                            "link-one",
                            {
                                Id = "link-one"
                                Shape = ProcessLinkShape.Endpointless
                            }
                            "link-two",
                            {
                                Id = "link-two"
                                Shape = ProcessLinkShape.Endpointless
                            }
                        ]
                        |> Map.ofList
                    Assignments = Map.ofList [ "assignment-one", assignment covered ]
            }

            let before = {
                empty with
                    Processes =
                        Map.ofList [
                            "process-one", structuralProcess (Set.ofList [ "link-one"; "link-two" ])
                        ]
            }

            let after = {
                before with
                    Processes =
                        Map.ofList [
                            "process-one", structuralProcess (Set.singleton "link-two")
                        ]
            }

            let beforeIndex = linkAssignments before
            let afterIndex = linkAssignments after
            Expect.equal beforeIndex["link-one"] (Set.singleton "assignment-one") "Dropped link starts covered."
            Expect.equal beforeIndex["link-two"] (Set.singleton "assignment-one") "Retained link starts covered."
            Expect.isFalse (afterIndex.ContainsKey "link-one") "Dropped coverage must disappear immediately."
            Expect.equal afterIndex["link-two"] (Set.singleton "assignment-one") "Retained coverage must remain."

        testCase "incident links classify all four link shapes"
        <| fun _ ->
            let initial = empty |> withLayers [ layer "layer-one" "source-one" ]
            let focusId, session = ensureNode sampleKind "focus" initial
            let inputId, session = ensureNode sampleKind "other-input" session
            let outputId, session = ensureNode sampleKind "other-output" session

            let missingLayerProcess = emptyProcess "missing-layer-process" "missing-layer"

            Expect.equal
                (addProcess missingLayerProcess session)
                (Error(LayerNotFound "missing-layer"))
                "A process must reference an existing origin layer."

            Expect.equal
                (addLink
                    "missing-process"
                    {
                        Id = "orphan"
                        Shape = ProcessLinkShape.Endpointless
                    }
                    session)
                (Error(ProcessNotFound "missing-process"))
                "A link owner must exist."

            let structuralProcess = emptyProcess "process-one" "layer-one"
            let session = addProcess structuralProcess session |> expectOk

            Expect.equal
                (addLink
                    "process-one"
                    {
                        Id = "bad-node"
                        Shape = ProcessLinkShape.InputOnly "missing-node"
                    }
                    session)
                (Error(NodeNotFound "missing-node"))
                "Link endpoints must reference existing nodes."

            let links = [
                {
                    Id = "between-outgoing"
                    Shape = ProcessLinkShape.Between(focusId, outputId)
                }
                {
                    Id = "between-incoming"
                    Shape = ProcessLinkShape.Between(inputId, focusId)
                }
                {
                    Id = "input-only"
                    Shape = ProcessLinkShape.InputOnly focusId
                }
                {
                    Id = "output-only"
                    Shape = ProcessLinkShape.OutputOnly focusId
                }
                {
                    Id = "endpointless"
                    Shape = ProcessLinkShape.Endpointless
                }
            ]

            let finalSession =
                links
                |> List.fold (fun state link -> addLink "process-one" link state |> expectOk) session

            let incidence = incidentLinks finalSession focusId
            Expect.equal incidence.OutgoingLinkIds [ "between-outgoing" ] "Between input is outgoing."
            Expect.equal incidence.IncomingLinkIds [ "between-incoming" ] "Between output is incoming."

            Expect.equal
                incidence.OneSidedLinkIds
                [ "input-only"; "output-only" ]
                "Both one-sided shapes are classified separately from direction."

            Expect.isFalse
                (incidence.OutgoingLinkIds
                 @ incidence.IncomingLinkIds
                 @ incidence.OneSidedLinkIds
                 |> List.contains "endpointless")
                "Endpointless links are absent from incidence."

            let storedShapes =
                finalSession.Processes["process-one"].Links
                |> Map.toList
                |> List.map (snd >> _.Shape)
                |> Set.ofList

            Expect.equal
                storedShapes
                (links |> List.map _.Shape |> Set.ofList)
                "All link shapes must be preserved exactly."

            Expect.equal
                finalSession.Layers["layer-one"].StructuralProcessIds
                (Set.singleton "process-one")
                "The origin layer owns the structural process."
    ]

let private valueAndCatalogTests =
    testList "value definitions and reference catalog" [
        testCase "equal header value and unit reuse one value definition"
        <| fun _ ->
            let header = category "Temperature" (Some "UO") (Some "UO:0000005")
            let unit = category "degree Celsius" (Some "UO") (Some "UO:0000027") |> Some
            let first = ensureValueDefinition header (ProvenanceValue.Float 20.0) unit empty
            let session = empty |> installPreparation first
            let nodeId, session = ensureNode sampleKind "S1" session

            let session = {
                session with
                    Nodes =
                        session.Nodes
                        |> Map.change
                            nodeId
                            (Option.map (fun node -> {
                                node with
                                    Assignments =
                                        Map.ofList [
                                            "assignment-one", nodeAssignment "assignment-one" first.ValueDefinition.Id
                                            "assignment-two", nodeAssignment "assignment-two" first.ValueDefinition.Id
                                        ]
                            }))
            }

            let second = ensureValueDefinition header (ProvenanceValue.Float 20.0) unit session
            Expect.equal second.ValueDefinition.Id first.ValueDefinition.Id "Equal content must reuse one value ID."

            Expect.equal
                second.PropertyDefinition.Id
                first.PropertyDefinition.Id
                "Equal headers must reuse one property."

            Expect.equal
                (valueDefinitionReferenceCounts session).[first.ValueDefinition.Id]
                2
                "Both assignments are counted."

        testCase "a differing unit produces a distinct value definition"
        <| fun _ ->
            let header = category "Temperature" (Some "UO") (Some "UO:0000005")
            let celsius = category "degree Celsius" (Some "UO") (Some "UO:0000027") |> Some
            let kelvin = category "kelvin" (Some "UO") (Some "UO:0000012") |> Some
            let first = ensureValueDefinition header (ProvenanceValue.Float 20.0) celsius empty
            let session = empty |> installPreparation first

            let second =
                ensureValueDefinition header (ProvenanceValue.Float 20.0) kelvin session

            Expect.notEqual second.ValueDefinition.Id first.ValueDefinition.Id "Unit participates in value identity."

            Expect.equal
                second.PropertyDefinition.Id
                first.PropertyDefinition.Id
                "The category still reuses its property."

            let positiveZero =
                ensureValueDefinition header (ProvenanceValue.Float 0.0) None empty

            let negativeZero =
                ensureValueDefinition header (ProvenanceValue.Float -0.0) None empty

            Expect.equal
                negativeZero.ValueDefinition.Id
                positiveZero.ValueDefinition.Id
                "Structurally equal signed zero values have one deterministic candidate ID."

        testCase "NaN normalization reuses an installed value definition"
        <| fun _ ->
            let header = category "Numeric quality" (Some "TEST") (Some "TEST:float")

            let first =
                ensureValueDefinition header (ProvenanceValue.Float System.Double.NaN) None empty

            let session = empty |> installPreparation first
            let nodeId, session = ensureNode sampleKind "S1" session
            let assignment = nodeAssignment "nan-assignment" first.ValueDefinition.Id

            let session = {
                session with
                    Nodes =
                        session.Nodes
                        |> Map.change
                            nodeId
                            (Option.map (fun node -> {
                                node with
                                    Assignments = Map.ofList [ assignment.Id, assignment ]
                            }))
            }

            let alternateNaN = System.BitConverter.Int64BitsToDouble(0x7ff8000000000001L)

            let second =
                ensureValueDefinition header (ProvenanceValue.Float alternateNaN) None session

            Expect.equal
                second.ValueDefinition.Id
                first.ValueDefinition.Id
                "All NaN payloads share one semantic identity."

            Expect.isEmpty (orphanValueDefinitionIds session) "The installed NaN definition remains assigned."

            let finiteOne = ensureValueDefinition header (ProvenanceValue.Float 1.0) None empty
            let finiteTwo = ensureValueDefinition header (ProvenanceValue.Float 2.0) None empty

            Expect.notEqual
                finiteOne.ValueDefinition.Id
                finiteTwo.ValueDefinition.Id
                "Distinct finite floats remain distinct."

        testCase "a value definition with no assignment is reported as an orphan"
        <| fun _ ->
            let prepared =
                ensureValueDefinition (category "Comment" None None) (ProvenanceValue.Text "kept") None empty

            let session = empty |> installPreparation prepared
            let nodeId, session = ensureNode sampleKind "S1" session
            let assignment = nodeAssignment "assignment-one" prepared.ValueDefinition.Id

            let assigned = {
                session with
                    Nodes =
                        session.Nodes
                        |> Map.change
                            nodeId
                            (Option.map (fun node -> {
                                node with
                                    Assignments = Map.ofList [ assignment.Id, assignment ]
                            }))
            }

            Expect.isEmpty (orphanValueDefinitionIds assigned) "A referenced definition is not orphaned."

            let afterRemoval = {
                assigned with
                    Nodes =
                        assigned.Nodes
                        |> Map.change nodeId (Option.map (fun node -> { node with Assignments = Map.empty }))
            }

            Expect.equal
                (orphanValueDefinitionIds afterRemoval)
                (Set.singleton prepared.ValueDefinition.Id)
                "Removing the last assignment exposes the orphan."

        testCase "catalog entries exist without any assignment"
        <| fun _ ->
            let catalog =
                normalizeCatalog [
                    catalogEntry "doi" "10.1000/alpha" "Extraction"
                    catalogEntry "doi" "10.1000/beta" "Extraction"
                ]

            Expect.equal (catalogEntries catalog |> List.length) 2 "Both catalog choices remain selectable."
            Expect.isEmpty empty.Values "Catalog choices do not create session value definitions."
            Expect.isEmpty (orphanValueDefinitionIds empty) "An empty session has no definition orphans."

        testCase "catalog identity ignores the display label"
        <| fun _ ->
            let first = catalogEntry "doi" "10.1000/shared" "First label"
            let relabeled = catalogEntry "doi" "10.1000/shared" "Second label"
            let sameLabelOtherId = catalogEntry "doi" "10.1000/other" "First label"
            let differentlyCasedScheme = catalogEntry "DOI" "10.1000/shared" "First label"

            let catalog =
                normalizeCatalog [
                    first
                    relabeled
                    sameLabelOtherId
                    differentlyCasedScheme
                ]

            Expect.equal catalog.Count 3 "Identity is exact and case-sensitive; labels do not participate."
            Expect.isSome (tryFindCatalogEntry "doi" "10.1000/shared" catalog) "The exact identity is selectable."
            Expect.isSome (tryFindCatalogEntry "doi" "10.1000/other" catalog) "Equal labels do not merge IDs."
            Expect.isSome (tryFindCatalogEntry "DOI" "10.1000/shared" catalog) "Scheme case remains significant."

        testCase "catalog normalization is stable for duplicate exact identities regardless of input order"
        <| fun _ ->
            let first = catalogEntry "doi" "10.1000/shared" "Zulu label"

            let conflicting = {
                catalogEntry "doi" "10.1000/shared" "Alpha label" with
                    Category = category "Conflicting category" (Some "TEST") (Some "TEST:1")
                    AssignmentKind = AnnotationOwnerKind.Node
                    PropertyKind = AssignmentPropertyKind.AdapterSpecific sampleKind
            }

            let forward = normalizeCatalog [ first; conflicting ]
            let reverse = normalizeCatalog [ conflicting; first ]

            Expect.equal forward reverse "A total-order winner makes duplicate normalization input-order independent."
            Expect.equal forward.Count 1 "An exact scheme/ID identity still produces one catalog entry."

            let promotedForward =
                forward
                |> catalogEntries
                |> List.exactlyOne
                |> fun entry -> promoteCatalogEntry entry empty

            let promotedReverse =
                reverse
                |> catalogEntries
                |> List.exactlyOne
                |> fun entry -> promoteCatalogEntry entry empty

            Expect.equal
                promotedForward
                promotedReverse
                "Promotion observes the same deterministic complete-entry winner."

        testCase "colliding labels disambiguate by shortest unique durable id suffix"
        <| fun _ ->
            let rendered =
                disambiguate [
                    {
                        DisplayLabel = "Extraction"
                        Scheme = "arc"
                        DurableId = "https://example.org/protocols/a1"
                    }
                    {
                        DisplayLabel = "Extraction"
                        Scheme = "arc"
                        DurableId = "https://example.org/protocols/b7"
                    }
                ]

            Expect.equal
                rendered[("arc", "https://example.org/protocols/a1")]
                "Extraction (a1)"
                "The shortest unique trailing segment is used."

            Expect.equal
                rendered[("arc", "https://example.org/protocols/b7")]
                "Extraction (b7)"
                "Distinct identities are never merged."

            let reversed =
                disambiguate [
                    {
                        DisplayLabel = "Extraction"
                        Scheme = "arc"
                        DurableId = "https://example.org/protocols/b7"
                    }
                    {
                        DisplayLabel = "Extraction"
                        Scheme = "arc"
                        DurableId = "https://example.org/protocols/a1"
                    }
                ]

            Expect.equal reversed rendered "Disambiguation is stable under input reordering."

        testCase "identical suffixes fall back to the full durable id"
        <| fun _ ->
            let rendered =
                disambiguate [
                    {
                        DisplayLabel = "Extraction"
                        Scheme = "arc"
                        DurableId = "protocols/a1"
                    }
                    {
                        DisplayLabel = "Extraction"
                        Scheme = "arc"
                        DurableId = "a1"
                    }
                ]

            Expect.equal rendered[("arc", "protocols/a1")] "Extraction (protocols/a1)" "Full first ID is shown."
            Expect.equal rendered[("arc", "a1")] "Extraction (a1)" "Full second ID is shown."

        testCase "equal durable ids fall back to the full scheme and id identity"
        <| fun _ ->
            let rendered =
                disambiguate [
                    {
                        DisplayLabel = "Extraction"
                        Scheme = "arc"
                        DurableId = "protocol/a1"
                    }
                    {
                        DisplayLabel = "Extraction"
                        Scheme = "doi"
                        DurableId = "protocol/a1"
                    }
                ]

            Expect.equal
                rendered[("arc", "protocol/a1")]
                "Extraction (arc, protocol/a1)"
                "The scheme distinguishes equal durable IDs."

            Expect.equal
                rendered[("doi", "protocol/a1")]
                "Extraction (doi, protocol/a1)"
                "Each exact identity remains present."

        testCase "reference counts and orphan assignments include node and process containers"
        <| fun _ ->
            let prepared =
                ensureValueDefinition (category "Reference" None None) (ProvenanceValue.Text "value") None empty

            let container =
                ensureValueDefinition
                    (category "Container" None None)
                    (ProvenanceValue.Reference {
                        Scheme = "arc"
                        Id = "protocol/parent"
                        Label = "Parent"
                    })
                    None
                    empty

            let session =
                empty
                |> installPreparation prepared
                |> installPreparation container
                |> withLayers [ layer "layer-one" "source-one" ]

            let nodeId, session = ensureNode sampleKind "S1" session
            let nodeValue = nodeAssignment "node-assignment" prepared.ValueDefinition.Id
            let link = processLink "link-one" ProcessLinkShape.Endpointless

            let processValue = {
                processAssignment "process-assignment" (Set.singleton link.Id) with
                    ValueId = prepared.ValueDefinition.Id
                    ContainerReferenceValueId = Some container.ValueDefinition.Id
                    ReferenceSlotId = None
            }

            let containerValue = {
                processAssignment "container-assignment" (Set.singleton link.Id) with
                    ValueId = container.ValueDefinition.Id
                    ReferenceSlotId = None
            }

            let invalidProcessValue = {
                processAssignment "invalid-assignment" (Set.singleton link.Id) with
                    ValueId = "missing-value"
            }

            let session = {
                session with
                    Nodes =
                        session.Nodes
                        |> Map.change
                            nodeId
                            (Option.map (fun node -> {
                                node with
                                    Assignments = Map.ofList [ nodeValue.Id, nodeValue ]
                            }))
                    Processes =
                        Map.ofList [
                            "process-one",
                            {
                                emptyProcess "process-one" "layer-one" with
                                    Links = Map.ofList [ link.Id, link ]
                                    Assignments =
                                        Map.ofList [
                                            containerValue.Id, containerValue
                                            processValue.Id, processValue
                                            invalidProcessValue.Id, invalidProcessValue
                                        ]
                            }
                        ]
            }

            let counts = valueDefinitionReferenceCounts session
            Expect.equal counts[prepared.ValueDefinition.Id] 2 "Node and process value references are both counted."
            Expect.equal counts[container.ValueDefinition.Id] 2 "Container and assignment references both count."

            Expect.isFalse
                (orphanAssignmentIds session |> Set.contains nodeValue.Id)
                "Valid node assignment is retained."

            Expect.isFalse
                (orphanAssignmentIds session |> Set.contains containerValue.Id)
                "Valid container is retained."

            Expect.isFalse
                (orphanAssignmentIds session |> Set.contains processValue.Id)
                "Valid process assignment is retained."

            Expect.equal
                (orphanAssignmentIds session)
                (Set.singleton invalidProcessValue.Id)
                "A missing value makes the otherwise valid process assignment orphaned."

            let withoutContainerAssignment = {
                session with
                    Processes =
                        session.Processes
                        |> Map.change
                            "process-one"
                            (Option.map (fun structuralProcess -> {
                                structuralProcess with
                                    Assignments = structuralProcess.Assignments |> Map.remove containerValue.Id
                            }))
            }

            Expect.isTrue
                (orphanAssignmentIds withoutContainerAssignment |> Set.contains processValue.Id)
                "A dependent assignment without per-link container coverage is orphaned."

        testCase "a dependent is orphaned when its sole container assignment is independently invalid"
        <| fun _ ->
            let dependent =
                ensureValueDefinition (category "Dependent" None None) (ProvenanceValue.Text "child") None empty

            let container =
                ensureValueDefinition
                    (category "Container" None None)
                    (ProvenanceValue.Reference {
                        Scheme = "arc"
                        Id = "protocol/parent"
                        Label = "Parent"
                    })
                    None
                    empty

            let session =
                empty
                |> installPreparation dependent
                |> installPreparation container
                |> withLayers [ layer "layer-one" "source-one" ]

            let validLink = processLink "link-one" ProcessLinkShape.Endpointless

            let containerAssignment = {
                processAssignment "container-assignment" (Set.ofList [ validLink.Id; "foreign-link" ]) with
                    ValueId = container.ValueDefinition.Id
            }

            let dependentAssignment = {
                processAssignment "dependent-assignment" (Set.singleton validLink.Id) with
                    ValueId = dependent.ValueDefinition.Id
                    ContainerReferenceValueId = Some container.ValueDefinition.Id
            }

            let session = {
                session with
                    Processes =
                        Map.ofList [
                            "process-one",
                            {
                                emptyProcess "process-one" "layer-one" with
                                    Links = Map.ofList [ validLink.Id, validLink ]
                                    Assignments =
                                        Map.ofList [
                                            containerAssignment.Id, containerAssignment
                                            dependentAssignment.Id, dependentAssignment
                                        ]
                            }
                        ]
            }

            Expect.equal
                (orphanAssignmentIds session)
                (Set.ofList [ containerAssignment.Id; dependentAssignment.Id ])
                "An intrinsically invalid container cannot validate its dependent."

        testCase "a dependent is orphaned when a second container becomes valid later"
        <| fun _ ->
            let dependent =
                ensureValueDefinition (category "Dependent" None None) (ProvenanceValue.Text "child") None empty

            let sharedContainer =
                ensureValueDefinition
                    (category "Shared container" None None)
                    (ProvenanceValue.Reference {
                        Scheme = "arc"
                        Id = "protocol/shared"
                        Label = "Shared"
                    })
                    None
                    empty

            let upstreamContainer =
                ensureValueDefinition
                    (category "Upstream container" None None)
                    (ProvenanceValue.Reference {
                        Scheme = "arc"
                        Id = "protocol/upstream"
                        Label = "Upstream"
                    })
                    None
                    empty

            let session =
                empty
                |> installPreparation dependent
                |> installPreparation sharedContainer
                |> installPreparation upstreamContainer
                |> withLayers [ layer "layer-one" "source-one" ]

            let link = processLink "link-one" ProcessLinkShape.Endpointless

            let upstream = {
                processAssignment "upstream" (Set.singleton link.Id) with
                    ValueId = upstreamContainer.ValueDefinition.Id
            }

            let firstShared = {
                processAssignment "shared-one" (Set.singleton link.Id) with
                    ValueId = sharedContainer.ValueDefinition.Id
            }

            let laterShared = {
                processAssignment "shared-two" (Set.singleton link.Id) with
                    ValueId = sharedContainer.ValueDefinition.Id
                    ContainerReferenceValueId = Some upstreamContainer.ValueDefinition.Id
            }

            let dependentAssignment = {
                processAssignment "dependent-assignment" (Set.singleton link.Id) with
                    ValueId = dependent.ValueDefinition.Id
                    ContainerReferenceValueId = Some sharedContainer.ValueDefinition.Id
            }

            let session = {
                session with
                    Processes =
                        Map.ofList [
                            "process-one",
                            {
                                emptyProcess "process-one" "layer-one" with
                                    Links = Map.ofList [ link.Id, link ]
                                    Assignments =
                                        [ upstream; firstShared; laterShared; dependentAssignment ]
                                        |> List.map (fun assignment -> assignment.Id, assignment)
                                        |> Map.ofList
                            }
                        ]
            }

            Expect.equal
                (orphanAssignmentIds session)
                (Set.singleton dependentAssignment.Id)
                "Final container ambiguity invalidates an earlier admitted dependent."

        testCase "orphan detection reports stored keys and globally duplicate assignment ids"
        <| fun _ ->
            let prepared =
                ensureValueDefinition (category "Comment" None None) (ProvenanceValue.Text "value") None empty

            let session =
                empty
                |> installPreparation prepared
                |> withLayers [ layer "layer-one" "source-one" ]

            let firstNodeId, session = ensureNode sampleKind "S1" session
            let secondNodeId, session = ensureNode sampleKind "S2" session
            let duplicate = nodeAssignment "global-duplicate" prepared.ValueDefinition.Id
            let mismatchedNode = nodeAssignment "embedded-duplicate" prepared.ValueDefinition.Id
            let link = processLink "link-one" ProcessLinkShape.Endpointless

            let mismatchedProcess = {
                processAssignment "embedded-duplicate" (Set.singleton link.Id) with
                    ValueId = prepared.ValueDefinition.Id
            }

            let session = {
                session with
                    Nodes =
                        session.Nodes
                        |> Map.change
                            firstNodeId
                            (Option.map (fun node -> {
                                node with
                                    Assignments = Map.ofList [ duplicate.Id, duplicate; "node-stored", mismatchedNode ]
                            }))
                        |> Map.change
                            secondNodeId
                            (Option.map (fun node -> {
                                node with
                                    Assignments = Map.ofList [ duplicate.Id, duplicate ]
                            }))
                    Processes =
                        Map.ofList [
                            "process-one",
                            {
                                emptyProcess "process-one" "layer-one" with
                                    Links = Map.ofList [ link.Id, link ]
                                    Assignments = Map.ofList [ "process-stored", mismatchedProcess ]
                            }
                        ]
            }

            Expect.equal
                (orphanAssignmentIds session)
                (Set.ofList [
                    "embedded-duplicate"
                    "global-duplicate"
                    "node-stored"
                    "process-stored"
                ])
                "Stored keys target malformed entries; a duplicate embedded ID targets every occurrence."

        testCase "a category with one concrete kind establishes that kind for its owner kind only"
        <| fun _ ->
            let header = category "Species" (Some "TEST") (Some "TEST:species")

            let concreteKind =
                AssignmentPropertyKind.AdapterSpecific {
                    Id = "adapter:characteristic"
                    Label = "Characteristic"
                }

            let prepared =
                ensureValueDefinition header (ProvenanceValue.Text "Arabidopsis") None empty

            let session = empty |> installPreparation prepared
            let nodeId, session = ensureNode sampleKind "S1" session

            let loaded = {
                nodeAssignment "loaded" prepared.ValueDefinition.Id with
                    PropertyKind = concreteKind
                    Lineage = AssignmentLineage.Loaded
            }

            let session = {
                session with
                    Nodes =
                        session.Nodes
                        |> Map.change
                            nodeId
                            (Option.map (fun node -> {
                                node with
                                    Assignments = Map.ofList [ loaded.Id, loaded ]
                            }))
            }

            Expect.equal
                (establishedPropertyKind AnnotationOwnerKind.Node header session)
                concreteKind
                "The loaded assignment's concrete kind is established for node reuse."

            Expect.equal
                (establishedPropertyKind AnnotationOwnerKind.Process header session)
                AssignmentPropertyKind.Generic
                "A node entry's kind does not leak into the process owner kind."

        testCase "a new category has no established kind"
        <| fun _ ->
            Expect.equal
                (establishedPropertyKind AnnotationOwnerKind.Node (category "Unknown" None None) empty)
                AssignmentPropertyKind.Generic
                "A category without assignments stays generic."

        testCase "disagreeing concrete kinds fall back to generic"
        <| fun _ ->
            let header = category "Species" (Some "TEST") (Some "TEST:species")

            let characteristic =
                AssignmentPropertyKind.AdapterSpecific {
                    Id = "adapter:characteristic"
                    Label = "Characteristic"
                }

            let factor =
                AssignmentPropertyKind.AdapterSpecific {
                    Id = "adapter:factor"
                    Label = "Factor"
                }

            let prepared =
                ensureValueDefinition header (ProvenanceValue.Text "Arabidopsis") None empty

            let session = empty |> installPreparation prepared
            let firstNodeId, session = ensureNode sampleKind "S1" session
            let secondNodeId, session = ensureNode sampleKind "S2" session

            let assignmentWith id kind = {
                nodeAssignment id prepared.ValueDefinition.Id with
                    PropertyKind = kind
                    Lineage = AssignmentLineage.Loaded
            }

            let install nodeId (assignment: NodeAssignment) (current: ProvenanceSession) = {
                current with
                    Nodes =
                        current.Nodes
                        |> Map.change
                            nodeId
                            (Option.map (fun node -> {
                                node with
                                    Assignments = Map.ofList [ assignment.Id, assignment ]
                            }))
            }

            let session =
                session
                |> install firstNodeId (assignmentWith "as-characteristic" characteristic)
                |> install secondNodeId (assignmentWith "as-factor" factor)

            Expect.equal
                (establishedPropertyKind AnnotationOwnerKind.Node header session)
                AssignmentPropertyKind.Generic
                "An entry whose assignments disagree on a concrete kind establishes none."

        testCase "a process category establishes its kind from process assignments"
        <| fun _ ->
            let header = category "Instrument" (Some "TEST") (Some "TEST:instrument")

            let parameterKind =
                AssignmentPropertyKind.AdapterSpecific {
                    Id = "adapter:parameter"
                    Label = "Parameter"
                }

            let prepared =
                ensureValueDefinition header (ProvenanceValue.Text "Old device") None empty

            let link = processLink "link-one" ProcessLinkShape.Endpointless

            let loaded = {
                processAssignment "loaded-instrument" (Set.singleton link.Id) with
                    ValueId = prepared.ValueDefinition.Id
                    PropertyKind = parameterKind
                    Lineage = AssignmentLineage.Loaded
            }

            let session = empty |> installPreparation prepared

            let session = {
                session with
                    Processes =
                        Map.ofList [
                            "process-one",
                            {
                                emptyProcess "process-one" "layer-one" with
                                    Links = Map.ofList [ link.Id, link ]
                                    Assignments = Map.ofList [ loaded.Id, loaded ]
                            }
                        ]
            }

            Expect.equal
                (establishedPropertyKind AnnotationOwnerKind.Process header session)
                parameterKind
                "The loaded process assignment's concrete kind is established for process reuse."

            Expect.equal
                (establishedPropertyKind AnnotationOwnerKind.Node header session)
                AssignmentPropertyKind.Generic
                "A process entry's kind does not leak into the node owner kind."
    ]

let tests =
    testList "CanonicalModel" [
        identityTests
        endpointTests
        structuralTests
        valueAndCatalogTests
        testList "canonical fixtures" [
            testCase "every fixture session satisfies the non-orphan value invariant"
            <| fun _ ->
                for session in allSessions () do
                    Expect.isEmpty
                        (orphanValueDefinitionIds session)
                        "Every fixture value is referenced by an assignment."

            testCase "every fixture session passes preparation"
            <| fun _ ->
                for session in allSessions () do
                    match prepareForWriteback session with
                    | Ok _ -> ()
                    | Error error -> failtestf "Fixture '%s' failed preparation: %A" session.ActiveLayerId error

            testCase "the branch fixture reproduces the sibling-leak graph"
            <| fun _ ->
                let session = createSiblingLeakSession ()

                let shapes =
                    session.Processes
                    |> Map.toList
                    |> List.collect (fun (_, structuralProcess) ->
                        structuralProcess.Links |> Map.toList |> List.map (snd >> _.Shape)
                    )
                    |> Set.ofList

                Expect.equal
                    shapes
                    (Set.ofList [
                        ProcessLinkShape.Between("node-a", "node-b")
                        ProcessLinkShape.Between("node-a", "node-c")
                        ProcessLinkShape.Between("node-b", "node-d")
                    ])
                    "The mandatory three-edge branch is exact."

            testCase "the shared-node fixture resolves equal endpoints to one canonical node"
            <| fun _ ->
                let session = createSharedNodeSession ()

                let sharedAppearances = nodeAppearances session "node-shared"

                Expect.equal sharedAppearances.Length 2 "The shared node appears twice."

                Expect.equal
                    (sharedAppearances
                     |> List.map (fun endpoint -> endpoint.Key.NodeId)
                     |> Set.ofList)
                    (Set.singleton "node-shared")
                    "Both source/layer appearances use one canonical identity."

                Expect.equal session.Nodes["node-shared"].Assignments.Count 1 "The owner stores its assignment once."
        ]
    ]

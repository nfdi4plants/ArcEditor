module CanonicalProjectionTests

open Expecto
open Swate.Components.Page.ProvenanceGrouping.Identifiers
open Swate.Components.Page.ProvenanceGrouping.Values
open Swate.Components.Page.ProvenanceGrouping.Domain
open Swate.Components.Page.ProvenanceGrouping.AvailabilityTypes
open Swate.Components.Page.ProvenanceGrouping.ProjectionTypes
open Swate.Components.Page.ProvenanceGrouping.MutationTypes
open Swate.Components.Page.ProvenanceGrouping.Model
open Swate.Components.Page.ProvenanceGrouping.Projection
open Swate.Components.Page.ProvenanceGrouping.ColorResolution
open Swate.Components.Page.ProvenanceGrouping.Commands

let private endpointKind = {
    Id = "canonical:endpoint:sample"
    Label = "Sample"
}

let private term name accession = {
    Name = name
    TermSource = Some "TEST"
    TermAccession = accession
}

let private source id name = { Id = id; Name = name }

let private property id header : PropertyDefinition = { Id = id; Category = header }

let private value id propertyId content : PropertyValueDefinition = {
    Id = id
    PropertyId = propertyId
    Value = content
    Unit = None
}

let private nodeAssignment id valueId propertyKind targetSource : NodeAssignment = {
    Id = id
    ValueId = valueId
    PropertyKind = propertyKind
    TargetSource = targetSource
    Lineage = AssignmentLineage.Loaded
}

let private processAssignment id valueId linkIds propertyKind : ProcessAssignment = {
    Id = id
    ValueId = valueId
    PropertyKind = propertyKind
    CoveredLinkIds = Set.ofList linkIds
    ContainerReferenceValueId = None
    ReferenceSlotId = None
    Lineage = AssignmentLineage.Loaded
}

let private node id (assignments: NodeAssignment list) : CanonicalNode = {
    Id = id
    Key = { KindId = endpointKind.Id; Name = id }
    Kind = endpointKind
    Name = id
    Assignments =
        assignments
        |> List.map (fun assignment -> assignment.Id, assignment)
        |> Map.ofList
}

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

let private link id shape : ProcessLink = { Id = id; Shape = shape }

let private layer id source : ProvenanceLayer = {
    Id = id
    Label = id
    Source = source
    InputEndpoints = Map.empty
    OutputEndpoints = Map.empty
    StructuralProcessIds = Set.empty
}

let private nodeReference (assignment: NodeAssignment) owner relation : AvailableAnnotationRef = {
    AssignmentId = assignment.Id
    ValueId = assignment.ValueId
    Owner = NodeOwner owner
    Relation = relation
    OriginatingLinkIds = Set.empty
    VisibleThroughLinkIds =
        match relation with
        | ForwardPropagated route -> Set.ofList route
        | ReverseConnectionLocal linkId -> Set.singleton linkId
        | OwnedNode
        | IncidentProcess _ -> Set.empty
}

let private processReference (assignment: ProcessAssignment) owner relation originLinks : AvailableAnnotationRef = {
    AssignmentId = assignment.Id
    ValueId = assignment.ValueId
    Owner = ProcessOwner owner
    Relation = relation
    OriginatingLinkIds = Set.ofList originLinks
    VisibleThroughLinkIds = Set.ofList originLinks
}

let private expectOk =
    function
    | Ok value -> value
    | Error error -> failtestf "Expected Ok but got %A" error

let private project reference session =
    projectAnnotation reference session |> expectOk

let private basicSession () =
    let header = term "Temperature" (Some "TEST:temperature")
    let definition = property "property-temperature" header

    let valueDefinition =
        value "value-temperature" definition.Id (ProvenanceValue.Text "20")

    {
        empty with
            Properties = Map.ofList [ definition.Id, definition ]
            Values = Map.ofList [ valueDefinition.Id, valueDefinition ]
            Layers =
                Map.ofList [
                    "layer-one", layer "layer-one" (source "source-one" "One")
                    "layer-two", layer "layer-two" (source "source-two" "Two")
                ]
    }

let private appearance layerId side nodeId position : LayerEndpoint = {
    Key = {
        LayerId = layerId
        Side = side
        NodeId = nodeId
    }
    Header = { Kind = endpointKind; Text = nodeId }
    LayerOrderPosition = position
}

let private surfaceFixture () =
    let nodeProperty = property "property-node" (term "Node value" (Some "TEST:node"))

    let processProperty =
        property "property-process" (term "Process value" (Some "TEST:process"))

    let endpointlessProperty =
        property "property-endpointless" (term "Endpointless" None)

    let nodeValue = value "value-node" nodeProperty.Id (ProvenanceValue.Text "node")

    let processValue =
        value "value-process" processProperty.Id (ProvenanceValue.Text "process")

    let endpointlessValue =
        value "value-endpointless" endpointlessProperty.Id (ProvenanceValue.Text "endpointless")

    let ownedNode =
        nodeAssignment
            "assignment-node"
            nodeValue.Id
            (AdapterSpecific {
                Id = "processcore:characteristic"
                Label = "Characteristic"
            })
            None

    let pooledProcess =
        processAssignment "assignment-process" processValue.Id [ "link-ab"; "link-ac" ] Generic

    let endpointless =
        processAssignment "assignment-endpointless" endpointlessValue.Id [ "link-endpointless" ] Generic

    let layerOne = {
        layer "layer-one" (source "source-one" "One") with
            InputEndpoints =
                Map.ofList [
                    "node-a", appearance "layer-one" ProvenanceSide.Input "node-a" 0
                ]
            OutputEndpoints =
                Map.ofList [
                    "node-b", appearance "layer-one" ProvenanceSide.Output "node-b" 0
                    "node-c", appearance "layer-one" ProvenanceSide.Output "node-c" 1
                ]
            StructuralProcessIds = Set.ofList [ "process-pooled"; "process-endpointless" ]
    }

    let layerTwo = {
        layer "layer-two" (source "source-two" "Two") with
            InputEndpoints =
                Map.ofList [
                    "node-a", appearance "layer-two" ProvenanceSide.Input "node-a" 0
                    "node-b", appearance "layer-two" ProvenanceSide.Input "node-b" 1
                ]
    }

    let session = {
        empty with
            Nodes =
                Map.ofList [
                    "node-a", node "node-a" [ ownedNode ]
                    "node-b", node "node-b" []
                    "node-c", node "node-c" []
                ]
            Processes =
                Map.ofList [
                    "process-pooled",
                    structuralProcess "process-pooled" "layer-one" [
                        link "link-ab" (ProcessLinkShape.Between("node-a", "node-b"))
                        link "link-ac" (ProcessLinkShape.Between("node-a", "node-c"))
                    ] [ pooledProcess ]
                    "process-endpointless",
                    structuralProcess "process-endpointless" "layer-one" [
                        link "link-endpointless" ProcessLinkShape.Endpointless
                    ] [ endpointless ]
                ]
            Properties =
                Map.ofList [
                    nodeProperty.Id, nodeProperty
                    processProperty.Id, processProperty
                    endpointlessProperty.Id, endpointlessProperty
                ]
            Values =
                Map.ofList [
                    nodeValue.Id, nodeValue
                    processValue.Id, processValue
                    endpointlessValue.Id, endpointlessValue
                ]
            Layers = Map.ofList [ layerOne.Id, layerOne; layerTwo.Id, layerTwo ]
            LayerOrder = [ layerOne.Id; layerTwo.Id ]
            ActiveLayerId = layerOne.Id
    }

    let catalogEntry = {
        Category = term "Recipe" (Some "TEST:recipe")
        Reference = {
            Scheme = "processcore:recipe"
            Id = "recipe-id"
            Label = "Stored recipe"
        }
        Unit = None
        AssignmentKind = AnnotationOwnerKind.Process
        PropertyKind =
            AdapterSpecific {
                Id = "processcore:recipe"
                Label = "Recipe"
            }
        Cardinality = AtMostOnePerLink "processcore:executes-recipe"
        DependentProcessValues = []
    }

    session, Map.ofList [ ("processcore:recipe", "recipe-id"), catalogEntry ]

let private commitEffect session effect =
    Swate.Components.Page.ProvenanceGrouping.CanonicalSession.commit effect session

let private shelfBacking =
    function
    | { Payload = AssignmentBacked payload } -> Some payload
    | _ -> None

let private colorSettings sourceColors setOrder manualColors : ColorSettings = {
    Palette = [| "#2563eb"; "#16a34a" |]
    SourceColors = Map.ofList sourceColors
    SourceColorSetOrder = Map.ofList setOrder
    ManualPropertyColors = Map.ofList manualColors
}

let private propertyColorKey kind header : PropertyColorKey = { Kind = kind; Header = header }

let private withOutputNodeD assignment (session: ProvenanceSession) = {
    session with
        Nodes = session.Nodes |> Map.add "node-d" (node "node-d" assignment)
        Layers =
            session.Layers
            |> Map.change
                "layer-one"
                (Option.map (fun layer -> {
                    layer with
                        OutputEndpoints =
                            layer.OutputEndpoints
                            |> Map.add "node-d" (appearance "layer-one" ProvenanceSide.Output "node-d" 2)
                }))
}

let tests =
    testList "CanonicalProjection" [
        testCase "characteristic and factor with equal header value and unit group together"
        <| fun _ ->
            let characteristic =
                nodeAssignment
                    "assignment-characteristic"
                    "value-temperature"
                    (AdapterSpecific {
                        Id = "processcore:characteristic"
                        Label = "Characteristic"
                    })
                    None

            let factor =
                nodeAssignment
                    "assignment-factor"
                    "value-temperature"
                    (AdapterSpecific {
                        Id = "processcore:factor"
                        Label = "Factor"
                    })
                    None

            let session = {
                basicSession () with
                    Nodes =
                        Map.ofList [
                            "node-one", node "node-one" [ characteristic ]
                            "node-two", node "node-two" [ factor ]
                        ]
            }

            let projected = [
                project (nodeReference characteristic "node-one" OwnedNode) session
                project (nodeReference factor "node-two" OwnedNode) session
            ]

            Expect.equal
                (projected |> List.map _.Key |> List.distinct |> List.length)
                1
                "Concrete adapter kind is backing metadata, not grouping identity."

        testCase "node grouping ignores origin source and origin layer"
        <| fun _ ->
            let first =
                nodeAssignment "assignment-one" "value-temperature" Generic (Some(source "source-one" "One"))

            let second =
                nodeAssignment "assignment-two" "value-temperature" Generic (Some(source "source-two" "Two"))

            let session = {
                basicSession () with
                    Nodes =
                        Map.ofList [
                            "node-one", node "node-one" [ first ]
                            "node-two", node "node-two" [ second ]
                        ]
            }

            let firstKey = project (nodeReference first "node-one" OwnedNode) session |> _.Key
            let secondKey = project (nodeReference second "node-two" OwnedNode) session |> _.Key
            Expect.equal firstKey secondKey "Node grouping is source-agnostic."

        testCase "process grouping separates by origin source"
        <| fun _ ->
            let first =
                processAssignment "assignment-one" "value-temperature" [ "link-one" ] Generic

            let second =
                processAssignment "assignment-two" "value-temperature" [ "link-two" ] Generic

            let session = {
                basicSession () with
                    Processes =
                        Map.ofList [
                            "process-one",
                            structuralProcess "process-one" "layer-one" [
                                link "link-one" ProcessLinkShape.Endpointless
                            ] [ first ]
                            "process-two",
                            structuralProcess "process-two" "layer-two" [
                                link "link-two" ProcessLinkShape.Endpointless
                            ] [ second ]
                        ]
            }

            let keys = [
                project (processReference first "process-one" (IncidentProcess "link-one") [ "link-one" ]) session
                |> _.Key
                project (processReference second "process-two" (IncidentProcess "link-two") [ "link-two" ]) session
                |> _.Key
            ]

            Expect.equal (keys |> List.distinct |> List.length) 2 "Process source identity separates keys."

        testCase "renaming a source does not split or merge process groups"
        <| fun _ ->
            let assignment =
                processAssignment "assignment" "value-temperature" [ "link" ] Generic

            let makeSession sourceName = {
                basicSession () with
                    Layers =
                        Map.ofList [
                            "layer-one", layer "layer-one" (source "source-one" sourceName)
                        ]
                    Processes =
                        Map.ofList [
                            "process",
                            structuralProcess "process" "layer-one" [ link "link" ProcessLinkShape.Endpointless ] [
                                assignment
                            ]
                        ]
            }

            let reference =
                processReference assignment "process" (IncidentProcess "link") [ "link" ]

            let beforeKey = project reference (makeSession "Before") |> _.Key
            let afterKey = project reference (makeSession "After") |> _.Key
            Expect.equal beforeKey afterKey "Only the source ID participates."

        testCase "a process assignment stores no origin, so its grouping key follows its owning process's layer source"
        <| fun _ ->
            let first =
                processAssignment "assignment-one" "value-temperature" [ "link-one" ] Generic

            let second =
                processAssignment "assignment-two" "value-temperature" [ "link-two" ] Generic

            let makeSession secondSourceId = {
                basicSession () with
                    Layers =
                        Map.ofList [
                            "layer-one", layer "layer-one" (source "source-one" "One")
                            "layer-two", layer "layer-two" (source secondSourceId "Two")
                        ]
                    Processes =
                        Map.ofList [
                            "process-one",
                            structuralProcess "process-one" "layer-one" [
                                link "link-one" ProcessLinkShape.Endpointless
                            ] [ first ]
                            "process-two",
                            structuralProcess "process-two" "layer-two" [
                                link "link-two" ProcessLinkShape.Endpointless
                            ] [ second ]
                        ]
            }

            let session = makeSession "source-two"

            let firstKey =
                project (processReference first "process-one" (IncidentProcess "link-one") [ "link-one" ]) session
                |> _.Key

            let secondReference =
                processReference second "process-two" (IncidentProcess "link-two") [ "link-two" ]

            let secondKey = project secondReference session |> _.Key
            let changedKey = project secondReference (makeSession "source-three") |> _.Key
            Expect.notEqual firstKey secondKey "Different owning-layer source IDs produce different keys."
            Expect.notEqual secondKey changedKey "Changing the owning-layer source ID changes the key."

        testCase "node and process values with equal header value and unit never collapse"
        <| fun _ ->
            let nodeAssignment =
                nodeAssignment "node-assignment" "value-temperature" Generic None

            let processAssignment =
                processAssignment "process-assignment" "value-temperature" [ "link" ] Generic

            let session = {
                basicSession () with
                    Nodes = Map.ofList [ "node", node "node" [ nodeAssignment ] ]
                    Processes =
                        Map.ofList [
                            "process",
                            structuralProcess "process" "layer-one" [ link "link" ProcessLinkShape.Endpointless ] [
                                processAssignment
                            ]
                        ]
            }

            let keys = [
                project (nodeReference nodeAssignment "node" OwnedNode) session |> _.Key
                project (processReference processAssignment "process" (IncidentProcess "link") [ "link" ]) session
                |> _.Key
            ]

            Expect.equal (keys |> List.distinct |> List.length) 2 "Owner kind is explicit in the key."

        testCase "the header carries term accession identity"
        <| fun _ ->
            let firstProperty = property "property-one" (term "Header" (Some "TEST:one"))
            let secondProperty = property "property-two" (term "Header" (Some "TEST:two"))
            let firstValue = value "value-one" firstProperty.Id (ProvenanceValue.Text "same")
            let secondValue = value "value-two" secondProperty.Id (ProvenanceValue.Text "same")
            let first = nodeAssignment "assignment-one" firstValue.Id Generic None
            let second = nodeAssignment "assignment-two" secondValue.Id Generic None

            let session = {
                empty with
                    Properties =
                        Map.ofList [
                            firstProperty.Id, firstProperty
                            secondProperty.Id, secondProperty
                        ]
                    Values = Map.ofList [ firstValue.Id, firstValue; secondValue.Id, secondValue ]
                    Nodes =
                        Map.ofList [
                            "node-one", node "node-one" [ first ]
                            "node-two", node "node-two" [ second ]
                        ]
            }

            Expect.notEqual
                (project (nodeReference first "node-one" OwnedNode) session |> _.Key)
                (project (nodeReference second "node-two" OwnedNode) session |> _.Key)
                "Term accession remains part of the header."

        testCase "reference values group by scheme and durable id and ignore the label"
        <| fun _ ->
            let header = term "Recipe" (Some "TEST:recipe")
            let definition = property "property-recipe" header

            let reference scheme id label =
                ProvenanceValue.Reference {
                    Scheme = scheme
                    Id = id
                    Label = label
                }

            let firstValue =
                value "value-one" definition.Id (reference "recipe" "durable" "First")

            let renamedValue =
                value "value-two" definition.Id (reference "recipe" "durable" "Renamed")

            let otherValue =
                value "value-three" definition.Id (reference "recipe" "other" "First")

            let assignments = [
                nodeAssignment "assignment-one" firstValue.Id Generic None
                nodeAssignment "assignment-two" renamedValue.Id Generic None
                nodeAssignment "assignment-three" otherValue.Id Generic None
            ]

            let session = {
                empty with
                    Properties = Map.ofList [ definition.Id, definition ]
                    Values =
                        Map.ofList [
                            firstValue.Id, firstValue
                            renamedValue.Id, renamedValue
                            otherValue.Id, otherValue
                        ]
                    Nodes =
                        assignments
                        |> List.mapi (fun index assignment -> $"node-{index}", node $"node-{index}" [ assignment ])
                        |> Map.ofList
            }

            let keys =
                assignments
                |> List.mapi (fun index assignment ->
                    project (nodeReference assignment $"node-{index}" OwnedNode) session |> _.Key
                )

            Expect.equal keys[0] keys[1] "Label changes are display-only."
            Expect.notEqual keys[0] keys[2] "A different durable ID is distinct."

        testCase "A,B and B,A produce the same composite key"
        <| fun _ ->
            let a = NodeValue(term "A" None, TextIdentity "a", None)
            let b = NodeValue(term "B" None, TextIdentity "b", None)

            Expect.equal
                (compositeGroupingKey "item" [ a; b ])
                (compositeGroupingKey "item" [ b; a ])
                "Composite grouping is order-independent."

        testCase "A,A,B normalizes to A,B"
        <| fun _ ->
            let a = NodeValue(term "A" None, TextIdentity "a", None)
            let b = NodeValue(term "B" None, TextIdentity "b", None)

            Expect.equal
                (normalizeGroupingKeys [ a; a; b ])
                [ a; b ]
                "Exact duplicates are removed before stable sorting."

        testCase "an item connected to opposite-side nodes carrying A and B is grouped under A,B"
        <| fun _ ->
            let propertyA = property "property-a" (term "A" None)
            let propertyB = property "property-b" (term "B" None)
            let valueA = value "value-a" propertyA.Id (ProvenanceValue.Text "a")
            let valueB = value "value-b" propertyB.Id (ProvenanceValue.Text "b")
            let assignmentA = nodeAssignment "assignment-a" valueA.Id Generic None
            let assignmentB = nodeAssignment "assignment-b" valueB.Id Generic None

            let session = {
                empty with
                    Properties = Map.ofList [ propertyA.Id, propertyA; propertyB.Id, propertyB ]
                    Values = Map.ofList [ valueA.Id, valueA; valueB.Id, valueB ]
                    Nodes =
                        Map.ofList [
                            "node-a", node "node-a" [ assignmentA ]
                            "node-b", node "node-b" [ assignmentB ]
                        ]
            }

            let keys = [
                project (nodeReference assignmentA "node-a" OwnedNode) session |> _.Key
                project (nodeReference assignmentB "node-b" (ReverseConnectionLocal "link-ab")) session
                |> _.Key
            ]

            match compositeGroupingKey "connected-item" keys with
            | GroupedValues normalized ->
                Expect.equal normalized (normalizeGroupingKeys keys) "Both opposite-side values contribute."
            | fallback -> failtestf "Expected grouped values, got %A" fallback

        testCase "items missing a value for an active header do not share a group"
        <| fun _ ->
            Expect.notEqual
                (compositeGroupingKey "item-one" [])
                (compositeGroupingKey "item-two" [])
                "Missing-value fallbacks are item-specific."

        testCase "grouping identity ignores assignment, value, owner, node, process and link ids"
        <| fun _ ->
            let firstValue =
                value "independent-value-one" "property-temperature" (ProvenanceValue.Text "20")

            let secondValue =
                value "independent-value-two" "property-temperature" (ProvenanceValue.Text "20")

            let first = nodeAssignment "independent-assignment-one" firstValue.Id Generic None
            let second = nodeAssignment "independent-assignment-two" secondValue.Id Generic None

            let session = {
                basicSession () with
                    Values = Map.ofList [ firstValue.Id, firstValue; secondValue.Id, secondValue ]
                    Nodes =
                        Map.ofList [
                            "independent-node-one", node "independent-node-one" [ first ]
                            "independent-node-two", node "independent-node-two" [ second ]
                        ]
            }

            Expect.equal
                (project (nodeReference first "independent-node-one" OwnedNode) session |> _.Key)
                (project
                    (nodeReference second "independent-node-two" (ReverseConnectionLocal "independent-link"))
                    session
                 |> _.Key)
                "Storage and evidence IDs remain outside grouping identity."

        testCase "every grouped value retains all backing availability references"
        <| fun _ ->
            let first =
                nodeAssignment
                    "assignment-one"
                    "value-temperature"
                    (AdapterSpecific { Id = "kind-one"; Label = "One" })
                    (Some(source "source-one" "One"))

            let second =
                nodeAssignment
                    "assignment-two"
                    "value-temperature"
                    (AdapterSpecific { Id = "kind-two"; Label = "Two" })
                    (Some(source "source-two" "Two"))

            let session = {
                basicSession () with
                    Nodes =
                        Map.ofList [
                            "node-one", node "node-one" [ first ]
                            "node-two", node "node-two" [ second ]
                        ]
            }

            let annotations = [
                project (nodeReference first "node-one" (ForwardPropagated [ "link-one" ])) session
                project (nodeReference second "node-two" (ReverseConnectionLocal "link-two")) session
            ]

            let grouped = groupProjectedAnnotations annotations |> List.exactlyOne
            Expect.equal grouped.Annotations.Length 2 "No backing reference is discarded."

            Expect.equal
                (grouped.Annotations
                 |> List.map (fun annotation -> annotation.Backing)
                 |> Set.ofList
                 |> Set.count)
                2
                "Assignment, owner, concrete kind and origin backing remain distinct."

            Expect.equal
                (grouped.Annotations
                 |> List.map (fun annotation -> annotation.Availability.Relation)
                 |> Set.ofList)
                (Set.ofList [
                    ForwardPropagated [ "link-one" ]
                    ReverseConnectionLocal "link-two"
                ])
                "Availability relation and link evidence are retained."

        testCase "a display group retains its member endpoint keys and backing references"
        <| fun _ ->
            let session, catalog = surfaceFixture ()
            let projection = projectLayer "layer-one" catalog session |> expectOk

            let outputGroup =
                projection.Groups
                |> List.find (fun group ->
                    group.Side = ProvenanceSide.Output
                    && group.CanonicalNodeIds = Set.ofList [ "node-b"; "node-c" ]
                )

            Expect.equal outputGroup.EndpointKeys.Count 2 "Both member appearances are retained."

            Expect.isTrue
                (outputGroup.Annotations
                 |> List.exists (fun annotation ->
                     match annotation.Backing with
                     | NodeAssignmentBacking(identity, ownerId, _) ->
                         identity.AssignmentId = "assignment-node"
                         && identity.ValueId = "value-node"
                         && ownerId = "node-a"
                         && (
                             match annotation.Availability.Relation with
                             | ForwardPropagated _ -> true
                             | _ -> false
                         )
                     | _ -> false
                 ))
                "The propagated node backing remains addressable."

            Expect.isTrue
                (outputGroup.Annotations
                 |> List.exists (fun annotation -> not annotation.Availability.OriginatingLinkIds.IsEmpty))
                "Process-link evidence remains attached."

        testCase "a display connector retains its backing link ids"
        <| fun _ ->
            let session, catalog = surfaceFixture ()
            let projection = projectLayer "layer-one" catalog session |> expectOk
            let connector = projection.Connectors |> List.exactlyOne
            Expect.equal connector.LinkIds (Set.ofList [ "link-ab"; "link-ac" ]) "Both links are retained."
            Expect.equal connector.StructuralProcessIds (Set.singleton "process-pooled") "Owner is retained."

        testCase "a pooled connector is reported as ambiguous for editing"
        <| fun _ ->
            let session, catalog = surfaceFixture ()

            let connector =
                (projectLayer "layer-one" catalog session |> expectOk).Connectors
                |> List.exactlyOne

            Expect.isTrue (isConnectorEditAmbiguous connector) "Two backing link references stay ambiguous."

        testCase "a pooled connector supports bulk removal"
        <| fun _ ->
            let session, catalog = surfaceFixture ()

            let connector =
                (projectLayer "layer-one" catalog session |> expectOk).Connectors
                |> List.exactlyOne

            let effect =
                availableReferencesForConnector connector
                |> fun references -> removeAvailableReferences "node-a" references session
                |> expectOk

            let actual = commitEffect session effect
            Expect.isEmpty actual.Processes["process-pooled"].Assignments "All represented coverage is removed."
            Expect.equal actual.Processes["process-pooled"].Links.Count 2 "Structural links remain."

        testCase "an endpointless process with a displayable assignment yields a process-only entry"
        <| fun _ ->
            let session, catalog = surfaceFixture ()
            let projection = projectLayer "layer-one" catalog session |> expectOk
            let entry = projection.ProcessOnlyEntries |> List.exactlyOne
            Expect.equal entry.StructuralProcessId "process-endpointless" "The process owner is retained."
            Expect.equal entry.LinkId "link-endpointless" "The endpointless link remains addressable."
            Expect.equal entry.Annotations.Length 1 "Its assignment is displayable."

        testCase "an endpointless process with no displayable assignment yields no entry"
        <| fun _ ->
            let session, catalog = surfaceFixture ()

            let withoutAssignment = {
                session with
                    Processes =
                        session.Processes
                        |> Map.change
                            "process-endpointless"
                            (Option.map (fun structuralProcess -> {
                                structuralProcess with
                                    Assignments = Map.empty
                            }))
            }

            let projection = projectLayer "layer-one" catalog withoutAssignment |> expectOk
            Expect.isEmpty projection.ProcessOnlyEntries "No empty process-only card is projected."
            Expect.isTrue (withoutAssignment.Processes.ContainsKey "process-endpointless") "The process remains."

        testCase "removing the last displayable assignment removes the process-only entry but keeps the process"
        <| fun _ ->
            let session, catalog = surfaceFixture ()

            let effect =
                removeProcessAssignmentLinks
                    "process-endpointless"
                    "assignment-endpointless"
                    (Set.singleton "link-endpointless")
                    session
                |> expectOk

            let actual = commitEffect session effect
            let projection = projectLayer "layer-one" catalog actual |> expectOk
            Expect.isEmpty projection.ProcessOnlyEntries "The empty entry disappears."
            Expect.isTrue (actual.Processes.ContainsKey "process-endpointless") "The structural process survives."

        testCase "a node annotation appears in every layer shelf whose layer contains its node"
        <| fun _ ->
            let session, catalog = surfaceFixture ()

            for layerId in [ "layer-one"; "layer-two" ] do
                let ownedEntries =
                    (projectLayer layerId catalog session |> expectOk).ShelfEntries
                    |> List.choose shelfBacking
                    |> List.filter (fun payload ->
                        match payload.Backing, payload.Availability.Relation with
                        | NodeAssignmentBacking(identity, "node-a", _), OwnedNode ->
                            identity.AssignmentId = "assignment-node"
                        | _ -> false
                    )

                Expect.hasLength ownedEntries 1 $"The owner appears once in {layerId}'s shelf."

        testCase "a propagated shelf entry is marked non-owning"
        <| fun _ ->
            let session, catalog = surfaceFixture ()

            let propagated =
                (projectLayer "layer-two" catalog session |> expectOk).ShelfEntries
                |> List.choose shelfBacking
                |> List.find (fun payload ->
                    payload.CanonicalNodeIds = Set.singleton "node-b"
                    && (
                        match payload.Availability.Relation with
                        | ForwardPropagated _ -> true
                        | _ -> false
                    )
                )

            Expect.notEqual propagated.Availability.Relation OwnedNode "The receiver has no ownership."

        testCase "a propagated shelf entry is copyable but not removable"
        <| fun _ ->
            let session, catalog = surfaceFixture ()

            let propagatedEntry =
                (projectLayer "layer-two" catalog session |> expectOk).ShelfEntries
                |> List.find (fun entry ->
                    shelfBacking entry
                    |> Option.exists (fun payload ->
                        payload.CanonicalNodeIds = Set.singleton "node-b"
                        && (
                            match payload.Availability.Relation with
                            | ForwardPropagated _ -> true
                            | _ -> false
                        )
                    )
                )

            let reference = availableReferenceForShelfEntry propagatedEntry |> Option.get

            let removal = removeAvailableReferences "node-b" [ reference ] session

            Expect.equal
                removal
                (Error(PropagatedRemovalAtReceiver("assignment-node", "node-b")))
                "The propagated shelf item cannot remove its owner."

            let sourceOwnerId, sourceAssignmentId, propertyKind =
                match propagatedEntry.Payload with
                | AssignmentBacked payload ->
                    match payload.Backing with
                    | NodeAssignmentBacking(identity, ownerId, _) ->
                        ownerId, identity.AssignmentId, identity.PropertyKind
                    | _ -> failtest "Expected node backing."
                | _ -> failtest "Expected assignment backing."

            let effect =
                copyLoadedNodeValue sourceOwnerId sourceAssignmentId (Set.singleton "node-c") None session
                |> expectOk

            let actual = commitEffect session effect

            let copied =
                actual.Nodes["node-c"].Assignments |> Map.toList |> List.exactlyOne |> snd

            Expect.equal copied.PropertyKind propertyKind "The concrete kind is retained."
            Expect.notEqual copied.Id sourceAssignmentId "Copying creates a new owned assignment."

        testCase "the catalog appears as read-only resource entries"
        <| fun _ ->
            let session, catalog = surfaceFixture ()
            let projection = projectLayer "layer-one" catalog session |> expectOk

            let catalogEntries =
                projection.ShelfEntries
                |> List.choose (fun entry ->
                    match entry.Payload with
                    | CatalogBacked payload -> Some payload.Entry
                    | _ -> None
                )

            let entry = catalogEntries |> List.exactlyOne
            Expect.equal entry.Reference.Scheme "processcore:recipe" "The exact scheme is retained."
            Expect.equal entry.Reference.Id "recipe-id" "The durable resource ID is retained."
            Expect.equal entry.AssignmentKind AnnotationOwnerKind.Process "The assignment kind is retained."
            Expect.equal session.Processes["process-pooled"].Assignments.Count 1 "Projection creates no assignment."

        testCase "node and process annotations of the same header are colored independently"
        <| fun _ ->
            let header = term "Temperature" (Some "TEST:temperature")
            let nodeKey = propertyColorKey AnnotationOwnerKind.Node header
            let processKey = propertyColorKey AnnotationOwnerKind.Process header

            let settings = colorSettings [] [] [ nodeKey, "#ff0000" ]

            Expect.equal (resolveColor settings nodeKey Set.empty) "#ff0000" "The node override applies."

            Expect.equal
                (resolveColor settings processKey Set.empty)
                defaultColor
                "The process key remains independent."

        testCase "the automatic color takes the source with the greatest set order"
        <| fun _ ->
            let key = propertyColorKey AnnotationOwnerKind.Node (term "Temperature" None)

            let settings =
                colorSettings [ "source-one", "#111111"; "source-two", "#222222" ] [ "source-one", 3; "source-two", 7 ] []

            Expect.equal
                (resolveColor settings key (Set.ofList [ "source-one"; "source-two" ]))
                "#222222"
                "The most recently set applicable source wins."

        testCase "an item with no applicable source color falls back to the fixed default"
        <| fun _ ->
            let session, catalog = surfaceFixture ()
            let settings = colorSettings [ "source-one", "#111111" ] [ "source-one", 1 ] []

            let key =
                propertyColorKey AnnotationOwnerKind.Process (term "Recipe" (Some "TEST:recipe"))

            let catalogEntry =
                (projectLayer "layer-one" catalog session |> expectOk).ShelfEntries
                |> List.find (fun entry ->
                    match entry.Payload with
                    | CatalogBacked _ -> true
                    | _ -> false
                )

            let origins = originSourceIdsForShelfEntry session catalogEntry
            Expect.isEmpty origins "Catalog entries have no backing source."
            Expect.equal (resolveColor settings key origins) defaultColor "The fallback is fixed."

        testCase "a manual color overrides the automatic result"
        <| fun _ ->
            let key = propertyColorKey AnnotationOwnerKind.Node (term "Temperature" None)

            let settings =
                colorSettings [ "source-one", "#111111" ] [ "source-one", 1 ] [ key, "#abcdef" ]

            Expect.equal
                (resolveColor settings key (Set.singleton "source-one"))
                "#abcdef"
                "Manual selection has highest precedence."

        testCase "every shelf representation of one owning node assignment has the same color"
        <| fun _ ->
            let session, catalog = surfaceFixture ()

            let settings =
                colorSettings [ "source-one", "#111111"; "source-two", "#222222" ] [ "source-one", 1; "source-two", 2 ] []

            let owningShelfEntry layerId =
                (projectLayer layerId catalog session |> expectOk).ShelfEntries
                |> List.find (fun entry ->
                    match entry.Payload with
                    | AssignmentBacked payload ->
                        match payload.Backing, payload.Availability.Relation with
                        | NodeAssignmentBacking(identity, "node-a", _), OwnedNode ->
                            identity.AssignmentId = "assignment-node"
                        | _ -> false
                    | _ -> false
                )

            let key =
                propertyColorKey AnnotationOwnerKind.Node (term "Node value" (Some "TEST:node"))

            let color layerId =
                let entry = owningShelfEntry layerId
                resolveColor settings key (originSourceIdsForShelfEntry session entry)

            Expect.equal (color "layer-one") (color "layer-two") "Both shelves resolve from the owner node."

        testCase "a grouping chip may differ between layers when its aggregated set differs"
        <| fun _ ->
            let session, catalog = surfaceFixture ()
            let owned = session.Nodes["node-a"].Assignments["assignment-node"]

            let separateAssignment = {
                owned with
                    Id = "assignment-node-d"
                    Lineage = AssignmentLineage.Created
            }

            let session = withOutputNodeD [ separateAssignment ] session

            let settings =
                colorSettings [ "source-one", "#111111"; "source-two", "#222222" ] [ "source-one", 1; "source-two", 2 ] []

            let nodeKey =
                NodeValue(term "Node value" (Some "TEST:node"), TextIdentity "node", None)

            let layerOneGroup =
                (projectLayer "layer-one" catalog session |> expectOk).Groups
                |> List.find (fun group -> group.CanonicalNodeIds = Set.singleton "node-d")

            let layerTwoGroup =
                (projectLayer "layer-two" catalog session |> expectOk).Groups
                |> List.find (fun group -> group.CanonicalNodeIds.Contains "node-a")

            let color (group: DisplayGroup) =
                originSourceIdsForGroupingValue session nodeKey group.Annotations
                |> resolveColor
                    settings
                    (propertyColorKey AnnotationOwnerKind.Node (term "Node value" (Some "TEST:node")))

            Expect.notEqual (color layerOneGroup) (color layerTwoGroup) "Each chip uses its own backing set."

        testCase "a connection that does not change an item's backing references leaves its color unchanged"
        <| fun _ ->
            let session, catalog = surfaceFixture ()
            let session = withOutputNodeD [] session

            let settings =
                colorSettings [ "source-one", "#111111"; "source-two", "#222222" ] [ "source-one", 1; "source-two", 2 ] []

            let shelfEntry current =
                (projectLayer "layer-one" catalog current |> expectOk).ShelfEntries
                |> List.find (fun entry ->
                    match entry.Payload with
                    | AssignmentBacked payload ->
                        match payload.Backing, payload.Availability.Relation with
                        | NodeAssignmentBacking(identity, "node-a", _), OwnedNode ->
                            identity.AssignmentId = "assignment-node"
                        | _ -> false
                    | _ -> false
                )

            let key =
                propertyColorKey AnnotationOwnerKind.Node (term "Node value" (Some "TEST:node"))

            let color current =
                let entry = shelfEntry current
                resolveColor settings key (originSourceIdsForShelfEntry current entry)

            let before = color session

            let after =
                Swate.Components.Page.ProvenanceGrouping.CanonicalSession.connectNodes
                    "layer-one"
                    [ "node-a", "node-d" ]
                    session
                |> expectOk

            Expect.equal (color after) before "The owner node's appearances did not change."

        testCase "a connection that changes availability may change a grouping chip's automatic color"
        <| fun _ ->
            let session, catalog = surfaceFixture ()
            let session = withOutputNodeD [] session

            let settings =
                colorSettings [ "source-one", "#111111"; "source-two", "#222222" ] [ "source-one", 1; "source-two", 2 ] []

            let key = NodeValue(term "Node value" (Some "TEST:node"), TextIdentity "node", None)

            let chipColor current =
                let group =
                    (projectLayer "layer-one" catalog current |> expectOk).Groups
                    |> List.find (fun group -> group.CanonicalNodeIds = Set.singleton "node-d")

                originSourceIdsForGroupingValue current key group.Annotations
                |> resolveColor
                    settings
                    (propertyColorKey AnnotationOwnerKind.Node (term "Node value" (Some "TEST:node")))

            let before = chipColor session

            let after =
                Swate.Components.Page.ProvenanceGrouping.CanonicalSession.connectNodes
                    "layer-one"
                    [ "node-a", "node-d" ]
                    session
                |> expectOk

            Expect.notEqual (chipColor after) before "The propagated backing adds its owner's sources."
    ]

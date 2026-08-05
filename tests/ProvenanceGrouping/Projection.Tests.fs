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
open Swate.Components.Page.ProvenanceGrouping.Types
open Swate.Components.Page.ProvenanceGrouping.Commands

module StoryFixtures = Swate.Components.Page.ProvenanceGrouping.StoryFixtures

module State = Swate.Components.Page.ProvenanceGrouping.State
module PropertyColors = Swate.Components.Page.ProvenanceGrouping.State.PropertyColors

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

/// A grouping selection, expressed the way the UI supplies one: the headers a
/// side currently groups by (intent §7). The cached projection is the finest
/// partition, so anything about *shared* cards - pooled connectors, several
/// members on one card - only exists once a header is active.
let private groupedBy (headerNames: string list) : ActiveGroupingKeys =
    fun _ key ->
        let header =
            match key with
            | NodeValue(header, _, _) -> header
            | ProcessValue(header, _, _, _) -> header

        headerNames |> List.contains header.Name

let private displaySurface active layerId catalog session =
    let layer = session.Layers |> Map.find layerId
    let projection = projectLayer layerId catalog session |> expectOk
    regroupLayer active layer session projection |> expectOk

let private displayGroups active layerId catalog session =
    displaySurface active layerId catalog session |> fst

let private commitEffect session effect =
    Swate.Components.Page.ProvenanceGrouping.CanonicalSession.commit effect session

let private shelfBacking =
    function
    | { Payload = AssignmentBacked payload } -> Some payload
    | _ -> None

let private colorSettings sourceColors setOrder manualColors : PropertyColorSettings = {
    SourceColors = Map.ofList sourceColors
    SourceColorSetOrder = Map.ofList setOrder
    ManualPropertyColors = Map.ofList manualColors
    NextSourceColorSetOrder = (setOrder |> List.fold (fun acc (_, order) -> max acc (order + 1)) 0)
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

        testCase "drop conflicts use exact property kind and identified assignment"
        <| fun _ ->
            let header = term "Temperature" (Some "TEST:temperature")
            let definition = property "property-temperature" header

            let characteristicKind =
                AdapterSpecific {
                    Id = "processcore:characteristic"
                    Label = "Characteristic"
                }

            let factorKind =
                AdapterSpecific {
                    Id = "processcore:factor"
                    Label = "Factor"
                }

            let firstValue = value "value-first" definition.Id (ProvenanceValue.Text "first")
            let secondValue = value "value-second" definition.Id (ProvenanceValue.Text "second")
            let factorValue = value "value-factor" definition.Id (ProvenanceValue.Text "factor")

            let first = nodeAssignment "assignment-first" firstValue.Id characteristicKind None

            let second =
                nodeAssignment "assignment-second" secondValue.Id characteristicKind None

            let factor = nodeAssignment "assignment-factor" factorValue.Id factorKind None

            let session = {
                empty with
                    Nodes = Map.ofList [ "node-one", node "node-one" [ first; second; factor ] ]
                    Properties = Map.ofList [ definition.Id, definition ]
                    Values =
                        Map.ofList [
                            firstValue.Id, firstValue
                            secondValue.Id, secondValue
                            factorValue.Id, factorValue
                        ]
            }

            let annotations =
                [ first; second; factor ]
                |> List.map (fun assignment -> project (nodeReference assignment "node-one" OwnedNode) session)

            let group = {
                Id = "group-one"
                Side = ProvenanceSide.Input
                GroupingValues = []
                CanonicalNodeIds = Set.singleton "node-one"
                EndpointKeys = Set.empty
                ProcessLinkIds = Set.empty
                Annotations = annotations
                AnnotationsByNodeId = Map.ofList [ "node-one", annotations ]
            }

            let source: ValueAssignmentSource = {
                Key = {
                    Kind = AnnotationOwnerKind.Node
                    Header = header
                }
                PropertyKind = characteristicKind
                Value = ProvenanceValue.Text "replacement"
                Unit = None
                ContainerReferenceValueId = None
                ReferenceSlotId = None
                CopiedFromAssignmentId = Some first.Id
            }

            let batch =
                Swate.Components.Page.ProvenanceGrouping.ValueAssignment.planNodeValueDropToGroups
                    source
                    definition.Id
                    source.CopiedFromAssignmentId
                    [ group ]
                    session
                |> expectOk

            Expect.isEmpty batch.Adds "The identified occurrence is replaced rather than duplicated."
            Expect.equal batch.Overwrites.Length 1 "One exact overwrite is planned."

            Expect.equal
                batch.Overwrites.Head.ExistingAssignmentIds
                (Set.singleton first.Id)
                "The other same-kind occurrence and the different concrete kind are preserved."

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

        testCase "a node assignment's writeback target does not project as its origin source"
        <| fun _ ->
            let assignment =
                nodeAssignment "assignment-one" "value-temperature" Generic (Some(source "source-one" "One"))

            let session = {
                basicSession () with
                    Nodes = Map.ofList [ "node-one", node "node-one" [ assignment ] ]
            }

            let projected = project (nodeReference assignment "node-one" OwnedNode) session

            Expect.equal
                projected.DerivedOriginSource
                None
                "A node assignment stores no origin (intent §2); its optional writeback target must not surface as one."

            match projected.Backing with
            | NodeAssignmentBacking(_, _, targetSource) ->
                Expect.equal
                    targetSource
                    (Some(source "source-one" "One"))
                    "The writeback target is still retained on the backing, just not projected as an origin."
            | _ -> failtest "Expected node backing."

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

        testCase "with no active grouping header every item keeps its own card"
        <| fun _ ->
            let session, catalog = surfaceFixture ()
            let groups = session |> displayGroups (groupedBy []) "layer-one" catalog

            Expect.equal
                (groups |> List.map _.CanonicalNodeIds |> Set.ofList)
                (Set.ofList [
                    Set.singleton "node-a"
                    Set.singleton "node-b"
                    Set.singleton "node-c"
                ])
                "Every endpoint keeps its item-specific fallback key."

            // node-b and node-c carry an identical annotation set, so a key built
            // from the annotations alone would merge them. The fallback key is
            // per item, which is what keeps them apart (intent §7).
            Expect.isTrue
                (groups |> List.forall (fun group -> group.GroupingValues.IsEmpty))
                "A fallback-keyed card has no grouping value and shows its endpoint name."

        testCase "group process targets are layer- and side-local"
        <| fun _ ->
            let session, catalog = surfaceFixture ()

            let inLayer = session.Processes["process-pooled"]

            let session = {
                session with
                    Processes =
                        session.Processes
                        |> Map.add "process-pooled" {
                            inLayer with
                                Links =
                                    inLayer.Links
                                    |> Map.add
                                        "wrong-output-only"
                                        (link "wrong-output-only" (ProcessLinkShape.OutputOnly "node-a"))
                                    |> Map.add
                                        "wrong-input-only"
                                        (link "wrong-input-only" (ProcessLinkShape.InputOnly "node-b"))
                        }
                        |> Map.add
                            "other-layer-process"
                            (structuralProcess "other-layer-process" "layer-two" [
                                link "other-layer-link" (ProcessLinkShape.InputOnly "node-a")
                            ] [])
            }

            let groups = session |> displayGroups (groupedBy []) "layer-one" catalog

            let inputA =
                groups
                |> List.find (fun group ->
                    group.Side = ProvenanceSide.Input
                    && group.CanonicalNodeIds = Set.singleton "node-a"
                )

            let outputB =
                groups
                |> List.find (fun group ->
                    group.Side = ProvenanceSide.Output
                    && group.CanonicalNodeIds = Set.singleton "node-b"
                )

            Expect.equal
                inputA.ProcessLinkIds
                (Set.ofList [ "link-ab"; "link-ac" ])
                "Input cards include only outgoing/input-only links from this layer."

            Expect.equal
                outputB.ProcessLinkIds
                (Set.singleton "link-ab")
                "Output cards include only incoming/output-only links from this layer."

        testCase "a placed catalog entry remains a catalog-backed rail value"
        <| fun _ ->
            let session, catalog = surfaceFixture ()
            let entry = catalog[("processcore:recipe", "recipe-id")]

            let secondEntry = {
                entry with
                    Reference = {
                        entry.Reference with
                            Id = "other/recipe-id"
                    }
            }

            let catalog =
                catalog
                |> Map.add (secondEntry.Reference.Scheme, secondEntry.Reference.Id) secondEntry

            let projection = projectLayer "layer-one" catalog session |> expectOk

            let header: GroupingKey = {
                Kind = entry.AssignmentKind
                Header = entry.Category
            }

            let uiState =
                Swate.Components.Page.ProvenanceGrouping.State.init session
                |> Swate.Components.Page.ProvenanceGrouping.State.PropertyPlacement.place
                    "layer-one"
                    ProvenanceSide.Input
                    header

            let rail =
                Swate.Components.Page.ProvenanceGrouping.PropertyProjection.railProjectionWithFilters
                    session
                    "layer-one"
                    ProvenanceSide.Input
                    projection
                    uiState

            Expect.contains rail.Headers header "Placement makes the catalog header visible on the requested rail."

            Expect.isTrue
                (rail.ValuesByHeader[header]
                 |> List.exists (
                     function
                     | Swate.Components.Page.ProvenanceGrouping.PropertyRails.CatalogValue(actual, _) -> actual = entry
                     | _ -> false
                 ))
                "The rail preserves catalog identity so assignment can use the catalog-aware command."

            let displayedLabels =
                rail.ValuesByHeader[header]
                |> List.choose (
                    function
                    | Swate.Components.Page.ProvenanceGrouping.PropertyRails.CatalogValue(_, displayLabel) ->
                        Some displayLabel
                    | _ -> None
                )

            Expect.equal displayedLabels.Length 2 "Both exact catalog identities remain visible."

            Expect.equal
                (displayedLabels |> List.distinct |> List.length)
                2
                "Colliding labels are disambiguated on the rail."

            Expect.isTrue
                (projection.ShelfEntries
                 |> List.exists (
                     function
                     | { Payload = CatalogBacked payload } -> payload.Entry = entry
                     | _ -> false
                 ))
                "Rail placement does not consume the external catalog resource."

        testCase "an active grouping header merges the items that share its value and separates the rest"
        <| fun _ ->
            let session, catalog = surfaceFixture ()

            // node-d is an output with no annotation at all, so it can neither
            // share a value nor collapse into a shared missing-value group.
            let session = {
                session with
                    Nodes = session.Nodes |> Map.add "node-d" (node "node-d" [])
                    Layers =
                        session.Layers
                        |> Map.add "layer-one" {
                            session.Layers["layer-one"] with
                                OutputEndpoints =
                                    session.Layers["layer-one"].OutputEndpoints
                                    |> Map.add "node-d" (appearance "layer-one" ProvenanceSide.Output "node-d" 2)
                        }
            }

            let outputs =
                session
                |> displayGroups (groupedBy [ "Node value" ]) "layer-one" catalog
                |> List.filter (fun group -> group.Side = ProvenanceSide.Output)

            Expect.equal
                (outputs |> List.map _.CanonicalNodeIds |> Set.ofList)
                (Set.ofList [
                    Set.ofList [ "node-b"; "node-c" ]
                    Set.singleton "node-d"
                ])
                "The two items holding the value share one card; the item without it stays on its own."

            let merged =
                outputs |> List.find (fun group -> group.CanonicalNodeIds.Contains "node-b")

            Expect.equal
                merged.GroupingValues
                [
                    NodeValue(term "Node value" (Some "TEST:node"), TextIdentity "node", None)
                ]
                "The card's key is the grouping value it was formed from, not every annotation its members carry."

            Expect.isTrue
                (merged.Annotations
                 |> List.exists (fun annotation ->
                     match annotation.Key with
                     | ProcessValue(header, _, _, _) -> header.Name = "Process value"
                     | NodeValue _ -> false
                 ))
                "Annotations outside the grouping key stay on the card as backing."

            Expect.isTrue
                ((outputs |> List.find (fun group -> group.CanonicalNodeIds.Contains "node-d")).GroupingValues.IsEmpty)
                "An item with no value for the active header keeps an item-specific fallback key."

        testCase "an item holding several values of the active header is keyed on all of them"
        <| fun _ ->
            let session, catalog = surfaceFixture ()

            // node-a is the input of both pooled links, so a second process value
            // on one of them reaches it alongside the first: intent §7's "an item
            // connected to opposite-side nodes carrying A and B is grouped under
            // the normalized key A, B", at card level.
            let second =
                value "value-process-second" "property-process" (ProvenanceValue.Text "second")

            let session = {
                session with
                    Values = session.Values |> Map.add second.Id second
                    Processes =
                        session.Processes
                        |> Map.add "process-pooled" {
                            session.Processes["process-pooled"] with
                                Assignments =
                                    session.Processes["process-pooled"].Assignments
                                    |> Map.add
                                        "assignment-process-second"
                                        (processAssignment "assignment-process-second" second.Id [ "link-ac" ] Generic)
                        }
            }

            let inputCard =
                session
                |> displayGroups (groupedBy [ "Process value" ]) "layer-one" catalog
                |> List.find (fun group -> group.Side = ProvenanceSide.Input)

            let keyedValues =
                inputCard.GroupingValues
                |> List.map (
                    function
                    | ProcessValue(_, TextIdentity value, _, _) -> value
                    | key -> failtestf "Expected a process text value but got %A" key
                )

            Expect.equal (Set.ofList keyedValues) (Set.ofList [ "process"; "second" ]) "Both values form the key."

            // The order is `normalizeGroupingKeys`' stable total order over the
            // encoded representation, not the value text: `valueSortKey` is
            // length-prefixed, so "6:second" precedes "7:process". Pinning it here
            // keeps the key from silently becoming order-sensitive.
            Expect.equal keyedValues [ "second"; "process" ] "The key is stably ordered by the encoded representation."

        testCase "node and process values of the same header never group together"
        <| fun _ ->
            let session, catalog = surfaceFixture ()

            // Same header text on both sides of the owner divide: node-b gets it
            // as a propagated node value, node-c only as an incident process one.
            let shared = property "property-shared" (term "Shared" None)
            let sharedValue = value "value-shared" shared.Id (ProvenanceValue.Text "same")

            let session = {
                session with
                    Properties = session.Properties |> Map.add shared.Id shared
                    Values = session.Values |> Map.add sharedValue.Id sharedValue
                    Nodes =
                        session.Nodes
                        |> Map.add
                            "node-b"
                            (node "node-b" [
                                nodeAssignment "assignment-shared-node" sharedValue.Id Generic None
                            ])
                    Processes =
                        session.Processes
                        |> Map.add "process-pooled" {
                            session.Processes["process-pooled"] with
                                Assignments =
                                    session.Processes["process-pooled"].Assignments
                                    |> Map.add
                                        "assignment-shared-process"
                                        (processAssignment
                                            "assignment-shared-process"
                                            sharedValue.Id
                                            [ "link-ac" ]
                                            Generic)
                        }
            }

            let outputs =
                session
                |> displayGroups (groupedBy [ "Shared" ]) "layer-one" catalog
                |> List.filter (fun group -> group.Side = ProvenanceSide.Output)

            Expect.equal
                (outputs |> List.map _.CanonicalNodeIds |> Set.ofList)
                (Set.ofList [ Set.singleton "node-b"; Set.singleton "node-c" ])
                "Equal header, value and unit still do not group a node value with a process value."

        testCase "a second active header composites, and an item missing one groups by the one it has"
        <| fun _ ->
            let session, catalog = surfaceFixture ()

            let outputs =
                session
                |> displayGroups (groupedBy [ "Node value"; "Process value" ]) "layer-one" catalog
                |> List.filter (fun group -> group.Side = ProvenanceSide.Output)

            let merged = outputs |> List.exactlyOne

            Expect.equal
                merged.GroupingValues.Length
                2
                "Both active headers contribute to the composite key (intent §7)."

            Expect.equal
                merged.CanonicalNodeIds
                (Set.ofList [ "node-b"; "node-c" ])
                "Items agreeing on both headers share one card."

        testCase "a display group retains its member endpoint keys and backing references"
        <| fun _ ->
            let session, catalog = surfaceFixture ()

            let outputGroup =
                session
                |> displayGroups (groupedBy [ "Node value" ]) "layer-one" catalog
                |> List.find (fun group ->
                    group.Side = ProvenanceSide.Output
                    && group.CanonicalNodeIds = Set.ofList [ "node-b"; "node-c" ]
                )

            Expect.equal outputGroup.EndpointKeys.Count 2 "Both member appearances are retained."

            for nodeId in [ "node-b"; "node-c" ] do
                let memberAnnotations =
                    outputGroup.AnnotationsByNodeId |> Map.tryFind nodeId |> Option.defaultValue []

                Expect.isTrue
                    (memberAnnotations
                     |> List.exists (fun annotation ->
                         match annotation.Backing with
                         | NodeAssignmentBacking(identity, ownerId, _) ->
                             identity.AssignmentId = "assignment-node" && ownerId = "node-a"
                         | _ -> false
                     ))
                    $"{nodeId} retains the propagated assignment visible on that exact member appearance."

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

            let connector =
                session
                |> displaySurface (groupedBy [ "Node value" ]) "layer-one" catalog
                |> snd
                |> List.exactlyOne

            Expect.equal connector.LinkIds (Set.ofList [ "link-ab"; "link-ac" ]) "Both links are retained."
            Expect.equal connector.StructuralProcessIds (Set.singleton "process-pooled") "Owner is retained."

        testCase "a pooled connector is reported as ambiguous for editing"
        <| fun _ ->
            let session, catalog = surfaceFixture ()

            let connector =
                session
                |> displaySurface (groupedBy [ "Node value" ]) "layer-one" catalog
                |> snd
                |> List.exactlyOne

            Expect.isTrue (isConnectorEditAmbiguous connector) "Two backing link references stay ambiguous."

        testCase "a pooled connector supports bulk removal"
        <| fun _ ->
            let session, catalog = surfaceFixture ()

            let connector =
                session
                |> displaySurface (groupedBy [ "Node value" ]) "layer-one" catalog
                |> snd
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

        testCase "a process assignment appears once in its layer shelf with exact coverage"
        <| fun _ ->
            let session, catalog = surfaceFixture ()

            let entries =
                (projectLayer "layer-one" catalog session |> expectOk).ShelfEntries
                |> List.choose shelfBacking
                |> List.filter (fun payload ->
                    match payload.Backing with
                    | ProcessAssignmentBacking(identity, "process-pooled", coveredLinkIds, _, _) ->
                        identity.AssignmentId = "assignment-process"
                        && coveredLinkIds = Set.ofList [ "link-ab"; "link-ac" ]
                    | _ -> false
                )

            Expect.hasLength entries 1 "One assignment-backed shelf entry retains the pooled link coverage."

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

            Expect.equal (PropertyColors.resolveColor settings nodeKey Set.empty) "#ff0000" "The node override applies."

            Expect.equal
                (PropertyColors.resolveColor settings processKey Set.empty)
                PropertyColors.defaultColor
                "The process key remains independent."

        testCase "the automatic color takes the source with the greatest set order"
        <| fun _ ->
            let key = propertyColorKey AnnotationOwnerKind.Node (term "Temperature" None)

            let settings =
                colorSettings [ "source-one", "#111111"; "source-two", "#222222" ] [ "source-one", 3; "source-two", 7 ] []

            Expect.equal
                (PropertyColors.resolveColor settings key (Set.ofList [ "source-one"; "source-two" ]))
                "#222222"
                "The most recently set applicable source wins."

        testCase "automatic source colors receive deterministic set-order entries"
        <| fun _ ->
            let session, _ = surfaceFixture ()
            let settings = State.init session |> PropertyColors.ensureSourceColors session
            let sourceId = session.Layers["layer-one"].Source.Id
            let key = propertyColorKey AnnotationOwnerKind.Node (term "Temperature" None)

            Expect.isTrue (settings.SourceColorSetOrder.ContainsKey sourceId) "The source participates in resolution."

            Expect.equal
                (PropertyColors.resolveColor settings key (Set.singleton sourceId))
                settings.SourceColors[sourceId]
                "An automatically assigned layer color is used instead of the source-less fallback."

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

            Expect.equal
                (PropertyColors.resolveColor settings key origins)
                PropertyColors.defaultColor
                "The fallback is fixed."

        testCase "a manual color overrides the automatic result"
        <| fun _ ->
            let key = propertyColorKey AnnotationOwnerKind.Node (term "Temperature" None)

            let settings =
                colorSettings [ "source-one", "#111111" ] [ "source-one", 1 ] [ key, "#abcdef" ]

            Expect.equal
                (PropertyColors.resolveColor settings key (Set.singleton "source-one"))
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
                PropertyColors.resolveColor settings key (originSourceIdsForShelfEntry session entry)

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
                |> PropertyColors.resolveColor
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
                PropertyColors.resolveColor settings key (originSourceIdsForShelfEntry current entry)

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
                |> PropertyColors.resolveColor
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

        // ── Story fixtures ──────────────────────────────────────────────────
        //
        // Every ProvenanceGrouping story renders one of these sessions, so a
        // wrong owner kind here silently changes what dozens of stories should
        // see. These assert the two claims the canonical port rests on: a
        // parameter is a process annotation that is still visible on the nodes
        // that carried it, and a cross-source characteristic reaches its
        // downstream node by propagation rather than by being copied into it.

        testCase "every story fixture projects its active layer and survives writeback preparation"
        <| fun _ ->
            let fixtures = [
                "sample", StoryFixtures.createSampleSession ()
                "chained", StoryFixtures.createChainedSession ()
                "inputOnly", StoryFixtures.createInputOnlySession ()
                "outputOnly", StoryFixtures.createOutputOnlySession ()
                "disconnectedProperty", StoryFixtures.createDisconnectedPropertySession ()
                "switchableProperty", StoryFixtures.createSwitchablePropertySession ()
                "typedSample", StoryFixtures.createTypedSampleSession ()
                "retaggedTypedSample", StoryFixtures.createRetaggedTypedSampleSession ()
                "dataOutputOnly", StoryFixtures.createDataOutputOnlySession ()
            ]

            for name, session in fixtures do
                for layerId in session.LayerOrder do
                    projectLayer layerId Map.empty session
                    |> function
                        | Ok _ -> ()
                        | Error error -> failtestf "%s: layer %s failed to project: %A" name layerId error

                Swate.Components.Page.ProvenanceGrouping.CanonicalSession.prepareForWriteback session
                |> function
                    | Ok _ -> ()
                    | Error error -> failtestf "%s: preparation failed: %A" name error

        testCase "the sample fixture keeps its parameters visible on the nodes that carried them"
        <| fun _ ->
            let session = StoryFixtures.createSampleSession ()
            let projection = projectLayer "layer-1" Map.empty session |> expectOk

            let headersOn nodeId =
                projection.Groups
                |> List.filter (fun group -> group.CanonicalNodeIds.Contains nodeId)
                |> List.collect _.Annotations
                |> List.map (fun annotation ->
                    match annotation.Key with
                    | NodeValue(header, _, _) -> header.Name
                    | ProcessValue(header, _, _, _) -> header.Name
                )
                |> Set.ofList

            // Temperature and Analysis are parameters, so they are process
            // annotations - and incident-process availability still puts them on
            // the endpoints of their covered links.
            Expect.isTrue
                ((headersOn "node-input-a").Contains "Temperature")
                "Input A still sees the parameter that the old fixture attached to it."

            Expect.isTrue ((headersOn "node-input-b").Contains "Temperature") "Input B still sees Temperature."
            Expect.isTrue ((headersOn "node-input-c").Contains "Temperature") "Input C still sees Temperature."
            Expect.isTrue ((headersOn "node-output-a").Contains "Analysis") "Output A still sees Analysis."
            Expect.isTrue ((headersOn "node-output-c").Contains "Analysis") "Output C still sees Analysis."
            Expect.isTrue ((headersOn "node-input-a").Contains "Species") "Species remains an owned node annotation."

            // Input D has no Temperature in the fixture, and grouping must not
            // invent one for it. Asserting Species first keeps the absence
            // meaningful: an empty projection would satisfy the absence alone.
            Expect.isTrue ((headersOn "node-input-d").Contains "Species") "Input D is projected and carries Species."
            Expect.isFalse ((headersOn "node-input-d").Contains "Temperature") "Input D carries no Temperature."

            let keyKinds nodeId =
                projection.Groups
                |> List.filter (fun group -> group.CanonicalNodeIds.Contains nodeId)
                |> List.collect _.Annotations
                |> List.choose (fun annotation ->
                    match annotation.Key with
                    | NodeValue(header, _, _) -> Some(header.Name, "node")
                    | ProcessValue(header, _, _, _) -> Some(header.Name, "process")
                )
                |> Set.ofList

            Expect.isTrue
                ((keyKinds "node-input-a").Contains("Temperature", "process"))
                "Temperature contributes a process grouping key, not a node one."

            Expect.isTrue
                ((keyKinds "node-input-a").Contains("Species", "node"))
                "Species contributes a node grouping key."

        testCase "the chained fixture's boundary node owns its annotation once and propagates it downstream"
        <| fun _ ->
            let session = StoryFixtures.createChainedSession ()

            let batchOriginOn layerId nodeId =
                (projectLayer layerId Map.empty session |> expectOk).Groups
                |> List.filter (fun group -> group.CanonicalNodeIds.Contains nodeId)
                |> List.collect _.Annotations
                |> List.filter (fun annotation ->
                    match annotation.Key with
                    | NodeValue(header, _, _) -> header.Name = "Batch Origin"
                    | ProcessValue _ -> false
                )

            // Owned where it lives, in both layers the boundary node appears in.
            for layerId in [ "layer-1"; "layer-2" ] do
                let owned = batchOriginOn layerId "node-culture"
                Expect.isNonEmpty owned $"Culture Batch shows its own annotation in {layerId}."

                for annotation in owned do
                    match annotation.Backing with
                    | NodeAssignmentBacking(_, ownerId, _) ->
                        Expect.equal ownerId "node-culture" "The boundary node owns it in every layer it appears in."
                    | backing -> failtestf "Expected a node backing but got %A" backing

                    Expect.equal
                        annotation.Availability.Relation
                        OwnedNode
                        "Every appearance of the owning node reports ownership, not propagation."

            // And propagated, not owned, on the downstream node.
            let downstream = batchOriginOn "layer-2" "node-extract"
            Expect.isNonEmpty downstream "Batch Origin reaches Extract Batch through the measurement link."

            for annotation in downstream do
                match annotation.Backing with
                | NodeAssignmentBacking(_, ownerId, _) ->
                    Expect.equal ownerId "node-culture" "Extract Batch does not own the propagated annotation."
                | backing -> failtestf "Expected a node backing but got %A" backing

                match annotation.Availability.Relation with
                | OwnedNode -> failtest "A propagated annotation must not be reported as owned on the receiver."
                | _ -> ()

            Expect.isTrue
                (session.Nodes["node-extract"].Assignments |> Map.isEmpty)
                "Extract Batch's own bucket stays empty."

        // Design §3.5: a node shelf entry's origin sources are "its one owning
        // node's appearance sources" - not the viewing layer's. A node does not
        // belong to a layer, so every layer the owner appears in contributes a
        // source to *every* annotation that owner holds.
        testCase "a node shelf entry takes its origin sources from its owning node's appearances"
        <| fun _ ->
            let sourcesForProperty session layerId propertyId =
                (projectLayer layerId Map.empty session |> expectOk).ShelfEntries
                |> List.filter (fun entry ->
                    match entry.Payload with
                    | AssignmentBacked payload ->
                        match payload.Backing with
                        | NodeAssignmentBacking(identity, _, _) ->
                            (session: ProvenanceSession).Values[identity.ValueId].PropertyId = propertyId
                        | ProcessAssignmentBacking _ -> false
                    | CatalogBacked _ -> false
                )
                |> List.map (originSourceIdsForShelfEntry session)
                |> List.fold Set.union Set.empty

            // The sample fixture is one layer, so every owned node annotation
            // resolves to that one source. This is why the fixture cannot give
            // Previous Treatment a second source by adding a layer: Input A owns
            // Species too, and an extra appearance would move both.
            let sample = StoryFixtures.createSampleSession ()

            Expect.equal
                (sourcesForProperty sample "layer-1" "property-species")
                (Set.singleton "fixture:assay-table")
                "Species resolves to its owners' only appearance source."

            Expect.equal
                (sourcesForProperty sample "layer-1" "property-previous-treatment")
                (Set.singleton "fixture:assay-table")
                "Previous Treatment is owned by Input A, so it resolves to Input A's appearance source."

            // The chained fixture is where a non-layer source genuinely arises:
            // Culture Batch is the boundary node, so it appears in both layers
            // and its owned annotation carries both sources - including one that
            // is not the viewing layer's.
            let chained = StoryFixtures.createChainedSession ()

            let batchOriginSources =
                sourcesForProperty chained "layer-2" "property-batch-origin"

            Expect.equal
                batchOriginSources
                (Set.ofList [ "fixture:growth-table"; "fixture:measurement-table" ])
                "The boundary node's annotation carries the union of its appearance sources."

            Expect.isTrue
                (batchOriginSources.Contains "fixture:growth-table")
                "Viewed from the measurement layer, that union includes a source other than the viewing layer's."
    ]

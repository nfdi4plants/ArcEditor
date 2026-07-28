module CanonicalProjectionTests

open Expecto
open Swate.Components.Page.ProvenanceGrouping.Identifiers
open Swate.Components.Page.ProvenanceGrouping.Values
open Swate.Components.Page.ProvenanceGrouping.Domain
open Swate.Components.Page.ProvenanceGrouping.AvailabilityTypes
open Swate.Components.Page.ProvenanceGrouping.ProjectionTypes
open Swate.Components.Page.ProvenanceGrouping.Model
open Swate.Components.Page.ProvenanceGrouping.Projection

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
    ]

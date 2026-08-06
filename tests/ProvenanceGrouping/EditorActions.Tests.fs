module EditorActionsTests

open Expecto
open Swate.Components.Page.ProvenanceGrouping
open Swate.Components.Page.ProvenanceGrouping.Identifiers
open Swate.Components.Page.ProvenanceGrouping.Values
open Swate.Components.Page.ProvenanceGrouping.Domain
open Swate.Components.Page.ProvenanceGrouping.ProjectionTypes
open Swate.Components.Page.ProvenanceGrouping.AvailabilityTypes
open Swate.Components.Page.ProvenanceGrouping.MutationTypes
open Swate.Components.Page.ProvenanceGrouping.Model
open Swate.Components.Page.ProvenanceGrouping.Types

let private sampleKind: ProvenanceKind = {
    Id = "test:endpoint:sample"
    Label = "Sample"
}

let private endpoint nodeId position : LayerEndpoint = {
    Key = {
        LayerId = "layer-1"
        Side = ProvenanceSide.Input
        NodeId = nodeId
    }
    Header = {
        Kind = sampleKind
        Text = $"Endpoint {nodeId}"
    }
    LayerOrderPosition = position
}

let private layer inputEndpoints outputEndpoints : ProvenanceLayer = {
    Id = "layer-1"
    Label = "Test Layer"
    Source = { Id = "src-1"; Name = "test" }
    InputEndpoints = inputEndpoints |> List.map (fun (id, pos) -> id, endpoint id pos) |> Map.ofList
    OutputEndpoints = outputEndpoints |> List.map (fun (id, pos) -> id, endpoint id pos) |> Map.ofList
    StructuralProcessIds = Set.empty
}

let private group side nodeIds : DisplayGroup = {
    Id = $"group-{side}"
    Side = side
    GroupingValues = []
    CanonicalNodeIds = Set.ofList nodeIds
    EndpointKeys = Set.empty
    ProcessLinkIds = Set.empty
    Annotations = []
    AnnotationsByNodeId = Map.empty
}

let tests =
    testList "EditorActions" [
        test "sorts by LayerOrderPosition, not map order or node name" {
            let testLayer =
                layer [ "z-node", 2; "a-node", 0; "m-node", 1 ] [ "output-z", 2; "output-a", 0; "output-m", 1 ]

            let inputGroup = group ProvenanceSide.Input [ "z-node"; "a-node"; "m-node" ]
            let outputGroup = group ProvenanceSide.Output [ "output-z"; "output-a"; "output-m" ]

            let result = EditorActions.orderedMemberPairs testLayer inputGroup outputGroup

            Expect.isSome result "equal-count groups should produce pairs"
            let pairs = result.Value
            Expect.equal pairs.Length 3 "should have 3 pairs"
            Expect.equal (pairs.[0]) ("a-node", "output-a") "position 0 pair"
            Expect.equal (pairs.[1]) ("m-node", "output-m") "position 1 pair"
            Expect.equal (pairs.[2]) ("z-node", "output-z") "position 2 pair"
        }

        test "returns None when input and output counts differ" {
            let testLayer = layer [ "a", 0; "b", 1 ] [ "x", 0 ]

            let inputGroup = group ProvenanceSide.Input [ "a"; "b" ]
            let outputGroup = group ProvenanceSide.Output [ "x" ]

            let result = EditorActions.orderedMemberPairs testLayer inputGroup outputGroup
            Expect.isNone result "mismatched counts should return None"
        }

        test "handles single-member groups" {
            let testLayer = layer [ "only-in", 0 ] [ "only-out", 0 ]
            let inputGroup = group ProvenanceSide.Input [ "only-in" ]
            let outputGroup = group ProvenanceSide.Output [ "only-out" ]

            let result = EditorActions.orderedMemberPairs testLayer inputGroup outputGroup

            Expect.isSome result "single-member groups should pair"
            Expect.equal result.Value [ "only-in", "only-out" ] "single pair"
        }

        test "position gaps do not affect pairing order" {
            let testLayer =
                layer [ "first", 10; "second", 50; "third", 100 ] [ "out-1", 5; "out-2", 25; "out-3", 75 ]

            let inputGroup = group ProvenanceSide.Input [ "third"; "first"; "second" ]
            let outputGroup = group ProvenanceSide.Output [ "out-3"; "out-1"; "out-2" ]

            let result = EditorActions.orderedMemberPairs testLayer inputGroup outputGroup
            Expect.isSome result "should pair despite gaps"

            let pairs = result.Value
            Expect.equal (pairs.[0]) ("first", "out-1") "lowest positions pair"
            Expect.equal (pairs.[1]) ("second", "out-2") "middle positions pair"
            Expect.equal (pairs.[2]) ("third", "out-3") "highest positions pair"
        }

        test "property value drag ids round-trip the exact assignment identity" {
            let term name = {
                Name = name
                TermSource = Some "urn:source"
                TermAccession = Some "accession|with-separator"
            }

            let source = {
                Key = {
                    Kind = AnnotationOwnerKind.Process
                    Header = term "header"
                }
                PropertyKind =
                    AssignmentPropertyKind.AdapterSpecific {
                        Id = "adapter:property"
                        Label = "Adapter property"
                    }
                Value =
                    ProvenanceValue.Reference {
                        Scheme = "doi"
                        Id = "10.1000/example"
                        Label = "A display label"
                    }
                Unit = Some(term "unit")
                ContainerReferenceValueId = Some "value-container"
                ReferenceSlotId = Some "slot-1"
                CopiedFromAssignmentId = Some "assignment-7"
            }

            let drag = {
                DefinitionId = Some "value-definition"
                DraftId = None
                Source = source
            }

            match DragDrop.tryDragId (DragDrop.valueDragId drag) with
            | Some(DragDrop.Payload.PropertyValue actual) ->
                Expect.equal actual drag "Every kind-bearing field survives the DOM id round-trip."
            | other -> failtestf "Expected a property value payload, got %A" other
        }

        test "catalog shelf and rail drags have distinct routes" {
            let folder =
                DragDrop.folderCatalogDragId ProvenanceSide.Output "scheme" "durable-id"

            let rail = DragDrop.catalogValueDragId ProvenanceSide.Output "scheme" "durable-id"

            Expect.equal
                (DragDrop.tryDragId folder)
                (Some(DragDrop.Payload.FolderCatalogValue(ProvenanceSide.Output, "scheme", "durable-id")))
                "Shelf resources route only to rail placement."

            Expect.equal
                (DragDrop.tryDragId rail)
                (Some(DragDrop.Payload.CatalogValue(ProvenanceSide.Output, "scheme", "durable-id")))
                "Rail resources route to catalog-aware assignment."
        }

        test "confirmed overwrite preserves assignment identity, kind, and lineage" {
            let header = {
                Name = "Temperature"
                TermSource = Some "TEST"
                TermAccession = Some "TEST:temperature"
            }

            let property: PropertyDefinition = { Id = "property"; Category = header }

            let originalValue: PropertyValueDefinition = {
                Id = "value-original"
                PropertyId = property.Id
                Value = ProvenanceValue.Text "20"
                Unit = None
            }

            let concreteKind =
                AssignmentPropertyKind.AdapterSpecific {
                    Id = "processcore:characteristic"
                    Label = "Characteristic"
                }

            let assignment: NodeAssignment = {
                Id = "assignment"
                ValueId = originalValue.Id
                PropertyKind = concreteKind
                TargetSource = Some { Id = "source"; Name = "Source" }
                Lineage = AssignmentLineage.Loaded
            }

            let node: CanonicalNode = {
                Id = "node"
                Key = {
                    KindId = sampleKind.Id
                    Name = "node"
                }
                Kind = sampleKind
                Name = "node"
                Assignments = Map.ofList [ assignment.Id, assignment ]
            }

            let session = {
                empty with
                    Nodes = Map.ofList [ node.Id, node ]
                    Properties = Map.ofList [ property.Id, property ]
                    Values = Map.ofList [ originalValue.Id, originalValue ]
            }

            let warning: ValueAssignmentWarning = {
                Target = NodeTargets(Set.singleton node.Id)
                ExistingAssignmentIds = Set.singleton assignment.Id
                Header = header
                Value = ProvenanceValue.Text "25"
                Unit = None
            }

            let mutable published = None

            EditorActions.applyAssignmentBatchWithSource session (fun result -> published <- Some result) None {
                Adds = []
                Overwrites = [ warning ]
            }

            let actual =
                match published with
                | Some(Ok actual) -> actual
                | other -> failtestf "Expected a published session, got %A" other

            let updated = actual.Nodes[node.Id].Assignments[assignment.Id]
            Expect.equal updated.PropertyKind concreteKind "The concrete property kind is not generalized."
            Expect.equal updated.Lineage AssignmentLineage.Loaded "Loaded lineage is retained."
            Expect.equal updated.TargetSource assignment.TargetSource "Source lineage is retained."
            Expect.equal actual.Values[updated.ValueId].Value (ProvenanceValue.Text "25") "The value is replaced."

            Expect.equal
                actual.AvailabilityTopologyRevision
                session.AvailabilityTopologyRevision
                "Identity did not change."

            Expect.equal
                actual.AnnotationValueRevision
                (session.AnnotationValueRevision + 1)
                "One value revision is recorded."

            Expect.isTrue
                (actual.MutationJournal
                 |> List.exists (
                     function
                     | NodeAssignmentValueChanged _
                     | PropertyValueDefinitionUpdated _ -> true
                     | _ -> false
                 ))
                "The journal records an exact value edit, not remove/add patch identities."

            Expect.isFalse
                (actual.MutationJournal
                 |> List.exists (
                     function
                     | NodeAssignmentAdded _
                     | NodeAssignmentRemoved _ -> true
                     | _ -> false
                 ))
                "Overwrite does not replace the canonical assignment occurrence."
        }

        test "reusing a loaded node value copies the exact assignment kind and lineage" {
            let header = {
                Name = "Species"
                TermSource = Some "NCBITaxon"
                TermAccession = Some "NCBITaxon:9606"
            }

            let property: PropertyDefinition = {
                Id = "property-species"
                Category = header
            }

            let value: PropertyValueDefinition = {
                Id = "value-human"
                PropertyId = property.Id
                Value = ProvenanceValue.Text "Homo sapiens"
                Unit = None
            }

            let concreteKind =
                AssignmentPropertyKind.AdapterSpecific {
                    Id = "processcore:characteristic"
                    Label = "Characteristic"
                }

            let sourceAssignment: NodeAssignment = {
                Id = "assignment-species"
                ValueId = value.Id
                PropertyKind = concreteKind
                TargetSource = Some { Id = "source"; Name = "Loaded table" }
                Lineage = AssignmentLineage.Loaded
            }

            let node nodeId assignments : CanonicalNode = {
                Id = nodeId
                Key = {
                    KindId = sampleKind.Id
                    Name = nodeId
                }
                Kind = sampleKind
                Name = nodeId
                Assignments = assignments
            }

            let session = {
                empty with
                    Nodes =
                        Map.ofList [
                            "source-node", node "source-node" (Map.ofList [ sourceAssignment.Id, sourceAssignment ])
                            "target-node", node "target-node" Map.empty
                        ]
                    Properties = Map.ofList [ property.Id, property ]
                    Values = Map.ofList [ value.Id, value ]
            }

            let source: ValueAssignmentSource = {
                Key = {
                    Kind = AnnotationOwnerKind.Node
                    Header = header
                }
                PropertyKind = concreteKind
                Value = value.Value
                Unit = value.Unit
                ContainerReferenceValueId = None
                ReferenceSlotId = None
                CopiedFromAssignmentId = Some sourceAssignment.Id
            }

            let request: ValueAssignmentRequest = {
                Target = NodeTargets(Set.singleton "target-node")
                OwnerKind = AnnotationOwnerKind.Node
                PropertyKind = concreteKind
                Category = header
                Value = value.Value
                Unit = value.Unit
            }

            let effect =
                match EditorActions.requestEffectWithSource (Some source) session request with
                | Ok effect -> effect
                | Error error -> failtestf "Expected an exact assignment copy, got %A" error

            let actual = Session.commit effect session

            let copied =
                actual.Nodes["target-node"].Assignments |> Map.toList |> List.exactlyOne |> snd

            Expect.equal copied.PropertyKind concreteKind "The adapter-specific assignment kind is retained."

            Expect.equal
                copied.Lineage
                (AssignmentLineage.DerivedFrom sourceAssignment.Id)
                "The new assignment records its exact source assignment."

            Expect.notEqual copied.Id sourceAssignment.Id "The target receives a new assignment occurrence."
            Expect.equal copied.ValueId sourceAssignment.ValueId "The canonical value definition is reused."
        }

        test "editing a single forward-propagated reference edits the owner and creates no assignment on the receiver" {
            let header = {
                Name = "Species"
                TermSource = Some "NCBITaxon"
                TermAccession = Some "NCBITaxon:9606"
            }

            let property: PropertyDefinition = {
                Id = "property-species"
                Category = header
            }

            let originalValue: PropertyValueDefinition = {
                Id = "value-species"
                PropertyId = property.Id
                Value = ProvenanceValue.Text "Arabidopsis"
                Unit = None
            }

            let concreteKind =
                AssignmentPropertyKind.AdapterSpecific {
                    Id = "processcore:characteristic"
                    Label = "Characteristic"
                }

            let assignment: NodeAssignment = {
                Id = "assignment-species"
                ValueId = originalValue.Id
                PropertyKind = concreteKind
                TargetSource = Some { Id = "source"; Name = "Source" }
                Lineage = AssignmentLineage.Loaded
            }

            let ownerNode: CanonicalNode = {
                Id = "owner-node"
                Key = {
                    KindId = sampleKind.Id
                    Name = "owner-node"
                }
                Kind = sampleKind
                Name = "owner-node"
                Assignments = Map.ofList [ assignment.Id, assignment ]
            }

            let receiverNode: CanonicalNode = {
                Id = "receiver-node"
                Key = {
                    KindId = sampleKind.Id
                    Name = "receiver-node"
                }
                Kind = sampleKind
                Name = "receiver-node"
                Assignments = Map.empty
            }

            let session = {
                empty with
                    Nodes = Map.ofList [ ownerNode.Id, ownerNode; receiverNode.Id, receiverNode ]
                    Properties = Map.ofList [ property.Id, property ]
                    Values = Map.ofList [ originalValue.Id, originalValue ]
            }

            let annotation: ProjectedAnnotation = {
                Key = NodeValue(header, TextIdentity "Arabidopsis", None)
                Backing =
                    NodeAssignmentBacking(
                        {
                            PropertyId = property.Id
                            ValueId = originalValue.Id
                            AssignmentId = assignment.Id
                            PropertyKind = concreteKind
                        },
                        ownerNode.Id,
                        assignment.TargetSource
                    )
                Availability = {
                    Relation = ForwardPropagated [ "link-1" ]
                    OriginatingLinkIds = Set.singleton "link-1"
                    VisibleThroughLinkIds = Set.singleton "link-1"
                }
                DerivedOriginSource = None
            }

            let content: Commands.NodeValueContent = {
                Category = header
                Value = ProvenanceValue.Text "Nicotiana"
                Unit = None
            }

            let result =
                EditorActions.editProjectedAnnotations receiverNode.Id Set.empty session [ annotation ] content

            let actual =
                match result with
                | Ok actual -> actual
                | Error error -> failtestf "Expected the propagated edit to succeed, got %A" error

            Expect.equal
                actual.Values[originalValue.Id].Value
                (ProvenanceValue.Text "Nicotiana")
                "The owner's value is edited in place."

            Expect.isTrue
                actual.Nodes[receiverNode.Id].Assignments.IsEmpty
                "The receiver gains no assignment of its own."

            Expect.isTrue
                (actual.Nodes[ownerNode.Id].Assignments.ContainsKey assignment.Id)
                "The owner keeps the same assignment identity."
        }

        // Intent §4's bulk rule: several origins that each resolve uniquely are
        // not ambiguous - the entity surface edits every owning assignment
        // behind the one displayed value as one atomic command.
        test "editing several uniquely resolvable propagated origins edits each origin" {
            let header = {
                Name = "Species"
                TermSource = Some "NCBITaxon"
                TermAccession = Some "NCBITaxon:9606"
            }

            let property: PropertyDefinition = {
                Id = "property-species"
                Category = header
            }

            let value: PropertyValueDefinition = {
                Id = "value-species"
                PropertyId = property.Id
                Value = ProvenanceValue.Text "Arabidopsis"
                Unit = None
            }

            let concreteKind =
                AssignmentPropertyKind.AdapterSpecific {
                    Id = "processcore:characteristic"
                    Label = "Characteristic"
                }

            let makeAssignment id : NodeAssignment = {
                Id = id
                ValueId = value.Id
                PropertyKind = concreteKind
                TargetSource = Some { Id = "source"; Name = "Source" }
                Lineage = AssignmentLineage.Loaded
            }

            let assignmentA = makeAssignment "assignment-a"
            let assignmentB = makeAssignment "assignment-b"

            let makeNode id (assignment: NodeAssignment) : CanonicalNode = {
                Id = id
                Key = { KindId = sampleKind.Id; Name = id }
                Kind = sampleKind
                Name = id
                Assignments = Map.ofList [ assignment.Id, assignment ]
            }

            let ownerA = makeNode "owner-a" assignmentA
            let ownerB = makeNode "owner-b" assignmentB

            let receiverNode: CanonicalNode = {
                Id = "receiver-node"
                Key = {
                    KindId = sampleKind.Id
                    Name = "receiver-node"
                }
                Kind = sampleKind
                Name = "receiver-node"
                Assignments = Map.empty
            }

            let session = {
                empty with
                    Nodes =
                        Map.ofList [
                            ownerA.Id, ownerA
                            ownerB.Id, ownerB
                            receiverNode.Id, receiverNode
                        ]
                    Properties = Map.ofList [ property.Id, property ]
                    Values = Map.ofList [ value.Id, value ]
            }

            let makeAnnotation (ownerId: CanonicalNodeId) (assignment: NodeAssignment) linkId : ProjectedAnnotation = {
                Key = NodeValue(header, TextIdentity "Arabidopsis", None)
                Backing =
                    NodeAssignmentBacking(
                        {
                            PropertyId = property.Id
                            ValueId = value.Id
                            AssignmentId = assignment.Id
                            PropertyKind = concreteKind
                        },
                        ownerId,
                        assignment.TargetSource
                    )
                Availability = {
                    Relation = ForwardPropagated [ linkId ]
                    OriginatingLinkIds = Set.singleton linkId
                    VisibleThroughLinkIds = Set.singleton linkId
                }
                DerivedOriginSource = None
            }

            let annotationA = makeAnnotation ownerA.Id assignmentA "link-a"
            let annotationB = makeAnnotation ownerB.Id assignmentB "link-b"

            let content: Commands.NodeValueContent = {
                Category = header
                Value = ProvenanceValue.Text "Nicotiana"
                Unit = None
            }

            let actual =
                EditorActions.editProjectedAnnotations
                    receiverNode.Id
                    Set.empty
                    session
                    [ annotationA; annotationB ]
                    content
                |> function
                    | Ok actual -> actual
                    | Error error -> failtestf "Expected the multi-origin bulk edit to resolve, got %A" error

            for ownerId, assignmentId in [ ownerA.Id, assignmentA.Id; ownerB.Id, assignmentB.Id ] do
                let owned = actual.Nodes[ownerId].Assignments[assignmentId]

                Expect.equal
                    actual.Values[owned.ValueId].Value
                    (ProvenanceValue.Text "Nicotiana")
                    $"The origin '{assignmentId}' carries the edit."

            Expect.isEmpty actual.Nodes[receiverNode.Id].Assignments "The receiving node gains no ownership."

            Expect.equal
                actual.AnnotationValueRevision
                (session.AnnotationValueRevision + 1)
                "One atomic command advances the value revision exactly once."
        }

        test "editing a reverse-connection-local reference is refused as read-only" {
            let header = {
                Name = "Outcome"
                TermSource = Some "TEST"
                TermAccession = Some "TEST:outcome"
            }

            let property: PropertyDefinition = {
                Id = "property-outcome"
                Category = header
            }

            let value: PropertyValueDefinition = {
                Id = "value-outcome"
                PropertyId = property.Id
                Value = ProvenanceValue.Text "Success"
                Unit = None
            }

            let concreteKind =
                AssignmentPropertyKind.AdapterSpecific {
                    Id = "processcore:parameter"
                    Label = "Parameter"
                }

            let assignment: NodeAssignment = {
                Id = "assignment-outcome"
                ValueId = value.Id
                PropertyKind = concreteKind
                TargetSource = Some { Id = "source"; Name = "Source" }
                Lineage = AssignmentLineage.Loaded
            }

            let ownerNode: CanonicalNode = {
                Id = "owner-node"
                Key = {
                    KindId = sampleKind.Id
                    Name = "owner-node"
                }
                Kind = sampleKind
                Name = "owner-node"
                Assignments = Map.ofList [ assignment.Id, assignment ]
            }

            let receiverNode: CanonicalNode = {
                Id = "receiver-node"
                Key = {
                    KindId = sampleKind.Id
                    Name = "receiver-node"
                }
                Kind = sampleKind
                Name = "receiver-node"
                Assignments = Map.empty
            }

            let session = {
                empty with
                    Nodes = Map.ofList [ ownerNode.Id, ownerNode; receiverNode.Id, receiverNode ]
                    Properties = Map.ofList [ property.Id, property ]
                    Values = Map.ofList [ value.Id, value ]
            }

            let annotation: ProjectedAnnotation = {
                Key = NodeValue(header, TextIdentity "Success", None)
                Backing =
                    NodeAssignmentBacking(
                        {
                            PropertyId = property.Id
                            ValueId = value.Id
                            AssignmentId = assignment.Id
                            PropertyKind = concreteKind
                        },
                        ownerNode.Id,
                        assignment.TargetSource
                    )
                Availability = {
                    Relation = ReverseConnectionLocal "link-1"
                    OriginatingLinkIds = Set.singleton "link-1"
                    VisibleThroughLinkIds = Set.singleton "link-1"
                }
                DerivedOriginSource = None
            }

            let content: Commands.NodeValueContent = {
                Category = header
                Value = ProvenanceValue.Text "Failure"
                Unit = None
            }

            let result =
                EditorActions.editProjectedAnnotations receiverNode.Id Set.empty session [ annotation ] content

            match result with
            | Error(ReadOnlyReverseLocalEdit _) -> ()
            | other -> failtestf "Expected a reverse-local read-only refusal, got %A" other
        }

        // A process annotation is reachable from both of its link's endpoints and
        // from the edge itself. Removing it works from any of them, so editing it
        // must too whenever the target resolves to one link - the user reported an
        // endpoint with a single incident link refusing the edit as "pooled".
        test "editing a single-link process annotation through its endpoint card resolves to that link" {
            let session = StoryFixtures.createSampleSession ()

            let projection =
                Projection.projectLayer session.ActiveLayerId Map.empty session
                |> function
                    | Ok projection -> projection
                    | Error error -> failtestf "Expected the fixture to project, got %A" error

            // node-output-c is an endpoint of exactly one link (link-d), and the
            // Analysis assignment "assignment-analysis-lcms" covers exactly that
            // link - the reported one-node/one-link/one-annotation shape.
            let card =
                projection.Groups
                |> List.find (fun group ->
                    group.Side = ProvenanceSide.Output
                    && group.CanonicalNodeIds = Set.singleton "node-output-c"
                )

            Expect.equal card.ProcessLinkIds (Set.singleton "link-d") "The endpoint has exactly one incident link."

            let analysis =
                card.Annotations
                |> List.filter (fun annotation ->
                    match annotation.Backing with
                    | ProcessAssignmentBacking(identity, _, _, _, _) ->
                        identity.AssignmentId = "assignment-analysis-lcms"
                    | NodeAssignmentBacking _ -> false
                )

            Expect.hasLength analysis 1 "The single covered link yields a single projected annotation."

            let content: Commands.NodeValueContent = {
                Category = {
                    Name = "Analysis"
                    TermSource = None
                    TermAccession = None
                }
                Value = ProvenanceValue.Text "GC-MS"
                Unit = None
            }

            let actual =
                EditorActions.editProjectedAnnotations "node-output-c" card.ProcessLinkIds session analysis content
                |> function
                    | Ok actual -> actual
                    | Error error -> failtestf "Expected the single-link edit to succeed, got %A" error

            let edited =
                actual.Processes
                |> Map.toList
                |> List.collect (fun (_, structuralProcess) ->
                    structuralProcess.Assignments
                    |> Map.toList
                    |> List.filter (fun (_, assignment) -> assignment.CoveredLinkIds = Set.singleton "link-d")
                    |> List.choose (fun (_, assignment) -> actual.Values |> Map.tryFind assignment.ValueId)
                )
                |> List.map _.Value

            Expect.contains edited (ProvenanceValue.Text "GC-MS") "The covered link carries the edited value."
        }

        // The reported symptom: an entity showing exactly one annotation refused
        // the edit as "pooled". Its originating link was upstream of the card, so
        // narrowing to the card's own links left nothing - and zero resolved links
        // was reported as ambiguity.
        test "editing a propagated process annotation resolves to its originating link" {
            let session = StoryFixtures.createChainedSession ()

            let projection =
                Projection.projectLayer "layer-2" Map.empty session
                |> function
                    | Ok projection -> projection
                    | Error error -> failtestf "Expected the fixture to project, got %A" error

            let card =
                projection.Groups
                |> List.find (fun group -> group.CanonicalNodeIds = Set.singleton "node-extract")

            let propagated =
                card.Annotations
                |> List.filter (fun annotation ->
                    match annotation.Backing with
                    | ProcessAssignmentBacking(identity, _, _, _, _) ->
                        identity.AssignmentId = "assignment-growth-temperature"
                    | NodeAssignmentBacking _ -> false
                )

            Expect.hasLength propagated 1 "The entity shows the annotation exactly once."

            Expect.isFalse
                (card.ProcessLinkIds
                 |> Set.intersect propagated.Head.Availability.OriginatingLinkIds
                 |> Set.isEmpty
                 |> not)
                "Its originating link is not one of the card's own links, which is what made this fail."

            let content: Commands.NodeValueContent = {
                Category = {
                    Name = "Growth Temperature"
                    TermSource = None
                    TermAccession = None
                }
                Value = ProvenanceValue.Text "31"
                Unit = None
            }

            let actual =
                EditorActions.editProjectedAnnotations "node-extract" card.ProcessLinkIds session propagated content
                |> function
                    | Ok actual -> actual
                    | Error error -> failtestf "Expected the propagated process edit to resolve, got %A" error

            let edited =
                actual.Processes
                |> Map.toList
                |> List.collect (fun (_, structuralProcess) ->
                    structuralProcess.Assignments |> Map.toList |> List.map snd
                )
                |> List.choose (fun assignment -> actual.Values |> Map.tryFind assignment.ValueId)
                |> List.map _.Value

            Expect.contains edited (ProvenanceValue.Text "31") "The originating assignment carries the edited value."
        }

        // Zero resolvable links is an empty target, not a pooled one. Reporting it
        // as ambiguity told the user several links covered an annotation that in
        // fact resolved to none.
        test "an edit that resolves to no backing link reports an empty target" {
            let reference = {
                AssignmentId = "assignment-process"
                ValueId = "value-process"
                Owner = ProcessOwner "process-one"
                Relation = IncidentProcess "link-one"
                OriginatingLinkIds = Set.empty
                VisibleThroughLinkIds = Set.empty
            }

            let content: Commands.NodeValueContent = {
                Category = {
                    Name = "Any"
                    TermSource = None
                    TermAccession = None
                }
                Value = ProvenanceValue.Text "x"
                Unit = None
            }

            // The relation names a link the reference no longer carries, so no
            // (process, assignment, link) triple survives resolution.
            let reference = {
                reference with
                    Relation = ForwardPropagated [ "link-elsewhere" ]
            }

            match Commands.editAvailableReferences "node-x" [ reference ] content empty with
            | Error EmptyTarget -> ()
            | other -> failtestf "Expected an empty-target refusal, got %A" other
        }

        // The reported case: dropping one process value on an entity whose
        // incident links belong to several structural processes creates one
        // assignment per process (intent §3). They share a grouping value, so the
        // entity shows them as one entry - and editing that entry must set that
        // one displayed value, exactly as removing it clears every represented
        // link. Editing at a single edge already worked, which is what made the
        // refusal at the entity look arbitrary.
        test "editing one displayed value backed by several assignments edits all of them" {
            let header = {
                Name = "Biosource amount"
                TermSource = None
                TermAccession = None
            }

            let property: PropertyDefinition = {
                Id = "property-amount"
                Category = header
            }

            let source = { Id = "source-one"; Name = "One" }

            let nodes =
                [
                    "node-q", "QA_Sample_A"
                    "node-out-a", "DB26"
                    "node-out-b", "DB40"
                ]
                |> List.map (fun (nodeId, name) -> {
                    Id = nodeId
                    Key = { KindId = sampleKind.Id; Name = name }
                    Kind = sampleKind
                    Name = name
                    Assignments = Map.empty
                })
                |> List.map (fun node -> node.Id, node)
                |> Map.ofList

            let processLink id shape : ProcessLink = { Id = id; Shape = shape }

            let structuralProcess id linkId shape : StructuralProcess = {
                Id = id
                OriginLayerId = "layer-1"
                Name = Some id
                Links = Map.ofList [ linkId, processLink linkId shape ]
                Assignments = Map.empty
            }

            // Two edges out of one entity, each its own structural process - the
            // shape ProcessCore produces, where every Process is one directed edge.
            let processes =
                [
                    structuralProcess "process-one" "link-one" (ProcessLinkShape.Between("node-q", "node-out-a"))
                    structuralProcess "process-two" "link-two" (ProcessLinkShape.Between("node-q", "node-out-b"))
                ]
                |> List.map (fun structuralProcess -> structuralProcess.Id, structuralProcess)
                |> Map.ofList

            let appearance side nodeId position : LayerEndpoint = {
                Key = {
                    LayerId = "layer-1"
                    Side = side
                    NodeId = nodeId
                }
                Header = { Kind = sampleKind; Text = nodeId }
                LayerOrderPosition = position
            }

            let testLayer: ProvenanceLayer = {
                Id = "layer-1"
                Label = "layer-1"
                Source = source
                InputEndpoints = Map.ofList [ "node-q", appearance ProvenanceSide.Input "node-q" 0 ]
                OutputEndpoints =
                    Map.ofList [
                        "node-out-a", appearance ProvenanceSide.Output "node-out-a" 0
                        "node-out-b", appearance ProvenanceSide.Output "node-out-b" 1
                    ]
                StructuralProcessIds = Set.ofList [ "process-one"; "process-two" ]
            }

            let session = {
                empty with
                    Nodes = nodes
                    Processes = processes
                    Properties = Map.ofList [ property.Id, property ]
                    Layers = Map.ofList [ testLayer.Id, testLayer ]
                    LayerOrder = [ testLayer.Id ]
                    ActiveLayerId = testLayer.Id
            }

            let draft: Commands.ProcessAssignmentDraft = {
                Content = {
                    Category = header
                    Value = ProvenanceValue.Text "5.2 milligram"
                    Unit = None
                }
                OwnerKind = AnnotationOwnerKind.Process
                PropertyKind = AssignmentPropertyKind.Generic
                ContainerReferenceValueId = None
                ReferenceSlotId = None
                Lineage = AssignmentLineage.Created
            }

            let assigned =
                Commands.assignProcessValue (Set.ofList [ "link-one"; "link-two" ]) draft session
                |> function
                    | Ok effect -> Session.commit effect session
                    | Error error -> failtestf "Expected the drop to assign, got %A" error

            let assignmentCount =
                assigned.Processes
                |> Map.toList
                |> List.sumBy (fun (_, structuralProcess) -> structuralProcess.Assignments.Count)

            Expect.equal assignmentCount 2 "One drop over two structural processes creates one assignment each."

            let projection =
                Projection.projectLayer "layer-1" Map.empty assigned
                |> function
                    | Ok projection -> projection
                    | Error error -> failtestf "Expected the session to project, got %A" error

            let card =
                projection.Groups
                |> List.find (fun group -> group.CanonicalNodeIds = Set.singleton "node-q")

            let displayed =
                Projection.groupProjectedAnnotations card.Annotations |> List.exactlyOne

            Expect.hasLength
                displayed.Annotations
                2
                "The entity shows one value backed by both assignments, which is what made this look like one annotation."

            let content: Commands.NodeValueContent = {
                Category = header
                Value = ProvenanceValue.Text "7.5 milligram"
                Unit = None
            }

            let actual =
                EditorActions.editProjectedAnnotations
                    "node-q"
                    card.ProcessLinkIds
                    assigned
                    displayed.Annotations
                    content
                |> function
                    | Ok actual -> actual
                    | Error error -> failtestf "Expected the entity-scoped edit to resolve, got %A" error

            let values =
                actual.Processes
                |> Map.toList
                |> List.collect (fun (_, structuralProcess) ->
                    structuralProcess.Assignments
                    |> Map.toList
                    |> List.choose (fun (_, assignment) -> actual.Values |> Map.tryFind assignment.ValueId)
                )
                |> List.map _.Value

            Expect.equal
                (values |> List.distinct)
                [ ProvenanceValue.Text "7.5 milligram" ]
                "Every assignment behind the one displayed value carries the edit."
        }

        // Intent §4's bulk rule for node values: a node grouping value backed by
        // several owning assignments receives the same bulk meaning at an entity
        // surface - one edit per owning assignment, one atomic command, one
        // revision advance per kind.
        test "an entity-scoped node edit spans every owning assignment behind the displayed value" {
            let header = {
                Name = "Species"
                TermSource = None
                TermAccession = None
            }

            let property: PropertyDefinition = {
                Id = "property-species"
                Category = header
            }

            let definition: PropertyValueDefinition = {
                Id = "value-species"
                PropertyId = property.Id
                Value = ProvenanceValue.Text "E. coli"
                Unit = None
            }

            let node nodeId : CanonicalNode = {
                Id = nodeId
                Key = {
                    KindId = sampleKind.Id
                    Name = nodeId
                }
                Kind = sampleKind
                Name = nodeId
                Assignments =
                    Map.ofList [
                        $"assignment-{nodeId}",
                        {
                            Id = $"assignment-{nodeId}"
                            ValueId = definition.Id
                            PropertyKind = AssignmentPropertyKind.Generic
                            TargetSource = None
                            Lineage = AssignmentLineage.Created
                        }
                    ]
            }

            let session = {
                empty with
                    Nodes = Map.ofList [ "node-a", node "node-a"; "node-b", node "node-b" ]
                    Properties = Map.ofList [ property.Id, property ]
                    Values = Map.ofList [ definition.Id, definition ]
            }

            let reference nodeId = {
                AssignmentId = $"assignment-{nodeId}"
                ValueId = definition.Id
                Owner = NodeOwner nodeId
                Relation = OwnedNode
                OriginatingLinkIds = Set.empty
                VisibleThroughLinkIds = Set.empty
            }

            let content: Commands.NodeValueContent = {
                Category = header
                Value = ProvenanceValue.Text "B. subtilis"
                Unit = None
            }

            let actual =
                Commands.editAvailableReferences "node-a" [ reference "node-a"; reference "node-b" ] content session
                |> function
                    | Ok effect -> Session.commit effect session
                    | Error error -> failtestf "Expected the node bulk edit to resolve, got %A" error

            let values =
                actual.Nodes
                |> Map.toList
                |> List.collect (fun (_, node) ->
                    node.Assignments
                    |> Map.toList
                    |> List.choose (fun (_, assignment) -> actual.Values |> Map.tryFind assignment.ValueId)
                )
                |> List.map _.Value

            Expect.equal
                (values |> List.distinct)
                [ ProvenanceValue.Text "B. subtilis" ]
                "Every owning assignment behind the displayed value carries the edit."

            Expect.equal
                actual.AnnotationValueRevision
                (session.AnnotationValueRevision + 1)
                "One atomic command advances the value revision exactly once."

            Expect.equal
                actual.AvailabilityTopologyRevision
                session.AvailabilityTopologyRevision
                "A value edit does not advance the topology revision."
        }

        // The resolvability gate's boundary: a mixed displayed entry - an
        // editable Parameter merged with a read-only Recipe Component - is
        // refused whole. Partially applying the edit to the Parameter alone
        // would silently split the displayed value it claimed to cover.
        test "a bulk edit over a mixed Parameter and Component entry is refused whole" {
            let header = {
                Name = "Device setting"
                TermSource = None
                TermAccession = None
            }

            let property: PropertyDefinition = {
                Id = "property-setting"
                Category = header
            }

            let definition: PropertyValueDefinition = {
                Id = "value-setting"
                PropertyId = property.Id
                Value = ProvenanceValue.Text "37"
                Unit = None
            }

            let processAssignment id containerReferenceValueId : ProcessAssignment = {
                Id = id
                ValueId = definition.Id
                PropertyKind = AssignmentPropertyKind.Generic
                CoveredLinkIds = Set.singleton "link-one"
                ContainerReferenceValueId = containerReferenceValueId
                ReferenceSlotId = None
                Lineage = AssignmentLineage.Created
            }

            let structuralProcess: StructuralProcess = {
                Id = "process-one"
                OriginLayerId = "layer-1"
                Name = Some "process-one"
                Links =
                    Map.ofList [
                        "link-one",
                        {
                            Id = "link-one"
                            Shape = ProcessLinkShape.Between("node-q", "node-out")
                        }
                    ]
                Assignments =
                    Map.ofList [
                        "assignment-parameter", processAssignment "assignment-parameter" None
                        "assignment-component", processAssignment "assignment-component" (Some "value-container")
                    ]
            }

            let node nodeId : CanonicalNode = {
                Id = nodeId
                Key = {
                    KindId = sampleKind.Id
                    Name = nodeId
                }
                Kind = sampleKind
                Name = nodeId
                Assignments = Map.empty
            }

            let session = {
                empty with
                    Nodes = Map.ofList [ "node-q", node "node-q"; "node-out", node "node-out" ]
                    Processes = Map.ofList [ structuralProcess.Id, structuralProcess ]
                    Properties = Map.ofList [ property.Id, property ]
                    Values = Map.ofList [ definition.Id, definition ]
            }

            let reference assignmentId = {
                AssignmentId = assignmentId
                ValueId = definition.Id
                Owner = ProcessOwner "process-one"
                Relation = IncidentProcess "link-one"
                OriginatingLinkIds = Set.singleton "link-one"
                VisibleThroughLinkIds = Set.singleton "link-one"
            }

            let content: Commands.NodeValueContent = {
                Category = header
                Value = ProvenanceValue.Text "42"
                Unit = None
            }

            match
                Commands.editAvailableReferences
                    "node-q"
                    [
                        reference "assignment-parameter"
                        reference "assignment-component"
                    ]
                    content
                    session
            with
            | Error ReadOnlyAdapterResourceMutation -> ()
            | other -> failtestf "Expected the mixed entry to refuse whole, got %A" other
        }

        // Intent §7 merges `Characteristic: X` and `Factor: X` into one grouping
        // value, so a merged chip can span concrete kinds. Its drag payload may
        // only claim a kind every backing shares; on disagreement it degrades to
        // a Generic draft with no copy source. The accepted tradeoff of
        // `CopiedFromAssignmentId = None`: the drop forfeits the identified
        // overwrite-narrowing, so dropping onto a target holding several
        // same-header assignments refuses (`MultiplePropertyValues`) instead of
        // narrowing - a refusal, never a misdirected overwrite.
        test "a kind-divergent merged chip degrades its payload to a Generic draft" {
            let header = {
                Name = "Temperature"
                TermSource = None
                TermAccession = None
            }

            let headerKey: GroupingKey = {
                Kind = AnnotationOwnerKind.Node
                Header = header
            }

            let definition: PropertyValueDefinition = {
                Id = "value-temperature"
                PropertyId = "property-temperature"
                Value = ProvenanceValue.Text "37"
                Unit = None
            }

            let characteristic =
                AssignmentPropertyKind.AdapterSpecific {
                    Id = "arc-isa:characteristic"
                    Label = "Characteristic"
                }

            let factor =
                AssignmentPropertyKind.AdapterSpecific {
                    Id = "arc-isa:factor"
                    Label = "Factor"
                }

            let annotation assignmentId kind : ProjectedAnnotation = {
                Key = NodeValue(header, TextIdentity "37", None)
                Backing =
                    NodeAssignmentBacking(
                        {
                            PropertyId = "property-temperature"
                            ValueId = definition.Id
                            AssignmentId = assignmentId
                            PropertyKind = kind
                        },
                        "node-a",
                        None
                    )
                Availability = {
                    Relation = OwnedNode
                    OriginatingLinkIds = Set.empty
                    VisibleThroughLinkIds = Set.empty
                }
                DerivedOriginSource = None
            }

            let divergent =
                PropertyRails.RailValue.AssignedValue(
                    definition,
                    [
                        annotation "assignment-characteristic" characteristic
                        annotation "assignment-factor" factor
                    ]
                )
                |> PropertyRails.RailValue.tryDragPayload headerKey
                |> Option.defaultWith (fun () -> failtest "Expected the merged chip to produce a payload.")

            Expect.equal
                divergent.Source.PropertyKind
                AssignmentPropertyKind.Generic
                "Disagreeing backings may not materialize the head's kind."

            Expect.isNone
                divergent.Source.CopiedFromAssignmentId
                "Disagreeing backings may not copy the head's lineage either - the drop must take the draft path."

            let agreeing =
                PropertyRails.RailValue.AssignedValue(
                    definition,
                    [
                        annotation "assignment-one" characteristic
                        annotation "assignment-two" characteristic
                    ]
                )
                |> PropertyRails.RailValue.tryDragPayload headerKey
                |> Option.defaultWith (fun () -> failtest "Expected the agreeing chip to produce a payload.")

            Expect.equal divergent.DefinitionId (Some definition.Id) "The payload still names its definition."
            Expect.equal agreeing.Source.PropertyKind characteristic "Agreeing backings keep their shared kind."

            Expect.equal
                agreeing.Source.CopiedFromAssignmentId
                (Some "assignment-one")
                "Agreeing backings keep the head's copy source."
        }
    ]

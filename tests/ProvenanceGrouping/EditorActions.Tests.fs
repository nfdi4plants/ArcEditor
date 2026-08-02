module EditorActionsTests

open Expecto
open Swate.Components.Page.ProvenanceGrouping
open Swate.Components.Page.ProvenanceGrouping.Identifiers
open Swate.Components.Page.ProvenanceGrouping.Values
open Swate.Components.Page.ProvenanceGrouping.Domain
open Swate.Components.Page.ProvenanceGrouping.ProjectionTypes
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
    ]

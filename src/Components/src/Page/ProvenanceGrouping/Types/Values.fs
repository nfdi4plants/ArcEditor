module Swate.Components.Page.ProvenanceGrouping.Values

open Swate.Components.Page.ProvenanceGrouping.Identifiers

type ReferenceValue = {
    Scheme: string
    Id: string
    Label: string
}

[<RequireQualifiedAccess>]
type ProvenanceValue =
    | Text of string
    | Integer of int
    | Float of float
    | Term of ProvenanceTerm
    | Reference of ReferenceValue

[<RequireQualifiedAccess>]
type AnnotationOwnerKind =
    | Node
    | Process

type ReferenceSlotId = string

type ReferenceCardinality =
    | Many
    | AtMostOnePerLink of ReferenceSlotId

type AssignmentPropertyKind =
    | Generic
    | AdapterSpecific of ProvenanceKind

type AssignmentLineage =
    | Loaded
    | DerivedFrom of AnnotationAssignmentId
    | DerivedFromCatalog of referenceScheme: string * referenceId: string * dependentKey: string
    | Created

type CanonicalNodeKey = { KindId: string; Name: string }

type ProcessLinkShape =
    | Between of input: CanonicalNodeId * output: CanonicalNodeId
    | InputOnly of input: CanonicalNodeId
    | OutputOnly of output: CanonicalNodeId
    | Endpointless

type PropertyDefinition = {
    Id: PropertyDefinitionId
    Category: ProvenanceTerm
}

type PropertyValueDefinition = {
    Id: PropertyValueDefinitionId
    PropertyId: PropertyDefinitionId
    Value: ProvenanceValue
    Unit: ProvenanceTerm option
}

type ProvenanceColor = string

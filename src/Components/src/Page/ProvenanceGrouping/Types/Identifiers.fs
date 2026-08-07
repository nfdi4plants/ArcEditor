module Swate.Components.Page.ProvenanceGrouping.Identifiers

type CanonicalNodeId = string
type StructuralProcessId = string
type ProcessLinkId = string
type PropertyDefinitionId = string
type PropertyValueDefinitionId = string
type AnnotationAssignmentId = string
type ProvenanceLayerId = string
type ProvenanceSourceId = string
type ProvenanceSourceName = string
type ProvenanceProcessName = string
type ProvenanceProcessId = string

type ProvenanceSourceRef = {
    Id: ProvenanceSourceId
    Name: ProvenanceSourceName
}

[<RequireQualifiedAccess>]
type ProvenanceSide =
    | Input
    | Output

type ProvenanceKind = { Id: string; Label: string }

type ProvenanceTerm = {
    Name: string
    TermSource: string option
    TermAccession: string option
}

type ProvenanceIOHeader = { Kind: ProvenanceKind; Text: string }

module Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreAdapterTypes

open ProcessCore
open Swate.Components.ProcessCore.Copy
open Swate.Components.Page.ProvenanceGrouping.Domain
open Swate.Components.Page.ProvenanceGrouping.Identifiers
open Swate.Components.Page.ProvenanceGrouping.ProjectionTypes

[<RequireQualifiedAccess>]
type ProcessCoreNodeKind =
    | Sample
    | Data

type ProcessCoreProcessLocation = {
    DatasetPath: string list
    ProcessIndex: int
    ExpectedName: string
}

type ProcessCoreNodeLocation = {
    Kind: ProcessCoreNodeKind
    Key: string
}

type ProcessCoreProcessGroupLocation = {
    DatasetPath: string list
    ProcessGroupName: string
}

type ProcessCoreCanonicalNodeSourceLocation = {
    ProcessGroup: ProcessCoreProcessGroupLocation
    Process: ProcessCoreProcessLocation
    Side: ProvenanceSide
    Node: ProcessCoreNodeLocation
    /// Stable source ordering captured at load. Display-layer ordering never
    /// rewrites this adapter provenance.
    SourceOrderHint: int
}

type ProcessCoreCanonicalLinkLocation = {
    Process: ProcessCoreProcessLocation
    Input: ProcessCoreCanonicalNodeSourceLocation option
    Output: ProcessCoreCanonicalNodeSourceLocation option
}

[<RequireQualifiedAccess>]
type ProcessCoreCanonicalAnnotationOwner =
    | NodeAdditionalProperty of ProcessCoreNodeLocation
    | ProcessParameterValue of ProcessCoreProcessLocation
    | RecipeComponent of scheme: string * resourceId: string

type ProcessCoreCanonicalAnnotationFingerprint = { Payload: string }

type ProcessCoreCanonicalAnnotationLocation = {
    Owner: ProcessCoreCanonicalAnnotationOwner
    Position: int
    /// Complete ArcEditor-owned serialization fingerprint, not
    /// ProcessCore.Annotation.Equals.
    Fingerprint: ProcessCoreCanonicalAnnotationFingerprint
}

type ProcessCoreRecipeComponentLocation = {
    ComponentKey: string
    Position: int
    Fingerprint: ProcessCoreCanonicalAnnotationFingerprint
}

type ProcessCoreRecipePayloadFingerprint = { Payload: string }

type ProcessCoreRecipeResourceLocation = {
    Scheme: string
    ResourceKey: RecipeResourceKey
    Resource: Recipe
    LoadFingerprint: ProcessCoreRecipePayloadFingerprint
    Components: ProcessCoreRecipeComponentLocation list
    ReferencingProcesses: ProcessCoreProcessLocation list
}

type ProcessCoreGenericPropertyMapping = { AdditionalType: string }

type ProcessCoreGenericPropertyMappings = {
    Node: ProcessCoreGenericPropertyMapping
    Process: ProcessCoreGenericPropertyMapping
}

[<RequireQualifiedAccess>]
module ProcessCoreGenericPropertyMappings =

    let defaults = {
        Node = { AdditionalType = "NodePropertyValue" }
        Process = {
            AdditionalType = "ProcessPropertyValue"
        }
    }

/// Construction input retains source ownership as a list so duplicate source
/// IDs cannot be silently collapsed by Map.ofList before validation.
type ProcessCoreCanonicalIndexSeed = {
    LoadedProcessGroups: ProcessCoreProcessGroupLocation list
    SourceLocations: (ProvenanceSourceId * ProcessCoreProcessGroupLocation) list
    NodeLocations: Map<CanonicalNodeId, ProcessCoreCanonicalNodeSourceLocation list>
    ProcessLocations: Map<StructuralProcessId, ProcessCoreProcessLocation>
    LinkLocations: Map<ProcessLinkId, ProcessCoreCanonicalLinkLocation>
    AssignmentLocations: Map<AnnotationAssignmentId, ProcessCoreCanonicalAnnotationLocation list>
    /// Exact converter-owned value identity for every loaded assignment.
    /// The source-location index cannot otherwise recover this after cleanup.
    AssignmentValueIds: Map<AnnotationAssignmentId, PropertyValueDefinitionId>
    ReferencingProcessesByRecipe: Map<RecipeResourceKey, ProcessCoreProcessLocation list>
    GenericPropertyMappings: ProcessCoreGenericPropertyMappings
}

type ProcessCoreCanonicalIndex = {
    LoadedProcessGroups: ProcessCoreProcessGroupLocation list
    SourceLocations: Map<ProvenanceSourceId, ProcessCoreProcessGroupLocation>
    ArcFingerprint: string
    NodeLocations: Map<CanonicalNodeId, ProcessCoreCanonicalNodeSourceLocation list>
    ProcessLocations: Map<StructuralProcessId, ProcessCoreProcessLocation>
    LinkLocations: Map<ProcessLinkId, ProcessCoreCanonicalLinkLocation>
    AssignmentLocations: Map<AnnotationAssignmentId, ProcessCoreCanonicalAnnotationLocation list>
    AssignmentValueIds: Map<AnnotationAssignmentId, PropertyValueDefinitionId>
    RecipeResources: Map<string * string, ProcessCoreRecipeResourceLocation>
    GenericPropertyMappings: ProcessCoreGenericPropertyMappings
}

[<RequireQualifiedAccess>]
type ProcessCoreCanonicalWarning =
    | BlankEndpoint of ProcessCoreProcessLocation * ProvenanceSide * int
    | BlankAnnotationName of ProcessCoreCanonicalAnnotationOwner * int
    | PropertyWithoutEndpoint of ProcessCoreProcessLocation * string

[<RequireQualifiedAccess>]
type ProcessCoreCanonicalConversionError =
    | EmptyDatasetPath
    | DatasetNotFound of string list
    | AmbiguousDatasetPath of string list
    | ProcessGroupNotFound of ProcessCoreProcessGroupLocation
    | AmbiguousRecipeResourceKey of RecipeResourceKey
    | RecipeResourceNotFound of RecipeResourceKey
    | DuplicateSourceOwnership of ProvenanceSourceId
    | ProcessGroupWithoutSource of ProcessCoreProcessGroupLocation
    | ProcessGroupOwnedByMultipleSources of ProcessCoreProcessGroupLocation * ProvenanceSourceId list
    | SourceOwnsUnselectedProcessGroup of ProvenanceSourceId * ProcessCoreProcessGroupLocation

type ProcessCoreCanonicalConversionResult = {
    Session: ProvenanceSession
    Index: ProcessCoreCanonicalIndex
    ReferenceCatalog: ReferenceCatalog
    Warnings: ProcessCoreCanonicalWarning list
    Locations: ProcessCoreProcessGroupLocation list
}

[<RequireQualifiedAccess>]
type ProcessCoreCanonicalWritebackError =
    | StaleArc
    | InitialLayerNotFound of ProvenanceSourceId
    | InvalidLayerOrder of ProvenanceLayerId list
    | LayerNotFound of ProvenanceLayerId
    | NodeNotFound of CanonicalNodeId
    | ProcessNotFound of StructuralProcessId
    | LinkNotFound of ProcessLinkId
    | AssignmentNotFound of AnnotationAssignmentId
    | ValueNotFound of PropertyValueDefinitionId
    | SourceLocationNotFound of string
    | AmbiguousSourceLocation of string
    | BlankLayerName of ProvenanceLayerId
    | DuplicateLayerName of string
    | ConflictingNodeIdentity of nodeKey: string * nodeIds: CanonicalNodeId list
    | ConflictingAnnotationIdentity of owner: string * existing: string * requested: string
    | GeneratedIdFormatChanged of id: string * expectedPrefix: string
    | UnsupportedEndpointKind of string
    | UnsupportedPropertyKind of string
    | InvalidProcessLink of ProcessLinkId
    | InconsistentCanonicalState of string
    | RecipeResourceNotFound of scheme: string * resourceKey: RecipeResourceKey
    | AmbiguousRecipeResourceKey of scheme: string * resourceKey: RecipeResourceKey
    | StaleRecipeResource of scheme: string * resourceId: string
    | ReadOnlyRecipeResourceMutation
    | ReadOnlyRecipeComponentMutation of AnnotationAssignmentId option
    | UnrepresentableExactLink of ProcessLinkId
    | InvalidPreparedState of string

type ProcessCoreWritebackSummary = {
    UpdatedAnnotations: int
    AddedAnnotations: int
    AddedNodes: int
    AddedProcesses: int
    RemovedProcesses: int
}

module ProcessCoreCanonicalKinds =

    [<Literal>]
    let processCoreRecipeScheme = "processcore:recipe"

    [<Literal>]
    let processCoreExecutesRecipeSlot = "processcore:executes-recipe"

    let private kind id label : ProvenanceKind = { Id = id; Label = label }

    let sampleEndpoint = kind "process-core:endpoint:sample" "Sample"
    let dataEndpoint = kind "process-core:endpoint:data" "Data"
    let characteristic = kind "process-core:property:characteristic" "Characteristic"
    let factor = kind "process-core:property:factor" "Factor"
    let parameter = kind "process-core:property:parameter" "Parameter"
    let componentKind = kind "process-core:property:component" "Component"

    let additionalProperty =
        kind "process-core:property:additional" "Additional property"

    let processCoreRecipeKind = kind processCoreRecipeScheme "Recipe"

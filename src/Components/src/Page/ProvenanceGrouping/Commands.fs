module Swate.Components.Page.ProvenanceGrouping.Commands

open Swate.Components.Page.ProvenanceGrouping.MutationTypes
open Swate.Components.Page.ProvenanceGrouping.ProjectionTypes
open Swate.Components.Page.ProvenanceGrouping.Identifiers
open Swate.Components.Page.ProvenanceGrouping.Values
open Swate.Components.Page.ProvenanceGrouping.Domain

[<RequireQualifiedAccess>]
type internal CommandChangeClassification =
    | Topology
    | Value
    | Both

type private CanonicalContent = {
    Nodes: Map<CanonicalNodeId, CanonicalNode>
    Processes: Map<StructuralProcessId, StructuralProcess>
    Properties: Map<PropertyDefinitionId, PropertyDefinition>
    Values: Map<PropertyValueDefinitionId, PropertyValueDefinition>
    Layers: Map<ProvenanceLayerId, ProvenanceLayer>
    LayerOrder: ProvenanceLayerId list
    ActiveLayerId: ProvenanceLayerId
}

type CommandEffect =
    private
    | NoChange
    | Changed of CommandChangeClassification * CanonicalContent * ProvenanceMutation list

[<RequireQualifiedAccess>]
type internal CommandEffectView =
    | NoChange
    | Changed of CommandChangeClassification * CanonicalContentView * ProvenanceMutation list

and internal CanonicalContentView = {
    Nodes: Map<CanonicalNodeId, CanonicalNode>
    Processes: Map<StructuralProcessId, StructuralProcess>
    Properties: Map<PropertyDefinitionId, PropertyDefinition>
    Values: Map<PropertyValueDefinitionId, PropertyValueDefinition>
    Layers: Map<ProvenanceLayerId, ProvenanceLayer>
    LayerOrder: ProvenanceLayerId list
    ActiveLayerId: ProvenanceLayerId
}

let private contentOf (session: ProvenanceSession) : CanonicalContent = {
    Nodes = session.Nodes
    Processes = session.Processes
    Properties = session.Properties
    Values = session.Values
    Layers = session.Layers
    LayerOrder = session.LayerOrder
    ActiveLayerId = session.ActiveLayerId
}

let private noChange = NoChange

let private topology resultingSession mutations =
    Changed(CommandChangeClassification.Topology, contentOf resultingSession, mutations)

let private value resultingSession mutations =
    Changed(CommandChangeClassification.Value, contentOf resultingSession, mutations)

let private topologyAndValue resultingSession mutations =
    Changed(CommandChangeClassification.Both, contentOf resultingSession, mutations)

/// Conservative default for a semantic change whose revision impact is not
/// known more precisely.
let private changed resultingSession mutations = topology resultingSession mutations

let private contentView (content: CanonicalContent) : CanonicalContentView = {
    Nodes = content.Nodes
    Processes = content.Processes
    Properties = content.Properties
    Values = content.Values
    Layers = content.Layers
    LayerOrder = content.LayerOrder
    ActiveLayerId = content.ActiveLayerId
}

let internal view =
    function
    | NoChange -> CommandEffectView.NoChange
    | Changed(classification, content, mutations) ->
        CommandEffectView.Changed(classification, contentView content, mutations)

module Swate.Components.Page.ProvenanceGrouping.CanonicalSession

open Swate.Components.Page.ProvenanceGrouping.Commands
open Swate.Components.Page.ProvenanceGrouping.ProjectionTypes

let commit (effect: CommandEffect) (session: ProvenanceSession) : ProvenanceSession =
    match view effect with
    | CommandEffectView.NoChange -> session
    | CommandEffectView.Changed(classification, content, mutations) ->
        let topologyRevision =
            match classification with
            | CommandChangeClassification.Topology
            | CommandChangeClassification.Both -> session.AvailabilityTopologyRevision + 1
            | CommandChangeClassification.Value -> session.AvailabilityTopologyRevision

        let valueRevision =
            match classification with
            | CommandChangeClassification.Value
            | CommandChangeClassification.Both -> session.AnnotationValueRevision + 1
            | CommandChangeClassification.Topology -> session.AnnotationValueRevision

        {
            session with
                Nodes = content.Nodes
                Processes = content.Processes
                Properties = content.Properties
                Values = content.Values
                Layers = content.Layers
                LayerOrder = content.LayerOrder
                ActiveLayerId = content.ActiveLayerId
                AvailabilityTopologyRevision = topologyRevision
                AnnotationValueRevision = valueRevision
                MutationJournal = session.MutationJournal @ mutations
                LayerProjections =
                    session.LayerProjections
                    |> Map.map (fun _ projection -> { projection with Stale = true })
        }

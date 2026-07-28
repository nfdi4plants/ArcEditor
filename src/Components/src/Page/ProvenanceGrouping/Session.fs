module Swate.Components.Page.ProvenanceGrouping.CanonicalSession

open Swate.Components.Page.ProvenanceGrouping.Commands
open Swate.Components.Page.ProvenanceGrouping.MutationTypes
open Swate.Components.Page.ProvenanceGrouping.ProjectionTypes

let private mutationContext =
    function
    | NodeAssignmentAdded(_, _, context)
    | NodeAssignmentValueChanged(_, _, _, context)
    | NodeAssignmentRemoved(_, context)
    | ProcessLinkRemoved(_, _, context)
    | PropertyDefinitionUpdated(_, _, context)
    | PropertyValueDefinitionUpdated(_, _, context)
    | PropertyValueDefinitionDeleted(_, _, context)
    | PropertyDefinitionDeleted(_, _, _, context)
    | ProcessAssignmentAdded(_, _, context)
    | ProcessAssignmentCoverageChanged(_, _, _, context)
    | ProcessAssignmentValueChanged(_, _, _, context)
    | ProcessAssignmentSplit(_, _, _, _, context)
    | ProcessAssignmentRemoved(_, context)
    | AdapterResourceReferenceReplaced(_, _, _, _, _, context) -> Some context
    | _ -> None

let private assignmentTombstone =
    function
    | NodeAssignmentRemoved(tombstone, _) -> Some(tombstone.Assignment.ValueId, NodeTombstone tombstone)
    | ProcessAssignmentRemoved(tombstone, _) -> Some(tombstone.Assignment.ValueId, ProcessTombstone tombstone)
    | _ -> None

let private appendImplicitGlobalValueDeletions (session: ProvenanceSession) (content: CanonicalContentView) mutations =
    let explicitlyDeletedValueIds =
        mutations
        |> List.collect (
            function
            | PropertyValueDefinitionDeleted(value, _, _) -> [ value.Id ]
            | PropertyDefinitionDeleted(_, values, _, _) -> values |> List.map _.Id
            | _ -> []
        )
        |> Set.ofList

    let globalContexts =
        mutations
        |> List.choose mutationContext
        |> List.filter (fun context -> context.Scope = GlobalDefinition)

    if globalContexts.IsEmpty then
        mutations
    else
        let tombstonesByValueId =
            mutations
            |> List.choose assignmentTombstone
            |> List.groupBy fst
            |> List.map (fun (valueId, entries) -> valueId, entries |> List.map snd)
            |> Map.ofList

        let implicitDeletions =
            session.Values
            |> Map.toList
            |> List.choose (fun (valueId, value) ->
                if
                    content.Values |> Map.containsKey valueId
                    || explicitlyDeletedValueIds |> Set.contains valueId
                then
                    None
                else
                    let tombstones =
                        tombstonesByValueId |> Map.tryFind valueId |> Option.defaultValue []

                    let removedAssignmentIds =
                        tombstones
                        |> List.map (
                            function
                            | NodeTombstone tombstone -> tombstone.Assignment.Id
                            | ProcessTombstone tombstone -> tombstone.Assignment.Id
                        )
                        |> Set.ofList

                    let contexts =
                        globalContexts
                        |> List.filter (fun context ->
                            removedAssignmentIds.IsEmpty
                            || not (Set.intersect removedAssignmentIds context.Coverage.AssignmentIds).IsEmpty
                        )

                    if contexts.IsEmpty then
                        None
                    else
                        let context = {
                            Scope = GlobalDefinition
                            Coverage = {
                                AssignmentIds =
                                    contexts
                                    |> List.collect (fun item -> item.Coverage.AssignmentIds |> Set.toList)
                                    |> Set.ofList
                                LinkIds =
                                    contexts
                                    |> List.collect (fun item -> item.Coverage.LinkIds |> Set.toList)
                                    |> Set.ofList
                            }
                        }

                        Some(PropertyValueDefinitionDeleted(value, tombstones, context))
            )

        mutations @ implicitDeletions

let commit (effect: CommandEffect) (session: ProvenanceSession) : ProvenanceSession =
    match view effect with
    | CommandEffectView.NoChange -> session
    | CommandEffectView.Changed(classification, content, mutations) ->
        let mutations = appendImplicitGlobalValueDeletions session content mutations

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

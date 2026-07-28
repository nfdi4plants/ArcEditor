module Swate.Components.Page.ProvenanceGrouping.Commands

open Swate.Components.Page.ProvenanceGrouping.MutationTypes
open Swate.Components.Page.ProvenanceGrouping.ProjectionTypes
open Swate.Components.Page.ProvenanceGrouping.Identifiers
open Swate.Components.Page.ProvenanceGrouping.Values
open Swate.Components.Page.ProvenanceGrouping.Domain
open Swate.Components.Page.ProvenanceGrouping.Model

type NodeValueContent = {
    Category: ProvenanceTerm
    Value: ProvenanceValue
    Unit: ProvenanceTerm option
}

type NodeAssignmentDraft = {
    Content: NodeValueContent
    OwnerKind: AnnotationOwnerKind
    PropertyKind: AssignmentPropertyKind
}

type NodeOverwriteSelection =
    | NoOverwrite
    | OverwriteAssignments of Map<CanonicalNodeId, AnnotationAssignmentId>

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

let private commandContext ownerIds assignmentIds = {
    Scope = ownerIds |> Seq.map NodeAssignmentOwner |> Set.ofSeq |> OwnerScoped
    Coverage = {
        AssignmentIds = assignmentIds |> Set.ofSeq
        LinkIds = Set.empty
    }
}

let private allAssignmentIds (session: ProvenanceSession) =
    seq {
        for KeyValue(_, node) in session.Nodes do
            yield! node.Assignments |> Map.keys

        for KeyValue(_, structuralProcess) in session.Processes do
            yield! structuralProcess.Assignments |> Map.keys
    }
    |> Set.ofSeq

let private nextAssignmentId usedIds =
    Seq.initInfinite (fun index -> $"assignment-{index + 1}")
    |> Seq.find (fun candidate -> usedIds |> Set.contains candidate |> not)

let private installPreparation (preparation: ValueDefinitionPreparation) (session: ProvenanceSession) = {
    session with
        Properties =
            session.Properties
            |> Map.add preparation.PropertyDefinition.Id preparation.PropertyDefinition
        Values =
            session.Values
            |> Map.add preparation.ValueDefinition.Id preparation.ValueDefinition
}

let private updateNode (node: CanonicalNode) (session: ProvenanceSession) = {
    session with
        Nodes = session.Nodes |> Map.add node.Id node
}

let private propertyOfAssignment (session: ProvenanceSession) (assignment: NodeAssignment) =
    session.Values
    |> Map.tryFind assignment.ValueId
    |> Option.bind (fun definition ->
        session.Properties
        |> Map.tryFind definition.PropertyId
        |> Option.map (fun property -> property, definition)
    )

let private matchingAssignments category propertyKind (node: CanonicalNode) (session: ProvenanceSession) =
    node.Assignments
    |> Map.toList
    |> List.choose (fun (_, assignment) ->
        match propertyOfAssignment session assignment with
        | Some(property, definition) when property.Category = category && assignment.PropertyKind = propertyKind ->
            Some(assignment, definition)
        | _ -> None
    )

let private samePreparedValue (preparation: ValueDefinitionPreparation) (definition: PropertyValueDefinition) =
    preparation.ValueDefinition.Id = definition.Id

let private semanticallyMatchesPreparation
    (preparation: ValueDefinitionPreparation)
    (definition: PropertyValueDefinition)
    (session: ProvenanceSession)
    =
    match session.Properties |> Map.tryFind definition.PropertyId with
    | None -> false
    | Some property ->
        let isolated = {
            session with
                Properties = Map.ofList [ property.Id, property ]
                Values = Map.ofList [ definition.Id, definition ]
        }

        let normalized =
            ensureValueDefinition
                preparation.PropertyDefinition.Category
                preparation.ValueDefinition.Value
                preparation.ValueDefinition.Unit
                isolated

        normalized.ValueDefinition.Id = definition.Id

let private validateTargets targets (session: ProvenanceSession) =
    if targets |> Set.isEmpty then
        Error EmptyTarget
    else
        targets
        |> Seq.tryFind (fun nodeId -> session.Nodes |> Map.containsKey nodeId |> not)
        |> function
            | Some nodeId -> Error(NodeNotFound nodeId)
            | None -> Ok()

let private cleanupValueAndProperty valueId (session: ProvenanceSession) =
    let references =
        valueDefinitionReferenceCounts session
        |> Map.tryFind valueId
        |> Option.defaultValue 0

    match references, session.Values |> Map.tryFind valueId with
    | 0, Some definition ->
        let withoutValue = {
            session with
                Values = session.Values |> Map.remove valueId
        }

        let propertyStillUsed =
            withoutValue.Values
            |> Map.exists (fun _ value -> value.PropertyId = definition.PropertyId)

        if propertyStillUsed then
            withoutValue
        else
            {
                withoutValue with
                    Properties = withoutValue.Properties |> Map.remove definition.PropertyId
            }
    | _ -> session

type private PlannedNodeAssignment =
    | Keep
    | Add
    | Replace of NodeAssignment

type private UnconfirmedNodeAssignmentPlan =
    | UnconfirmedKeep
    | UnconfirmedAdd
    | Conflicts of (NodeAssignment * PropertyValueDefinition) list

type private AssignmentNoOpPolicy =
    | SemanticAssignment
    | StrictOccurrence

let private planNodeAssignment preparation propertyKind lineage targetSource noOpPolicy node session =
    let matching =
        matchingAssignments preparation.PropertyDefinition.Category propertyKind node session

    let exactOccurrence =
        matching
        |> List.tryFind (fun (assignment, definition) ->
            match noOpPolicy with
            | SemanticAssignment -> semanticallyMatchesPreparation preparation definition session
            | StrictOccurrence ->
                samePreparedValue preparation definition
                && assignment.Lineage = lineage
                && assignment.TargetSource = targetSource
        )

    match exactOccurrence with
    | Some _ -> UnconfirmedKeep
    | None ->
        let differentValues =
            matching
            |> List.filter (fun (_, definition) -> semanticallyMatchesPreparation preparation definition session |> not)

        if differentValues.IsEmpty then
            UnconfirmedAdd
        else
            Conflicts differentValues

let private assignPreparedNodeValue targets preparation propertyKind lineage targetSource noOpPolicy overwrite session =
    match validateTargets targets session with
    | Error error -> Error error
    | Ok() ->
        let plans =
            targets
            |> Seq.map (fun nodeId ->
                let node = session.Nodes[nodeId]

                nodeId, planNodeAssignment preparation propertyKind lineage targetSource noOpPolicy node session
            )
            |> Seq.toList

        let conflicts =
            plans
            |> List.choose (fun (ownerId, plan) ->
                match plan with
                | Conflicts assignments -> Some(ownerId, assignments)
                | _ -> None
            )

        let conflictAssignmentIds =
            conflicts |> List.collect (snd >> List.map (fst >> _.Id)) |> Set.ofList

        let overwriteRequired () =
            Error(OverwriteConfirmationRequired(preparation.PropertyDefinition.Id, conflictAssignmentIds))

        let confirmedPlans =
            match overwrite with
            | NoOverwrite when conflicts.IsEmpty ->
                plans
                |> List.map (fun (ownerId, plan) ->
                    ownerId,
                    match plan with
                    | UnconfirmedKeep -> Keep
                    | UnconfirmedAdd -> Add
                    | Conflicts _ -> failwith "Conflicts were already ruled out."
                )
                |> Ok
            | NoOverwrite -> overwriteRequired ()
            | OverwriteAssignments confirmations ->
                let ambiguous =
                    conflicts |> List.tryFind (fun (_, assignments) -> assignments.Length <> 1)

                match ambiguous with
                | Some(_, assignments) ->
                    Error(
                        MultiplePropertyValues(
                            preparation.PropertyDefinition.Id,
                            assignments |> List.map (fst >> _.Id) |> Set.ofList
                        )
                    )
                | None ->
                    let expectedConfirmations =
                        conflicts
                        |> List.map (fun (ownerId, assignments) ->
                            ownerId, (assignments |> List.exactlyOne |> fst |> _.Id)
                        )
                        |> Map.ofList

                    if confirmations <> expectedConfirmations then
                        overwriteRequired ()
                    else
                        plans
                        |> List.map (fun (ownerId, plan) ->
                            ownerId,
                            match plan with
                            | UnconfirmedKeep -> Keep
                            | UnconfirmedAdd -> Add
                            | Conflicts assignments -> assignments |> List.exactlyOne |> fst |> Replace
                        )
                        |> Ok

        match confirmedPlans with
        | Error error -> Error error
        | Ok confirmedPlans ->
            let changes =
                confirmedPlans
                |> List.choose (fun (nodeId, plan) ->
                    match plan with
                    | Keep -> None
                    | plan -> Some(nodeId, plan)
                )

            if changes.IsEmpty then
                Ok noChange
            else
                let mutable usedIds = allAssignmentIds session
                let mutable resultingSession = installPreparation preparation session
                let mutable added = []
                let mutable replaced = []

                for nodeId, plan in changes do
                    let node: CanonicalNode = resultingSession.Nodes[nodeId]

                    match plan with
                    | Keep -> ()
                    | Add ->
                        let assignmentId = nextAssignmentId usedIds
                        usedIds <- usedIds |> Set.add assignmentId

                        let assignment: NodeAssignment = {
                            Id = assignmentId
                            ValueId = preparation.ValueDefinition.Id
                            PropertyKind = propertyKind
                            TargetSource = targetSource
                            Lineage = lineage
                        }

                        resultingSession <-
                            resultingSession
                            |> updateNode {
                                node with
                                    Assignments = node.Assignments |> Map.add assignment.Id assignment
                            }

                        added <- (nodeId, assignment) :: added
                    | Replace before ->
                        let after = {
                            before with
                                ValueId = preparation.ValueDefinition.Id
                        }

                        resultingSession <-
                            resultingSession
                            |> updateNode {
                                node with
                                    Assignments = node.Assignments |> Map.add after.Id after
                            }
                            |> cleanupValueAndProperty before.ValueId

                        replaced <- (nodeId, before, after) :: replaced

                let changedOwners = changes |> List.map fst |> Set.ofList

                let changedAssignments =
                    [
                        yield! added |> List.map (snd >> _.Id)
                        yield! replaced |> List.map (fun (_, _, assignment) -> assignment.Id)
                    ]
                    |> Set.ofList

                let context = commandContext changedOwners changedAssignments

                let mutations = [
                    yield!
                        added
                        |> List.rev
                        |> List.map (fun (ownerId, assignment) -> NodeAssignmentAdded(ownerId, assignment, context))

                    yield!
                        replaced
                        |> List.rev
                        |> List.map (fun (ownerId, before, after) ->
                            NodeAssignmentValueChanged(ownerId, before, after, context)
                        )
                ]

                match added.IsEmpty, replaced.IsEmpty with
                | false, true -> Ok(topology resultingSession mutations)
                | true, false -> Ok(value resultingSession mutations)
                | false, false -> Ok(topologyAndValue resultingSession mutations)
                | true, true -> Ok noChange

let assignNodeValue
    (targets: Set<CanonicalNodeId>)
    (draft: NodeAssignmentDraft)
    (overwrite: NodeOverwriteSelection)
    (session: ProvenanceSession)
    : Result<CommandEffect, ProvenanceCommandError> =
    if draft.OwnerKind <> AnnotationOwnerKind.Node then
        Error(InconsistentCanonicalState "A node assignment command requires AnnotationOwnerKind.Node.")
    elif draft.PropertyKind <> AssignmentPropertyKind.Generic then
        Error(InconsistentCanonicalState "A newly created node property must use AssignmentPropertyKind.Generic.")
    else
        let preparation =
            ensureValueDefinition draft.Content.Category draft.Content.Value draft.Content.Unit session

        assignPreparedNodeValue
            targets
            preparation
            AssignmentPropertyKind.Generic
            AssignmentLineage.Created
            None
            SemanticAssignment
            overwrite
            session

let assignCatalogNodeValue
    (targets: Set<CanonicalNodeId>)
    (catalog: ReferenceCatalog)
    (entry: ReferenceCatalogEntry)
    (overwrite: NodeOverwriteSelection)
    (session: ProvenanceSession)
    : Result<CommandEffect, ProvenanceCommandError> =
    if entry.AssignmentKind <> AnnotationOwnerKind.Node then
        Error(InconsistentCanonicalState "A process catalog entry cannot be assigned to canonical nodes.")
    elif
        tryFindCatalogEntry entry.Reference.Scheme entry.Reference.Id catalog
        <> Some entry
    then
        Error(InconsistentCanonicalState "The catalog does not contain the exact requested entry.")
    else
        let preparation = promoteCatalogEntry entry session

        assignPreparedNodeValue
            targets
            preparation
            entry.PropertyKind
            AssignmentLineage.Created
            None
            SemanticAssignment
            overwrite
            session

let editNodeAssignment
    (ownerId: CanonicalNodeId)
    (assignmentId: AnnotationAssignmentId)
    (content: NodeValueContent)
    (session: ProvenanceSession)
    : Result<CommandEffect, ProvenanceCommandError> =
    match session.Nodes |> Map.tryFind ownerId with
    | None -> Error(NodeNotFound ownerId)
    | Some node ->
        match node.Assignments |> Map.tryFind assignmentId with
        | None -> Error(AssignmentNotFound(Some(NodeAssignmentOwner ownerId), assignmentId))
        | Some assignment ->
            match session.Values |> Map.tryFind assignment.ValueId with
            | None -> Error(ValueNotFound assignment.ValueId)
            | Some beforeValue ->
                match session.Properties |> Map.tryFind beforeValue.PropertyId with
                | None -> Error(PropertyNotFound beforeValue.PropertyId)
                | Some _ ->
                    let preparation =
                        ensureValueDefinition content.Category content.Value content.Unit session

                    if semanticallyMatchesPreparation preparation beforeValue session then
                        Ok noChange
                    else
                        let referenceCount =
                            valueDefinitionReferenceCounts session
                            |> Map.tryFind assignment.ValueId
                            |> Option.defaultValue 0

                        let context = commandContext (Set.singleton ownerId) (Set.singleton assignmentId)

                        let targetAlreadyExists =
                            preparation.ValueDefinition.Id <> beforeValue.Id
                            && session.Values |> Map.containsKey preparation.ValueDefinition.Id

                        if targetAlreadyExists || referenceCount > 1 then
                            let afterAssignment = {
                                assignment with
                                    ValueId = preparation.ValueDefinition.Id
                            }

                            let resultingSession =
                                session
                                |> installPreparation preparation
                                |> updateNode {
                                    node with
                                        Assignments = node.Assignments |> Map.add afterAssignment.Id afterAssignment
                                }
                                |> cleanupValueAndProperty assignment.ValueId

                            Ok(
                                value resultingSession [
                                    NodeAssignmentValueChanged(ownerId, assignment, afterAssignment, context)
                                ]
                            )
                        else
                            let property = preparation.PropertyDefinition

                            let afterValue = {
                                beforeValue with
                                    PropertyId = property.Id
                                    Value = content.Value
                                    Unit = content.Unit
                            }

                            let resultingSession = {
                                session with
                                    Properties = session.Properties |> Map.add property.Id property
                                    Values = session.Values |> Map.add afterValue.Id afterValue
                            }

                            let resultingSession =
                                if beforeValue.PropertyId = afterValue.PropertyId then
                                    resultingSession
                                else
                                    let oldPropertyStillUsed =
                                        resultingSession.Values
                                        |> Map.exists (fun _ value -> value.PropertyId = beforeValue.PropertyId)

                                    if oldPropertyStillUsed then
                                        resultingSession
                                    else
                                        {
                                            resultingSession with
                                                Properties =
                                                    resultingSession.Properties |> Map.remove beforeValue.PropertyId
                                        }

                            Ok(
                                value resultingSession [
                                    PropertyValueDefinitionUpdated(beforeValue, afterValue, context)
                                ]
                            )

let removeNodeAssignment
    (ownerId: CanonicalNodeId)
    (assignmentId: AnnotationAssignmentId)
    (session: ProvenanceSession)
    : Result<CommandEffect, ProvenanceCommandError> =
    match session.Nodes |> Map.tryFind ownerId with
    | None -> Error(NodeNotFound ownerId)
    | Some node ->
        match node.Assignments |> Map.tryFind assignmentId with
        | None -> Error(AssignmentNotFound(Some(NodeAssignmentOwner ownerId), assignmentId))
        | Some assignment ->
            match session.Values |> Map.tryFind assignment.ValueId with
            | None -> Error(ValueNotFound assignment.ValueId)
            | Some _ ->
                let context = commandContext (Set.singleton ownerId) (Set.singleton assignmentId)

                let resultingSession =
                    session
                    |> updateNode {
                        node with
                            Assignments = node.Assignments |> Map.remove assignmentId
                    }
                    |> cleanupValueAndProperty assignment.ValueId

                Ok(
                    topology resultingSession [
                        NodeAssignmentRemoved(
                            {
                                OwnerId = ownerId
                                Assignment = assignment
                            },
                            context
                        )
                    ]
                )

let copyLoadedNodeValue
    (sourceOwnerId: CanonicalNodeId)
    (sourceAssignmentId: AnnotationAssignmentId)
    (targets: Set<CanonicalNodeId>)
    (targetSource: ProvenanceSourceRef option)
    (session: ProvenanceSession)
    : Result<CommandEffect, ProvenanceCommandError> =
    match session.Nodes |> Map.tryFind sourceOwnerId with
    | None -> Error(NodeNotFound sourceOwnerId)
    | Some sourceNode ->
        match sourceNode.Assignments |> Map.tryFind sourceAssignmentId with
        | None -> Error(AssignmentNotFound(Some(NodeAssignmentOwner sourceOwnerId), sourceAssignmentId))
        | Some sourceAssignment ->
            match propertyOfAssignment session sourceAssignment with
            | None ->
                if session.Values |> Map.containsKey sourceAssignment.ValueId then
                    Error(PropertyNotFound(session.Values[sourceAssignment.ValueId].PropertyId))
                else
                    Error(ValueNotFound sourceAssignment.ValueId)
            | Some(property, definition) ->
                let preparation = {
                    PropertyDefinition = property
                    ValueDefinition = definition
                }

                assignPreparedNodeValue
                    targets
                    preparation
                    sourceAssignment.PropertyKind
                    (AssignmentLineage.DerivedFrom sourceAssignment.Id)
                    targetSource
                    StrictOccurrence
                    NoOverwrite
                    session

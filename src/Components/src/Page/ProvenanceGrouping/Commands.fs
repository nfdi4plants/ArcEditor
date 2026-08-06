module Swate.Components.Page.ProvenanceGrouping.Commands

open Swate.Components.Page.ProvenanceGrouping.MutationTypes
open Swate.Components.Page.ProvenanceGrouping.ProjectionTypes
open Swate.Components.Page.ProvenanceGrouping.Identifiers
open Swate.Components.Page.ProvenanceGrouping.Values
open Swate.Components.Page.ProvenanceGrouping.Domain
open Swate.Components.Page.ProvenanceGrouping.AvailabilityTypes
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

type ProcessAssignmentDraft = {
    Content: NodeValueContent
    OwnerKind: AnnotationOwnerKind
    PropertyKind: AssignmentPropertyKind
    ContainerReferenceValueId: PropertyValueDefinitionId option
    ReferenceSlotId: ReferenceSlotId option
    Lineage: AssignmentLineage
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

let private combineClassification left right =
    match left, right with
    | CommandChangeClassification.Both, _
    | _, CommandChangeClassification.Both -> CommandChangeClassification.Both
    | CommandChangeClassification.Topology, CommandChangeClassification.Value
    | CommandChangeClassification.Value, CommandChangeClassification.Topology -> CommandChangeClassification.Both
    | CommandChangeClassification.Topology, CommandChangeClassification.Topology -> CommandChangeClassification.Topology
    | CommandChangeClassification.Value, CommandChangeClassification.Value -> CommandChangeClassification.Value

/// Runs several already-validated command plans against one canonical base and
/// returns one effect. The individual plans still see the canonical content
/// produced by their predecessors, while the caller commits the resulting
/// topology/value classification and journal exactly once.
let atomic
    (operations: (ProvenanceSession -> Result<CommandEffect, ProvenanceCommandError>) list)
    (session: ProvenanceSession)
    : Result<CommandEffect, ProvenanceCommandError> =
    let contentSession (content: CanonicalContent) = {
        session with
            Nodes = content.Nodes
            Processes = content.Processes
            Properties = content.Properties
            Values = content.Values
            Layers = content.Layers
            LayerOrder = content.LayerOrder
            ActiveLayerId = content.ActiveLayerId
    }

    let rec run remaining current classification (content: CanonicalContent) mutations =
        match remaining with
        | [] ->
            match mutations with
            | [] -> Ok noChange
            | _ ->
                let resultingSession = contentSession content

                match classification with
                | Some classification -> Ok(CommandEffect.Changed(classification, content, mutations))
                | None -> Ok(topology resultingSession mutations)
        | operation :: rest ->
            match operation (contentSession content) with
            | Error error -> Error error
            | Ok effect ->
                match effect with
                | CommandEffect.NoChange -> run rest current classification content mutations
                | CommandEffect.Changed(nextClassification, nextContent, nextMutations) ->
                    let nextClassification =
                        match classification with
                        | Some previous -> Some(combineClassification previous nextClassification)
                        | None -> Some nextClassification

                    run rest (contentSession nextContent) nextClassification nextContent (mutations @ nextMutations)

    run operations session None (contentOf session) []

let private commandContext ownerIds assignmentIds = {
    Scope = ownerIds |> Seq.map NodeAssignmentOwner |> Set.ofSeq |> OwnerScoped
    Coverage = {
        AssignmentIds = assignmentIds |> Set.ofSeq
        LinkIds = Set.empty
    }
}

let private processCommandContext ownerIds assignmentIds linkIds = {
    Scope = ownerIds |> Seq.map ProcessAssignmentOwner |> Set.ofSeq |> OwnerScoped
    Coverage = {
        AssignmentIds = assignmentIds |> Set.ofSeq
        LinkIds = linkIds
    }
}

let private mixedOwnerCommandContext owners assignmentIds = {
    Scope = OwnerScoped owners
    Coverage = {
        AssignmentIds = assignmentIds
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

let private updateProcess (structuralProcess: StructuralProcess) (session: ProvenanceSession) = {
    session with
        Processes = session.Processes |> Map.add structuralProcess.Id structuralProcess
}

let private propertyOfAssignment (session: ProvenanceSession) (assignment: NodeAssignment) =
    session.Values
    |> Map.tryFind assignment.ValueId
    |> Option.bind (fun definition ->
        session.Properties
        |> Map.tryFind definition.PropertyId
        |> Option.map (fun property -> property, definition)
    )

let private propertyOfProcessAssignment (session: ProvenanceSession) (assignment: ProcessAssignment) =
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
    elif
        // A genuinely new property is generic; a draft reusing a kind-bearing
        // entry carries that entry's one established kind (intent §1, §3).
        draft.PropertyKind <> AssignmentPropertyKind.Generic
        && draft.PropertyKind
           <> establishedPropertyKind AnnotationOwnerKind.Node draft.Content.Category session
    then
        Error(
            InconsistentCanonicalState
                "A node property draft must use AssignmentPropertyKind.Generic or the entry's established concrete kind."
        )
    else
        let preparation =
            ensureValueDefinition draft.Content.Category draft.Content.Value draft.Content.Unit session

        assignPreparedNodeValue
            targets
            preparation
            draft.PropertyKind
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

let private resolveLinkOwners (linkIds: Set<ProcessLinkId>) (session: ProvenanceSession) =
    if linkIds.IsEmpty then
        Error EmptyTarget
    else
        let owners =
            linkIds
            |> Seq.map (fun linkId ->
                let matches =
                    session.Processes
                    |> Map.toList
                    |> List.choose (fun (processId, structuralProcess) ->
                        if structuralProcess.Links |> Map.containsKey linkId then
                            Some processId
                        else
                            None
                    )

                linkId, matches
            )
            |> Seq.toList

        match owners |> List.tryFind (snd >> List.isEmpty) with
        | Some(linkId, _) -> Error(LinkNotFound linkId)
        | None ->
            match owners |> List.tryFind (fun (_, matches) -> matches.Length <> 1) with
            | Some(linkId, matches) ->
                Error(
                    InconsistentCanonicalState
                        $"Process link '{linkId}' is owned by {matches.Length} structural processes."
                )
            | None ->
                owners
                |> List.map (fun (linkId, matches) -> matches.Head, linkId)
                |> List.groupBy fst
                |> List.map (fun (processId, links) -> processId, (links |> List.map snd |> Set.ofList))
                |> Map.ofList
                |> Ok

let private matchingProcessAssignments
    preparation
    propertyKind
    containerReferenceValueId
    referenceSlotId
    lineage
    structuralProcess
    session
    =
    structuralProcess.Assignments
    |> Map.toList
    |> List.choose (fun (_, assignment) ->
        match propertyOfProcessAssignment session assignment with
        | Some(property, definition) when
            property.Category = preparation.PropertyDefinition.Category
            && assignment.PropertyKind = propertyKind
            && assignment.ContainerReferenceValueId = containerReferenceValueId
            && assignment.ReferenceSlotId = referenceSlotId
            && assignment.Lineage = lineage
            && semanticallyMatchesPreparation preparation definition session
            ->
            Some assignment
        | _ -> None
    )

let private isReferenceValue valueId (session: ProvenanceSession) =
    match session.Values |> Map.tryFind valueId with
    | Some { Value = ProvenanceValue.Reference _ } -> true
    | _ -> false

let private hasContainerBoundOccurrence valueId (session: ProvenanceSession) =
    session.Processes
    |> Map.exists (fun _ structuralProcess ->
        structuralProcess.Assignments
        |> Map.exists (fun _ assignment -> assignment.ValueId = valueId && assignment.ContainerReferenceValueId.IsSome)
    )

let private boundDependentLinks requiredValueId selectedLinks (structuralProcess: StructuralProcess) =
    structuralProcess.Assignments
    |> Map.toList
    |> List.map snd
    |> List.filter (fun assignment -> assignment.ContainerReferenceValueId = Some requiredValueId)
    |> List.collect (fun assignment -> assignment.CoveredLinkIds |> Set.intersect selectedLinks |> Set.toList)
    |> Set.ofList

let private validateExactReferenceBacking
    (structuralProcess: StructuralProcess)
    (requiredValueId: PropertyValueDefinitionId)
    (expectedAssignmentId: AnnotationAssignmentId option)
    (linkIds: Set<ProcessLinkId>)
    (session: ProvenanceSession)
    =
    linkIds
    |> Seq.tryPick (fun linkId ->
        let backing =
            structuralProcess.Assignments
            |> Map.toList
            |> List.map snd
            |> List.filter (fun assignment ->
                assignment.ContainerReferenceValueId.IsNone
                && assignment.ValueId = requiredValueId
                && assignment.CoveredLinkIds.Contains linkId
            )

        match backing with
        | [ assignment ] when
            isReferenceValue assignment.ValueId session
            && (expectedAssignmentId.IsNone || expectedAssignmentId = Some assignment.Id)
            ->
            None
        | _ ->
            Some(
                InconsistentCanonicalState
                    $"Link '{linkId}' does not have the exact unambiguous reference backing '{requiredValueId}'."
            )
    )

let private sessionWithContent (content: CanonicalContent) (session: ProvenanceSession) : ProvenanceSession = {
    session with
        Nodes = content.Nodes
        Processes = content.Processes
        Properties = content.Properties
        Values = content.Values
        Layers = content.Layers
        LayerOrder = content.LayerOrder
        ActiveLayerId = content.ActiveLayerId
}

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

let private replaceMutationContext context =
    function
    | NodeAssignmentAdded(ownerId, assignment, _) -> NodeAssignmentAdded(ownerId, assignment, context)
    | NodeAssignmentValueChanged(ownerId, before, after, _) ->
        NodeAssignmentValueChanged(ownerId, before, after, context)
    | NodeAssignmentRemoved(tombstone, _) -> NodeAssignmentRemoved(tombstone, context)
    | ProcessLinkRemoved(processId, link, _) -> ProcessLinkRemoved(processId, link, context)
    | PropertyDefinitionUpdated(before, after, _) -> PropertyDefinitionUpdated(before, after, context)
    | PropertyValueDefinitionUpdated(before, after, _) -> PropertyValueDefinitionUpdated(before, after, context)
    | PropertyValueDefinitionDeleted(value, tombstones, _) -> PropertyValueDefinitionDeleted(value, tombstones, context)
    | PropertyDefinitionDeleted(property, values, tombstones, _) ->
        PropertyDefinitionDeleted(property, values, tombstones, context)
    | ProcessAssignmentAdded(ownerId, assignment, _) -> ProcessAssignmentAdded(ownerId, assignment, context)
    | ProcessAssignmentCoverageChanged(ownerId, before, after, _) ->
        ProcessAssignmentCoverageChanged(ownerId, before, after, context)
    | ProcessAssignmentValueChanged(ownerId, before, after, _) ->
        ProcessAssignmentValueChanged(ownerId, before, after, context)
    | ProcessAssignmentSplit(ownerId, original, retained, split, _) ->
        ProcessAssignmentSplit(ownerId, original, retained, split, context)
    | ProcessAssignmentRemoved(tombstone, _) -> ProcessAssignmentRemoved(tombstone, context)
    | AdapterResourceReferenceReplaced(ownerId, before, after, removed, added, _) ->
        AdapterResourceReferenceReplaced(ownerId, before, after, removed, added, context)
    | mutation -> mutation

let private combineEffects scopeOverride (effects: CommandEffect list) (session: ProvenanceSession) =
    let changedEffects =
        effects
        |> List.choose (
            function
            | NoChange -> None
            | Changed(classification, content, mutations) -> Some(classification, content, mutations)
        )

    match changedEffects with
    | [] -> noChange
    | _ ->
        let _, finalContent, _ = List.last changedEffects
        let mutations = changedEffects |> List.collect (fun (_, _, items) -> items)
        let contexts = mutations |> List.choose mutationContext

        let assignmentIds =
            contexts
            |> List.collect (fun item -> Set.toList item.Coverage.AssignmentIds)
            |> Set.ofList

        let linkIds =
            contexts
            |> List.collect (fun item -> Set.toList item.Coverage.LinkIds)
            |> Set.ofList

        let owners =
            contexts
            |> List.collect (fun item ->
                match item.Scope with
                | OwnerScoped scopedOwners -> Set.toList scopedOwners
                | GlobalDefinition -> []
            )
            |> Set.ofList

        let context = {
            Scope =
                match scopeOverride with
                | Some scope -> scope
                | None -> OwnerScoped owners
            Coverage = {
                AssignmentIds = assignmentIds
                LinkIds = linkIds
            }
        }

        let journal = mutations |> List.map (replaceMutationContext context)

        let hasTopology =
            changedEffects
            |> List.exists (fun (classification, _, _) ->
                classification = CommandChangeClassification.Topology
                || classification = CommandChangeClassification.Both
            )

        let hasValue =
            changedEffects
            |> List.exists (fun (classification, _, _) ->
                classification = CommandChangeClassification.Value
                || classification = CommandChangeClassification.Both
            )

        let classification =
            match hasTopology, hasValue with
            | true, true -> CommandChangeClassification.Both
            | true, false -> CommandChangeClassification.Topology
            | false, true -> CommandChangeClassification.Value
            | false, false -> CommandChangeClassification.Topology

        Changed(classification, finalContent, journal)

let private assignProcessValueWithReservedIds
    (reservedAssignmentIds: Set<AnnotationAssignmentId>)
    (linkIds: Set<ProcessLinkId>)
    (draft: ProcessAssignmentDraft)
    (session: ProvenanceSession)
    : Result<CommandEffect, ProvenanceCommandError> =
    if draft.OwnerKind <> AnnotationOwnerKind.Process then
        Error(InconsistentCanonicalState "A process assignment command requires AnnotationOwnerKind.Process.")
    elif
        draft.ReferenceSlotId.IsSome
        && (
            match draft.Content.Value with
            | ProvenanceValue.Reference _ -> false
            | _ -> true
        )
    then
        Error(InconsistentCanonicalState "Only a reference-valued process assignment may carry a reference slot.")
    elif
        draft.PropertyKind <> AssignmentPropertyKind.Generic
        && draft.Lineage = AssignmentLineage.Created
        && draft.ContainerReferenceValueId.IsNone
        && draft.ReferenceSlotId.IsNone
        && (
            match draft.Content.Value with
            | ProvenanceValue.Reference _ -> false
            | _ -> true
        )
        // A genuinely new property is generic; a draft reusing a kind-bearing
        // entry carries that entry's one established kind (intent §1, §3).
        && draft.PropertyKind
           <> establishedPropertyKind AnnotationOwnerKind.Process draft.Content.Category session
    then
        Error(
            InconsistentCanonicalState
                "A process property draft must use AssignmentPropertyKind.Generic or the entry's established concrete kind."
        )
    else
        match resolveLinkOwners linkIds session with
        | Error error -> Error error
        | Ok linksByProcess ->
            let preparation =
                ensureValueDefinition draft.Content.Category draft.Content.Value draft.Content.Unit session

            let assigningReference =
                match draft.Content.Value with
                | ProvenanceValue.Reference _ -> true
                | _ -> false

            let usedAssignmentIds = (allAssignmentIds session) + reservedAssignmentIds

            let prospectiveAssignmentId = nextAssignmentId usedAssignmentIds

            let slotError =
                match assigningReference, draft.ReferenceSlotId with
                | true, Some slotId ->
                    linksByProcess
                    |> Map.toList
                    |> List.tryPick (fun (processId, selectedLinks) ->
                        let structuralProcess = session.Processes[processId]

                        selectedLinks
                        |> Seq.tryPick (fun linkId ->
                            let occupied =
                                structuralProcess.Assignments
                                |> Map.toList
                                |> List.map snd
                                |> List.filter (fun assignment ->
                                    assignment.ReferenceSlotId = Some slotId
                                    && assignment.CoveredLinkIds.Contains linkId
                                )

                            if occupied.Length > 1 then
                                Some(
                                    InconsistentCanonicalState
                                        $"Reference slot '{slotId}' has multiple assignments on link '{linkId}'."
                                )
                            elif
                                occupied
                                |> List.exists (fun assignment -> not (isReferenceValue assignment.ValueId session))
                            then
                                Some(
                                    InconsistentCanonicalState
                                        $"Reference slot '{slotId}' has a non-reference assignment on link '{linkId}'."
                                )
                            else
                                None
                        )
                    )
                | _ -> None

            let containerError =
                draft.ContainerReferenceValueId
                |> Option.bind (fun requiredValueId ->
                    linksByProcess
                    |> Map.toList
                    |> List.tryPick (fun (processId, selectedLinks) ->
                        let structuralProcess = session.Processes[processId]
                        let mutable missing = Set.empty
                        let mutable corrupt = None

                        for linkId in selectedLinks do
                            let backing =
                                structuralProcess.Assignments
                                |> Map.toList
                                |> List.map snd
                                |> List.filter (fun assignment ->
                                    assignment.ValueId = requiredValueId
                                    && assignment.CoveredLinkIds.Contains linkId
                                )

                            match backing with
                            | [ assignment ] when
                                assignment.ContainerReferenceValueId.IsNone
                                && isReferenceValue assignment.ValueId session
                                ->
                                ()
                            | [] -> missing <- missing |> Set.add linkId
                            | _ ->
                                corrupt <-
                                    Some(
                                        InconsistentCanonicalState
                                            $"Link '{linkId}' does not have exactly one canonical reference container '{requiredValueId}'."
                                    )

                        match corrupt with
                        | Some error -> Some error
                        | None when missing.IsEmpty -> None
                        | None -> Some(MissingReferenceContainer(prospectiveAssignmentId, requiredValueId, missing))
                    )
                )

            let cascadeError =
                match assigningReference, draft.ReferenceSlotId with
                | true, Some slotId ->
                    linksByProcess
                    |> Map.toList
                    |> List.tryPick (fun (processId, selectedLinks) ->
                        let structuralProcess = session.Processes[processId]

                        let conflicting =
                            selectedLinks
                            |> Seq.collect (fun linkId ->
                                structuralProcess.Assignments
                                |> Map.toSeq
                                |> Seq.map snd
                                |> Seq.filter (fun assignment ->
                                    assignment.ReferenceSlotId = Some slotId
                                    && assignment.CoveredLinkIds.Contains linkId
                                )
                            )
                            |> Seq.distinctBy _.Id
                            |> Seq.filter (fun assignment ->
                                assignment.ValueId <> preparation.ValueDefinition.Id
                                || assignment.PropertyKind <> draft.PropertyKind
                                || assignment.Lineage <> draft.Lineage
                                || assignment.ReferenceSlotId <> draft.ReferenceSlotId
                                || assignment.ContainerReferenceValueId <> draft.ContainerReferenceValueId
                            )
                            |> Seq.toList

                        conflicting
                        |> List.tryPick (fun oldReference ->
                            let removedLinks = oldReference.CoveredLinkIds |> Set.intersect selectedLinks

                            let dependentLinks =
                                boundDependentLinks oldReference.ValueId removedLinks structuralProcess

                            validateExactReferenceBacking
                                structuralProcess
                                oldReference.ValueId
                                (Some oldReference.Id)
                                dependentLinks
                                session
                        )
                    )
                | _ -> None

            match slotError |> Option.orElse containerError |> Option.orElse cascadeError with
            | Some error -> Error error
            | None ->
                let mutable usedIds = usedAssignmentIds
                let mutable resultingSession = installPreparation preparation session

                let changes =
                    ResizeArray<StructuralProcessId * ProcessAssignment option * ProcessAssignment option>()

                let replacements =
                    ResizeArray<
                        StructuralProcessId *
                        ProcessAssignment *
                        ProcessAssignmentTombstone list *
                        ProcessAssignment option
                     >()

                for KeyValue(processId, selectedLinks) in linksByProcess do
                    let structuralProcess = resultingSession.Processes[processId]
                    let mutable assignments = structuralProcess.Assignments

                    if assigningReference && draft.ReferenceSlotId.IsSome then
                        let slotId = draft.ReferenceSlotId.Value

                        let occupied =
                            selectedLinks
                            |> Seq.collect (fun linkId ->
                                assignments
                                |> Map.toSeq
                                |> Seq.map snd
                                |> Seq.filter (fun assignment ->
                                    assignment.ReferenceSlotId = Some slotId
                                    && assignment.CoveredLinkIds.Contains linkId
                                )
                            )
                            |> Seq.distinctBy _.Id
                            |> Seq.toList

                        let conflicting =
                            occupied
                            |> List.filter (fun assignment ->
                                assignment.ValueId <> preparation.ValueDefinition.Id
                                || assignment.PropertyKind <> draft.PropertyKind
                                || assignment.Lineage <> draft.Lineage
                                || assignment.ReferenceSlotId <> draft.ReferenceSlotId
                                || assignment.ContainerReferenceValueId <> draft.ContainerReferenceValueId
                            )

                        for oldReference in conflicting do
                            let removedLinks = oldReference.CoveredLinkIds |> Set.intersect selectedLinks

                            let family =
                                assignments
                                |> Map.toList
                                |> List.map snd
                                |> List.filter (fun assignment ->
                                    assignment.Id = oldReference.Id
                                    || assignment.ContainerReferenceValueId = Some oldReference.ValueId
                                )

                            let mutable removedDependents = []

                            for before in family do
                                let remainder = before.CoveredLinkIds - removedLinks

                                if remainder.IsEmpty then
                                    assignments <- assignments |> Map.remove before.Id
                                    changes.Add(processId, Some before, None)

                                    if before.Id <> oldReference.Id then
                                        removedDependents <-
                                            {
                                                OwnerId = processId
                                                Assignment = before
                                            }
                                            :: removedDependents
                                elif remainder <> before.CoveredLinkIds then
                                    let after = {
                                        before with
                                            CoveredLinkIds = remainder
                                    }

                                    assignments <- assignments |> Map.add after.Id after
                                    changes.Add(processId, Some before, Some after)

                            replacements.Add(processId, oldReference, List.rev removedDependents, None)

                    let currentProcess = {
                        structuralProcess with
                            Assignments = assignments
                    }

                    let matching =
                        matchingProcessAssignments
                            preparation
                            draft.PropertyKind
                            draft.ContainerReferenceValueId
                            draft.ReferenceSlotId
                            draft.Lineage
                            currentProcess
                            resultingSession

                    let alreadyCovered = matching |> Seq.collect _.CoveredLinkIds |> Set.ofSeq
                    let missing = selectedLinks - alreadyCovered
                    let mutable assignedOccurrence = matching |> List.tryHead

                    if not missing.IsEmpty then
                        match assignedOccurrence with
                        | Some before ->
                            let after = {
                                before with
                                    CoveredLinkIds = before.CoveredLinkIds + missing
                            }

                            assignments <- assignments |> Map.add after.Id after
                            changes.Add(processId, Some before, Some after)
                            assignedOccurrence <- Some after
                        | None ->
                            let assignmentId = nextAssignmentId usedIds
                            usedIds <- usedIds |> Set.add assignmentId

                            let assignment = {
                                Id = assignmentId
                                ValueId = preparation.ValueDefinition.Id
                                PropertyKind = draft.PropertyKind
                                CoveredLinkIds = missing
                                ContainerReferenceValueId = draft.ContainerReferenceValueId
                                ReferenceSlotId = draft.ReferenceSlotId
                                Lineage = draft.Lineage
                            }

                            assignments <- assignments |> Map.add assignment.Id assignment
                            changes.Add(processId, None, Some assignment)
                            assignedOccurrence <- Some assignment

                    if replacements.Count > 0 then
                        for index in 0 .. replacements.Count - 1 do
                            let replacementProcessId, before, removed, after = replacements[index]

                            if replacementProcessId = processId && after.IsNone then
                                replacements[index] <- replacementProcessId, before, removed, assignedOccurrence

                    resultingSession <-
                        resultingSession
                        |> updateProcess {
                            structuralProcess with
                                Assignments = assignments
                        }

                if changes.Count = 0 then
                    Ok noChange
                else
                    let changedOwners = changes |> Seq.map (fun (ownerId, _, _) -> ownerId) |> Set.ofSeq

                    let changedIds =
                        changes
                        |> Seq.collect (fun (_, before, after) -> seq {
                            yield! before |> Option.map _.Id |> Option.toList
                            yield! after |> Option.map _.Id |> Option.toList
                        })
                        |> Set.ofSeq

                    let changedLinks =
                        changes
                        |> Seq.collect (fun (_, before, after) ->
                            let beforeLinks =
                                before |> Option.map _.CoveredLinkIds |> Option.defaultValue Set.empty

                            let afterLinks =
                                after |> Option.map _.CoveredLinkIds |> Option.defaultValue Set.empty

                            (beforeLinks - afterLinks) + (afterLinks - beforeLinks)
                        )
                        |> Set.ofSeq

                    let context = processCommandContext changedOwners changedIds changedLinks

                    let journal =
                        changes
                        |> Seq.map (fun (ownerId, before, after) ->
                            match before, after with
                            | None, Some assignment -> ProcessAssignmentAdded(ownerId, assignment, context)
                            | Some oldAssignment, Some newAssignment ->
                                ProcessAssignmentCoverageChanged(ownerId, oldAssignment, newAssignment, context)
                            | Some assignment, None ->
                                ProcessAssignmentRemoved(
                                    {
                                        OwnerId = ownerId
                                        Assignment = assignment
                                    },
                                    context
                                )
                            | None, None -> failwith "Impossible empty assignment change."
                        )
                        |> Seq.toList

                    let replacementJournal =
                        replacements
                        |> Seq.choose (fun (ownerId, before, removed, after) ->
                            after
                            |> Option.map (fun replacement ->
                                AdapterResourceReferenceReplaced(ownerId, before, replacement, removed, [], context)
                            )
                        )
                        |> Seq.toList

                    let cleanupIds =
                        changes
                        |> Seq.collect (fun (_, before, _) -> before |> Option.map _.ValueId |> Option.toList)
                        |> Set.ofSeq

                    resultingSession <-
                        cleanupIds
                        |> Set.fold (fun current valueId -> cleanupValueAndProperty valueId current) resultingSession

                    Ok(topology resultingSession (journal @ replacementJournal))

let private assignProcessValueWithoutOverwrite
    (linkIds: Set<ProcessLinkId>)
    (draft: ProcessAssignmentDraft)
    (session: ProvenanceSession)
    : Result<CommandEffect, ProvenanceCommandError> =
    let referenceSlotReplacement =
        draft.ReferenceSlotId.IsSome
        && (
            match draft.Content.Value with
            | ProvenanceValue.Reference _ -> true
            | _ -> false
        )

    if referenceSlotReplacement then
        assignProcessValueWithReservedIds Set.empty linkIds draft session
    else
        match resolveLinkOwners linkIds session with
        | Error error -> Error error
        | Ok linksByProcess ->
            let preparation =
                ensureValueDefinition draft.Content.Category draft.Content.Value draft.Content.Unit session

            let sameHeaderByLink =
                linksByProcess
                |> Map.toList
                |> List.collect (fun (processId, selectedLinks) ->
                    let structuralProcess = session.Processes[processId]

                    selectedLinks
                    |> Set.toList
                    |> List.map (fun linkId ->
                        let assignments =
                            structuralProcess.Assignments
                            |> Map.toList
                            |> List.map snd
                            |> List.choose (fun assignment ->
                                match propertyOfProcessAssignment session assignment with
                                | Some(property, definition) when
                                    property.Category = preparation.PropertyDefinition.Category
                                    && assignment.PropertyKind = draft.PropertyKind
                                    && assignment.CoveredLinkIds.Contains linkId
                                    ->
                                    Some(assignment, definition)
                                | _ -> None
                            )

                        linkId, assignments
                    )
                )
                |> Map.ofList

            let missingLinks =
                sameHeaderByLink
                |> Map.toList
                |> List.choose (fun (linkId, assignments) -> if assignments.IsEmpty then Some linkId else None)
                |> Set.ofList

            let conflictsByLink =
                sameHeaderByLink
                |> Map.toList
                |> List.choose (fun (linkId, assignments) ->
                    let hasExactValue =
                        assignments
                        |> List.exists (fun (_, definition) ->
                            semanticallyMatchesPreparation preparation definition session
                        )

                    if hasExactValue || assignments.IsEmpty then
                        None
                    else
                        Some(linkId, assignments |> List.map fst)
                )
                |> Map.ofList

            if conflictsByLink.IsEmpty then
                if missingLinks.IsEmpty then
                    Ok noChange
                else
                    assignProcessValueWithReservedIds Set.empty missingLinks draft session
            else
                let conflictCounts =
                    conflictsByLink |> Map.toList |> List.map (snd >> List.length) |> Set.ofList

                if not missingLinks.IsEmpty || conflictCounts.Count > 1 then
                    let countsByLink =
                        sameHeaderByLink |> Map.map (fun _ assignments -> assignments.Length)

                    Error(MixedPropertyValueCounts(preparation.PropertyDefinition.Id, countsByLink))
                else
                    let assignmentIds =
                        conflictsByLink
                        |> Map.toList
                        |> List.collect (snd >> List.map _.Id)
                        |> Set.ofList

                    if conflictCounts |> Set.exists (fun count -> count > 1) then
                        Error(MultiplePropertyValues(preparation.PropertyDefinition.Id, assignmentIds))
                    else
                        Error(OverwriteConfirmationRequired(preparation.PropertyDefinition.Id, assignmentIds))

let assignProcessValue
    (linkIds: Set<ProcessLinkId>)
    (draft: ProcessAssignmentDraft)
    (session: ProvenanceSession)
    : Result<CommandEffect, ProvenanceCommandError> =
    if
        draft.ContainerReferenceValueId.IsSome
        || (
            match draft.Content.Value with
            | ProvenanceValue.Reference _ -> true
            | _ -> false
        )
    then
        Error ReadOnlyAdapterResourceMutation
    else
        assignProcessValueWithoutOverwrite linkIds draft session

let assignCatalogProcessValue
    (linkIds: Set<ProcessLinkId>)
    (catalog: ReferenceCatalog)
    (entry: ReferenceCatalogEntry)
    (session: ProvenanceSession)
    : Result<CommandEffect, ProvenanceCommandError> =
    let catalogEntry =
        tryFindCatalogEntry entry.Reference.Scheme entry.Reference.Id catalog

    if catalogEntry.IsNone then
        Error(InconsistentCanonicalState "The catalog does not contain the exact requested entry.")
    elif catalogEntry.Value.AssignmentKind <> AnnotationOwnerKind.Process then
        Error(InconsistentCanonicalState "A node catalog entry cannot be assigned to structural processes.")
    elif
        catalogEntry.Value.DependentProcessValues
        |> List.groupBy _.Key
        |> List.exists (fun (_, dependents) -> dependents.Length > 1)
    then
        Error(InconsistentCanonicalState "Catalog dependent process value keys must be unique.")
    else
        let entry = catalogEntry.Value
        let reservedAssignmentIds = allAssignmentIds session

        let slot =
            match entry.Cardinality with
            | ReferenceCardinality.Many -> None
            | ReferenceCardinality.AtMostOnePerLink slotId -> Some slotId

        let referenceDraft = {
            Content = {
                Category = entry.Category
                Value = ProvenanceValue.Reference entry.Reference
                Unit = entry.Unit
            }
            OwnerKind = AnnotationOwnerKind.Process
            PropertyKind = entry.PropertyKind
            ContainerReferenceValueId = None
            ReferenceSlotId = slot
            Lineage = AssignmentLineage.Created
        }

        let referenceValueId = (promoteCatalogEntry entry session).ValueDefinition.Id

        match assignProcessValueWithoutOverwrite linkIds referenceDraft session with
        | Error error -> Error error
        | Ok referenceEffect ->
            let mutable effects = [ referenceEffect ]

            let mutable currentSession =
                match referenceEffect with
                | NoChange -> session
                | Changed(_, content, _) -> sessionWithContent content session

            let mutable error = None

            for dependent in entry.DependentProcessValues do
                if error.IsNone then
                    let dependentDraft = {
                        Content = {
                            Category = dependent.Category
                            Value = dependent.Value
                            Unit = dependent.Unit
                        }
                        OwnerKind = AnnotationOwnerKind.Process
                        PropertyKind = dependent.PropertyKind
                        ContainerReferenceValueId = Some referenceValueId
                        ReferenceSlotId = None
                        Lineage =
                            AssignmentLineage.DerivedFromCatalog(
                                entry.Reference.Scheme,
                                entry.Reference.Id,
                                dependent.Key
                            )
                    }

                    match
                        assignProcessValueWithReservedIds reservedAssignmentIds linkIds dependentDraft currentSession
                    with
                    | Error commandError -> error <- Some commandError
                    | Ok effect ->
                        effects <- effects @ [ effect ]

                        match effect with
                        | NoChange -> ()
                        | Changed(_, content, _) -> currentSession <- sessionWithContent content currentSession

            match error with
            | Some commandError -> Error commandError
            | None ->
                let combined = combineEffects None effects session

                match combined with
                | NoChange -> Ok noChange
                | Changed(classification, content, mutations) ->
                    let originalIds = allAssignmentIds session

                    let addedDependentsByOwnerAndContainer =
                        content.Processes
                        |> Map.toList
                        |> List.collect (fun (processId, structuralProcess) ->
                            structuralProcess.Assignments
                            |> Map.toList
                            |> List.map snd
                            |> List.choose (fun assignment ->
                                if
                                    not (originalIds.Contains assignment.Id)
                                    && assignment.ContainerReferenceValueId.IsSome
                                then
                                    Some((processId, assignment.ContainerReferenceValueId.Value), assignment)
                                else
                                    None
                            )
                        )
                        |> List.groupBy fst
                        |> List.map (fun (key, assignments) -> key, assignments |> List.map snd)
                        |> Map.ofList

                    let journal =
                        mutations
                        |> List.map (
                            function
                            | AdapterResourceReferenceReplaced(ownerId, before, after, removedDependents, _, context) ->
                                AdapterResourceReferenceReplaced(
                                    ownerId,
                                    before,
                                    after,
                                    removedDependents,
                                    addedDependentsByOwnerAndContainer
                                    |> Map.tryFind (ownerId, after.ValueId)
                                    |> Option.defaultValue [],
                                    context
                                )
                            | mutation -> mutation
                        )

                    Ok(Changed(classification, content, journal))

let private validateProcessAssignmentOwnership
    ownerId
    assignmentId
    (structuralProcess: StructuralProcess)
    (assignment: ProcessAssignment)
    =
    if structuralProcess.Id <> ownerId then
        Error(
            InconsistentCanonicalState
                $"Structural process map key '{ownerId}' does not match embedded ID '{structuralProcess.Id}'."
        )
    elif assignment.Id <> assignmentId then
        Error(
            InconsistentCanonicalState
                $"Process assignment map key '{assignmentId}' does not match embedded ID '{assignment.Id}'."
        )
    elif assignment.CoveredLinkIds.IsEmpty then
        Error(InconsistentCanonicalState $"Process assignment '{assignmentId}' must cover at least one link.")
    else
        match
            assignment.CoveredLinkIds
            |> Seq.tryFind (fun linkId -> structuralProcess.Links |> Map.containsKey linkId |> not)
        with
        | Some linkId ->
            Error(
                InconsistentCanonicalState
                    $"Process assignment '{assignmentId}' covers link '{linkId}' outside structural process '{ownerId}'."
            )
        | None -> Ok()

let private validateProcessAssignment ownerId assignmentId (session: ProvenanceSession) =
    match session.Processes |> Map.tryFind ownerId with
    | None -> Error(ProcessNotFound ownerId)
    | Some structuralProcess ->
        match structuralProcess.Assignments |> Map.tryFind assignmentId with
        | None -> Error(AssignmentNotFound(Some(ProcessAssignmentOwner ownerId), assignmentId))
        | Some assignment ->
            match validateProcessAssignmentOwnership ownerId assignmentId structuralProcess assignment with
            | Error error -> Error error
            | Ok() ->
                match session.Values |> Map.tryFind assignment.ValueId with
                | None -> Error(ValueNotFound assignment.ValueId)
                | Some beforeValue ->
                    match session.Properties |> Map.tryFind beforeValue.PropertyId with
                    | None -> Error(PropertyNotFound beforeValue.PropertyId)
                    | Some _ -> Ok(structuralProcess, assignment, beforeValue)

let private validateSelectedProcessLinks
    (structuralProcess: StructuralProcess)
    (assignment: ProcessAssignment)
    selectedLinkIds
    =
    match
        selectedLinkIds
        |> Seq.tryFind (fun linkId -> structuralProcess.Links |> Map.containsKey linkId |> not)
    with
    | Some linkId -> Error(LinkNotFound linkId)
    | None ->
        match
            selectedLinkIds
            |> Seq.tryFind (fun linkId -> assignment.CoveredLinkIds.Contains linkId |> not)
        with
        | Some linkId -> Error(LinkNotFound linkId)
        | None -> Ok()

let editProcessAssignment
    (ownerId: StructuralProcessId)
    (assignmentId: AnnotationAssignmentId)
    (content: NodeValueContent)
    (session: ProvenanceSession)
    : Result<CommandEffect, ProvenanceCommandError> =
    match validateProcessAssignment ownerId assignmentId session with
    | Error error -> Error error
    | Ok(_, assignment, beforeValue) when
        assignment.ContainerReferenceValueId.IsSome
        || (
            match beforeValue.Value with
            | ProvenanceValue.Reference _ -> true
            | _ -> false
        )
        ->
        Error ReadOnlyAdapterResourceMutation
    | Ok(structuralProcess, assignment, beforeValue) ->
        let preparation =
            ensureValueDefinition content.Category content.Value content.Unit session

        if semanticallyMatchesPreparation preparation beforeValue session then
            Ok noChange
        else
            let referenceCount =
                valueDefinitionReferenceCounts session
                |> Map.tryFind assignment.ValueId
                |> Option.defaultValue 0

            let targetAlreadyExists =
                preparation.ValueDefinition.Id <> beforeValue.Id
                && session.Values |> Map.containsKey preparation.ValueDefinition.Id

            let context =
                processCommandContext (Set.singleton ownerId) (Set.singleton assignmentId) assignment.CoveredLinkIds

            let resultingSession, mutation =
                if targetAlreadyExists || referenceCount > 1 then
                    let after = {
                        assignment with
                            ValueId = preparation.ValueDefinition.Id
                    }

                    session
                    |> installPreparation preparation
                    |> updateProcess {
                        structuralProcess with
                            Assignments = structuralProcess.Assignments |> Map.add after.Id after
                    }
                    |> cleanupValueAndProperty assignment.ValueId,
                    ProcessAssignmentValueChanged(ownerId, assignment, after, context)
                else
                    let afterValue = {
                        beforeValue with
                            PropertyId = preparation.PropertyDefinition.Id
                            Value = content.Value
                            Unit = content.Unit
                    }

                    let updated = {
                        session with
                            Properties =
                                session.Properties
                                |> Map.add preparation.PropertyDefinition.Id preparation.PropertyDefinition
                            Values = session.Values |> Map.add afterValue.Id afterValue
                    }

                    let updated =
                        if beforeValue.PropertyId = afterValue.PropertyId then
                            updated
                        elif
                            updated.Values
                            |> Map.exists (fun _ value -> value.PropertyId = beforeValue.PropertyId)
                        then
                            updated
                        else
                            {
                                updated with
                                    Properties = updated.Properties |> Map.remove beforeValue.PropertyId
                            }

                    updated, PropertyValueDefinitionUpdated(beforeValue, afterValue, context)

            Ok(value resultingSession [ mutation ])

let editProcessAssignmentSubset
    (ownerId: StructuralProcessId)
    (assignmentId: AnnotationAssignmentId)
    (selectedLinkIds: Set<ProcessLinkId>)
    (content: NodeValueContent)
    (session: ProvenanceSession)
    : Result<CommandEffect, ProvenanceCommandError> =
    if selectedLinkIds.IsEmpty then
        Error EmptyTarget
    else
        match validateProcessAssignment ownerId assignmentId session with
        | Error error -> Error error
        | Ok(_, assignment, beforeValue) when
            assignment.ContainerReferenceValueId.IsSome
            || (
                match beforeValue.Value with
                | ProvenanceValue.Reference _ -> true
                | _ -> false
            )
            ->
            Error ReadOnlyAdapterResourceMutation
        | Ok(structuralProcess, assignment, beforeValue) ->
            match validateSelectedProcessLinks structuralProcess assignment selectedLinkIds with
            | Error error -> Error error
            | Ok() when selectedLinkIds = assignment.CoveredLinkIds ->
                editProcessAssignment ownerId assignmentId content session
            | Ok() ->
                let preparation =
                    ensureValueDefinition content.Category content.Value content.Unit session

                if semanticallyMatchesPreparation preparation beforeValue session then
                    Ok noChange
                else
                    let splitId = nextAssignmentId (allAssignmentIds session)

                    let retained = {
                        assignment with
                            CoveredLinkIds = assignment.CoveredLinkIds - selectedLinkIds
                    }

                    let split = {
                        assignment with
                            Id = splitId
                            ValueId = preparation.ValueDefinition.Id
                            CoveredLinkIds = selectedLinkIds
                            Lineage = AssignmentLineage.DerivedFrom assignment.Id
                    }

                    let resultingSession =
                        session
                        |> installPreparation preparation
                        |> updateProcess {
                            structuralProcess with
                                Assignments =
                                    structuralProcess.Assignments
                                    |> Map.add retained.Id retained
                                    |> Map.add split.Id split
                        }

                    let context =
                        processCommandContext
                            (Set.singleton ownerId)
                            (Set.ofList [ retained.Id; split.Id ])
                            selectedLinkIds

                    Ok(
                        topologyAndValue resultingSession [
                            ProcessAssignmentSplit(ownerId, assignment, retained, split, context)
                        ]
                    )

let removeProcessAssignmentsByOwner
    (selections: Map<StructuralProcessId, Map<AnnotationAssignmentId, Set<ProcessLinkId>>>)
    (session: ProvenanceSession)
    : Result<CommandEffect, ProvenanceCommandError> =
    let flattened =
        selections
        |> Map.toList
        |> List.collect (fun (ownerId, assignments) ->
            assignments
            |> Map.toList
            |> List.map (fun (assignmentId, linkIds) -> ownerId, assignmentId, linkIds)
        )

    if
        flattened.IsEmpty
        || flattened |> List.exists (fun (_, _, linkIds) -> linkIds.IsEmpty)
    then
        Error EmptyTarget
    else
        let validated =
            flattened
            |> List.map (fun (ownerId, assignmentId, linkIds) ->
                match validateProcessAssignment ownerId assignmentId session with
                | Error error -> Error error
                | Ok(structuralProcess, assignment, _) ->
                    match validateSelectedProcessLinks structuralProcess assignment linkIds with
                    | Error error -> Error error
                    | Ok() ->
                        let dependentLinks =
                            if
                                assignment.ContainerReferenceValueId.IsNone
                                && isReferenceValue assignment.ValueId session
                            then
                                boundDependentLinks assignment.ValueId linkIds structuralProcess
                            else
                                Set.empty

                        match
                            validateExactReferenceBacking
                                structuralProcess
                                assignment.ValueId
                                (Some assignment.Id)
                                dependentLinks
                                session
                        with
                        | Some error -> Error error
                        | None -> Ok(ownerId, structuralProcess, assignment, linkIds)
            )

        match
            validated
            |> List.tryPick (
                function
                | Error error -> Some error
                | Ok _ -> None
            )
        with
        | Some error -> Error error
        | None ->
            let explicitPlans =
                validated
                |> List.choose (
                    function
                    | Ok plan -> Some plan
                    | Error _ -> None
                )

            if
                explicitPlans
                |> List.exists (fun (_, _, assignment, _) -> assignment.ContainerReferenceValueId.IsSome)
            then
                Error ReadOnlyAdapterResourceMutation
            else
                let cascadedSelections =
                    explicitPlans
                    |> List.collect (fun (ownerId, structuralProcess, assignment, linkIds) ->
                        let dependentSelections =
                            if isReferenceValue assignment.ValueId session then
                                structuralProcess.Assignments
                                |> Map.toList
                                |> List.map snd
                                |> List.choose (fun dependentAssignment ->
                                    if dependentAssignment.ContainerReferenceValueId = Some assignment.ValueId then
                                        let dependentLinks =
                                            dependentAssignment.CoveredLinkIds |> Set.intersect linkIds

                                        if dependentLinks.IsEmpty then
                                            None
                                        else
                                            Some(ownerId, dependentAssignment.Id, dependentLinks)
                                    else
                                        None
                                )
                            else
                                []

                        (ownerId, assignment.Id, linkIds) :: dependentSelections
                    )
                    |> List.groupBy (fun (ownerId, assignmentId, _) -> ownerId, assignmentId)
                    |> List.map (fun ((ownerId, assignmentId), grouped) ->
                        let links =
                            grouped
                            |> List.collect (fun (_, _, selectedLinks) -> Set.toList selectedLinks)
                            |> Set.ofList

                        let structuralProcess = session.Processes[ownerId]
                        ownerId, structuralProcess, structuralProcess.Assignments[assignmentId], links
                    )

                let plans = cascadedSelections

                let ownerIds = plans |> List.map (fun (ownerId, _, _, _) -> ownerId) |> Set.ofList

                let assignmentIds =
                    plans |> List.map (fun (_, _, assignment, _) -> assignment.Id) |> Set.ofList

                let removedLinks =
                    plans |> List.collect (fun (_, _, _, links) -> Set.toList links) |> Set.ofList

                let cleanupCandidateIds =
                    plans
                    |> List.collect (fun (_, _, assignment, _) -> [
                        yield assignment.ValueId
                        yield! assignment.ContainerReferenceValueId |> Option.toList
                    ])
                    |> Set.ofList

                let context = processCommandContext ownerIds assignmentIds removedLinks
                let mutable resultingSession = session
                let mutable journal = []

                for ownerId, _, assignment, linkIds in plans do
                    let currentProcess = resultingSession.Processes[ownerId]
                    let remainder = assignment.CoveredLinkIds - linkIds

                    if remainder.IsEmpty then
                        resultingSession <-
                            resultingSession
                            |> updateProcess {
                                currentProcess with
                                    Assignments = currentProcess.Assignments |> Map.remove assignment.Id
                            }

                        journal <-
                            ProcessAssignmentRemoved(
                                {
                                    OwnerId = ownerId
                                    Assignment = assignment
                                },
                                context
                            )
                            :: journal
                    else
                        let after = {
                            assignment with
                                CoveredLinkIds = remainder
                        }

                        resultingSession <-
                            resultingSession
                            |> updateProcess {
                                currentProcess with
                                    Assignments = currentProcess.Assignments |> Map.add after.Id after
                            }

                        journal <- ProcessAssignmentCoverageChanged(ownerId, assignment, after, context) :: journal

                resultingSession <-
                    cleanupCandidateIds
                    |> Set.fold (fun current valueId -> cleanupValueAndProperty valueId current) resultingSession

                Ok(topology resultingSession (List.rev journal))

let removeProcessAssignmentLinks ownerId assignmentId linkIds session =
    removeProcessAssignmentsByOwner (Map.ofList [ ownerId, Map.ofList [ assignmentId, linkIds ] ]) session

let removeNodeAssignmentsByOwner
    (selections: Map<CanonicalNodeId, Set<AnnotationAssignmentId>>)
    (session: ProvenanceSession)
    : Result<CommandEffect, ProvenanceCommandError> =
    let flattened =
        selections
        |> Map.toList
        |> List.collect (fun (ownerId, assignmentIds) ->
            assignmentIds
            |> Set.toList
            |> List.map (fun assignmentId -> ownerId, assignmentId)
        )

    if flattened.IsEmpty then
        Error EmptyTarget
    else
        let validated =
            flattened
            |> List.map (fun (ownerId, assignmentId) ->
                match session.Nodes |> Map.tryFind ownerId with
                | None -> Error(NodeNotFound ownerId)
                | Some node ->
                    match node.Assignments |> Map.tryFind assignmentId with
                    | None -> Error(AssignmentNotFound(Some(NodeAssignmentOwner ownerId), assignmentId))
                    | Some assignment ->
                        if session.Values |> Map.containsKey assignment.ValueId then
                            Ok(ownerId, assignment)
                        else
                            Error(ValueNotFound assignment.ValueId)
            )

        match
            validated
            |> List.tryPick (
                function
                | Error error -> Some error
                | Ok _ -> None
            )
        with
        | Some error -> Error error
        | None ->
            let plans =
                validated
                |> List.choose (
                    function
                    | Ok plan -> Some plan
                    | Error _ -> None
                )

            let owners = plans |> List.map (fst >> NodeAssignmentOwner) |> Set.ofList
            let assignmentIds = plans |> List.map (snd >> _.Id) |> Set.ofList
            let cleanupCandidateIds = plans |> List.map (snd >> _.ValueId) |> Set.ofList
            let context = mixedOwnerCommandContext owners assignmentIds
            let mutable resultingSession = session
            let mutable journal = []

            for ownerId, assignment in plans do
                let node = resultingSession.Nodes[ownerId]

                resultingSession <-
                    resultingSession
                    |> updateNode {
                        node with
                            Assignments = node.Assignments |> Map.remove assignment.Id
                    }

                journal <-
                    NodeAssignmentRemoved(
                        {
                            OwnerId = ownerId
                            Assignment = assignment
                        },
                        context
                    )
                    :: journal

            resultingSession <-
                cleanupCandidateIds
                |> Set.fold (fun current valueId -> cleanupValueAndProperty valueId current) resultingSession

            Ok(topology resultingSession (List.rev journal))

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

let removeReferenceValueGlobally
    (valueId: PropertyValueDefinitionId)
    (session: ProvenanceSession)
    : Result<CommandEffect, ProvenanceCommandError> =
    match session.Values |> Map.tryFind valueId with
    | None -> Error(ValueNotFound valueId)
    | Some { Value = ProvenanceValue.Reference _ } ->
        let processSelections =
            session.Processes
            |> Map.toList
            |> List.choose (fun (processId, structuralProcess) ->
                let selected =
                    structuralProcess.Assignments
                    |> Map.toList
                    |> List.choose (fun (assignmentId, assignment) ->
                        if assignment.ValueId = valueId then
                            Some(assignmentId, assignment.CoveredLinkIds)
                        else
                            None
                    )
                    |> Map.ofList

                if selected.IsEmpty then None else Some(processId, selected)
            )
            |> Map.ofList

        let globalBackingError =
            session.Processes
            |> Map.toList
            |> List.tryPick (fun (_, structuralProcess) ->
                structuralProcess.Assignments
                |> Map.toList
                |> List.map snd
                |> List.filter (fun assignment -> assignment.ContainerReferenceValueId = Some valueId)
                |> List.tryPick (fun dependentAssignment ->
                    validateExactReferenceBacking
                        structuralProcess
                        valueId
                        None
                        dependentAssignment.CoveredLinkIds
                        session
                )
            )

        let nodeSelections =
            session.Nodes
            |> Map.toList
            |> List.choose (fun (nodeId, node) ->
                let selected =
                    node.Assignments
                    |> Map.toList
                    |> List.choose (fun (assignmentId, assignment) ->
                        if assignment.ValueId = valueId then
                            Some assignmentId
                        else
                            None
                    )
                    |> Set.ofList

                if selected.IsEmpty then None else Some(nodeId, selected)
            )
            |> Map.ofList

        if globalBackingError.IsSome then
            Error globalBackingError.Value
        elif processSelections.IsEmpty && nodeSelections.IsEmpty then
            let value = session.Values[valueId]

            let context = {
                Scope = GlobalDefinition
                Coverage = {
                    AssignmentIds = Set.empty
                    LinkIds = Set.empty
                }
            }

            let resultingSession = cleanupValueAndProperty valueId session
            Ok(topology resultingSession [ PropertyValueDefinitionDeleted(value, [], context) ])
        else
            let mutable currentSession = session
            let mutable effects = []
            let mutable error = None

            if not processSelections.IsEmpty then
                match removeProcessAssignmentsByOwner processSelections currentSession with
                | Error commandError -> error <- Some commandError
                | Ok effect ->
                    effects <- effects @ [ effect ]

                    match effect with
                    | NoChange -> ()
                    | Changed(_, content, _) -> currentSession <- sessionWithContent content currentSession

            if error.IsNone && not nodeSelections.IsEmpty then
                match removeNodeAssignmentsByOwner nodeSelections currentSession with
                | Error commandError -> error <- Some commandError
                | Ok effect ->
                    effects <- effects @ [ effect ]

                    match effect with
                    | NoChange -> ()
                    | Changed(_, content, _) -> currentSession <- sessionWithContent content currentSession

            match error with
            | Some commandError -> Error commandError
            | None -> Ok(combineEffects (Some GlobalDefinition) effects session)
    | Some _ -> Error(InconsistentCanonicalState $"Value '{valueId}' is not a reference value.")

let private allProcessLinkIds (session: ProvenanceSession) =
    session.Processes
    |> Map.toSeq
    |> Seq.collect (snd >> _.Links >> Map.keys)
    |> Set.ofSeq

let private nextStructuralProcessId usedIds =
    Seq.initInfinite (fun index -> $"structural-process-{index + 1}")
    |> Seq.find (fun candidate -> usedIds |> Set.contains candidate |> not)

let private nextProcessLinkId usedIds =
    Seq.initInfinite (fun index -> $"process-link-{index + 1}")
    |> Seq.find (fun candidate -> usedIds |> Set.contains candidate |> not)

let private removeProcessFromLayer processId layerId (session: ProvenanceSession) =
    let layers =
        session.Layers
        |> Map.change
            layerId
            (Option.map (fun layer -> {
                layer with
                    StructuralProcessIds = layer.StructuralProcessIds |> Set.remove processId
            }))

    {
        session with
            Processes = session.Processes |> Map.remove processId
            Layers = layers
    }

let addEndpoint
    (layerId: ProvenanceLayerId)
    (side: ProvenanceSide)
    (kind: ProvenanceKind)
    (header: ProvenanceIOHeader)
    (name: string)
    (layerOrderPosition: int)
    (session: ProvenanceSession)
    : Result<CommandEffect, ProvenanceCommandError> =
    match session.Layers |> Map.tryFind layerId with
    | None -> Error(LayerNotFound layerId)
    | Some layer ->
        let key = canonicalKey kind name

        let existingNodeId =
            session.Nodes
            |> Map.tryPick (fun nodeId node -> if node.Key = key then Some nodeId else None)

        let duplicate =
            existingNodeId
            |> Option.exists (fun nodeId ->
                match side with
                | ProvenanceSide.Input -> layer.InputEndpoints |> Map.containsKey nodeId
                | ProvenanceSide.Output -> layer.OutputEndpoints |> Map.containsKey nodeId
            )

        if duplicate then
            Error(
                DuplicateEndpointAppearance {
                    LayerId = layerId
                    Side = side
                    NodeId = existingNodeId.Value
                }
            )
        else
            let nodeId, withNode = ensureNode kind name session
            let nodeWasCreated = existingNodeId.IsNone

            let endpoint = {
                Key = {
                    LayerId = layerId
                    Side = side
                    NodeId = nodeId
                }
                Header = header
                LayerOrderPosition = layerOrderPosition
            }

            let processId =
                nextStructuralProcessId (withNode.Processes |> Map.keys |> Set.ofSeq)

            let linkId = nextProcessLinkId (allProcessLinkIds withNode)

            let processLink = {
                Id = linkId
                Shape =
                    match side with
                    | ProvenanceSide.Input -> ProcessLinkShape.InputOnly nodeId
                    | ProvenanceSide.Output -> ProcessLinkShape.OutputOnly nodeId
            }

            let structuralProcess = {
                Id = processId
                OriginLayerId = layerId
                Name = None
                Links = Map.ofList [ processLink.Id, processLink ]
                Assignments = Map.empty
            }

            match addLayerEndpoint endpoint withNode with
            | Error error -> Error error
            | Ok withEndpoint ->
                match addProcess structuralProcess withEndpoint with
                | Error error -> Error error
                | Ok resultingSession ->
                    let mutations = [
                        if nodeWasCreated then
                            CanonicalNodeCreated resultingSession.Nodes[nodeId]

                        LayerEndpointAdded endpoint
                        StructuralProcessCreated structuralProcess
                        ProcessLinkAdded(processId, processLink)
                    ]

                    Ok(topology resultingSession mutations)

let private nextLayerIndex (session: ProvenanceSession) =
    let rec loop index =
        if session.Layers |> Map.containsKey $"layer-{index}" then
            loop (index + 1)
        else
            index

    loop 1

/// Seeds a new layer from a selection in the active layer. The old model copied
/// each selected set into the new layer and joined them with a
/// `ProvenanceReferenceLink`; canonically the *same* canonical node simply gains
/// an appearance in the new layer, so nothing is copied, no annotations are
/// duplicated, and no reference link exists to reconcile. An empty selection
/// seeds from the active layer's outputs, as before.
let addLayer
    (name: string)
    (selected: (ProvenanceSide * CanonicalNodeId) list)
    (session: ProvenanceSession)
    : Result<CommandEffect, ProvenanceCommandError> =
    match session.Layers |> Map.tryFind session.ActiveLayerId with
    | None -> Error(LayerNotFound session.ActiveLayerId)
    | Some current ->
        let seeds =
            match selected with
            | [] ->
                current.OutputEndpoints
                |> Map.toList
                |> List.sortBy (fun (nodeId, endpoint) -> endpoint.LayerOrderPosition, nodeId)
                |> List.map (fun (nodeId, _) -> ProvenanceSide.Output, nodeId)
            | selected -> selected

        // Every seed must be an appearance of the active layer on that side, so a
        // stale selection cannot fabricate an appearance for an absent node.
        let missing =
            seeds
            |> List.tryPick (fun (side, nodeId) ->
                let appearances =
                    match side with
                    | ProvenanceSide.Input -> current.InputEndpoints
                    | ProvenanceSide.Output -> current.OutputEndpoints

                if appearances |> Map.containsKey nodeId then
                    None
                else
                    Some nodeId
            )

        match missing with
        | Some nodeId -> Error(NodeNotFound nodeId)
        | None ->
            let layerId = $"layer-{nextLayerIndex session}"

            // The id is namespaced with the layer id so two layers added under the
            // same entered name never collide: source colours and process origin
            // sources are keyed by Source.Id.
            let source: ProvenanceSourceRef = {
                Id = $"{layerId}:{name}"
                Name = name
            }

            // Seeds arrive on the new layer's input side, keeping their source
            // appearance's header and taking rail order from the seed order.
            let inputEndpoints =
                seeds
                |> List.distinctBy snd
                |> List.mapi (fun position (side, nodeId) ->
                    let sourceEndpoint =
                        match side with
                        | ProvenanceSide.Input -> current.InputEndpoints[nodeId]
                        | ProvenanceSide.Output -> current.OutputEndpoints[nodeId]

                    nodeId,
                    {
                        Key = {
                            LayerId = layerId
                            Side = ProvenanceSide.Input
                            NodeId = nodeId
                        }
                        Header = sourceEndpoint.Header
                        LayerOrderPosition = position
                    }
                )

            let layer = {
                Id = layerId
                Label = name
                Source = source
                InputEndpoints = inputEndpoints |> Map.ofList
                OutputEndpoints = Map.empty
                StructuralProcessIds = Set.empty
            }

            let resultingSession = {
                session with
                    Layers = session.Layers |> Map.add layer.Id layer
                    LayerOrder = session.LayerOrder @ [ layer.Id ]
                    ActiveLayerId = layer.Id
            }

            // New appearances change reachability projections, so this is a
            // topology change; there is no `LayerCreated` mutation because a
            // layer's membership *is* its appearances.
            Ok(topology resultingSession (inputEndpoints |> List.map (snd >> LayerEndpointAdded)))

type private OneSidedPromotionCandidate = {
    ProcessId: StructuralProcessId
    Process: StructuralProcess
    Link: ProcessLink
    IsEditorCreated: bool
}

type private PromotionPlan = {
    Candidate: OneSidedPromotionCandidate
    Pairs: (CanonicalNodeId * CanonicalNodeId) list
}

let private candidateMatchesPair candidate (inputId, outputId) =
    match candidate.Link.Shape with
    | ProcessLinkShape.InputOnly candidateInput -> candidateInput = inputId
    | ProcessLinkShape.OutputOnly candidateOutput -> candidateOutput = outputId
    | _ -> false

let private pairAlreadyExists inputId outputId (session: ProvenanceSession) =
    session.Processes
    |> Map.exists (fun _ structuralProcess ->
        structuralProcess.Links
        |> Map.exists (fun _ processLink -> processLink.Shape = ProcessLinkShape.Between(inputId, outputId))
    )

let private processWasCreatedByEditor processId (session: ProvenanceSession) =
    session.MutationJournal
    |> List.exists (
        function
        | StructuralProcessCreated structuralProcess -> structuralProcess.Id = processId
        | _ -> false
    )

let connectNodes
    (layerId: ProvenanceLayerId)
    (pairs: (CanonicalNodeId * CanonicalNodeId) list)
    (session: ProvenanceSession)
    : Result<CommandEffect, ProvenanceCommandError> =
    if pairs.IsEmpty then
        Error EmptyTarget
    else
        match session.Layers |> Map.tryFind layerId with
        | None -> Error(LayerNotFound layerId)
        | Some layer ->
            let distinctPairs = pairs |> List.distinct

            let missingNode =
                distinctPairs
                |> List.collect (fun (inputId, outputId) -> [ inputId; outputId ])
                |> List.tryFind (fun nodeId -> session.Nodes |> Map.containsKey nodeId |> not)

            let missingAppearance =
                distinctPairs
                |> List.tryPick (fun (inputId, outputId) ->
                    if layer.InputEndpoints |> Map.containsKey inputId |> not then
                        Some inputId
                    elif layer.OutputEndpoints |> Map.containsKey outputId |> not then
                        Some outputId
                    else
                        None
                )

            match missingNode |> Option.orElse missingAppearance with
            | Some nodeId -> Error(NodeNotFound nodeId)
            | None ->
                let orderedPairs =
                    distinctPairs
                    |> List.filter (fun (inputId, outputId) -> pairAlreadyExists inputId outputId session |> not)
                    |> List.sortBy (fun (inputId, outputId) ->
                        layer.InputEndpoints[inputId].LayerOrderPosition,
                        layer.OutputEndpoints[outputId].LayerOrderPosition,
                        inputId,
                        outputId
                    )

                if orderedPairs.IsEmpty then
                    Ok noChange
                else
                    let candidates =
                        layer.StructuralProcessIds
                        |> Seq.choose (fun processId ->
                            session.Processes
                            |> Map.tryFind processId
                            |> Option.bind (fun structuralProcess ->
                                if structuralProcess.Links.Count <> 1 then
                                    None
                                else
                                    let processLink = structuralProcess.Links |> Map.toSeq |> Seq.exactlyOne |> snd

                                    match processLink.Shape with
                                    | ProcessLinkShape.InputOnly _
                                    | ProcessLinkShape.OutputOnly _ ->
                                        Some {
                                            ProcessId = processId
                                            Process = structuralProcess
                                            Link = processLink
                                            IsEditorCreated = processWasCreatedByEditor processId session
                                        }
                                    | _ -> None
                            )
                        )
                        |> Seq.sortBy (fun candidate ->
                            (if candidate.IsEditorCreated then 1 else 0),
                            (match candidate.Link.Shape with
                             | ProcessLinkShape.InputOnly _ -> 0
                             | _ -> 1),
                            candidate.ProcessId,
                            candidate.Link.Id
                        )
                        |> Seq.toList

                    let mutable plans: PromotionPlan list = []
                    let mutable unpromotedPairs = []
                    let mutable claimedCandidateIds = Set.empty

                    for pair in orderedPairs do
                        match plans |> List.tryFind (fun plan -> candidateMatchesPair plan.Candidate pair) with
                        | Some existing ->
                            plans <-
                                plans
                                |> List.map (fun plan ->
                                    if plan.Candidate.ProcessId = existing.Candidate.ProcessId then
                                        {
                                            plan with
                                                Pairs = plan.Pairs @ [ pair ]
                                        }
                                    else
                                        plan
                                )
                        | None ->
                            let candidate =
                                candidates
                                |> List.tryFind (fun item ->
                                    claimedCandidateIds |> Set.contains item.ProcessId |> not
                                    && candidateMatchesPair item pair
                                )

                            match candidate with
                            | Some item ->
                                claimedCandidateIds <- claimedCandidateIds |> Set.add item.ProcessId

                                plans <- plans @ [ { Candidate = item; Pairs = [ pair ] } ]
                            | None -> unpromotedPairs <- unpromotedPairs @ [ pair ]

                    let mutable usedProcessIds = session.Processes |> Map.keys |> Set.ofSeq

                    let mutable usedLinkIds = allProcessLinkIds session
                    let mutable resultingSession = session
                    let mutable mutations = []

                    for plan in plans do
                        let before = resultingSession.Processes[plan.Candidate.ProcessId]
                        let firstPair = plan.Pairs.Head

                        let retainedLink = {
                            plan.Candidate.Link with
                                Shape = ProcessLinkShape.Between firstPair
                        }

                        let mutable links = before.Links |> Map.add retainedLink.Id retainedLink

                        let mutable addedLinks = []

                        for pair in plan.Pairs.Tail do
                            let linkId = nextProcessLinkId usedLinkIds
                            usedLinkIds <- usedLinkIds |> Set.add linkId

                            let processLink = {
                                Id = linkId
                                Shape = ProcessLinkShape.Between pair
                            }

                            links <- links |> Map.add processLink.Id processLink
                            addedLinks <- addedLinks @ [ processLink ]

                        let after = { before with Links = links }

                        resultingSession <- resultingSession |> updateProcess after
                        mutations <- mutations @ [ StructuralProcessReshaped(before, after) ]

                        mutations <-
                            mutations
                            @ (addedLinks
                               |> List.map (fun processLink -> ProcessLinkAdded(after.Id, processLink)))

                    for inputId, outputId in unpromotedPairs do
                        let processId = nextStructuralProcessId usedProcessIds
                        usedProcessIds <- usedProcessIds |> Set.add processId
                        let linkId = nextProcessLinkId usedLinkIds
                        usedLinkIds <- usedLinkIds |> Set.add linkId

                        let processLink = {
                            Id = linkId
                            Shape = ProcessLinkShape.Between(inputId, outputId)
                        }

                        let structuralProcess = {
                            Id = processId
                            OriginLayerId = layerId
                            Name = None
                            Links = Map.ofList [ processLink.Id, processLink ]
                            Assignments = Map.empty
                        }

                        match addProcess structuralProcess resultingSession with
                        | Error error -> failwithf "Prevalidated structural process creation failed: %A" error
                        | Ok updated -> resultingSession <- updated

                        mutations <-
                            mutations
                            @ [
                                StructuralProcessCreated structuralProcess
                                ProcessLinkAdded(processId, processLink)
                            ]

                    let connectedPairs = orderedPairs |> Set.ofList

                    let absorbable =
                        candidates
                        |> List.filter (fun candidate ->
                            claimedCandidateIds |> Set.contains candidate.ProcessId |> not
                            && candidate.IsEditorCreated
                            && candidate.Process.Assignments.IsEmpty
                            && candidate.Process.Links.Count = 1
                            && connectedPairs |> Seq.exists (candidateMatchesPair candidate)
                        )

                    for candidate in absorbable do
                        let context =
                            processCommandContext
                                (Set.singleton candidate.ProcessId)
                                Set.empty
                                (Set.singleton candidate.Link.Id)

                        resultingSession <-
                            resultingSession
                            |> removeProcessFromLayer candidate.ProcessId candidate.Process.OriginLayerId

                        mutations <-
                            mutations
                            @ [
                                ProcessLinkRemoved(candidate.ProcessId, candidate.Link, context)
                            ]

                    Ok(topology resultingSession mutations)

let private processLinkOwner linkId (session: ProvenanceSession) =
    session.Processes
    |> Map.toList
    |> List.choose (fun (processId, structuralProcess) ->
        structuralProcess.Links
        |> Map.tryFind linkId
        |> Option.map (fun processLink -> processId, structuralProcess, processLink)
    )

let private nodeHasIncidence nodeId (session: ProvenanceSession) =
    session.Processes
    |> Map.exists (fun _ structuralProcess ->
        structuralProcess.Links
        |> Map.exists (fun _ processLink ->
            match processLink.Shape with
            | ProcessLinkShape.Between(inputId, outputId) -> inputId = nodeId || outputId = nodeId
            | ProcessLinkShape.InputOnly inputId -> inputId = nodeId
            | ProcessLinkShape.OutputOnly outputId -> outputId = nodeId
            | ProcessLinkShape.Endpointless -> false
        )
    )

let disconnectLinks
    (linkIds: Set<ProcessLinkId>)
    (session: ProvenanceSession)
    : Result<CommandEffect, ProvenanceCommandError> =
    if linkIds.IsEmpty then
        Error EmptyTarget
    else
        let resolved =
            linkIds
            |> Seq.map (fun linkId ->
                match processLinkOwner linkId session with
                | [] -> Error(LinkNotFound linkId)
                | [ owner ] -> Ok owner
                | owners ->
                    Error(
                        InconsistentCanonicalState
                            $"Process link '{linkId}' is owned by {owners.Length} structural processes."
                    )
            )
            |> Seq.toList

        match
            resolved
            |> List.tryPick (
                function
                | Error error -> Some error
                | Ok _ -> None
            )
        with
        | Some error -> Error error
        | None ->
            let removals =
                resolved
                |> List.choose (
                    function
                    | Ok(processId, structuralProcess, processLink) ->
                        match processLink.Shape with
                        | ProcessLinkShape.Between(inputId, outputId) ->
                            Some(processId, structuralProcess, processLink, inputId, outputId)
                        | _ -> None
                    | Error _ -> None
                )

            if removals.Length <> linkIds.Count then
                Error(InconsistentCanonicalState "Only two-sided process links can be disconnected.")
            else
                let ownerIds =
                    removals |> List.map (fun (processId, _, _, _, _) -> processId) |> Set.ofList

                let affectedAssignments =
                    removals
                    |> List.collect (fun (_, structuralProcess, processLink, _, _) ->
                        structuralProcess.Assignments
                        |> Map.toList
                        |> List.map snd
                        |> List.filter (fun assignment -> assignment.CoveredLinkIds.Contains processLink.Id)
                    )
                    |> List.distinctBy _.Id

                let context =
                    processCommandContext ownerIds (affectedAssignments |> List.map _.Id |> Set.ofList) linkIds

                let mutable resultingSession = session
                let mutable mutations = []
                let mutable cleanupValueIds = Set.empty

                for processId in ownerIds do
                    let before = resultingSession.Processes[processId]

                    let removedForProcess =
                        removals
                        |> List.choose (fun (ownerId, _, processLink, _, _) ->
                            if ownerId = processId then Some processLink else None
                        )

                    let removedIds = removedForProcess |> List.map _.Id |> Set.ofList

                    let mutable assignments = before.Assignments

                    for KeyValue(assignmentId, assignment) in before.Assignments do
                        let removedCoverage = assignment.CoveredLinkIds |> Set.intersect removedIds

                        if not removedCoverage.IsEmpty then
                            cleanupValueIds <- cleanupValueIds |> Set.add assignment.ValueId

                            let remainder = assignment.CoveredLinkIds - removedCoverage

                            if remainder.IsEmpty then
                                assignments <- assignments |> Map.remove assignmentId

                                mutations <-
                                    mutations
                                    @ [
                                        ProcessAssignmentRemoved(
                                            {
                                                OwnerId = processId
                                                Assignment = assignment
                                            },
                                            context
                                        )
                                    ]
                            else
                                let after = {
                                    assignment with
                                        CoveredLinkIds = remainder
                                }

                                assignments <- assignments |> Map.add after.Id after

                                mutations <-
                                    mutations
                                    @ [
                                        ProcessAssignmentCoverageChanged(processId, assignment, after, context)
                                    ]

                    let afterRemoval = {
                        before with
                            Links =
                                removedIds
                                |> Set.fold (fun links linkId -> links |> Map.remove linkId) before.Links
                            Assignments = assignments
                    }

                    resultingSession <- resultingSession |> updateProcess afterRemoval

                    mutations <-
                        mutations
                        @ (removedForProcess
                           |> List.map (fun processLink -> ProcessLinkRemoved(processId, processLink, context)))

                let mutable usedLinkIds = allProcessLinkIds resultingSession

                for processId, _, removedLink, inputId, outputId in removals do
                    if nodeHasIncidence outputId resultingSession |> not then
                        let outputContinuation = {
                            Id = removedLink.Id
                            Shape = ProcessLinkShape.OutputOnly outputId
                        }

                        let structuralProcess = resultingSession.Processes[processId]

                        resultingSession <-
                            resultingSession
                            |> updateProcess {
                                structuralProcess with
                                    Links = structuralProcess.Links |> Map.add outputContinuation.Id outputContinuation
                            }

                        usedLinkIds <- usedLinkIds |> Set.add outputContinuation.Id

                        mutations <- mutations @ [ ProcessLinkAdded(processId, outputContinuation) ]

                    if nodeHasIncidence inputId resultingSession |> not then
                        let inputLinkId = nextProcessLinkId usedLinkIds
                        usedLinkIds <- usedLinkIds |> Set.add inputLinkId

                        let inputContinuation = {
                            Id = inputLinkId
                            Shape = ProcessLinkShape.InputOnly inputId
                        }

                        let structuralProcess = resultingSession.Processes[processId]

                        resultingSession <-
                            resultingSession
                            |> updateProcess {
                                structuralProcess with
                                    Links = structuralProcess.Links |> Map.add inputContinuation.Id inputContinuation
                            }

                        mutations <- mutations @ [ ProcessLinkAdded(processId, inputContinuation) ]

                resultingSession <-
                    cleanupValueIds
                    |> Set.fold (fun current valueId -> cleanupValueAndProperty valueId current) resultingSession

                Ok(topology resultingSession mutations)

let private assignmentsReferencingValueId valueId (session: ProvenanceSession) =
    let nodeOccurrences =
        session.Nodes
        |> Map.toList
        |> List.collect (fun (ownerId, node) ->
            node.Assignments
            |> Map.toList
            |> List.map snd
            |> List.choose (fun assignment ->
                if assignment.ValueId = valueId then
                    Some(NodeAssignmentOwner ownerId, assignment.Id, Set.empty)
                else
                    None
            )
        )

    let processOccurrences =
        session.Processes
        |> Map.toList
        |> List.collect (fun (ownerId, structuralProcess) ->
            structuralProcess.Assignments
            |> Map.toList
            |> List.map snd
            |> List.choose (fun assignment ->
                if
                    assignment.ValueId = valueId
                    || assignment.ContainerReferenceValueId = Some valueId
                then
                    Some(ProcessAssignmentOwner ownerId, assignment.Id, assignment.CoveredLinkIds)
                else
                    None
            )
        )

    nodeOccurrences @ processOccurrences

let editValueGlobally
    (valueId: PropertyValueDefinitionId)
    (content: NodeValueContent)
    (session: ProvenanceSession)
    : Result<CommandEffect, ProvenanceCommandError> =
    match session.Values |> Map.tryFind valueId with
    | None -> Error(ValueNotFound valueId)
    | Some beforeValue when
        (match beforeValue.Value with
         | ProvenanceValue.Reference _ -> true
         | _ -> false)
        || hasContainerBoundOccurrence valueId session
        ->
        Error ReadOnlyAdapterResourceMutation
    | Some beforeValue ->
        match session.Properties |> Map.tryFind beforeValue.PropertyId with
        | None -> Error(PropertyNotFound beforeValue.PropertyId)
        | Some _ ->
            let preparation =
                ensureValueDefinition content.Category content.Value content.Unit session

            if semanticallyMatchesPreparation preparation beforeValue session then
                Ok noChange
            else
                let occurrences = assignmentsReferencingValueId valueId session

                let context = {
                    Scope = GlobalDefinition
                    Coverage = {
                        AssignmentIds = occurrences |> List.map (fun (_, assignmentId, _) -> assignmentId) |> Set.ofList
                        LinkIds =
                            occurrences
                            |> List.collect (fun (_, _, linkIds) -> linkIds |> Set.toList)
                            |> Set.ofList
                    }
                }

                let afterValue = {
                    beforeValue with
                        PropertyId = preparation.PropertyDefinition.Id
                        Value = content.Value
                        Unit = content.Unit
                }

                let updated = {
                    session with
                        Properties =
                            session.Properties
                            |> Map.add preparation.PropertyDefinition.Id preparation.PropertyDefinition
                        Values = session.Values |> Map.add afterValue.Id afterValue
                }

                let resultingSession =
                    if beforeValue.PropertyId = afterValue.PropertyId then
                        updated
                    elif
                        updated.Values
                        |> Map.exists (fun _ value -> value.PropertyId = beforeValue.PropertyId)
                    then
                        updated
                    else
                        {
                            updated with
                                Properties = updated.Properties |> Map.remove beforeValue.PropertyId
                        }

                Ok(
                    value resultingSession [
                        PropertyValueDefinitionUpdated(beforeValue, afterValue, context)
                    ]
                )

type private GlobalDeletionTarget =
    | ValueDefinitions
    | PropertyDefinition of PropertyDefinition

let private removeDefinitionsGlobally
    (valueIds: Set<PropertyValueDefinitionId>)
    (target: GlobalDeletionTarget)
    (session: ProvenanceSession)
    : Result<CommandEffect, ProvenanceCommandError> =
    if valueIds.IsEmpty then
        Error EmptyTarget
    else
        match
            valueIds
            |> Seq.tryFind (fun valueId -> session.Values |> Map.containsKey valueId |> not)
        with
        | Some valueId -> Error(ValueNotFound valueId)
        | None when
            valueIds
            |> Set.exists (fun valueId ->
                isReferenceValue valueId session || hasContainerBoundOccurrence valueId session
            )
            ->
            Error ReadOnlyAdapterResourceMutation
        | None ->
            let nodeRemovals =
                session.Nodes
                |> Map.toList
                |> List.collect (fun (ownerId, node) ->
                    node.Assignments
                    |> Map.toList
                    |> List.map snd
                    |> List.choose (fun assignment ->
                        if valueIds |> Set.contains assignment.ValueId then
                            Some(ownerId, assignment)
                        else
                            None
                    )
                )

            let processRemovals =
                session.Processes
                |> Map.toList
                |> List.collect (fun (ownerId, structuralProcess) ->
                    structuralProcess.Assignments
                    |> Map.toList
                    |> List.map snd
                    |> List.choose (fun assignment ->
                        if
                            valueIds |> Set.contains assignment.ValueId
                            || assignment.ContainerReferenceValueId
                               |> Option.exists (fun containerId -> valueIds |> Set.contains containerId)
                        then
                            Some(ownerId, assignment)
                        else
                            None
                    )
                )

            let owners =
                [
                    yield! nodeRemovals |> List.map (fst >> NodeAssignmentOwner)
                    yield! processRemovals |> List.map (fst >> ProcessAssignmentOwner)
                ]
                |> Set.ofList

            let assignmentIds =
                [
                    yield! nodeRemovals |> List.map (snd >> _.Id)
                    yield! processRemovals |> List.map (snd >> _.Id)
                ]
                |> Set.ofList

            let linkIds =
                processRemovals
                |> List.collect (snd >> _.CoveredLinkIds >> Set.toList)
                |> Set.ofList

            let context = {
                Scope = GlobalDefinition
                Coverage = {
                    AssignmentIds = assignmentIds
                    LinkIds = linkIds
                }
            }

            let nodeTombstones: NodeAssignmentTombstone list =
                nodeRemovals
                |> List.map (fun (ownerId, assignment) -> {
                    OwnerId = ownerId
                    Assignment = assignment
                })

            let processTombstones: ProcessAssignmentTombstone list =
                processRemovals
                |> List.map (fun (ownerId, assignment) -> {
                    OwnerId = ownerId
                    Assignment = assignment
                })

            let allTombstones = [
                yield! nodeTombstones |> List.map NodeTombstone
                yield! processTombstones |> List.map ProcessTombstone
            ]

            let mutable resultingSession = session
            let mutable mutations = []

            for tombstone in nodeTombstones do
                let node = resultingSession.Nodes[tombstone.OwnerId]

                resultingSession <-
                    resultingSession
                    |> updateNode {
                        node with
                            Assignments = node.Assignments |> Map.remove tombstone.Assignment.Id
                    }

                mutations <- mutations @ [ NodeAssignmentRemoved(tombstone, context) ]

            for tombstone in processTombstones do
                let structuralProcess = resultingSession.Processes[tombstone.OwnerId]

                resultingSession <-
                    resultingSession
                    |> updateProcess {
                        structuralProcess with
                            Assignments = structuralProcess.Assignments |> Map.remove tombstone.Assignment.Id
                    }

                mutations <- mutations @ [ ProcessAssignmentRemoved(tombstone, context) ]

            let cleanupValueIds =
                [
                    yield! valueIds |> Set.toList

                    yield! nodeRemovals |> List.map (snd >> _.ValueId)

                    yield!
                        processRemovals
                        |> List.collect (fun (_, assignment) -> [
                            assignment.ValueId
                            yield! assignment.ContainerReferenceValueId |> Option.toList
                        ])
                ]
                |> Set.ofList

            resultingSession <-
                cleanupValueIds
                |> Set.fold
                    (fun current cleanupValueId -> cleanupValueAndProperty cleanupValueId current)
                    resultingSession

            let deletionMutation =
                match target with
                | ValueDefinitions ->
                    valueIds
                    |> Set.toList
                    |> List.map (fun removedValueId ->
                        let value = session.Values[removedValueId]

                        let relevantTombstones =
                            allTombstones
                            |> List.filter (
                                function
                                | NodeTombstone tombstone -> tombstone.Assignment.ValueId = removedValueId
                                | ProcessTombstone tombstone ->
                                    tombstone.Assignment.ValueId = removedValueId
                                    || tombstone.Assignment.ContainerReferenceValueId = Some removedValueId
                            )

                        PropertyValueDefinitionDeleted(value, relevantTombstones, context)
                    )
                | PropertyDefinition property ->
                    let values =
                        valueIds
                        |> Set.toList
                        |> List.map (fun removedValueId -> session.Values[removedValueId])

                    resultingSession <- {
                        resultingSession with
                            Properties = resultingSession.Properties |> Map.remove property.Id
                    }

                    [
                        PropertyDefinitionDeleted(property, values, allTombstones, context)
                    ]

            let allMutations = mutations @ deletionMutation

            if assignmentIds.IsEmpty then
                Ok(value resultingSession allMutations)
            else
                Ok(topologyAndValue resultingSession allMutations)

let removeValuesGlobally
    (valueIds: Set<PropertyValueDefinitionId>)
    (session: ProvenanceSession)
    : Result<CommandEffect, ProvenanceCommandError> =
    removeDefinitionsGlobally valueIds ValueDefinitions session

let removePropertyGlobally
    (propertyId: PropertyDefinitionId)
    (session: ProvenanceSession)
    : Result<CommandEffect, ProvenanceCommandError> =
    match session.Properties |> Map.tryFind propertyId with
    | None -> Error(PropertyNotFound propertyId)
    | Some property ->
        let valueIds =
            session.Values
            |> Map.toList
            |> List.choose (fun (valueId, value) -> if value.PropertyId = propertyId then Some valueId else None)
            |> Set.ofList

        if valueIds.IsEmpty then
            let resultingSession = {
                session with
                    Properties = session.Properties |> Map.remove propertyId
            }

            let context = {
                Scope = GlobalDefinition
                Coverage = {
                    AssignmentIds = Set.empty
                    LinkIds = Set.empty
                }
            }

            Ok(value resultingSession [ PropertyDefinitionDeleted(property, [], [], context) ])
        else
            removeDefinitionsGlobally valueIds (PropertyDefinition property) session

let private ambiguityEvidence (references: AvailableAnnotationRef list) =
    let originatingLinkIds =
        references
        |> List.fold (fun links reference -> Set.union links reference.OriginatingLinkIds) Set.empty

    let linkIds =
        if originatingLinkIds.IsEmpty then
            references
            |> List.fold (fun links reference -> Set.union links reference.VisibleThroughLinkIds) Set.empty
        else
            originatingLinkIds

    let assignmentIds = references |> List.map _.AssignmentId |> Set.ofList
    AmbiguousPooledEdit(linkIds, assignmentIds)

let private processBackingReferences (references: AvailableAnnotationRef list) =
    references
    |> List.collect (fun reference ->
        match reference.Owner with
        | NodeOwner _ -> []
        | ProcessOwner processId ->
            let linkIds =
                if reference.OriginatingLinkIds.IsEmpty then
                    match reference.Relation with
                    | IncidentProcess linkId -> Set.singleton linkId
                    | _ -> Set.empty
                else
                    reference.OriginatingLinkIds

            linkIds
            |> Set.toList
            |> List.map (fun linkId -> processId, reference.AssignmentId, linkId)
    )

/// Which surface an availability edit was issued from. The two differ only for
/// process annotations, and only because the surfaces mean different things.
type ProcessEditScope =
    /// One displayed connector. It must resolve to exactly one backing link:
    /// intent §4 keeps a pooled connector ambiguous "even if every reference
    /// currently points to the same assignment ID", because the user cannot
    /// have indicated which of the pooled links they meant.
    | SingleBackingLink
    /// A node or group card. Here the user indicated an *entity*, and the edit
    /// means "this annotation, on the links this entity carries it through" -
    /// the same scope removal already has at this surface, where removing from a
    /// node removes from every edge connected to it. It therefore resolves
    /// whenever one assignment backs every visible link, and refuses only when
    /// several distinct assignments are in play.
    | OwnerScopedLinks

let editAvailableReferences
    (scope: ProcessEditScope)
    (receiverId: CanonicalNodeId)
    (references: AvailableAnnotationRef list)
    (content: NodeValueContent)
    (session: ProvenanceSession)
    : Result<CommandEffect, ProvenanceCommandError> =
    if references.IsEmpty then
        Error EmptyTarget
    else
        match
            references
            |> List.tryPick (fun reference ->
                match reference.Relation with
                | ReverseConnectionLocal linkId -> Some(reference.AssignmentId, linkId)
                | _ -> None
            )
        with
        | Some(assignmentId, linkId) -> Error(ReadOnlyReverseLocalEdit(assignmentId, linkId))
        | None ->
            let nodeReferences =
                references
                |> List.choose (fun reference ->
                    match reference.Owner with
                    | NodeOwner nodeId -> Some(nodeId, reference)
                    | ProcessOwner _ -> None
                )

            let processReferences =
                references
                |> List.filter (fun reference ->
                    match reference.Owner with
                    | ProcessOwner _ -> true
                    | NodeOwner _ -> false
                )

            match nodeReferences.IsEmpty, processReferences.IsEmpty with
            | false, false ->
                Error(
                    InconsistentCanonicalState
                        "One availability edit cannot mix node-owned and process-owned references."
                )
            | false, true ->
                match nodeReferences with
                | [ ownerId, reference ] ->
                    match reference.Relation with
                    | OwnedNode when ownerId <> receiverId ->
                        Error(
                            InconsistentCanonicalState
                                $"Owned-node reference '{reference.AssignmentId}' does not belong to receiver '{receiverId}'."
                        )
                    | OwnedNode
                    | ForwardPropagated _ -> editNodeAssignment ownerId reference.AssignmentId content session
                    | IncidentProcess _
                    | ReverseConnectionLocal _ ->
                        Error(
                            InconsistentCanonicalState
                                $"Node-owned reference '{reference.AssignmentId}' has an invalid availability relation."
                        )
                | _ -> Error(ambiguityEvidence references)
            | true, false ->
                let backingReferences = processBackingReferences processReferences

                match scope, backingReferences with
                // No backing link resolved at all. This is an empty target, not a
                // pooled one - reporting it as ambiguous told the user "several
                // links cover this" when the true count was zero.
                | _, [] -> Error EmptyTarget
                | SingleBackingLink, [ processId, assignmentId, linkId ] ->
                    editProcessAssignmentSubset processId assignmentId (Set.singleton linkId) content session
                | SingleBackingLink, _ -> Error(ambiguityEvidence references)
                | OwnerScopedLinks, entries ->
                    // Everything reaching here was deduplicated into one displayed
                    // value by the grouping key, so every reference currently holds
                    // the same header, value and unit: setting that value has
                    // exactly one meaning however many assignments carry it. One
                    // drop on an entity already creates one assignment per
                    // structural process it touches (intent §3), so several
                    // assignments behind one displayed value is the ordinary shape
                    // here, not an exotic one. Each is edited over the links this
                    // entity carries it through - the scope removal has at the same
                    // surface - as a single atomic command.
                    entries
                    |> List.groupBy (fun (processId, assignmentId, _) -> processId, assignmentId)
                    |> List.map (fun ((processId, assignmentId), assignmentEntries) ->
                        let linkIds =
                            assignmentEntries |> List.map (fun (_, _, linkId) -> linkId) |> Set.ofList

                        editProcessAssignmentSubset processId assignmentId linkIds content
                    )
                    |> fun operations -> atomic operations session
            | true, true -> Error EmptyTarget

let removeAvailableReferences
    (receiverId: CanonicalNodeId)
    (references: AvailableAnnotationRef list)
    (session: ProvenanceSession)
    : Result<CommandEffect, ProvenanceCommandError> =
    if references.IsEmpty then
        Error EmptyTarget
    else
        match
            references
            |> List.tryPick (fun reference ->
                match reference.Relation with
                | ReverseConnectionLocal linkId -> Some(ReadOnlyReverseLocalRemoval(reference.AssignmentId, linkId))
                | ForwardPropagated _ -> Some(PropagatedRemovalAtReceiver(reference.AssignmentId, receiverId))
                | OwnedNode
                | IncidentProcess _ -> None
            )
        with
        | Some error -> Error error
        | None ->
            let nodeReferences =
                references
                |> List.choose (fun reference ->
                    match reference.Owner, reference.Relation with
                    | NodeOwner nodeId, OwnedNode -> Some(nodeId, reference.AssignmentId)
                    | NodeOwner _, _
                    | ProcessOwner _, _ -> None
                )

            let processReferences =
                references
                |> List.filter (fun reference ->
                    match reference.Owner, reference.Relation with
                    | ProcessOwner _, IncidentProcess _ -> true
                    | _ -> false
                )

            match nodeReferences.IsEmpty, processReferences.IsEmpty with
            | false, false ->
                Error(
                    InconsistentCanonicalState
                        "One availability removal cannot mix node-owned and process-owned references."
                )
            | false, true ->
                let selections =
                    nodeReferences
                    |> List.groupBy fst
                    |> List.map (fun (nodeId, entries) -> nodeId, entries |> List.map snd |> Set.ofList)
                    |> Map.ofList

                removeNodeAssignmentsByOwner selections session
            | true, false ->
                let selections =
                    processBackingReferences processReferences
                    |> List.groupBy (fun (processId, _, _) -> processId)
                    |> List.map (fun (processId, processEntries) ->
                        let assignments =
                            processEntries
                            |> List.groupBy (fun (_, assignmentId, _) -> assignmentId)
                            |> List.map (fun (assignmentId, assignmentEntries) ->
                                assignmentId,
                                assignmentEntries |> List.map (fun (_, _, linkId) -> linkId) |> Set.ofList
                            )
                            |> Map.ofList

                        processId, assignments
                    )
                    |> Map.ofList

                removeProcessAssignmentsByOwner selections session
            | true, true ->
                Error(
                    InconsistentCanonicalState
                        "Availability removal references do not describe an owning node or incident process."
                )

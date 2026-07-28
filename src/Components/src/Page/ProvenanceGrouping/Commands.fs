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

type ProcessAssignmentDraft = {
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

let private matchingProcessAssignments preparation propertyKind structuralProcess session =
    structuralProcess.Assignments
    |> Map.toList
    |> List.choose (fun (_, assignment) ->
        match propertyOfProcessAssignment session assignment with
        | Some(property, definition) when
            property.Category = preparation.PropertyDefinition.Category
            && assignment.PropertyKind = propertyKind
            && assignment.ContainerReferenceValueId.IsNone
            && assignment.ReferenceSlotId.IsNone
            && semanticallyMatchesPreparation preparation definition session
            ->
            Some assignment
        | _ -> None
    )

let assignProcessValue
    (linkIds: Set<ProcessLinkId>)
    (draft: ProcessAssignmentDraft)
    (session: ProvenanceSession)
    : Result<CommandEffect, ProvenanceCommandError> =
    if draft.OwnerKind <> AnnotationOwnerKind.Process then
        Error(InconsistentCanonicalState "A process assignment command requires AnnotationOwnerKind.Process.")
    elif draft.PropertyKind <> AssignmentPropertyKind.Generic then
        Error(InconsistentCanonicalState "A newly created process property must use AssignmentPropertyKind.Generic.")
    else
        match resolveLinkOwners linkIds session with
        | Error error -> Error error
        | Ok linksByProcess ->
            let preparation =
                ensureValueDefinition draft.Content.Category draft.Content.Value draft.Content.Unit session

            let plans =
                linksByProcess
                |> Map.toList
                |> List.choose (fun (processId, selectedLinks) ->
                    let structuralProcess = session.Processes[processId]

                    let matching =
                        matchingProcessAssignments preparation AssignmentPropertyKind.Generic structuralProcess session

                    let alreadyCovered = matching |> Seq.collect _.CoveredLinkIds |> Set.ofSeq

                    let missing = selectedLinks - alreadyCovered

                    if missing.IsEmpty then
                        None
                    else
                        Some(processId, structuralProcess, matching |> List.tryHead, missing)
                )

            if plans.IsEmpty then
                Ok noChange
            else
                let mutable usedIds = allAssignmentIds session
                let mutable resultingSession = installPreparation preparation session
                let mutable mutations = []
                let mutable changedAssignmentIds = Set.empty
                let mutable changedLinks = Set.empty

                for processId, structuralProcess, compatible, missing in plans do
                    match compatible with
                    | Some before ->
                        let after = {
                            before with
                                CoveredLinkIds = before.CoveredLinkIds + missing
                        }

                        resultingSession <-
                            resultingSession
                            |> updateProcess {
                                structuralProcess with
                                    Assignments = structuralProcess.Assignments |> Map.add after.Id after
                            }

                        changedAssignmentIds <- changedAssignmentIds |> Set.add after.Id
                        changedLinks <- changedLinks + missing
                        mutations <- (processId, Choice1Of2(before, after)) :: mutations
                    | None ->
                        let assignmentId = nextAssignmentId usedIds
                        usedIds <- usedIds |> Set.add assignmentId

                        let assignment = {
                            Id = assignmentId
                            ValueId = preparation.ValueDefinition.Id
                            PropertyKind = AssignmentPropertyKind.Generic
                            CoveredLinkIds = missing
                            ContainerReferenceValueId = None
                            ReferenceSlotId = None
                            Lineage = AssignmentLineage.Created
                        }

                        resultingSession <-
                            resultingSession
                            |> updateProcess {
                                structuralProcess with
                                    Assignments = structuralProcess.Assignments |> Map.add assignment.Id assignment
                            }

                        changedAssignmentIds <- changedAssignmentIds |> Set.add assignment.Id
                        changedLinks <- changedLinks + missing
                        mutations <- (processId, Choice2Of2 assignment) :: mutations

                let changedOwners =
                    plans |> List.map (fun (processId, _, _, _) -> processId) |> Set.ofList

                let context = processCommandContext changedOwners changedAssignmentIds changedLinks

                let journal =
                    mutations
                    |> List.rev
                    |> List.map (fun (processId, mutation) ->
                        match mutation with
                        | Choice1Of2(before, after) ->
                            ProcessAssignmentCoverageChanged(processId, before, after, context)
                        | Choice2Of2 assignment -> ProcessAssignmentAdded(processId, assignment, context)
                    )

                Ok(topology resultingSession journal)

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
                    | Ok() -> Ok(ownerId, structuralProcess, assignment, linkIds)
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

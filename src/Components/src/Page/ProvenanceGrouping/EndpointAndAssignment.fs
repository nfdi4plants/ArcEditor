namespace Swate.Components.Page.ProvenanceGrouping

/// Derives endpoint defaults, identities, and display headers for empty-side creation.
module Endpoints =

    open Swate.Components.Page.ProvenanceGrouping.Identifiers

    let fallbackKind: ProvenanceKind = {
        Id = "editor:endpoint"
        Label = "Endpoint"
    }

    let endpointKindOptions () : ProvenanceKind list = [
        {
            Id = "arc-isa:endpoint:source"
            Label = "Source"
        }
        {
            Id = "arc-isa:endpoint:sample"
            Label = "Sample"
        }
        {
            Id = "arc-isa:endpoint:material"
            Label = "Material"
        }
        {
            Id = "arc-isa:endpoint:data"
            Label = "Data"
        }
    ]

    /// Endpoint kinds offered when creating an endpoint, taken from the kinds the
    /// session's own nodes already carry. The host adapter that loaded those
    /// nodes is the same one that has to materialize a new endpoint on writeback,
    /// so offering any other kind would create nodes it must reject. Falls back
    /// to the ISA list only for sessions with no nodes to learn from.
    let kindsForNodes (kinds: ProvenanceKind seq) : ProvenanceKind list =
        let known =
            kinds
            |> Seq.distinctBy (fun kind -> kind.Id)
            |> Seq.sortBy (fun kind -> kind.Label)
            |> List.ofSeq

        if known.IsEmpty then endpointKindOptions () else known

    let defaultEndpointKind () : ProvenanceKind =
        endpointKindOptions () |> List.tryHead |> Option.defaultValue fallbackKind

    let endpointKindIdentity (kind: ProvenanceKind) = kind.Id

    let endpointHeader side (kind: ProvenanceKind) : ProvenanceIOHeader =
        let prefix = if side = ProvenanceSide.Input then "Input" else "Output"

        {
            Kind = kind
            Text = $"{prefix} [{kind.Label}]"
        }

/// Plans how a dropped value should be added or overwritten across the targets a
/// drop resolves to.
module ValueAssignment =

    open Swate.Components.Page.ProvenanceGrouping.Identifiers
    open Swate.Components.Page.ProvenanceGrouping.Values
    open Swate.Components.Page.ProvenanceGrouping.Domain
    open Swate.Components.Page.ProvenanceGrouping.MutationTypes
    open Swate.Components.Page.ProvenanceGrouping.AvailabilityTypes
    open Swate.Components.Page.ProvenanceGrouping.ProjectionTypes
    open Swate.Components.Page.ProvenanceGrouping.Types

    /// Assignment targets owners, so only *owned* references count as existing
    /// values. A propagated or reverse-local reference on the same item is
    /// non-owning evidence and must not be mistaken for an assignment there.
    let private isOwnedHere (annotation: ProjectedAnnotation) =
        match annotation.Availability.Relation with
        | OwnedNode -> true
        | IncidentProcess _ -> true
        | ForwardPropagated _
        | ReverseConnectionLocal _ -> false

    let private assignmentIdOf (annotation: ProjectedAnnotation) =
        match annotation.Backing with
        | NodeAssignmentBacking(identity, _, _) -> identity.AssignmentId
        | ProcessAssignmentBacking(identity, _, _, _, _) -> identity.AssignmentId

    let private valueIdOf (annotation: ProjectedAnnotation) =
        match annotation.Backing with
        | NodeAssignmentBacking(identity, _, _) -> identity.ValueId
        | ProcessAssignmentBacking(identity, _, _, _, _) -> identity.ValueId

    let private propertyKindOf (annotation: ProjectedAnnotation) =
        match annotation.Backing with
        | NodeAssignmentBacking(identity, _, _)
        | ProcessAssignmentBacking(identity, _, _, _, _) -> identity.PropertyKind

    let private matchesKey
        (key: AnnotationHeaderKey)
        (propertyKind: AssignmentPropertyKind)
        (annotation: ProjectedAnnotation)
        =
        PropertyRails.headerKeyOf annotation = key
        && propertyKindOf annotation = propertyKind

    /// The value a source would write, in the same shape the projection uses, so
    /// an idempotent drop can be recognised without consulting the session.
    let private sourceIdentity (source: ValueAssignmentSource) =
        Projection.toGroupingValueIdentity source.Value, source.Unit

    let private annotationIdentity (session: ProvenanceSession) (annotation: ProjectedAnnotation) =
        session.Values
        |> Map.tryFind (valueIdOf annotation)
        |> Option.map (fun definition -> Projection.toGroupingValueIdentity definition.Value, definition.Unit)

    /// One resolved target and the owned assignments it already has for the
    /// source's kind-bearing entry.
    type ResolvedTarget = {
        NodeId: CanonicalNodeId option
        LinkId: ProcessLinkId option
        Existing: ProjectedAnnotation list
    }

    let private nodeTargets (key: AnnotationHeaderKey) (propertyKind: AssignmentPropertyKind) (group: DisplayGroup) =
        group.CanonicalNodeIds
        |> Set.toList
        |> List.map (fun nodeId -> {
            NodeId = Some nodeId
            LinkId = None
            Existing =
                group.Annotations
                |> List.filter (fun annotation ->
                    matchesKey key propertyKind annotation
                    && isOwnedHere annotation
                    && (
                        match annotation.Backing with
                        | NodeAssignmentBacking(_, ownerId, _) -> ownerId = nodeId
                        | ProcessAssignmentBacking _ -> false
                    )
                )
        })

    let private linkTargets
        (key: AnnotationHeaderKey)
        (propertyKind: AssignmentPropertyKind)
        (linkIds: Set<ProcessLinkId>)
        (annotations: ProjectedAnnotation list)
        =
        linkIds
        |> Set.toList
        |> List.map (fun linkId -> {
            NodeId = None
            LinkId = Some linkId
            Existing =
                annotations
                |> List.filter (fun annotation ->
                    matchesKey key propertyKind annotation
                    && (
                        match annotation.Backing with
                        | ProcessAssignmentBacking(_, _, coveredLinkIds, _, _) -> coveredLinkIds.Contains linkId
                        | NodeAssignmentBacking _ -> false
                    )
                )
        })

    /// Container and slot rules are enforced for the whole batch before anything
    /// mutates: a container-bound dependent needs its reference on the same link,
    /// and a reference occupies at most one slot per link.
    let private validateReferenceRules
        (source: ValueAssignmentSource)
        (session: ProvenanceSession)
        (targets: ResolvedTarget list)
        : Result<unit, ProvenanceCommandError> =
        let linkIds = targets |> List.choose _.LinkId |> Set.ofList

        let assignmentsCovering linkId =
            session.Processes
            |> Map.toList
            |> List.collect (fun (_, structuralProcess) ->
                structuralProcess.Assignments
                |> Map.toList
                |> List.map snd
                |> List.filter (fun assignment -> assignment.CoveredLinkIds.Contains linkId)
            )

        match source.ContainerReferenceValueId with
        | Some containerValueId when
            linkIds
            |> Set.exists (fun linkId ->
                assignmentsCovering linkId
                |> List.exists (fun assignment -> assignment.ValueId = containerValueId)
                |> not
            )
            ->
            Error(
                MissingReferenceContainer(
                    source.CopiedFromAssignmentId |> Option.defaultValue "",
                    containerValueId,
                    linkIds
                )
            )
        | _ ->
            match source.ReferenceSlotId with
            | Some slotId ->
                let occupied =
                    linkIds
                    |> Set.toList
                    |> List.collect (fun linkId ->
                        assignmentsCovering linkId
                        |> List.filter (fun assignment ->
                            assignment.ReferenceSlotId = Some slotId
                            && session.Values
                               |> Map.tryFind assignment.ValueId
                               |> Option.map (fun definition ->
                                   (Projection.toGroupingValueIdentity definition.Value, definition.Unit)
                                   <> sourceIdentity source
                               )
                               |> Option.defaultValue true
                        )
                        |> List.map (fun assignment -> linkId, assignment.Id)
                    )

                // A second reference dropped onto an occupied slot is a
                // replacement, so this only refuses when the caller has not
                // asked for one.
                if occupied.IsEmpty then
                    Ok()
                else
                    Error(
                        ReferenceSlotOccupied(
                            slotId,
                            occupied |> List.map fst |> Set.ofList,
                            occupied |> List.map snd |> Set.ofList
                        )
                    )
            | None -> Ok()

    /// Plans one side's drop. Per intent §3: a missing target gets a new
    /// assignment, a target already holding the same value is an idempotent
    /// no-op, a partial aggregate adds only to the missing targets, and replacing
    /// a different value needs an explicit overwrite. Several distinguishable
    /// same-header assignments on one target are ambiguous and refused unless one
    /// is explicitly identified.
    let planValueDrop
        (source: ValueAssignmentSource)
        (propertyId: PropertyDefinitionId)
        (identified: AnnotationAssignmentId option)
        (targets: ResolvedTarget list)
        (session: ProvenanceSession)
        : Result<ValueAssignmentPlan list, ProvenanceCommandError> =
        if targets.IsEmpty then
            Error EmptyTarget
        else
            validateReferenceRules source session targets
            |> Result.bind (fun () ->
                let targets =
                    match identified with
                    | None -> targets
                    | Some assignmentId ->
                        targets
                        |> List.map (fun target ->
                            match
                                target.Existing
                                |> List.tryFind (fun annotation -> assignmentIdOf annotation = assignmentId)
                            with
                            | Some identifiedAnnotation -> {
                                target with
                                    Existing = [ identifiedAnnotation ]
                              }
                            | None -> target
                        )

                let ambiguous = targets |> List.tryFind (fun target -> target.Existing.Length > 1)

                match ambiguous with
                | Some target ->
                    Error(MultiplePropertyValues(propertyId, target.Existing |> List.map assignmentIdOf |> Set.ofList))
                | None ->
                    // Mixed counts across the resolved links of one process are
                    // reported per link, which is what the retargeted error
                    // carries now that sets are gone.
                    let linkCounts =
                        targets
                        |> List.choose (fun target ->
                            target.LinkId |> Option.map (fun linkId -> linkId, target.Existing.Length)
                        )

                    let distinctLinkCounts = linkCounts |> List.map snd |> List.distinct

                    if distinctLinkCounts.Length > 1 && source.ReferenceSlotId.IsSome then
                        Error(MixedPropertyValueCounts(propertyId, Map.ofList linkCounts))
                    else
                        let missing, present =
                            targets |> List.partition (fun target -> target.Existing.IsEmpty)

                        let sameValue target =
                            target.Existing
                            |> List.forall (fun annotation ->
                                annotationIdentity session annotation = Some(sourceIdentity source)
                            )

                        let toReplace = present |> List.filter (sameValue >> not)

                        let request targets = {
                            Target =
                                match targets |> List.choose _.NodeId with
                                | [] -> ProcessTargets(targets |> List.choose _.LinkId |> Set.ofList)
                                | nodeIds -> NodeTargets(Set.ofList nodeIds)
                            OwnerKind = source.Key.Kind
                            PropertyKind = source.PropertyKind
                            Category = source.Key.Header
                            Value = source.Value
                            Unit = source.Unit
                        }

                        Ok [
                            if not missing.IsEmpty then
                                AddCurrent(request missing)

                            if not toReplace.IsEmpty then
                                ConfirmOverwrite {
                                    Target = (request toReplace).Target
                                    ExistingAssignmentIds =
                                        toReplace
                                        |> List.collect (fun target -> target.Existing |> List.map assignmentIdOf)
                                        |> Set.ofList
                                    Header = source.Key.Header
                                    Value = source.Value
                                    Unit = source.Unit
                                }
                        ]
            )

    let planNodeValueDropToGroups
        (source: ValueAssignmentSource)
        (propertyId: PropertyDefinitionId)
        (identified: AnnotationAssignmentId option)
        (groups: DisplayGroup list)
        (session: ProvenanceSession)
        : Result<PropertyAssignmentBatch, ProvenanceCommandError> =
        let targets =
            groups
            |> List.collect (nodeTargets source.Key source.PropertyKind)
            |> List.distinctBy _.NodeId

        planValueDrop source propertyId identified targets session
        |> Result.map (fun plans -> {
            Adds =
                plans
                |> List.choose (
                    function
                    | AddCurrent request -> Some request
                    | ConfirmOverwrite _ -> None
                )
            Overwrites =
                plans
                |> List.choose (
                    function
                    | ConfirmOverwrite warning -> Some warning
                    | AddCurrent _ -> None
                )
        })

    /// A process value resolves to links, partitioned per structural process so
    /// each partition becomes one assignment covering its own links.
    let planProcessValueDropToLinks
        (source: ValueAssignmentSource)
        (propertyId: PropertyDefinitionId)
        (identified: AnnotationAssignmentId option)
        (linkIds: Set<ProcessLinkId>)
        (annotations: ProjectedAnnotation list)
        (session: ProvenanceSession)
        : Result<PropertyAssignmentBatch, ProvenanceCommandError> =
        let byProcess =
            session.Processes
            |> Map.toList
            |> List.choose (fun (processId, structuralProcess) ->
                let owned =
                    structuralProcess.Links |> Map.keys |> Set.ofSeq |> Set.intersect linkIds

                if owned.IsEmpty then None else Some(processId, owned)
            )

        byProcess
        |> List.fold
            (fun result (_, owned) ->
                result
                |> Result.bind (fun (batch: PropertyAssignmentBatch) ->
                    planValueDrop
                        source
                        propertyId
                        identified
                        (linkTargets source.Key source.PropertyKind owned annotations)
                        session
                    |> Result.map (fun plans ->
                        plans
                        |> List.fold
                            (fun current plan ->
                                match plan with
                                | AddCurrent request -> {
                                    current with
                                        Adds = current.Adds @ [ request ]
                                  }
                                | ConfirmOverwrite warning -> {
                                    current with
                                        Overwrites = current.Overwrites @ [ warning ]
                                  }
                            )
                            batch
                    )
                )
            )
            (Ok { Adds = []; Overwrites = [] })

    /// Annotation targeting is side-local (design §6.1): only selected groups on
    /// the action's own side become targets. An opposite-side selection is left
    /// untouched so it can still seed layer creation.
    let selectedTargetGroupsForDrop
        (layerId: ProvenanceLayerId)
        (dropSide: ProvenanceSide)
        (dropGroupId: string)
        (selectedInputs: Set<ProvenanceLayerId * string>)
        (selectedOutputs: Set<ProvenanceLayerId * string>)
        (findGroup: ProvenanceSide -> string -> DisplayGroup option)
        : DisplayGroup list =
        let idsForLayer selected =
            selected
            |> Set.filter (fun (currentLayerId, _) -> currentLayerId = layerId)
            |> Set.map snd

        let sideIds =
            match dropSide with
            | ProvenanceSide.Input -> idsForLayer selectedInputs
            | ProvenanceSide.Output -> idsForLayer selectedOutputs

        if sideIds.Contains dropGroupId then
            sideIds |> Set.toList |> List.choose (findGroup dropSide)
        else
            findGroup dropSide dropGroupId |> Option.toList

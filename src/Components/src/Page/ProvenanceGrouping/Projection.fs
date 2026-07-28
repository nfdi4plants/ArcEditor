module Swate.Components.Page.ProvenanceGrouping.Projection

open System
open System.Globalization
open Swate.Components.Page.ProvenanceGrouping.Identifiers
open Swate.Components.Page.ProvenanceGrouping.Values
open Swate.Components.Page.ProvenanceGrouping.Domain
open Swate.Components.Page.ProvenanceGrouping.AvailabilityTypes
open Swate.Components.Page.ProvenanceGrouping.ProjectionTypes
open Swate.Components.Page.ProvenanceGrouping.MutationTypes

type CompositeGroupingKey =
    | GroupedValues of GroupingValueKey list
    | MissingValueForItem of itemId: string

type GroupedProjectedValue = {
    Key: GroupingValueKey
    Annotations: ProjectedAnnotation list
}

let toGroupingValueIdentity =
    function
    | ProvenanceValue.Text value -> TextIdentity value
    | ProvenanceValue.Integer value -> IntegerIdentity value
    | ProvenanceValue.Float value -> FloatIdentity value
    | ProvenanceValue.Term value -> TermIdentity value
    | ProvenanceValue.Reference value -> ReferenceIdentity(value.Scheme, value.Id)

let private encodeString (value: string) =
    value.Length.ToString(CultureInfo.InvariantCulture) + ":" + value

let private encodeOption encode =
    function
    | None -> "0:"
    | Some value -> "1:" + encode value

let private termSortKey (value: ProvenanceTerm) =
    String.concat "|" [
        encodeString value.Name
        encodeOption encodeString value.TermSource
        encodeOption encodeString value.TermAccession
    ]

let private valueSortKey =
    function
    | TextIdentity value -> "0|" + encodeString value
    | IntegerIdentity value -> "1|" + value.ToString("D11", CultureInfo.InvariantCulture)
    | FloatIdentity value ->
        let bits = BitConverter.DoubleToInt64Bits value
        "2|" + bits.ToString("X16", CultureInfo.InvariantCulture)
    | TermIdentity value -> "3|" + termSortKey value
    | ReferenceIdentity(scheme, id) -> String.concat "|" [ "4"; encodeString scheme; encodeString id ]

let private groupingKeySortKey =
    function
    | NodeValue(header, value, unit) ->
        String.concat "|" [
            "0"
            termSortKey header
            valueSortKey value
            encodeOption termSortKey unit
        ]
    | ProcessValue(header, value, unit, sourceId) ->
        String.concat "|" [
            "1"
            termSortKey header
            valueSortKey value
            encodeOption termSortKey unit
            encodeString sourceId
        ]

let normalizeGroupingKeys (keys: GroupingValueKey list) =
    keys
    |> List.groupBy groupingKeySortKey
    |> List.sortBy fst
    |> List.map (snd >> List.head)

let compositeGroupingKey itemId keys =
    match normalizeGroupingKeys keys with
    | [] -> MissingValueForItem itemId
    | normalized -> GroupedValues normalized

let private projectionIdentity propertyId assignmentId valueId propertyKind = {
    PropertyId = propertyId
    ValueId = valueId
    AssignmentId = assignmentId
    PropertyKind = propertyKind
}

let private availabilityEvidence (reference: AvailableAnnotationRef) = {
    Relation = reference.Relation
    OriginatingLinkIds = reference.OriginatingLinkIds
    VisibleThroughLinkIds = reference.VisibleThroughLinkIds
}

let private valueAndProperty valueId (session: ProvenanceSession) =
    match session.Values |> Map.tryFind valueId with
    | None -> Error(ValueNotFound valueId)
    | Some definition ->
        match session.Properties |> Map.tryFind definition.PropertyId with
        | None -> Error(PropertyNotFound definition.PropertyId)
        | Some property -> Ok(definition, property)

let projectAnnotation
    (reference: AvailableAnnotationRef)
    (session: ProvenanceSession)
    : Result<ProjectedAnnotation, ProvenanceCommandError> =
    match reference.Owner with
    | NodeOwner nodeId ->
        match session.Nodes |> Map.tryFind nodeId with
        | None -> Error(NodeNotFound nodeId)
        | Some node ->
            match node.Assignments |> Map.tryFind reference.AssignmentId with
            | None -> Error(AssignmentNotFound(Some(NodeAssignmentOwner nodeId), reference.AssignmentId))
            | Some assignment ->
                valueAndProperty assignment.ValueId session
                |> Result.map (fun (definition, property) ->
                    let identity =
                        projectionIdentity property.Id assignment.Id assignment.ValueId assignment.PropertyKind

                    {
                        Key = NodeValue(property.Category, toGroupingValueIdentity definition.Value, definition.Unit)
                        Backing = NodeAssignmentBacking(identity, nodeId, assignment.TargetSource)
                        Availability = availabilityEvidence reference
                        OriginSource = assignment.TargetSource
                    }
                )
    | ProcessOwner processId ->
        match session.Processes |> Map.tryFind processId with
        | None -> Error(ProcessNotFound processId)
        | Some structuralProcess ->
            match structuralProcess.Assignments |> Map.tryFind reference.AssignmentId with
            | None -> Error(AssignmentNotFound(Some(ProcessAssignmentOwner processId), reference.AssignmentId))
            | Some assignment ->
                match session.Layers |> Map.tryFind structuralProcess.OriginLayerId with
                | None -> Error(LayerNotFound structuralProcess.OriginLayerId)
                | Some layer ->
                    valueAndProperty assignment.ValueId session
                    |> Result.map (fun (definition, property) ->
                        let identity =
                            projectionIdentity property.Id assignment.Id assignment.ValueId assignment.PropertyKind

                        {
                            Key =
                                ProcessValue(
                                    property.Category,
                                    toGroupingValueIdentity definition.Value,
                                    definition.Unit,
                                    layer.Source.Id
                                )
                            Backing =
                                ProcessAssignmentBacking(
                                    identity,
                                    processId,
                                    assignment.CoveredLinkIds,
                                    assignment.ContainerReferenceValueId,
                                    assignment.ReferenceSlotId
                                )
                            Availability = availabilityEvidence reference
                            OriginSource = Some layer.Source
                        }
                    )

let projectAnnotations references session =
    let folder state reference =
        state
        |> Result.bind (fun annotations ->
            projectAnnotation reference session
            |> Result.map (fun annotation -> annotation :: annotations)
        )

    references |> List.fold folder (Ok []) |> Result.map List.rev

let groupProjectedAnnotations (annotations: ProjectedAnnotation list) : GroupedProjectedValue list =
    annotations
    |> List.groupBy _.Key
    |> List.sortBy (fst >> groupingKeySortKey)
    |> List.map (fun (key, backing) -> { Key = key; Annotations = backing })

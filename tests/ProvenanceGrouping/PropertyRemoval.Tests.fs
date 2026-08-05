module ProcessCorePropertyRemovalTests

open Expecto
open ProcessCore
open ProcessCoreProvenanceFixtures
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreAdapterTypes
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreConverter
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreWriteback

module CanonicalCommands = Swate.Components.Page.ProvenanceGrouping.Commands
module CanonicalDomain = Swate.Components.Page.ProvenanceGrouping.Domain
module CanonicalProjectionTypes = Swate.Components.Page.ProvenanceGrouping.ProjectionTypes
module Session = Swate.Components.Page.ProvenanceGrouping.Session
module CanonicalValues = Swate.Components.Page.ProvenanceGrouping.Values

type private RemovalFixture = {
    Arc: ARC
    Dataset: Dataset
    FirstProcess: Process
    SecondProcess: Process
    FirstInput: Sample
    SecondInput: Sample
}

/// Two structural processes in one loaded group, each carrying one node
/// annotation on its input and one process parameter. All four annotations
/// share the same header, value and unit, so the converter normalizes them
/// onto one property and one value definition while their owners stay
/// distinct: two canonical nodes and two structural processes. That is what
/// makes "only that assignment" and "every referencing assignment"
/// distinguishable at all.
let private sharedValueFixture () =
    let annotation additionalType =
        Annotation("shared-neutral", value = "shared-value", additionalType = additionalType)

    let firstInput = Sample("input-one")
    firstInput.AddAdditionalProperty(annotation "CharacteristicValue")

    let secondInput = Sample("input-two")
    secondInput.AddAdditionalProperty(annotation "CharacteristicValue")

    let firstProcess =
        mkProcessFull "stage-neutral" None [ SampleNode firstInput ] [ SampleNode(Sample("output-one")) ] [
            annotation "ParameterValue"
        ]

    let secondProcess =
        mkProcessFull "stage-neutral" None [ SampleNode secondInput ] [ SampleNode(Sample("output-two")) ] [
            annotation "ParameterValue"
        ]

    let dataset =
        Dataset("dataset-neutral", processes = [ firstProcess; secondProcess ])

    {
        Arc = ARC("arc-neutral", hasPart = [ dataset ])
        Dataset = dataset
        FirstProcess = firstProcess
        SecondProcess = secondProcess
        FirstInput = firstInput
        SecondInput = secondInput
    }

let private canonicalLocation: ProcessCoreProcessGroupLocation = {
    DatasetPath = [ "arc-neutral"; "dataset-neutral" ]
    ProcessGroupName = "stage-neutral"
}

let private convertCanonical (arc: ARC) =
    fromArcMany [ canonicalLocation ] arc |> expectOk

let private prepareCanonical (session: CanonicalProjectionTypes.ProvenanceSession) =
    Session.prepareForWriteback session |> expectOk

let private commitCanonical effect (session: CanonicalProjectionTypes.ProvenanceSession) = Session.commit effect session

let private nodeAnnotationNames (node: Sample) =
    node.AdditionalProperty |> Seq.map _.Name |> List.ofSeq

let private parameterNames (processObject: Process) =
    processObject.ParameterValue |> Seq.map _.Name |> List.ofSeq

let private endpointName (node: IONode) =
    match node with
    | SampleNode sample -> sample.Name
    | DataNode data -> data.Path

let private processShapes (dataset: Dataset) =
    dataset.Processes
    |> Seq.map (fun proc ->
        (proc.Input |> Option.toList |> List.map endpointName), (proc.Output |> Option.toList |> List.map endpointName)
    )
    |> List.ofSeq

let private nodeIdByName name (session: CanonicalProjectionTypes.ProvenanceSession) =
    session.Nodes
    |> Map.toList
    |> List.find (fun (_, node) -> node.Name = name)
    |> fst

let private linkInputNodeId (link: CanonicalDomain.ProcessLink) =
    match link.Shape with
    | CanonicalValues.ProcessLinkShape.Between(inputId, _)
    | CanonicalValues.ProcessLinkShape.InputOnly inputId -> Some inputId
    | CanonicalValues.ProcessLinkShape.OutputOnly _
    | CanonicalValues.ProcessLinkShape.Endpointless -> None

/// The structural process whose link consumes the named canonical node.
let private processIdByInputName name (session: CanonicalProjectionTypes.ProvenanceSession) =
    let nodeId = nodeIdByName name session

    session.Processes
    |> Map.toList
    |> List.find (fun (_, structuralProcess) ->
        structuralProcess.Links
        |> Map.exists (fun _ link -> linkInputNodeId link = Some nodeId)
    )
    |> fst

let private soleNodeAssignment nodeId (session: CanonicalProjectionTypes.ProvenanceSession) =
    session.Nodes[nodeId].Assignments
    |> Map.toList
    |> List.map snd
    |> List.exactlyOne

let private soleProcessAssignment ownerId (session: CanonicalProjectionTypes.ProvenanceSession) =
    session.Processes[ownerId].Assignments
    |> Map.toList
    |> List.map snd
    |> List.exactlyOne

let private sharedValueId (session: CanonicalProjectionTypes.ProvenanceSession) =
    session.Values
    |> Map.toList
    |> List.find (fun (_, definition) -> session.Properties[definition.PropertyId].Category.Name = "shared-neutral")
    |> fst

let private sharedPropertyId (session: CanonicalProjectionTypes.ProvenanceSession) =
    session.Properties
    |> Map.toList
    |> List.find (fun (_, property) -> property.Category.Name = "shared-neutral")
    |> fst

let private removeProcessAssignmentEntirely ownerId (session: CanonicalProjectionTypes.ProvenanceSession) =
    let assignment = soleProcessAssignment ownerId session

    CanonicalCommands.removeProcessAssignmentLinks ownerId assignment.Id assignment.CoveredLinkIds session
    |> expectOk
    |> fun effect -> commitCanonical effect session

let tests =
    testList "ProcessCore canonical property removal" [
        testCase "owner-scoped removal omits only that assignment from its writeback target"
        <| fun _ ->
            let fixture = sharedValueFixture ()
            let converted = convertCanonical fixture.Arc
            let firstNodeId = nodeIdByName "input-one" converted.Session
            let assignment = soleNodeAssignment firstNodeId converted.Session

            let prepared =
                CanonicalCommands.removeNodeAssignment firstNodeId assignment.Id converted.Session
                |> expectOk
                |> fun effect -> commitCanonical effect converted.Session
                |> prepareCanonical

            let summary = writeBackMany converted.Index prepared fixture.Arc |> expectOk

            Expect.isEmpty
                (nodeAnnotationNames fixture.FirstInput)
                "The owner-scoped node removal omits its assignment from that node."

            Expect.equal
                (nodeAnnotationNames fixture.SecondInput)
                [ "shared-neutral" ]
                "An equal assignment on an unrelated canonical node still materializes."

            Expect.equal
                (parameterNames fixture.FirstProcess)
                [ "shared-neutral" ]
                "A node removal never touches an equal process assignment on the same link."

            Expect.equal
                (parameterNames fixture.SecondProcess)
                [ "shared-neutral" ]
                "A node removal never touches an equal process assignment elsewhere."

            Expect.equal summary.RemovedProcesses 0 "An annotation removal removes no Process."

            // The same rule for a process owner: one of two structurally
            // identical processes loses its parameter, the other keeps it.
            let processFixture = sharedValueFixture ()
            let processConverted = convertCanonical processFixture.Arc
            let ownerId = processIdByInputName "input-one" processConverted.Session

            let processPrepared =
                removeProcessAssignmentEntirely ownerId processConverted.Session
                |> prepareCanonical

            let processSummary =
                writeBackMany processConverted.Index processPrepared processFixture.Arc
                |> expectOk

            Expect.isEmpty
                (parameterNames processFixture.FirstProcess)
                "The owner-scoped process removal omits its assignment from that process."

            Expect.equal
                (parameterNames processFixture.SecondProcess)
                [ "shared-neutral" ]
                "An equal assignment on an unrelated structural process still materializes."

            Expect.equal
                (nodeAnnotationNames processFixture.FirstInput)
                [ "shared-neutral" ]
                "A process removal never touches the equal node assignment on its own input."

            Expect.equal processSummary.AddedProcesses 0 "An annotation removal adds no Process."
            Expect.equal processSummary.RemovedProcesses 0 "An annotation removal removes no Process."

        testCase "global value removal omits every referencing assignment from every target"
        <| fun _ ->
            let fixture = sharedValueFixture ()
            let converted = convertCanonical fixture.Arc
            let valueId = sharedValueId converted.Session

            let referencingOwners =
                (converted.Session.Nodes
                 |> Map.toList
                 |> List.sumBy (fun (_, node) ->
                     node.Assignments
                     |> Map.toList
                     |> List.filter (fun (_, assignment) -> assignment.ValueId = valueId)
                     |> List.length
                 ))
                + (converted.Session.Processes
                   |> Map.toList
                   |> List.sumBy (fun (_, structuralProcess) ->
                       structuralProcess.Assignments
                       |> Map.toList
                       |> List.filter (fun (_, assignment) -> assignment.ValueId = valueId)
                       |> List.length
                   ))

            Expect.equal referencingOwners 4 "One value definition backs two node and two process assignments."

            let prepared =
                CanonicalCommands.removeValuesGlobally (Set.singleton valueId) converted.Session
                |> expectOk
                |> fun effect -> commitCanonical effect converted.Session
                |> prepareCanonical

            writeBackMany converted.Index prepared fixture.Arc |> expectOk |> ignore

            Expect.isEmpty
                (nodeAnnotationNames fixture.FirstInput)
                "Global value removal omits the first node assignment."

            Expect.isEmpty
                (nodeAnnotationNames fixture.SecondInput)
                "Global value removal omits the second node assignment."

            Expect.isEmpty
                (parameterNames fixture.FirstProcess)
                "Global value removal omits the first process assignment."

            Expect.isEmpty
                (parameterNames fixture.SecondProcess)
                "Global value removal omits the second process assignment."

            let reloaded = convertCanonical fixture.Arc

            Expect.isEmpty
                (reloaded.Session.Values |> Map.toList)
                "No target reloads a value definition for the globally removed value."

        testCase "global property removal omits the property everywhere and leaves structural links intact"
        <| fun _ ->
            let fixture = sharedValueFixture ()
            let converted = convertCanonical fixture.Arc
            let propertyId = sharedPropertyId converted.Session
            let shapesBefore = processShapes fixture.Dataset

            let prepared =
                CanonicalCommands.removePropertyGlobally propertyId converted.Session
                |> expectOk
                |> fun effect -> commitCanonical effect converted.Session
                |> prepareCanonical

            let summary = writeBackMany converted.Index prepared fixture.Arc |> expectOk

            Expect.isEmpty
                (nodeAnnotationNames fixture.FirstInput)
                "Global property removal omits the property from the first node."

            Expect.isEmpty
                (nodeAnnotationNames fixture.SecondInput)
                "Global property removal omits the property from the second node."

            Expect.isEmpty
                (parameterNames fixture.FirstProcess)
                "Global property removal omits the property from the first process."

            Expect.isEmpty
                (parameterNames fixture.SecondProcess)
                "Global property removal omits the property from the second process."

            Expect.equal summary.AddedProcesses 0 "Global property removal adds no Process."
            Expect.equal summary.RemovedProcesses 0 "Global property removal removes no Process."
            Expect.equal fixture.Dataset.Processes.Count 2 "Both structural processes survive global property removal."

            Expect.equal
                (processShapes fixture.Dataset)
                shapesBefore
                "Global property removal leaves every structural link intact."

            let reloaded = convertCanonical fixture.Arc

            Expect.hasLength
                (reloaded.Session.Processes |> Map.toList)
                2
                "Both structural processes reload after global property removal."

            Expect.isEmpty (reloaded.Session.Properties |> Map.toList) "The globally removed property does not reload."

        testCase "removing the last process assignment leaves the structural link writeable"
        <| fun _ ->
            let fixture = sharedValueFixture ()
            let converted = convertCanonical fixture.Arc
            let ownerId = processIdByInputName "input-one" converted.Session
            let shapesBefore = processShapes fixture.Dataset

            let prepared =
                removeProcessAssignmentEntirely ownerId converted.Session |> prepareCanonical

            Expect.isEmpty
                (prepared.Processes[ownerId].Assignments |> Map.toList)
                "The emptied structural process keeps no process assignment."

            writeBackMany converted.Index prepared fixture.Arc |> expectOk |> ignore

            let reloaded = convertCanonical fixture.Arc
            let reloadedOwnerId = processIdByInputName "input-one" reloaded.Session
            let reloadedProcess = reloaded.Session.Processes[reloadedOwnerId]

            Expect.isEmpty
                (reloadedProcess.Assignments |> Map.toList)
                "The emptied structural process reloads with no process assignment."

            let reloadedLink =
                reloadedProcess.Links |> Map.toList |> List.map snd |> List.exactlyOne

            Expect.equal
                reloadedLink.Shape
                (CanonicalValues.ProcessLinkShape.Between(
                    nodeIdByName "input-one" reloaded.Session,
                    nodeIdByName "output-one" reloaded.Session
                ))
                "The emptied link reloads with its exact endpoint shape."

            // Writeable, not merely present: the reloaded empty link saves
            // again without adding, removing or reshaping a Process.
            let secondSummary =
                writeBackMany reloaded.Index (prepareCanonical reloaded.Session) fixture.Arc
                |> expectOk

            Expect.equal secondSummary.AddedProcesses 0 "Saving the emptied link adds no Process."
            Expect.equal secondSummary.RemovedProcesses 0 "Saving the emptied link removes no Process."

            Expect.equal
                (processShapes fixture.Dataset)
                shapesBefore
                "The emptied structural link survives a second save unchanged."

            Expect.isEmpty
                (parameterNames fixture.FirstProcess)
                "The removed process assignment stays absent across the second save."
    ]

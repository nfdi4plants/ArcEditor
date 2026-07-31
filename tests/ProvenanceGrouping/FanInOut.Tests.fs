module ProcessCoreFanInOutTests

open Expecto
open ProcessCore
open ProcessCoreProvenanceFixtures
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreAdapterTypes
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreConverter
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreWriteback

module CanonicalCommands = Swate.Components.Page.ProvenanceGrouping.Commands

/// Two processes in one group feeding one shared output sample - the fan-in
/// mirror of the `allToAll` fan-out fixture. Each edge belongs to its own
/// ProcessCore process, so connection-targeted parameters must separate.
let private fanIn () =
    let inputOne = Sample("fan-input-one")
    let inputTwo = Sample("fan-input-two")
    let shared = Sample("fan-output-shared")

    let processOne =
        mkProcess "stage-neutral" [ SampleNode inputOne ] [ SampleNode shared ]

    let processTwo =
        mkProcess "stage-neutral" [ SampleNode inputTwo ] [ SampleNode shared ]

    let dataset = Dataset("dataset-neutral", processes = [ processOne; processTwo ])

    let arc = ARC("arc-neutral", hasPart = [ dataset ])
    arc, dataset, processOne, processTwo, shared

module CanonicalProjectionTypes = Swate.Components.Page.ProvenanceGrouping.ProjectionTypes
module CanonicalSession = Swate.Components.Page.ProvenanceGrouping.CanonicalSession
module CanonicalValues = Swate.Components.Page.ProvenanceGrouping.Values

let private canonicalLocation: ProcessCoreProcessGroupLocation = {
    DatasetPath = [ "arc-neutral"; "dataset-neutral" ]
    ProcessGroupName = "stage-neutral"
}

let private convertCanonical locations arc = fromArcMany locations arc |> expectOk

let private prepareCanonical (session: CanonicalProjectionTypes.ProvenanceSession) =
    CanonicalSession.prepareForWriteback session |> expectOk

/// Three explicit pairs over two inputs and two outputs: Cartesian inference
/// would add a fourth, so the missing pair is the assertion that matters.
let private explicitPairs = [
    "fan-input-one", "fan-output-one"
    "fan-input-one", "fan-output-two"
    "fan-input-two", "fan-output-one"
]

/// One process group whose processes materialize exactly the requested pairs,
/// each endpoint name interned to one shared ProcessCore node.
let private canonicalPairFixture (pairs: (string * string) list) =
    let nodes = System.Collections.Generic.Dictionary<string, Sample>()

    let node name =
        match nodes.TryGetValue name with
        | true, existing -> existing
        | false, _ ->
            let created = Sample(name)
            nodes[name] <- created
            created

    let processes =
        pairs
        |> List.map (fun (input, output) ->
            mkProcess "stage-neutral" [ SampleNode(node input) ] [ SampleNode(node output) ]
        )

    let dataset = Dataset("dataset-neutral", processes = processes)
    ARC("arc-neutral", hasPart = [ dataset ]), dataset

let private canonicalPairs (dataset: Dataset) =
    dataset.Processes
    |> Seq.choose (fun proc ->
        match proc.Input, proc.Output with
        | Some input, Some output -> Some(input.AsSample().Name, output.AsSample().Name)
        | _ -> None
    )
    |> List.ofSeq

let private canonicalLinkPairs (converted: ProcessCoreCanonicalConversionResult) =
    converted.Session.Processes
    |> Map.toList
    |> List.collect (fun (_, structuralProcess) ->
        structuralProcess.Links
        |> Map.toList
        |> List.choose (fun (_, link) ->
            match link.Shape with
            | CanonicalValues.ProcessLinkShape.Between(inputId, outputId) ->
                Some(converted.Session.Nodes[inputId].Name, converted.Session.Nodes[outputId].Name)
            | _ -> None
        )
    )


let private commitCanonical effect (session: CanonicalProjectionTypes.ProvenanceSession) =
    CanonicalSession.commit effect session

let private parameterNames (proc: Process) =
    proc.ParameterValue |> Seq.map _.Name |> List.ofSeq |> List.sort

let tests =
    testList "ProcessCore fan-in/fan-out property assignment" [
        testCase "removing one fan-in edge retracts its parameter from that process only"
        <| fun _ ->
            // The model-level coverage subtraction is Commands.Tests.fs's
            // "removing a connection subtracts coverage and deletes emptied
            // assignments"; this row is the adapter end of it - the retracted
            // parameter must also disappear from the written ProcessCore object,
            // while the surviving edge keeps its own share.
            let arc, dataset, processOne, processTwo, _ = fanIn ()
            let converted = convertCanonical [ canonicalLocation ] arc

            let linkOf inputName =
                let nodeId =
                    converted.Session.Nodes
                    |> Map.toList
                    |> List.find (fun (_, node) -> node.Name = inputName)
                    |> fst

                converted.Session.Processes
                |> Map.toList
                |> List.pick (fun (ownerId, structuralProcess) ->
                    structuralProcess.Links
                    |> Map.toList
                    |> List.tryPick (fun (linkId, link) ->
                        match link.Shape with
                        | CanonicalValues.ProcessLinkShape.Between(input, _) when input = nodeId ->
                            Some(ownerId, linkId)
                        | _ -> None
                    )
                )

            let _, linkOne = linkOf "fan-input-one"
            let _, linkTwo = linkOf "fan-input-two"

            let draft: CanonicalCommands.ProcessAssignmentDraft = {
                Content = {
                    Category = {
                        Name = "shared-parameter"
                        TermSource = None
                        TermAccession = None
                    }
                    Value = CanonicalValues.ProvenanceValue.Text "shared"
                    Unit = None
                }
                OwnerKind = CanonicalValues.AnnotationOwnerKind.Process
                // A newly created process property is Generic by rule; only a
                // loaded one keeps a concrete adapter kind.
                PropertyKind = CanonicalValues.AssignmentPropertyKind.Generic
                ContainerReferenceValueId = None
                ReferenceSlotId = None
                Lineage = CanonicalValues.AssignmentLineage.Created
            }

            // One assignment per owning process, both carrying the same header.
            let assigned =
                [ linkOne; linkTwo ]
                |> List.fold
                    (fun session linkId ->
                        CanonicalCommands.assignProcessValue (Set.singleton linkId) draft session
                        |> expectOk
                        |> fun effect -> commitCanonical effect session
                    )
                    converted.Session

            let disconnected =
                CanonicalCommands.disconnectLinks (Set.singleton linkOne) assigned
                |> expectOk
                |> fun effect -> commitCanonical effect assigned
                |> prepareCanonical

            canonicalWriteBackMany converted.Index disconnected arc |> expectOk |> ignore

            Expect.isEmpty
                (parameterNames processOne)
                "The disconnected edge's process loses the parameter its only link carried."

            Expect.equal
                (parameterNames processTwo)
                [ "shared-parameter" ]
                "The surviving edge keeps its own share of the same header."

            Expect.equal (Seq.length dataset.Processes) 2 "Retraction adds and removes no Process."

        testCase "explicit all-to-all pairs round-trip as exact links"
        <| fun _ ->
            let arc, dataset = canonicalPairFixture explicitPairs
            let converted = convertCanonical [ canonicalLocation ] arc

            canonicalWriteBackMany converted.Index (prepareCanonical converted.Session) arc
            |> expectOk
            |> ignore

            Expect.equal
                (canonicalPairs dataset |> List.sort)
                (explicitPairs |> List.sort)
                "Every explicit pair survives."

            let reloaded = convertCanonical [ canonicalLocation ] arc

            Expect.equal
                (canonicalLinkPairs reloaded |> List.sort)
                (explicitPairs |> List.sort)
                "Reload reconstructs exactly the explicit pairs."

            Expect.isFalse
                (canonicalLinkPairs reloaded |> List.contains ("fan-input-two", "fan-output-two"))
                "No Cartesian pair is inferred from the independent endpoint collections."

        testCase "YAML grouping of equal-state singular processes preserves positional pairs and repeated endpoints"
        <| fun _ ->
            let repeatedPairs = [
                "fan-input-one", "fan-output-one"
                "fan-input-one", "fan-output-one"
                "fan-input-one", "fan-output-two"
            ]

            let arc, _ = canonicalPairFixture repeatedPairs
            let converted = convertCanonical [ canonicalLocation ] arc

            canonicalWriteBackMany converted.Index (prepareCanonical converted.Session) arc
            |> expectOk
            |> ignore

            let roundTripped = ARC.fromYamlString (arc.toYamlString ())
            let reloaded = convertCanonical [ canonicalLocation ] roundTripped

            Expect.equal
                (canonicalLinkPairs reloaded |> List.sort)
                (repeatedPairs |> List.sort)
                "YAML grouping preserves every positional pair, including the repeated endpoints."
    ]

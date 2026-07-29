module Swate.Components.ProcessCore.RendererModel

open System.Collections.Generic
open ProcessCore

/// One input/output connection represented by a singular-I/O ProcessCore process.
type ProcessConnection = {
    Process: Process
    Input: IONode option
    Output: IONode option
}

/// Renderer representation of one logical process while retaining its input/output
/// connections.
type ProcessView = {
    Representative: Process
    Connections: ProcessConnection array
} with
    member this.Members = this.Connections |> Array.map _.Process
    member this.Inputs = this.Connections |> Array.choose _.Input
    member this.Outputs = this.Connections |> Array.choose _.Output

/// Immutable renderer projection derived from one ProcessCore ARC.
type ArcView = {
    Processes: ProcessView array
    Samples: Sample array
    Data: Data array
    ProcessesByDataset: Dictionary<Dataset, ProcessView array>
    ProcessByRepresentative: Dictionary<Process, ProcessView>
}

let private connection processObject = {
    Process = processObject
    Input = processObject.Input
    Output = processObject.Output
}

let ofProcess processObject = {
    Representative = processObject
    Connections = [| connection processObject |]
}

let private groupProcesses (processes: seq<Process>) =
    let groups = Dictionary<string, ResizeArray<Process>>()
    let order = ResizeArray<string>()

    for processObject in processes do
        // Input and output are excluded because table rows may use different lanes
        // for the same logical process.
        let key = ProcessCore.Yaml.Process.groupingKey processObject

        match groups.TryGetValue key with
        | true, group -> group.Add processObject
        | false, _ ->
            groups.[key] <- ResizeArray [ processObject ]
            order.Add key

    order
    |> Seq.map (fun key ->
        let members = groups.[key] |> Seq.toArray

        {
            Representative = members.[0]
            Connections = members |> Array.map connection
        }
    )
    |> Seq.toArray

/// Creates a renderer projection without mutating the ProcessCore graph.
let create (arc: ARC) =
    let processesByDataset = Dictionary<Dataset, ProcessView array>(HashIdentity.Reference)
    let processByRepresentative = Dictionary<Process, ProcessView>(HashIdentity.Reference)

    let processes =
        Swate.Components.ProcessCore.ObjectGraph.datasetsIncludingRoot arc
        |> Array.collect (fun dataset ->
            let groupedProcesses = groupProcesses dataset.Processes
            processesByDataset.[dataset] <- groupedProcesses

            for processView in groupedProcesses do
                processByRepresentative.[processView.Representative] <- processView

            groupedProcesses
        )

    {
        Processes = processes
        Samples = arc.AllSamples() |> Seq.toArray
        Data = arc.AllData() |> Seq.toArray
        ProcessesByDataset = processesByDataset
        ProcessByRepresentative = processByRepresentative
    }

let forDataset dataset view =
    match view.ProcessesByDataset.TryGetValue dataset with
    | true, processes -> processes
    | false, _ -> [||]

let forProcess processObject view =
    match view.ProcessByRepresentative.TryGetValue processObject with
    | true, processView -> processView
    | false, _ -> ofProcess processObject

let removeProcess processObject view =
    for memberProcess in (forProcess processObject view).Members do
        memberProcess.ProcessOf
        |> Option.iter (fun dataset -> dataset.RemoveProcess memberProcess)

let moveProcess (targetDataset: Dataset) processObject view =
    for memberProcess in (forProcess processObject view).Members do
        match memberProcess.ProcessOf with
        | Some owner when not (obj.ReferenceEquals(owner, targetDataset)) ->
            owner.RemoveProcess memberProcess
        | _ -> ()

        targetDataset.AddProcess memberProcess

let private createLaneMember (view: ProcessView) =
    let representative = view.Representative

    let memberProcess =
        Process(
            representative.Name,
            ?additionalType = representative.AdditionalType,
            ?executesRecipe = representative.ExecutesRecipe,
            parameterValue = representative.ParameterValue
        )

    representative.Properties
    |> Seq.iter (fun property -> memberProcess.SetProperty(property.Key, property.Value))

    representative.ProcessOf
    |> Option.iter (fun dataset -> dataset.AddProcess memberProcess)

    memberProcess

let private removeLane
    (tryGetLane: Process -> IONode option)
    (clearLane: Process -> unit)
    (hasOtherLane: Process -> bool)
    (node: IONode)
    (view: ProcessView)
    =
    let members = view.Members

    members
    |> Array.tryFind (fun processObject ->
        tryGetLane processObject
        |> Option.exists (fun candidate -> candidate.EqualTo node)
    )
    |> Option.iter (fun processObject ->
        if members.Length > 1 && not (hasOtherLane processObject) then
            processObject.ProcessOf
            |> Option.iter (fun dataset -> dataset.RemoveProcess processObject)
        else
            clearLane processObject
    )

let private addLane
    (tryGetLane: Process -> IONode option)
    (setLane: Process -> IONode -> unit)
    (node: IONode)
    (view: ProcessView)
    =
    view.Members
    |> Array.tryFind (tryGetLane >> Option.isNone)
    |> Option.defaultWith (fun () -> createLaneMember view)
    |> fun processObject -> setLane processObject node

let addInput =
    addLane _.Input (fun processObject node -> processObject.SetInput node)

let addOutput =
    addLane _.Output (fun processObject node -> processObject.SetOutput node)

let removeInput node view =
    removeLane _.Input (fun processObject -> processObject.ClearInput()) (fun processObject -> processObject.Output.IsSome) node view

let removeOutput node view =
    removeLane
        _.Output
        (fun processObject -> processObject.ClearOutput())
        (fun processObject -> processObject.Input.IsSome)
        node
        view

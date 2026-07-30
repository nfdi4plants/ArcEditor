module Swate.Components.ProcessCore.RendererModel

open System.Collections.Generic
open ProcessCore
open Swate.Components.ProcessCore.Types

let private processView (processes: Process array) =
    let keyedProcesses = Dictionary<int, Process>()
    let inputs = Dictionary<int, IONode>()
    let outputs = Dictionary<int, IONode>()

    processes
    |> Array.iteri (fun key processObject ->
        let addNode (nodes: Dictionary<int, IONode>) node =
            node |> Option.iter (fun value -> nodes.[key] <- value)

        keyedProcesses.[key] <- processObject
        addNode inputs processObject.Input
        addNode outputs processObject.Output
    )

    {
        Processes = keyedProcesses
        Inputs = inputs
        Outputs = outputs
    }

let private groupProcesses (processes: seq<Process>) =
    let groups = Dictionary<string, ResizeArray<Process>>()
    let order = ResizeArray<string>()

    for processObject in processes do
        // Input and output are excluded because a process may span several rows
        // for the same logical process.
        let key = ProcessCore.Yaml.Process.groupingKey processObject

        match groups.TryGetValue key with
        | true, group -> group.Add processObject
        | false, _ ->
            groups.[key] <- ResizeArray [ processObject ]
            order.Add key

    order
    |> Seq.map (fun key -> groups.[key] |> Seq.toArray |> processView)
    |> Seq.toArray

/// Creates a renderer projection without mutating the ProcessCore graph.
let create (arc: ARC) =
    let processesByDataset =
        Dictionary<Dataset, ProcessView array>(HashIdentity.Reference)

    let processByRepresentative =
        Dictionary<Process, ProcessView>(HashIdentity.Reference)

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
    | true, groupedProcess -> groupedProcess
    | false, _ -> processView [| processObject |]

let private removeFromOwner (processObject: Process) =
    processObject.ProcessOf
    |> Option.iter (fun dataset -> dataset.RemoveProcess processObject)

let removeProcess processObject view =
    for memberProcess in (forProcess processObject view).Processes.Values do
        removeFromOwner memberProcess

let moveProcess (targetDataset: Dataset) processObject view =
    for memberProcess in (forProcess processObject view).Processes.Values do
        match memberProcess.ProcessOf with
        | Some owner when not (obj.ReferenceEquals(owner, targetDataset)) -> owner.RemoveProcess memberProcess
        | _ -> ()

        targetDataset.AddProcess memberProcess

let private createRowMember (view: ProcessView) =
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

let private replaceRow (source: Process) (target: Process) =
    let input = source.Input
    let output = source.Output

    target.ClearInput()
    target.ClearOutput()
    input |> Option.iter target.SetInput
    output |> Option.iter target.SetOutput

let private tryFindRowProcess (rows: Dictionary<int, IONode>) (node: IONode) (view: ProcessView) =
    let tryFind predicate =
        rows
        |> Seq.tryFind (fun pair -> predicate pair.Value)
        |> Option.bind (fun pair ->
            match view.Processes.TryGetValue pair.Key with
            | true, processObject -> Some processObject
            | false, _ -> None
        )

    tryFind (fun candidate -> obj.ReferenceEquals(candidate, node))
    |> Option.orElseWith (fun () -> tryFind (fun candidate -> candidate.EqualTo node))

let private promoteRow (removed: Process) (view: ProcessView) =
    view.Processes.Values
    |> Seq.tryFind (fun candidate -> not (obj.ReferenceEquals(candidate, removed)))
    |> Option.iter (fun donor ->
        replaceRow donor removed
        removeFromOwner donor
    )

let private removeRow
    (rows: ProcessView -> Dictionary<int, IONode>)
    (clearRow: Process -> unit)
    (hasOtherRow: Process -> bool)
    (node: IONode)
    (view: ProcessView)
    =
    match tryFindRowProcess (rows view) node view with
    | None -> ()
    | Some processObject when view.Processes.Count = 1 || hasOtherRow processObject -> clearRow processObject
    | Some processObject when obj.ReferenceEquals(processObject, view.Representative) ->
        // Keep the object anchoring the open metadata panel alive.
        promoteRow processObject view
    | Some processObject -> removeFromOwner processObject

let private addRow
    (tryGetRow: Process -> IONode option)
    (setRow: Process -> IONode -> unit)
    (node: IONode)
    (view: ProcessView)
    =
    // Read the live process because a multi-import can perform several additions
    // before ProcessView is rebuilt.
    view.Processes.Values
    |> Seq.tryFind (tryGetRow >> Option.isNone)
    |> Option.defaultWith (fun () -> createRowMember view)
    |> fun processObject -> setRow processObject node

let addInput =
    addRow _.Input (fun processObject node -> processObject.SetInput node)

let addOutput =
    addRow _.Output (fun processObject node -> processObject.SetOutput node)

let removeInput node view =
    removeRow
        _.Inputs
        (fun processObject -> processObject.ClearInput())
        (fun processObject -> processObject.Output.IsSome)
        node
        view

let removeOutput node view =
    removeRow
        _.Outputs
        (fun processObject -> processObject.ClearOutput())
        (fun processObject -> processObject.Input.IsSome)
        node
        view

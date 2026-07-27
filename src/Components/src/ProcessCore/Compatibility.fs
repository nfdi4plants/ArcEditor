[<AutoOpen>]
module ProcessCore.Compatibility

open ProcessCore

// ProcessCore 0.0.10 models process I/O as singular optional values. Keep the
// collection-shaped surface used by the UI while it transitions to that model.
type Process with
    member this.Inputs = this.Input |> Option.toList |> ResizeArray
    member this.Outputs = this.Output |> Option.toList |> ResizeArray

    member this.ExecutesProtocol
        with get () = this.ExecutesRecipe
        and set value = this.ExecutesRecipe <- value

    member this.AddInput(node: IONode) = this.SetInput(node)
    member this.AddOutput(node: IONode) = this.SetOutput(node)
    member this.AddInputSample(sample: Sample) = this.SetInputSample(sample)
    member this.AddInputData(data: Data) = this.SetInputData(data)
    member this.AddOutputSample(sample: Sample) = this.SetOutputSample(sample)
    member this.AddOutputData(data: Data) = this.SetOutputData(data)

    member this.RemoveInput(node: IONode) =
        if this.Input |> Option.exists (fun current -> current.EqualTo(node)) then
            this.ClearInput()

    member this.RemoveOutput(node: IONode) =
        if this.Output |> Option.exists (fun current -> current.EqualTo(node)) then
            this.ClearOutput()

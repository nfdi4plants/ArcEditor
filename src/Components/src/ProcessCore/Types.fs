module Swate.Components.ProcessCore.Types

open System.Collections.Generic
open ProcessCore

/// Renderer representation of one logical process. Inputs and outputs belonging
/// to the same singular-I/O ProcessCore process share its integer key.
type ProcessView = {
    Processes: Dictionary<int, Process>
    Inputs: Dictionary<int, IONode>
    Outputs: Dictionary<int, IONode>
} with

    member this.Representative = this.Processes.[0]

/// Immutable renderer projection derived from one ProcessCore ARC.
type ArcView = {
    Processes: ProcessView array
    Samples: Sample array
    Data: Data array
    ProcessesByDataset: Dictionary<Dataset, ProcessView array>
    ProcessByRepresentative: Dictionary<Process, ProcessView>
}

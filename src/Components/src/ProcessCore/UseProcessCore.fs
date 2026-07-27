module Swate.Components.ProcessCore.UseProcessCore

open Feliz
open ProcessCore

type private Snapshot = { Arc: ARC; Version: int }

type private ProcessCoreStore(initialArc: ARC) =
    let mutable arc = initialArc
    let mutable version = 0
    let mutable snapshot = { Arc = initialArc; Version = version }
    let subscribers = ResizeArray<unit -> unit>()

    /// Publishes a new immutable snapshot after an in-place ProcessCore mutation.
    member private this.Notify() =
        version <- version + 1
        snapshot <- { Arc = arc; Version = version }

        for callback in subscribers do
            callback ()

    /// Returns the stable snapshot required by React's external-store contract.
    member this.GetSnapshot() = snapshot

    /// Registers a React store subscriber and returns its unsubscribe callback.
    member this.Subscribe(callback: unit -> unit) =
        subscribers.Add(callback)
        fun () -> subscribers.Remove(callback) |> ignore

    /// Replaces the tracked ARC when the owning component supplies a new reference.
    member this.SetArc(newArc: ARC) =
        if not (obj.ReferenceEquals(arc, newArc)) then
            arc <- newArc
            this.Notify()

    /// Applies an in-place mutation and notifies all subscribers.
    member this.Mutate(fn: ARC -> unit) =
        fn arc
        this.Notify()

/// Adapts a mutable ProcessCore ARC to React and returns its mutation function and revision.
[<Hook>]
let useProcessCore (initialArc: ARC) =
    let storeRef = React.useRef (ProcessCoreStore(initialArc))

    // Keep the store synchronized if the parent swaps the ARC instance.
    React.useEffect ((fun () -> storeRef.current.SetArc(initialArc)), [| box initialArc |])

    let snapshot =
        React.useSyncExternalStore (
            storeRef.current.Subscribe,
            storeRef.current.GetSnapshot,
            storeRef.current.GetSnapshot
        )

    snapshot.Arc, storeRef.current.Mutate, snapshot.Version

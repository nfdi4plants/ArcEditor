module Swate.Components.ProcessCore.UseProcessCore

open Feliz
open ProcessCore

type private Snapshot = { Arc: ARC; Version: int }

type private ProcessCoreStore(initialArc: ARC) =
    let mutable arc = initialArc
    let mutable version = 0
    let mutable snapshot = { Arc = initialArc; Version = version }
    let subscribers = ResizeArray<unit -> unit>()

    member private this.Notify() =
        version <- version + 1
        snapshot <- { Arc = arc; Version = version }

        for callback in subscribers do
            callback ()

    member this.GetSnapshot() = snapshot

    member this.Subscribe(callback: unit -> unit) =
        subscribers.Add(callback)
        fun () -> subscribers.Remove(callback) |> ignore

    member this.SetArc(newArc: ARC) =
        if not (obj.ReferenceEquals(arc, newArc)) then
            arc <- newArc
            this.Notify()

    member this.Mutate(fn: ARC -> unit) =
        fn arc
        this.Notify()

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

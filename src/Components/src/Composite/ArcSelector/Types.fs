module Swate.Components.Composite.ArcSelector.Types

type ARCPointer = {
    name: string
    path: string
    isActive: bool
} with

    static member create(name: string, path: string, isActive: bool) = {
        name = name
        path = path
        isActive = isActive
    }


type ArcSelectorRef = { toggle: unit -> unit }

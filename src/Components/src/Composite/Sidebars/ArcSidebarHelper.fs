module Swate.Components.Composite.Sidebars.ArcSidebarHelper

open System

let private nonEmpty (value: string) =
    if String.IsNullOrWhiteSpace value then
        None
    else
        Some(value.Trim())

let displayName (title: string option) (identifier: string) =
    title
    |> Option.bind nonEmpty
    |> Option.orElseWith (fun () -> nonEmpty identifier)
    |> Option.defaultValue "Untitled ARC"

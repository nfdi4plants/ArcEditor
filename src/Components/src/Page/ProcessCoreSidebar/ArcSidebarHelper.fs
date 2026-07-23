module Swate.Components.Page.ProcessCoreSidebar.ArcSidebarHelper

open System
open Feliz
open Swate.Components.Primitive.ContextMenu.Types

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

let createContextMenuItems
    (path: string)
    (onOpenFolder: (string -> unit) option)
    (onRevealFolder: (string -> unit) option)
    (onCopyPath: (string -> unit) option)
    =
    [
        for label, icon, callback in
            [
                "Open Folder", "swt:fluent--folder-open-24-regular", onOpenFolder
                "Reveal in File Explorer", "swt:fluent--folder-search-24-regular", onRevealFolder
                "Copy Path", "swt:fluent--copy-24-regular", onCopyPath
            ] do
            match callback with
            | None -> ()
            | Some callback ->
                ContextMenuItem(
                    text = Html.span label,
                    icon = Html.i [ prop.className $"swt:iconify {icon} swt:size-4" ],
                    onClick = fun _ -> callback path
                )
    ]

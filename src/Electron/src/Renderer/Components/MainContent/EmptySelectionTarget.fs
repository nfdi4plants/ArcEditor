module Renderer.Components.MainContent.EmptySelectionTarget

open Feliz
open Browser.Dom
open ProcessCore
open Swate.Electron.Shared.IPCTypes

[<ReactComponent(true)>]
let EmptySelectionTarget () =
    Html.div [
        prop.className "swt:size-full swt:flex swt:justify-center swt:items-center"
        prop.children [
            Html.h1 [
                prop.className
                    "swt:text-xl swt:font-semibold swt:text-transparent swt:bg-clip-text swt:bg-linear-to-r swt:from-primary swt:to-secondary"
                prop.text "Select files in the file tree to edit them."
            ]
        // Html.button [
        //     prop.text "Debug"
        //     prop.onClick (fun _ ->
        //         promise {
        //             match! Api.ipcProcessCoreApi.getArc() with
        //             | Ok yaml ->
        //                 let arc = ARC.fromYamlString yaml
        //                 console.log arc.Identifier
        //             | Error err -> console.error($"Error getting Arc: {err.Message}")
        //         }
        //         |> Promise.catch (fun ex -> console.error($"Unexpected error: {ex.Message}"))
        //         |> Promise.start
        //     )
        // ]
        ]
    ]

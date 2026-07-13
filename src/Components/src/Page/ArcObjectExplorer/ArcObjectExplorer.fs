namespace Swate.Components.Page.ArcObjectExplorer

open Fable.Core
open Feliz
open Swate.Components

[<Erase; Mangle(false)>]
type ArcObjectExplorer =

    [<ReactComponent(true)>]
    static member ArcObjectExplorer () =
        Html.div [
            prop.className "swt:h-full swt:w-full swt:bg-base-200"
            prop.text "ArcObjectExplorer"
        ]

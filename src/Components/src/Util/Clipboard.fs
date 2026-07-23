module Swate.Components.Util.Clipboard

open Fable.Core
open Fable.Core.JsInterop

let copyPath (path: string) =
    promise {
        try
            let windowObj: obj = Browser.Dom.window
            do! windowObj?navigator?clipboard?writeText (path)
        with error ->
            Browser.Dom.console.warn ($"Could not copy path: {path}", error)
    }
    |> Promise.start

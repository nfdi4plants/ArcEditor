namespace Main.Helper

[<RequireQualifiedAccess>]
module AppLogging =

    let printf id fmt =
        Printf.kprintf (fun s -> Browser.Dom.console.log ("[ArcEditor-" + string id + "] " + s)) fmt

    let failfn id fmt =
        Printf.kprintf (fun s -> failwith ("[ArcEditor-" + string id + "] " + s)) fmt

[<AutoOpen>]
module Main.ArcVaultTypes

/// Describes the outcome of an ARC lifecycle action performed by the controller.
[<RequireQualifiedAccess>]
type ArcOpenDisposition =
    | OpenedInCurrent of path: string
    | OpenedInNewWindow of path: string
    | FocusedExisting of path: string
    | CreatedInCurrent of path: string
    | CreatedInNewWindow of path: string

    member this.CreatedArcPath =
        match this with
        | CreatedInCurrent path
        | CreatedInNewWindow path -> Some path
        | OpenedInCurrent _
        | OpenedInNewWindow _
        | FocusedExisting _ -> None

    static member path =
        function
        | OpenedInCurrent path
        | OpenedInNewWindow path
        | FocusedExisting path
        | CreatedInCurrent path
        | CreatedInNewWindow path -> path

type ArcVaultFileSystemEvent = {
    EventName: string
    RelativePath: string
    AbsolutePath: string
}

namespace Swate.Components.Composite.InteractiveList


open System
open Fable.Core
open Feliz
// Workspace
open Types


module private InteractiveListFixtureData =

    /// Some fields extending label and icon; Maybe go for a file direction, as this should be designed similiar to a file explorer, with a label and an icon, and some data attached to it.
    type ExamplePayload = { type': string; size: int }

    let generateRandomEntry () =
        let fileIcons = [
            "swt:iconify-color swt:fluent-color--document-20"
            "swt:iconify-color swt:fluent-color--settings-20"
            "swt:iconify-color swt:fluent-color--person-20"
            "swt:iconify-color swt:fluent-color--code-block-20"
        ]

        let random = Random()
        let isFolder = random.Next(0, 2) = 0

        let payload =
            if isFolder then
                { type' = "folder"; size = 0 }
            else
                {
                    type' = "file"
                    size = random.Next(1, 10000)
                }

        let icon =
            if isFolder then
                "swt:iconify-color swt:fluent-color--document-folder-20"
            else // randomize icon for files.
                let index = random.Next(0, fileIcons.Length)
                fileIcons.[index]

        // also randomize icon for files. Use fixed icon for folders.
        {
            icon = icon
            label =
                if isFolder then
                    sprintf "Folder %d" (random.Next(1, 100))
                else
                    sprintf "File %d.txt" (random.Next(1, 100))
            data = payload
        }

    /// icon uses, iconofy syntax for tailwind plugin: swt:iconify swt:fluent--delete-12-regular
    /// Some folders, some files of different types
    let Data: InteractiveListData<ExamplePayload>[] =
        [|
            for i in 1..100 do
                yield generateRandomEntry ()
        |]
        |> Array.sortBy (fun entry -> entry.icon)

    /// Sort should return folders first, then the rest based on the label.
    let sort (entries: InteractiveListData<ExamplePayload>[]) =
        entries
        |> Array.sortBy (fun entry ->
            match entry.data.type' with
            | "folder" -> 0, entry.label
            | _ -> 1, entry.label
        )


[<Erase; Mangle(false)>]
type InteractiveListFixture =

    [<ReactComponent(true)>]
    static member InteractiveListFixture() =

        let lastClicked, setLastClicked =
            React.useState (None: InteractiveListData<InteractiveListFixtureData.ExamplePayload> option)

        let handleClick (entry: InteractiveListData<InteractiveListFixtureData.ExamplePayload>) =
            setLastClicked (Some entry)

        let lastClickedText =
            match lastClicked with
            | Some entry -> sprintf "Last clicked: %s" entry.label
            | None -> "No item clicked yet."

        Html.div [
            prop.className "swt:flex swt:flex-col swt:gap-4 swt:overflow-y-scroll"
            prop.children [
                Html.div lastClickedText
                InteractiveList.InteractiveList(
                    InteractiveListFixtureData.Data,
                    handleClick,
                    sortFn = InteractiveListFixtureData.sort,
                    styles = (InteractiveListStyles(tableClassName = "swt:table-xs"))
                )
            ]
        ]

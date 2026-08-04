namespace Swate.Components.Page.ArcObjectExplorer

open Fable.Core
open Feliz
open ProcessCore
open Swate.Components
open Swate.Components.Page.ArcObjectExplorer.Types
open Swate.Components.Page.ObjectBrowser.Types

[<Erase; Mangle(false)>]
type ArcObjectExplorer =

    [<ReactComponent(true)>]
    static member ArcObjectExplorer
        (
            arcStateCtx: StateUpdaterContext<ARC option>,
            arcView: Swate.Components.ProcessCore.Types.ArcView,
            ?selectedTarget: ExplorerTreeTarget,
            ?onOpenInMetadataEditor: ProcessCoreEntity -> unit,
            ?onOpenInTableEditor: ProcessCoreEntity -> unit
        ) =
        let selectionKey =
            selectedTarget
            |> Option.map (fun target ->
                let levels = target.Levels |> List.map _.RelationshipKey |> String.concat "/"
                $"{target.Dataset.key}/{levels}"
            )
            |> Option.defaultValue "root"

        Html.div [
            prop.key selectionKey
            prop.className "swt:relative swt:size-full swt:min-h-0 swt:min-w-0 swt:flex-1 swt:self-stretch"
            prop.children [
                ArcObjectExplorerContent.ArcObjectExplorerContent(
                    arcStateCtx,
                    arcView,
                    selectedTarget,
                    onOpenInMetadataEditor,
                    onOpenInTableEditor
                )
            ]
        ]

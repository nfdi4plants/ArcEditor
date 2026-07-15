namespace Swate.Components.Composite.ProcessCore

open Fable.Core
open Feliz
open Swate.Components.Composite.InteractiveList
open Swate.Components.Composite.InteractiveList.Types

[<Erase; Mangle(false)>]
type ProcessCoreMemberList =

    [<ReactComponent(true)>]
    static member Main() =
        InteractiveList.InteractiveList(
            ProcessCoreMemberCatalog.Items,
            ignore,
            styles = InteractiveListStyles(tableClassName = "swt:table-sm")
        )

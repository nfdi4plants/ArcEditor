namespace Swate.Components.Page.Metadata.FormComponents

open Fable.Core
open Feliz
open Swate.Components.Primitive.LayoutComponents

[<RequireQualifiedAccess; Erase; Mangle(false)>]
type CollectionCollapse =

    [<ReactComponent>]
    static member Main(title: string, subtitle: string, count: int, content: ReactElement, ?iconClass: string) =
        LayoutComponents.Collapse(
            [
                LayoutComponents.CollapseTitle(title, subtitle, count = string count, ?iconClass = iconClass)
            ],
            [ content ],
            stickyHeader = true
        )

namespace Swate.Components.Primitive

open Fable.Core
open Feliz

[<Erase; Mangle(false)>]
type Icon =

    [<ReactComponent>]
    static member Render(iconClassName: string, ?className: string, ?props: IReactProperty list) =
        Html.i [
            prop.ariaHidden true
            prop.className [
                "swt:iconify swt:shrink-0"
                iconClassName
                className |> Option.defaultValue "swt:size-6"
            ]
            match props with
            | Some props -> yield! props
            | None -> ()
        ]

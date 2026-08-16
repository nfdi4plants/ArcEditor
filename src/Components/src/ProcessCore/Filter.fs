namespace Swate.Components.ProcessCore

open Fable.Core
open Feliz

/// Reusable object-kind filter for ProcessCore explorer and editor views.
[<Erase; Mangle(false)>]
type Filter =

    /// Renders a compact optional selection and reports the selected option index.
    [<ReactComponent>]
    static member Filter
        (options: string array, selectedIndex: int option, onChange: int option -> unit, disabled: bool)
        =
        let filterRef = React.useElementRef ()

        Html.select [
            prop.ref filterRef
            prop.className "swt:select swt:select-bordered swt:select-sm swt:w-32"
            prop.style [ style.custom ("appearance", "none") ]
            prop.ariaLabel "Filter by object type"
            prop.disabled disabled
            prop.value (selectedIndex |> Option.map string |> Option.defaultValue "")
            prop.onMouseDown (fun (event: Browser.Types.MouseEvent) ->
                let filter = event.currentTarget :?> Browser.Types.HTMLElement

                if obj.ReferenceEquals(Browser.Dom.document.activeElement, filter) then
                    // Defer until after the native click, which would otherwise focus the select again.
                    Browser.Dom.window.setTimeout (filter.blur, 0) |> ignore
            )
            prop.onChange (fun (value: string) ->
                match System.Int32.TryParse value with
                | true, index -> onChange (Some index)
                | _ -> onChange None

                filterRef.current |> Option.iter (fun filter -> filter.blur ())
            )
            prop.children [
                Html.option [ prop.value ""; prop.text "All types" ]

                for index, label in Array.indexed options do
                    Html.option [ prop.value index; prop.text label ]
            ]
        ]

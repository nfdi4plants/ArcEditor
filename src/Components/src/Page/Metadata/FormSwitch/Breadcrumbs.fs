namespace Swate.Components.Page.Metadata.FormSwitch

open Fable.Core
open Feliz
open Types

[<Erase; Mangle(false)>]
type Breadcrumbs =

    [<ReactComponent>]
    static member BreadcrumbItem(path: FormSwitchPath, removeChildPaths: unit -> unit) =
        Html.li [
            prop.onClick (fun _ ->
                path.goto ()
                removeChildPaths ()
            )
            prop.children [
                Html.a [
                    Html.i [ prop.className [ path.icon ] ]
                    Html.text path.label
                ]
            ]
        ]

    [<ReactComponent(true)>]
    static member Breadcrumbs() =

        let formSwitchContext = Context.useFormSwitchContext ()

        Html.div [
            prop.className "swt:breadcrumbs swt:text-sm swt:min-h-fit"
            prop.children [
                Html.ul [
                    for i in 0 .. formSwitchContext.state.paths.Length - 1 do
                        let path = formSwitchContext.state.paths.[i]
                        // remove all paths after this one when clicked
                        let removeChildPaths () =
                            let newPaths = formSwitchContext.state.paths |> List.take (i + 1)

                            formSwitchContext.setState {
                                formSwitchContext.state with
                                    paths = newPaths
                            }

                        Breadcrumbs.BreadcrumbItem(path, removeChildPaths)

                ]
            ]
        ]

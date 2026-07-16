namespace Swate.Components.Page.Metadata.FormSwitch

open Fable.Core
open Feliz
open Swate.Components
open Types

[<Erase; Mangle(false)>]
type FormSwitch =


    [<ReactComponent(true)>]
    static member FormSwitch(children: ReactElement, ?initial: FormSwitchContext) =

        let formSwitch, setFormSwitch =
            React.useState (defaultArg initial (FormSwitchContext.init ()))

        let context: StateContext<FormSwitchContext> = {
            state = formSwitch
            setState = setFormSwitch
        }

        Context.FormSwitchCtx.Provider(context, [ Breadcrumbs.Breadcrumbs(); children ])

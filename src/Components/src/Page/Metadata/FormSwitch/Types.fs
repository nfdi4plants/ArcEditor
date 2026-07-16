module Swate.Components.Page.Metadata.FormSwitch.Types

open Fable.Core
open Feliz

type FormSwitchPath = {
    label: string
    icon: string
    goto: unit -> unit
}

type FormSwitchContext = {
    paths: FormSwitchPath list
} with

    static member init() : FormSwitchContext = { paths = [] }

    static member addPath (path: FormSwitchPath) (context: FormSwitchContext) : FormSwitchContext = {
        context with
            paths = context.paths @ [ path ]
    }

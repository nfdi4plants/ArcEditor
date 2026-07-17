module Swate.Components.Page.Metadata.FormSwitch.Context

open Fable.Core
open Feliz
open Types
open Swate.Components

let FormSwitchCtx =
    React.createContext<StateContext<FormSwitchContext>> (StateContext.init (FormSwitchContext.init ()))

[<Hook>]
let useFormSwitchContext () : StateContext<FormSwitchContext> = React.useContext FormSwitchCtx

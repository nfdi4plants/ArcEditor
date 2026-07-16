namespace Swate.Components.Page.Metadata.FormComponents

open System
open Feliz
open Fable.Core
open Browser.Types
open Swate.Components
open Swate.Components.Primitive
open Swate.Components.Primitive.LayoutComponents
open Swate.Components
open Swate.Components.Composite
open Swate.Components.Primitive
open Swate.Components.Primitive.BaseModal
open Swate.Components.Primitive.LayoutComponents
open Swate.Components.JsBindings
open Swate.Components.Page.Metadata.FormSwitch
open Swate.Components.Page.Metadata.FormSwitch.Types

[<Erase; Mangle(false)>]
type NavigableSequence =

    [<ReactComponent(true)>]
    static member NavigableSequence<'T>
        (
            inputs: ResizeArray<'T>,
            dataFn: 'T -> InteractiveList.Types.InteractiveListData<'T>,
            goto: 'T -> unit,
            back: unit -> unit,
            ?renderListItem: InteractiveList.Types.InteractiveListData<'T> -> ReactElement
        ) =

        let formSwitchCtx = Context.useFormSwitchContext ()

        let data = inputs |> Seq.map dataFn |> Seq.toArray

        let handleOnClick =
            fun (item: InteractiveList.Types.InteractiveListData<'T>) ->
                let nextPath: FormSwitchPath = {
                    goto = fun () -> goto item.data
                    icon = item.icon
                    label = item.label
                }

                let nextContext = formSwitchCtx.state |> FormSwitchContext.addPath nextPath
                formSwitchCtx.setState nextContext
                goto item.data


        InteractiveList.InteractiveList.InteractiveList(data, onClick = handleOnClick, ?rowRender = renderListItem)

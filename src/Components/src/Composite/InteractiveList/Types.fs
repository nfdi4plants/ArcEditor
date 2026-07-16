module Swate.Components.Composite.InteractiveList.Types

open Fable.Core
open Fable.Core.JS

[<Pojo>]
type InteractiveListStyles(?tableClassName: string) =
    member val tableClassName = tableClassName with get, set

type InteractiveListData<'A> = {
    icon: string
    label: string
    data: 'A
}

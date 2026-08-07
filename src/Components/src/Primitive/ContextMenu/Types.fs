module Swate.Components.Primitive.ContextMenu.Types

open Fable.Core
open Feliz

/// One inline action button on a context-menu row. A disabled action stays
/// visible but greyed out, with `disabledHint` explaining why it cannot run
/// here — for rows where the *entry* must read as one thing while its actions
/// differ in availability.
[<Global; AllowNullLiteral>]
type ContextMenuAction
    [<ParamObjectAttribute; Emit("$0")>]
    (
        icon: ReactElement,
        label: string,
        ?disabled: bool,
        ?disabledHint: string,
        ?onClick:
            {|
                buttonEvent: Browser.Types.MouseEvent
                spawnData: obj
            |}
                -> unit
    ) =
    member val icon = icon with get, set
    member val label = label with get, set
    member val disabled: bool = defaultArg disabled false with get, set
    member val disabledHint = disabledHint with get, set
    member val onClick = onClick with get, set

[<Global; AllowNullLiteral>]
type ContextMenuItem
    [<ParamObjectAttribute; Emit("$0")>]
    (
        ?text: ReactElement,
        ?icon: ReactElement,
        ?kbdbutton:
            {|
                element: ReactElement
                label: string
            |},
        ?isDivider: bool,
        ?onClick:
            {|
                buttonEvent: Browser.Types.MouseEvent
                spawnData: obj
            |}
                -> unit,
        ?actions: ContextMenuAction list
    ) =
    member val text = text with get, set
    member val icon = icon with get, set
    member val kbdbutton = kbdbutton with get, set
    member val isDivider: bool = defaultArg isDivider false with get, set
    member val onClick = onClick with get, set
    member val actions = actions with get, set

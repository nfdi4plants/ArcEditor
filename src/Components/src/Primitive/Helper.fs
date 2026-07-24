module Swate.Components.Primitive.Helper

open Swate.Components.Primitive

let sizeClass prefix size =
    let suffix =
        match size with
        | DaisyuiSize.XS -> "xs"
        | DaisyuiSize.SM -> "sm"
        | DaisyuiSize.MD -> "md"
        | DaisyuiSize.LG -> "lg"
        | DaisyuiSize.XL -> "xl"

    $"swt:{prefix}-{suffix}"

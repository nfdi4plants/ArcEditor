namespace Swate.Components.ProcessCore

open Fable.Core
open Feliz

[<Erase; Mangle(false)>]
type SearchBar =

    [<ReactComponent>]
    static member SearchBar(searchText: string, onSearch: string -> unit, disabled: bool) =
        Html.div [
            prop.className "swt:relative swt:flex swt:w-40 swt:shrink-0 swt:items-center"
            prop.testId "process-core-search"
            prop.children [
                Html.i [
                    prop.ariaHidden true
                    prop.className
                        "swt:iconify swt:fluent--search-20-regular swt:pointer-events-none swt:absolute swt:left-1 swt:size-3 swt:text-base-content/50"
                ]
                Html.input [
                    prop.type'.search
                    prop.className "swt:input swt:input-bordered swt:input-sm swt:w-full swt:pl-4"
                    prop.placeholder "Placeholder..."
                    prop.ariaLabel "Search objects"
                    prop.disabled disabled
                    prop.value searchText
                    prop.onChange onSearch
                ]
            ]
        ]

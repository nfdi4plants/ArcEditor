namespace Swate.Components.Composite.ProcessCore

open Fable.Core
open Feliz
open ProcessCore

[<Erase; Mangle(false)>]
type ObjectBrowser =

    [<ReactComponent(true)>]
    static member Main(arc: ARC, kind: MemberKind) =
        let descriptor = MemberCatalog.find kind
        let names = ObjectViewModel.getNames arc kind

        Html.section [
            prop.testId "process-core-object-browser"
            prop.ariaLabel descriptor.label
            prop.className "swt:size-full swt:min-h-0 swt:overflow-y-auto swt:bg-base-200 swt:p-6"
            prop.children [
                Html.h1 [
                    prop.className "swt:mb-6 swt:text-xl swt:font-semibold"
                    prop.text descriptor.label
                ]

                if Array.isEmpty names then
                    Html.div [
                        prop.testId "process-core-object-browser-empty"
                        prop.role.status
                        prop.className
                            "swt:flex swt:min-h-48 swt:items-center swt:justify-center swt:text-base-content/60"
                        prop.text $"No {descriptor.label} available in this ARC."
                    ]
                else
                    Html.ul [
                        prop.testId "process-core-object-list"
                        prop.className
                            "swt:grid swt:grid-cols-[repeat(auto-fill,minmax(8rem,1fr))] swt:items-start swt:gap-5"
                        prop.children [
                            for index, name in Array.indexed names do
                                Html.li [
                                    prop.key $"{descriptor.label}-{index}"
                                    prop.testId "process-core-object-item"
                                    prop.className
                                        "swt:flex swt:min-w-0 swt:flex-col swt:items-center swt:gap-2 swt:rounded-box swt:bg-base-100 swt:p-4 swt:text-center swt:shadow-sm"
                                    prop.children [
                                        Html.i [
                                            prop.ariaHidden true
                                            prop.className [ descriptor.icon; "swt:size-12 swt:shrink-0" ]
                                        ]
                                        Html.span [
                                            prop.className "swt:w-full swt:break-words swt:text-sm"
                                            prop.title name
                                            prop.text name
                                        ]
                                    ]
                                ]
                        ]
                    ]
            ]
        ]

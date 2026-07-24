namespace Swate.Components.Primitive.Tree

open Fable.Core
open Feliz
open Swate.Components.Primitive.Tree.Types

[<Erase; Mangle(false)>]
type Tree =

    [<ReactComponent>]
    static member private Node<'T>(node: TreeNode<'T>, onSelect: 'T -> unit) : ReactElement =
        let isExpanded, setIsExpanded = React.useState false
        let hasChildren = not (Array.isEmpty node.children)

        Html.li [
            prop.key node.key
            prop.role "treeitem"
            prop.className "swt:w-full"
            if hasChildren then
                prop.ariaExpanded isExpanded
            prop.children [
                Html.button [
                    prop.type'.button
                    prop.className
                        "swt:grid swt:w-full swt:grid-cols-[1rem_1.25rem_minmax(0,1fr)] swt:items-center swt:gap-2 swt:text-left"
                    prop.title node.label
                    prop.ariaLabel node.label
                    if hasChildren then
                        prop.ariaExpanded isExpanded
                    prop.onClick (fun _ ->
                        node.data |> Option.iter onSelect

                        if hasChildren then
                            setIsExpanded (not isExpanded)
                    )
                    prop.children [
                        if hasChildren then
                            Html.i [
                                prop.className [
                                    "swt:iconify swt:size-4 swt:shrink-0"
                                    if isExpanded then
                                        "swt:fluent--chevron-down-20-filled"
                                    else
                                        "swt:fluent--chevron-right-20-filled"
                                ]
                            ]
                        else
                            Html.span [ prop.className "swt:size-4 swt:shrink-0" ]

                        node.icon
                        |> Option.map (fun icon ->
                            Html.i [
                                prop.className [ icon; "swt:size-5 swt:shrink-0 swt:justify-self-center" ]
                            ]
                        )
                        |> Option.defaultValue (Html.span [ prop.className "swt:size-5" ])

                        Html.span [
                            prop.className "swt:min-w-0 swt:truncate swt:text-left"
                            prop.text node.label
                        ]
                    ]
                ]

                if hasChildren && isExpanded then
                    Html.ul [
                        prop.role "group"
                        prop.className "swt:w-full"
                        prop.children [
                            for child in node.children do
                                Tree.Node(child, onSelect)
                        ]
                    ]
            ]
        ]

    [<ReactComponent(true)>]
    static member Main<'T>
        (nodes: TreeNode<'T> array, onSelect: 'T -> unit, ?className: string, ?testId: string)
        : ReactElement =
        Html.ul [
            prop.role "tree"
            prop.className [
                "swt:menu swt:menu-xs swt:w-auto"
                className |> Option.defaultValue ""
            ]
            match testId with
            | Some testId -> prop.testId testId
            | None -> ()
            prop.children [
                for node in nodes do
                    Tree.Node(node, onSelect)
            ]
        ]

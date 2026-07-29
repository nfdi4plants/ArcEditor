namespace Swate.Components.Primitive.Tree

open Fable.Core
open Feliz
open Swate.Components.Primitive.Tree.Types

[<Erase; Mangle(false)>]
/// Accessible recursive tree renderer with local expansion state and optional selection data.
type Tree =

    /// Recursively renders one tree item and its expanded descendants.
    [<ReactMemoComponent>]
    static member private Node<'T>
        (
            node: TreeNode<'T>,
            onActivate: 'T -> bool option -> unit,
            expandedKeys: Set<string>,
            toggleExpanded: string -> unit,
            ?key: string
        ) : ReactElement =
        let isExpanded = expandedKeys.Contains node.key
        let hasChildren = not (Array.isEmpty node.children)

        Html.li [
            prop.key (defaultArg key node.key)
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
                        node.data
                        |> Option.iter (fun data ->
                            onActivate data (if hasChildren then Some(not isExpanded) else None)
                        )

                        if hasChildren then
                            toggleExpanded node.key
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
                                Tree.Node(child, onActivate, expandedKeys, toggleExpanded, key = child.key)
                        ]
                    ]
            ]
        ]

    /// Renders a tree whose entity rows can be selected independently of expansion.
    [<ReactMemoComponent>]
    static member Main<'T>
        (nodes: TreeNode<'T> array, onActivate: 'T -> bool option -> unit, ?className: string, ?testId: string)
        : ReactElement =
        let (expandedKeys: Set<string>), setExpandedKeys =
            React.useStateWithUpdater Set.empty

        let onActivateRef = React.useRef onActivate
        onActivateRef.current <- onActivate

        let stableOnActivate =
            React.useCallback (
                (fun (data: 'T) (nextExpansion: bool option) -> onActivateRef.current data nextExpansion),
                [||]
            )

        let toggleExpanded =
            React.useCallback (
                (fun key ->
                    setExpandedKeys (fun current ->
                        if current.Contains key then
                            current.Remove key
                        else
                            current.Add key
                    )
                ),
                [||]
            )

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
                    Tree.Node(node, stableOnActivate, expandedKeys, toggleExpanded, key = node.key)
            ]
        ]

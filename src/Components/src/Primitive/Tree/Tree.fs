namespace Swate.Components.Primitive.Tree

open Fable.Core
open Feliz
open Swate.Components.Primitive.Tree.Types

/// Accessible recursive tree renderer with local expansion state and optional selection data.
[<Erase; Mangle(false)>]
type Tree =

    /// Recursively renders one tree item and its expanded descendants.
    [<ReactMemoComponent>]
    static member private Node<'T>
        (
            node: TreeNode<'T>,
            onActivate: string -> 'T -> unit,
            expandedKeys: Set<string>,
            selectedKey: string option,
            toggleExpanded: string -> unit,
            contextMenuIndex: (string -> int option) option,
            ?key: string
        ) : ReactElement =
        let isExpanded = expandedKeys.Contains node.key
        let isSelected = selectedKey = Some node.key
        let hasChildren = not (Array.isEmpty node.children)

        Html.li [
            prop.key (defaultArg key node.key)
            prop.role "treeitem"
            prop.className "swt:w-full"
            if node.data.IsSome then
                prop.ariaSelected isSelected
            match contextMenuIndex |> Option.bind (fun getIndex -> getIndex node.key) with
            | Some index -> prop.custom ("data-interactive-list-index", index)
            | None -> ()
            if hasChildren then
                prop.ariaExpanded isExpanded
            prop.children [
                Html.div [
                    prop.className [
                        "swt:grid swt:w-full swt:grid-cols-[1rem_1.25rem_minmax(0,1fr)] swt:items-center swt:gap-2 swt:text-left"
                        if isSelected then
                            "swt:bg-base-300"
                    ]
                    prop.children [
                        if hasChildren then
                            Html.button [
                                prop.type'.button
                                prop.className "swt:flex swt:size-4 swt:items-center swt:justify-center"
                                prop.ariaLabel (
                                    if isExpanded then
                                        $"Collapse {node.label}"
                                    else
                                        $"Expand {node.label}"
                                )
                                prop.ariaExpanded isExpanded
                                prop.onClick (fun event ->
                                    event.stopPropagation ()
                                    toggleExpanded node.key
                                )
                                prop.children [
                                    Html.i [
                                        prop.className [
                                            "swt:iconify swt:size-4 swt:shrink-0"
                                            if isExpanded then
                                                "swt:fluent--chevron-down-20-filled"
                                            else
                                                "swt:fluent--chevron-right-20-filled"
                                        ]
                                    ]
                                ]
                            ]
                        else
                            Html.span [ prop.className "swt:size-4 swt:shrink-0" ]

                        Html.button [
                            prop.type'.button
                            prop.className
                                "swt:col-span-2 swt:grid swt:min-w-0 swt:grid-cols-[1.25rem_minmax(0,1fr)] swt:items-center swt:gap-2 swt:text-left"
                            prop.title node.label
                            prop.ariaLabel node.label
                            prop.disabled (node.data.IsNone && not hasChildren)
                            prop.onClick (fun _ ->
                                match node.data with
                                | Some data -> onActivate node.key data
                                | None when hasChildren -> toggleExpanded node.key
                                | None -> ()
                            )
                            prop.children [
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
                    ]
                ]

                if hasChildren && isExpanded then
                    Html.ul [
                        prop.role "group"
                        prop.className "swt:w-full"
                        prop.children [
                            for child in node.children do
                                Tree.Node(
                                    child,
                                    onActivate,
                                    expandedKeys,
                                    selectedKey,
                                    toggleExpanded,
                                    contextMenuIndex,
                                    key = child.key
                                )
                        ]
                    ]
            ]
        ]

    /// Renders a tree whose entity rows can be selected independently of expansion.
    [<ReactMemoComponent>]
    static member Tree<'T>
        (
            nodes: TreeNode<'T> array,
            onActivate: 'T -> unit,
            ?contextMenuIndex: string -> int option,
            ?onExpandedKeysChange: Set<string> -> unit,
            ?className: string,
            ?testId: string
        ) : ReactElement =
        let (expandedKeys: Set<string>), setExpandedKeys =
            React.useStateWithUpdater Set.empty

        let selectedKey, setSelectedKey = React.useState<string option> None

        let onActivateRef = React.useRef onActivate
        onActivateRef.current <- onActivate

        let stableOnActivate =
            React.useCallback (
                (fun key (data: 'T) ->
                    setSelectedKey (Some key)
                    onActivateRef.current data
                ),
                [||]
            )

        let toggleExpanded =
            React.useCallback (
                (fun key ->
                    let updated =
                        if expandedKeys.Contains key then
                            expandedKeys.Remove key
                        else
                            expandedKeys.Add key

                    setExpandedKeys (fun _ -> updated)
                    onExpandedKeysChange |> Option.iter (fun onChange -> onChange updated)
                ),
                [| box expandedKeys; box onExpandedKeysChange |]
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
                    Tree.Node(
                        node,
                        stableOnActivate,
                        expandedKeys,
                        selectedKey,
                        toggleExpanded,
                        contextMenuIndex,
                        key = node.key
                    )
            ]
        ]

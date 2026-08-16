module Swate.Components.Primitive.Tree.Types

/// Describes one renderable tree item. Nodes with data are selectable entities;
/// nodes without data act as structural relationship folders.
type TreeNode<'T> = {
    /// Stable path-like identity used for React rendering and expansion state.
    key: string
    /// Human-readable text shown in the tree row.
    label: string
    /// Optional Iconify CSS classes for the row icon.
    icon: string option
    /// Optional application value emitted when the row is selected.
    data: 'T option
    /// Child nodes displayed when this node is expanded.
    children: TreeNode<'T> array
}

/// Returns the deepest expanded nodes that are still reachable through expanded ancestors.
let deepestExpandedNodes expandedKeys (nodes: TreeNode<'T> array) =
    let rec collect depth (node: TreeNode<'T>) = seq {
        if Set.contains node.key expandedKeys then
            yield depth, node

            for child in node.children do
                yield! collect (depth + 1) child
    }

    let expandedNodes = nodes |> Seq.collect (collect 0) |> Seq.toArray

    if Array.isEmpty expandedNodes then
        None
    else
        let deepestLevel = expandedNodes |> Array.map fst |> Array.max

        expandedNodes
        |> Array.choose (fun (depth, node) -> if depth = deepestLevel then Some node else None)
        |> Some

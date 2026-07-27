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

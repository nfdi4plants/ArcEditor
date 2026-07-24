module Swate.Components.Primitive.Tree.Types

type TreeNode<'T> = {
    key: string
    label: string
    icon: string option
    data: 'T option
    children: TreeNode<'T> array
}

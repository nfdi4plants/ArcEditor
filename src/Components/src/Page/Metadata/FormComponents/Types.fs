module Swate.Components.Page.Metadata.FormComponents.Types

type RelationshipMutations<'T> = {
    Add: 'T -> unit
    Remove: 'T -> unit
    Reorder: ResizeArray<'T> -> unit
}

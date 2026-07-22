module Swate.Components.Page.Metadata.FormComponents.MetadataRelationship

open ProcessCore
open Swate.Components.Page.Metadata.FormComponents.Types

let create (mutate: (ARC -> unit) -> unit) (items: ResizeArray<'T>) add remove : RelationshipMutations<'T> = {
    Add = fun item -> mutate (fun _ -> add item)
    Remove = fun item -> mutate (fun _ -> remove item)
}

module Swate.Components.Page.Metadata.FormComponents.ImportCatalogContext

open Feliz
open Swate.Components.ProcessCore
open Swate.Components.ProcessCore.Types

module ImportCatalogContextHelper =

    /// Traverses the current ARC and builds the candidate snapshot. Types that are not
    /// exposed by a direct ARC traversal are collected through their owning relationships.
    let create = EntityCatalog.createImportCatalog

/// Provided by ArcObjectEditor so relationship components do not need to know ARC ownership.
/// None also allows the reusable metadata components to render outside ArcObjectEditor.
type ImportContext = {
    Catalog: ImportCatalog
    RunAsyncMutation: ((unit -> unit) -> Fable.Core.JS.Promise<unit>) option
}

let ImportCatalogCtx = React.createContext<ImportContext option> None

[<Hook>]
let useImportCatalogCtx () = React.useContext ImportCatalogCtx

module Swate.Components.Page.Metadata.FormComponents.ImportCatalogContext

open Feliz
open Swate.Components.ProcessCore.Types

/// Provided by ArcObjectEditor so relationship components do not need to know ARC ownership.
/// None also allows the reusable metadata components to render outside ArcObjectEditor.
type ImportContext = {
    Catalog: ImportCatalog
    RunAsyncMutation: ((unit -> unit) -> Fable.Core.JS.Promise<unit>) option
}

/// React context supplying import candidates and the editor's persistence boundary.
let ImportCatalogCtx = React.createContext<ImportContext option> None

/// Returns the import context when a metadata component is hosted by an ARC editor.
[<Hook>]
let useImportCatalogCtx () = React.useContext ImportCatalogCtx

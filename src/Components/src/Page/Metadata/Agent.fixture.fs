namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
open ProcessCore
open Swate.Components
open Swate.Components.Primitive.LayoutComponents
open Swate.Components.Page.Metadata
open Swate.Components.Page.Metadata.FormComponents
open Swate.Components.Composite.InteractiveList.Types


module AgentMetadataFixtureHelper =

    [<RequireQualifiedAccess>]
    type ProcessCoreObjects =
        | ARC
        | Agent of id: string
        | Annotation of ProcessCore.Annotation

    /// This type should be replaced by PageState.XX in Electron
    [<RequireQualifiedAccess>]
    type PageState = ProcessCoreObject of ProcessCoreObjects

open AgentMetadataFixtureHelper

[<Erase; Mangle(false)>]
type AgentMetadataFixture =

    [<ReactComponent(true)>]
    static member AgentMetadataFixture() =
        let annotations = [
            ProcessCore.Annotation("Annotation 1", "Value 1")
            ProcessCore.Annotation("Annotation 2", "Value 2")
        ]

        let agent = ProcessCore.Agent("Cool Agent", additionalProperty = annotations)

        let agent, setAgent = React.useState (agent)

        AgentMetadata.AgentMetadata(agent, setAgent)

namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
open ProcessCore
open Swate.Components
open Swate.Components.Primitive.LayoutComponents
open Swate.Components.Page.Metadata
open Swate.Components.Page.Metadata.FormComponents


[<Erase; Mangle(false)>]
type AgentMetadataFixture =

    [<ReactComponent(true)>]
    static member AgentMetadataFixture() =

        let agent = ProcessCore.Agent("Cool Agent")

        let arc = ProcessCore.ARC("Cool Arc", agents = [ agent ])

        let arc, setArc = React.useState (arc)

        let arcAgent = arc.Agents |> Seq.tryHead

        match arcAgent with
        | None -> Html.div [ prop.text "No agent found in the ARC." ]
        | Some arcAgent ->
            let setAgent =
                fun (newAgent: ProcessCore.Agent) -> Browser.Dom.window.alert ($"Agent updated to: {newAgent}")

            AgentMetadata.AgentMetadata(arcAgent, setAgent)

namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
open ProcessCore
open Swate.Components
open Swate.Components.Primitive.LayoutComponents
open Swate.Components.Page.Metadata
open Swate.Components.Page.Metadata.FormComponents
open Swate.Components.Composite.InteractiveList.Types


module ArcMetadataFixtureHelper =

    [<RequireQualifiedAccess>]
    type ProcessCoreObjects =
        | ARC
        | Agent of id: string
        | Annotation of ProcessCore.Annotation

    /// This type should be replaced by PageState.XX in Electron
    [<RequireQualifiedAccess>]
    type PageState = ProcessCoreObject of ProcessCoreObjects

open ArcMetadataFixtureHelper

[<Erase; Mangle(false)>]
type ArcMetadataFixture =

    [<ReactComponent(true)>]
    static member ArcMetadataFixture() =

        let annotations = [
            ProcessCore.Annotation("Annotation 1", "Value 1")
            ProcessCore.Annotation("Annotation 2", "Value 2")
        ]

        let agent = ProcessCore.Agent("Cool Agent", additionalProperty = annotations)

        let arc = ProcessCore.ARC("Cool Arc", agents = [ agent ])

        let arc, setArc = React.useState (arc)

        let dataFnAgents =
            fun (agent: ProcessCore.Agent) -> {
                data = agent
                label =
                    if agent.GivenName = "" then
                        "(Unnamed)"
                    else
                        agent.GivenName
                icon = "swt:iconify-color swt:fluent-color--person-20"
            }

        // let initialFormSwitchContext = {
        //     Swate.Components.Page.Metadata.FormSwitch.Types.FormSwitchContext.init () with
        //         paths = [
        //             {
        //                 label = "ARC"
        //                 icon = "swt:iconify-color swt:fluent-color--diversity-20"
        //                 goto = fun () -> ProcessCoreObjects.ARC arc |> goto
        //             }
        //         ]
        // }

        Html.div [
            prop.className "swt:p-4 swt:w-screen swt:h-screen swt:flex swt:flex-col swt:overflow-hidden"
            prop.children []
        ]

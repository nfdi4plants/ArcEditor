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
        | ARC of ProcessCore.ARC
        | Agent of ProcessCore.Agent
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

        let currentView, setCurrentView =
            React.useState (
                ArcMetadataFixtureHelper.PageState.ProcessCoreObject(
                    ArcMetadataFixtureHelper.ProcessCoreObjects.ARC arc
                )
            )

        let backToRoot =
            fun () -> ProcessCoreObjects.ARC arc |> PageState.ProcessCoreObject |> setCurrentView

        let goto =
            (fun processCoreObjects -> PageState.ProcessCoreObject processCoreObjects |> setCurrentView)

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

        let gotoAgent (agent: ProcessCore.Agent) = ProcessCoreObjects.Agent agent |> goto

        let gotoAnnotation (annotation: ProcessCore.Annotation) =
            ProcessCoreObjects.Annotation annotation |> goto

        let initialFormSwitchContext = {
            Swate.Components.Page.Metadata.FormSwitch.Types.FormSwitchContext.init () with
                paths = [
                    {
                        label = "ARC"
                        icon = "swt:iconify-color swt:fluent-color--diversity-20"
                        goto = fun () -> ProcessCoreObjects.ARC arc |> goto
                    }
                ]
        }

        Html.div [
            prop.className "swt:p-4 swt:w-screen swt:h-screen swt:flex swt:flex-col swt:overflow-hidden"
            prop.children [
                FormSwitch.FormSwitch.FormSwitch(
                    initial = initialFormSwitchContext,
                    children =
                        match currentView with
                        | PageState.ProcessCoreObject(ProcessCoreObjects.ARC arc) ->
                            // Placeholder, this will be replaced by the ARC metadata form
                            NavigableSequence.NavigableSequence<ProcessCore.Agent>(
                                ResizeArray arc.Agents,
                                dataFn = dataFnAgents,
                                goto = gotoAgent,
                                back = backToRoot
                            )
                        | PageState.ProcessCoreObject(ProcessCoreObjects.Agent agent) ->
                            AgentMetadata.AgentMetadata(
                                agent,
                                (fun updatedAgent ->
                                    /// This is very dirty. Must be updated with correct update logic
                                    let newArc = ProcessCore.ARC(arc.Identifier, agents = [ updatedAgent ])
                                    setArc newArc

                                    ProcessCoreObjects.ARC newArc |> PageState.ProcessCoreObject |> setCurrentView
                                ),
                                gotoAnnotation,
                                backToRoot
                            )
                        | PageState.ProcessCoreObject(ProcessCoreObjects.Annotation annotation) ->
                            // Placeholder, this will be replaced by the Annotation metadata form
                            LayoutComponents.Section [
                                Html.h2 "Annotation Metadata"
                                Html.p $"Name: {annotation.Name}"
                                Html.p $"Value: {annotation.Value}"
                            ]
                )
            ]
        ]

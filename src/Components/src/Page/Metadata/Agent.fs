namespace Swate.Components.Page.Metadata

open Feliz
open ProcessCore
open Fable.Core
open Swate.Components.Page.Metadata
open Swate.Components.Primitive.LayoutComponents
open Swate.Components.Composite.InteractiveList.Types

module AgentMetadataHelper =

    [<RequireQualifiedAccess>]
    type ProcessCoreObjects =
        | Root
        | Annotation of ProcessCore.Annotation

open AgentMetadataHelper

[<Erase; Mangle(false)>]
type AgentMetadata =

    [<ReactComponent>]
    static member AgentView
        (
            agent: ProcessCore.Agent,
            setAgent: (ProcessCore.Agent -> unit),
            navigateToAnnotation: InteractiveListData<ProcessCore.Annotation> -> unit
        ) =
        let dataFnAdditionalProperty
            (annotation: ProcessCore.Annotation)
            : InteractiveListData<ProcessCore.Annotation> =
            {
                data = annotation
                label =
                    if annotation.Name = "" then
                        "(Unnamed)"
                    else
                        annotation.Name
                icon = "swt:iconify-color swt:fluent-color--comment-multiple-20"
            }

        React.Fragment [
            FormComponents.TextInput.TextInput(
                agent.Id |> Option.defaultValue "",
                (fun input ->
                    let nextAgent = agent.Copy(id = input)
                    setAgent nextAgent
                ),
                label = "Id",
                disabled = true
            )
            FormComponents.TextInput.TextInput(
                agent.GivenName,
                (fun newGivenName ->
                    let nextAgent = agent.Copy(givenName = newGivenName)
                    setAgent nextAgent
                ),
                label = "Given Name"
            )
            FormComponents.TextInput.TextInput(
                agent.FamilyName |> Option.defaultValue "",
                (fun input ->
                    let nextAgent = agent.Copy(familyName = input)
                    setAgent nextAgent
                ),
                label = "Family Name"
            )
            FormComponents.TextInput.TextInput(
                agent.Email |> Option.defaultValue "",
                (fun input ->
                    let nextAgent = agent.Copy(email = input)
                    setAgent nextAgent
                ),
                label = "Email"
            )
            // TODO Affiliation is an Organization, which is a complex type. We need a way to select or create an Organization.
            Html.div
                "Placeholder for Affiliation (Organization) input. This should be a dropdown or a search field to select an existing Organization or create a new one."
            FormComponents.TextInput.TextInput(
                agent.Identifier |> Option.defaultValue "",
                (fun input ->
                    let nextAgent = agent.Copy(identifier = input)
                    setAgent nextAgent
                ),
                label = "Affiliation"
            )
            FormComponents.NavigableSequence.NavigableSequence(
                ResizeArray agent.AdditionalProperty,
                dataFn = dataFnAdditionalProperty,
                onClick = navigateToAnnotation
            )
            // TODO: JobTitles is a list of strings. We need a way to add/edit/remove job titles.
            Html.div "Placeholder for Job Titles input. This should allow adding/editing/removing job titles."
        ]

    [<ReactComponent(true)>]
    static member AgentMetadata(agent: ProcessCore.Agent, setAgent: ProcessCore.Agent -> unit) =

        let currentView, setCurrentView = React.useState ProcessCoreObjects.Root

        LayoutComponents.Section [
            LayoutComponents.BoxedField(
                "Agent Metadata",
                content = [
                    match currentView with
                    | ProcessCoreObjects.Root ->
                        AgentMetadata.AgentView(
                            agent,
                            setAgent,
                            (fun annotationData -> setCurrentView (ProcessCoreObjects.Annotation annotationData.data))
                        )
                    | ProcessCoreObjects.Annotation annotation ->
                        Html.div [
                            Html.h1 "Placeholder needs: Annotation View"
                            Html.button [ // This button should stay in whatever form, we need a way to go back to the Agent view. This is a temporary solution.
                                prop.className "swt:link"
                                prop.text "Back to Agent Metadata"
                                prop.onClick (fun _ -> setCurrentView ProcessCoreObjects.Root)
                            ]
                            Html.div [
                                Html.h2 "Annotation Details"
                                Html.p $"Name: {annotation.Name}"
                                Html.p $"Value: {annotation.Value}"
                            ]
                        ]
                ]
            )
        ]

namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
open ProcessCore
open Swate.Components
open Swate.Components.Primitive.LayoutComponents
open Swate.Components.Page.Metadata
open Swate.Components.Page.Metadata.FormComponents

[<Erase; Mangle(false)>]
type AgentMetadata =

    [<ReactComponent(true)>]
    static member AgentMetadata(agent: ProcessCore.Agent, setAgent: ProcessCore.Agent -> unit) =

        let updateAgent (updateFn: ProcessCore.Agent -> ProcessCore.Agent) =
            let copy =
                ProcessCore.Agent(
                    agent.GivenName,
                    ?id = agent.Id,
                    ?familyName = agent.FamilyName,
                    ?email = agent.Email,
                    ?affiliation = agent.Affiliation,
                    ?identifier = agent.Identifier,
                    jobTitles = agent.JobTitles,
                    ?additionalName = agent.AdditionalName,
                    ?address = agent.Address,
                    ?telephone = agent.Telephone,
                    additionalProperty = agent.AdditionalProperty
                )

            let updatedAgent = updateFn copy
            setAgent updatedAgent

        LayoutComponents.Section [
            LayoutComponents.BoxedField(
                "Agent Metadata",
                content = [
                    FormComponents.TextInput.TextInput(
                        agent.Id |> Option.defaultValue "",
                        (fun input ->
                            updateAgent (fun a ->
                                a.Id <- Some input
                                a
                            )
                        ),
                        label = "Id",
                        disabled = true
                    )
                    FormComponents.TextInput.TextInput(
                        agent.GivenName,
                        (fun newGivenName ->
                            updateAgent (fun a ->
                                a.GivenName <- newGivenName
                                a
                            )
                        ),
                        label = "Given Name"
                    )
                    FormComponents.TextInput.TextInput(
                        agent.FamilyName |> Option.defaultValue "",
                        (fun input ->
                            updateAgent (fun a ->
                                a.FamilyName <- Some input
                                a
                            )
                        ),
                        label = "Family Name"
                    )
                    FormComponents.TextInput.TextInput(
                        agent.Email |> Option.defaultValue "",
                        (fun input ->
                            updateAgent (fun a ->
                                a.Email <- Some input
                                a
                            )
                        ),
                        label = "Email"
                    )
                    // TODO Affiliation is an Organization, which is a complex type. We need a way to select or create an Organization.
                    Html.div
                        "Placeholder for Affiliation (Organization) input. This should be a dropdown or a search field to select an existing Organization or create a new one."
                    FormComponents.TextInput.TextInput(
                        agent.Identifier |> Option.defaultValue "",
                        (fun input ->
                            updateAgent (fun a ->
                                a.Identifier <- Some input
                                a
                            )
                        ),
                        label = "Affiliation"
                    )
                    // TODO: AdtionalProperty is a complex type. We need a way to add/edit/remove additional properties.
                    Html.div
                        "Placeholder for Additional Properties input. This should allow adding/editing/removing additional properties."
                    // TODO: JobTitles is a list of strings. We need a way to add/edit/remove job titles.
                    Html.div "Placeholder for Job Titles input. This should allow adding/editing/removing job titles."
                ]
            )
        ]

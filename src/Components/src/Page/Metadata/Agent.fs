namespace Swate.Components.Page.Metadata

open Feliz
open ProcessCore
open Fable.Core
open Swate.Components.Shared
open Swate.Components.Primitive.LayoutComponents
open Swate.Components.Page.ObjectBrowser.Types
open Swate.Components.Page.Metadata.FormComponents


[<Erase; Mangle(false)>]
type AgentMetadata =

    [<ReactComponent(true)>]
    static member AgentView(agent: Agent, mutate: (ARC -> unit) -> unit, ?onNavigate: ProcessCoreEntityValue -> unit) =
        let navigate = defaultArg onNavigate ignore

        let setAffiliation affiliation =
            mutate (fun _ -> agent.Affiliation <- affiliation)

        let additionalProperties =
            MetadataRelationship.create
                mutate
                agent.AdditionalProperty
                agent.AddAdditionalProperty
                agent.RemoveAdditionalProperty

        let jobTitles =
            MetadataRelationship.create mutate agent.JobTitles agent.AddJobTitle agent.RemoveJobTitle

        LayoutComponents.Section [
            LayoutComponents.BoxedField(
                "Agent Metadata",
                content = [
                    TextInput.TextInput(
                        agent.Id |> Option.defaultValue "",
                        (fun value ->
                            mutate (fun _ -> agent.Id <- Option.whereNot System.String.IsNullOrWhiteSpace value)
                        ),
                        label = "Id"
                    )
                    TextInput.TextInput(
                        agent.GivenName,
                        (fun value -> mutate (fun _ -> agent.GivenName <- value)),
                        label = "Given Name",
                        // ProcessCore hotfix: prevent clearing this mandatory primary field.
                        validator = Swate.Components.ProcessCoreHotfixes.required "Given name"
                    )
                    TextInput.TextInput(
                        agent.FamilyName |> Option.defaultValue "",
                        (fun value ->
                            mutate (fun _ ->
                                agent.FamilyName <- Option.whereNot System.String.IsNullOrWhiteSpace value
                            )
                        ),
                        label = "Family Name"
                    )
                    TextInput.TextInput(
                        agent.Email |> Option.defaultValue "",
                        (fun value ->
                            mutate (fun _ -> agent.Email <- Option.whereNot System.String.IsNullOrWhiteSpace value)
                        ),
                        label = "Email"
                    )
                    (NestedMetadataInput.OptionalRow(
                        "Affiliation",
                        agent.Affiliation,
                        (fun () -> Organization("New Organisation", System.Guid.NewGuid().ToString())),
                        setAffiliation,
                        "swt:iconify-color swt:fluent-color--organization-20",
                        (fun organization -> NestedMetadataInput.nonEmptyOr "Unnamed organization" organization.Name),
                        (ProcessCoreEntityValue.Organization >> navigate),
                        imports = (fun catalog -> catalog.Organizations)
                    ))
                    TextInput.TextInput(
                        agent.Identifier |> Option.defaultValue "",
                        (fun value ->
                            mutate (fun _ ->
                                agent.Identifier <- Option.whereNot System.String.IsNullOrWhiteSpace value
                            )
                        ),
                        label = "Identifier"
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        agent.AdditionalProperty,
                        (fun () -> Annotation("")),
                        "Additional Properties",
                        NestedMetadataInput.Annotation,
                        (ProcessCoreEntityValue.Annotation >> navigate),
                        reorderItems = additionalProperties.Reorder,
                        imports = (fun catalog -> catalog.Annotations),
                        duplicateCandidates = (fun catalog -> catalog.Annotations),
                        addItem = additionalProperties.Add,
                        removeItem = additionalProperties.Remove
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        agent.JobTitles,
                        (fun () -> DefinedTerm("New Defined Term")),
                        "Job Titles",
                        (fun jobTitle ->
                            let _, label = NestedMetadataInput.DefinedTerm jobTitle
                            "swt:iconify swt:fluent--briefcase-20-regular", label
                        ),
                        (ProcessCoreEntityValue.DefinedTerm >> navigate),
                        reorderItems = jobTitles.Reorder,
                        imports = (fun catalog -> catalog.DefinedTerms),
                        addItem = (fun item -> mutate (fun _ -> agent.AddJobTitle item)),
                        removeItem = (fun item -> mutate (fun _ -> agent.RemoveJobTitle item))
                    )
                ]
            )
        ]

type AgentMetadata with

    [<ReactComponent>]
    static member Agents(agents: ResizeArray<Agent>, mutate: (ARC -> unit) -> unit) =
        Html.div [
            prop.className "swt:space-y-4"
            prop.children [
                for agent in agents do
                    AgentMetadata.AgentView(agent, mutate)
            ]
        ]

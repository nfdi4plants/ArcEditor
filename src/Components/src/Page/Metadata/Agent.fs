namespace Swate.Components.Page.Metadata

open Feliz
open ProcessCore
open Fable.Core
open Swate.Components.Page.ObjectBrowser.Types
open Swate.Components.Primitive.LayoutComponents
open Swate.Components.Page.Metadata.FormComponents
open Swate.Components.Shared

[<Erase; Mangle(false)>]
type AgentMetadata =

    [<ReactComponent(true)>]
    static member AgentView(agent: Agent, setAgent: Agent -> unit, ?onNavigate: ProcessCoreEntityValue -> unit) =
        let navigate = defaultArg onNavigate ignore

        LayoutComponents.Section [
            LayoutComponents.BoxedField(
                "Agent Metadata",
                content = [
                    TextInput.TextInput(
                        agent.Id |> Option.defaultValue "",
                        (fun value ->
                            agent.Copy(id = Option.whereNot System.String.IsNullOrWhiteSpace value)
                            |> setAgent
                        ),
                        label = "Id",
                        disabled = true
                    )
                    TextInput.TextInput(
                        agent.GivenName,
                        (fun value -> agent.Copy(givenName = value) |> setAgent),
                        label = "Given Name",
                        // ProcessCore hotfix: prevent clearing this mandatory primary field.
                        validator = Swate.Components.ProcessCoreHotfixes.required "Given name"
                    )
                    TextInput.TextInput(
                        agent.FamilyName |> Option.defaultValue "",
                        (fun value ->
                            agent.Copy(familyName = Option.whereNot System.String.IsNullOrWhiteSpace value)
                            |> setAgent
                        ),
                        label = "Family Name"
                    )
                    TextInput.TextInput(
                        agent.Email |> Option.defaultValue "",
                        (fun value ->
                            agent.Copy(email = Option.whereNot System.String.IsNullOrWhiteSpace value)
                            |> setAgent
                        ),
                        label = "Email"
                    )
                    (NestedMetadataInput.OptionalRow(
                        "Affiliation",
                        agent.Affiliation,
                        (fun () -> Organization("New Organisation", System.Guid.NewGuid().ToString())),
                        (fun affiliation -> agent.Copy(affiliation = affiliation) |> setAgent),
                        "swt:iconify-color swt:fluent-color--organization-20",
                        (fun organization -> NestedMetadataInput.nonEmptyOr "Unnamed organization" organization.Name),
                        (ProcessCoreEntityValue.Organization >> navigate),
                        imports = (fun catalog -> catalog.Organizations)
                    ))
                    TextInput.TextInput(
                        agent.Identifier |> Option.defaultValue "",
                        (fun value ->
                            agent.Copy(identifier = Option.whereNot System.String.IsNullOrWhiteSpace value)
                            |> setAgent
                        ),
                        label = "Identifier"
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        (ResizeArray agent.AdditionalProperty),
                        (fun () -> Annotation("")),
                        (fun properties -> agent.Copy(additionalProperty = properties) |> setAgent),
                        "Additional Properties",
                        NestedMetadataInput.Annotation,
                        (ProcessCoreEntityValue.Annotation >> navigate),
                        imports = (fun catalog -> catalog.Annotations)
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        (ResizeArray agent.JobTitles),
                        (fun () -> DefinedTerm("")),
                        (fun jobTitles -> agent.Copy(jobTitles = jobTitles) |> setAgent),
                        "Job Titles",
                        (fun jobTitle ->
                            let _, label = NestedMetadataInput.DefinedTerm jobTitle
                            "swt:iconify swt:fluent--briefcase-20-regular", label
                        ),
                        (ProcessCoreEntityValue.DefinedTerm >> navigate),
                        imports = (fun catalog -> catalog.DefinedTerms)
                    )
                ]
            )
        ]

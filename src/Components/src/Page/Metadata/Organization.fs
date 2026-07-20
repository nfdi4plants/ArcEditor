namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
open ProcessCore
open Swate.Components.Shared
open Swate.Components.Page.Metadata
open Swate.Components.Primitive.LayoutComponents

[<Erase; Mangle(false)>]
type OrganizationMetadata =

    [<ReactComponent(true)>]
    static member OrganizationView(organization: ProcessCore.Organization, mutate: (ARC -> unit) -> unit) =

        LayoutComponents.Section [
            LayoutComponents.BoxedField(
                "Organization Metadata",
                content = [
                    FormComponents.TextInput.TextInput(
                        organization.Name,
                        (fun value -> mutate (fun _ -> organization.Name <- value)),
                        label = "Name",
                        // ProcessCore hotfix: prevent clearing this mandatory primary field.
                        validator = Swate.Components.ProcessCoreHotfixes.required "Name"
                    )
                    FormComponents.TextInput.TextInput(
                        organization.Id |> Option.defaultValue "",
                        (fun value ->
                            mutate (fun _ -> organization.Id <- Option.whereNot System.String.IsNullOrWhiteSpace value)
                        ),
                        label = "Id"
                    )
                    FormComponents.TextInput.TextInput(
                        organization.Url |> Option.defaultValue "",
                        (fun value ->
                            mutate (fun _ ->
                                organization.Url <- Option.whereNot System.String.IsNullOrWhiteSpace value
                            )
                        ),
                        label = "Url"
                    )
                ]
            )
        ]

type OrganizationMetadata with

    [<ReactComponent>]
    static member Organizations(organizations: ResizeArray<Organization>, mutate: (ARC -> unit) -> unit) =
        Html.div [
            prop.className "swt:space-y-4"
            prop.children [
                for organization in organizations do
                    Html.div [
                        prop.className "swt:space-y-2"
                        prop.children [
                            OrganizationMetadata.OrganizationView(organization, mutate)
                        ]
                    ]
            ]
        ]

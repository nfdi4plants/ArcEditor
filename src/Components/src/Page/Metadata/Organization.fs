namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
open Swate.Components.Page.Metadata
open Swate.Components.Primitive.LayoutComponents
open ProcessCore
open Swate.Components.Shared

[<Erase; Mangle(false)>]
type OrganizationMetadata =

    [<ReactComponent(true)>]
    static member OrganizationView
        (organization: ProcessCore.Organization, setOrganization: ProcessCore.Organization -> unit)
        =

        LayoutComponents.Section [
            LayoutComponents.BoxedField(
                "Organization Metadata",
                content = [
                    FormComponents.TextInput.TextInput(
                        organization.Name,
                        (fun value -> organization.Copy(name = value) |> setOrganization),
                        label = "Name",
                        // ProcessCore hotfix: prevent clearing this mandatory primary field.
                        validator = Swate.Components.ProcessCoreHotfixes.required "Name"
                    )
                    FormComponents.TextInput.TextInput(
                        organization.Id |> Option.defaultValue "",
                        (fun value ->
                            organization.Copy(id = Option.whereNot System.String.IsNullOrWhiteSpace value)
                            |> setOrganization
                        ),
                        label = "Id"
                    )
                    FormComponents.TextInput.TextInput(
                        organization.Url |> Option.defaultValue "",
                        (fun value ->
                            organization.Copy(url = Option.whereNot System.String.IsNullOrWhiteSpace value)
                            |> setOrganization
                        ),
                        label = "Url"
                    )
                ]
            )
        ]

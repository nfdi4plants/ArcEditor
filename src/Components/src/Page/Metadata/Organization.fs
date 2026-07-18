namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
open Swate.Components.Page.Metadata
open Swate.Components.Primitive.LayoutComponents

[<Erase; Mangle(false)>]
type OrganizationMetadata =

    [<ReactComponent(true)>]
    static member OrganizationView
        (organization: ProcessCore.Organization, setOrganization: ProcessCore.Organization -> unit)
        =

        let updateOrganization (updateFn: ProcessCore.Organization -> ProcessCore.Organization) =
            let copy =
                ProcessCore.Organization(organization.Name, ?id = organization.Id, ?url = organization.Url)

            let updatedOrganization = updateFn copy
            setOrganization updatedOrganization

        LayoutComponents.Section [
            LayoutComponents.BoxedField(
                "Organization Metadata",
                content = [
                    FormComponents.TextInput.TextInput(
                        organization.Name,
                        (fun value ->
                            updateOrganization (fun updatedOrganization ->
                                updatedOrganization.Name <- value
                                updatedOrganization
                            )
                        ),
                        label = "Name",
                        // ProcessCore hotfix: prevent clearing this mandatory primary field.
                        validator = Swate.Components.ProcessCoreHotfixes.required "Name"
                    )
                    FormComponents.TextInput.TextInput(
                        organization.Id |> Option.defaultValue "",
                        (fun value ->
                            updateOrganization (fun updatedOrganization ->
                                updatedOrganization.Id <- Some value
                                updatedOrganization
                            )
                        ),
                        label = "Id"
                    )
                    FormComponents.TextInput.TextInput(
                        organization.Url |> Option.defaultValue "",
                        (fun value ->
                            updateOrganization (fun updatedOrganization ->
                                updatedOrganization.Url <- Some value
                                updatedOrganization
                            )
                        ),
                        label = "Url"
                    )
                ]
            )
        ]

namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
open ProcessCore
open Swate.Components.Shared // Option extension
open Swate.Components.Composite.TermSearch.Types
open Swate.Components.Primitive.LayoutComponents
open Swate.Components.Page.Metadata
open Swate.Components.Page.ObjectBrowser.Types
open Swate.Components.Page.Metadata.FormComponents

[<Erase; Mangle(false)>]
type AnnotationMetadata =

    [<ReactComponent(true)>]
    static member AnnotationView
        (annotation: ProcessCore.Annotation, mutate: (ARC -> unit) -> unit, ?onNavigate: ProcessCoreEntityValue -> unit)
        =

        let navigate = defaultArg onNavigate ignore

        let nameTerm = Term(name = annotation.Name, ?id = annotation.NameTAN)

        let unitTerm =
            annotation.Unit
            |> Option.map (fun unitName -> Term(name = unitName, ?id = annotation.UnitTAN))

        let updateNameTerm (selectedTerm: Term) =
            mutate (fun _ ->
                annotation.Name <- Option.defaultValue "" selectedTerm.name
                annotation.NameTAN <- selectedTerm.id
            )

        let updateUnit (selectedTerm: Term option) =
            mutate (fun _ ->
                annotation.Unit <- selectedTerm |> Option.bind (fun term -> term.name)
                annotation.UnitTAN <- selectedTerm |> Option.bind (fun term -> term.id)
            )

        LayoutComponents.Section [
            LayoutComponents.BoxedField(
                "Annotation Metadata",
                content = [
                    Helpers.RequiredTermInput(nameTerm, updateNameTerm, "Name")
                    FormComponents.TextInput.TextInput(
                        annotation.Value |> Option.defaultValue "",
                        (fun value ->
                            mutate (fun _ ->
                                annotation.Value <- Option.whereNot System.String.IsNullOrWhiteSpace value
                            )
                        ),
                        label = "Value"
                    )
                    Helpers.TermInput(unitTerm, updateUnit, "Unit")
                    FormComponents.TextInput.TextInput(
                        annotation.NameTAN |> Option.defaultValue "",
                        (fun value ->
                            mutate (fun _ ->
                                annotation.NameTAN <- Option.whereNot System.String.IsNullOrWhiteSpace value
                            )
                        ),
                        label = "Name TAN"
                    )
                    FormComponents.TextInput.TextInput(
                        annotation.ValueTAN |> Option.defaultValue "",
                        (fun value ->
                            mutate (fun _ ->
                                annotation.ValueTAN <- Option.whereNot System.String.IsNullOrWhiteSpace value
                            )
                        ),
                        label = "Value TAN"
                    )
                    FormComponents.TextInput.TextInput(
                        annotation.UnitTAN |> Option.defaultValue "",
                        (fun value ->
                            mutate (fun _ ->
                                annotation.UnitTAN <- Option.whereNot System.String.IsNullOrWhiteSpace value
                            )
                        ),
                        label = "Unit TAN"
                    )
                    FormComponents.TextInput.TextInput(
                        annotation.AdditionalType |> Option.defaultValue "",
                        (fun value ->
                            mutate (fun _ ->
                                annotation.AdditionalType <- Option.whereNot System.String.IsNullOrWhiteSpace value
                            )
                        ),
                        label = "Additional Type"
                    )
                    (NestedMetadataInput.OptionalRow(
                        "Instance Of",
                        annotation.InstanceOf,
                        (fun () -> FormalParameter("New Formal Parameter")),
                        (fun instanceOf -> mutate (fun _ -> annotation.InstanceOf <- instanceOf)),
                        "swt:iconify swt:fluent--options-20-regular",
                        (fun parameter -> NestedMetadataInput.nonEmptyOr "Unnamed formal parameter" parameter.Name),
                        (ProcessCoreEntityValue.FormalParameter >> navigate)
                    ))
                ]
            )
        ]

type AnnotationMetadata with

    [<ReactComponent>]
    static member Annotations(annotations: ResizeArray<Annotation>, mutate: (ARC -> unit) -> unit) =
        Html.div [
            prop.className "swt:space-y-4"
            prop.children [
                for annotation in annotations do
                    Html.div [
                        prop.className "swt:space-y-2"
                        prop.children [ AnnotationMetadata.AnnotationView(annotation, mutate) ]
                    ]
            ]
        ]

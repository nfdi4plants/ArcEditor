namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
open ProcessCore
open Swate.Components.Primitive.LayoutComponents
open Swate.Components.Page.ObjectBrowser.Types
open Swate.Components.Page.Metadata.FormComponents
open Swate.Components.Shared

module private ProcessMetadataTypes =
    type ProcessChildren = {
        Inputs: ResizeArray<IONode>
        Outputs: ResizeArray<IONode>
        ParameterValues: ResizeArray<Annotation>
    }

open ProcessMetadataTypes

[<Erase; Mangle(false)>]
type ProcessMetadata =

    [<ReactComponent(true)>]
    static member ProcessView
        (
            processObject: ProcessCore.Process,
            setProcess: ProcessCore.Process -> unit,
            ?onNavigate: ProcessCoreEntityValue -> unit
        ) =

        let navigate = defaultArg onNavigate ignore

        let ioNodePresentation =
            function
            | SampleNode sample ->
                "swt:iconify-color swt:fluent-color--molecule-20",
                NestedMetadataInput.nonEmptyOr "Unnamed sample" sample.Name
            | DataNode data ->
                "swt:iconify-color swt:fluent-color--data-line-20",
                NestedMetadataInput.nonEmptyOr "Unnamed data" data.Name

        let navigateToNode =
            function
            | SampleNode sample -> navigate (ProcessCoreEntityValue.Sample sample)
            | DataNode data -> navigate (ProcessCoreEntityValue.Data data)

        LayoutComponents.Section [
            LayoutComponents.BoxedField(
                "Process Metadata",
                content = [
                    TextInput.TextInput(
                        processObject.Name,
                        (fun value -> processObject.Copy(name = value) |> setProcess),
                        label = "Name",
                        // ProcessCore hotfix: prevent clearing this mandatory primary field.
                        validator = Swate.Components.ProcessCoreHotfixes.required "Name"
                    )
                    (NestedMetadataInput.OptionalRow(
                        "Executes Protocol",
                        processObject.ExecutesProtocol,
                        (fun () -> Recipe()),
                        (fun protocol -> processObject.Copy(executesProtocol = protocol) |> setProcess),
                        "swt:iconify-color swt:fluent-color--clipboard-text-edit-20",
                        (fun recipe -> NestedMetadataInput.optionOr "Unnamed recipe" recipe.Name),
                        (ProcessCoreEntityValue.Recipe >> navigate),
                        imports = (fun catalog -> catalog.Recipes)
                    ))
                    TextInput.TextInput(
                        processObject.AdditionalType |> Option.defaultValue "",
                        (fun value ->
                            processObject.Copy(additionalType = Option.whereNot System.String.IsNullOrWhiteSpace value)
                            |> setProcess
                        ),
                        label = "Additional Type"
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        processObject.Inputs,
                        (fun () -> SampleNode(ProcessCore.Sample("New Sample"))),
                        (fun inputs -> processObject.Copy(inputs = inputs) |> setProcess),
                        "Inputs",
                        ioNodePresentation,
                        navigateToNode,
                        imports = (fun catalog -> catalog.IONodes)
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        processObject.Outputs,
                        (fun () -> DataNode(ProcessCore.Data("New Data"))),
                        (fun outputs -> processObject.Copy(outputs = outputs) |> setProcess),
                        "Outputs",
                        ioNodePresentation,
                        navigateToNode,
                        imports = (fun catalog -> catalog.IONodes)
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        processObject.ParameterValue,
                        (fun () -> Annotation("New Annotation")),
                        (fun parameterValues -> processObject.Copy(parameterValues = parameterValues) |> setProcess),
                        "Parameter Values",
                        NestedMetadataInput.Annotation,
                        (ProcessCoreEntityValue.Annotation >> navigate),
                        imports = (fun catalog -> catalog.Annotations)
                    )
                ]
            )
        ]

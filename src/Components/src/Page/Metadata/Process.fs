namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
open ProcessCore
open Swate.Components.Primitive.LayoutComponents
open Swate.Components.Page.ObjectBrowser.Types
open Swate.Components.Page.Metadata.FormComponents

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

        let copyProcess children =
            // Keep I/O back-references attached to only the replacement process.
            processObject.Inputs |> Seq.toArray |> Array.iter processObject.RemoveInput
            processObject.Outputs |> Seq.toArray |> Array.iter processObject.RemoveOutput

            let copy =
                ProcessCore.Process(
                    processObject.Name,
                    ?executesProtocol = processObject.ExecutesProtocol,
                    ?additionalType = processObject.AdditionalType,
                    inputs = children.Inputs,
                    outputs = children.Outputs,
                    parameterValue = children.ParameterValues
                )

            copy.ProcessOf <- processObject.ProcessOf
            copy

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

        let children = {
            Inputs = ResizeArray processObject.Inputs
            Outputs = ResizeArray processObject.Outputs
            ParameterValues = ResizeArray processObject.ParameterValue
        }

        let updateProcess update =
            children |> copyProcess |> update |> setProcess

        let setChildren = copyProcess >> setProcess

        LayoutComponents.Section [
            LayoutComponents.BoxedField(
                "Process Metadata",
                content = [
                    TextInput.TextInput(
                        processObject.Name,
                        (fun value ->
                            updateProcess (fun updatedProcess ->
                                updatedProcess.Name <- value
                                updatedProcess
                            )
                        ),
                        label = "Name",
                        // ProcessCore hotfix: prevent clearing this mandatory primary field.
                        validator = Swate.Components.ProcessCoreHotfixes.required "Name"
                    )
                    (NestedMetadataInput.OptionalRow(
                        "Executes Protocol",
                        processObject.ExecutesProtocol,
                        (fun () -> Recipe()),
                        (fun protocol ->
                            updateProcess (fun updatedProcess ->
                                updatedProcess.ExecutesProtocol <- protocol
                                updatedProcess
                            )
                        ),
                        "swt:iconify-color swt:fluent-color--clipboard-text-edit-20",
                        (fun recipe -> NestedMetadataInput.optionOr "Unnamed recipe" recipe.Name),
                        (ProcessCoreEntityValue.Recipe >> navigate)
                    ))
                    TextInput.TextInput(
                        processObject.AdditionalType |> Option.defaultValue "",
                        (fun value ->
                            updateProcess (fun updatedProcess ->
                                updatedProcess.AdditionalType <-
                                    if System.String.IsNullOrWhiteSpace value then
                                        None
                                    else
                                        Some value

                                updatedProcess
                            )
                        ),
                        label = "Additional Type"
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        children.Inputs,
                        (fun () -> SampleNode(ProcessCore.Sample(""))),
                        (fun inputs -> setChildren { children with Inputs = inputs }),
                        "Inputs",
                        ioNodePresentation,
                        navigateToNode
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        children.Outputs,
                        (fun () -> DataNode(ProcessCore.Data(""))),
                        (fun outputs -> setChildren { children with Outputs = outputs }),
                        "Outputs",
                        ioNodePresentation,
                        navigateToNode
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        children.ParameterValues,
                        (fun () -> Annotation("")),
                        (fun parameterValues ->
                            setChildren {
                                children with
                                    ParameterValues = parameterValues
                            }
                        ),
                        "Parameter Values",
                        NestedMetadataInput.Annotation,
                        (ProcessCoreEntityValue.Annotation >> navigate)
                    )
                ]
            )
        ]

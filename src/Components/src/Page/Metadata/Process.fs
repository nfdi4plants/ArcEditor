namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
open ProcessCore
open Swate.Components.Primitive.LayoutComponents
open Swate.Components.Page.ObjectBrowser.Types
open Swate.Components.Page.Metadata.FormComponents

type private ProcessChildren = {
    inputs: ResizeArray<IONode>
    outputs: ResizeArray<IONode>
    parameterValues: ResizeArray<Annotation>
}

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

        let copyProcess
            (inputs: ResizeArray<IONode>)
            (outputs: ResizeArray<IONode>)
            (parameterValues: ResizeArray<Annotation>)
            =
            let requestedInputs = ResizeArray inputs
            let requestedOutputs = ResizeArray outputs

            // Keep I/O back-references attached to only the replacement process.
            processObject.Inputs |> Seq.toArray |> Array.iter processObject.RemoveInput
            processObject.Outputs |> Seq.toArray |> Array.iter processObject.RemoveOutput

            let copy =
                ProcessCore.Process(
                    processObject.Name,
                    ?executesProtocol = processObject.ExecutesProtocol,
                    ?additionalType = processObject.AdditionalType,
                    inputs = requestedInputs,
                    outputs = requestedOutputs,
                    parameterValue = parameterValues
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

        let updateProcess (updateFn: ProcessCore.Process -> ProcessCore.Process) =
            let copy =
                copyProcess processObject.Inputs processObject.Outputs processObject.ParameterValue

            setProcess (updateFn copy)

        let children = {
            inputs = processObject.Inputs
            outputs = processObject.Outputs
            parameterValues = processObject.ParameterValue
        }

        let setChildren children =
            copyProcess children.inputs children.outputs children.parameterValues
            |> setProcess

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
                        label = "Name"
                    )
                    (NestedMetadataInput.optionalRow
                        "Executes Protocol"
                        processObject.ExecutesProtocol
                        (fun () -> Recipe())
                        (fun protocol ->
                            let copy = copyProcess children.inputs children.outputs children.parameterValues

                            copy.ExecutesProtocol <- protocol
                            setProcess copy
                        )
                        "swt:iconify-color swt:fluent-color--clipboard-text-edit-20"
                        (fun recipe -> NestedMetadataInput.optionOr "Unnamed recipe" recipe.Name)
                        (ProcessCoreEntityValue.Recipe >> navigate))
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
                    NestedMetadataInput.sequence
                        (ResizeArray processObject.Inputs)
                        (fun () -> SampleNode(ProcessCore.Sample("")))
                        (fun inputs -> setChildren { children with inputs = inputs })
                        "Inputs"
                        ioNodePresentation
                        navigateToNode
                    NestedMetadataInput.sequence
                        (ResizeArray processObject.Outputs)
                        (fun () -> DataNode(ProcessCore.Data("")))
                        (fun outputs -> setChildren { children with outputs = outputs })
                        "Outputs"
                        ioNodePresentation
                        navigateToNode
                    NestedMetadataInput.sequence
                        (ResizeArray processObject.ParameterValue)
                        (fun () -> Annotation(""))
                        (fun parameterValues ->
                            setChildren {
                                children with
                                    parameterValues = parameterValues
                            }
                        )
                        "Parameter Values"
                        NestedMetadataInput.annotation
                        (ProcessCoreEntityValue.Annotation >> navigate)
                ]
            )
        ]

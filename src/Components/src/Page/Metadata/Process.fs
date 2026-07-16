namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
open Swate.Components.Primitive.LayoutComponents
open Swate.Components.Page.Metadata.FormComponents

[<Erase; Mangle(false)>]
type ProcessMetadata =

    [<ReactComponent(true)>]
    static member ProcessMetadata(processObject: ProcessCore.Process, setProcess: ProcessCore.Process -> unit) =

        let updateProcess (updateFn: ProcessCore.Process -> ProcessCore.Process) =
            let copy =
                ProcessCore.Process(
                    processObject.Name,
                    ?executesProtocol = processObject.ExecutesProtocol,
                    ?additionalType = processObject.AdditionalType,
                    inputs = processObject.Inputs,
                    outputs = processObject.Outputs,
                    parameterValue = processObject.ParameterValue
                )

            copy.ProcessOf <- processObject.ProcessOf
            setProcess (updateFn copy)

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
                    // TODO ExecutesProtocol is a Recipe, which is a complex type. We need a way to select or create a Recipe.
                    Html.div
                        "Placeholder for ExecutesProtocol (Recipe) input. This should be a dropdown or a search field to select an existing Recipe or create a new one."
                    FormComponents.TextInput.TextInput(
                        processObject.Name,
                        (fun input ->
                            updateProcess (fun updatedProcess ->
                                updatedProcess.Name <- input
                                updatedProcess
                            )
                        ),
                        label = "ExecutesProtocol"
                    )
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
                    // TODO Inputs is a IONode, which is a complex type. We need a way to select or create an IONode.
                    Html.div
                        "Placeholder for Inputs (IONode seq) input. This should be a dropdown or a search field to select an existing IONode or create a new one."
                    FormComponents.TextInput.TextInput(
                        processObject.Name,
                        (fun input ->
                            updateProcess (fun updatedProcess ->
                                updatedProcess.Name <- input
                                updatedProcess
                            )
                        ),
                        label = "Inputs"
                    )
                    // TODO Outputs is an IONode seq, which is a complex type. We need a way to select or create an IONode.
                    Html.div
                        "Placeholder for Outputs (IONode seq) input. This should be a dropdown or a search field to select an existing IONode or create a new one."
                    FormComponents.TextInput.TextInput(
                        processObject.Name,
                        (fun input ->
                            updateProcess (fun updatedProcess ->
                                updatedProcess.Name <- input
                                updatedProcess
                            )
                        ),
                        label = "Outputs"
                    )
                    // TODO ParameterValue is an Annotation seq, which is a complex type. We need a way to select or create an Annotation.
                    Html.div
                        "Placeholder for ParameterValue (Annotation seq) input. This should be a dropdown or a search field to select an existing Annotation or create a new one."
                    FormComponents.TextInput.TextInput(
                        processObject.Name,
                        (fun input ->
                            updateProcess (fun updatedProcess ->
                                updatedProcess.Name <- input
                                updatedProcess
                            )
                        ),
                        label = "ParameterValue"
                    )
                ]
            )
        ]

namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
open ProcessCore
open Swate.Components.Shared
open Swate.Components.Primitive.LayoutComponents
open Swate.Components.Page.ObjectBrowser.Types
open Swate.Components.Page.Metadata.FormComponents

[<Erase; Mangle(false)>]
type ProcessMetadata =

    [<ReactComponent(true)>]
    static member ProcessView
        (processObject: ProcessCore.Process, mutate: (ARC -> unit) -> unit, ?onNavigate: ProcessCoreEntityValue -> unit)
        =

        let navigate = defaultArg onNavigate ignore

        let allIONodes (catalog: ImportCatalogContext.ImportCatalog) =
            Array.append (catalog.Samples |> Array.map SampleNode) (catalog.Data |> Array.map DataNode)

        let setRecipe recipe =
            mutate (fun _ -> processObject.ExecutesProtocol <- recipe)

        let inputs =
            MetadataRelationship.create mutate processObject.Inputs processObject.AddInput processObject.RemoveInput

        let outputs =
            MetadataRelationship.create mutate processObject.Outputs processObject.AddOutput processObject.RemoveOutput

        let parameterValues =
            MetadataRelationship.create
                mutate
                processObject.ParameterValue
                processObject.AddParameterValue
                processObject.RemoveParameterValue

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
                        (fun value -> mutate (fun _ -> processObject.Name <- value)),
                        label = "Name",
                        // ProcessCore hotfix: prevent clearing this mandatory primary field.
                        validator = Swate.Components.ProcessCoreHotfixes.required "Name"
                    )
                    (NestedMetadataInput.OptionalRow(
                        "Executes Protocol",
                        processObject.ExecutesProtocol,
                        (fun () -> Recipe()),
                        setRecipe,
                        "swt:iconify-color swt:fluent-color--clipboard-text-edit-20",
                        (fun recipe -> NestedMetadataInput.optionOr "Unnamed recipe" recipe.Name),
                        (ProcessCoreEntityValue.Recipe >> navigate),
                        imports = (fun catalog -> catalog.Recipes)
                    ))
                    TextInput.TextInput(
                        processObject.AdditionalType |> Option.defaultValue "",
                        (fun value ->
                            mutate (fun _ ->
                                processObject.AdditionalType <- Option.whereNot System.String.IsNullOrWhiteSpace value
                            )
                        ),
                        label = "Additional Type"
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        processObject.Inputs,
                        (fun () -> SampleNode(ProcessCore.Sample("New Sample"))),
                        "Inputs",
                        ioNodePresentation,
                        navigateToNode,
                        reorderItems = inputs.Reorder,
                        imports = (fun catalog -> catalog.IONodes),
                        duplicateCandidates = allIONodes,
                        addItem = inputs.Add,
                        removeItem = inputs.Remove
                    )

                    NestedMetadataInput.CreatePCInputSequence(
                        processObject.Outputs,
                        (fun () -> DataNode(ProcessCore.Data("New Data"))),
                        "Outputs",
                        ioNodePresentation,
                        navigateToNode,
                        reorderItems = outputs.Reorder,
                        imports = (fun catalog -> catalog.IONodes),
                        duplicateCandidates = allIONodes,
                        addItem = outputs.Add,
                        removeItem = outputs.Remove
                    )
                    NestedMetadataInput.CreatePCInputSequence(
                        processObject.ParameterValue,
                        (fun () -> Annotation("New Annotation")),
                        "Parameter Values",
                        NestedMetadataInput.Annotation,
                        (ProcessCoreEntityValue.Annotation >> navigate),
                        reorderItems = parameterValues.Reorder,
                        imports = (fun catalog -> catalog.Annotations),
                        duplicateCandidates = (fun catalog -> catalog.Annotations),
                        addItem = parameterValues.Add,
                        removeItem = parameterValues.Remove
                    )
                ]
            )
        ]

type ProcessMetadata with

    [<ReactComponent>]
    static member Processes(processes: ResizeArray<Process>, mutate: (ARC -> unit) -> unit) =
        Html.div [
            prop.className "swt:space-y-4"
            prop.children [
                for processObject in processes do
                    ProcessMetadata.ProcessView(processObject, mutate)
            ]
        ]

namespace Swate.Components.Page.Metadata

open Feliz
open Fable.Core
open ProcessCore
open Swate.Components.Shared
open Swate.Components.ProcessCore
open Swate.Components.Primitive.LayoutComponents
open Swate.Components.Page.ObjectBrowser.Types
open Swate.Components.Page.Metadata.FormComponents

[<Erase; Mangle(false)>]
type ProcessMetadata =

    [<ReactComponent(true)>]
    static member ProcessView
        (
            processView: Swate.Components.ProcessCore.Types.ProcessView,
            mutate: (ARC -> unit) -> unit,
            ?onNavigate: ProcessCoreEntityValue -> unit
        ) =

        let navigate = defaultArg onNavigate ignore

        let allIONodes (catalog: Swate.Components.ProcessCore.Types.ImportCatalog) =
            Array.append (catalog.Samples |> Array.map SampleNode) (catalog.Data |> Array.map DataNode)

        let mutateMembers update =
            mutate (fun _ -> RendererModel.updateMembers update processView)

        let ioCollapse
            (values: IONode array)
            (constructor: unit -> IONode)
            (label: string)
            (subtitle: string)
            (iconClass: string)
            (add: IONode -> Swate.Components.ProcessCore.Types.ProcessView -> unit)
            (remove: IONode -> Swate.Components.ProcessCore.Types.ProcessView -> unit)
            =
            let relationship =
                MetadataRelationship.create
                    mutate
                    (ResizeArray values)
                    (fun node -> add node processView)
                    (fun node -> remove node processView)

            NestedMetadataInput.CreatePCInputSequence(
                ResizeArray values,
                constructor,
                label,
                (function
                | SampleNode sample -> Icons.sampleIcon, NestedMetadataInput.nonEmptyOr "Unnamed sample" sample.Name
                | DataNode data -> Icons.dataIcon, NestedMetadataInput.nonEmptyOr "Unnamed data" data.Name),
                (function
                | SampleNode sample -> navigate (ProcessCoreEntityValue.Sample sample)
                | DataNode data -> navigate (ProcessCoreEntityValue.Data data)),
                imports = (fun catalog -> catalog.IONodes),
                duplicateCandidates = allIONodes,
                isImportable = (fun node -> RendererModel.isNodeUnassociated node processView),
                showLabel = false,
                stickyFooter = true,
                createOptions = [|
                    "Sample", (fun () -> SampleNode(ProcessCore.Sample("New Sample")))
                    "Data", (fun () -> DataNode(ProcessCore.Data("New Data")))
                |],
                addItem = relationship.Add,
                removeItem = relationship.Remove
            )
            |> fun content -> CollectionCollapse.Main(label, subtitle, values.Length, content, iconClass = iconClass)

        let parameterValues =
            MetadataRelationship.create
                mutate
                processView.Representative.ParameterValue
                (fun annotation ->
                    processView.Processes.Values
                    |> Seq.toArray
                    |> Array.iter (fun item -> item.AddParameterValue annotation)
                )
                (fun annotation ->
                    processView.Processes.Values
                    |> Seq.toArray
                    |> Array.iter (fun item -> item.RemoveParameterValue annotation)
                )

        LayoutComponents.Section(
            [
                LayoutComponents.BoxedField(
                    "Process Metadata",
                    content = [
                        TextInput.TextInput(
                            processView.Representative.Name,
                            (fun value -> mutateMembers (fun memberProcess -> memberProcess.Name <- value)),
                            label = "Name",
                            // ProcessCore hotfix: prevent clearing this mandatory primary field.
                            validator = Swate.Components.ProcessCore.Hotfixes.required "Name"
                        )
                        (NestedMetadataInput.OptionalRow(
                            "Executes Protocol",
                            processView.Representative.ExecutesProtocol,
                            (fun () -> Recipe()),
                            (fun recipe ->
                                mutateMembers (fun memberProcess -> memberProcess.ExecutesProtocol <- recipe)
                            ),
                            Icons.recipeIcon,
                            (fun recipe -> NestedMetadataInput.optionOr "Unnamed recipe" recipe.Name),
                            (ProcessCoreEntityValue.Recipe >> navigate),
                            imports = (fun catalog -> catalog.Recipes)
                        ))
                        TextInput.TextInput(
                            processView.Representative.AdditionalType |> Option.defaultValue "",
                            (fun value ->
                                let additionalType = Option.whereNot System.String.IsNullOrWhiteSpace value

                                mutateMembers (fun memberProcess -> memberProcess.AdditionalType <- additionalType)
                            ),
                            label = "Additional Type"
                        )
                        ioCollapse
                            (processView.Inputs.Values |> Seq.toArray)
                            (fun () -> SampleNode(ProcessCore.Sample("New Sample")))
                            "Inputs"
                            "Samples and data consumed by this process"
                            Icons.inputIcon
                            RendererModel.addInput
                            RendererModel.removeInput
                        ioCollapse
                            (processView.Outputs.Values |> Seq.toArray)
                            (fun () -> DataNode(ProcessCore.Data("New Data")))
                            "Outputs"
                            "Samples and data produced by this process"
                            Icons.outputIcon
                            RendererModel.addOutput
                            RendererModel.removeOutput
                        NestedMetadataInput.CreatePCInputSequence(
                            processView.Representative.ParameterValue,
                            (fun () -> Annotation("New Annotation")),
                            "Parameter Values",
                            NestedMetadataInput.Annotation,
                            (ProcessCoreEntityValue.Annotation >> navigate),
                            imports = (fun catalog -> catalog.Annotations),
                            duplicateCandidates = (fun catalog -> catalog.Annotations),
                            showLabel = false,
                            stickyFooter = true,
                            addItem = parameterValues.Add,
                            removeItem = parameterValues.Remove
                        )
                        |> fun content ->
                            CollectionCollapse.Main(
                                "Parameter Values",
                                "Annotations assigned to this process",
                                processView.Representative.ParameterValue.Count,
                                content,
                                iconClass = Icons.formalParameterIcon
                            )
                    ]
                )
            ],
            overflowVisible = true
        )

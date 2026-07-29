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
            processView: RendererModel.ProcessView,
            mutate: (ARC -> unit) -> unit,
            ?onNavigate: ProcessCoreEntityValue -> unit
        ) =

        let processObject = processView.Representative
        let members = processView.Members
        let navigate = defaultArg onNavigate ignore

        let allIONodes (catalog: ImportCatalogContext.ImportCatalog) =
            Array.append (catalog.Samples |> Array.map SampleNode) (catalog.Data |> Array.map DataNode)

        let isUnassociated node =
            processView.Connections
            |> Array.exists (fun connection ->
                connection.Input |> Option.exists (fun associated -> associated.EqualTo node)
                || connection.Output |> Option.exists (fun associated -> associated.EqualTo node)
            )
            |> not

        let mutateMembers update =
            mutate (fun _ -> members |> Array.iter update)

        let ioCollapse
            (values: IONode array)
            (constructor: unit -> IONode)
            (label: string)
            (subtitle: string)
            (iconClass: string)
            (add: IONode -> RendererModel.ProcessView -> unit)
            (remove: IONode -> RendererModel.ProcessView -> unit)
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
                | SampleNode sample ->
                    "swt:iconify-color swt:fluent-color--molecule-20",
                    NestedMetadataInput.nonEmptyOr "Unnamed sample" sample.Name
                | DataNode data ->
                    "swt:iconify-color swt:fluent-color--data-line-20",
                    NestedMetadataInput.nonEmptyOr "Unnamed data" data.Name),
                (function
                | SampleNode sample -> navigate (ProcessCoreEntityValue.Sample sample)
                | DataNode data -> navigate (ProcessCoreEntityValue.Data data)),
                imports = (fun catalog -> catalog.IONodes),
                duplicateCandidates = allIONodes,
                isImportable = isUnassociated,
                showLabel = false,
                stickyFooter = true,
                createOptions = [|
                    "Sample", (fun () -> SampleNode(ProcessCore.Sample("New Sample")))
                    "Data", (fun () -> DataNode(ProcessCore.Data("New Data")))
                |],
                addItem = relationship.Add,
                removeItem = relationship.Remove
            )
            |> fun content ->
                LayoutComponents.CollectionCollapse(label, subtitle, values.Length, content, iconClass = iconClass)

        let parameterValues =
            MetadataRelationship.create
                mutate
                processObject.ParameterValue
                (fun annotation -> members |> Array.iter (fun item -> item.AddParameterValue annotation))
                (fun annotation -> members |> Array.iter (fun item -> item.RemoveParameterValue annotation))

        LayoutComponents.Section(
            [
                LayoutComponents.BoxedField(
                    "Process Metadata",
                    content = [
                        TextInput.TextInput(
                            processObject.Name,
                            (fun value -> mutateMembers (fun memberProcess -> memberProcess.Name <- value)),
                            label = "Name",
                            // ProcessCore hotfix: prevent clearing this mandatory primary field.
                            validator = Swate.Components.ProcessCore.Hotfixes.required "Name"
                        )
                        (NestedMetadataInput.OptionalRow(
                            "Executes Protocol",
                            processObject.ExecutesProtocol,
                            (fun () -> Recipe()),
                            (fun recipe ->
                                mutateMembers (fun memberProcess -> memberProcess.ExecutesProtocol <- recipe)
                            ),
                            "swt:iconify-color swt:fluent-color--clipboard-text-edit-20",
                            (fun recipe -> NestedMetadataInput.optionOr "Unnamed recipe" recipe.Name),
                            (ProcessCoreEntityValue.Recipe >> navigate),
                            imports = (fun catalog -> catalog.Recipes)
                        ))
                        TextInput.TextInput(
                            processObject.AdditionalType |> Option.defaultValue "",
                            (fun value ->
                                let additionalType = Option.whereNot System.String.IsNullOrWhiteSpace value

                                mutateMembers (fun memberProcess -> memberProcess.AdditionalType <- additionalType)
                            ),
                            label = "Additional Type"
                        )
                        ioCollapse
                            processView.Inputs
                            (fun () -> SampleNode(ProcessCore.Sample("New Sample")))
                            "Inputs"
                            "Samples and data consumed by this process"
                            "swt:iconify swt:fluent--arrow-download-20-regular"
                            RendererModel.addInput
                            RendererModel.removeInput
                        ioCollapse
                            processView.Outputs
                            (fun () -> DataNode(ProcessCore.Data("New Data")))
                            "Outputs"
                            "Samples and data produced by this process"
                            "swt:iconify swt:fluent--arrow-upload-20-regular"
                            RendererModel.addOutput
                            RendererModel.removeOutput
                        NestedMetadataInput.CreatePCInputSequence(
                            processObject.ParameterValue,
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
                            LayoutComponents.CollectionCollapse(
                                "Parameter Values",
                                "Annotations assigned to this process",
                                processObject.ParameterValue.Count,
                                content,
                                iconClass = "swt:iconify swt:fluent--options-20-regular"
                            )
                    ]
                )
            ],
            true
        )

type ProcessMetadata with

    [<ReactComponent>]
    static member Processes(processes: ResizeArray<Process>, mutate: (ARC -> unit) -> unit) =
        Html.div [
            prop.className "swt:space-y-4"
            prop.children [
                for processObject in processes do
                    ProcessMetadata.ProcessView(RendererModel.ofProcess processObject, mutate)
            ]
        ]

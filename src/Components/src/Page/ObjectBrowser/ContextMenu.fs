namespace Swate.Components.Page.ObjectBrowser

open System
open Fable.Core
open Feliz
open Browser.Types
open ProcessCore
open Swate.Components
open Swate.Components.Composite.InteractiveList
open Swate.Components.Primitive.BaseModal
open Swate.Components.Primitive.Select.Types
open Swate.Components.Primitive.ContextMenu.Types
open Swate.Components.Primitive.ErrorModal.Context
open Swate.Components.Page.ObjectBrowser.Types
open Swate.Components.ProcessCore

module private ContextMenuTypes =

    type ContextMenuTarget = {
        memberKinds: MemberKind array
        entity: ProcessCoreEntity option
        addActions: ContextMenuRequest array
    }

    type MemberCreationConfig = {
        objectName: string
        inputLabel: string
        inputTestId: string
        isInputRequired: bool
        addToArc: ARC -> string -> unit
    }

open ContextMenuTypes

module private ContextMenuHelper =

    let createMemberCreationConfig objectName inputLabel inputTestId isInputRequired addToArc = {
        objectName = objectName
        inputLabel = inputLabel
        inputTestId = inputTestId
        isInputRequired = isInputRequired
        addToArc = addToArc
    }

    let tryGetRowIndex (event: MouseEvent) =
        let target = event.target :?> HTMLElement

        target.closest $"[{Attributes.RowIndex}]"
        |> Option.bind (fun element ->
            match Int32.TryParse(element.getAttribute Attributes.RowIndex) with
            | true, index when index >= 0 -> Some index
            | _ -> None
        )

    let supportsRootCreation =
        function
        | MemberKind.Recipe -> false
        | _ -> true

    let getMemberCreationConfig kind : MemberCreationConfig =
        match kind with
        | MemberKind.Dataset ->
            createMemberCreationConfig "dataset" "Identifier" "dataset-identifier" true EntityCommands.addDataset
        | MemberKind.Process ->
            createMemberCreationConfig "process" "Name" "process-name" true EntityCommands.addProcess
        | MemberKind.Sample -> createMemberCreationConfig "sample" "Name" "sample-name" true EntityCommands.addSample
        | MemberKind.Data -> createMemberCreationConfig "data" "Path" "data-path" true EntityCommands.addData
        | MemberKind.Recipe ->
            createMemberCreationConfig
                "recipe"
                "Name"
                "recipe-name"
                false
                (fun _ _ -> invalidOp "Recipes must be created through a process.")
        | MemberKind.Annotation ->
            createMemberCreationConfig "annotation" "Name" "annotation-name" true EntityCommands.addAnnotation
        | MemberKind.DataContext ->
            createMemberCreationConfig "data context" "Data path" "data-context-path" true EntityCommands.addDataContext
        | MemberKind.Agent ->
            createMemberCreationConfig "agent" "Given name" "agent-given-name" true EntityCommands.addAgent
        | MemberKind.Organization ->
            createMemberCreationConfig "organization" "Name" "organization-name" true EntityCommands.addOrganization
        | MemberKind.ScholarlyArticle ->
            createMemberCreationConfig
                "scholarly article"
                "Headline"
                "article-headline"
                true
                EntityCommands.addScholarlyArticle

    let tryDuplicateMemberWarning arcView (arc: ARC) kind creationConfig value =
        let newMemberName =
            if String.IsNullOrWhiteSpace value then
                $"Unnamed {creationConfig.objectName}"
            else
                value.Trim()

        let alreadyExists =
            ObjectViewModel.getEntitiesWithView arcView arc kind
            |> Seq.map _.displayName
            |> Seq.exists (fun existingName ->
                String.Equals(existingName, newMemberName, StringComparison.OrdinalIgnoreCase)
            )

        if alreadyExists then
            Some $"A {creationConfig.objectName} named '{newMemberName}' already exists."
        else
            None

open ContextMenuHelper

[<Erase; Mangle(false)>]
type ContextMenu =

    [<ReactComponent>]
    static member ContextMenu
        (
            containerRef: IRefValue<HTMLElement option>,
            arcStateCtx: StateUpdaterContext<ARC option>,
            arcView: Swate.Components.ProcessCore.Types.ArcView,
            selectedMemberKind: MemberKind option,
            onArcChanged: MemberKind -> unit,
            ?contextMenuMemberKinds: MemberKind array,
            ?tryGetContextMenuEntity: int -> ProcessCoreEntity option,
            ?tryGetContextMenuMemberKinds: int -> MemberKind array option,
            ?contextMenuAddActions: ContextMenuRequest array,
            ?allowDeleteMembers: bool,
            ?onOpenInMetadataEditor: ProcessCoreEntity -> unit,
            ?onOpenInTableEditor: ProcessCoreEntity -> unit,
            ?actionRequest: ContextMenuRequest,
            ?onActionRequestClosed: unit -> unit
        ) =
        let contextMenuAction, setContextMenuAction =
            React.useState<ContextMenuRequest option> None

        let inputValue, setInputValue = React.useState ""
        let inputRef = React.useInputRef ()

        let duplicateWarning, setDuplicateWarning = React.useState (None: string option)

        let selectedEntityIndices, setSelectedEntityIndices =
            React.useState<Set<int>> Set.empty

        let ioMemberKind, setIOMemberKind = React.useState MemberKind.Sample

        let errorModal = useErrorModalCtx ()
        let allowDeleteMembers = defaultArg allowDeleteMembers true

        let tryPersistArcChange memberKind updateArc =
            match arcStateCtx.state with
            | None -> false
            | Some arc ->
                try
                    updateArc arc
                    arcStateCtx.setStateUpdater (fun _ -> Some arc)
                    onArcChanged memberKind
                    true
                with error ->
                    errorModal.report error.Message
                    false

        let closeModal () =
            setContextMenuAction None
            setInputValue ""
            setDuplicateWarning None
            setSelectedEntityIndices Set.empty
            setIOMemberKind MemberKind.Sample
            onActionRequestClosed |> Option.iter (fun close -> close ())

        let handleModalOpenChange isOpen =
            if not isOpen then
                closeModal ()

        let modalFooter
            (actionTestId: string)
            (actionClass: string)
            (actionLabel: string)
            (isActionDisabled: bool)
            (onAction: unit -> unit)
            =
            React.Fragment [
                Html.button [
                    prop.className "swt:btn"
                    prop.text "Cancel"
                    prop.onClick (fun _ -> closeModal ())
                ]
                Html.button [
                    prop.testId actionTestId
                    prop.className [ "swt:btn swt:ml-auto"; actionClass ]
                    prop.text actionLabel
                    prop.disabled isActionDisabled
                    prop.onClick (fun _ -> onAction ())
                ]
            ]

        let deleteModal
            (objectName: string)
            (description: ReactElement option)
            (children: ReactElement)
            (actionTestId: string)
            (actionLabel: string)
            (isActionDisabled: bool)
            (onDelete: unit -> unit)
            (debug: string)
            =
            BaseModal.Modal(
                isOpen = true,
                setIsOpen = handleModalOpenChange,
                header = Html.text $"Delete {objectName}",
                ?description = description,
                children = children,
                footer = modalFooter actionTestId "swt:btn-error" actionLabel isActionDisabled onDelete,
                debug = debug
            )

        let boxContextMenuTarget memberKinds (entity: ProcessCoreEntity option) addActions =
            box {
                memberKinds = memberKinds
                entity = entity
                addActions = addActions
            }

        let contextMenuItem (label: string) (iconClass: string) action =
            ContextMenuItem(
                text = Html.span label,
                icon = Html.i [ prop.className [ "swt:iconify swt:size-4"; iconClass ] ],
                onClick = (fun _ -> setContextMenuAction (Some action))
            )

        let requiredNameInput submit =
            Html.label [
                prop.className "swt:form-control swt:w-full"
                prop.children [
                    Html.span [
                        prop.className "swt:label-text swt:mb-1"
                        prop.text "Name *"
                    ]
                    Html.input [
                        prop.ref inputRef
                        prop.className "swt:input swt:input-bordered swt:w-full"
                        prop.value inputValue
                        prop.onChange setInputValue
                        prop.onKeyDown (fun event ->
                            if event.key = "Enter" then
                                submit ()
                        )
                    ]
                ]
            ]

        let persistProcessChange update =
            if tryPersistArcChange MemberKind.Process (fun _ -> update ()) then
                closeModal ()

        let processRelationshipModal objectName createChildren update =
            let submittedValue = inputValue.Trim()
            let isInputValid = not (String.IsNullOrWhiteSpace submittedValue)

            let createRelationship () =
                if isInputValid then
                    persistProcessChange (fun () -> update submittedValue)

            BaseModal.Modal(
                isOpen = true,
                setIsOpen = handleModalOpenChange,
                header = Html.text $"Add {objectName}",
                children = createChildren createRelationship,
                footer =
                    modalFooter
                        "process-core-create-relationship"
                        "swt:btn-primary"
                        "Create"
                        (not isInputValid)
                        createRelationship,
                initialFocusRef = unbox inputRef,
                debug = "process-core-create-relationship"
            )

        let tryGetContextMenuSpawnData (event: MouseEvent) =
            let contextIndex = tryGetRowIndex event

            let tryGetAtContextIndex lookup =
                lookup
                |> Option.bind (fun tryGetValue -> contextIndex |> Option.bind tryGetValue)

            let contextEntity = tryGetAtContextIndex tryGetContextMenuEntity
            let contextMemberKinds = tryGetAtContextIndex tryGetContextMenuMemberKinds

            let addActions = contextMenuAddActions |> Option.defaultValue [||]

            match contextEntity, contextMemberKinds |> Option.orElse contextMenuMemberKinds, selectedMemberKind with
            | Some entity, _, _ -> Some(boxContextMenuTarget [| entity.memberKind |] (Some entity) [||])
            | None, Some memberKinds, _ when not (Array.isEmpty memberKinds) ->
                Some(boxContextMenuTarget (Array.distinct memberKinds) None addActions)
            | None, _, Some memberKind ->
                let entity =
                    arcStateCtx.state
                    |> Option.bind (fun arc ->
                        contextIndex
                        |> Option.bind (fun index ->
                            ObjectViewModel.getEntitiesWithView arcView arc memberKind
                            |> Array.tryItem index
                        )
                    )

                Some(boxContextMenuTarget [| memberKind |] entity addActions)
            | None, _, None ->
                contextIndex
                |> Option.filter (fun index -> index < MemberCatalog.Items.Length)
                |> Option.map (fun index -> MemberCatalog.Items.[index].data)
                |> Option.map (fun memberKind -> boxContextMenuTarget [| memberKind |] None addActions)

        let createContextMenuItems (spawnData: obj) =
            let target = unbox<ContextMenuTarget> spawnData

            [
                match onOpenInMetadataEditor, target.entity with
                | Some openInMetadataEditor, Some entity ->
                    ContextMenuItem(
                        text = Html.span "Open in editor",
                        icon =
                            Html.i [
                                prop.className [
                                    "swt:iconify swt:size-4"
                                    "swt:fluent--document-edit-20-regular"
                                ]
                            ],
                        onClick = (fun _ -> openInMetadataEditor entity)
                    )
                | _ -> ()

                match onOpenInTableEditor, target.entity with
                | Some openInTableEditor, Some entity when
                    entity.memberKind = MemberKind.Dataset || entity.memberKind = MemberKind.Process
                    ->
                    ContextMenuItem(
                        text = Html.span "Open in table editor",
                        icon =
                            Html.i [
                                prop.className [ "swt:iconify swt:size-4"; "swt:fluent--table-20-filled" ]
                            ],
                        onClick = (fun _ -> openInTableEditor entity)
                    )
                | _ -> ()

                if target.entity.IsNone then
                    for action in target.addActions do
                        match action with
                        | ContextMenuRequest.AddProcessRelationship(_, relationship) ->
                            let label, icon =
                                match relationship with
                                | ProcessRelationship.Input -> "Add input", "swt:fluent--arrow-enter-20-regular"
                                | ProcessRelationship.Output -> "Add output", "swt:fluent--arrow-exit-20-regular"
                                | ProcessRelationship.ParameterValue ->
                                    "Add parameter value", "swt:fluent--text-bullet-list-add-20-regular"

                            contextMenuItem label icon action
                        | _ -> ()

                if target.entity.IsSome || Array.isEmpty target.addActions then
                    for memberKind in target.memberKinds do
                        let creationConfig = getMemberCreationConfig memberKind

                        if target.entity.IsNone && supportsRootCreation memberKind then
                            contextMenuItem
                                $"Add {creationConfig.objectName}"
                                "swt:fluent--add-20-filled"
                                (ContextMenuRequest.AddMember memberKind)

                        match target.entity with
                        | Some entity ->
                            contextMenuItem
                                $"Delete {creationConfig.objectName}"
                                "swt:fluent--delete-20-filled"
                                (ContextMenuRequest.DeleteEntity entity)
                        | None when allowDeleteMembers ->
                            contextMenuItem
                                $"Delete {creationConfig.objectName}"
                                "swt:fluent--delete-20-filled"
                                (ContextMenuRequest.DeleteMembers memberKind)
                        | None -> ()
            ]

        React.Fragment [
            Swate.Components.Primitive.ContextMenu.ContextMenu.ContextMenu(
                createContextMenuItems,
                containerRef,
                onSpawn = tryGetContextMenuSpawnData
            )

            BaseModal.Modal(
                isOpen = duplicateWarning.IsSome,
                setIsOpen =
                    (fun isOpen ->
                        if not isOpen then
                            setDuplicateWarning None
                    ),
                header = Html.text "Duplicate name",
                children = Html.text (duplicateWarning |> Option.defaultValue ""),
                debug = "process-core-duplicate-warning"
            )

            let activeAction = actionRequest |> Option.orElse contextMenuAction

            match activeAction, arcStateCtx.state with
            | Some(ContextMenuRequest.AddMember memberKind), _ when supportsRootCreation memberKind ->
                let creationConfig = getMemberCreationConfig memberKind
                let submittedValue = inputValue.Trim()

                let isInputValid =
                    not creationConfig.isInputRequired
                    || not (String.IsNullOrWhiteSpace submittedValue)

                let createMember () =
                    if isInputValid then
                        match arcStateCtx.state with
                        | Some arc ->
                            match tryDuplicateMemberWarning arcView arc memberKind creationConfig submittedValue with
                            | Some warning -> setDuplicateWarning (Some warning)
                            | None ->
                                let memberWasCreated =
                                    tryPersistArcChange
                                        memberKind
                                        (fun currentArc -> creationConfig.addToArc currentArc submittedValue)

                                if memberWasCreated then
                                    closeModal ()
                        | None -> ()

                BaseModal.Modal(
                    isOpen = true,
                    setIsOpen = handleModalOpenChange,
                    header = Html.text $"Add {creationConfig.objectName}",
                    description = Html.text "Mandatory fields are marked with an asterisk.",
                    children =
                        Html.label [
                            prop.className "swt:form-control swt:w-full"
                            prop.children [
                                Html.span [
                                    prop.className "swt:label-text swt:mb-1"
                                    prop.text (
                                        if creationConfig.isInputRequired then
                                            $"{creationConfig.inputLabel} *"
                                        else
                                            $"{creationConfig.inputLabel} (optional)"
                                    )
                                ]
                                Html.input [
                                    prop.testId creationConfig.inputTestId
                                    prop.ref inputRef
                                    prop.className "swt:input swt:input-bordered swt:w-full"
                                    prop.required creationConfig.isInputRequired
                                    prop.value inputValue
                                    prop.onChange setInputValue
                                    prop.onKeyDown (fun event ->
                                        if event.key = "Enter" then
                                            createMember ()
                                    )
                                ]
                            ]
                        ],
                    footer =
                        modalFooter "process-core-create" "swt:btn-primary" "Create" (not isInputValid) createMember,
                    initialFocusRef = unbox inputRef,
                    debug = "process-core-create"
                )
            | Some(ContextMenuRequest.AddProcessRelationship(processObject, relationship)), _ when
                relationship = ProcessRelationship.Input
                || relationship = ProcessRelationship.Output
                ->
                let isInput = relationship = ProcessRelationship.Input

                let relationshipName = if isInput then "input" else "output"

                processRelationshipModal
                    relationshipName
                    (fun createRelationship ->
                        Html.div [
                            prop.className "swt:flex swt:flex-col swt:gap-3"
                            prop.children [
                                Html.label [
                                    prop.className "swt:form-control swt:w-full"
                                    prop.children [
                                        Html.span [
                                            prop.className "swt:label-text swt:mb-1"
                                            prop.text "Type"
                                        ]
                                        Html.select [
                                            prop.className "swt:select swt:select-bordered swt:w-full"
                                            prop.value (
                                                if ioMemberKind = MemberKind.Sample then
                                                    "sample"
                                                else
                                                    "data"
                                            )
                                            prop.onChange (fun value ->
                                                setIOMemberKind (
                                                    if value = "sample" then
                                                        MemberKind.Sample
                                                    else
                                                        MemberKind.Data
                                                )
                                            )
                                            prop.children [
                                                Html.option [ prop.value "sample"; prop.text "Sample" ]
                                                Html.option [ prop.value "data"; prop.text "Data" ]
                                            ]
                                        ]
                                    ]
                                ]
                                requiredNameInput createRelationship
                            ]
                        ]
                    )
                    (fun submittedValue ->
                        match isInput, ioMemberKind with
                        | true, MemberKind.Sample -> EntityCommands.addProcessInputSample processObject submittedValue
                        | true, MemberKind.Data -> EntityCommands.addProcessInputData processObject submittedValue
                        | false, MemberKind.Sample ->
                            EntityCommands.addProcessOutputSample processObject submittedValue
                        | false, MemberKind.Data -> EntityCommands.addProcessOutputData processObject submittedValue
                        | _ -> invalidOp "Process inputs and outputs must be samples or data."
                    )
            | Some(ContextMenuRequest.AddProcessRelationship(processObject, relationship)), _ ->
                processRelationshipModal
                    "parameter value"
                    requiredNameInput
                    (EntityCommands.addProcessParameterValue processObject)
            | Some(ContextMenuRequest.DeleteMembers memberKind), Some arc ->
                let creationConfig = getMemberCreationConfig memberKind
                let memberLabel = (MemberCatalog.find memberKind).label
                let entities = ObjectViewModel.getEntitiesWithView arcView arc memberKind

                let selectorOptions: SelectItem<ProcessCoreEntity>[] =
                    entities
                    |> Array.map (fun entity -> {|
                        item = entity
                        label = entity.displayName
                    |})

                let deleteSelectedEntities () =
                    let selectedEntities =
                        selectedEntityIndices
                        |> Seq.choose (fun index -> Array.tryItem index entities)
                        |> Array.ofSeq

                    if
                        not (Array.isEmpty selectedEntities)
                        && tryPersistArcChange
                            memberKind
                            (fun arc -> selectedEntities |> Seq.iter (ObjectViewModel.removeEntityWithView arcView arc))
                    then
                        closeModal ()

                let content =
                    if Array.isEmpty selectorOptions then
                        Html.p [
                            prop.testId "process-core-delete-empty"
                            prop.role.status
                            prop.className "swt:text-base-content/60"
                            prop.text $"No {memberLabel.ToLowerInvariant()} are available."
                        ]
                    else
                        Swate.Components.Primitive.Select.Select.Select(
                            selectorOptions,
                            selectedEntityIndices,
                            setSelectedEntityIndices
                        )

                deleteModal
                    creationConfig.objectName
                    (Some(Html.text $"Select the {memberLabel.ToLowerInvariant()} to delete."))
                    content
                    "process-core-delete-selected"
                    "Delete selected"
                    selectedEntityIndices.IsEmpty
                    deleteSelectedEntities
                    "process-core-delete-selection"
            | Some(ContextMenuRequest.DeleteEntity entity), _ ->
                let deleteEntity () =
                    if
                        tryPersistArcChange
                            entity.memberKind
                            (fun arc -> ObjectViewModel.removeEntityWithView arcView arc entity)
                    then
                        closeModal ()

                deleteModal
                    (getMemberCreationConfig entity.memberKind).objectName
                    None
                    (Html.p $"Shall â€˜{entity.displayName}â€™ really be deleted?")
                    "process-core-delete-entity"
                    "Delete"
                    false
                    deleteEntity
                    "process-core-delete-confirmation"
            | _ -> Html.none
        ]

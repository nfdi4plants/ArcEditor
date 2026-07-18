namespace Swate.Components.Page.ObjectBrowser

open System
open Fable.Core
open Feliz
open Browser.Types
open ProcessCore
open Swate.Components
open Swate.Components.Composite.InteractiveList
open Swate.Components.Primitive.BaseModal
open Swate.Components.Primitive.ContextMenu.Types
open Swate.Components.Primitive.ErrorModal.Context
open Swate.Components.Primitive.Select.Types
open Swate.Components.Page.ObjectBrowser.Types

module ChangeNotification =

    [<Literal>]
    let ArcChangedEvent = "swate-process-core-arc-changed"

    [<Emit("new Event($0)")>]
    let private createEvent (_eventName: string) : Event = jsNative

    let dispatch () =
        Browser.Dom.window.dispatchEvent (createEvent ArcChangedEvent) |> ignore

module private ContextMenuHelper =

    type ContextMenuTarget = {
        memberKind: MemberKind
        entity: ProcessCoreEntity option
    }

    type ContextMenuAction =
        | AddMember of MemberKind
        | DeleteMembers of MemberKind
        | DeleteEntity of ProcessCoreEntity

    type MemberCreationConfig = {
        objectName: string
        inputLabel: string
        inputTestId: string
        isInputRequired: bool
        addToArc: ARC -> string -> unit
    }

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

    let tryGetMemberKind event =
        tryGetRowIndex event
        |> Option.filter (fun index -> index < MemberCatalog.Items.Length)
        |> Option.map (fun index -> MemberCatalog.Items.[index].data)

    let tryGetEntityIndex = tryGetRowIndex

    let getMemberCreationConfig kind : MemberCreationConfig =
        match kind with
        | MemberKind.Dataset ->
            createMemberCreationConfig
                "dataset"
                "Identifier"
                "dataset-identifier"
                true
                (fun arc value -> arc.AddPart(Dataset(value)))
        | MemberKind.Process ->
            createMemberCreationConfig
                "process"
                "Name"
                "process-name"
                true
                (fun arc value -> arc.AddProcess(Process(value)))
        | MemberKind.Sample ->
            createMemberCreationConfig
                "sample"
                "Name"
                "sample-name"
                true
                (fun arc value ->
                    let processObject = Process($"Process for {value}")
                    processObject.AddInputSample(Sample(value))
                    arc.AddProcess processObject
                )
        | MemberKind.Data ->
            createMemberCreationConfig "data" "Path" "data-path" true (fun arc value -> arc.AddDataFile(Data(value)))
        | MemberKind.Recipe ->
            createMemberCreationConfig
                "recipe"
                "Name"
                "recipe-name"
                false
                (fun arc value ->
                    let name = if String.IsNullOrEmpty value then None else Some value
                    let processName = name |> Option.defaultValue $"recipe {Guid.NewGuid():N}"
                    arc.AddProcess(Process($"Process for {processName}", executesProtocol = Recipe(?name = name)))
                )
        | MemberKind.Annotation ->
            createMemberCreationConfig
                "annotation"
                "Name"
                "annotation-name"
                true
                (fun arc value -> arc.AddAdditionalProperty(Annotation(value)))
        | MemberKind.DataContext ->
            createMemberCreationConfig
                "data context"
                "Data path"
                "data-context-path"
                true
                (fun arc value -> arc.AddDataContext(DataContext(Data(value))))
        | MemberKind.Agent ->
            createMemberCreationConfig
                "agent"
                "Given name"
                "agent-given-name"
                true
                (fun arc value -> arc.AddAgent(Agent(value)))
        | MemberKind.Organization ->
            createMemberCreationConfig
                "organization"
                "Name"
                "organization-name"
                true
                (fun arc value -> arc.AddAgent(Agent("Organization contact", affiliation = Organization(value))))
        | MemberKind.ScholarlyArticle ->
            createMemberCreationConfig
                "scholarly article"
                "Headline"
                "article-headline"
                true
                (fun arc value -> arc.AddCitation(ScholarlyArticle(value)))

    let ensureMemberNameIsUnique (arc: ARC) kind creationConfig value =
        let newMemberName =
            if String.IsNullOrWhiteSpace value then
                $"Unnamed {creationConfig.objectName}"
            else
                value.Trim()

        let alreadyExists =
            ObjectViewModel.getNames arc kind
            |> Seq.exists (fun existingName ->
                String.Equals(existingName, newMemberName, StringComparison.OrdinalIgnoreCase)
            )

        if alreadyExists then
            invalidOp $"A {creationConfig.objectName} named '{newMemberName}' already exists."

open ContextMenuHelper

[<Erase; Mangle(false)>]
type ContextMenu =

    [<ReactComponent>]
    static member ContextMenu
        (
            containerRef: IRefValue<HTMLElement option>,
            arcStateCtx: StateUpdaterContext<ARC option>,
            selectedMemberKind: MemberKind option,
            onArcChanged: MemberKind -> unit,
            ?onOpenInTableEditor: ProcessCoreEntity -> unit
        ) =
        let contextMenuAction, setContextMenuAction =
            React.useState<ContextMenuAction option> None

        let inputValue, setInputValue = React.useState ""
        let inputRef = React.useInputRef ()

        let selectedEntityIndices, setSelectedEntityIndices =
            React.useState<Set<int>> Set.empty

        let errorModal = useErrorModalCtx ()

        let tryPersistArcChange memberKind updateArc =
            match arcStateCtx.state with
            | None -> false
            | Some arc ->
                try
                    updateArc arc
                    arcStateCtx.setStateUpdater (fun _ -> Some arc)
                    ChangeNotification.dispatch ()
                    onArcChanged memberKind
                    true
                with error ->
                    errorModal.report error.Message
                    false

        let closeModal () =
            setContextMenuAction None
            setInputValue ""
            setSelectedEntityIndices Set.empty

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

        let boxContextMenuTarget (memberKind: MemberKind) (entity: ProcessCoreEntity option) =
            box {
                memberKind = memberKind
                entity = entity
            }

        let contextMenuItem (label: string) (iconClass: string) action =
            ContextMenuItem(
                text = Html.span label,
                icon = Html.i [ prop.className [ "swt:iconify swt:size-4"; iconClass ] ],
                onClick = (fun _ -> setContextMenuAction (Some action))
            )

        let tryGetContextMenuSpawnData (event: MouseEvent) =
            match selectedMemberKind with
            | Some memberKind ->
                let entity =
                    arcStateCtx.state
                    |> Option.bind (fun arc ->
                        tryGetEntityIndex event
                        |> Option.bind (fun index -> ObjectViewModel.getEntities arc memberKind |> Array.tryItem index)
                    )

                Some(boxContextMenuTarget memberKind entity)
            | None ->
                tryGetMemberKind event
                |> Option.map (fun memberKind -> boxContextMenuTarget memberKind None)

        let createContextMenuItems (spawnData: obj) =
            let target = unbox<ContextMenuTarget> spawnData
            let creationConfig = getMemberCreationConfig target.memberKind

            let deleteAction =
                target.entity
                |> Option.map DeleteEntity
                |> Option.defaultValue (DeleteMembers target.memberKind)

            [
                match onOpenInTableEditor, target.entity, target.memberKind with
                | Some openInTableEditor, Some entity, (MemberKind.Dataset | MemberKind.Process) ->
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
                    contextMenuItem
                        $"Add {creationConfig.objectName}"
                        "swt:fluent--add-20-filled"
                        (AddMember target.memberKind)

                contextMenuItem $"Delete {creationConfig.objectName}" "swt:fluent--delete-20-filled" deleteAction
            ]

        React.Fragment [
            Swate.Components.Primitive.ContextMenu.ContextMenu.ContextMenu(
                createContextMenuItems,
                containerRef,
                onSpawn = tryGetContextMenuSpawnData
            )

            match contextMenuAction, arcStateCtx.state with
            | Some(AddMember memberKind), _ ->
                let creationConfig = getMemberCreationConfig memberKind
                let submittedValue = inputValue.Trim()

                let isInputValid =
                    not creationConfig.isInputRequired
                    || not (String.IsNullOrWhiteSpace submittedValue)

                let createMember () =
                    if isInputValid then
                        let memberWasCreated =
                            tryPersistArcChange
                                memberKind
                                (fun arc ->
                                    ensureMemberNameIsUnique arc memberKind creationConfig submittedValue
                                    creationConfig.addToArc arc submittedValue
                                )

                        if memberWasCreated then
                            closeModal ()

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
            | Some(DeleteMembers memberKind), Some arc ->
                let creationConfig = getMemberCreationConfig memberKind
                let memberLabel = (MemberCatalog.find memberKind).label
                let entities = ObjectViewModel.getEntities arc memberKind

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
                            (fun arc -> ObjectViewModel.removeEntities arc selectedEntities)
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
            | Some(DeleteEntity entity), _ ->
                let deleteEntity () =
                    if tryPersistArcChange entity.memberKind (fun arc -> ObjectViewModel.removeEntity arc entity) then
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

namespace Swate.Components.Composite.ProcessCore

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

module ChangeNotification =

    [<Literal>]
    let ArcChangedEvent = "swate-process-core-arc-changed"

    [<Emit("new Event($0)")>]
    let private createEvent (_eventName: string) : Event = jsNative

    let dispatch () =
        Browser.Dom.window.dispatchEvent (createEvent ArcChangedEvent) |> ignore

module private ContextMenuHelper =

    [<RequireQualifiedAccess>]
    type ContextMenuTarget =
        | MemberKindTarget of MemberKind
        | MemberEntityTarget of ProcessCoreEntity

        member this.MemberKind =
            match this with
            | ContextMenuTarget.MemberKindTarget memberKind -> memberKind
            | ContextMenuTarget.MemberEntityTarget entity -> entity.memberKind

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

    [<Literal>]
    let MemberKindAttribute = "data-process-core-kind"

    let tryGetElementIndex attributeName (event: MouseEvent) =
        let target = event.target :?> HTMLElement

        target.closest $"[{attributeName}]"
        |> Option.bind (fun element ->
            match Int32.TryParse(element.getAttribute attributeName) with
            | true, index when index >= 0 -> Some index
            | _ -> None
        )

    let tryGetMemberKind event =
        tryGetElementIndex MemberKindAttribute event
        |> Option.filter (fun index -> index < MemberCatalog.Items.Length)
        |> Option.map (fun index -> MemberCatalog.Items.[index].data)

    let tryGetEntityIndex event =
        tryGetElementIndex Attributes.RowIndex event

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
            onArcChanged: MemberKind -> unit
        ) =
        let memberKindToCreate, setMemberKindToCreate =
            React.useState<MemberKind option> None

        let inputValue, setInputValue = React.useState ""
        let inputRef = React.useInputRef ()

        let memberKindToDelete, setMemberKindToDelete =
            React.useState<MemberKind option> None

        let entityToDelete, setEntityToDelete =
            React.useState<ProcessCoreEntity option> None

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

        let handleModalOpenChange isOpen =
            if not isOpen then
                setMemberKindToCreate None
                setInputValue ""

        let closeMemberSelectionModal () =
            setMemberKindToDelete None
            setSelectedEntityIndices Set.empty

        let tryGetContextMenuSpawnData (event: MouseEvent) =
            match selectedMemberKind with
            | Some memberKind ->
                let entityTarget =
                    arcStateCtx.state
                    |> Option.bind (fun arc ->
                        tryGetEntityIndex event
                        |> Option.bind (fun index -> ObjectViewModel.getEntities arc memberKind |> Array.tryItem index)
                    )
                    |> Option.map ContextMenuTarget.MemberEntityTarget

                entityTarget
                |> Option.defaultValue (ContextMenuTarget.MemberKindTarget memberKind)
                |> box
                |> Some
            | None -> tryGetMemberKind event |> Option.map (ContextMenuTarget.MemberKindTarget >> box)

        let createContextMenuItems (spawnData: obj) =
            let target = unbox<ContextMenuTarget> spawnData
            let memberKind = target.MemberKind
            let creationConfig = getMemberCreationConfig memberKind

            let addItems =
                match target with
                | ContextMenuTarget.MemberKindTarget _ -> [
                    ContextMenuItem(
                        text = Html.span $"Add {creationConfig.objectName}",
                        icon =
                            Html.i [
                                prop.className "swt:iconify swt:fluent--add-20-filled swt:size-4"
                            ],
                        onClick = (fun _ -> setMemberKindToCreate (Some memberKind))
                    )
                  ]
                | ContextMenuTarget.MemberEntityTarget _ -> []

            addItems
            @ [
                ContextMenuItem(
                    text = Html.span $"Delete {creationConfig.objectName}",
                    icon =
                        Html.i [
                            prop.className "swt:iconify swt:fluent--delete-20-filled swt:size-4"
                        ],
                    onClick =
                        (fun _ ->
                            match target with
                            | ContextMenuTarget.MemberKindTarget memberKind ->
                                setSelectedEntityIndices Set.empty
                                setMemberKindToDelete (Some memberKind)
                            | ContextMenuTarget.MemberEntityTarget entity -> setEntityToDelete (Some entity)
                        )
                )
            ]

        React.Fragment [
            Swate.Components.Primitive.ContextMenu.ContextMenu.ContextMenu(
                createContextMenuItems,
                containerRef,
                onSpawn = tryGetContextMenuSpawnData
            )

            match memberKindToCreate with
            | None -> Html.none
            | Some memberKind ->
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
                            handleModalOpenChange false

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
                        React.Fragment [
                            Html.button [
                                prop.className "swt:btn"
                                prop.text "Cancel"
                                prop.onClick (fun _ -> handleModalOpenChange false)
                            ]
                            Html.button [
                                prop.testId "process-core-create"
                                prop.className "swt:btn swt:btn-primary swt:ml-auto"
                                prop.text "Create"
                                prop.disabled (not isInputValid)
                                prop.onClick (fun _ -> createMember ())
                            ]
                        ],
                    initialFocusRef = unbox inputRef,
                    debug = "process-core-create"
                )

            match memberKindToDelete, arcStateCtx.state with
            | Some memberKind, Some arc ->
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
                        closeMemberSelectionModal ()

                BaseModal.Modal(
                    isOpen = true,
                    setIsOpen =
                        (fun isOpen ->
                            if not isOpen then
                                closeMemberSelectionModal ()
                        ),
                    header = Html.text $"Delete {creationConfig.objectName}",
                    description = Html.text $"Select the {memberLabel.ToLowerInvariant()} to delete.",
                    children =
                        (if Array.isEmpty selectorOptions then
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
                             )),
                    footer =
                        React.Fragment [
                            Html.button [
                                prop.className "swt:btn"
                                prop.text "Cancel"
                                prop.onClick (fun _ -> closeMemberSelectionModal ())
                            ]
                            Html.button [
                                prop.testId "process-core-delete-selected"
                                prop.className "swt:btn swt:btn-error swt:ml-auto"
                                prop.text "Delete selected"
                                prop.disabled selectedEntityIndices.IsEmpty
                                prop.onClick (fun _ -> deleteSelectedEntities ())
                            ]
                        ],
                    debug = "process-core-delete-selection"
                )
            | _ -> Html.none

            match entityToDelete with
            | None -> Html.none
            | Some entity ->
                let creationConfig = getMemberCreationConfig entity.memberKind

                let deleteEntity () =
                    if tryPersistArcChange entity.memberKind (fun arc -> ObjectViewModel.removeEntity arc entity) then
                        setEntityToDelete None

                BaseModal.Modal(
                    isOpen = true,
                    setIsOpen =
                        (fun isOpen ->
                            if not isOpen then
                                setEntityToDelete None
                        ),
                    header = Html.text $"Delete {creationConfig.objectName}",
                    children = Html.p $"Shall ‘{entity.displayName}’ really be deleted?",
                    footer =
                        React.Fragment [
                            Html.button [
                                prop.className "swt:btn"
                                prop.text "Cancel"
                                prop.onClick (fun _ -> setEntityToDelete None)
                            ]
                            Html.button [
                                prop.testId "process-core-delete-entity"
                                prop.className "swt:btn swt:btn-error swt:ml-auto"
                                prop.text "Delete"
                                prop.onClick (fun _ -> deleteEntity ())
                            ]
                        ],
                    debug = "process-core-delete-confirmation"
                )
        ]

namespace Swate.Components.Composite.ProcessCore

open System
open Fable.Core
open Fable.Core.JsInterop
open Feliz
open Browser.Types
open ProcessCore
open Swate.Components
open Swate.Components.Primitive.BaseModal
open Swate.Components.Primitive.ContextMenu.Types
open Swate.Components.Primitive.ErrorModal.Context

module private ContextMenuHelper =

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

    let tryGetMemberKind (event: MouseEvent) =
        let target = event.target :?> HTMLElement

        target.closest $"[{MemberKindAttribute}]"
        |> Option.bind (fun element ->
            let index: obj = (element :?> HTMLElement)?dataset?processCoreKind

            match Int32.TryParse(string index) with
            | true, index when index >= 0 && index < MemberCatalog.Items.Length ->
                Some MemberCatalog.Items.[index].data
            | _ -> None
        )

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
            onMemberCreated: MemberKind -> unit
        ) =
        let selectedMemberKind, setSelectedMemberKind =
            React.useState<MemberKind option> None

        let inputValue, setInputValue = React.useState ""
        let inputRef = React.useInputRef ()
        let errorModal = useErrorModalCtx ()

        let handleModalOpenChange isOpen =
            if not isOpen then
                setSelectedMemberKind None
                setInputValue ""

        let tryGetContextMenuSpawnData (event: MouseEvent) =
            tryGetMemberKind event |> Option.map box

        let createContextMenuItems (spawnData: obj) =
            let memberKind = unbox<MemberKind> spawnData
            let creationConfig = getMemberCreationConfig memberKind

            [
                ContextMenuItem(
                    text = Html.span $"Add {creationConfig.objectName}",
                    icon =
                        Html.i [
                            prop.className "swt:iconify swt:fluent--add-20-filled swt:size-4"
                        ],
                    onClick = (fun _ -> setSelectedMemberKind (Some memberKind))
                )
            ]

        React.Fragment [
            Swate.Components.Primitive.ContextMenu.ContextMenu.ContextMenu(
                createContextMenuItems,
                containerRef,
                onSpawn = tryGetContextMenuSpawnData
            )

            match selectedMemberKind with
            | None -> Html.none
            | Some memberKind ->
                let creationConfig = getMemberCreationConfig memberKind
                let submittedValue = inputValue.Trim()

                let isInputValid =
                    not creationConfig.isInputRequired
                    || not (String.IsNullOrWhiteSpace submittedValue)

                let createMember () =
                    if isInputValid then
                        match arcStateCtx.state with
                        | None -> ()
                        | Some arc ->
                            try
                                ensureMemberNameIsUnique arc memberKind creationConfig submittedValue

                                arcStateCtx.setStateUpdater (
                                    Option.map (fun arc ->
                                        creationConfig.addToArc arc submittedValue
                                        arc
                                    )
                                )

                                onMemberCreated memberKind
                                handleModalOpenChange false
                            with error ->
                                errorModal.report error.Message

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
        ]

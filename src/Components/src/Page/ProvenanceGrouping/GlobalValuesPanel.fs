namespace Swate.Components.Page.ProvenanceGrouping

open Fable.Core
open Feliz
open Swate.Components.Primitive.Popover
open Swate.Components.Page.ProvenanceGrouping.Identifiers
open Swate.Components.Page.ProvenanceGrouping.Values
open Swate.Components.Page.ProvenanceGrouping.Domain
open Swate.Components.Page.ProvenanceGrouping.MutationTypes
open Swate.Components.Page.ProvenanceGrouping.ProjectionTypes
open Swate.Components.Page.ProvenanceGrouping.Types
open Swate.Components.Composite.TermSearch

/// Impact counts for the destructive-confirmation step (design §5: "Global
/// removal actions should be presented as destructive operations because
/// they may affect several layers, nodes, and processes"). Shared with the
/// rail removal confirms, which receive the counts as plain values.
module GlobalValuesImpact =

    let valueAssignmentCount (valueId: PropertyValueDefinitionId) (session: ProvenanceSession) =
        let nodeCount =
            session.Nodes
            |> Map.toList
            |> List.sumBy (fun (_, node) ->
                node.Assignments
                |> Map.toList
                |> List.filter (fun (_, a) -> a.ValueId = valueId)
                |> List.length
            )

        let processCount =
            session.Processes
            |> Map.toList
            |> List.sumBy (fun (_, proc) ->
                proc.Assignments
                |> Map.toList
                |> List.filter (fun (_, a) -> a.ValueId = valueId)
                |> List.length
            )

        nodeCount + processCount

    let propertyValueIds (propertyId: PropertyDefinitionId) (session: ProvenanceSession) =
        session.Values
        |> Map.toList
        |> List.choose (fun (valueId, value) -> if value.PropertyId = propertyId then Some valueId else None)

    let propertyAssignmentCount (propertyId: PropertyDefinitionId) (session: ProvenanceSession) =
        propertyValueIds propertyId session
        |> List.sumBy (fun valueId -> valueAssignmentCount valueId session)

type private GlobalRemovalTarget =
    | GlobalValueRemoval of PropertyValueDefinitionId
    | GlobalPropertyRemoval of PropertyDefinitionId

/// The global value/property management surface design §15.3 calls "the
/// sidebar": editing or removing a value/property here is explicit global
/// scope (intent §4/§5), distinct from every owner-scoped rail/canvas action
/// elsewhere in this editor. Read-only dependents (Recipe Components,
/// Reference values) are not special-cased in this UI - matching the
/// recorded decision for the connector menu, the command layer
/// (`Commands.editValueGlobally`/`removeValuesGlobally`/`removePropertyGlobally`)
/// is the enforcement point, surfacing `ReadOnlyAdapterResourceMutation`
/// through the ordinary error banner via the caller's `publish`.
[<Erase; Mangle(false)>]
type GlobalValuesPanel =

    [<ReactComponent>]
    static member Main
        (session: ProvenanceSession, publish: Result<ProvenanceSession, ProvenanceCommandError> -> unit, ?debug: bool)
        =
        let debug = defaultArg debug false

        let editingValueId, setEditingValueId =
            React.useState (None: PropertyValueDefinitionId option)

        let draftKind, setDraftKind = React.useState DraftText
        let draftText, setDraftText = React.useState ""
        let draftTerm, setDraftTerm = React.useState (None: ProvenanceTerm option)
        let draftUnit, setDraftUnit = React.useState (None: ProvenanceTerm option)

        let pendingRemoval, setPendingRemoval =
            React.useState (None: GlobalRemovalTarget option)

        let beginEdit (value: PropertyValueDefinition) =
            setEditingValueId (Some value.Id)
            setDraftKind (ValueDrafts.kindOf value.Value)
            setDraftText (ValueDrafts.textOf value.Value)
            setDraftTerm (ValueDrafts.termOf value.Value)
            setDraftUnit value.Unit

        let confirmEdit (value: PropertyValueDefinition) =
            match ValueDrafts.tryValue draftKind draftText draftTerm with
            | None -> ()
            | Some nextValue ->
                match session.Properties |> Map.tryFind value.PropertyId with
                | None -> ()
                | Some property ->
                    let content: Commands.NodeValueContent = {
                        Category = property.Category
                        Value = nextValue
                        Unit = draftUnit
                    }

                    Session.editValueGlobally value.Id content session |> publish
                    setEditingValueId None

        let confirmRemoval target =
            match target with
            | GlobalValueRemoval valueId -> Session.removeValuesGlobally (Set.singleton valueId) session |> publish
            | GlobalPropertyRemoval propertyId -> Session.removePropertyGlobally propertyId session |> publish

            setPendingRemoval None

        let editForm (value: PropertyValueDefinition) =
            let canConfirm = (ValueDrafts.tryValue draftKind draftText draftTerm).IsSome

            Html.form [
                prop.className "swt:flex swt:flex-col swt:gap-2 swt:rounded swt:border swt:border-base-300 swt:p-2"
                prop.onSubmit (fun event ->
                    event.preventDefault ()
                    confirmEdit value
                )
                prop.children [
                    Html.select [
                        prop.ariaLabel "Value type"
                        prop.className "swt:select swt:select-bordered swt:select-xs"
                        prop.value (ValueDrafts.kindName draftKind)
                        prop.onChange (ValueDrafts.kindFromName >> setDraftKind)
                        prop.children [
                            Html.option [ prop.value "Text"; prop.text "Text" ]
                            Html.option [ prop.value "Integer"; prop.text "Integer" ]
                            Html.option [ prop.value "Float"; prop.text "Float" ]
                            Html.option [ prop.value "Term"; prop.text "Term" ]
                        ]
                    ]
                    match draftKind with
                    | DraftTerm ->
                        TermSearch.TermSearch(
                            draftTerm |> Option.map TermSearchMapping.toTermSearchTerm,
                            (fun next -> setDraftTerm (next |> Option.bind TermSearchMapping.fromTermSearchTerm))
                        )
                    | _ ->
                        Html.input [
                            prop.ariaLabel "value"
                            prop.className "swt:input swt:input-bordered swt:input-xs"
                            prop.value draftText
                            prop.onChange setDraftText
                            if debug then
                                prop.testId "provenance-global-edit-value-input"
                        ]
                    Html.div [
                        prop.className "swt:flex swt:gap-2"
                        prop.children [
                            Html.button [
                                prop.type'.submit
                                prop.disabled (not canConfirm)
                                prop.className "swt:btn swt:btn-primary swt:btn-xs"
                                if debug then
                                    prop.testId "provenance-confirm-global-value-edit"
                                prop.text "Save"
                            ]
                            Html.button [
                                prop.type'.button
                                prop.className "swt:btn swt:btn-ghost swt:btn-xs"
                                prop.onClick (fun _ -> setEditingValueId None)
                                prop.text "Cancel"
                            ]
                        ]
                    ]
                ]
            ]

        let removalConfirm (target: GlobalRemovalTarget) =
            let headerText, description =
                match target with
                | GlobalValueRemoval valueId ->
                    let count = GlobalValuesImpact.valueAssignmentCount valueId session
                    GlobalRemovalWording.valueRemoval count
                | GlobalPropertyRemoval propertyId ->
                    let name =
                        session.Properties
                        |> Map.tryFind propertyId
                        |> Option.map (fun property -> property.Category.Name)
                        |> Option.defaultValue "this property"

                    let count = GlobalValuesImpact.propertyAssignmentCount propertyId session
                    GlobalRemovalWording.propertyRemoval name count

            Html.div [
                prop.className "swt:alert swt:alert-warning swt:flex-wrap swt:items-start"
                if debug then
                    prop.testId "provenance-global-removal-prompt"
                prop.children [
                    Html.div [
                        prop.className "swt:flex swt:flex-col swt:gap-1"
                        prop.children [
                            Html.strong [ prop.text headerText ]
                            Html.span [ prop.className "swt:text-sm"; prop.text description ]
                        ]
                    ]
                    Html.div [
                        prop.className "swt:ml-auto swt:flex swt:gap-2"
                        prop.children [
                            Html.button [
                                prop.type'.button
                                prop.className "swt:btn swt:btn-warning swt:btn-xs"
                                if debug then
                                    prop.testId "provenance-confirm-global-removal"
                                prop.onClick (fun _ -> confirmRemoval target)
                                prop.text "Delete"
                            ]
                            Html.button [
                                prop.type'.button
                                prop.className "swt:btn swt:btn-ghost swt:btn-xs"
                                if debug then
                                    prop.testId "provenance-cancel-global-removal"
                                prop.onClick (fun _ -> setPendingRemoval None)
                                prop.text "Cancel"
                            ]
                        ]
                    ]
                ]
            ]

        let valueRow (value: PropertyValueDefinition) =
            let text = Formatting.formatValue value.Value value.Unit

            Html.div [
                prop.key value.Id
                prop.className "swt:flex swt:flex-col swt:gap-1"
                prop.children [
                    Html.div [
                        prop.className "swt:flex swt:items-center swt:gap-2 swt:pl-4"
                        prop.children [
                            Html.span [
                                prop.className "swt:flex-1 swt:truncate swt:text-sm"
                                prop.text text
                            ]
                            Html.button [
                                prop.type'.button
                                prop.className "swt:btn swt:btn-ghost swt:btn-xs"
                                if debug then
                                    prop.testId $"provenance-global-edit-value-{value.Id}"
                                prop.onClick (fun _ -> beginEdit value)
                                prop.text "Edit"
                            ]
                            Html.button [
                                prop.type'.button
                                prop.className "swt:btn swt:btn-ghost swt:btn-xs swt:text-error"
                                if debug then
                                    prop.testId $"provenance-global-remove-value-{value.Id}"
                                prop.onClick (fun _ -> setPendingRemoval (Some(GlobalValueRemoval value.Id)))
                                prop.text "Delete"
                            ]
                        ]
                    ]
                    match editingValueId with
                    | Some editingId when editingId = value.Id -> editForm value
                    | _ -> Html.none
                    match pendingRemoval with
                    | Some(GlobalValueRemoval removalId) when removalId = value.Id ->
                        removalConfirm (GlobalValueRemoval removalId)
                    | _ -> Html.none
                ]
            ]

        let propertyRow (property: PropertyDefinition) =
            let values =
                session.Values
                |> Map.toList
                |> List.map snd
                |> List.filter (fun value -> value.PropertyId = property.Id)
                |> List.sortBy (fun value -> Formatting.formatValue value.Value value.Unit)

            Html.div [
                prop.key property.Id
                prop.className "swt:flex swt:flex-col swt:gap-1"
                prop.children [
                    Html.div [
                        prop.className "swt:flex swt:items-center swt:gap-2"
                        prop.children [
                            Html.strong [
                                prop.className "swt:flex-1 swt:text-sm"
                                prop.text property.Category.Name
                            ]
                            Html.button [
                                prop.type'.button
                                prop.className "swt:btn swt:btn-ghost swt:btn-xs swt:text-error"
                                if debug then
                                    prop.testId $"provenance-global-remove-property-{property.Id}"
                                prop.onClick (fun _ -> setPendingRemoval (Some(GlobalPropertyRemoval property.Id)))
                                prop.text "Delete property"
                            ]
                        ]
                    ]
                    for value in values do
                        valueRow value
                    match pendingRemoval with
                    | Some(GlobalPropertyRemoval removalId) when removalId = property.Id ->
                        removalConfirm (GlobalPropertyRemoval removalId)
                    | _ -> Html.none
                ]
            ]

        let properties =
            session.Properties
            |> Map.toList
            |> List.map snd
            |> List.sortBy (fun property -> property.Category.Name)

        Popover.Simple(
            trigger =
                Html.button [
                    prop.type'.button
                    prop.className "swt:btn swt:btn-ghost swt:btn-sm"
                    if debug then
                        prop.testId "provenance-global-values-trigger"
                    prop.text "Manage values"
                ],
            content =
                Html.div [
                    prop.className "swt:flex swt:max-h-96 swt:w-80 swt:flex-col swt:gap-3 swt:overflow-y-auto swt:p-2"
                    if debug then
                        prop.testId "provenance-global-values-panel"
                    prop.children [
                        if properties.IsEmpty then
                            Html.p [
                                prop.className "swt:text-sm swt:text-base-content/60"
                                prop.text "No annotation properties in this session."
                            ]
                        for property in properties do
                            propertyRow property
                    ]
                ],
            ?debug = (if debug then Some "provenance-global-values" else None)
        )

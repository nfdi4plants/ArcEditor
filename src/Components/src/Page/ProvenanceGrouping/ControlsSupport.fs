namespace Swate.Components.Page.ProvenanceGrouping

open System
open Fable.Core
open Fable.Core.JsInterop
open Feliz
open Swate.Components
open Swate.Components.JsBindings
open Swate.Components.Primitive.Buttons
open Swate.Components.Primitive.Dropdown
open Swate.Components.Primitive.Popover
open Swate.Components.Page.ProvenanceGrouping.ProvenanceTypes
open Swate.Components.Page.ProvenanceGrouping.Grouping
open Swate.Components.Page.ProvenanceGrouping.Edit
open Swate.Components.Page.ProvenanceGrouping.Session
open Swate.Components.Page.ProvenanceGrouping.Types
open Swate.Components.Composite.TermSearch
open Swate.Components.Composite.TermSearch.Types

type DraftValueKind =
    | DraftText
    | DraftInteger
    | DraftFloat
    | DraftTerm

/// Converts provenance sides into lower-case text used in labels and test ids.
module SideLabels =

    let sideName side =
        match side with
        | ProvenanceSide.Input -> "input"
        | ProvenanceSide.Output -> "output"

module ColorPicker =

    let fallbackColor =
        State.PropertyColors.palette |> Array.tryHead |> Option.defaultValue "#2563eb"

    let currentOrFallback color =
        match color with
        | Some c when c <> "" -> c
        | _ -> fallbackColor

    let content ariaLabel (draftColor: string) (setDraftColor: string -> unit) onSetColor =
        Html.div [
            prop.className "swt:flex swt:items-center swt:gap-2 swt:p-2"
            prop.children [
                Html.input [
                    prop.custom ("type", "color")
                    prop.className "swt:h-8 swt:w-10 swt:cursor-pointer swt:rounded swt:border swt:border-base-300"
                    prop.value draftColor
                    prop.ariaLabel ariaLabel
                    prop.onChange (fun (color: string) -> setDraftColor color)
                ]
                Html.button [
                    prop.type'.button
                    prop.className "swt:btn swt:btn-xs swt:btn-primary swt:min-h-0 swt:py-0"
                    prop.text "Select"
                    prop.onClick (fun _ -> onSetColor (Some draftColor))
                ]
                Html.button [
                    prop.type'.button
                    prop.className "swt:btn swt:btn-xs swt:btn-ghost swt:min-h-0 swt:py-0"
                    prop.text "Clear"
                    prop.onClick (fun _ ->
                        setDraftColor fallbackColor
                        onSetColor None
                    )
                ]
            ]
        ]

module OriginSymbols =

    let upstreamIcon size =
        Html.i [
            prop.className [ "swt:iconify swt:fluent--arrow-up-20-regular"; size ]
        ]

    let currentIcon size =
        Html.i [
            prop.className [ "swt:iconify swt:fluent--circle-20-filled"; size ]
        ]

    let bothIcons size =
        Html.span [
            prop.className "swt:inline-flex swt:items-center swt:gap-1"
            prop.children [
                upstreamIcon size
                Html.span [
                    prop.className "swt:h-4 swt:w-px swt:bg-current swt:opacity-60"
                ]
                currentIcon size
            ]
        ]

/// Converts between draft form state and typed provenance values.
module ValueDrafts =

    let kindName kind =
        match kind with
        | DraftText -> "Text"
        | DraftInteger -> "Integer"
        | DraftFloat -> "Float"
        | DraftTerm -> "Term"

    let kindFromName value =
        match value with
        | "Integer" -> DraftInteger
        | "Float" -> DraftFloat
        | "Term" -> DraftTerm
        | _ -> DraftText

    let tryValue kind (text: string) term =
        match kind with
        | DraftText -> Some(ProvenanceValue.Text text)
        | DraftInteger ->
            match Int32.TryParse text with
            | true, value -> Some(ProvenanceValue.Integer value)
            | _ -> None
        | DraftFloat ->
            match Double.TryParse text with
            | true, value when not (Double.IsNaN value || Double.IsInfinity value) -> Some(ProvenanceValue.Float value)
            | _ -> None
        | DraftTerm -> term |> Option.map ProvenanceValue.Term

/// Maps provenance terms to the TermSearch component shape and back.
module TermSearchMapping =

    let toTermSearchTerm (term: ProvenanceTerm) =
        Term(name = term.Name, ?id = term.TermAccession, ?source = term.TermSource)

    let fromTermSearchTerm (term: Term) =
        term.name
        |> Option.map (fun name -> {
            Name = name
            TermSource = term.source
            TermAccession = term.id
        })

/// Creates editor-owned generic provenance kinds for user-created values.
module KindNames =

    let editorProperty = ProvenanceKind.create "editor:property" "Annotation"

/// Right-click removal on the property rail. The rail marks its header
/// buttons and value chips with identity attributes, and one context menu per
/// rail resolves the clicked element back to the property key or value.
module RailContextMenu =

    [<Literal>]
    let headerAttribute = "data-provenance-rail-property"

    [<Literal>]
    let valueAttribute = "data-provenance-rail-value"

    [<RequireQualifiedAccess>]
    type RailMenuTarget =
        | Header of ProvenancePropertyKey
        | Value of ProvenancePropertyValue

    let private closestAttribute (attribute: string) (event: Browser.Types.MouseEvent) =
        let targetObj: obj = box event.target

        if isNullOrUndefined targetObj || isNullOrUndefined targetObj?closest then
            None
        else
            let node: Browser.Types.Element = !! targetObj?closest ($"[{attribute}]")

            if isNull node then
                None
            else
                let value = node.getAttribute attribute
                if isNull value then None else Some value

    /// Values win over headers: a chip sits inside the header's row, so the
    /// nearest match decides which of the two the click meant.
    let spawnData
        (headers: ProvenancePropertyKey list)
        (valuesForHeader: ProvenancePropertyKey -> ProvenancePropertyValue list)
        (identityOfHeader: ProvenancePropertyKey -> string)
        (event: Browser.Types.MouseEvent)
        =
        let byValue =
            closestAttribute valueAttribute event
            |> Option.bind (fun valueId ->
                headers
                |> List.collect valuesForHeader
                |> List.tryFind (fun propertyValue -> propertyValue.Id = valueId)
                |> Option.map RailMenuTarget.Value
            )

        let byHeader () =
            closestAttribute headerAttribute event
            |> Option.bind (fun identity ->
                headers
                |> List.tryFind (fun header -> identityOfHeader header = identity)
                |> Option.map RailMenuTarget.Header
            )

        byValue |> Option.orElseWith byHeader |> Option.map box

    let items
        (onRemoveHeader: ProvenancePropertyKey -> unit)
        (onRemoveValue: ProvenancePropertyValue -> unit)
        (data: obj)
        =
        let icon =
            Html.i [
                prop.className "swt:iconify swt:fluent--delete-20-regular swt:size-4"
            ]

        match data |> unbox<RailMenuTarget> with
        | RailMenuTarget.Header header -> [
            Swate.Components.Primitive.ContextMenu.Types.ContextMenuItem(
                text = Html.span $"Remove {header.Header.Category.Name} everywhere",
                icon = icon,
                onClick =
                    (fun event ->
                        event.buttonEvent.stopPropagation ()
                        onRemoveHeader header
                    )
            )
          ]
        | RailMenuTarget.Value propertyValue -> [
            Swate.Components.Primitive.ContextMenu.Types.ContextMenuItem(
                text = Html.span "Remove this value everywhere",
                icon = icon,
                onClick =
                    (fun event ->
                        event.buttonEvent.stopPropagation ()
                        onRemoveValue propertyValue
                    )
            )
          ]

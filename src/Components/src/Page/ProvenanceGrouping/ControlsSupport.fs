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
open Swate.Components.Page.ProvenanceGrouping.Identifiers
open Swate.Components.Page.ProvenanceGrouping.Values
open Swate.Components.Page.ProvenanceGrouping.Types
open Swate.Components.Composite.TermSearch
open Swate.Components.Composite.TermSearch.Types

/// Converts provenance sides into lower-case text used in labels and test ids.
module SideLabels =

    let sideName side =
        match side with
        | ProvenanceSide.Input -> "input"
        | ProvenanceSide.Output -> "output"

module ColorPicker =

    let fallbackColor = State.PropertyColors.defaultColor

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

    /// The draft kind an already-typed value round-trips through when editing
    /// it. A `Reference` has no draft representation (Recipe/catalog values are
    /// edited through their own flow, not this generic form), so it falls back
    /// to `Text` rather than losing the edit surface entirely.
    let kindOf value =
        match value with
        | ProvenanceValue.Text _ -> DraftText
        | ProvenanceValue.Integer _ -> DraftInteger
        | ProvenanceValue.Float _ -> DraftFloat
        | ProvenanceValue.Term _ -> DraftTerm
        | ProvenanceValue.Reference _ -> DraftText

    let textOf value =
        match value with
        | ProvenanceValue.Text v -> v
        | ProvenanceValue.Integer v -> string v
        | ProvenanceValue.Float v -> string v
        | _ -> ""

    let termOf value =
        match value with
        | ProvenanceValue.Term term -> Some term
        | _ -> None

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

    let editorProperty: ProvenanceKind = {
        Id = "editor:property"
        Label = "Annotation"
    }

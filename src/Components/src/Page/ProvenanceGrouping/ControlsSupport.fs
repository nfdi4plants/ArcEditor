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

/// Node and process annotations never group together and are edited and written
/// back differently, so every surface that shows one says which it is. A node
/// annotation is owned by the endpoint itself; a process annotation belongs to a
/// process and reaches the endpoint through the edges incident to it.
module AnnotationKindSymbols =

    let name =
        function
        | AnnotationOwnerKind.Node -> "Node annotation"
        | AnnotationOwnerKind.Process -> "Edge annotation"

    /// The one-line explanation used as a tooltip wherever the icon appears.
    let description =
        function
        | AnnotationOwnerKind.Node -> "Node annotation: owned by this entity."
        | AnnotationOwnerKind.Process -> "Edge annotation: carried by the connections at this entity."

    let icon (size: string) (kind: AnnotationOwnerKind) =
        let glyph =
            match kind with
            | AnnotationOwnerKind.Node -> "swt:iconify swt:fluent--circle-20-filled"
            | AnnotationOwnerKind.Process -> "swt:iconify swt:fluent--flow-20-regular"

        Html.i [ prop.ariaHidden true; prop.className [ glyph; size ] ]

    /// Icon plus a screen-reader-only name, for surfaces with no other place to
    /// carry the distinction.
    let badge (size: string) (kind: AnnotationOwnerKind) =
        Html.span [
            prop.className "swt:inline-flex swt:shrink-0 swt:items-center"
            prop.title (description kind)
            prop.children [
                icon size kind
                Html.span [ prop.className "swt:sr-only"; prop.text (name kind) ]
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

/// Confirm wording for the two global-removal surfaces - the rail's inline
/// confirm and the Manage-values panel - which must say the same thing and
/// must not drift. Removal is by value definition, and grouping keys carry
/// origin source while value definitions do not: two displayed entries that
/// share header, value and unit share one definition, so deleting it takes
/// every entry that displays it, wherever it is shown. The wording states
/// that reach outright rather than counting sibling entries, because such a
/// count would be side- and layer-dependent and understate the reach
/// whenever a sibling is not currently displayed.
module GlobalRemovalWording =

    let valueRemoval (assignmentCount: int) =
        "Delete this value?",
        $"Deletes the value definition itself, so every entry that displays it disappears - including entries on the other side's rail and process entries of another origin - and removes it from {assignmentCount} assignment(s) across the session."

    let propertyRemoval (categoryName: string) (assignmentCount: int) =
        $"Delete {categoryName} everywhere?",
        $"Deletes this category for node and process annotations alike, with every value it has: every entry that displays one of its values disappears - including entries on the other side's rail and process entries of another origin - along with {assignmentCount} assignment(s) across the session."

namespace Swate.Components.ProcessCore

open Fable.Core
open Feliz
open ProcessCore
open Swate.Components.ProcessCore.Copy
open Swate.Components.Primitive.BaseModal

[<Erase; Mangle(false)>]
type MandatoryFieldRepair =

    /// Presents each missing mandatory field and emits a repaired ARC after completion.
    [<ReactComponent>]
    static member MandatoryFieldRepair(arc: ARC option, onRepaired: ARC -> unit) =
        let issues, setIssues = React.useState<Hotfixes.PrimaryFieldIssue list> []
        let value, setValue = React.useState ""
        let inputRef = React.useInputRef ()
        let normalizedValue = Hotfixes.tryNormalizeRequiredValue value

        React.useEffect (
            (fun () ->
                setIssues (arc |> Option.map Hotfixes.findEmptyPrimaryFields |> Option.defaultValue [])
                setValue ""
            ),
            [| box arc |]
        )

        match issues with
        | [] -> Html.none
        | issue :: remainingIssues ->
            let submit (event: Browser.Types.Event) =
                event.preventDefault ()

                match normalizedValue with
                | Some normalizedValue ->
                    issue.SetValue normalizedValue
                    setIssues remainingIssues
                    setValue ""

                    if remainingIssues.IsEmpty then
                        arc |> Option.iter (fun currentArc -> onRepaired (currentArc.Copy()))
                | None -> ()

            BaseModal.Modal(
                true,
                ignore,
                Html.text "Required metadata is missing",
                Html.form [
                    prop.onSubmit submit
                    prop.className "swt:flex swt:flex-col swt:gap-4"
                    prop.children [
                        Html.label [
                            prop.className "swt:fieldset swt:w-full"
                            prop.children [
                                Html.span [
                                    prop.className "swt:fieldset-legend"
                                    prop.text issue.FieldLabel
                                ]
                                Html.input [
                                    prop.ref inputRef
                                    prop.className "swt:input swt:input-bordered swt:w-full"
                                    prop.value value
                                    prop.required true
                                    prop.onChange setValue
                                    prop.ariaLabel $"{issue.ObjectType} {issue.FieldLabel}"
                                ]
                            ]
                        ]
                        Html.div [
                            prop.className "swt:flex swt:items-center swt:justify-between swt:gap-4"
                            prop.children [
                                Html.span [
                                    prop.className "swt:text-sm swt:opacity-60"
                                    prop.text $"{issues.Length} required field(s) remaining"
                                ]
                                Html.button [
                                    prop.type'.submit
                                    prop.className "swt:btn swt:btn-primary"
                                    prop.disabled normalizedValue.IsNone
                                    prop.text "Save and continue"
                                ]
                            ]
                        ]
                    ]
                ],
                description =
                    Html.text
                        $"Enter the missing {issue.FieldLabel.ToLowerInvariant()} for this {issue.ObjectType.ToLowerInvariant()}.",
                initialFocusRef = unbox inputRef,
                canClose = false,
                debug = "mandatory-metadata"
            )

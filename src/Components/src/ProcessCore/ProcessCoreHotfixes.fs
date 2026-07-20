module Swate.Components.ProcessCoreHotfixes

// ProcessCore hotfix: recover, detect, and repair missing mandatory primary fields until upstream decoding is tolerant.

open ProcessCore
open YAMLicious.YAMLiciousTypes
open Fable.Core
open Feliz
open System.Collections.Generic
open Swate.Components.Primitive.BaseModal

type PrimaryFieldIssue = {
    ObjectType: string
    FieldLabel: string
    SetValue: string -> unit
}

// ProcessCore hotfix: validate and locate empty mandatory primary fields in a loaded object graph.
let tryNormalizeRequiredValue (value: string) =
    if System.String.IsNullOrWhiteSpace value then
        None
    else
        Some(value.Trim())

let required fieldLabel value =
    match tryNormalizeRequiredValue value with
    | Some _ -> Ok()
    | None -> Error $"{fieldLabel} is required."

let private distinctReferences (items: seq<'T>) =
    let seen = HashSet<obj>(HashIdentity.Reference)
    items |> Seq.filter (box >> seen.Add) |> Seq.toArray

let rec private descendantDatasets (dataset: Dataset) = seq {
    for child in dataset.HasPart do
        yield child
        yield! descendantDatasets child
}

let findEmptyPrimaryFields (arc: ARC) =
    let issues = ResizeArray<PrimaryFieldIssue>()

    let add objectType fieldLabel currentValue setValue =
        if tryNormalizeRequiredValue currentValue |> Option.isNone then
            issues.Add {
                ObjectType = objectType
                FieldLabel = fieldLabel
                SetValue = setValue
            }

    let datasets =
        seq {
            yield arc :> Dataset
            yield! descendantDatasets arc
        }
        |> distinctReferences

    let processes = arc.AllProcesses() |> distinctReferences
    let samples = arc.AllSamples() |> distinctReferences
    let data = arc.AllData() |> distinctReferences
    let annotations = arc.AllAnnotations() |> distinctReferences
    let contexts = arc.AllDataContexts() |> distinctReferences
    let articles = arc.AllCitations() |> distinctReferences

    let recipes = processes |> Seq.choose _.ExecutesProtocol |> distinctReferences

    let agents =
        seq {
            for dataset in datasets do
                yield! dataset.Agents

            for article in articles do
                yield! article.Authors
        }
        |> distinctReferences

    let organizations = agents |> Seq.choose _.Affiliation |> distinctReferences

    let parameters =
        seq {
            for recipe in recipes do
                yield! recipe.Parameters

            for annotation in annotations do
                yield! annotation.InstanceOf |> Option.toList
        }
        |> distinctReferences

    let terms =
        seq {
            for recipe in recipes do
                yield! recipe.IntendedUse |> Option.toList

            for parameter in parameters do
                yield! parameter.DefaultValue |> Option.toList

            for context in contexts do
                yield! context.Explication |> Option.toList
                yield! context.ObjectType |> Option.toList
                yield! context.Unit |> Option.toList

            for agent in agents do
                yield! agent.JobTitles

            for article in articles do
                yield! article.CreativeWorkStatus |> Option.toList
        }
        |> distinctReferences

    let addAll items objectType fieldLabel getValue setValue =
        items
        |> Seq.iter (fun item -> add objectType fieldLabel (getValue item) (setValue item))

    addAll datasets "Dataset" "Identifier" _.Identifier (fun item value -> item.Identifier <- value)
    addAll processes "Process" "Name" _.Name (fun item value -> item.Name <- value)
    addAll samples "Sample" "Name" _.Name (fun item value -> item.Name <- value)
    addAll data "Data" "Path" _.Path (fun item value -> item.Path <- value)
    addAll annotations "Annotation" "Name" _.Name (fun item value -> item.Name <- value)
    addAll parameters "Formal parameter" "Name" _.Name (fun item value -> item.Name <- value)
    addAll terms "Defined term" "Name" _.Name (fun item value -> item.Name <- value)
    addAll agents "Agent" "Given name" _.GivenName (fun item value -> item.GivenName <- value)
    addAll organizations "Organization" "Name" _.Name (fun item value -> item.Name <- value)
    addAll articles "Scholarly article" "Headline" _.Headline (fun item value -> item.Headline <- value)

    issues |> Seq.toList

// ProcessCore hotfix: inject in-memory placeholders for mandatory fields rejected by the upstream YAML decoder.
let private keyEquals expected (content: YAMLContent) =
    System.String.Equals(content.Value, expected, System.StringComparison.OrdinalIgnoreCase)

let private tryField name fields =
    fields
    |> List.tryPick (
        function
        | YAMLElement.Mapping(key, value) when keyEquals name key -> Some value
        | _ -> None
    )

let private tryString =
    function
    | YAMLElement.Value content -> Some content.Value
    | YAMLElement.Object [ YAMLElement.Value content ] -> Some content.Value
    | _ -> None

let private mapping name value =
    YAMLElement.Mapping(YAMLContent.create name, value)

let private emptyString = YAMLElement.Value(YAMLContent.create "")

let rec private replaceOrAdd name value =
    function
    | [] -> [ mapping name value ]
    | YAMLElement.Mapping(key, _) :: fields when keyEquals name key -> mapping name value :: fields
    | field :: fields -> field :: replaceOrAdd name value fields

let private ensureString name fields =
    match tryField name fields |> Option.bind tryString with
    | Some value when not (System.String.IsNullOrWhiteSpace value) -> fields
    | None -> replaceOrAdd name emptyString fields
    | Some _ -> replaceOrAdd name emptyString fields

let private dataPlaceholder () =
    YAMLElement.Object [
        mapping "type" (YAMLElement.Value(YAMLContent.create "Data"))
        mapping "path" emptyString
    ]

// Keep this in sync with findEmptyPrimaryFields: the decoder must first accept an
// empty placeholder before the mandatory-field modal can collect a real value.
let private requiredStringFields =
    Map [
        "dataset", "identifier"
        "process", "name"
        "sample", "name"
        "data", "path"
        "annotation", "name"
        "formalparameter", "name"
        "definedterm", "name"
        "agent", "givenName"
        "organization", "name"
        "scholarlyarticle", "headline"
    ]

let rec private repairYamlElement element =
    match element with
    | YAMLElement.Mapping(key, value) -> YAMLElement.Mapping(key, repairYamlElement value)
    | YAMLElement.Sequence items -> YAMLElement.Sequence(items |> List.map repairYamlElement)
    | YAMLElement.Object fields ->
        let repairedFields = fields |> List.map repairYamlElement

        let objectType =
            repairedFields
            |> tryField "type"
            |> Option.bind tryString
            |> Option.map _.ToLowerInvariant()

        match objectType with
        | Some "datacontext" ->
            match tryField "data" repairedFields with
            | Some(YAMLElement.Object dataFields) ->
                dataFields
                |> ensureString "path"
                |> YAMLElement.Object
                |> fun data -> replaceOrAdd "data" data repairedFields
            | Some _
            | None -> replaceOrAdd "data" (dataPlaceholder ()) repairedFields
        | Some objectType ->
            requiredStringFields
            |> Map.tryFind objectType
            |> Option.map (fun field -> ensureString field repairedFields)
            |> Option.defaultValue repairedFields
        | None -> repairedFields
        |> YAMLElement.Object
    | value -> value

let decodeWithEmptyPrimaryFields arcPath yaml =
    let repairedRoot =
        match YAMLicious.Reader.read yaml |> repairYamlElement with
        | YAMLElement.Object fields -> YAMLElement.Object(ensureString "identifier" fields)
        | element -> element

    let arc =
        ProcessCore.Yaml.Dataset.decoderGeneric (fun identifier -> ARC(identifier)) None None false repairedRoot

    if not (System.String.IsNullOrWhiteSpace arcPath) then
        arc.ArcPath <- Some arcPath

    arc

// ProcessCore hotfix: retry only missing-field load failures while preserving the original error for other failures.
let loadWithEmptyPrimaryFieldRecovery arcPath loadArc tryReadYaml = promise {
    try
        return! loadArc ()
    with originalError ->
        match! tryReadYaml () with
        | None -> return raise originalError
        | Some yaml ->
            try
                return decodeWithEmptyPrimaryFields arcPath yaml
            with _ ->
                return raise originalError
}

[<Erase; Mangle(false)>]
type HotfixComponents =

    [<ReactComponent>]
    // ProcessCore hotfix: collect missing mandatory fields sequentially before allowing normal ARC editing.
    static member MandatoryFieldRepair(arc: ARC option, onRepaired: ARC -> unit) =
        let issues, setIssues = React.useState<PrimaryFieldIssue list> []
        let value, setValue = React.useState ""
        let inputRef = React.useInputRef ()
        let normalizedValue = tryNormalizeRequiredValue value

        React.useEffect (
            (fun () ->
                setIssues (arc |> Option.map findEmptyPrimaryFields |> Option.defaultValue [])
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

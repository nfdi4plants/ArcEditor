module Swate.Components.ProcessCore.Hotfixes

// ProcessCore hotfix: recover, detect, and repair missing mandatory primary fields until upstream decoding is tolerant.

open ProcessCore
open YAMLicious.YAMLiciousTypes
open Swate.Components.ProcessCore.ObjectGraph

type PrimaryFieldIssue = {
    ObjectType: string
    FieldLabel: string
    SetValue: string -> unit
}

// ProcessCore hotfix: validate and locate empty mandatory primary fields in a loaded object graph.
/// Trims a required string and rejects null, empty, or whitespace-only values.
let tryNormalizeRequiredValue (value: string) =
    if System.String.IsNullOrWhiteSpace value then
        None
    else
        Some(value.Trim())

/// Validates a required ProcessCore string field for use in metadata forms.
let required fieldLabel value =
    match tryNormalizeRequiredValue value with
    | Some _ -> Ok()
    | None -> Error $"{fieldLabel} is required."

/// Finds mutable setters for every missing mandatory primary field in the ARC graph.
let findEmptyPrimaryFields (arc: ARC) =
    let issues = ResizeArray<PrimaryFieldIssue>()

    let add objectType fieldLabel currentValue setValue =
        if tryNormalizeRequiredValue currentValue |> Option.isNone then
            issues.Add {
                ObjectType = objectType
                FieldLabel = fieldLabel
                SetValue = setValue
            }

    let datasets = datasetsIncludingRoot arc

    let processes = arc.AllProcesses()
    let samples = arc.AllSamples()
    let data = arc.AllData()
    let recipes = recipes arc
    let parameters = formalParameters arc
    let terms = definedTerms arc
    let annotations = arc.AllAnnotations()
    let agents = arc.AllAgents()
    let organizations = arc.AllOrganizations()
    let articles = arc.AllCitations()

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
/// Compares a YAML scalar key without case sensitivity.
let private keyEquals expected (content: YAMLContent) =
    System.String.Equals(content.Value, expected, System.StringComparison.OrdinalIgnoreCase)

/// Finds a named value in a YAML object field list.
let private tryField name fields =
    fields
    |> List.tryPick (
        function
        | YAMLElement.Mapping(key, value) when keyEquals name key -> Some value
        | _ -> None
    )

/// Extracts a string from the scalar YAML shapes accepted by ProcessCore.
let private tryString =
    function
    | YAMLElement.Value content -> Some content.Value
    | YAMLElement.Object [ YAMLElement.Value content ] -> Some content.Value
    | _ -> None

/// Creates a YAML mapping with a scalar key.
let private mapping name value =
    YAMLElement.Mapping(YAMLContent.create name, value)

/// Shared empty YAML string used for temporary mandatory-field placeholders.
let private emptyString = YAMLElement.Value(YAMLContent.create "")

/// Replaces the first matching YAML field or appends it when absent.
let rec private replaceOrAdd name value =
    function
    | [] -> [ mapping name value ]
    | YAMLElement.Mapping(key, _) :: fields when keyEquals name key -> mapping name value :: fields
    | field :: fields -> field :: replaceOrAdd name value fields

/// Ensures that a YAML object contains a string field, inserting an empty placeholder if needed.
let private ensureString name fields =
    match tryField name fields |> Option.bind tryString with
    | Some value when not (System.String.IsNullOrWhiteSpace value) -> fields
    | None -> replaceOrAdd name emptyString fields
    | Some _ -> replaceOrAdd name emptyString fields

/// Creates the minimum Data object accepted for a missing DataContext data relationship.
let private dataPlaceholder () =
    YAMLElement.Object [
        mapping "type" (YAMLElement.Value(YAMLContent.create "Data"))
        mapping "path" emptyString
    ]

// Keep this in sync with findEmptyPrimaryFields: the decoder must first accept an
// empty placeholder before the mandatory-field modal can collect a real value.
/// Maps ProcessCore object types to the mandatory string field required by their decoder.
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

/// Recursively inserts mandatory-field placeholders into a parsed YAML graph.
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

/// Decodes YAML after inserting empty placeholders for mandatory fields rejected upstream.
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
/// Loads an ARC normally and retries through tolerant YAML decoding only when loading fails.
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

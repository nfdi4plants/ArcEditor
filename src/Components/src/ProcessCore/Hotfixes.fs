module Swate.Components.ProcessCore.Hotfixes

// ProcessCore hotfix: recover, detect, and repair missing mandatory primary fields until upstream decoding is tolerant.

open ProcessCore
open YAMLicious.YAMLiciousTypes
open Swate.Components.ProcessCore.GetAll

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

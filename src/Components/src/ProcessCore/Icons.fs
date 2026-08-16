module Swate.Components.ProcessCore.Icons

let datasetIcon = "swt:iconify-color swt:fluent-color--database-20"

let processIcon =
    "swt:iconify-color swt:fluent-color--arrow-clockwise-dashes-settings-20"

let sampleIcon = "swt:iconify-color swt:fluent-color--molecule-20"
let dataIcon = "swt:iconify-color swt:fluent-color--document-20"
let recipeIcon = "swt:iconify-color swt:fluent-color--clipboard-text-edit-20"
let annotationIcon = "swt:iconify-color swt:fluent-color--comment-multiple-20"
let dataContextIcon = "swt:iconify-color swt:fluent-color--content-view-20"
let agentIcon = "swt:iconify-color swt:fluent-color--agents-20"
let organizationIcon = "swt:iconify-color swt:fluent-color--org-20"
let scholarlyArticleIcon = "swt:iconify-color swt:fluent-color--document-text-20"
let formalParameterIcon = "swt:iconify swt:fluent--options-20-regular"
let definedTermIcon = "swt:iconify swt:fluent--tag-20-regular"
let jobTitleIcon = "swt:iconify swt:fluent--briefcase-20-regular"
let inputIcon = "swt:iconify-color swt:fluent-color--arrow-square-down-20"
let outputIcon = "swt:iconify-color swt:fluent-color--send-20"

/// Maps the stable case name of an Object Browser member kind without coupling
/// this ProcessCore presentation module to the page-specific union type.
let forMemberKindName =
    function
    | "Dataset" -> datasetIcon
    | "Process" -> processIcon
    | "Sample" -> sampleIcon
    | "Data" -> dataIcon
    | "Recipe" -> recipeIcon
    | "Annotation" -> annotationIcon
    | "DataContext" -> dataContextIcon
    | "Agent" -> agentIcon
    | "Organization" -> organizationIcon
    | "ScholarlyArticle" -> scholarlyArticleIcon
    | kind -> invalidArg (nameof kind) $"Unsupported ProcessCore member kind: {kind}"

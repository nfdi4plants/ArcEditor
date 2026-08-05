module ProcessCoreConverterTests

open Expecto
open ProcessCore
open Swate.Components.ProcessCore.Copy
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreAdapterTypes
open ProcessCoreProvenanceFixtures
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreConverter

module CanonicalValues = Swate.Components.Page.ProvenanceGrouping.Values

let private canonicalLocation processGroupName : ProcessCoreProcessGroupLocation = {
    DatasetPath = [ "arc-neutral"; "dataset-neutral" ]
    ProcessGroupName = processGroupName
}

let private canonicalNodeAssignments result =
    result.Session.Nodes
    |> Map.toList
    |> List.collect (fun (_, node: Swate.Components.Page.ProvenanceGrouping.Domain.CanonicalNode) ->
        node.Assignments |> Map.toList |> List.map snd
    )

let private canonicalProcessAssignments result =
    result.Session.Processes
    |> Map.toList
    |> List.collect (fun (_, structuralProcess: Swate.Components.Page.ProvenanceGrouping.Domain.StructuralProcess) ->
        structuralProcess.Assignments |> Map.toList |> List.map snd
    )

let private categoryNameFor valueId result =
    let valueDefinition = result.Session.Values[valueId]
    result.Session.Properties[valueDefinition.PropertyId].Category.Name

let tests =
    testList "ProcessCore converter" [
        testCase "converts sample and data endpoints"
        <| fun _ ->
            let fixture = basic ()
            let result = fromArcMany [ loadedTable ] fixture.Arc |> expectOk
            let layer = result.Session.Layers[result.Session.ActiveLayerId]

            let namesOf endpoints =
                endpoints
                |> Map.toList
                |> List.map (fun (_, endpoint: Swate.Components.Page.ProvenanceGrouping.Domain.LayerEndpoint) ->
                    result.Session.Nodes[endpoint.Key.NodeId].Name
                )
                |> List.sort

            Expect.sequenceEqual (namesOf layer.InputEndpoints) [ "input-neutral" ] "Input sample must be projected."
            Expect.sequenceEqual (namesOf layer.OutputEndpoints) [ "output-neutral" ] "Output sample must be projected."
            Expect.equal result.Index.NodeLocations.Count 2 "Both source endpoint locations must be indexed."

        testCase "a node annotation is owned by exactly the node that carried it"
        <| fun _ ->
            let arc, _, _ = annotated ()
            let result = fromArcMany [ loadedTable ] arc |> expectOk

            let ownerNames categoryName =
                result.Session.Nodes
                |> Map.toList
                |> List.filter (fun (_, node) ->
                    node.Assignments
                    |> Map.exists (fun _ assignment -> categoryNameFor assignment.ValueId result = categoryName)
                )
                |> List.map (fun (_, node) -> node.Name)
                |> List.sort

            // The old model attached a value to a *side*; ownership is now the
            // node itself, so "does not spread to the other side" is the stronger
            // claim that exactly one node owns it.
            Expect.sequenceEqual
                (ownerNames "node-parameter-neutral")
                [ "input-neutral" ]
                "A node annotation on the input must be owned by that node alone."

            Expect.sequenceEqual
                (ownerNames "node-component-neutral")
                [ "output-neutral" ]
                "A node annotation on the output must be owned by that node alone."

        testCase "preserves annotation term and unit accessions"
        <| fun _ ->
            let arc, _, _ = annotated ()
            let result = fromArcMany [ loadedTable ] arc |> expectOk

            let assignment =
                canonicalNodeAssignments result
                |> List.find (fun assignment -> categoryNameFor assignment.ValueId result = "category-neutral")

            let definition = result.Session.Values[assignment.ValueId]
            let category = result.Session.Properties[definition.PropertyId].Category

            Expect.equal category.TermAccession (Some "term:category") "Category accession must round-trip."
            Expect.isNone category.TermSource "ProcessCore does not supply a category term source."
            Expect.equal definition.Unit.Value.TermAccession (Some "term:unit") "Unit accession must round-trip."
            Expect.isNone definition.Unit.Value.TermSource "ProcessCore does not supply a unit term source."

            match definition.Value with
            | CanonicalValues.ProvenanceValue.Term term ->
                Expect.equal term.TermAccession (Some "term:value") "Value accession must round-trip."
                Expect.isNone term.TermSource "ProcessCore does not supply a value term source."
            | other -> failtestf "Expected term value but received %A" other

        testCase "collapses exact duplicate values but indexes every annotation occurrence"
        <| fun _ ->
            let arc, _, _ = annotated ()
            let result = fromArcMany [ loadedTable ] arc |> expectOk

            let assignments =
                canonicalProcessAssignments result
                |> List.filter (fun assignment -> categoryNameFor assignment.ValueId result = "parameter-neutral")

            // Two equal ParameterValue annotations on one Process stay two
            // distinguishable assignments - collapsing them would silently drop
            // an occurrence on write - while their equal content normalizes onto
            // a single value definition.
            Expect.equal
                (assignments |> List.map _.ValueId |> Set.ofList |> Set.count)
                1
                "Equal category/value/unit content must normalize to one value definition."

            Expect.equal
                (assignments
                 |> List.sumBy (fun assignment -> result.Index.AssignmentLocations[assignment.Id].Length))
                2
                "Both duplicate source occurrences must be retained."

        testCase "an endpointless process keeps its annotations on its endpointless link"
        <| fun _ ->
            // The old converter warned and dropped these as unanchored. A
            // process with no endpoints now imports an Endpointless link, so its
            // annotations are ordinary process assignments with real coverage.
            let orphanParameter =
                Annotation("orphan-parameter", value = "parameter-value", additionalType = "ParameterValue")

            let orphanComponent =
                Annotation("orphan-component", value = "component-value", additionalType = "Component")

            let recipe = Recipe(name = "orphan-recipe", components = [ orphanComponent ])
            recipe.SetProperty("@id", "recipe:orphan")
            let proc = mkProcessFull "stage-neutral" (Some recipe) [] [] [ orphanParameter ]
            let dataset = Dataset("dataset-neutral", processes = [ proc ])
            let arc = ARC("arc-neutral", hasPart = [ dataset ])
            arc.AddRecipe recipe
            let result = fromArcMany [ loadedTable ] arc |> expectOk

            let endpointlessLinkIds =
                result.Session.Processes
                |> Map.toList
                |> List.collect (fun (_, structuralProcess) -> structuralProcess.Links |> Map.toList)
                |> List.choose (fun (linkId, link) ->
                    match link.Shape with
                    | CanonicalValues.ProcessLinkShape.Endpointless -> Some linkId
                    | _ -> None
                )
                |> Set.ofList

            Expect.hasLength endpointlessLinkIds 1 "A process with no endpoints imports one endpointless link."

            let assignmentsFor categoryName =
                canonicalProcessAssignments result
                |> List.filter (fun assignment -> categoryNameFor assignment.ValueId result = categoryName)

            for categoryName in [ "orphan-parameter"; "orphan-component" ] do
                let assignment = assignmentsFor categoryName |> List.exactlyOne

                Expect.equal
                    assignment.CoveredLinkIds
                    endpointlessLinkIds
                    $"'{categoryName}' must cover exactly the endpointless link instead of being dropped."

            Expect.isEmpty
                (result.Warnings
                 |> List.filter (
                     function
                     | ProcessCoreConversionWarning.PropertyWithoutEndpoint _ -> true
                     | _ -> false
                 ))
                "Nothing is unanchored any more, so no endpoint warning may be produced."

        testList "fromArcMany" [
            testCase "case whitespace endpoint kind and data selector differences stay distinct"
            <| fun _ ->
                let nodes: IONode list = [
                    SampleNode(Sample("same"))
                    SampleNode(Sample("Same"))
                    SampleNode(Sample(" same"))
                    DataNode(Data("same"))
                    DataNode(Data("same", selector = "row=1"))
                    DataNode(Data("same", selector = "row=2"))
                ]

                let processes =
                    nodes
                    |> List.mapi (fun index node ->
                        mkProcess "stage-neutral" [ node ] [ SampleNode(Sample($"out-{index}")) ]
                    )

                let dataset = Dataset("dataset-neutral", processes = processes)
                let arc = ARC("arc-neutral", hasPart = [ dataset ])
                let result = fromArcMany [ canonicalLocation "stage-neutral" ] arc |> expectOk

                let inputKeys =
                    result.Session.Layers[result.Session.ActiveLayerId].InputEndpoints
                    |> Map.toList
                    |> List.map (fun (_, endpoint) -> result.Session.Nodes[endpoint.Key.NodeId].Key)
                    |> Set.ofList

                Expect.equal
                    inputKeys.Count
                    6
                    "Exact case, whitespace, kind and selector identities must stay distinct."

                Expect.contains
                    inputKeys
                    {
                        KindId = ProcessCoreKinds.sampleEndpoint.Id
                        Name = "same"
                    }
                    "The Sample identity must remain distinct from the equal-name Data identity."

                Expect.contains
                    inputKeys
                    {
                        KindId = ProcessCoreKinds.dataEndpoint.Id
                        Name = "same"
                    }
                    "The Data identity must remain distinct from the equal-name Sample identity."

                Expect.isTrue
                    (inputKeys |> Set.exists (fun key -> key.Name = "same#row=1"))
                    "The complete first Data selector must be retained."

                Expect.isTrue
                    (inputKeys |> Set.exists (fun key -> key.Name = "same#row=2"))
                    "The complete second Data selector must be retained."

            testCase "each ProcessCore process imports exactly one link of the correct shape"
            <| fun _ ->
                let between =
                    mkProcess "stage-neutral" [ SampleNode(Sample("between-in")) ] [ SampleNode(Sample("between-out")) ]

                let inputOnlyProcess =
                    mkProcess "stage-neutral" [ SampleNode(Sample("input-only")) ] []

                let outputOnlyProcess =
                    mkProcess "stage-neutral" [] [ SampleNode(Sample("output-only")) ]

                let endpointless = mkProcess "stage-neutral" [] []

                let dataset =
                    Dataset(
                        "dataset-neutral",
                        processes = [
                            between
                            inputOnlyProcess
                            outputOnlyProcess
                            endpointless
                        ]
                    )

                let arc = ARC("arc-neutral", hasPart = [ dataset ])
                let result = fromArcMany [ canonicalLocation "stage-neutral" ] arc |> expectOk

                let shapes =
                    result.Session.Processes
                    |> Map.toList
                    |> List.collect (fun (_, structuralProcess) ->
                        structuralProcess.Links |> Map.toList |> List.map (fun (_, link) -> link.Shape)
                    )

                Expect.equal shapes.Length 4 "Every source Process must produce exactly one canonical link."

                Expect.equal
                    (shapes
                     |> List.countBy (fun shape ->
                         match shape with
                         | Swate.Components.Page.ProvenanceGrouping.Values.ProcessLinkShape.Between _ -> "between"
                         | Swate.Components.Page.ProvenanceGrouping.Values.ProcessLinkShape.InputOnly _ -> "input"
                         | Swate.Components.Page.ProvenanceGrouping.Values.ProcessLinkShape.OutputOnly _ -> "output"
                         | Swate.Components.Page.ProvenanceGrouping.Values.ProcessLinkShape.Endpointless -> "none"
                     )
                     |> Map.ofList)
                    (Map [ "between", 1; "input", 1; "output", 1; "none", 1 ])
                    "All four singular link shapes must be represented exactly once."

            testCase "no Cartesian link is inferred from independent endpoint collections"
            <| fun _ ->
                let processes = [
                    mkProcess "stage-neutral" [ SampleNode(Sample("in-a")) ] [ SampleNode(Sample("out-a")) ]
                    mkProcess "stage-neutral" [ SampleNode(Sample("in-b")) ] [ SampleNode(Sample("out-b")) ]
                ]

                let dataset = Dataset("dataset-neutral", processes = processes)
                let arc = ARC("arc-neutral", hasPart = [ dataset ])
                let result = fromArcMany [ canonicalLocation "stage-neutral" ] arc |> expectOk

                Expect.equal result.Session.Processes.Count 2 "Independent source rows must remain distinct processes."
                Expect.equal result.Index.LinkLocations.Count 2 "Only the two explicit source links may be indexed."

            testCase "annotations recipes catalog and concrete kinds load canonically"
            <| fun _ ->
                let input = Sample("input")

                input.AddAdditionalProperty(
                    Annotation("characteristic", value = "same", additionalType = "CharacteristicValue")
                )

                let output = Sample("output")
                output.AddAdditionalProperty(Annotation("factor", value = "same", additionalType = "FactorValue"))

                let parameter =
                    Annotation("parameter", value = "same", additionalType = "ParameterValue")

                let recipeComponent =
                    Annotation("component", value = "same", additionalType = "Component")

                let recipe = Recipe(name = "assigned", components = [ recipeComponent ])
                recipe.SetProperty("@id", "recipe:assigned")
                let unreferenced = Recipe(name = "unreferenced")
                unreferenced.SetProperty("@id", "recipe:unreferenced")

                let sourceProcess =
                    mkProcessFull "stage-neutral" (Some recipe) [ SampleNode input ] [ SampleNode output ] [ parameter ]

                let dataset = Dataset("dataset-neutral", processes = [ sourceProcess ])
                let arc = ARC("arc-neutral", hasPart = [ dataset ])
                arc.AddRecipe recipe
                arc.AddRecipe unreferenced
                let result = fromArcMany [ canonicalLocation "stage-neutral" ] arc |> expectOk

                let nodeKinds =
                    canonicalNodeAssignments result |> List.map _.PropertyKind |> Set.ofList

                Expect.contains
                    nodeKinds
                    (Swate.Components.Page.ProvenanceGrouping.Values.AssignmentPropertyKind.AdapterSpecific
                        ProcessCoreKinds.characteristic)
                    "Characteristic occurrences must retain their concrete adapter kind."

                Expect.contains
                    nodeKinds
                    (Swate.Components.Page.ProvenanceGrouping.Values.AssignmentPropertyKind.AdapterSpecific
                        ProcessCoreKinds.factor)
                    "Factor occurrences must retain their concrete adapter kind."

                let processAssignments = canonicalProcessAssignments result

                let byCategory name =
                    processAssignments
                    |> List.filter (fun assignment -> categoryNameFor assignment.ValueId result = name)

                Expect.hasLength (byCategory "parameter") 1 "The Parameter must become a process assignment."
                Expect.hasLength (byCategory "component") 1 "The Component must become a process assignment."
                Expect.hasLength (byCategory "Recipe") 1 "ExecutesRecipe must become a process assignment."

                let recipeAssignment = byCategory "Recipe" |> List.exactlyOne
                let recipeDefinition = result.Session.Values[recipeAssignment.ValueId]

                match recipeDefinition.Value with
                | Swate.Components.Page.ProvenanceGrouping.Values.ProvenanceValue.Reference reference ->
                    Expect.equal reference.Scheme "processcore:recipe" "The Recipe scheme must be canonical."

                    Expect.equal
                        reference.Id
                        (RecipeResourceKey.ofRecipeStableString recipe)
                        "The Recipe reference must use the exact stored-resource identity."
                | other -> failtestf "Expected Recipe reference value but received %A" other

                Expect.equal
                    recipeAssignment.ReferenceSlotId
                    (Some "processcore:executes-recipe")
                    "The Recipe assignment alone occupies the executes-recipe slot."

                Expect.isNone
                    recipeAssignment.ContainerReferenceValueId
                    "The Recipe assignment must not point at itself as a container."

                let componentAssignment = byCategory "component" |> List.exactlyOne
                Expect.equal componentAssignment.ContainerReferenceValueId (Some recipeAssignment.ValueId) "Component."
                Expect.isNone componentAssignment.ReferenceSlotId "Components must not occupy the Recipe slot."

                match componentAssignment.Lineage with
                | Swate.Components.Page.ProvenanceGrouping.Values.AssignmentLineage.DerivedFromCatalog("processcore:recipe",
                                                                                                       referenceId,
                                                                                                       _) ->
                    Expect.equal
                        referenceId
                        (RecipeResourceKey.ofRecipeStableString recipe)
                        "Component lineage must retain the owning Recipe identity."
                | other -> failtestf "Expected catalog-derived Component lineage but received %A" other

                Expect.equal
                    result.ReferenceCatalog.Count
                    2
                    "Assigned and unreferenced stored Recipes must be cataloged."

                let unreferencedId = RecipeResourceKey.ofRecipeStableString unreferenced

                Expect.isTrue
                    (result.ReferenceCatalog.ContainsKey("processcore:recipe", unreferencedId))
                    "The unreferenced Recipe must appear in the catalog."

                Expect.isFalse
                    (result.Session.Values
                     |> Map.exists (fun _ definition ->
                         match definition.Value with
                         | Swate.Components.Page.ProvenanceGrouping.Values.ProvenanceValue.Reference reference ->
                             reference.Id = unreferencedId
                         | _ -> false
                     ))
                    "An unreferenced catalog entry must not create a session value definition."

            testCase "each Component occurrence is process owned and points at its Recipe value definition"
            <| fun _ ->
                let recipeComponent =
                    Annotation("component", value = "shared", additionalType = "Component")

                let recipe = Recipe(name = "shared-recipe", components = [ recipeComponent ])
                recipe.SetProperty("@id", "recipe:shared")

                let processes = [
                    mkProcessFull "stage-neutral" (Some recipe) [ SampleNode(Sample("in-a")) ] [
                        SampleNode(Sample("out-a"))
                    ] []
                    mkProcessFull "stage-neutral" (Some recipe) [ SampleNode(Sample("in-b")) ] [
                        SampleNode(Sample("out-b"))
                    ] []
                ]

                let dataset = Dataset("dataset-neutral", processes = processes)
                let arc = ARC("arc-neutral", hasPart = [ dataset ])
                arc.AddRecipe recipe
                let result = fromArcMany [ canonicalLocation "stage-neutral" ] arc |> expectOk

                let components =
                    result.Session.Processes
                    |> Map.toList
                    |> List.map (fun (_, structuralProcess) ->
                        let assignments = structuralProcess.Assignments |> Map.toList |> List.map snd

                        let recipeAssignment =
                            assignments
                            |> List.find (fun assignment -> categoryNameFor assignment.ValueId result = "Recipe")

                        let componentAssignment =
                            assignments
                            |> List.find (fun assignment -> categoryNameFor assignment.ValueId result = "component")

                        structuralProcess.Id, recipeAssignment, componentAssignment
                    )

                Expect.equal components.Length 2 "Each referencing Process must own a Component occurrence."

                Expect.equal
                    (components
                     |> List.map (fun (_, _, componentAssignment) -> componentAssignment.ValueId)
                     |> Set.ofList
                     |> Set.count)
                    1
                    "Equal physical Component values may share one normalized value definition."

                components
                |> List.iter (fun (_, recipeAssignment, componentAssignment) ->
                    Expect.equal
                        componentAssignment.ContainerReferenceValueId
                        (Some recipeAssignment.ValueId)
                        "Each Component must point at its process's Recipe reference value."

                    Expect.equal
                        componentAssignment.CoveredLinkIds.Count
                        1
                        "Each occurrence covers its exact source link."
                )

            testCase "loaded Recipe Components are read-only dependents"
            <| fun _ ->
                let recipeComponent =
                    Annotation("component", value = "original", additionalType = "Component")

                let recipe = Recipe(name = "read-only-recipe", components = [ recipeComponent ])
                recipe.SetProperty("@id", "recipe:read-only")

                let sourceProcess =
                    mkProcessFull "stage-neutral" (Some recipe) [ SampleNode(Sample("input")) ] [
                        SampleNode(Sample("output"))
                    ] []

                let dataset = Dataset("dataset-neutral", processes = [ sourceProcess ])
                let arc = ARC("arc-neutral", hasPart = [ dataset ])
                arc.AddRecipe recipe
                let result = fromArcMany [ canonicalLocation "stage-neutral" ] arc |> expectOk

                let processId, structuralProcess =
                    result.Session.Processes |> Map.toList |> List.exactlyOne

                let componentAssignment =
                    structuralProcess.Assignments
                    |> Map.toList
                    |> List.map snd
                    |> List.find (fun assignment -> categoryNameFor assignment.ValueId result = "component")

                let componentValue = result.Session.Values[componentAssignment.ValueId]
                let componentProperty = result.Session.Properties[componentValue.PropertyId]

                let changedContent: Swate.Components.Page.ProvenanceGrouping.Commands.NodeValueContent = {
                    Category = componentProperty.Category
                    Value = Swate.Components.Page.ProvenanceGrouping.Values.ProvenanceValue.Text "attempted mutation"
                    Unit = componentValue.Unit
                }

                let copiedDraft: Swate.Components.Page.ProvenanceGrouping.Commands.ProcessAssignmentDraft = {
                    Content = changedContent
                    OwnerKind = Swate.Components.Page.ProvenanceGrouping.Values.AnnotationOwnerKind.Process
                    PropertyKind = componentAssignment.PropertyKind
                    ContainerReferenceValueId = componentAssignment.ContainerReferenceValueId
                    ReferenceSlotId = None
                    Lineage =
                        Swate.Components.Page.ProvenanceGrouping.Values.AssignmentLineage.DerivedFrom
                            componentAssignment.Id
                }

                let rejectedAsUnit result = result |> Result.map (fun _ -> ())

                let rejected = [
                    Swate.Components.Page.ProvenanceGrouping.Commands.assignProcessValue
                        componentAssignment.CoveredLinkIds
                        copiedDraft
                        result.Session
                    |> rejectedAsUnit
                    Swate.Components.Page.ProvenanceGrouping.Commands.editProcessAssignment
                        processId
                        componentAssignment.Id
                        changedContent
                        result.Session
                    |> rejectedAsUnit
                    Swate.Components.Page.ProvenanceGrouping.Commands.editProcessAssignmentSubset
                        processId
                        componentAssignment.Id
                        componentAssignment.CoveredLinkIds
                        changedContent
                        result.Session
                    |> rejectedAsUnit
                    Swate.Components.Page.ProvenanceGrouping.Commands.removeProcessAssignmentLinks
                        processId
                        componentAssignment.Id
                        componentAssignment.CoveredLinkIds
                        result.Session
                    |> rejectedAsUnit
                    Swate.Components.Page.ProvenanceGrouping.Session.editValueGlobally
                        componentAssignment.ValueId
                        changedContent
                        result.Session
                    |> rejectedAsUnit
                    Swate.Components.Page.ProvenanceGrouping.Session.removeValuesGlobally
                        (Set.singleton componentAssignment.ValueId)
                        result.Session
                    |> rejectedAsUnit
                    Swate.Components.Page.ProvenanceGrouping.Session.removePropertyGlobally
                        componentProperty.Id
                        result.Session
                    |> rejectedAsUnit
                ]

                rejected
                |> List.iter (fun rejection ->
                    Expect.equal
                        rejection
                        (Error
                            Swate.Components.Page.ProvenanceGrouping.MutationTypes.ProvenanceCommandError.ReadOnlyAdapterResourceMutation)
                        "Loaded Components must reject direct add/copy, edit/overwrite, and removal commands."
                )

            testCase "previous process context is not copied onto later inputs"
            <| fun _ ->
                let arc, _ = withPreviousContext ()
                let result = fromArcMany [ canonicalLocation "stage-neutral" ] arc |> expectOk

                let categories =
                    result.Session.Values
                    |> Map.toList
                    |> List.map (fun (_, value) -> result.Session.Properties[value.PropertyId].Category.Name)

                Expect.isFalse
                    (categories |> List.contains "previous-parameter")
                    "Upstream context remains owned upstream and is not copied into the selected layer."

            testCase "five distinct nodes with equal annotations produce five assignments over one value definition"
            <| fun _ ->
                let processes = [
                    for index in 1..5 do
                        let input = Sample($"input-{index}")

                        input.AddAdditionalProperty(
                            Annotation(
                                "shared-category",
                                value = "shared-value",
                                additionalType = "CharacteristicValue"
                            )
                        )

                        yield mkProcess "stage-neutral" [ SampleNode input ] [ SampleNode(Sample($"output-{index}")) ]
                ]

                let dataset = Dataset("dataset-neutral", processes = processes)
                let arc = ARC("arc-neutral", hasPart = [ dataset ])
                let result = fromArcMany [ canonicalLocation "stage-neutral" ] arc |> expectOk

                let assignments =
                    canonicalNodeAssignments result
                    |> List.filter (fun assignment -> categoryNameFor assignment.ValueId result = "shared-category")

                Expect.equal assignments.Length 5 "Every distinct canonical node must own an assignment occurrence."

                Expect.equal
                    (assignments |> List.map _.ValueId |> Set.ofList |> Set.count)
                    1
                    "Equal category/value/unit content must normalize to one value definition."

            testCase "generic node and process properties reload as generic kinds"
            <| fun _ ->
                let input = Sample("input")

                input.AddAdditionalProperty(
                    Annotation(
                        "generic-node",
                        value = "node-value",
                        additionalType = ProcessCoreGenericPropertyMappings.defaults.Node.AdditionalType
                    )
                )

                let genericProcess =
                    Annotation(
                        "generic-process",
                        value = "process-value",
                        additionalType = ProcessCoreGenericPropertyMappings.defaults.Process.AdditionalType
                    )

                let sourceProcess =
                    mkProcessFull "stage-neutral" None [ SampleNode input ] [ SampleNode(Sample("output")) ] [
                        genericProcess
                    ]

                let dataset = Dataset("dataset-neutral", processes = [ sourceProcess ])
                let arc = ARC("arc-neutral", hasPart = [ dataset ])
                let result = fromArcMany [ canonicalLocation "stage-neutral" ] arc |> expectOk

                let nodeAssignment =
                    canonicalNodeAssignments result
                    |> List.find (fun assignment -> categoryNameFor assignment.ValueId result = "generic-node")

                let processAssignment =
                    canonicalProcessAssignments result
                    |> List.find (fun assignment -> categoryNameFor assignment.ValueId result = "generic-process")

                Expect.equal
                    nodeAssignment.PropertyKind
                    Swate.Components.Page.ProvenanceGrouping.Values.AssignmentPropertyKind.Generic
                    "Configured generic node storage must reload as Generic."

                Expect.equal
                    processAssignment.PropertyKind
                    Swate.Components.Page.ProvenanceGrouping.Values.AssignmentPropertyKind.Generic
                    "Configured generic process storage must reload as Generic."
        ]
    ]

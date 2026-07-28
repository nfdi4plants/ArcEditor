module ProcessCoreMultiSourceTests

open Expecto
open ProcessCore
open ProcessCoreProvenanceFixtures
open Swate.Components.Page.ProvenanceGrouping.ProvenanceTypes
open Swate.Components.Page.ProvenanceGrouping.Session
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreAdapterTypes
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreConverter
open Swate.Electron.Shared.ProvenanceGrouping.ProcessCoreWriteback

/// Hand-built minimal models for the pure `Session.initMany` link rules -
/// no converter involved, so kinds and names are fully controlled.
module private ModelBuilder =

    let sampleKind = { Id = "sample"; Label = "Sample" }
    let dataKind = { Id = "data"; Label = "Data" }

    let private ioHeader (kind: ProvenanceKind) side = {
        Kind = kind
        Text =
            match side with
            | ProvenanceSide.Input -> $"Input [{kind.Label}]"
            | ProvenanceSide.Output -> $"Output [{kind.Label}]"
    }

    let private set (source: ProvenanceSourceRef) side (kind: ProvenanceKind) name : ProvenanceSet = {
        Id = $"{source.Id}::set:%A{side}:{kind.Id}:{name}"
        Source = source
        Header = ioHeader kind side
        Name = name
        PropertyValueIds = []
        InheritedPropertyValueIds = Map.empty
    }

    let model id (inputs: (ProvenanceKind * string) list) (outputs: (ProvenanceKind * string) list) : ProvenanceModel =
        let source = { Id = id; Name = id }

        let toMap side entries =
            entries
            |> List.map (fun (kind, name) ->
                let entry = set source side kind name
                entry.Id, entry
            )
            |> Map.ofList

        {
            Source = source
            PropertyValues = Map.empty
            InputSets = toMap ProvenanceSide.Input inputs
            OutputSets = toMap ProvenanceSide.Output outputs
            Connections = Map.empty
        }

/// Two chained process groups in one dataset: stage-one turns chain-input
/// into chain-mid, stage-two turns chain-mid into chain-output. Each stage
/// carries one process-level parameter for value-edit routing checks.
let private chainedTables () =
    let input = Sample("chain-input")
    let mid = Sample("chain-mid")
    let output = Sample("chain-output")

    let parameterOne =
        Annotation("param-one", value = "1", additionalType = "ParameterValue")

    let parameterTwo =
        Annotation("param-two", value = "2", additionalType = "ParameterValue")

    let stageOne =
        mkProcessFull "stage-one" None [ SampleNode input ] [ SampleNode mid ] [ parameterOne ]

    let stageTwo =
        mkProcessFull "stage-two" None [ SampleNode mid ] [ SampleNode output ] [ parameterTwo ]

    let dataset = Dataset("dataset-neutral", processes = [ stageOne; stageTwo ])
    let arc = ARC("arc-neutral", hasPart = [ dataset ])
    arc, dataset, parameterOne, parameterTwo

let private tableOne: ProcessCoreTableLocation = {
    DatasetPath = [ "arc-neutral"; "dataset-neutral" ]
    TableName = "stage-one"
}

let private tableTwo: ProcessCoreTableLocation = {
    DatasetPath = [ "arc-neutral"; "dataset-neutral" ]
    TableName = "stage-two"
}

let private loadChained arc =
    let one = fromArc tableOne arc |> expectOk
    let two = fromArc tableTwo arc |> expectOk
    let session = Session.initMany [ one.Model; two.Model ]

    let indices =
        Map [
            one.Model.Source.Id, one.Index
            two.Model.Source.Id, two.Index
        ]

    one, two, session, indices

let private propertyIdByName name (model: ProvenanceModel) =
    model.PropertyValues
    |> Map.toList
    |> List.find (fun (_, value) -> value.Header.Category.Name = name)
    |> fst

let private update propertyId value unit session =
    Session.updatePropertyValue propertyId value unit session |> expectOk |> fst

let private selectLayer layerId session =
    Session.selectLayer layerId session |> expectOk |> fst

let private createSet side header name session =
    Session.createLoadedSet
        {
            Side = side
            Header = header
            Name = name
        }
        session
    |> expectOk
    |> fst

let private connect inputId outputId session =
    Session.connectSets inputId outputId None session |> expectOk |> fst

let private addLayer name selectedSets session =
    Session.addLayer
        {
            Name = name
            SelectedSets = selectedSets
        }
        session
    |> expectOk
    |> fst

let private processCountByName (dataset: Dataset) name =
    dataset.Processes |> Seq.filter (fun proc -> proc.Name = name) |> Seq.length

let tests =
    testList "ProcessCore multi-source sessions" [
        testList "Session.initMany" [
            testCase "creates one layer per model in order with the first layer active"
            <| fun _ ->
                let session =
                    Session.initMany [
                        ModelBuilder.model "table-a" [] [ ModelBuilder.sampleKind, "shared" ]
                        ModelBuilder.model "table-b" [ ModelBuilder.sampleKind, "shared" ] []
                    ]

                Expect.equal
                    (session.Layers |> List.map (fun layer -> layer.Id))
                    [ "layer-1"; "layer-2" ]
                    "One layer per model, in the given order."

                Expect.equal session.LayerOrder [ "layer-1"; "layer-2" ] "Layer order must follow the model order."
                Expect.equal session.ActiveLayerId "layer-1" "The first layer must be active."
                Expect.isEmpty session.PatchLog "A freshly loaded session carries no patches."

            testCase "links same-named same-kinded sets of consecutive layers"
            <| fun _ ->
                let session =
                    Session.initMany [
                        ModelBuilder.model "table-a" [] [ ModelBuilder.sampleKind, "shared" ]
                        ModelBuilder.model "table-b" [ ModelBuilder.sampleKind, "shared" ] []
                    ]

                let link =
                    Expect.wantSome (session.ReferenceLinks |> List.tryExactlyOne) "Exactly one link must be created."

                Expect.equal link.Source.LayerId "layer-1" "Link source is the earlier layer."
                Expect.equal link.Source.Side ProvenanceSide.Output "Link source is an output set."
                Expect.equal link.Target.LayerId "layer-2" "Link target is the later layer."
                Expect.equal link.Target.Side ProvenanceSide.Input "Link target is an input set."

                let sourceLayer = Session.layerById "layer-1" session
                let targetLayer = Session.layerById "layer-2" session

                Expect.equal sourceLayer.Model.OutputSets.[link.Source.SetId].Name "shared" "Source set is 'shared'."
                Expect.equal targetLayer.Model.InputSets.[link.Target.SetId].Name "shared" "Target set is 'shared'."

            testCase "does not link sets whose kinds differ"
            <| fun _ ->
                let session =
                    Session.initMany [
                        ModelBuilder.model "table-a" [] [ ModelBuilder.sampleKind, "shared" ]
                        ModelBuilder.model "table-b" [ ModelBuilder.dataKind, "shared" ] []
                    ]

                Expect.isEmpty session.ReferenceLinks "A name match with a different kind is not a link."

            testCase "links to the nearest preceding layer only"
            <| fun _ ->
                let session =
                    Session.initMany [
                        ModelBuilder.model "table-a" [] [ ModelBuilder.sampleKind, "shared" ]
                        ModelBuilder.model "table-b" [] [ ModelBuilder.sampleKind, "shared" ]
                        ModelBuilder.model "table-c" [ ModelBuilder.sampleKind, "shared" ] []
                    ]

                let link =
                    Expect.wantSome (session.ReferenceLinks |> List.tryExactlyOne) "Exactly one link must be created."

                Expect.equal link.Source.LayerId "layer-2" "The nearest preceding match wins."
                Expect.equal link.Target.LayerId "layer-3" "The consuming layer is the target."

            testCase "requires at least one model"
            <| fun _ ->
                Expect.throwsT<System.ArgumentException>
                    (fun () -> Session.initMany [] |> ignore)
                    "An empty model list is a caller error."
        ]

        testList "writeBackMany" [
            testCase "routes value edits to each loaded table's annotations"
            <| fun _ ->
                let arc, _, parameterOne, parameterTwo = chainedTables ()
                let one, two, session, indices = loadChained arc

                let session =
                    session
                    |> update (propertyIdByName "param-one" one.Model) (ProvenanceValue.Integer 9) None
                    |> selectLayer "layer-2"
                    |> update (propertyIdByName "param-two" two.Model) (ProvenanceValue.Integer 11) None

                let summary = writeBackMany indices session arc |> expectOk

                Expect.equal summary.UpdatedAnnotations 2 "One annotation per table must be updated."
                Expect.equal parameterOne.Value (Some "9") "stage-one's parameter receives its own edit."
                Expect.equal parameterTwo.Value (Some "11") "stage-two's parameter receives its own edit."

            testCase "materializes structural edits under the edited table only"
            <| fun _ ->
                let arc, dataset, _, _ = chainedTables ()
                let _, two, session, indices = loadChained arc

                let existingHeader =
                    two.Model.OutputSets |> Map.toList |> List.head |> snd |> _.Header

                let session = session |> selectLayer "layer-2"

                let session =
                    session |> createSet ProvenanceSide.Output existingHeader "extra-output"

                let newSetId =
                    (Session.layerById "layer-2" session).Model.OutputSets
                    |> Map.toList
                    |> List.find (fun (_, set) -> set.Name = "extra-output")
                    |> fst

                let inputSetId = two.Model.InputSets |> Map.toList |> List.head |> fst
                let session = session |> connect inputSetId newSetId

                let summary = writeBackMany indices session arc |> expectOk

                Expect.equal summary.AddedProcesses 1 "The connection materializes one new process row."
                Expect.equal (processCountByName dataset "stage-two") 2 "The new row belongs to stage-two."
                Expect.equal (processCountByName dataset "stage-one") 1 "stage-one must stay untouched."

            testCase "materializes a session-created layer exactly once"
            <| fun _ ->
                let arc, dataset, _, _ = chainedTables ()
                let _, _, session, indices = loadChained arc

                let session = session |> selectLayer "layer-2" |> addLayer "analysis-neutral" []

                let summary = writeBackMany indices session arc |> expectOk

                Expect.equal summary.AddedProcesses 1 "The new layer materializes one process."
                Expect.equal summary.RemovedProcesses 0 "No loaded process may be removed."
                Expect.equal (processCountByName dataset "analysis-neutral") 1 "The new layer's process exists once."
                Expect.equal (processCountByName dataset "stage-one") 1 "Loaded tables are never re-materialized."
                Expect.equal (processCountByName dataset "stage-two") 1 "Loaded tables are never re-materialized."

            testCase "an untouched multi-source session writes back as a no-op"
            <| fun _ ->
                let arc, dataset, _, _ = chainedTables ()
                let _, _, session, indices = loadChained arc

                let summary = writeBackMany indices session arc |> expectOk

                Expect.equal summary.AddedProcesses 0 "Nothing was edited."
                Expect.equal summary.RemovedProcesses 0 "Nothing was edited."
                Expect.equal summary.UpdatedAnnotations 0 "Nothing was edited."
                Expect.equal (Seq.length dataset.Processes) 2 "The dataset keeps its two loaded processes."

            testCase "single-index writeBack rejects a session with a second loaded layer"
            <| fun _ ->
                let arc, _, _, _ = chainedTables ()
                let one, _, session, _ = loadChained arc

                let errors = writeBack one.Index session arc |> expectError

                Expect.contains
                    errors
                    (ProcessCoreWritebackError.DuplicateLayerName "stage-two")
                    "Without stage-two's index, its loaded layer would be re-materialized as a duplicate process."

            testCase "rejects two loaded layers sharing one table name"
            <| fun _ ->
                let datasetOne =
                    Dataset(
                        "dataset-one",
                        processes = [
                            mkProcess "stage-neutral" [ SampleNode(Sample("input-one")) ] [
                                SampleNode(Sample("output-one"))
                            ]
                        ]
                    )

                let datasetTwo =
                    Dataset(
                        "dataset-two",
                        processes = [
                            mkProcess "stage-neutral" [ SampleNode(Sample("input-two")) ] [
                                SampleNode(Sample("output-two"))
                            ]
                        ]
                    )

                let arc = ARC("arc-neutral", hasPart = [ datasetOne; datasetTwo ])

                let one =
                    fromArc
                        {
                            DatasetPath = [ "arc-neutral"; "dataset-one" ]
                            TableName = "stage-neutral"
                        }
                        arc
                    |> expectOk

                let two =
                    fromArc
                        {
                            DatasetPath = [ "arc-neutral"; "dataset-two" ]
                            TableName = "stage-neutral"
                        }
                        arc
                    |> expectOk

                let session = Session.initMany [ one.Model; two.Model ]

                let indices =
                    Map [
                        one.Model.Source.Id, one.Index
                        two.Model.Source.Id, two.Index
                    ]

                let errors = writeBackMany indices session arc |> expectError

                Expect.contains
                    errors
                    (ProcessCoreWritebackError.DuplicateLayerName "stage-neutral")
                    "Structural patches route by table name, so equal names are ambiguous."

            testCase "requires at least one index"
            <| fun _ ->
                let arc, _, _, _ = chainedTables ()
                let _, _, session, _ = loadChained arc

                Expect.throwsT<System.ArgumentException>
                    (fun () -> writeBackMany Map.empty session arc |> ignore)
                    "An empty index map is a caller error."

            testCase "selectLayer round-trips after editing a chained loaded layer"
            <| fun _ ->
                let session = Swate.Components.Page.ProvenanceGrouping.Fixtures.chainedSession ()
                let session = Session.selectLayer "layer-2" session |> expectOk |> fst

                let session =
                    Session.updatePropertyValue "pv-measurement-analysis" (ProvenanceValue.Text "Imaging") None session
                    |> expectOk
                    |> fst

                let session = Session.selectLayer "layer-1" session |> expectOk |> fst
                Expect.equal session.ActiveLayerId "layer-1" "Focus returns to the first loaded layer."

            testCase "requires index keys to equal their InitialSourceId"
            <| fun _ ->
                let arc, _, _, _ = chainedTables ()
                let one, _, session, _ = loadChained arc

                Expect.throwsT<System.ArgumentException>
                    (fun () -> writeBackMany (Map [ "wrong-key", one.Index ]) session arc |> ignore)
                    "A mis-keyed index map is a caller error."
        ]
    ]

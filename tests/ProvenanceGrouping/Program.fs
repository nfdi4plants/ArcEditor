module ProcessCoreTests.Program

open Expecto
open ProcessCore

[<EntryPoint>]
let main argv =
    // ProcessCore.Javascript 0.1.2 lazily initializes mutable YAML encoder
    // metadata. Initialize the two encoders used by these suites before
    // Expecto starts parallel tests, otherwise their first calls can race.
    ProcessCore.Yaml.Annotation.toYamlString None (Annotation("serializer-initialization"))
    |> ignore

    ProcessCore.Yaml.Recipe.toYamlString None (Recipe(name = "serializer-initialization"))
    |> ignore

    testList "ProcessCore provenance adapter" [
        CanonicalModelTests.tests
        CanonicalCommandsTests.tests
        CanonicalAvailabilityTests.tests
        CanonicalProjectionTests.tests
        CanonicalPreparationTests.tests
        ProcessCoreAdapterContractTests.tests
        ProcessCoreConverterTests.tests
        ProcessCoreWritebackTests.tests
        ProcessCoreMultiSourceTests.tests
        ProcessCoreFanInOutTests.tests
        ProcessCoreSessionLoaderTests.tests
        ProcessCoreSupersedeTests.tests
        ProcessCorePropertyRemovalTests.tests
    ]
    |> runTestsWithCLIArgs [] argv

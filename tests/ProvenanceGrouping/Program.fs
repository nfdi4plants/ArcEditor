module ProcessCoreTests.Program

open Expecto

[<EntryPoint>]
let main argv =
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
    ]
    |> runTestsWithCLIArgs [] argv

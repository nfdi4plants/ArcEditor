module ProcessCoreTests.Program

open Expecto

[<EntryPoint>]
let main argv =
    testList "ProcessCore provenance adapter" [
        ProcessCoreAdapterContractTests.tests
        ProcessCoreConverterTests.tests
        ProcessCoreWritebackTests.tests
        ProcessCoreMultiSourceTests.tests
        ProcessCoreFanInOutTests.tests
        ProcessCoreSessionLoaderTests.tests
        ProcessCoreSupersedeTests.tests
    ]
    |> runTestsWithCLIArgs [] argv

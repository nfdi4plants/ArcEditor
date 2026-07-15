#r "nuget: ProcessCore, 0.0.8"

open System
open System.IO
open ProcessCore

let defaultSourceRoot = @"C:\Users\Patri\Desktop\ARCs\xlsx"
let defaultDestinationRoot = @"C:\Users\Patri\Desktop\ARCs\yml"

let fullPath (path: string) =
    Path.GetFullPath path
    |> fun value -> value.TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar)

let sourceRootArgument, destinationRootArgument =
    match fsi.CommandLineArgs |> Array.skip 1 with
    | [||] -> defaultSourceRoot, defaultDestinationRoot
    | [| source; destination |] -> source, destination
    | _ -> failwith "Usage: dotnet fsi scripts/ConvertArcsToYml.fsx [<source-root> <destination-root>]"

let sourceRoot = fullPath sourceRootArgument
let destinationRoot = fullPath destinationRootArgument

if not (Directory.Exists sourceRoot) then
    failwith $"Source directory does not exist: {sourceRoot}"

if String.Equals(sourceRoot, destinationRoot, StringComparison.OrdinalIgnoreCase) then
    failwith "Source and destination directories must be different."

let arcRoots =
    Directory.EnumerateFiles(sourceRoot, "isa.investigation.xlsx", SearchOption.AllDirectories)
    |> Seq.map Path.GetDirectoryName
    |> Seq.distinct
    |> Seq.sort
    |> Seq.toArray

if Array.isEmpty arcRoots then
    failwith $"No spreadsheet ARCs were found below: {sourceRoot}"

Directory.CreateDirectory destinationRoot |> ignore

// The spreadsheet reader can materialize completely blank data cells as Data("").
// ARC YAML requires every Data object to have a non-empty path, so discard those
// empty references in the same way an empty spreadsheet cell would be ignored.
let removeEmptyDataReferences (arc: ARC) =
    let mutable removedCount = 0
    let hasEmptyPath (data: Data) = String.IsNullOrWhiteSpace data.Path

    let rec cleanDataParts (data: Data) =
        for part in data.HasPart |> Seq.toArray do
            if hasEmptyPath part then
                data.RemovePart part
                removedCount <- removedCount + 1
            else
                cleanDataParts part

    let rec cleanDataset (dataset: Dataset) =
        for proc in dataset.Processes do
            for input in proc.Inputs |> Seq.toArray do
                match input with
                | DataNode data when hasEmptyPath data ->
                    proc.RemoveInput input
                    removedCount <- removedCount + 1
                | DataNode data -> cleanDataParts data
                | SampleNode _ -> ()

            for output in proc.Outputs |> Seq.toArray do
                match output with
                | DataNode data when hasEmptyPath data ->
                    proc.RemoveOutput output
                    removedCount <- removedCount + 1
                | DataNode data -> cleanDataParts data
                | SampleNode _ -> ()

        for data in dataset.DataFiles |> Seq.toArray do
            if hasEmptyPath data then
                dataset.RemoveDataFile data
                removedCount <- removedCount + 1
            else
                cleanDataParts data

        for dataContext in dataset.DataContexts |> Seq.toArray do
            if hasEmptyPath dataContext.Data then
                dataset.RemoveDataContext dataContext
                removedCount <- removedCount + 1
            else
                cleanDataParts dataContext.Data

        for child in dataset.HasPart do
            cleanDataset child

    cleanDataset arc
    removedCount

let failures = ResizeArray<string * exn>()
let mutable convertedCount = 0

for sourceArcRoot in arcRoots do
    let relativePath = Path.GetRelativePath(sourceRoot, sourceArcRoot)
    let destinationArcRoot = Path.Combine(destinationRoot, relativePath)

    try
        printfn "Converting: %s" relativePath
        Directory.CreateDirectory destinationArcRoot |> ignore

        let arc = ARC.load sourceArcRoot
        let removedEmptyDataCount = removeEmptyDataReferences arc

        if removedEmptyDataCount > 0 then
            printfn "Ignored:    %d empty spreadsheet data reference(s)" removedEmptyDataCount

        arc.Write destinationArcRoot

        let yamlPath = Path.Combine(destinationArcRoot, "arc.yml")

        if not (File.Exists yamlPath) then
            failwith $"The converter did not create {yamlPath}"

        // Loading the result checks that the generated document is valid ARC YAML.
        ARC.load destinationArcRoot |> ignore
        convertedCount <- convertedCount + 1
        printfn "Written:    %s" yamlPath
    with ex ->
        failures.Add(relativePath, ex)
        eprintfn "Failed:     %s" relativePath
        eprintfn "            %s" ex.Message

printfn ""
printfn "Converted %d of %d spreadsheet ARC(s) into %s" convertedCount arcRoots.Length destinationRoot

if failures.Count > 0 then
    let details =
        failures
        |> Seq.map (fun (relativePath, ex) -> $"- {relativePath}: {ex.Message}")
        |> String.concat Environment.NewLine

    failwith $"{failures.Count} ARC conversion(s) failed:{Environment.NewLine}{details}"

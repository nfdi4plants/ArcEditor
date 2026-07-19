[<AutoOpen>]
module ProcessCore.ExtensionsCopy

open ProcessCore


[<RequireQualifiedAccess>]
module ResizeArray =

    let Copy (copyFunc: 'T -> 'T) (seq: seq<'T>) : ResizeArray<'T> = seq |> Seq.map copyFunc |> ResizeArray

type DefinedTerm with
    /// Create a new object reference (copy) with optional overrides.
    member this.Copy(?name, ?tan, ?inDefinedTermSet) : DefinedTerm =
        let name = name |> Option.defaultValue this.Name
        let tan = tan |> Option.defaultValue this.TAN
        let inDefinedTermSet = inDefinedTermSet |> Option.defaultValue this.InDefinedTermSet

        let copy =
            DefinedTerm(name = name, ?tan = tan, ?inDefinedTermSet = inDefinedTermSet)

        this.Properties |> Seq.iter (fun v -> copy.SetProperty(v.Key, v.Value))
        copy

type FormalParameter with

    member this.Copy(?name, ?nameTAN, ?defaultValue) : FormalParameter =
        let nameTAN = nameTAN |> Option.defaultValue this.NameTAN

        let defaultValue =
            defaultValue
            |> Option.defaultValue this.DefaultValue
            |> Option.map (fun dv -> dv.Copy())

        let copy =
            FormalParameter(name = defaultArg name this.Name, ?nameTAN = nameTAN, ?defaultValue = defaultValue)

        this.Properties |> Seq.iter (fun v -> copy.SetProperty(v.Key, v.Value))
        copy

type Annotation with
    member this.Copy(?name, ?value, ?unit, ?nameTAN, ?valueTAN, ?unitTAN, ?additionalType, ?instanceOf) : Annotation =
        let name = name |> Option.defaultValue this.Name
        let value = value |> Option.defaultValue this.Value
        let unit = unit |> Option.defaultValue this.Unit
        let nameTAN = nameTAN |> Option.defaultValue this.NameTAN
        let valueTAN = valueTAN |> Option.defaultValue this.ValueTAN
        let unitTAN = unitTAN |> Option.defaultValue this.UnitTAN
        let additionalType = additionalType |> Option.defaultValue this.AdditionalType

        let instanceOf =
            instanceOf
            |> Option.defaultValue this.InstanceOf
            |> Option.map (fun io -> io.Copy())

        let copy =
            Annotation(
                name = name,
                ?value = value,
                ?unit = unit,
                ?nameTAN = nameTAN,
                ?valueTAN = valueTAN,
                ?unitTAN = unitTAN,
                ?additionalType = additionalType,
                ?instanceOf = instanceOf
            )

        this.Properties |> Seq.iter (fun v -> copy.SetProperty(v.Key, v.Value))
        copy

type Organization with

    member this.Copy(?name, ?id, ?url) : Organization =
        let name = name |> Option.defaultValue this.Name
        let id = id |> Option.defaultValue this.Id
        let url = url |> Option.defaultValue this.Url
        let copy = Organization(name = name, ?id = id, ?url = url)
        this.Properties |> Seq.iter (fun v -> copy.SetProperty(v.Key, v.Value))
        copy

type Agent with
    member this.Copy
        (
            ?givenName,
            ?id,
            ?familyName,
            ?email,
            ?affiliation: Organization option,
            ?identifier,
            ?jobTitles,
            ?additionalName,
            ?address,
            ?telephone,
            ?additionalProperty
        ) : Agent =
        let givenName = givenName |> Option.defaultValue this.GivenName
        let id = id |> Option.defaultValue this.Id
        let familyName = familyName |> Option.defaultValue this.FamilyName
        let email = email |> Option.defaultValue this.Email

        let affiliation =
            affiliation
            |> Option.defaultValue this.Affiliation
            |> Option.map (fun affiliation -> affiliation.Copy())

        let identifier = identifier |> Option.defaultValue this.Identifier

        let jobTitles =
            jobTitles |> Option.defaultValue this.JobTitles |> ResizeArray.Copy _.Copy()

        let additionalName = additionalName |> Option.defaultValue this.AdditionalName
        let address = address |> Option.defaultValue this.Address
        let telephone = telephone |> Option.defaultValue this.Telephone

        let additionalProperty =
            additionalProperty
            |> Option.defaultValue this.AdditionalProperty
            |> ResizeArray.Copy _.Copy()

        let copy =
            Agent(
                givenName = givenName,
                ?id = id,
                ?familyName = familyName,
                ?email = email,
                ?affiliation = affiliation,
                ?identifier = identifier,
                jobTitles = jobTitles,
                ?additionalName = additionalName,
                ?address = address,
                ?telephone = telephone,
                additionalProperty = additionalProperty
            )

        this.Properties |> Seq.iter (fun v -> copy.SetProperty(v.Key, v.Value))
        copy

type Data with
    member this.Copy
        (?path, ?selector, ?selectorFormat, ?encodingFormat, ?additionalType, ?hasPart, ?additionalProperty)
        : Data =
        let path = path |> Option.defaultValue this.Path
        let selector = selector |> Option.defaultValue this.Selector
        let selectorFormat = selectorFormat |> Option.defaultValue this.SelectorFormat
        let encodingFormat = encodingFormat |> Option.defaultValue this.EncodingFormat
        let additionalType = additionalType |> Option.defaultValue this.AdditionalType

        let hasPart =
            hasPart |> Option.defaultValue this.HasPart |> ResizeArray.Copy _.Copy()

        let additionalProperty =
            additionalProperty
            |> Option.defaultValue this.AdditionalProperty
            |> ResizeArray.Copy _.Copy()

        let copy =
            Data(
                path = path,
                ?selector = selector,
                ?selectorFormat = selectorFormat,
                ?encodingFormat = encodingFormat,
                ?additionalType = additionalType,
                hasPart = hasPart,
                additionalProperty = additionalProperty
            )

        this.Properties |> Seq.iter (fun v -> copy.SetProperty(v.Key, v.Value))
        copy

type Sample with
    member this.Copy(?name, ?additionalType, ?additionalProperty) : Sample =
        let name = name |> Option.defaultValue this.Name
        let additionalType = additionalType |> Option.defaultValue this.AdditionalType

        let additionalProperty =
            additionalProperty
            |> Option.defaultValue this.AdditionalProperty
            |> ResizeArray.Copy _.Copy()

        let copy =
            Sample(name = name, ?additionalType = additionalType, additionalProperty = additionalProperty)

        this.Properties |> Seq.iter (fun v -> copy.SetProperty(v.Key, v.Value))
        copy

type IONode with
    /// If you must update a IONode, you should create a new one with the updated values instead of modifying the existing one. This method creates a new IONode with the same values as the current one.
    member this.Copy() : IONode =
        match this with
        | DataNode dataNode -> DataNode(dataNode.Copy())
        | SampleNode sample -> SampleNode(sample.Copy())

type Recipe with
    member this.Copy
        (
            ?name,
            ?description,
            ?version,
            ?url,
            ?intendedUse,
            ?additionalType,
            ?parameters,
            ?components,
            ?additionalProperty
        ) : Recipe =

        let name = name |> Option.defaultValue this.Name
        let description = description |> Option.defaultValue this.Description
        let version = version |> Option.defaultValue this.Version
        let url = url |> Option.defaultValue this.Url

        let intendedUse =
            intendedUse
            |> Option.defaultValue this.IntendedUse
            |> Option.map (fun iu -> iu.Copy())

        let additionalType = additionalType |> Option.defaultValue this.AdditionalType

        let parameters =
            parameters |> Option.defaultValue this.Parameters |> ResizeArray.Copy _.Copy()

        let components =
            components |> Option.defaultValue this.Components |> ResizeArray.Copy _.Copy()

        let additionalProperty =
            additionalProperty
            |> Option.defaultValue this.AdditionalProperty
            |> ResizeArray.Copy _.Copy()

        let copy =
            Recipe(
                ?name = name,
                ?description = description,
                ?version = version,
                ?url = url,
                ?intendedUse = intendedUse,
                ?additionalType = additionalType,
                parameters = parameters,
                components = components,
                additionalProperty = additionalProperty
            )

        this.Properties |> Seq.iter (fun v -> copy.SetProperty(v.Key, v.Value))
        copy

type Process with
    member this.Copy(?name, ?executesProtocol, ?additionalType, ?inputs, ?outputs, ?parameterValues) : Process =
        let inputs = inputs |> Option.defaultValue this.Inputs |> ResizeArray.Copy _.Copy()

        let outputs =
            outputs |> Option.defaultValue this.Outputs |> ResizeArray.Copy _.Copy()

        let parameterValues =
            parameterValues
            |> Option.defaultValue this.ParameterValue
            |> ResizeArray.Copy _.Copy()

        // Keep I/O back-references attached to only the replacement process.
        // Copied from @Etschbeijer
        this.Inputs |> Seq.toArray |> Array.iter this.RemoveInput
        this.Outputs |> Seq.toArray |> Array.iter this.RemoveOutput

        let name = name |> Option.defaultValue this.Name

        let executesProtocol =
            executesProtocol
            |> Option.defaultValue this.ExecutesProtocol
            |> Option.map (fun ep -> ep.Copy())

        let additionalType = additionalType |> Option.defaultValue this.AdditionalType

        let copy =
            Process(
                name = name,
                ?executesProtocol = executesProtocol,
                ?additionalType = additionalType,
                inputs = inputs,
                outputs = outputs,
                parameterValue = parameterValues
            )

        copy.ProcessOf <- this.ProcessOf
        this.Properties |> Seq.iter (fun v -> copy.SetProperty(v.Key, v.Value))
        copy

type ScholarlyArticle with
    member this.Copy
        (?headline, ?id, ?identifier, ?creativeWorkStatus, ?authors, ?additionalProperty)
        : ScholarlyArticle =
        let headline = headline |> Option.defaultValue this.Headline
        let id = id |> Option.defaultValue this.Id
        let identifier = identifier |> Option.defaultValue this.Identifier

        let creativeWorkStatus =
            creativeWorkStatus
            |> Option.defaultValue this.CreativeWorkStatus
            |> Option.map (fun cws -> cws.Copy())

        let authors =
            authors |> Option.defaultValue this.Authors |> ResizeArray.Copy _.Copy()

        let additionalProperty =
            additionalProperty
            |> Option.defaultValue this.AdditionalProperty
            |> ResizeArray.Copy _.Copy()


        let copy =
            ScholarlyArticle(
                headline = headline,
                ?id = id,
                ?identifier = identifier,
                ?creativeWorkStatus = creativeWorkStatus,
                authors = authors,
                additionalProperty = additionalProperty
            )

        this.Properties |> Seq.iter (fun v -> copy.SetProperty(v.Key, v.Value))
        copy

type DataContext with
    member this.Copy(?data, ?explication, ?objectType, ?unit, ?label, ?description, ?generatedBy) : DataContext =
        let data = data |> Option.defaultValue this.Data |> (fun d -> d.Copy())

        let explication =
            explication
            |> Option.defaultValue this.Explication
            |> Option.map (fun e -> e.Copy())

        let objectType =
            objectType
            |> Option.defaultValue this.ObjectType
            |> Option.map (fun ot -> ot.Copy())

        let unit = unit |> Option.defaultValue this.Unit |> Option.map (fun u -> u.Copy())
        let label = label |> Option.defaultValue this.Label
        let description = description |> Option.defaultValue this.Description
        let generatedBy = generatedBy |> Option.defaultValue this.GeneratedBy

        let copy =
            DataContext(
                data = data,
                ?explication = explication,
                ?objectType = objectType,
                ?unit = unit,
                ?label = label,
                ?description = description,
                ?generatedBy = generatedBy
            )

        this.Properties |> Seq.iter (fun v -> copy.SetProperty(v.Key, v.Value))

        copy

type Dataset with
    member this.Copy
        (
            ?identifier,
            ?title,
            ?description,
            ?additionalType,
            ?license,
            ?datePublished,
            ?dateCreated,
            ?dateModified,
            ?processes,
            ?hasPart,
            ?dataFiles,
            ?agents,
            ?citations,
            ?dataContexts,
            ?additionalProperty
        ) : Dataset =

        let identifier = identifier |> Option.defaultValue this.Identifier
        let title = title |> Option.defaultValue this.Title
        let description = description |> Option.defaultValue this.Description
        let additionalType = additionalType |> Option.defaultValue this.AdditionalType
        let license = license |> Option.defaultValue this.License
        let datePublished = datePublished |> Option.defaultValue this.DatePublished
        let dateCreated = dateCreated |> Option.defaultValue this.DateCreated
        let dateModified = dateModified |> Option.defaultValue this.DateModified

        let processes =
            processes |> Option.defaultValue this.Processes |> ResizeArray.Copy _.Copy()

        let hasPart =
            hasPart |> Option.defaultValue this.HasPart |> ResizeArray.Copy _.Copy()

        let dataFiles =
            dataFiles |> Option.defaultValue this.DataFiles |> ResizeArray.Copy _.Copy()

        let agents = agents |> Option.defaultValue this.Agents |> ResizeArray.Copy _.Copy()

        let citations =
            citations |> Option.defaultValue this.Citations |> ResizeArray.Copy _.Copy()

        let dataContexts =
            dataContexts
            |> Option.defaultValue this.DataContexts
            |> ResizeArray.Copy _.Copy()

        let additionalProperty =
            additionalProperty
            |> Option.defaultValue this.AdditionalProperty
            |> ResizeArray.Copy _.Copy()

        let copy =
            Dataset(
                identifier = identifier,
                ?title = title,
                ?description = description,
                ?additionalType = additionalType,
                ?license = license,
                ?datePublished = datePublished,
                ?dateCreated = dateCreated,
                ?dateModified = dateModified,
                processes = processes,
                hasPart = hasPart,
                dataFiles = dataFiles,
                agents = agents,
                citations = citations,
                dataContexts = dataContexts,
                additionalProperty = additionalProperty
            )

        this.Properties |> Seq.iter (fun v -> copy.SetProperty(v.Key, v.Value))

        copy

type ARC with
    member this.Copy
        (
            ?identifier,
            ?title,
            ?description,
            ?additionalType,
            ?license,
            ?datePublished,
            ?dateCreated,
            ?dateModified,
            ?processes,
            ?hasPart,
            ?dataFiles,
            ?agents,
            ?citations,
            ?dataContexts,
            ?additionalProperty
        ) : ARC =


        let processes =
            processes |> Option.defaultValue this.Processes |> ResizeArray.Copy _.Copy()

        let hasPart =
            hasPart |> Option.defaultValue this.HasPart |> ResizeArray.Copy _.Copy()

        // ProcessCore relationships carry parent back-references. Detach them from
        // the old dataset before attaching the requested collection to its copy.
        // Copied from @Etschbeijer
        for process' in this.Processes do
            this.RemoveProcess(process')

        for part in this.HasPart do
            this.RemovePart(part)

        let identifier = identifier |> Option.defaultValue this.Identifier
        let title = title |> Option.defaultValue this.Title
        let description = description |> Option.defaultValue this.Description
        let additionalType = additionalType |> Option.defaultValue this.AdditionalType
        let license = license |> Option.defaultValue this.License
        let datePublished = datePublished |> Option.defaultValue this.DatePublished
        let dateCreated = dateCreated |> Option.defaultValue this.DateCreated
        let dateModified = dateModified |> Option.defaultValue this.DateModified

        let dataFiles =
            dataFiles |> Option.defaultValue this.DataFiles |> ResizeArray.Copy _.Copy()

        let agents = agents |> Option.defaultValue this.Agents |> ResizeArray.Copy _.Copy()

        let citations =
            citations |> Option.defaultValue this.Citations |> ResizeArray.Copy _.Copy()

        let dataContexts =
            dataContexts
            |> Option.defaultValue this.DataContexts
            |> ResizeArray.Copy _.Copy()

        let additionalProperty =
            additionalProperty
            |> Option.defaultValue this.AdditionalProperty
            |> ResizeArray.Copy _.Copy()

        let copy =
            ARC(
                identifier = identifier,
                ?title = title,
                ?description = description,
                ?additionalType = additionalType,
                ?license = license,
                ?datePublished = datePublished,
                ?dateCreated = dateCreated,
                ?dateModified = dateModified,
                processes = processes,
                hasPart = hasPart,
                dataFiles = dataFiles,
                agents = agents,
                citations = citations,
                dataContexts = dataContexts,
                additionalProperty = additionalProperty
            )

        this.Properties |> Seq.iter (fun v -> copy.SetProperty(v.Key, v.Value))

        // ProcessCore hotfix: preserve ARC-only state when mandatory-field repair rebuilds the object graph.
        copy.ArcPath <- this.ArcPath
        copy.IsSpreadsheetScaffold <- this.IsSpreadsheetScaffold
        copy

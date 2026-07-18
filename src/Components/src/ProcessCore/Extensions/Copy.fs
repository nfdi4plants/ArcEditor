[<AutoOpen>]
module ProcessCore.ExtensionsCopy

open ProcessCore

module ExtensionsCopyHelper =

    let copyResizeArray (copyFunc: 'T -> 'T) (seq: seq<'T>) : ResizeArray<'T> = seq |> Seq.map copyFunc |> ResizeArray

open ExtensionsCopyHelper

type DefinedTerm with
    /// Create a new object reference (copy) with optional overrides.
    member this.Copy(?name, ?tan, ?inDefinedTermSet) : DefinedTerm =
        let tan = tan |> Option.orElse this.TAN
        let inDefinedTermSet = inDefinedTermSet |> Option.orElse this.InDefinedTermSet
        DefinedTerm(name = defaultArg name this.Name, ?tan = tan, ?inDefinedTermSet = inDefinedTermSet)

type FormalParameter with

    member this.Copy(?name, ?nameTAN, ?defaultValue) : FormalParameter =
        let nameTAN = nameTAN |> Option.orElse this.NameTAN

        let defaultValue =
            defaultValue
            |> Option.orElse this.DefaultValue
            |> Option.map (fun dv -> dv.Copy())

        FormalParameter(name = defaultArg name this.Name, ?nameTAN = nameTAN, ?defaultValue = defaultValue)

type Annotation with
    member this.Copy(?name, ?value, ?unit, ?nameTAN, ?valueTAN, ?unitTAN, ?additionalType, ?instanceOf) : Annotation =
        let name = name |> Option.defaultValue this.Name
        let value = value |> Option.orElse this.Value
        let unit = unit |> Option.orElse this.Unit
        let nameTAN = nameTAN |> Option.orElse this.NameTAN
        let valueTAN = valueTAN |> Option.orElse this.ValueTAN
        let unitTAN = unitTAN |> Option.orElse this.UnitTAN
        let additionalType = additionalType |> Option.orElse this.AdditionalType

        let instanceOf =
            instanceOf |> Option.orElse this.InstanceOf |> Option.map (fun io -> io.Copy())

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

type Organization with

    member this.Copy(?name, ?id, ?url) : Organization =
        let name = name |> Option.defaultValue this.Name
        let id = id |> Option.orElse this.Id
        let url = url |> Option.orElse this.Url
        Organization(name = name, ?id = id, ?url = url)

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
        let id = id |> Option.orElse this.Id
        let familyName = familyName |> Option.orElse this.FamilyName
        let email = email |> Option.orElse this.Email

        let affiliation =
            affiliation
            |> Option.defaultValue this.Affiliation
            |> Option.map (fun affiliation -> affiliation.Copy())

        let identifier = identifier |> Option.orElse this.Identifier

        let jobTitles =
            jobTitles |> Option.defaultValue this.JobTitles |> copyResizeArray _.Copy()

        let additionalName = additionalName |> Option.orElse this.AdditionalName
        let address = address |> Option.orElse this.Address
        let telephone = telephone |> Option.orElse this.Telephone

        let additionalProperty =
            additionalProperty
            |> Option.defaultValue this.AdditionalProperty
            |> copyResizeArray _.Copy()

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

type Data with
    member this.Copy
        (?path, ?selector, ?selectorFormat, ?encodingFormat, ?additionalType, ?hasPart, ?additionalProperty)
        : Data =
        let path = path |> Option.defaultValue this.Path
        let selector = selector |> Option.orElse this.Selector
        let selectorFormat = selectorFormat |> Option.orElse this.SelectorFormat
        let encodingFormat = encodingFormat |> Option.orElse this.EncodingFormat
        let additionalType = additionalType |> Option.orElse this.AdditionalType

        let hasPart =
            hasPart |> Option.defaultValue this.HasPart |> copyResizeArray _.Copy()

        let additionalProperty =
            additionalProperty
            |> Option.defaultValue this.AdditionalProperty
            |> copyResizeArray _.Copy()

        Data(
            path = path,
            ?selector = selector,
            ?selectorFormat = selectorFormat,
            ?encodingFormat = encodingFormat,
            ?additionalType = additionalType,
            hasPart = hasPart,
            additionalProperty = additionalProperty
        )

type Sample with
    member this.Copy(?name, ?additionalType, ?additionalProperty) : Sample =
        let name = name |> Option.defaultValue this.Name
        let additionalType = additionalType |> Option.orElse this.AdditionalType

        let additionalProperty =
            additionalProperty
            |> Option.defaultValue this.AdditionalProperty
            |> copyResizeArray _.Copy()

        Sample(name = name, ?additionalType = additionalType, additionalProperty = additionalProperty)

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
        let description = description |> Option.orElse this.Description
        let version = version |> Option.orElse this.Version
        let url = url |> Option.orElse this.Url

        let intendedUse =
            intendedUse
            |> Option.orElse this.IntendedUse
            |> Option.map (fun iu -> iu.Copy())

        let additionalType = additionalType |> Option.orElse this.AdditionalType

        let parameters =
            parameters |> Option.defaultValue this.Parameters |> copyResizeArray _.Copy()

        let components =
            components |> Option.defaultValue this.Components |> copyResizeArray _.Copy()

        let additionalProperty =
            additionalProperty
            |> Option.defaultValue this.AdditionalProperty
            |> copyResizeArray _.Copy()

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

type Process with
    member this.Copy() : Process =
        Process(
            name = this.Name,
            ?executesProtocol = (this.ExecutesProtocol |> Option.map _.Copy()),
            ?additionalType = this.AdditionalType,
            inputs = (this.Inputs |> copyResizeArray _.Copy()),
            outputs = (this.Outputs |> copyResizeArray _.Copy()),
            parameterValue = (this.ParameterValue |> copyResizeArray _.Copy())
        )

type ScholarlyArticle with
    member this.Copy
        (?headline, ?id, ?identifier, ?creativeWorkStatus, ?authors, ?additionalProperty)
        : ScholarlyArticle =
        let headline = headline |> Option.defaultValue this.Headline
        let id = id |> Option.orElse this.Id
        let identifier = identifier |> Option.orElse this.Identifier

        let creativeWorkStatus =
            creativeWorkStatus
            |> Option.orElse this.CreativeWorkStatus
            |> Option.map (fun cws -> cws.Copy())

        let authors =
            authors |> Option.defaultValue this.Authors |> copyResizeArray _.Copy()

        let additionalProperty =
            additionalProperty
            |> Option.defaultValue this.AdditionalProperty
            |> copyResizeArray _.Copy()

        ScholarlyArticle(
            headline = headline,
            ?id = id,
            ?identifier = identifier,
            ?creativeWorkStatus = creativeWorkStatus,
            authors = authors,
            additionalProperty = additionalProperty
        )

type DataContext with
    member this.Copy(?data, ?explication, ?objectType, ?unit, ?label, ?description, ?generatedBy) : DataContext =
        let data = data |> Option.defaultValue this.Data |> (fun d -> d.Copy())

        let explication =
            explication |> Option.orElse this.Explication |> Option.map (fun e -> e.Copy())

        let objectType =
            objectType |> Option.orElse this.ObjectType |> Option.map (fun ot -> ot.Copy())

        let unit = unit |> Option.orElse this.Unit |> Option.map (fun u -> u.Copy())
        let label = label |> Option.orElse this.Label
        let description = description |> Option.orElse this.Description
        let generatedBy = generatedBy |> Option.orElse this.GeneratedBy

        DataContext(
            data = data,
            ?explication = explication,
            ?objectType = objectType,
            ?unit = unit,
            ?label = label,
            ?description = description,
            ?generatedBy = generatedBy
        )

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
        let title = title |> Option.orElse this.Title
        let description = description |> Option.orElse this.Description
        let additionalType = additionalType |> Option.orElse this.AdditionalType
        let license = license |> Option.orElse this.License
        let datePublished = datePublished |> Option.orElse this.DatePublished
        let dateCreated = dateCreated |> Option.orElse this.DateCreated
        let dateModified = dateModified |> Option.orElse this.DateModified

        let processes =
            processes |> Option.defaultValue this.Processes |> copyResizeArray _.Copy()

        let hasPart =
            hasPart |> Option.defaultValue this.HasPart |> copyResizeArray _.Copy()

        let dataFiles =
            dataFiles |> Option.defaultValue this.DataFiles |> copyResizeArray _.Copy()

        let agents = agents |> Option.defaultValue this.Agents |> copyResizeArray _.Copy()

        let citations =
            citations |> Option.defaultValue this.Citations |> copyResizeArray _.Copy()

        let dataContexts =
            dataContexts
            |> Option.defaultValue this.DataContexts
            |> copyResizeArray _.Copy()

        let additionalProperty =
            additionalProperty
            |> Option.defaultValue this.AdditionalProperty
            |> copyResizeArray _.Copy()

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

        let identifier = identifier |> Option.defaultValue this.Identifier
        let title = title |> Option.orElse this.Title
        let description = description |> Option.orElse this.Description
        let additionalType = additionalType |> Option.orElse this.AdditionalType
        let license = license |> Option.orElse this.License
        let datePublished = datePublished |> Option.orElse this.DatePublished
        let dateCreated = dateCreated |> Option.orElse this.DateCreated
        let dateModified = dateModified |> Option.orElse this.DateModified

        let processes =
            processes |> Option.defaultValue this.Processes |> copyResizeArray _.Copy()

        let hasPart =
            hasPart |> Option.defaultValue this.HasPart |> copyResizeArray _.Copy()

        let dataFiles =
            dataFiles |> Option.defaultValue this.DataFiles |> copyResizeArray _.Copy()

        let agents = agents |> Option.defaultValue this.Agents |> copyResizeArray _.Copy()

        let citations =
            citations |> Option.defaultValue this.Citations |> copyResizeArray _.Copy()

        let dataContexts =
            dataContexts
            |> Option.defaultValue this.DataContexts
            |> copyResizeArray _.Copy()

        let additionalProperty =
            additionalProperty
            |> Option.defaultValue this.AdditionalProperty
            |> copyResizeArray _.Copy()

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

        // ProcessCore hotfix: preserve ARC-only state when mandatory-field repair rebuilds the object graph.
        copy.ArcPath <- this.ArcPath
        copy.IsSpreadsheetScaffold <- this.IsSpreadsheetScaffold
        copy

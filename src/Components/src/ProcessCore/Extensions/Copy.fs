[<AutoOpen>]
module ProcessCore.ExtensionsCopy

open ProcessCore

module ExtensionsCopyHelper =

    let copyResizeArray (copyFunc: 'T -> 'T) (seq: seq<'T>) : ResizeArray<'T> = seq |> Seq.map copyFunc |> ResizeArray

open ExtensionsCopyHelper

type DefinedTerm with
    member this.Copy() : DefinedTerm =
        DefinedTerm(name = this.Name, ?tan = this.TAN, ?inDefinedTermSet = this.InDefinedTermSet)

type FormalParameter with

    member this.Copy() : FormalParameter =
        FormalParameter(
            name = this.Name,
            ?nameTAN = this.NameTAN,
            ?defaultValue = (this.DefaultValue |> Option.map _.Copy())
        )

type Annotation with
    member this.Copy() : Annotation =
        Annotation(
            name = this.Name,
            ?value = this.Value,
            ?unit = this.Unit,
            ?nameTAN = this.NameTAN,
            ?valueTAN = this.ValueTAN,
            ?unitTAN = this.UnitTAN,
            ?additionalType = this.AdditionalType,
            ?instanceOf = (this.InstanceOf |> Option.map _.Copy())
        )

type Organization with

    member this.Copy() : Organization =
        Organization(name = this.Name, ?id = this.Id, ?url = this.Url)

type Agent with
    member this.Copy() : Agent =
        Agent(
            givenName = this.GivenName,
            ?id = this.Id,
            ?familyName = this.FamilyName,
            ?email = this.Email,
            ?affiliation = (this.Affiliation |> Option.map (fun a -> a.Copy())),
            ?identifier = this.Identifier,
            jobTitles = (this.JobTitles |> copyResizeArray _.Copy()),
            ?additionalName = this.AdditionalName,
            ?address = this.Address,
            ?telephone = this.Telephone,
            additionalProperty = (this.AdditionalProperty |> copyResizeArray _.Copy())
        )

type Data with
    member this.Copy() : Data =
        Data(
            path = this.Path,
            ?selector = this.Selector,
            ?selectorFormat = this.SelectorFormat,
            ?encodingFormat = this.EncodingFormat,
            ?additionalType = this.AdditionalType,
            hasPart = (this.HasPart |> copyResizeArray _.Copy()),
            additionalProperty = (this.AdditionalProperty |> copyResizeArray _.Copy())
        )

type Sample with
    member this.Copy() : Sample =
        Sample(
            name = this.Name,
            ?additionalType = this.AdditionalType,
            additionalProperty = (this.AdditionalProperty |> copyResizeArray _.Copy())
        )

type IONode with
    member this.Copy() : IONode =
        match this with
        | DataNode dataNode -> DataNode(dataNode.Copy())
        | SampleNode sample -> SampleNode(sample.Copy())

type Recipe with
    member this.Copy() : Recipe =
        Recipe(
            ?name = this.Name,
            ?description = this.Description,
            ?version = this.Version,
            ?url = this.Url,
            ?intendedUse = (this.IntendedUse |> Option.map _.Copy()),
            ?additionalType = this.AdditionalType,
            parameters = (this.Parameters |> copyResizeArray _.Copy()),
            components = (this.Components |> copyResizeArray _.Copy()),
            additionalProperty = (this.AdditionalProperty |> copyResizeArray _.Copy())
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
    member this.Copy() : ScholarlyArticle =
        ScholarlyArticle(
            headline = this.Headline,
            ?id = this.Id,
            ?identifier = this.Identifier,
            ?creativeWorkStatus = (this.CreativeWorkStatus |> Option.map _.Copy()),
            authors = (this.Authors |> copyResizeArray _.Copy()),
            additionalProperty = (this.AdditionalProperty |> copyResizeArray _.Copy())
        )

type DataContext with
    member this.Copy() : DataContext =
        DataContext(
            data = this.Data.Copy(),
            ?explication = (this.Explication |> Option.map _.Copy()),
            ?objectType = (this.ObjectType |> Option.map _.Copy()),
            ?unit = (this.Unit |> Option.map _.Copy()),
            ?label = this.Label,
            ?description = this.Description,
            ?generatedBy = this.GeneratedBy
        )

type Dataset with
    member this.Copy() : Dataset =
        Dataset(
            identifier = this.Identifier,
            ?title = this.Title,
            ?description = this.Description,
            ?additionalType = this.AdditionalType,
            ?license = this.License,
            ?datePublished = this.DatePublished,
            ?dateCreated = this.DateCreated,
            ?dateModified = this.DateModified,
            processes = (this.Processes |> copyResizeArray _.Copy()),
            hasPart = (this.HasPart |> copyResizeArray _.Copy()),
            dataFiles = (this.DataFiles |> copyResizeArray _.Copy()),
            agents = (this.Agents |> copyResizeArray _.Copy()),
            citations = (this.Citations |> copyResizeArray _.Copy()),
            dataContexts = (this.DataContexts |> copyResizeArray _.Copy()),
            additionalProperty = (this.AdditionalProperty |> copyResizeArray _.Copy())
        )

type ARC with
    member this.Copy() : ARC =
        ARC(
            identifier = this.Identifier,
            ?title = this.Title,
            ?description = this.Description,
            ?additionalType = this.AdditionalType,
            ?license = this.License,
            ?datePublished = this.DatePublished,
            ?dateCreated = this.DateCreated,
            ?dateModified = this.DateModified,
            processes = (this.Processes |> copyResizeArray _.Copy()),
            hasPart = (this.HasPart |> copyResizeArray _.Copy()),
            dataFiles = (this.DataFiles |> copyResizeArray _.Copy()),
            agents = (this.Agents |> copyResizeArray _.Copy()),
            citations = (this.Citations |> copyResizeArray _.Copy()),
            dataContexts = (this.DataContexts |> copyResizeArray _.Copy()),
            additionalProperty = (this.AdditionalProperty |> copyResizeArray _.Copy())
        )

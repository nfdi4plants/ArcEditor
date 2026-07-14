/// Plain data-transfer records mirroring the class-based ProcessCore.ARC graph.
/// ProcessCore uses classes (with back-edges, registries, DynamicObj members) which
/// cannot be sent across the Electron IPC boundary. These DTOs capture only the
/// forward data; the class-side back-edges (InputOf/OutputOf/ProcessOf/PartOf and the
/// node registry) are rebuilt automatically by the constructors and Add* methods when
/// converting back via fromDTO.
module Swate.Electron.Shared.DTOs.ArcDto

open ProcessCore

// ─────────────────────────────────────────────────────────────────────────────
// DTO records (leaf → root)
// ─────────────────────────────────────────────────────────────────────────────

type DefinedTermDto = {
    name: string
    tan: string option
    inDefinedTermSet: string option
}

type FormalParameterDto = {
    name: string
    nameTAN: string option
    defaultValue: DefinedTermDto option
}

type AnnotationDto = {
    name: string
    value: string option
    unit: string option
    nameTAN: string option
    valueTAN: string option
    unitTAN: string option
    additionalType: string option
    instanceOf: FormalParameterDto option
}

type OrganizationDto = {
    id: string option
    name: string
    url: string option
}

type AgentDto = {
    id: string option
    givenName: string
    familyName: string option
    email: string option
    affiliation: OrganizationDto option
    identifier: string option
    jobTitles: DefinedTermDto[]
    additionalName: string option
    address: string option
    telephone: string option
    additionalProperty: AnnotationDto[]
}

type ScholarlyArticleDto = {
    id: string option
    headline: string
    identifier: string option
    creativeWorkStatus: DefinedTermDto option
    authors: AgentDto[]
    additionalProperty: AnnotationDto[]
}

type SampleDto = {
    name: string
    additionalType: string option
    additionalProperty: AnnotationDto[]
}

type DataDto = {
    path: string
    selector: string option
    selectorFormat: string option
    encodingFormat: string option
    additionalType: string option
    hasPart: DataDto[]
    additionalProperty: AnnotationDto[]
}

type DataContextDto = {
    data: DataDto
    explication: DefinedTermDto option
    objectType: DefinedTermDto option
    unit: DefinedTermDto option
    label: string option
    description: string option
    generatedBy: string option
}

type RecipeDto = {
    name: string option
    description: string option
    version: string option
    url: string option
    intendedUse: DefinedTermDto option
    additionalType: string option
    parameters: FormalParameterDto[]
    components: AnnotationDto[]
    additionalProperty: AnnotationDto[]
}

[<RequireQualifiedAccess>]
type IONodeDto =
    | SampleNodeDto of SampleDto
    | DataNodeDto of DataDto

type ProcessDto = {
    name: string
    executesProtocol: RecipeDto option
    additionalType: string option
    inputs: IONodeDto[]
    outputs: IONodeDto[]
    parameterValue: AnnotationDto[]
}

type DatasetDto = {
    identifier: string
    title: string option
    description: string option
    additionalType: string option
    license: string option
    datePublished: string option
    dateCreated: string option
    dateModified: string option
    processes: ProcessDto[]
    hasPart: DatasetDto[]
    dataFiles: DataDto[]
    agents: AgentDto[]
    citations: ScholarlyArticleDto[]
    dataContexts: DataContextDto[]
    additionalProperty: AnnotationDto[]
}

type ArcDto = {
    dataset: DatasetDto
    arcPath: string option
    isSpreadsheetScaffold: bool
}

// ─────────────────────────────────────────────────────────────────────────────
// Conversions (leaf → root; each type declared before the ones that reference it)
// ─────────────────────────────────────────────────────────────────────────────

type DefinedTerm with

    static member toDTO(x: DefinedTerm) : DefinedTermDto = {
        name = x.Name
        tan = x.TAN
        inDefinedTermSet = x.InDefinedTermSet
    }

    static member fromDTO(dto: DefinedTermDto) : DefinedTerm =
        DefinedTerm(dto.name, ?tan = dto.tan, ?inDefinedTermSet = dto.inDefinedTermSet)

type FormalParameter with

    static member toDTO(x: FormalParameter) : FormalParameterDto = {
        name = x.Name
        nameTAN = x.NameTAN
        defaultValue = x.DefaultValue |> Option.map DefinedTerm.toDTO
    }

    static member fromDTO(dto: FormalParameterDto) : FormalParameter =
        FormalParameter(
            dto.name,
            ?nameTAN = dto.nameTAN,
            ?defaultValue = (dto.defaultValue |> Option.map DefinedTerm.fromDTO)
        )

type Annotation with

    static member toDTO(x: Annotation) : AnnotationDto = {
        name = x.Name
        value = x.Value
        unit = x.Unit
        nameTAN = x.NameTAN
        valueTAN = x.ValueTAN
        unitTAN = x.UnitTAN
        additionalType = x.AdditionalType
        instanceOf = x.InstanceOf |> Option.map FormalParameter.toDTO
    }

    static member fromDTO(dto: AnnotationDto) : Annotation =
        Annotation(
            dto.name,
            ?value = dto.value,
            ?unit = dto.unit,
            ?nameTAN = dto.nameTAN,
            ?valueTAN = dto.valueTAN,
            ?unitTAN = dto.unitTAN,
            ?additionalType = dto.additionalType,
            ?instanceOf = (dto.instanceOf |> Option.map FormalParameter.fromDTO)
        )

type Organization with

    static member toDTO(x: Organization) : OrganizationDto = {
        id = x.Id
        name = x.Name
        url = x.Url
    }

    static member fromDTO(dto: OrganizationDto) : Organization =
        Organization(dto.name, ?id = dto.id, ?url = dto.url)

type Agent with

    static member toDTO(x: Agent) : AgentDto = {
        id = x.Id
        givenName = x.GivenName
        familyName = x.FamilyName
        email = x.Email
        affiliation = x.Affiliation |> Option.map Organization.toDTO
        identifier = x.Identifier
        jobTitles = x.JobTitles |> Seq.map DefinedTerm.toDTO |> Array.ofSeq
        additionalName = x.AdditionalName
        address = x.Address
        telephone = x.Telephone
        additionalProperty = x.AdditionalProperty |> Seq.map Annotation.toDTO |> Array.ofSeq
    }

    static member fromDTO(dto: AgentDto) : Agent =
        Agent(
            dto.givenName,
            ?id = dto.id,
            ?familyName = dto.familyName,
            ?email = dto.email,
            ?affiliation = (dto.affiliation |> Option.map Organization.fromDTO),
            ?identifier = dto.identifier,
            jobTitles = (dto.jobTitles |> Seq.map DefinedTerm.fromDTO),
            ?additionalName = dto.additionalName,
            ?address = dto.address,
            ?telephone = dto.telephone,
            additionalProperty = (dto.additionalProperty |> Seq.map Annotation.fromDTO)
        )

type ScholarlyArticle with

    static member toDTO(x: ScholarlyArticle) : ScholarlyArticleDto = {
        id = x.Id
        headline = x.Headline
        identifier = x.Identifier
        creativeWorkStatus = x.CreativeWorkStatus |> Option.map DefinedTerm.toDTO
        authors = x.Authors |> Seq.map Agent.toDTO |> Array.ofSeq
        additionalProperty = x.AdditionalProperty |> Seq.map Annotation.toDTO |> Array.ofSeq
    }

    static member fromDTO(dto: ScholarlyArticleDto) : ScholarlyArticle =
        ScholarlyArticle(
            dto.headline,
            ?id = dto.id,
            ?identifier = dto.identifier,
            ?creativeWorkStatus = (dto.creativeWorkStatus |> Option.map DefinedTerm.fromDTO),
            authors = (dto.authors |> Seq.map Agent.fromDTO),
            additionalProperty = (dto.additionalProperty |> Seq.map Annotation.fromDTO)
        )

type Sample with

    static member toDTO(x: Sample) : SampleDto = {
        name = x.Name
        additionalType = x.AdditionalType
        additionalProperty = x.AdditionalProperty |> Seq.map Annotation.toDTO |> Array.ofSeq
    }

    static member fromDTO(dto: SampleDto) : Sample =
        Sample(
            dto.name,
            ?additionalType = dto.additionalType,
            additionalProperty = (dto.additionalProperty |> Seq.map Annotation.fromDTO)
        )

type Data with

    static member toDTO(x: Data) : DataDto = {
        path = x.Path
        selector = x.Selector
        selectorFormat = x.SelectorFormat
        encodingFormat = x.EncodingFormat
        additionalType = x.AdditionalType
        hasPart = x.HasPart |> Seq.map Data.toDTO |> Array.ofSeq
        additionalProperty = x.AdditionalProperty |> Seq.map Annotation.toDTO |> Array.ofSeq
    }

    static member fromDTO(dto: DataDto) : Data =
        Data(
            dto.path,
            ?selector = dto.selector,
            ?selectorFormat = dto.selectorFormat,
            ?encodingFormat = dto.encodingFormat,
            ?additionalType = dto.additionalType,
            hasPart = (dto.hasPart |> Seq.map Data.fromDTO),
            additionalProperty = (dto.additionalProperty |> Seq.map Annotation.fromDTO)
        )

module private IONodeDtoConvert =

    let toDTO (node: IONode) : IONodeDto =
        match node with
        | SampleNode m -> IONodeDto.SampleNodeDto(Sample.toDTO m)
        | DataNode d -> IONodeDto.DataNodeDto(Data.toDTO d)

    let fromDTO (dto: IONodeDto) : IONode =
        match dto with
        | IONodeDto.SampleNodeDto m -> SampleNode(Sample.fromDTO m)
        | IONodeDto.DataNodeDto d -> DataNode(Data.fromDTO d)

type DataContext with

    static member toDTO(x: DataContext) : DataContextDto = {
        data = Data.toDTO x.Data
        explication = x.Explication |> Option.map DefinedTerm.toDTO
        objectType = x.ObjectType |> Option.map DefinedTerm.toDTO
        unit = x.Unit |> Option.map DefinedTerm.toDTO
        label = x.Label
        description = x.Description
        generatedBy = x.GeneratedBy
    }

    static member fromDTO(dto: DataContextDto) : DataContext =
        DataContext(
            Data.fromDTO dto.data,
            ?explication = (dto.explication |> Option.map DefinedTerm.fromDTO),
            ?objectType = (dto.objectType |> Option.map DefinedTerm.fromDTO),
            ?unit = (dto.unit |> Option.map DefinedTerm.fromDTO),
            ?label = dto.label,
            ?description = dto.description,
            ?generatedBy = dto.generatedBy
        )

type Recipe with

    static member toDTO(x: Recipe) : RecipeDto = {
        name = x.Name
        description = x.Description
        version = x.Version
        url = x.Url
        intendedUse = x.IntendedUse |> Option.map DefinedTerm.toDTO
        additionalType = x.AdditionalType
        parameters = x.Parameters |> Seq.map FormalParameter.toDTO |> Array.ofSeq
        components = x.Components |> Seq.map Annotation.toDTO |> Array.ofSeq
        additionalProperty = x.AdditionalProperty |> Seq.map Annotation.toDTO |> Array.ofSeq
    }

    static member fromDTO(dto: RecipeDto) : Recipe =
        Recipe(
            ?name = dto.name,
            ?description = dto.description,
            ?version = dto.version,
            ?url = dto.url,
            ?intendedUse = (dto.intendedUse |> Option.map DefinedTerm.fromDTO),
            ?additionalType = dto.additionalType,
            parameters = (dto.parameters |> Seq.map FormalParameter.fromDTO),
            components = (dto.components |> Seq.map Annotation.fromDTO),
            additionalProperty = (dto.additionalProperty |> Seq.map Annotation.fromDTO)
        )

type Process with

    static member toDTO(x: Process) : ProcessDto = {
        name = x.Name
        executesProtocol = x.ExecutesProtocol |> Option.map Recipe.toDTO
        additionalType = x.AdditionalType
        inputs = x.Inputs |> Seq.map IONodeDtoConvert.toDTO |> Array.ofSeq
        outputs = x.Outputs |> Seq.map IONodeDtoConvert.toDTO |> Array.ofSeq
        parameterValue = x.ParameterValue |> Seq.map Annotation.toDTO |> Array.ofSeq
    }

    static member fromDTO(dto: ProcessDto) : Process =
        Process(
            dto.name,
            ?executesProtocol = (dto.executesProtocol |> Option.map Recipe.fromDTO),
            ?additionalType = dto.additionalType,
            inputs = (dto.inputs |> Seq.map IONodeDtoConvert.fromDTO),
            outputs = (dto.outputs |> Seq.map IONodeDtoConvert.fromDTO),
            parameterValue = (dto.parameterValue |> Seq.map Annotation.fromDTO)
        )

type Dataset with

    static member toDTO(x: Dataset) : DatasetDto = {
        identifier = x.Identifier
        title = x.Title
        description = x.Description
        additionalType = x.AdditionalType
        license = x.License
        datePublished = x.DatePublished
        dateCreated = x.DateCreated
        dateModified = x.DateModified
        processes = x.Processes |> Seq.map Process.toDTO |> Array.ofSeq
        hasPart = x.HasPart |> Seq.map Dataset.toDTO |> Array.ofSeq
        dataFiles = x.DataFiles |> Seq.map Data.toDTO |> Array.ofSeq
        agents = x.Agents |> Seq.map Agent.toDTO |> Array.ofSeq
        citations = x.Citations |> Seq.map ScholarlyArticle.toDTO |> Array.ofSeq
        dataContexts = x.DataContexts |> Seq.map DataContext.toDTO |> Array.ofSeq
        additionalProperty = x.AdditionalProperty |> Seq.map Annotation.toDTO |> Array.ofSeq
    }

    static member fromDTO(dto: DatasetDto) : Dataset =
        Dataset(
            dto.identifier,
            ?title = dto.title,
            ?description = dto.description,
            ?additionalType = dto.additionalType,
            ?license = dto.license,
            ?datePublished = dto.datePublished,
            ?dateCreated = dto.dateCreated,
            ?dateModified = dto.dateModified,
            processes = (dto.processes |> Seq.map Process.fromDTO),
            hasPart = (dto.hasPart |> Seq.map Dataset.fromDTO),
            dataFiles = (dto.dataFiles |> Seq.map Data.fromDTO),
            agents = (dto.agents |> Seq.map Agent.fromDTO),
            citations = (dto.citations |> Seq.map ScholarlyArticle.fromDTO),
            dataContexts = (dto.dataContexts |> Seq.map DataContext.fromDTO),
            additionalProperty = (dto.additionalProperty |> Seq.map Annotation.fromDTO)
        )

type ARC with

    static member toDTO(x: ARC) : ArcDto = {
        dataset = Dataset.toDTO (x :> Dataset)
        arcPath = x.ArcPath
        isSpreadsheetScaffold = x.IsSpreadsheetScaffold
    }

    static member fromDTO(dto: ArcDto) : ARC =
        let d = dto.dataset
        let arc =
            ARC(
                d.identifier,
                ?title = d.title,
                ?description = d.description,
                ?additionalType = d.additionalType,
                ?license = d.license,
                ?datePublished = d.datePublished,
                ?dateCreated = d.dateCreated,
                ?dateModified = d.dateModified,
                processes = (d.processes |> Seq.map Process.fromDTO),
                hasPart = (d.hasPart |> Seq.map Dataset.fromDTO),
                dataFiles = (d.dataFiles |> Seq.map Data.fromDTO),
                agents = (d.agents |> Seq.map Agent.fromDTO),
                citations = (d.citations |> Seq.map ScholarlyArticle.fromDTO),
                dataContexts = (d.dataContexts |> Seq.map DataContext.fromDTO),
                additionalProperty = (d.additionalProperty |> Seq.map Annotation.fromDTO)
            )
        arc.ArcPath <- dto.arcPath
        arc.IsSpreadsheetScaffold <- dto.isSpreadsheetScaffold
        arc

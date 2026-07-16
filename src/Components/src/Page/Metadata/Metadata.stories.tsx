import React from 'react';
import type { Meta, StoryObj } from '@storybook/react-vite';
import {
  Agent as ProcessCoreAgent,
  Organization as ProcessCoreOrganization,
  ScholarlyArticle as ProcessCoreScholarlyArticle,
} from '../../fable_modules/ProcessCore.Javascript.0.0.8/Administrative.fs.js';
import { Annotation as ProcessCoreAnnotation } from '../../fable_modules/ProcessCore.Javascript.0.0.8/Annotation.fs.js';
import { DefinedTerm as ProcessCoreDefinedTerm } from '../../fable_modules/ProcessCore.Javascript.0.0.8/DefinedTerm.fs.js';
import { FormalParameter as ProcessCoreFormalParameter } from '../../fable_modules/ProcessCore.Javascript.0.0.8/FormalParameter.fs.js';
import {
  Data as ProcessCoreData,
  DataContext as ProcessCoreDataContext,
  Dataset as ProcessCoreDataset,
  Process as ProcessCoreProcess,
  Recipe as ProcessCoreRecipe,
  Sample as ProcessCoreSample,
} from '../../fable_modules/ProcessCore.Javascript.0.0.8/Graph.fs.js';
import { AnnotationMetadata } from './AnnotationMetadata.fs.js';
import { DataContextMetadata } from './DataContextMetadata.fs.js';
import { DataMetadata } from './DataMetadata.fs.js';
import { DatasetMetadata } from './DatasetMetadata.fs.js';
import { DefinedTermMetadata } from './DefinedTermMetadata.fs.js';
import { FormalParameterMetadata } from './FormalParameterMetadata.fs.js';
import { AgentMetadata } from './AgentMetadata.fs.js';
import { OrganizationMetadata } from './OrganizationMetadata.fs.js';
import { ProcessMetadata } from './ProcessMetadata.fs.js';
import { RecipeMetadata } from './RecipeMetadata.fs.js';
import { SampleMetadata } from './SampleMetadata.fs.js';
import { ScholarlyArticleMetadata } from './ScholarlyArticleMetadata.fs.js';

function AgentMetadataStory() {
  const [agent, setAgent] = React.useState(
    () => new ProcessCoreAgent('Ada', 'agent-1', 'Lovelace', 'ada.lovelace@example.org'),
  );

  return <AgentMetadata agent={agent} setAgent={setAgent} goto={() => {}} back={() => {}} />;
}

function AnnotationMetadataStory() {
  const [annotation, setAnnotation] = React.useState(
    () =>
      new ProcessCoreAnnotation(
        'Temperature',
        '22',
        'degree Celsius',
        'NCIT:C25206',
        undefined,
        'UO:0000027',
        'Parameter value',
      ),
  );

  return <AnnotationMetadata annotation={annotation} setAnnotation={setAnnotation} />;
}

function DataMetadataStory() {
  const [data, setData] = React.useState(
    () => new ProcessCoreData('data/raw/readings.csv', undefined, undefined, 'text/csv', 'Raw data'),
  );

  return <DataMetadata data={data} setData={setData} />;
}

function DataContextMetadataStory() {
  const [dataContext, setDataContext] = React.useState(
    () =>
      new ProcessCoreDataContext(
        new ProcessCoreData('data/derived/results.csv', undefined, undefined, 'text/csv'),
        undefined,
        undefined,
        undefined,
        'Normalized results',
        'Normalized measurement results.',
        'Normalization process',
      ),
  );

  return <DataContextMetadata dataContext={dataContext} setDataContext={setDataContext} />;
}

function DatasetMetadataStory() {
  const [dataset, setDataset] = React.useState(
    () =>
      new ProcessCoreDataset(
        'example-dataset',
        'Example dataset',
        'A dataset used to preview the metadata editor.',
        'Study',
        'https://creativecommons.org/licenses/by/4.0/',
        '2026-07-16T10:00',
        '2026-07-15T09:00',
        '2026-07-16T10:00',
      ),
  );

  return <DatasetMetadata dataset={dataset} setDataset={setDataset} />;
}

function DefinedTermMetadataStory() {
  const [definedTerm, setDefinedTerm] = React.useState(
    () =>
      new ProcessCoreDefinedTerm(
        'temperature',
        'PATO:0000146',
        'http://purl.obolibrary.org/obo/pato.owl',
      ),
  );

  return <DefinedTermMetadata definedTerm={definedTerm} setDefinedTerm={setDefinedTerm} />;
}

function FormalParameterMetadataStory() {
  const [formalParameter, setFormalParameter] = React.useState(
    () =>
      new ProcessCoreFormalParameter(
        'Temperature',
        'PATO:0000146',
        new ProcessCoreDefinedTerm('room temperature', 'ENVO:01001859'),
      ),
  );

  return (
    <FormalParameterMetadata
      formalParameter={formalParameter}
      setFormalParameter={setFormalParameter}
    />
  );
}

function ProcessMetadataStory() {
  const [process, setProcess] = React.useState(
    () =>
      new ProcessCoreProcess(
        'Sample extraction',
        new ProcessCoreRecipe('Extraction protocol'),
        'Sample processing',
      ),
  );

  return <ProcessMetadata processObject={process} setProcess={setProcess} />;
}

function OrganizationMetadataStory() {
  const [organization, setOrganization] = React.useState(
    () =>
      new ProcessCoreOrganization(
        'DataPLANT',
        'organization-1',
        'https://www.nfdi4plants.org/',
      ),
  );

  return <OrganizationMetadata organization={organization} setOrganization={setOrganization} />;
}

function RecipeMetadataStory() {
  const [recipe, setRecipe] = React.useState(
    () =>
      new ProcessCoreRecipe(
        'Extraction protocol',
        'Extract material for downstream analysis.',
        '1.0',
        'https://example.org/protocols/extraction',
        undefined,
        'Sample processing',
      ),
  );

  return <RecipeMetadata recipe={recipe} setData={setRecipe} />;
}

function SampleMetadataStory() {
  const [sample, setSample] = React.useState(
    () => new ProcessCoreSample('Leaf sample', 'Biological sample'),
  );

  return <SampleMetadata sample={sample} setSample={setSample} />;
}

function ScholarlyArticleMetadataStory() {
  const [article, setArticle] = React.useState(
    () =>
      new ProcessCoreScholarlyArticle(
        'An example research article',
        'article-1',
        'https://doi.org/10.0000/example',
      ),
  );

  return <ScholarlyArticleMetadata sample={article} setSample={setArticle} />;
}

const meta = {
  title: 'Page Components/Metadata',
  decorators: [
    Story => (
      <div className="swt:max-w-4xl swt:p-4">
        <Story />
      </div>
    ),
  ],
  tags: ['autodocs'],
} satisfies Meta;

export default meta;

type Story = StoryObj<typeof meta>;

export const Agent: Story = {
  render: () => <AgentMetadataStory />,
};

export const Annotation: Story = {
  render: () => <AnnotationMetadataStory />,
};

export const Data: Story = {
  render: () => <DataMetadataStory />,
};

export const DataContext: Story = {
  render: () => <DataContextMetadataStory />,
};

export const Dataset: Story = {
  render: () => <DatasetMetadataStory />,
};

export const DefinedTerm: Story = {
  render: () => <DefinedTermMetadataStory />,
};

export const FormalParameter: Story = {
  render: () => <FormalParameterMetadataStory />,
};

export const Organization: Story = {
  render: () => <OrganizationMetadataStory />,
};

export const Process: Story = {
  render: () => <ProcessMetadataStory />,
};

export const Recipe: Story = {
  render: () => <RecipeMetadataStory />,
};

export const Sample: Story = {
  render: () => <SampleMetadataStory />,
};

export const ScholarlyArticle: Story = {
  render: () => <ScholarlyArticleMetadataStory />,
};

import React from 'react';
import type { Meta, StoryObj } from '@storybook/react-vite';
import {
  Agent as ProcessCoreAgent,
  Organization as ProcessCoreOrganization,
  ScholarlyArticle as ProcessCoreScholarlyArticle,
} from '../../fable_modules/ProcessCore.Javascript.0.0.10/Administrative.fs.js';
import { Annotation as ProcessCoreAnnotation } from '../../fable_modules/ProcessCore.Javascript.0.0.10/Annotation.fs.js';
import { DefinedTerm as ProcessCoreDefinedTerm } from '../../fable_modules/ProcessCore.Javascript.0.0.10/DefinedTerm.fs.js';
import { FormalParameter as ProcessCoreFormalParameter } from '../../fable_modules/ProcessCore.Javascript.0.0.10/FormalParameter.fs.js';
import {
  Data as ProcessCoreData,
  DataContext as ProcessCoreDataContext,
  Dataset as ProcessCoreDataset,
  Process as ProcessCoreProcess,
  Recipe as ProcessCoreRecipe,
  Sample as ProcessCoreSample,
} from '../../fable_modules/ProcessCore.Javascript.0.0.10/Graph.fs.js';
import { ARC } from '../../fable_modules/ProcessCore.Javascript.0.0.10/ARC.fs.js';
import { create as createArcView, forProcess } from '../../ProcessCore/RendererModel.fs.js';
import AgentView from './Agent.fs.js';
import AnnotationView from './Annotation.fs.js';
import DataContextView from './DataContext.fs.js';
import DataView from './Data.fs.js';
import DatasetView from './Dataset.fs.js';
import DefinedTermView from './DefinedTerm.fs.js';
import FormalParameterView from './FormalParameter.fs.js';
import OrganizationView from './Organization.fs.js';
import ProcessView from './Process.fs.js';
import RecipeView from './Recipe.fs.js';
import SampleView from './Sample.fs.js';
import ScholarlyArticleView from './ScholarlyArticle.fs.js';
import {
  ImportCatalogContextHelper_create,
  ImportContext,
  ImportCatalogCtx,
} from './FormComponents/ImportCatalogContext.fs.js';

function MetadataStoryProvider({ children }: { children: React.ReactNode }) {
  const [arc] = React.useState(() => new ARC('metadata-story-catalog'));

  return (
    <ImportCatalogCtx.Provider
      value={new ImportContext(ImportCatalogContextHelper_create(arc), undefined)}
    >
      {children}
    </ImportCatalogCtx.Provider>
  );
}

function useMetadataMutation() {
  const [arc] = React.useState(() => new ARC('metadata-story'));
  const [, setRevision] = React.useState(0);

  return {
    arc,
    mutate: (mutation: (arc: ARC) => void) => {
      mutation(arc);
      setRevision(current => current + 1);
    },
  };
}

function AgentMetadataStory() {
  const [agent] = React.useState(
    () => new ProcessCoreAgent('Ada', 'agent-1', 'Lovelace', 'ada.lovelace@example.org'),
  );
  const { mutate } = useMetadataMutation();

  return <AgentView agent={agent} mutate={mutate} />;
}

function AnnotationMetadataStory() {
  const [annotation] = React.useState(
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
  const { mutate } = useMetadataMutation();

  return <AnnotationView annotation={annotation} mutate={mutate} />;
}

function DataMetadataStory() {
  const [data] = React.useState(
    () => new ProcessCoreData('data/raw/readings.csv', undefined, undefined, 'text/csv', 'Raw data'),
  );
  const { mutate } = useMetadataMutation();

  return <DataView data={data} mutate={mutate} />;
}

function DataContextMetadataStory() {
  const [dataContext] = React.useState(
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
  const { mutate } = useMetadataMutation();

  return <DataContextView dataContext={dataContext} mutate={mutate} />;
}

function DatasetMetadataStory() {
  const [dataset] = React.useState(
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
  const { arc, mutate } = useMetadataMutation();

  return <DatasetView dataset={dataset} arcView={createArcView(arc)} mutate={mutate} />;
}

function DefinedTermMetadataStory() {
  const [definedTerm] = React.useState(
    () =>
      new ProcessCoreDefinedTerm(
        'temperature',
        'PATO:0000146',
        'http://purl.obolibrary.org/obo/pato.owl',
      ),
  );
  const { mutate } = useMetadataMutation();

  return <DefinedTermView definedTerm={definedTerm} mutate={mutate} />;
}

function FormalParameterMetadataStory() {
  const [formalParameter] = React.useState(
    () =>
      new ProcessCoreFormalParameter(
        'Temperature',
        'PATO:0000146',
        new ProcessCoreDefinedTerm('room temperature', 'ENVO:01001859'),
      ),
  );
  const { mutate } = useMetadataMutation();

  return <FormalParameterView formalParameter={formalParameter} mutate={mutate} />;
}

function ProcessMetadataStory() {
  const [process] = React.useState(
    () =>
      new ProcessCoreProcess(
        'Sample extraction',
        new ProcessCoreRecipe('Extraction protocol'),
        'Sample processing',
      ),
  );
  const { arc, mutate } = useMetadataMutation();

  return <ProcessView processView={forProcess(process, createArcView(arc))} mutate={mutate} />;
}

function OrganizationMetadataStory() {
  const [organization] = React.useState(
    () =>
      new ProcessCoreOrganization(
        'DataPLANT',
        'organization-1',
        'https://www.nfdi4plants.org/',
      ),
  );
  const { mutate } = useMetadataMutation();

  return <OrganizationView organization={organization} mutate={mutate} />;
}

function RecipeMetadataStory() {
  const [recipe] = React.useState(
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
  const { mutate } = useMetadataMutation();

  return <RecipeView recipe={recipe} mutate={mutate} />;
}

function SampleMetadataStory() {
  const [sample] = React.useState(
    () => new ProcessCoreSample('Leaf sample', 'Biological sample'),
  );
  const { mutate } = useMetadataMutation();

  return <SampleView sample={sample} mutate={mutate} />;
}

function ScholarlyArticleMetadataStory() {
  const [article] = React.useState(
    () =>
      new ProcessCoreScholarlyArticle(
        'An example research article',
        'article-1',
        'https://doi.org/10.0000/example',
      ),
  );
  const { mutate } = useMetadataMutation();

  return <ScholarlyArticleView article={article} mutate={mutate} />;
}

const meta = {
  title: 'Page Components/Metadata',
  decorators: [
    Story => (
      <MetadataStoryProvider>
        <div className="swt:max-w-4xl swt:p-4">
          <Story />
        </div>
      </MetadataStoryProvider>
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

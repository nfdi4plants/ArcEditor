import React from 'react';
import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, screen, userEvent, waitFor, within } from 'storybook/test';
import {
  Agent as ProcessCoreAgent,
  Organization as ProcessCoreOrganization,
  ScholarlyArticle as ProcessCoreScholarlyArticle,
} from '../../fable_modules/ProcessCore.Javascript.0.1.2/Administrative.fs.js';
import { Annotation as ProcessCoreAnnotation } from '../../fable_modules/ProcessCore.Javascript.0.1.2/Annotation.fs.js';
import { ARC as ProcessCoreARC } from '../../fable_modules/ProcessCore.Javascript.0.1.2/ARC.fs.js';
import { DefinedTerm as ProcessCoreDefinedTerm } from '../../fable_modules/ProcessCore.Javascript.0.1.2/DefinedTerm.fs.js';
import { FormalParameter as ProcessCoreFormalParameter } from '../../fable_modules/ProcessCore.Javascript.0.1.2/FormalParameter.fs.js';
import {
  Data as ProcessCoreData,
  DataContext as ProcessCoreDataContext,
  Dataset as ProcessCoreDataset,
  Process as ProcessCoreProcess,
  Recipe as ProcessCoreRecipe,
  Sample as ProcessCoreSample,
} from '../../fable_modules/ProcessCore.Javascript.0.1.2/Graph.fs.js';
import { AnnotationView } from './Annotation.fs.js';
import { DataContextView } from './DataContext.fs.js';
import { DataView } from './Data.fs.js';
import { DatasetView } from './Dataset.fs.js';
import { DefinedTermView } from './DefinedTerm.fs.js';
import { FormalParameterView } from './FormalParameter.fs.js';
import { AgentView } from './Agent.fs.js';
import { OrganizationView } from './Organization.fs.js';
import { ProcessView } from './Process.fs.js';
import { RecipeView } from './Recipe.fs.js';
import { SampleView } from './Sample.fs.js';
import { ScholarlyArticleView } from './ScholarlyArticle.fs.js';
import {
  ImportCatalogCtx,
  ImportCatalogContextHelper_withRecipes as catalogWithRecipes,
} from './FormComponents/ImportCatalogContext.fs.js';

// The metadata views take `mutate: (ARC -> unit) -> unit` from their host: the
// callback mutates the entity (usually via closure) against the live ARC and
// the host re-renders. Stories host a throwaway ARC and a forced re-render.
function useMutate(): (fn: (arc: ProcessCoreARC) => void) => void {
  const [arc] = React.useState(() => new ProcessCoreARC('story-arc'));
  const [, bump] = React.useReducer((x: number) => x + 1, 0);

  return React.useCallback(
    (fn: (arc: ProcessCoreARC) => void) => {
      fn(arc);
      bump();
    },
    [arc],
  );
}

function AgentMetadataStory() {
  const [agent] = React.useState(
    () => new ProcessCoreAgent('Ada', 'agent-1', 'Lovelace', 'ada.lovelace@example.org'),
  );

  return <AgentView agent={agent} mutate={useMutate()} />;
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

  return <AnnotationView annotation={annotation} mutate={useMutate()} />;
}

function DataMetadataStory() {
  const [data] = React.useState(
    () => new ProcessCoreData('data/raw/readings.csv', undefined, undefined, 'text/csv', 'Raw data'),
  );

  return <DataView data={data} mutate={useMutate()} />;
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

  return <DataContextView dataContext={dataContext} mutate={useMutate()} />;
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

  return <DatasetView dataset={dataset} mutate={useMutate()} />;
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

  return <DefinedTermView definedTerm={definedTerm} mutate={useMutate()} />;
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

  return <FormalParameterView formalParameter={formalParameter} mutate={useMutate()} />;
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

  return <ProcessView processObject={process} mutate={useMutate()} />;
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

  return <OrganizationView organization={organization} mutate={useMutate()} />;
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

  return <RecipeView recipe={recipe} mutate={useMutate()} />;
}

function SampleMetadataStory() {
  const [sample] = React.useState(
    () => new ProcessCoreSample('Leaf sample', 'Biological sample'),
  );

  return <SampleView sample={sample} mutate={useMutate()} />;
}

// Two distinct existing stored Recipes sharing one display label. The import
// selector must disambiguate them from their ArcEditor resource keys, computed
// once over the whole candidate set - not fabricated or cloned here, just two
// genuinely distinct resources reused from the catalog.
function RecipeSelectorDisambiguationStory() {
  const [process] = React.useState(
    () => new ProcessCoreProcess('Sample extraction'),
  );

  const [catalog] = React.useState(() => {
    const first = new ProcessCoreRecipe('Extraction protocol');
    first.SetProperty('@id', 'arc:recipes/extraction-one');
    const second = new ProcessCoreRecipe('Extraction protocol');
    second.SetProperty('@id', 'arc:recipes/extraction-two');
    return catalogWithRecipes([first, second]);
  });

  return (
    <ImportCatalogCtx.Provider value={catalog}>
      <ProcessView processObject={process} mutate={useMutate()} />
    </ImportCatalogCtx.Provider>
  );
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

  return <ScholarlyArticleView article={article} mutate={useMutate()} />;
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

export const RecipeSelectorDisambiguatesSameLabelCandidates: Story = {
  render: () => <RecipeSelectorDisambiguationStory />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await userEvent.click(canvas.getByRole('button', { name: 'Import' }));

    const modal = await waitFor(() => screen.getByTestId('modal_content_process-core-import'));
    const select = within(modal).getByRole('combobox');
    const optionLabels = within(select)
      .getAllByRole('option')
      .map((option) => option.textContent);

    // Both stored Recipes are named "Extraction protocol"; a per-item label
    // cannot tell them apart; the shared batch-aware hook must, from their
    // ArcEditor resource keys, without either candidate being dropped as a
    // duplicate.
    expect(optionLabels).toContain('Extraction protocol (extraction-one)');
    expect(optionLabels).toContain('Extraction protocol (extraction-two)');
    expect(optionLabels).toHaveLength(3);
  },
};

export const Sample: Story = {
  render: () => <SampleMetadataStory />,
};

export const ScholarlyArticle: Story = {
  render: () => <ScholarlyArticleMetadataStory />,
};

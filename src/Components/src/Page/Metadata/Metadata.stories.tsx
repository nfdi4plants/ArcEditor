import React from 'react';
import type { Meta, StoryObj } from '@storybook/react-vite';
import { Agent as ProcessCoreAgent } from '../../fable_modules/ProcessCore.Javascript.0.0.8/Administrative.fs.js';
import {
  Dataset as ProcessCoreDataset,
  Process as ProcessCoreProcess,
  Recipe,
  Sample as ProcessCoreSample,
} from '../../fable_modules/ProcessCore.Javascript.0.0.8/Graph.fs.js';
import { DatasetMetadata } from './DatasetMetadata.fs.js';
import { ProcessMetadata } from './ProcessMetadata.fs.js';
import { AgentMetadata } from './AgentMetadata.fs.js';
import { SampleMetadata } from './SampleMetadata.fs.js';

function AgentMetadataStory() {
  const [agent, setAgent] = React.useState(
    () => new ProcessCoreAgent('Ada', 'agent-1', 'Lovelace', 'ada.lovelace@example.org'),
  );

  return <AgentMetadata agent={agent} setAgent={setAgent} />;
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

function ProcessMetadataStory() {
  const [process, setProcess] = React.useState(
    () =>
      new ProcessCoreProcess(
        'Sample extraction',
        new Recipe('Extraction protocol'),
        'Sample processing',
      ),
  );

  return <ProcessMetadata processObject={process} setProcess={setProcess} />;
}

function SampleMetadataStory() {
  const [sample, setSample] = React.useState(
    () => new ProcessCoreSample('Leaf sample', 'Biological sample'),
  );

  return <SampleMetadata sample={sample} setSample={setSample} />;
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

export const Dataset: Story = {
  render: () => <DatasetMetadataStory />,
};

export const Process: Story = {
  render: () => <ProcessMetadataStory />,
};

export const Sample: Story = {
  render: () => <SampleMetadataStory />,
};

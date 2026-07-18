import type { Meta, StoryObj } from '@storybook/react-vite';
import React from 'react';
import { expect, userEvent, within } from 'storybook/test';
import ErrorModalProvider from '../../Primitive/ErrorModal/Provider.fs.js';
import { Items as memberCatalogItems } from './MemberCatalog.fs.js';
import MetadataBrowser from './MetadataBrowser.fs.js';
import { createProcessCoreArcFixture } from './ObjectBrowser.fixture.js';

function DatasetMetadataBrowser() {
  const [arc, setArc] = React.useState(createProcessCoreArcFixture);

  return (
    <ErrorModalProvider>
      <MetadataBrowser
        arcStateCtx={{
          state: arc,
          setStateUpdater: update => setArc(current => update(current) ?? current),
        }}
        kind={memberCatalogItems[0].data}
      />
    </ErrorModalProvider>
  );
}

function ProcessMetadataBrowser() {
  const [arc, setArc] = React.useState(createProcessCoreArcFixture);

  return (
    <ErrorModalProvider>
      <MetadataBrowser
        arcStateCtx={{
          state: arc,
          setStateUpdater: update => setArc(current => update(current) ?? current),
        }}
        kind={memberCatalogItems[1].data}
      />
    </ErrorModalProvider>
  );
}

const meta = {
  title: 'Page/ObjectBrowser/MetadataBrowser',
  component: MetadataBrowser,
  render: () => <DatasetMetadataBrowser />,
  tags: ['autodocs'],
} satisfies Meta<typeof MetadataBrowser>;

export default meta;

type Story = StoryObj<typeof meta>;

export const DatasetViewSwitch: Story = {
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await userEvent.click(canvas.getByRole('row', { name: /Child dataset/ }));
    expect(canvas.getByRole('heading', { name: 'Dataset Metadata' })).toBeVisible();

    const processes = canvas.getByText('Processes').parentElement!;
    await userEvent.click(within(processes).getByRole('button', { name: '+' }));
    expect(canvas.getByRole('button', { name: 'Open Unnamed process metadata' })).toBeVisible();

    await userEvent.click(canvas.getByRole('button', { name: 'Open Extraction process metadata' }));
    expect(canvas.getByRole('heading', { name: 'Process Metadata' })).toBeVisible();

    const processName = canvas.getAllByRole('textbox')[0];
    await userEvent.clear(processName);
    await userEvent.type(processName, 'Updated extraction process');
    await new Promise(resolve => setTimeout(resolve, 350));

    const parameterValues = canvas.getByText('Parameter Values').parentElement!;
    await userEvent.click(within(parameterValues).getByRole('button', { name: '+' }));
    await userEvent.click(canvas.getByRole('button', { name: 'Open Temperature metadata' }));

    const annotationName = canvas.getAllByRole('textbox')[0];
    await userEvent.clear(annotationName);
    await userEvent.type(annotationName, 'Updated temperature');
    await new Promise(resolve => setTimeout(resolve, 350));

    await userEvent.click(canvas.getByRole('button', { name: 'Back to Updated extraction process' }));

    const orderedAnnotations = within(
      canvas.getByText('Parameter Values').parentElement!,
    ).getAllByRole('button', { name: /^Open/ });
    expect(orderedAnnotations[0]).toHaveAccessibleName('Open Updated temperature metadata');
    expect(orderedAnnotations[1]).toHaveAccessibleName('Open Unnamed annotation metadata');

    const inputs = canvas.getByText('Inputs').parentElement!;
    await userEvent.click(within(inputs).getByRole('button', { name: '+' }));
    expect(canvas.getByRole('button', { name: 'Open Unnamed sample metadata' })).toBeVisible();

    await userEvent.click(canvas.getByRole('button', { name: 'Open Source sample metadata' }));
    expect(canvas.getByRole('heading', { name: 'Sample Metadata' })).toBeVisible();

    const sampleName = canvas.getAllByRole('textbox')[0];
    await userEvent.clear(sampleName);
    await userEvent.type(sampleName, 'Updated source sample');
    await new Promise(resolve => setTimeout(resolve, 350));

    await userEvent.click(canvas.getByRole('button', { name: 'Back to Updated extraction process' }));
    expect(canvas.getByRole('button', { name: 'Open Updated source sample metadata' })).toBeVisible();

    const outputs = canvas.getByText('Outputs').parentElement!;
    await userEvent.click(within(outputs).getByRole('button', { name: '+' }));
    expect(canvas.getByRole('button', { name: 'Open Unnamed data metadata' })).toBeVisible();

    await userEvent.click(canvas.getByRole('button', { name: 'Open dataset/results.csv metadata' }));
    expect(canvas.getByRole('heading', { name: 'Data Metadata' })).toBeVisible();
    await userEvent.click(canvas.getByRole('button', { name: 'Back to Updated extraction process' }));

    await userEvent.click(canvas.getByRole('button', { name: 'Back to Child dataset' }));
    expect(canvas.getByRole('heading', { name: 'Dataset Metadata' })).toBeVisible();
    expect(canvas.getByRole('button', { name: 'Open Updated extraction process metadata' })).toBeVisible();
    expect(canvas.getByRole('button', { name: 'Open grandchild-dataset metadata' })).toBeVisible();

    await userEvent.click(canvas.getByRole('button', { name: 'Back to Datasets' }));
    expect(canvas.getByRole('heading', { name: 'Datasets' })).toBeVisible();
  },
};

export const DeepNestedMetadata: Story = {
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await userEvent.click(canvas.getByRole('row', { name: /Child dataset/ }));
    await userEvent.click(canvas.getByRole('button', { name: 'Open Extraction process metadata' }));
    await userEvent.click(canvas.getByRole('button', { name: 'Open Extraction recipe metadata' }));
    expect(canvas.getByRole('heading', { name: 'Recipe Metadata' })).toBeVisible();

    const parameters = canvas.getByText('Parameters').parentElement!;
    await userEvent.click(within(parameters).getByRole('button', { name: '+' }));
    await userEvent.click(canvas.getByRole('button', { name: 'Open Unnamed formal parameter metadata' }));
    expect(canvas.getByRole('heading', { name: 'Formal Parameter Metadata' })).toBeVisible();

    const defaultValue = canvas.getByText('Default Value').parentElement!;
    await userEvent.click(within(defaultValue).getByRole('button', { name: '+' }));
    await userEvent.click(canvas.getByRole('button', { name: 'Open Unnamed defined term metadata' }));
    expect(canvas.getByRole('heading', { name: 'Defined Term Metadata' })).toBeVisible();

    const termName = canvas.getAllByRole('textbox')[0];
    await userEvent.type(termName, 'Configured term');
    await new Promise(resolve => setTimeout(resolve, 350));

    await userEvent.click(canvas.getByRole('button', { name: 'Back to Unnamed formal parameter' }));
    expect(canvas.getByRole('button', { name: 'Open Configured term metadata' })).toBeVisible();
    await userEvent.click(canvas.getByRole('button', { name: 'Back to Extraction recipe' }));
    expect(canvas.getByRole('heading', { name: 'Recipe Metadata' })).toBeVisible();
  },
};

export const DirectProcessMetadata: Story = {
  render: () => <ProcessMetadataBrowser />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await userEvent.click(canvas.getByRole('row', { name: /Extraction process/ }));
    expect(canvas.getByRole('heading', { name: 'Process Metadata' })).toBeVisible();
    expect(canvas.getByRole('button', { name: 'Back to Processes' })).toBeVisible();

    const processName = canvas.getAllByRole('textbox')[0];
    await userEvent.clear(processName);
    await userEvent.type(processName, 'Directly updated process');
    await new Promise(resolve => setTimeout(resolve, 350));

    await userEvent.click(canvas.getByRole('button', { name: 'Back to Processes' }));
    expect(canvas.getByRole('row', { name: /Directly updated process/ })).toBeVisible();
  },
};

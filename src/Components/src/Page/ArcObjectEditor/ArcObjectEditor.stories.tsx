import type { Meta, StoryObj } from '@storybook/react-vite';
import { useState } from 'react';
import { expect, userEvent, within } from 'storybook/test';
import ErrorModalProvider from '../../Primitive/ErrorModal/Provider.fs.js';
import { create as createArcView } from '../../ProcessCore/RendererModel.fs.js';
import { Items as memberCatalogItems } from '../ObjectBrowser/MemberCatalog.fs.js';
import { createProcessCoreArcFixture } from '../ObjectBrowser/ObjectBrowser.fixture.js';
import ArcObjectEditor from './ArcObjectEditor.fs.js';

function ArcObjectEditorExample({ kindIndex }: { kindIndex: number }) {
  const [arc] = useState(createProcessCoreArcFixture);
  const [, setRevision] = useState(0);

  return (
    <ErrorModalProvider>
      <ArcObjectEditor
        arc={arc}
        arcView={createArcView(arc)}
        mutate={mutation => {
          mutation(arc);
          setRevision(current => current + 1);
        }}
        kind={memberCatalogItems[kindIndex].data}
      />
    </ErrorModalProvider>
  );
}

const meta = {
  title: 'Page Components/ArcObjectEditor',
  component: ArcObjectEditor,
  render: () => <ArcObjectEditorExample kindIndex={0} />,
  tags: ['autodocs'],
} satisfies Meta<typeof ArcObjectEditor>;

export default meta;
type Story = StoryObj<typeof meta>;

export const DatasetViewSwitch: Story = {
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const search = canvas.getByRole('searchbox', { name: 'Search objects' });
    expect(search).toBeEnabled();
    expect(canvas.queryByRole('combobox', { name: 'Filter by object type' })).not.toBeInTheDocument();
    await userEvent.type(search, 'not present');
    expect(canvas.getByText('No objects match "not present".')).toBeVisible();
    await userEvent.clear(search);

    await userEvent.click(canvas.getByRole('row', { name: /Child dataset/ }));
    expect(canvas.getByRole('heading', { name: 'Dataset Metadata' })).toBeVisible();
    expect(search).toBeDisabled();

    await userEvent.click(canvas.getByRole('button', { name: 'Open Extraction process metadata' }));
    expect(canvas.getByRole('heading', { name: 'Process Metadata' })).toBeVisible();
    const processBreadcrumb = within(canvas.getByRole('navigation', { name: 'Breadcrumb' }));
    expect(processBreadcrumb.getByRole('button', { name: 'Datasets' })).toBeVisible();
    expect(processBreadcrumb.getByRole('button', { name: 'Child dataset' })).toBeVisible();
    expect(processBreadcrumb.getByText('Extraction process')).toBeVisible();

    const processName = canvas.getAllByRole('textbox')[0];
    await userEvent.clear(processName);
    await userEvent.type(processName, 'Updated extraction process');
    await new Promise(resolve => setTimeout(resolve, 350));

    const parameterValues = canvas.getByText('Parameter Values').parentElement!.parentElement!.parentElement!.parentElement!;
    await userEvent.click(canvas.getByRole('button', { name: 'Open Temperature metadata' }));
    await userEvent.click(canvas.getByRole('button', { name: 'Back to Updated extraction process' }));

    const orderedAnnotations = within(parameterValues).getAllByRole('button', { name: /^Open/ });
    expect(orderedAnnotations[0]).toHaveAccessibleName('Open Temperature metadata');

    await userEvent.click(canvas.getByRole('button', { name: 'Open Source sample metadata' }));
    expect(canvas.getByRole('heading', { name: 'Sample Metadata' })).toBeVisible();

    const sampleName = canvas.getAllByRole('textbox')[0];
    await userEvent.clear(sampleName);
    await userEvent.type(sampleName, 'Updated source sample');
    await new Promise(resolve => setTimeout(resolve, 350));
    await userEvent.click(canvas.getByRole('button', { name: 'Back to Updated extraction process' }));
    expect(canvas.getByRole('button', { name: 'Open Updated source sample metadata' })).toBeVisible();

    await userEvent.click(canvas.getByRole('button', { name: 'Open dataset/results.csv metadata' }));
    expect(canvas.getByRole('heading', { name: 'Data Metadata' })).toBeVisible();
    await userEvent.click(canvas.getByRole('button', { name: 'Back to Updated extraction process' }));
    await userEvent.click(
      within(canvas.getByRole('navigation', { name: 'Breadcrumb' })).getByRole('button', {
        name: 'Child dataset',
      }),
    );
    expect(canvas.getByRole('heading', { name: 'Dataset Metadata' })).toBeVisible();
    expect(canvas.getByRole('button', { name: 'Open Updated extraction process metadata' })).toBeVisible();
    expect(canvas.getByRole('button', { name: 'Open grandchild-dataset metadata' })).toBeVisible();
    await userEvent.click(
      within(canvas.getByRole('navigation', { name: 'Breadcrumb' })).getByRole('button', { name: 'Datasets' }),
    );
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
    await userEvent.click(within(parameters).getByRole('button', { name: 'Add' }));
    await userEvent.click(canvas.getByRole('button', { name: 'Open New Formal Parameter metadata' }));
    expect(canvas.getByRole('heading', { name: 'Formal Parameter Metadata' })).toBeVisible();

    const defaultValue = canvas.getByText('Default Value').parentElement!;
    await userEvent.click(within(defaultValue).getByRole('button', { name: 'Add' }));
    await userEvent.click(canvas.getByRole('button', { name: 'Open New Defined Term metadata' }));
    expect(canvas.getByRole('heading', { name: 'Defined Term Metadata' })).toBeVisible();
    await userEvent.click(canvas.getByRole('button', { name: 'Back to New Formal Parameter' }));
    expect(canvas.getByRole('button', { name: 'Open New Defined Term metadata' })).toBeVisible();
    await userEvent.click(canvas.getByRole('button', { name: 'Back to Extraction recipe' }));
    expect(canvas.getByRole('heading', { name: 'Recipe Metadata' })).toBeVisible();
  },
};

export const DirectProcessMetadata: Story = {
  render: () => <ArcObjectEditorExample kindIndex={1} />,
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

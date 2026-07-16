import type { Meta, StoryObj } from '@storybook/react-vite';
import React from 'react';
import { expect, userEvent, within } from 'storybook/test';
import ErrorModalProvider from '../../Primitive/ErrorModal/Provider.fs.js';
import { MemberCatalog_Items } from './MemberCatalog.fs.js';
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
        kind={MemberCatalog_Items[0].data}
      />
    </ErrorModalProvider>
  );
}

const meta = {
  title: 'Composite Components/ProcessCore/MetadataBrowser',
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

    await userEvent.click(canvas.getByRole('button', { name: 'Back to Child dataset' }));
    expect(canvas.getByRole('heading', { name: 'Dataset Metadata' })).toBeVisible();
    expect(canvas.getByRole('button', { name: 'Open Updated extraction process metadata' })).toBeVisible();
    expect(canvas.getByRole('button', { name: 'Open grandchild-dataset metadata' })).toBeVisible();

    await userEvent.click(canvas.getByRole('button', { name: 'Back to Datasets' }));
    expect(canvas.getByRole('heading', { name: 'Datasets' })).toBeVisible();
  },
};

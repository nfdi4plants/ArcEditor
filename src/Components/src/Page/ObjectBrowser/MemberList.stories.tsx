import type { Meta, StoryObj } from '@storybook/react-vite';
import React, { useState } from 'react';
import { expect, fireEvent, screen, userEvent, within } from 'storybook/test';
import MemberList from './MemberList.fs.js';
import { Items as memberCatalogItems } from './MemberCatalog.fs.js';
import { createProcessCoreArcFixture } from './ObjectBrowser.fixture.js';

const labels = [
  'Datasets',
  'Processes',
  'Samples',
  'Data',
  'Recipes',
  'Annotations',
  'DataContexts',
  'Agents',
  'Organizations',
  'ScholarlyArticles',
];

const MemberListExample = () => {
  const [arc, setArc] = useState(createProcessCoreArcFixture);
  const [selectedKind, setSelectedKind] = useState(memberCatalogItems[0].data);

  return (
    <>
      <MemberList
        arcStateCtx={{
          state: arc,
          setStateUpdater: update => setArc(current => update(current) ?? current),
        }}
        onSelect={setSelectedKind}
        selectedKind={selectedKind}
      />
      <span data-testid="selected-process-core-kind">{selectedKind.tag}</span>
    </>
  );
};

const meta = {
  title: 'Pages/ObjectBrowser/MemberList',
  component: MemberListExample,
  tags: ['autodocs'],
} satisfies Meta<typeof MemberListExample>;

export default meta;

type Story = StoryObj<typeof meta>;

export const Default: Story = {
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const rows = canvas.getAllByRole('row');

    expect(rows).toHaveLength(10);
    expect(rows[0]).toHaveAttribute('aria-selected', 'true');

    for (const [index, label] of labels.entries()) {
      expect(canvas.getByText(label)).toBeVisible();
      expect(rows[index].querySelector('i')).toHaveClass('swt:iconify-color');
      await userEvent.click(rows[index]);
      expect(rows[index]).toHaveAttribute('aria-selected', 'true');
      expect(canvas.getByTestId('selected-process-core-kind')).toHaveTextContent(String(index));
    }

    expect(canvas.getAllByRole('row')).toHaveLength(10);

    fireEvent.contextMenu(rows[1], { clientX: 40, clientY: 40 });
    expect(await screen.findByRole('button', { name: 'Add process' })).toBeVisible();
    await userEvent.click(screen.getByRole('button', { name: 'Delete process' }));
    const selectTrigger = await screen.findByRole('button', { name: 'Select an option' });
    await userEvent.click(selectTrigger.parentElement!);
    expect(await screen.findByRole('option', { name: /Extraction process/ })).toBeVisible();
    expect(screen.getByRole('option', { name: /Analysis process/ })).toBeVisible();
    await userEvent.click(screen.getByRole('button', { name: 'Cancel' }));

    fireEvent.contextMenu(canvas.getByRole('table'));
    expect(screen.queryByRole('button', { name: /Add process/ })).not.toBeInTheDocument();
    expect(screen.queryByRole('button', { name: /Delete process/ })).not.toBeInTheDocument();
  },
};

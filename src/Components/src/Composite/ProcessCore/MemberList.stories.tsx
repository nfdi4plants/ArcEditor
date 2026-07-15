import type { Meta, StoryObj } from '@storybook/react-vite';
import React, { useState } from 'react';
import {
  expect,
  fireEvent,
  screen,
  userEvent,
  within,
} from 'storybook/test';
import MemberList from './MemberList.fs.js';
import ErrorModalProvider from '../../Primitive/ErrorModal/Provider.fs.js';
import {
  MemberKind_Dataset,
  type MemberKind_$union,
} from './MemberCatalog.fs.js';
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
  const [selectedKind, setSelectedKind] = useState<MemberKind_$union>(
    MemberKind_Dataset(),
  );

  return (
    <ErrorModalProvider>
      <MemberList
        arcStateCtx={{
          state: arc,
          setStateUpdater: update => setArc(current => update(current) ?? current),
        }}
        onSelect={setSelectedKind}
        selectedKind={selectedKind}
      />
      <span data-testid="selected-process-core-kind">{selectedKind.tag}</span>
    </ErrorModalProvider>
  );
};

const meta = {
  title: 'Composite Components/ProcessCore/MemberList',
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
    await userEvent.click(await screen.findByRole('button', { name: 'Add process' }));
    await userEvent.type(screen.getByTestId('process-name'), 'Extraction process');
    await userEvent.click(screen.getByTestId('process-core-create'));

    expect(
      await screen.findByText("A process named 'Extraction process' already exists."),
    ).toBeVisible();
    expect(screen.getByTestId('process-name')).toBeVisible();
  },
};

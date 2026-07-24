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
  const [selectedEntity, setSelectedEntity] = useState('');

  return (
    <>
      <MemberList
        arcStateCtx={{
          state: arc,
          setStateUpdater: update => setArc(current => update(current) ?? current),
        }}
        onSelect={setSelectedKind}
        onSelectEntity={entity => {
          setSelectedKind(entity.memberKind);
          setSelectedEntity(entity.displayName);
        }}
        selectedKind={selectedKind}
      />
      <span data-testid="selected-process-core-kind">{selectedKind.tag}</span>
      <span data-testid="selected-process-core-entity">{selectedEntity}</span>
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

    expect(rows).toHaveLength(11);
    expect(rows[0]).toHaveAttribute('aria-selected', 'true');
    expect(rows[0]).toHaveAttribute('aria-expanded', 'true');
    const datasetChildren = within(canvas.getByTestId('dataset-folder-children'));
    const childDataset = datasetChildren.getByRole('button', { name: 'Child dataset' });
    expect(childDataset).toBeVisible();
    expect(childDataset).toHaveAttribute('aria-expanded', 'false');
    expect(datasetChildren.getByRole('button', { name: 'grandchild-dataset' })).toBeVisible();

    await userEvent.click(childDataset);
    expect(childDataset).toHaveAttribute('aria-expanded', 'true');
    const folderLabels = ['Has Part', 'Processes', 'Data Files', 'Agents', 'Citations', 'Data Contexts'];
    for (const folderLabel of folderLabels) {
      const collectionFolder = datasetChildren.getByRole('button', { name: folderLabel });
      expect(collectionFolder).toHaveAttribute('aria-expanded', 'false');
      expect(collectionFolder.querySelector('i.swt\\:iconify-color')).toBeVisible();
    }
    expect(datasetChildren.queryByRole('button', { name: 'Extraction process' })).not.toBeInTheDocument();

    const processesFolder = datasetChildren.getByRole('button', { name: 'Processes' });
    await userEvent.click(processesFolder);
    expect(processesFolder).toHaveAttribute('aria-expanded', 'true');
    const extractionProcess = datasetChildren.getByRole('button', { name: 'Extraction process' });
    expect(extractionProcess).toBeVisible();
    await userEvent.click(extractionProcess);
    expect(canvas.getByTestId('selected-process-core-kind')).toHaveTextContent('1');
    expect(canvas.getByTestId('selected-process-core-entity')).toHaveTextContent('Extraction process');

    const processBranch = within(extractionProcess.closest('[role="treeitem"]')!);
    const processCollections = ['Executes Protocol', 'Inputs', 'Outputs', 'Parameter Values'];
    for (const collectionLabel of processCollections) {
      expect(processBranch.getByRole('button', { name: collectionLabel })).toHaveAttribute(
        'aria-expanded',
        'false',
      );
    }

    const inputsFolder = processBranch.getByRole('button', { name: 'Inputs' });
    await userEvent.click(inputsFolder);
    expect(processBranch.getByRole('button', { name: 'Source sample' })).toBeVisible();

    const agentsFolder = datasetChildren.getByRole('button', { name: 'Agents' });
    await userEvent.click(agentsFolder);
    const datasetAgent = datasetChildren.getByRole('button', { name: 'Ada Lovelace' });
    await userEvent.click(datasetAgent);
    const agentBranch = within(datasetAgent.closest('[role="treeitem"]')!);
    const affiliationFolder = agentBranch.getByRole('button', { name: 'Affiliation' });
    await userEvent.click(affiliationFolder);
    const organization = agentBranch.getByRole('button', { name: 'Research organization' });
    await userEvent.click(organization);
    expect(canvas.getByTestId('selected-process-core-kind')).toHaveTextContent('8');
    expect(canvas.getByTestId('selected-process-core-entity')).toHaveTextContent('Research organization');

    for (const [index, label] of labels.entries()) {
      const row = rows[index + 1];
      expect(within(row).getByText(new RegExp(`^${label} \\(\\d+\\)$`))).toBeVisible();
      expect(row).toHaveAttribute('data-interactive-list-index', String(index));
      expect(row.querySelector('i.swt\\:iconify-color')).toBeVisible();
      await userEvent.click(row);
      expect(row).toHaveAttribute('aria-selected', 'true');
      expect(canvas.getByTestId('selected-process-core-kind')).toHaveTextContent(String(index));
    }

    expect(canvas.getAllByRole('row')).toHaveLength(11);

    fireEvent.contextMenu(rows[2], { clientX: 40, clientY: 40 });
    expect(await screen.findByRole('button', { name: 'Add process' })).toBeVisible();
    await userEvent.click(screen.getByRole('button', { name: 'Delete process' }));
    const selectTrigger = await screen.findByRole('button', { name: 'Select an option' });
    await userEvent.click(selectTrigger.parentElement!);
    expect(await screen.findByRole('option', { name: /Extraction process/ })).toBeVisible();
    expect(screen.getByRole('option', { name: /Analysis process/ })).toBeVisible();
    await userEvent.click(screen.getByRole('button', { name: 'Cancel' }));

    fireEvent.contextMenu(canvas.getAllByRole('table')[1]);
    expect(screen.queryByRole('button', { name: /Add process/ })).not.toBeInTheDocument();
    expect(screen.queryByRole('button', { name: /Delete process/ })).not.toBeInTheDocument();
  },
};

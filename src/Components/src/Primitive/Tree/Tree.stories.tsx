import type { Meta, StoryObj } from '@storybook/react-vite';
import { useState } from 'react';
import { expect, userEvent, within } from 'storybook/test';
import { Tree as TreeMain } from './Tree.fs.js';

const nodes = [
  {
    key: 'workspace',
    label: 'Workspace',
    icon: 'swt:iconify swt:fluent--folder-20-regular',
    data: 'Workspace',
    children: [
      {
        key: 'workspace/src',
        label: 'src',
        icon: 'swt:iconify swt:fluent--folder-20-regular',
        data: 'src',
        children: [
          {
            key: 'workspace/src/main',
            label: 'Main.fs',
            icon: 'swt:iconify swt:fluent--document-20-regular',
            data: 'Main.fs',
            children: [],
          },
        ],
      },
      {
        key: 'workspace/readme',
        label: 'README.md',
        icon: 'swt:iconify swt:fluent--document-20-regular',
        data: 'README.md',
        children: [],
      },
    ],
  },
];

const TreeExample = () => {
  const [selected, setSelected] = useState('None');
  const [expandedCount, setExpandedCount] = useState(0);

  return (
    <div className="swt:w-72 swt:space-y-3">
      <TreeMain
        nodes={nodes}
        onActivate={setSelected}
        onExpandedKeysChange={keys => setExpandedCount(keys.size)}
        testId="generic-tree"
      />
      <p>Selected: {selected}</p>
      <p>Expanded: {expandedCount}</p>
    </div>
  );
};

const meta = {
  title: 'Primitive Components/Tree',
  tags: ['autodocs'],
  parameters: {
    layout: 'centered',
    viewport: { defaultViewport: 'responsive' },
  },
  component: TreeExample,
} satisfies Meta<typeof TreeExample>;

export default meta;

type Story = StoryObj<typeof meta>;

export const Basic: Story = {
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    expect(canvas.queryByRole('button', { name: 'README.md' })).not.toBeInTheDocument();

    await userEvent.click(canvas.getByRole('button', { name: 'Expand Workspace' }));

    expect(canvas.getByRole('button', { name: 'README.md' })).toBeInTheDocument();
    expect(canvas.getByText('Expanded: 1')).toBeInTheDocument();

    const readme = canvas.getByRole('button', { name: 'README.md' });
    await userEvent.click(readme);

    expect(canvas.getByText('Selected: README.md')).toBeInTheDocument();
    expect(readme.closest('[role="treeitem"]')).toHaveAttribute('aria-selected', 'true');
    expect(readme.parentElement).toHaveClass('swt:bg-base-300');

    await userEvent.click(canvas.getByRole('button', { name: 'Collapse Workspace' }));
    expect(canvas.getByText('Expanded: 0')).toBeInTheDocument();
  },
};

import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, userEvent, within } from 'storybook/test';
import ProcessCoreMemberList from './ProcessCoreMemberList.fs.js';

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

const meta = {
  title: 'Composite Components/ProcessCore/ProcessCoreMemberList',
  component: ProcessCoreMemberList,
  tags: ['autodocs'],
} satisfies Meta<typeof ProcessCoreMemberList>;

export default meta;

type Story = StoryObj<typeof meta>;

export const Default: Story = {
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const rows = canvas.getAllByRole('row');

    expect(rows).toHaveLength(10);

    for (const [index, label] of labels.entries()) {
      expect(canvas.getByText(label)).toBeVisible();
      expect(rows[index].querySelector('i')).toHaveClass('swt:iconify-color');
      await userEvent.click(rows[index]);
    }

    expect(canvas.getAllByRole('row')).toHaveLength(10);
  },
};

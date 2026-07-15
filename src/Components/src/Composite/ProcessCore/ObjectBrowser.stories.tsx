import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, within } from 'storybook/test';
import { ARC } from '../../fable_modules/ProcessCore.Javascript.0.0.8/ARC.fs.js';
import ObjectBrowser from './ObjectBrowser.fs.js';
import { MemberCatalog_Items } from './MemberCatalog.fs.js';
import { createProcessCoreArcFixture } from './ObjectBrowser.fixture.js';

const sampleIcon = 'swt:iconify-color swt:fluent-color--molecule-20';

const meta = {
  title: 'Composite Components/ProcessCore/ObjectBrowser',
  component: ObjectBrowser,
  tags: ['autodocs'],
} satisfies Meta<typeof ObjectBrowser>;

export default meta;

type Story = StoryObj<typeof meta>;

export const Samples: Story = {
  args: {
    arc: createProcessCoreArcFixture(),
    kind: MemberCatalog_Items[2].data,
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const items = canvas.getAllByTestId('process-core-object-item');
    expect(canvas.getByRole('heading', { name: 'Samples' })).toBeVisible();
    expect(items).toHaveLength(2);
    expect(canvas.getByText('Source sample')).toBeVisible();
    expect(canvas.getByText('Result sample')).toBeVisible();

    for (const item of items) {
      expect(item.children[0]).toHaveClass(...sampleIcon.split(' '));
      expect(item.children[0].tagName).toBe('I');
      expect(item.children[1].tagName).toBe('SPAN');
    }
  },
};

export const Empty: Story = {
  args: {
    arc: new ARC('empty-arc', 'Empty ARC'),
    kind: MemberCatalog_Items[2].data,
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    expect(canvas.getByRole('status')).toHaveTextContent('No Samples available in this ARC.');
    expect(canvas.queryByTestId('process-core-object-list')).not.toBeInTheDocument();
  },
};

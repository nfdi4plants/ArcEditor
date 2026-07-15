import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fireEvent, screen, userEvent, waitFor, within } from 'storybook/test';
import { ARC } from '../../fable_modules/ProcessCore.Javascript.0.0.8/ARC.fs.js';
import ErrorModalProvider from '../../Primitive/ErrorModal/Provider.fs.js';
import ObjectBrowser from './ObjectBrowser.fs.js';
import { MemberCatalog_Items } from './MemberCatalog.fs.js';
import { createProcessCoreArcFixture } from './ObjectBrowser.fixture.js';

const sampleIcon = 'swt:iconify-color swt:fluent-color--molecule-20';
const samplesArc = createProcessCoreArcFixture();
const emptyArc = new ARC('empty-arc', 'Empty ARC');
const entityDeletionArc = createProcessCoreArcFixture();
const typeDeletionArc = createProcessCoreArcFixture();

const meta = {
  title: 'Composite Components/ProcessCore/ObjectBrowser',
  component: ObjectBrowser,
  render: args => (
    <ErrorModalProvider>
      <ObjectBrowser {...args} />
    </ErrorModalProvider>
  ),
  tags: ['autodocs'],
} satisfies Meta<typeof ObjectBrowser>;

export default meta;

type Story = StoryObj<typeof meta>;

export const Samples: Story = {
  args: {
    arcStateCtx: {
      state: samplesArc,
      setStateUpdater: update => void update(samplesArc),
    },
    kind: MemberCatalog_Items[2].data,
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const rows = canvas.getAllByRole('row');
    expect(canvas.getByRole('heading', { name: 'Samples' })).toBeVisible();
    expect(rows).toHaveLength(2);
    expect(canvas.getByText('Source sample')).toBeVisible();
    expect(canvas.getByText('Result sample')).toBeVisible();

    for (const row of rows) {
      expect(row.querySelector('i')).toHaveClass(...sampleIcon.split(' '));
    }

    await userEvent.click(rows[0]);
    expect(rows[0]).toHaveAttribute('aria-selected', 'true');
    expect(rows[1]).toHaveAttribute('aria-selected', 'false');

    fireEvent.contextMenu(canvas.getByTestId('process-core-object-browser'), {
      clientX: 40,
      clientY: 40,
    });
    expect(await screen.findByRole('button', { name: 'Delete sample' })).toBeVisible();
    await userEvent.click(await screen.findByRole('button', { name: 'Add sample' }));
    await userEvent.type(screen.getByTestId('sample-name'), 'Source sample');
    await userEvent.click(screen.getByTestId('process-core-create'));

    await waitFor(() =>
      expect(
        screen.getByText("A sample named 'Source sample' already exists."),
      ).toBeVisible(),
    );
    expect(screen.getByTestId('sample-name')).toBeInTheDocument();
  },
};

export const Empty: Story = {
  args: {
    arcStateCtx: {
      state: emptyArc,
      setStateUpdater: update => void update(emptyArc),
    },
    kind: MemberCatalog_Items[2].data,
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    expect(canvas.getByRole('status')).toHaveTextContent('No Samples available in this ARC.');
    expect(canvas.queryByTestId('process-core-object-list')).not.toBeInTheDocument();

    fireEvent.contextMenu(canvas.getByTestId('process-core-object-browser'));
    expect(await screen.findByRole('button', { name: 'Add sample' })).toBeVisible();
    await userEvent.click(screen.getByRole('button', { name: 'Delete sample' }));
    await waitFor(() => expect(screen.getByTestId('process-core-delete-empty')).toBeVisible());
    expect(screen.getByTestId('process-core-delete-selected')).toBeDisabled();
    await userEvent.click(screen.getByRole('button', { name: 'Cancel' }));
  },
};

export const EntityDeletion: Story = {
  args: {
    arcStateCtx: {
      state: entityDeletionArc,
      setStateUpdater: update => void update(entityDeletionArc),
    },
    kind: MemberCatalog_Items[2].data,
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const sourceRow = canvas.getByRole('row', { name: /Source sample/ });

    fireEvent.contextMenu(sourceRow);
    expect(await screen.findByRole('button', { name: 'Delete sample' })).toBeVisible();
    expect(screen.queryByRole('button', { name: 'Add sample' })).not.toBeInTheDocument();
    await userEvent.click(screen.getByRole('button', { name: 'Delete sample' }));

    await waitFor(() =>
      expect(screen.getByText('Shall ‘Source sample’ really be deleted?')).toBeVisible(),
    );
    await userEvent.click(screen.getByRole('button', { name: 'Cancel' }));
    expect(canvas.getByText('Source sample')).toBeVisible();

    fireEvent.contextMenu(sourceRow);
    await userEvent.click(await screen.findByRole('button', { name: 'Delete sample' }));
    await userEvent.click(screen.getByTestId('process-core-delete-entity'));

    expect(canvas.queryByText('Source sample')).not.toBeInTheDocument();
    expect(canvas.getByText('Result sample')).toBeVisible();
  },
};

export const TypeDeletionWithSelectAll: Story = {
  args: {
    arcStateCtx: {
      state: typeDeletionArc,
      setStateUpdater: update => void update(typeDeletionArc),
    },
    kind: MemberCatalog_Items[2].data,
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    fireEvent.contextMenu(canvas.getByTestId('process-core-object-browser'));
    expect(await screen.findByRole('button', { name: 'Add sample' })).toBeVisible();
    await userEvent.click(screen.getByRole('button', { name: 'Delete sample' }));
    const selectTrigger = await screen.findByRole('button', { name: 'Select an option' });
    await userEvent.click(selectTrigger.parentElement!);

    expect(await screen.findByRole('option', { name: /Source sample/ })).toBeVisible();
    expect(screen.getByRole('option', { name: /Result sample/ })).toBeVisible();
    await userEvent.click(screen.getByRole('option', { name: /Select all/ }));
    await userEvent.click(screen.getByTestId('process-core-delete-selected'));

    expect(await canvas.findByRole('status')).toHaveTextContent('No Samples available in this ARC.');
  },
};

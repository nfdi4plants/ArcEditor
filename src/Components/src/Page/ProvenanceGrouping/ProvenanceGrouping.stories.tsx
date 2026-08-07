import React from 'react';
import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fireEvent, screen, userEvent, waitFor, within } from 'storybook/test';
import { Main as ProvenanceGrouping } from './ProvenanceGrouping.fs.js';
import { sampleDroppedPropertyRailColor } from './Helper.fs.js';
import {
  createSampleSession,
  createInputOnlySession,
  createOutputOnlySession,
  createDisconnectedPropertySession,
  createSwitchablePropertySession,
  createTypedSampleSession,
  createDataOutputOnlySession,
  createRetaggedTypedSampleSession,
  createChainedSession,
  createLayerOrderSession,
  createAmbiguousProcessAssignmentSession,
  createChainedAlternateAnalysisSession,
  createReverseLocalSession,
  createAllLinkShapesSession,
  createFanOutSession,
  createReferenceCatalogSession,
  createPerformanceSession,
  sampleAndDataEndpointKinds,
  JournalPreview_journalDetails as mutationJournal,
} from './StoryFixtures.fs.js';

type Fixture =
  | 'sample'
  | 'inputOnly'
  | 'outputOnly'
  | 'disconnectedProperty'
  | 'switchableProperty'
  | 'typedSample'
  | 'dataOutputOnly'
  | 'chained'
  | 'layerOrder'
  | 'ambiguousProcessAssignment'
  | 'chainedAlternateAnalysis'
  | 'reverseLocal'
  | 'allLinkShapes'
  | 'fanOut'
  | 'referenceCatalog'
  | 'performance';

// Step L.1's repaint-half workload: small enough to render in a browser test
// within a reasonable time, while still exercising many more nodes and links
// than every other fixture in this file. See
// StoryFixtures.createPerformanceSession for what these three numbers mean.
const perfLayers = 2;
const perfNodesPerSide = 30;
const perfEdgeDensity = 0.15;

type Side = 'Input' | 'Output';

function processAssignmentLinkCount(preview: HTMLElement) {
  return (preview.textContent ?? '')
    .split('\n')
    .filter((line) => line.startsWith('ProcessAssignmentAdded:'))
    .reduce((count, line) => {
      const links = line.split(':links=')[1] ?? '';
      return count + links.split(',').filter(Boolean).length;
    }, 0);
}

function createSessionForFixture(selected: Fixture) {
  switch (selected) {
    case 'chained':
      return createChainedSession();
    case 'layerOrder':
      return createLayerOrderSession();
    case 'ambiguousProcessAssignment':
      return createAmbiguousProcessAssignmentSession();
    case 'chainedAlternateAnalysis':
      return createChainedAlternateAnalysisSession();
    case 'reverseLocal':
      return createReverseLocalSession();
    case 'allLinkShapes':
      return createAllLinkShapesSession();
    case 'fanOut':
      return createFanOutSession();
    case 'referenceCatalog':
      return createReferenceCatalogSession()[0];
    case 'performance':
      return createPerformanceSession(perfLayers, perfNodesPerSide, perfEdgeDensity);
    case 'inputOnly':
      return createInputOnlySession();
    case 'outputOnly':
      return createOutputOnlySession();
    case 'disconnectedProperty':
      return createDisconnectedPropertySession();
    case 'switchableProperty':
      return createSwitchablePropertySession();
    case 'typedSample':
      return createTypedSampleSession();
    case 'dataOutputOnly':
      return createDataOutputOnlySession();
    default:
      return createSampleSession();
  }
}

// `createReferenceCatalogSession` is the only fixture pairing a session with a
// host-controlled ReferenceCatalog; every other fixture has none.
function referenceCatalogForFixture(selected: Fixture) {
  return selected === 'referenceCatalog' ? createReferenceCatalogSession()[1] : undefined;
}

function Harness({
  inputOnly = false,
  outputOnly = false,
  fixture = 'sample',
  debug = true,
  allowTermReplacement = false,
  allowEndpointReplacement = false,
  endpointKinds,
}: {
  inputOnly?: boolean;
  outputOnly?: boolean;
  fixture?: Fixture;
  debug?: boolean;
  allowTermReplacement?: boolean;
  allowEndpointReplacement?: boolean;
  endpointKinds?: unknown;
}) {
  const selected = inputOnly ? 'inputOnly' : outputOnly ? 'outputOnly' : fixture;
  const id = React.useId();

  return (
    <HarnessState
      key={`${selected}:${id}`}
      selected={selected}
      debug={debug}
      allowTermReplacement={allowTermReplacement}
      allowEndpointReplacement={allowEndpointReplacement}
      endpointKinds={endpointKinds}
    />
  );
}

function HarnessState({
  selected,
  debug,
  allowTermReplacement,
  allowEndpointReplacement,
  endpointKinds,
}: {
  selected: Fixture;
  debug: boolean;
  allowTermReplacement: boolean;
  allowEndpointReplacement: boolean;
  endpointKinds?: unknown;
}) {
  const [session, setSession] = React.useState(() => createSessionForFixture(selected));
  const referenceCatalog = React.useMemo(() => referenceCatalogForFixture(selected), [selected]);

  React.useEffect(() => {
    setSession(createSessionForFixture(selected));
  }, [selected]);

  // The session's own MutationJournal is the authoritative unsaved-change
  // record - reading it directly (instead of accumulating each change's delta
  // host-side) means undo retracts already-recorded mutations for free, since
  // undo restores a prior session snapshot complete with its own (shorter)
  // journal.
  const mutations = Array.from(mutationJournal(session));

  return (
    <div className="swt:flex swt:flex-col swt:gap-4 swt:min-h-screen swt:bg-base-200 swt:p-4">
      {allowTermReplacement && (
        <button type="button" onClick={() => setSession(createRetaggedTypedSampleSession())}>
          Replace term metadata
        </button>
      )}
      {allowEndpointReplacement && (
        <button type="button" onClick={() => setSession(createOutputOnlySession())}>
          Replace endpoint context
        </button>
      )}
      <ProvenanceGrouping
        session={session}
        height={960}
        debug={debug}
        endpointKinds={endpointKinds}
        referenceCatalog={referenceCatalog}
        onChange={(change: any) => {
          setSession(change.Session);
        }}
      />
      <section className="swt:rounded-box swt:border swt:border-base-300 swt:bg-base-100 swt:p-4">
        <h3 className="swt:text-primary swt:font-semibold">Writeback mutation preview</h3>
        <pre data-testid="provenance-mutation-preview" className="swt:text-xs swt:whitespace-pre-wrap">
          {mutations.length === 0 ? 'No mutations recorded.' : mutations.join('\n')}
        </pre>
      </section>
    </div>
  );
}

const meta = {
  title: 'Page Components/ProvenanceGrouping',
  component: ProvenanceGrouping,
  tags: ['autodocs'],
  parameters: { layout: 'fullscreen', isolated: true },
} satisfies Meta<typeof ProvenanceGrouping>;

export default meta;
type Story = StoryObj<typeof meta>;

export const ExampleModel: Story = {
  render: () => <Harness />,
};

export const InputOnlyModel: Story = {
  render: () => <Harness inputOnly />,
};

export const OutputOnlyModel: Story = {
  render: () => <Harness outputOnly />,
};

export const GroupsByPropertiesAndShowsMembers: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await groupByProperty(canvasElement, 'Output', 'Species');
    const grouped = await waitFor(() => getGroupCard(canvasElement, 'Output', 'Species: Arabidopsis'));
    expect(getGroupCard(canvasElement, 'Output', 'Species: Chlamydomonas')).toBeInTheDocument();

    // The grouping shows as an organizer tab "Category: Value" on top of the member folder.
    const tab = groupCardTab(grouped, 'Species: Arabidopsis');
    expect(tab).toHaveTextContent('Species: Arabidopsis');

    await userEvent.click(within(grouped).getByRole('button', { name: 'Show members' }));
    await waitFor(() => expect(grouped).toHaveTextContent('Output A'));
  },
};

export const ExpandedGroupsShowMemberHoverValues: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // Single-entry cards share the folder silhouette, so they expand the same way.
    expect(within(getGroupCard(canvasElement, 'Output', 'Output A')).getByRole('button', { name: 'Show members' }))
      .toBeInTheDocument();

    await groupByProperty(canvasElement, 'Output', 'Species');
    const grouped = await waitFor(() => getGroupCard(canvasElement, 'Output', 'Species: Arabidopsis'));

    await userEvent.click(within(grouped).getByRole('button', { name: 'Show members' }));
    const member = within(grouped).getByTestId('provenance-group-member-Output-node-output-a');

    expect(within(grouped).queryByTestId('provenance-member-values-Output-node-output-a')).not.toBeInTheDocument();
    await userEvent.hover(member);

    await waitFor(() => {
      const details = within(grouped).getByTestId('provenance-member-values-Output-node-output-a');
      expect(details).toHaveTextContent('Species: Arabidopsis');
      expect(details).toHaveTextContent('Analysis: Mass Spectrometry');
    });

    await userEvent.unhover(member);
  },
};

export const ShowsEntityTypesAndCollapsedSymbols: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await groupByProperty(canvasElement, 'Output', 'Species');
    const grouped = await waitFor(() => getGroupCard(canvasElement, 'Output', 'Species: Arabidopsis'));

    // The collapsed card previews its member types as symbols instead of a bare "×3" count.
    expect(groupCardSymbols(grouped)).toBeInTheDocument();
    expect(grouped).not.toHaveTextContent('×3');

    // Expanding shows each member with its endpoint type ("Sample") above the name.
    await userEvent.click(within(grouped).getByRole('button', { name: 'Show members' }));
    const member = within(grouped).getByTestId('provenance-group-member-Output-node-output-a');
    expect(member).toHaveTextContent('Sample');
    expect(member).toHaveTextContent('Output A');
  },
};

export const HoveringGroupTabHighlightsItAndKeepsFolderPreview: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await groupByProperty(canvasElement, 'Output', 'Species');
    const grouped = await waitFor(() => getGroupCard(canvasElement, 'Output', 'Species: Arabidopsis'));
    const tab = groupCardTab(grouped, 'Species: Arabidopsis');

    expect(tab).toHaveAttribute('data-hovered', 'false');

    // Hovering the tab highlights it and the folder previews that tab's members.
    await userEvent.hover(tab);
    await waitFor(() => expect(tab).toHaveAttribute('data-hovered', 'true'));
    expect(groupCardSymbols(grouped)).toBeInTheDocument();

    await userEvent.unhover(tab);
    await waitFor(() => expect(tab).toHaveAttribute('data-hovered', 'false'));
  },
};

export const ShowsFileTypeForDataEndpoints: Story = {
  render: () => <Harness fixture="dataOutputOnly" />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // A Data endpoint shows its type as a document symbol in the folder body;
    // the "File" type line appears on the expanded member row.
    const card = await waitFor(() => canvas.getByText('Data Output A').closest('article')!);
    expect(card.querySelector('[class*="fluent--document"]')).toBeInTheDocument();

    await userEvent.click(within(card).getByRole('button', { name: 'Show members' }));
    await waitFor(() => expect(card).toHaveTextContent('File'));
  },
};

export const GroupCardsSelectWithCheckboxAndExpandFromSurface: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const outputA = canvas.getByText('Output A').closest('article')!;

    // Selection is an explicit checkbox; a selection bar with a clear action
    // appears while any group is selected.
    await userEvent.click(within(outputA).getByRole('checkbox'));
    await waitFor(() => expect(outputA).toHaveClass('swt:border-primary'));
    expect(canvas.getByTestId('provenance-selection-bar')).toHaveTextContent('1 group selected');

    await userEvent.click(canvas.getByTestId('provenance-clear-selection'));
    await waitFor(() => {
      expect(outputA).not.toHaveClass('swt:border-primary');
      expect(canvas.queryByTestId('provenance-selection-bar')).not.toBeInTheDocument();
    });

    // Clicking the card body expands the members instead of selecting.
    const expandSurface = outputA.querySelector<HTMLElement>('[data-testid^="provenance-group-expand-surface-"]')!;
    await userEvent.click(expandSurface);
    await waitFor(() =>
      expect(within(outputA).getByTestId('provenance-group-member-Output-node-output-a')).toBeInTheDocument(),
    );
    expect(outputA).not.toHaveClass('swt:border-primary');
  },
};

export const GroupsBothSidesFromOutputProperty: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // "Both" applies one grouping header to both sides at once; each side then
    // keys its own items on the values *that side* actually holds.
    for (
      let attempt = 0;
      attempt < 3 && !queryGroupCard(canvasElement, 'Output', 'Replicate: 1, Replicate: 2');
      attempt += 1
    ) {
      await showPropertyControls(canvas, 'Output', 'Replicate');
      fireEvent.click(canvas.getByTestId('provenance-property-both-Output-Replicate'));
      await waitFor(() => expect(queryGroupCard(canvasElement, 'Output', 'Replicate: 1, Replicate: 2')).toBeInTheDocument(), {
        timeout: 1000,
      }).catch(() => undefined);
    }

    await waitFor(() => {
      // Output B is incident to both replicate links, so it is keyed on both
      // values (intent §7's normalized "A, B" key).
      expect(getGroupCard(canvasElement, 'Output', 'Replicate: 1, Replicate: 2')).toBeInTheDocument();
      // Each input is incident to one of them, so each keys on its own value.
      // The old model merged them through symmetric, transitive same-layer
      // inheritance - the defect intent §14 removes - so this expectation
      // changes with the model rather than the story losing coverage.
      expect(getGroupCard(canvasElement, 'Input', 'Replicate: 1')).toBeInTheDocument();
      expect(getGroupCard(canvasElement, 'Input', 'Replicate: 2')).toBeInTheDocument();
      // Inputs C and D touch no replicate link, so they keep item-specific
      // fallback keys instead of collapsing into one missing-value group.
      expect(getGroupCard(canvasElement, 'Input', 'Input C')).toBeInTheDocument();
      expect(getGroupCard(canvasElement, 'Input', 'Input D')).toBeInTheDocument();
    }, { timeout: 6000 });
  },
};

export const MissingSecondGroupingKeyKeepsAvailableGroupingKeys: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await groupByProperty(canvasElement, 'Input', 'Species');
    await groupByProperty(canvasElement, 'Input', 'Temperature');

    await waitFor(() => {
      expect(getGroupCard(canvasElement, 'Input', 'Species: Arabidopsis, Temperature: 12 C'))
        .toBeInTheDocument();
      expect(getGroupCard(canvasElement, 'Input', 'Species: Arabidopsis, Temperature: 24 C'))
        .toBeInTheDocument();
      expect(getGroupCard(canvasElement, 'Input', 'Species: Chlamydomonas')).toBeInTheDocument();
      expect(queryGroupCard(canvasElement, 'Input', 'Input D')).not.toBeInTheDocument();
    });
  },
};

export const ConnectedOutputsKeepPropertiesInRailsAndConnections: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const outputA = canvas.getByText('Output A').closest('article')!;

    expect(outputA).toHaveTextContent('Output A');
    expect(outputA).not.toHaveTextContent('Analysis: Mass Spectrometry');
    expect(outputA).not.toHaveTextContent('Species: Arabidopsis');
    expect(outputA).not.toHaveTextContent('Temperature: 12 C');
    expect(canvas.getAllByTestId('provenance-connection').length).toBeGreaterThan(0);
  },
};

export const PropertiesStartInOriginFoldersAndSideDropZonesAreEmpty: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    expect(canvas.getByTestId('foldered-draggable-list')).toBeInTheDocument();
    expect(canvas.getByTestId('foldered-draggable-folder-source-fixture-assay-table')).toBeInTheDocument();
    expect(canvas.getByTestId('provenance-property-rail-Input').querySelector('[data-testid^="provenance-property-Input-"]'))
      .not.toBeInTheDocument();
    expect(canvas.getByTestId('provenance-property-rail-Output').querySelector('[data-testid^="provenance-property-Output-"]'))
      .not.toBeInTheDocument();
    // waitFor: the shelf pops in with a brief opacity animation on mount, and
    // toBeVisible treats the first opacity-0 frame as hidden.
    await waitFor(() =>
      expect(within(canvas.getByTestId('foldered-draggable-item-row')).getAllByRole('button', { name: /^Drag Species$/ })[0])
        .toBeVisible());

    await userEvent.click(canvas.getByRole('button', { name: 'Minimize annotation folders' }));
    await waitFor(() => expect(canvas.queryByTestId('foldered-draggable-list')).not.toBeInTheDocument());

    await userEvent.click(canvas.getByRole('button', { name: 'Expand annotation folders' }));
    await waitFor(() => expect(canvas.getByTestId('foldered-draggable-list')).toBeInTheDocument());
    await waitFor(() =>
      expect(within(canvas.getByTestId('foldered-draggable-item-row')).getAllByRole('button', { name: /^Drag Species$/ })[0])
        .toBeVisible());

    const species = await shelfProperty(canvas, 'Species');
    expect(species).toBeInTheDocument();
    expect(species).toHaveTextContent('Species');
  },
};

export const DroppedShelfPropertyLeavesFolders: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await ensurePropertyInRail(canvas, 'Output', 'Species');

    expect(canvas.getByTestId('provenance-property-Output-Species')).toBeInTheDocument();
    const currentLayerShelf = await openShelfFolder(
      canvas,
      canvas.getByTestId('foldered-draggable-folder-source-fixture-assay-table'),
    );
    expect(currentLayerShelf.queryAllByRole('button', { name: /^Drag Species$/ })).toHaveLength(0);
  },
};

export const DroppedShelfPropertyKeepsLayerColorAndSyncsUpdates: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const initialLayerColor = canvas.getByTestId('provenance-layer-layer-1').getAttribute('data-provenance-layer-color') ?? '';

    const property = await ensurePropertyInRail(canvas, 'Output', 'Species');
    expect(propertyColorSwatch(property)).toHaveStyle({ backgroundColor: initialLayerColor });

    expect(sampleDroppedPropertyRailColor('Output', 'Species', '#dc2626')).toBe('#dc2626');
  },
};

export const FolderColorPreviewSyncsLayerTabAndRailProperty: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await setFolderPreviewColor(canvas, canvas.getByTestId('foldered-draggable-folder-source-fixture-assay-table'), '#be185d');

    await waitFor(() => {
      expect(canvas.getByTestId('provenance-layer-layer-1')).toHaveAttribute(
        'data-provenance-layer-color',
        '#be185d',
      );
    });

    const property = await ensurePropertyInRail(canvas, 'Output', 'Species');
    expect(propertyColorSwatch(property)).toHaveStyle({ backgroundColor: '#be185d' });
  },
};

// Re-pointed at the chained fixture: a node does not belong to a layer, it
// *appears* in layers, and a shelf entry's colour resolves from its owning
// node's appearance sources. The sample fixture is one layer, so it cannot
// express a non-layer source; `Culture Batch` can, because it is the boundary
// node of both chained layers and owns `Batch Origin`. Viewed from the
// measurement layer, that entry resolves to {growth, measurement} - a union
// containing a source that is not the viewing layer's, which is the behaviour
// this story tests.
export const NonLayerFolderColorAppliesToShelfAndRailProperties: Story = {
  render: () => <Harness fixture="chained" />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const growthFolder = canvas.getByTestId('foldered-draggable-folder-source-fixture-growth-table');

    await setFolderPreviewColor(canvas, growthFolder, '#0891b2');

    const growthShelf = await openShelfFolder(canvas, growthFolder);
    const shelfPropertyButton = growthShelf.getByRole('button', { name: /^Drag Batch Origin$/ });
    const shelfSwatch = shelfPropertyButton.querySelector<HTMLElement>('[data-foldered-color-swatch="true"]');

    expect(shelfSwatch).not.toBeNull();
    expect(shelfSwatch!).toHaveStyle({ backgroundColor: '#0891b2' });

    const property = await ensurePropertyInRail(canvas, 'Input', 'Batch Origin');
    expect(propertyColorSwatch(property)).toHaveStyle({ backgroundColor: '#0891b2' });
  },
};

export const RejectedShelfPropertyDropRestoresFolderItem: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const source = await shelfProperty(canvas, 'Species');
    const target = canvas.getByText('Input A').closest('article')!;

    await dragByPointer(source, target);

    await waitFor(() => {
      expect(canvas.queryByTestId('foldered-draggable-drag-overlay')).not.toBeInTheDocument();
      expect(within(canvas.getByTestId('foldered-draggable-item-row')).getAllByRole('button', { name: /^Drag Species$/ })[0])
        .toBeVisible();
    });
  },
};

export const SingleSidedShelfPropertiesCannotDropOnOppositeSide: Story = {
  render: () => <Harness fixture="disconnectedProperty" />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const source = await shelfProperty(canvas, 'Analysis');
    const inputRail = canvas.getByTestId('provenance-property-rail-Input');

    const pointer = await startDragByPointer(source);
    await moveDragPointerTo(inputRail, pointer.pointerId);
    await waitFor(() => {
      expect(inputRail).toHaveAttribute('data-provenance-drop-state', 'rejecting');
      expect(inputRail).toHaveClass('swt:border-warning');
    });

    fireEvent.pointerUp(inputRail, {
      clientX: pointer.x,
      clientY: pointer.y,
      button: 0,
      buttons: 0,
      isPrimary: true,
      pointerId: pointer.pointerId,
    });
    await waitFor(() => expect(canvas.queryByTestId('foldered-draggable-drag-overlay')).not.toBeInTheDocument());

    expect(canvas.queryByTestId('provenance-property-Input-Analysis')).not.toBeInTheDocument();
    await waitFor(() => {
      expect(within(canvas.getByTestId('foldered-draggable-item-row')).getByRole('button', { name: /^Drag Analysis$/ }))
        .toBeVisible();
    });
  },
};

export const HelpLegendExplainsWorkflowAndSymbols: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await userEvent.click(canvas.getByTestId('provenance-help-trigger'));
    const content = await waitFor(() => within(document.body).getByTestId('provenance-help-content'));

    expect(content).toHaveTextContent('Group');
    expect(content).toHaveTextContent('Annotate');
    expect(content).toHaveTextContent('Connect');
    expect(content).toHaveTextContent('Continue');
    expect(content).toHaveTextContent(/upstream table/i);
    await userEvent.keyboard('{Escape}');
  },
};

export const ToolbarUsesSinglePropertySortAndOriginButtons: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const toolbar = within(canvas.getByTestId('provenance-filter-toolbar'));

    expect(toolbar.getByPlaceholderText('Search annotations & values...')).toBeInTheDocument();

    await userEvent.click(toolbar.getByRole('button', { name: /^Sort By$/i }));
    expect(toolbar.getByRole('button', { name: /^Annotation Value Count$/i })).toBeInTheDocument();
    expect(toolbar.getByRole('button', { name: /^Name$/i })).toBeInTheDocument();
    expect(toolbar.getAllByRole('button', { name: /^Connection Count$/i })).toHaveLength(1);

    expect(toolbar.getByRole('button', { name: /^Show upstream annotations$/i }).querySelector('[class*="fluent--arrow-up-20"]'))
      .toBeInTheDocument();
    expect(toolbar.getByRole('button', { name: /^Show current annotations$/i }).querySelector('[class*="fluent--circle-20-filled"]'))
      .toBeInTheDocument();
    const both = toolbar.getByRole('button', { name: /^Show current and upstream annotations$/i });
    expect(both.querySelector('[class*="fluent--arrow-up-20"]')).toBeInTheDocument();
    expect(both.querySelector('[class*="fluent--circle-20-filled"]')).toBeInTheDocument();
  },
};

export const TopControlsShareOneRowWhenSpaceAllows: Story = {
  // The controls row wraps by design (flex-wrap) once it runs out of width, so
  // this story must guarantee the ample width its name promises. At the default
  // 1280px browser viewport the row sits right at the edge - it fits under one
  // platform's font metrics and wraps under another's (Windows passes, Linux CI
  // wraps by a row), which is what made this test flaky. A fixed wide wrapper
  // pins the layout well clear of that edge so the single-row assertion is
  // deterministic regardless of the runner's font rendering.
  render: () => (
    <div style={{ width: 1600 }}>
      <Harness />
    </div>
  ),
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const topControls = canvas.getByTestId('provenance-top-controls');
    const toolbar = canvas.getByTestId('provenance-filter-toolbar');
    const search = canvas.getByTestId('provenance-search');
    const viewActions = canvas.getByTestId('provenance-view-actions');
    const valueFilter = canvas.getByRole('combobox', { name: 'Filter by annotation value count' });
    const originFilter = canvas.getByRole('button', { name: /^Show upstream annotations$/i });

    const rowTop = (element: HTMLElement) => Math.round(element.getBoundingClientRect().top);
    const rowCenter = (element: HTMLElement) => {
      const rect = element.getBoundingClientRect();
      return rect.top + rect.height / 2;
    };

    expect(topControls).toContainElement(toolbar);
    expect(topControls).toContainElement(viewActions);
    expect(rowTop(toolbar)).toBe(rowTop(search));
    expect(rowTop(search)).toBe(rowTop(valueFilter));
    expect(rowTop(search)).toBe(rowTop(originFilter));
    // The view actions are deliberately smaller (btn-xs) than the toolbar
    // controls, so on the shared items-center row their tops differ while the
    // vertical centers align; a wrap onto a second row would offset the
    // center by a full row height.
    expect(Math.abs(rowCenter(toolbar) - rowCenter(viewActions))).toBeLessThanOrEqual(1);
  },
};

export const SearchInputUpdatesImmediatelyButFiltersAfterDebounce: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const toolbar = within(canvas.getByTestId('provenance-filter-toolbar'));
    const search = toolbar.getByPlaceholderText('Search annotations & values...') as HTMLInputElement;

    await ensurePropertyInRail(canvas, 'Output', 'Species');
    await ensurePropertyInRail(canvas, 'Output', 'Analysis');

    const outputRail = within(canvas.getByTestId('provenance-property-rail-Output'));

    expect(outputRail.getByTestId('provenance-property-Output-Species')).toBeInTheDocument();
    expect(outputRail.getByTestId('provenance-property-Output-Analysis')).toBeInTheDocument();

    await userEvent.type(search, 'mass');

    expect(search).toHaveValue('mass');
    expect(outputRail.getByTestId('provenance-property-Output-Species')).toBeInTheDocument();

    await waitFor(() => {
      expect(outputRail.queryByTestId('provenance-property-Output-Species')).not.toBeInTheDocument();
      expect(outputRail.getByTestId('provenance-property-Output-Analysis')).toBeInTheDocument();
    }, { timeout: 1200 });
  },
};

export const SortsPropertiesByNameAndConnectionCount: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const toolbar = within(canvas.getByTestId('provenance-filter-toolbar'));

    await userEvent.click(toolbar.getByRole('button', { name: /^Sort By$/i }));
    await userEvent.click(toolbar.getByRole('button', { name: /^Name$/i }));

    await waitFor(async () => {
      expect((await shelfPropertyOrder(canvas)).slice(0, 5)).toEqual([
        'Analysis',
        'Previous Treatment',
        'Replicate',
        'Species',
        'Temperature',
      ]);
    });

    await userEvent.click(toolbar.getByRole('button', { name: /^Sort By$/i }));
    await userEvent.click(toolbar.getByRole('button', { name: /^Connection Count$/i }));

    await waitFor(async () => {
      expect((await shelfPropertyOrder(canvas)).slice(0, 5)).toEqual([
        'Species',
        'Analysis',
        'Temperature',
        'Previous Treatment',
        'Replicate',
      ]);
    });
  },
};

export const SortsGroupsByMemberCount: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await groupByProperty(canvasElement, 'Output', 'Species');
    await waitFor(() =>
      expect(getGroupCard(canvasElement, 'Output', 'Species: Arabidopsis')).toBeInTheDocument(),
    );

    const toolbar = within(canvas.getByTestId('provenance-filter-toolbar'));
    await userEvent.click(toolbar.getByRole('button', { name: /^Sort Groups$/i }));
    await userEvent.click(toolbar.getByRole('button', { name: /^Member Count$/i }));

    await waitFor(() => {
      expect(groupCardTitles(canvasElement, 'Output')[0]).toBe('Species: Arabidopsis');
    });
  },
};

export const AddedRailPropertiesAreCurrentAndPinnedToTheirSide: Story = {
  render: () => <Harness inputOnly />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const inputRail = within(canvas.getByTestId('provenance-property-rail-Input'));
    const outputRail = within(canvas.getByTestId('provenance-property-rail-Output'));

    const source = await addRailProperty(canvas, 'Input', 'Treatment', 'Drought');
    expect(inputRail.getByTestId('provenance-property-Input-Treatment')).toBeInTheDocument();
    expect(within(inputRail.getByTestId('provenance-property-Input-Treatment')).getByTitle('Current')).toBeInTheDocument();
    expect(outputRail.queryByTestId('provenance-property-Output-Treatment')).not.toBeInTheDocument();

    await userEvent.click(within(canvas.getByTestId('provenance-filter-toolbar')).getByRole('button', { name: /^Show current annotations$/i }));
    await waitFor(() => expect(inputRail.getByTestId('provenance-property-Input-Treatment')).toBeInTheDocument());
    expect(outputRail.queryByTestId('provenance-property-Output-Treatment')).not.toBeInTheDocument();

    const target = canvas.getByText('Input Only A').closest('article')!;
    await dragByPointer(source, target);

    await waitFor(() =>
      expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('NodeAssignmentAdded'),
    );
  },
};

export const LayerFocusDoesNotResortInitializedRails: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const initialOutputOrder = (await shelfPropertyOrder(canvas)).slice(0, 4);

    await selectGroup(canvas.getByText('Output A').closest('article')!);
    await createLayer(canvas, 'Layer 2');
    await waitFor(() => expect(canvas.getByTestId('provenance-layer-layer-2')).toHaveClass('swt:btn-primary'));

    const toolbar = within(canvas.getByTestId('provenance-filter-toolbar'));
    await userEvent.click(toolbar.getByRole('button', { name: /^Sort By$/i }));
    await userEvent.click(toolbar.getByRole('button', { name: /^Connection Count$/i }));

    await userEvent.click(canvas.getByTestId('provenance-layer-layer-1'));
    await waitFor(() => expect(canvas.getByTestId('provenance-layer-layer-1')).toHaveClass('swt:btn-primary'));
    expect((await shelfPropertyOrder(canvas)).slice(0, 4)).toEqual(initialOutputOrder);
  },
};

export const PropertyRailExpandsValuesAndAddControls: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const outputRail = within(canvas.getByTestId('provenance-property-rail-Output'));

    expect(outputRail.queryByText('Arabidopsis')).not.toBeInTheDocument();
    expect(outputRail.getByText('Add annotation')).toBeInTheDocument();

    const panel = await expandProperty(canvas, 'Output', 'Species');
    const arabidopsis = panel.getAllByText('Arabidopsis')[0].closest('button, div')!;
    expect(arabidopsis).toBeInTheDocument();
    expect(arabidopsis).toHaveClass('swt:btn');
    // Outline, not primary: value chips share the ungrouped header button look so
    // they stay distinguishable from their header, which turns primary when grouped.
    expect(arabidopsis).toHaveClass('swt:btn-outline');
    expect(arabidopsis).toHaveClass('swt:w-fit');
    expect(arabidopsis).toHaveClass('swt:cursor-grab');
    expect(arabidopsis.querySelector('[class*="re-order-dots"]')).not.toBeInTheDocument();
    expect(panel.getByText('Chlamydomonas')).toBeInTheDocument();
    const addValue = panel.getByText('Add value').closest('button')!;
    expect(addValue).toHaveClass('swt:btn');
    expect(addValue).toHaveClass('swt:btn-outline');
    expect(addValue).toHaveClass('swt:w-fit');
    expect(addValue.querySelector('[class*="fluent--add-20-regular"]')).toBeInTheDocument();
  },
};

export const ExpandsPropertyValuesWithoutGrouping: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await expandProperty(canvas, 'Output', 'Species');

    expect(getGroupCard(canvasElement, 'Output', 'Output A')).toBeInTheDocument();
    expect(queryGroupCard(canvasElement, 'Output', 'Species: Arabidopsis')).not.toBeInTheDocument();
  },
};

export const RailValueShowsDragIndicatorWhileDragging: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const source = await railValue(canvas, 'Output', 'Analysis', 'Mass Spectrometry');

    const pointer = await startDragByPointer(source);

    await waitFor(() => expect(source).toHaveClass('swt:ring-2'));
    await waitFor(() => expect(screen.getByTestId('provenance-drag-overlay-value')).toHaveTextContent('Mass Spectrometry'));
    fireEvent.pointerUp(document, {
      clientX: source.getBoundingClientRect().left + 12,
      clientY: source.getBoundingClientRect().top + 12,
      button: 0,
      buttons: 0,
      isPrimary: true,
      pointerId: pointer.pointerId,
    });
  },
};

export const SingleSidedPropertiesCannotSwitchSides: Story = {
  render: () => <Harness fixture="disconnectedProperty" />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await ensurePropertyInRail(canvas, 'Output', 'Analysis');
    await ensurePropertyInRail(canvas, 'Output', 'Replicate');
    expect(canvas.getByTestId('provenance-property-drag-Output-Analysis')).toBeDisabled();
    expect(canvas.getByTestId('provenance-property-drag-Output-Replicate')).toBeDisabled();
  },
};

export const SwitchesPropertyGroupingSideByDrag: Story = {
  render: () => <Harness fixture="switchableProperty" />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await groupByProperty(canvasElement, 'Output', 'Batch');
    expect(queryGroupCard(canvasElement, 'Input', 'Batch: A')).not.toBeInTheDocument();

    await waitFor(() => {
      expect(canvas.getByTestId('provenance-property-Output-Batch')).toBeInTheDocument();
    }, { timeout: 10_000 });

    await dragByPointer(
      canvas.getByTestId('provenance-property-Output-Batch'),
      canvas.getByTestId('provenance-property-rail-Input'),
    );

    await waitFor(() => {
      expect(queryGroupCard(canvasElement, 'Output', 'Batch: A')).not.toBeInTheDocument();
      expect(getGroupCard(canvasElement, 'Output', 'Output A')).toBeInTheDocument();
      expect(getGroupCard(canvasElement, 'Input', 'Batch: A')).toBeInTheDocument();
      expect(getGroupCard(canvasElement, 'Input', 'Batch: B')).toBeInTheDocument();
      expect(queryGroupCard(canvasElement, 'Input', 'Input B')).not.toBeInTheDocument();
    });
  },
};

export const SwitchesInheritedPropertyToInputSideWithoutGrouping: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const inputRail = within(canvas.getByTestId('provenance-property-rail-Input'));
    const outputRail = within(canvas.getByTestId('provenance-property-rail-Output'));

    await ensurePropertyInRail(canvas, 'Output', 'Species');
    // This is the switch button between the two sides, which is only enabled for properties that are allowed to be dragged to the other side.
    expect(canvas.getByTestId('provenance-property-drag-Output-Species')).not.toBeDisabled();
    await waitFor(() => {
      expect(canvas.getByTestId('provenance-property-Output-Species')).toBeInTheDocument();
    }, { timeout: 10_000 });

    await dragByPointer(
      canvas.getByTestId('provenance-property-Output-Species'),
      canvas.getByTestId('provenance-property-rail-Input'),
    );

    await waitFor(() => {
      expect(inputRail.getByTestId('provenance-property-Input-Species')).toBeInTheDocument();
      expect(outputRail.queryByTestId('provenance-property-Output-Species')).not.toBeInTheDocument();
    }, { timeout: 10_000 });

    // Switching an ungrouped property only moves it; it must not group either side.
    expect(queryGroupCard(canvasElement, 'Input', 'Species: Arabidopsis')).not.toBeInTheDocument();
    expect(queryGroupCard(canvasElement, 'Output', 'Species: Arabidopsis')).not.toBeInTheDocument();
    expect(getGroupCard(canvasElement, 'Input', 'Input D')).toBeInTheDocument();
  },
};

export const ClicksSwapHandleToSwitchSideWithoutGrouping: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const inputRail = within(canvas.getByTestId('provenance-property-rail-Input'));
    const outputRail = within(canvas.getByTestId('provenance-property-rail-Output'));

    await ensurePropertyInRail(canvas, 'Output', 'Species');
    await userEvent.hover(canvas.getByTestId('provenance-property-Output-Species'));
    await userEvent.click(canvas.getByTestId('provenance-property-drag-Output-Species'));

    await waitFor(() => {
      expect(inputRail.getByTestId('provenance-property-Input-Species')).toBeInTheDocument();
      expect(outputRail.queryByTestId('provenance-property-Output-Species')).not.toBeInTheDocument();
    });

    // Switching an ungrouped property only moves it; it must not group either side.
    expect(queryGroupCard(canvasElement, 'Input', 'Species: Arabidopsis')).not.toBeInTheDocument();
    expect(queryGroupCard(canvasElement, 'Output', 'Species: Arabidopsis')).not.toBeInTheDocument();
  },
};

export const RegroupedValuesAreReadOnlyOnCards: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await groupByProperty(canvasElement, 'Output', 'Species');
    const grouped = await waitFor(
      () => getGroupCard(canvasElement, 'Output', 'Species: Chlamydomonas'),
      { timeout: 3000 },
    );

    const species = within(grouped).queryByTestId('provenance-value-pv-input-d-species');
    expect(species).not.toBeInTheDocument();
  },
};

export const RendersMeasuredConnections: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await waitFor(() => {
      const connector = canvas.getAllByTestId('provenance-connection')[0];
      expect(connector.getAttribute('d')).toMatch(/^M\s+\d/);
      expect(connector.getAttribute('d')).not.toContain('M 0 32');
    });
  },
};

export const ConnectorOverlayDoesNotMeasureConnectionNodesWhileIdle: Story = {
  render: () => {
    activeMeasurementCounter?.restore();
    activeMeasurementCounter = installConnectionNodeMeasurementCounter();
    return <Harness />;
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    try {
      await waitFor(() => expect(canvas.getAllByTestId('provenance-connection').length).toBeGreaterThan(0));
      await waitForStableConnectionMeasurements();

      const baseline = activeMeasurementCounter!.count();

      await waitForMilliseconds(180);

      expect(activeMeasurementCounter!.count()).toBe(baseline);
    } finally {
      activeMeasurementCounter?.restore();
    }
  },
};

export const RemeasuresConnectionsAfterGroupExpansion: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await groupByProperty(canvasElement, 'Output', 'Species');

    const grouped = await waitFor(() => getGroupCard(canvasElement, 'Output', 'Species: Arabidopsis'));

    const before = await waitFor(() => {
      const paths = canvas.getAllByTestId('provenance-connection').map((connector) => connector.getAttribute('d'));
      expect(paths.length).toBeGreaterThan(0);
      return paths;
    });

    await userEvent.click(within(grouped).getByRole('button', { name: 'Show members' }));

    await waitFor(() => {
      const after = canvas.getAllByTestId('provenance-connection').map((connector) => connector.getAttribute('d'));
      expect(after).not.toEqual(before);
    });
  },
};

export const ConnectorPathsUpdateAfterPanelResize: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const surface = canvas.getByTestId('provenance-surface');

    await waitFor(() => expect(canvas.getAllByTestId('provenance-connection').length).toBeGreaterThan(0));

    const path = firstMeasuredConnectorPath(canvasElement);
    const before = path.getAttribute('d');
    expect(before).not.toBeNull();

    const splitter = canvas.getByTestId('provenance-left-splitter');
    const surfaceRect = surface.getBoundingClientRect();
    const splitterRect = splitter.getBoundingClientRect();
    const pointerId = 41;

    fireEvent.pointerDown(splitter, {
      clientX: splitterRect.left + 2,
      clientY: splitterRect.top + 8,
      button: 0,
      buttons: 1,
      isPrimary: true,
      pointerId,
    });

    fireEvent.pointerMove(document, {
      clientX: surfaceRect.left + surfaceRect.width * 0.36,
      clientY: splitterRect.top + 8,
      button: 0,
      buttons: 1,
      isPrimary: true,
      pointerId,
    });

    fireEvent.pointerUp(document, {
      button: 0,
      buttons: 0,
      isPrimary: true,
      pointerId,
    });

    await waitFor(() => expect(firstMeasuredConnectorPath(canvasElement).getAttribute('d')).not.toBe(before), {
      timeout: 1200,
    });
  },
};

export const RendersConnectionsForQuotedGroupingValues: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const source = await addRailValue(canvas, 'Output', 'Analysis', "Farmer's field");
    await groupByProperty(canvasElement, 'Output', 'Analysis');
    const outputD = canvas.getByText('Output D').closest('article')!;

    await dragByPointer(source, outputD);

    await waitFor(() => {
      const connectors = canvas.getAllByTestId('provenance-connection');
      expect(connectors).toHaveLength(4);
      expect(connectors.every((connector) => connector.getAttribute('d')?.startsWith('M '))).toBe(true);
    });
  },
};

export const ShowsLiveConnectionPreviewWhileDraggingHandle: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const input = canvas.getByText('Input C').closest('article')!;
    const handle = within(input).getByTestId('provenance-connection-handle-Input-GroupCard');

    const pointer = await startDragByPointer(handle);

    await waitFor(() => {
      const preview = canvas.getByTestId('provenance-live-connection');
      expect(preview.getAttribute('d')).toMatch(/^M\s+\d/);
    });

    fireEvent.pointerUp(document, { button: 0, buttons: 0, isPrimary: true, pointerId: pointer.pointerId });
    await waitFor(() => expect(canvas.queryByTestId('provenance-live-connection')).not.toBeInTheDocument());
  },
};

export const ExpandedGroupsRenderMemberLevelConnections: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await groupByProperty(canvasElement, 'Output', 'Species');

    const grouped = await waitFor(() => getGroupCard(canvasElement, 'Output', 'Species: Arabidopsis'));

    await userEvent.click(within(grouped).getByRole('button', { name: 'Show members' }));

    await waitFor(() => {
      const paths = canvas.getAllByTestId('provenance-member-connection');
      expect(paths.length).toBeGreaterThan(0);
      expect(paths.every((path) => path.getAttribute('d')?.startsWith('M '))).toBe(true);
    });
  },
};

export const ExpandedGroupsHideGroupConnectionAnchors: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await groupByProperty(canvasElement, 'Output', 'Species');

    const grouped = await waitFor(() => getGroupCard(canvasElement, 'Output', 'Species: Arabidopsis'));
    const initialAggregateConnectionCount = canvas.getAllByTestId('provenance-connection').length;
    expect(within(grouped).getByTestId('provenance-connection-handle-Output-GroupCard')).toBeInTheDocument();

    await userEvent.click(within(grouped).getByRole('button', { name: 'Show members' }));

    await waitFor(() => {
      const expanded = getGroupCard(canvasElement, 'Output', 'Species: Arabidopsis');
      expect(within(expanded).queryByTestId('provenance-connection-handle-Output-GroupCard')).not.toBeInTheDocument();
      expect(within(expanded).getAllByTestId('provenance-connection-handle-Output-GroupMember').length).toBeGreaterThan(0);
      expect(canvas.queryAllByTestId('provenance-connection').length).toBeLessThan(initialAggregateConnectionCount);
      expect(connectionKeys(canvas.getAllByTestId('provenance-member-connection'))
        .every((key) => key.startsWith('member:connector:layer-1:'))).toBe(true);
    });
  },
};

const connectionKeys = (paths: HTMLElement[]) =>
  paths.map((path) => path.getAttribute('data-provenance-connection-key') ?? '');

export const ExpandedPropertyValuesConnectValueChipsToMatchingGroups: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await ensurePropertyInRail(canvas, 'Output', 'Species');
    await waitFor(() => {
      const headerKeys = connectionKeys(canvas.getAllByTestId('provenance-property-connection'));
      expect(headerKeys.some((key) => key.includes('Species'))).toBe(true);
    });

    const panel = await expandProperty(canvas, 'Output', 'Species');

    await waitFor(() => {
      const headerKeys = connectionKeys(canvas.queryAllByTestId('provenance-property-connection'));
      expect(headerKeys.some((key) => key.includes('Species'))).toBe(false);

      const valueKeys = connectionKeys(canvas.getAllByTestId('provenance-value-connection'));
      expect(valueKeys.some((key) => key.includes('Species') && key.includes('Arabidopsis'))).toBe(true);
      expect(valueKeys.some((key) => key.includes('Species') && key.includes('Chlamydomonas'))).toBe(true);
      expect(canvas.getAllByTestId('provenance-value-connection').every((path) => path.getAttribute('d')?.startsWith('M '))).toBe(true);
    });

    expect(panel.queryByTestId('provenance-connection-handle-Output-PropertyValue')).not.toBeInTheDocument();

    // Collapsing again must restore header connectors and drop value connectors,
    // so the expanded-header filter cannot become a one-way switch.
    await userEvent.hover(canvas.getByTestId('provenance-property-Output-Species'));
    await userEvent.click(canvas.getByTestId('provenance-property-expand-Output-Species'));

    await waitFor(() => {
      expect(canvas.queryByTestId('provenance-property-values-Output-Species')).not.toBeInTheDocument();
      const headerKeys = connectionKeys(canvas.getAllByTestId('provenance-property-connection'));
      expect(headerKeys.some((key) => key.includes('Species'))).toBe(true);
      expect(canvas.queryAllByTestId('provenance-value-connection')).toHaveLength(0);
    });
  },
};

export const ExpandedGroupPropertyConnectorsTargetMatchingMembers: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await groupByProperty(canvasElement, 'Output', 'Species');

    const grouped = await waitFor(() => getGroupCard(canvasElement, 'Output', 'Species: Arabidopsis'));
    const groupId = groupCardId(grouped);
    await userEvent.click(within(grouped).getByRole('button', { name: 'Show members' }));

    await waitFor(() => {
      const speciesKeys = connectionKeys(canvas.getAllByTestId('provenance-property-connection'))
        .filter((key) => key.includes('Species'));

      expect(speciesKeys.some((key) => key.includes('node-output-a'))).toBe(true);
      expect(speciesKeys.some((key) => key.includes('node-output-b'))).toBe(true);
      expect(speciesKeys.some((key) => key.includes('node-output-c'))).toBe(true);
      expect(speciesKeys.some((key) => key.endsWith(`:${groupId}`))).toBe(false);
    });

    await expandProperty(canvas, 'Output', 'Species');

    await waitFor(() => {
      const arabidopsisKeys = connectionKeys(canvas.getAllByTestId('provenance-value-connection'))
        .filter((key) => key.includes('Species') && key.includes('Arabidopsis'));

      expect(arabidopsisKeys.some((key) => key.includes('output-a'))).toBe(true);
      expect(arabidopsisKeys.some((key) => key.includes('output-b'))).toBe(true);
      expect(arabidopsisKeys.some((key) => key.includes('output-c'))).toBe(true);
      expect(arabidopsisKeys.some((key) => key.endsWith(`:${groupId}`))).toBe(false);
    });
  },
};

export const ConnectedExpandedGroupPropertyConnectorsTargetMatchingMembers: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    // Output B is incident to both replicate links, so grouping by Replicate keys
    // it on both values at once - intent §7's "an item connected to opposite-side
    // nodes carrying A and B is grouped under the normalized key A, B".
    const pooledTitle = 'Replicate: 1, Replicate: 2';

    for (let attempt = 0; attempt < 3 && !queryGroupCard(canvasElement, 'Output', pooledTitle); attempt += 1) {
      await showPropertyControls(canvas, 'Output', 'Replicate');
      fireEvent.click(canvas.getByTestId('provenance-property-both-Output-Replicate'));
      await waitFor(() => expect(queryGroupCard(canvasElement, 'Output', pooledTitle)).toBeInTheDocument(), {
        timeout: 1000,
      }).catch(() => undefined);
    }

    // "Both" scope groups the input side too. Each input is incident to one
    // replicate link only, so it keys on that single value: the old model merged
    // them because same-layer inheritance was symmetric and transitive, which is
    // exactly the defect this model removes (intent §14).
    await waitFor(() => {
      expect(queryGroupCard(canvasElement, 'Output', pooledTitle)).toBeInTheDocument();
      expect(queryGroupCard(canvasElement, 'Input', 'Replicate: 1')).toBeInTheDocument();
      expect(queryGroupCard(canvasElement, 'Input', 'Replicate: 2')).toBeInTheDocument();
    }, { timeout: 6000 });

    const outputGroup = await waitFor(() => getGroupCard(canvasElement, 'Output', pooledTitle));
    const outputGroupId = groupCardId(outputGroup);

    await userEvent.click(within(outputGroup).getByRole('button', { name: 'Show members' }));

    await waitFor(() => {
      expect(within(getGroupCard(canvasElement, 'Output', pooledTitle)).getByTestId('provenance-group-member-Output-node-output-b'))
        .toBeInTheDocument();
    });

    await waitFor(() => {
      const replicateKeys = connectionKeys(canvas.getAllByTestId('provenance-property-connection'))
        .filter((key) => key.includes('Output') && key.includes('Replicate'));

      expect(replicateKeys.some((key) => key.includes('output-b'))).toBe(true);
      expect(replicateKeys.some((key) => key.endsWith(`:${outputGroupId}`))).toBe(false);
    });
  },
};

export const CollapsedPropertiesConnectToMatchingGroupsAutomatically: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await ensurePropertyInRail(canvas, 'Output', 'Species');
    await waitFor(() => {
      const paths = canvas.getAllByTestId('provenance-property-connection');
      expect(paths.every((path) => path.getAttribute('d')?.startsWith('M '))).toBe(true);
      // Species has Arabidopsis on Input A/B/C and Chlamydomonas on Input D; lines to
      // groups closer than the minimum connector distance are intentionally skipped.
      const speciesLines = connectionKeys(paths).filter((key) => key.includes('Species'));
      expect(speciesLines.length).toBeGreaterThanOrEqual(3);
    });

    // Property headers expose no draggable connection handles; their connectors derive
    // from the values they contain.
    expect(canvas.queryByTestId('provenance-connection-handle-Input-PropertyHeader')).not.toBeInTheDocument();
    expect(canvas.queryByTestId('provenance-connection-handle-Output-PropertyHeader')).not.toBeInTheDocument();
  },
};

export const PropertyConnectorPathsUpdateWhenRailControlsAppear: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await ensurePropertyInRail(canvas, 'Output', 'Species');

    const before = await waitFor(() => {
      const path = firstPropertyConnectorPath(canvasElement, 'Species');
      expect(path.getAttribute('d')).not.toBeNull();
      return path.getAttribute('d');
    });

    await userEvent.hover(canvas.getByTestId('provenance-property-Output-Species'));

    await waitFor(() => expect(firstPropertyConnectorPath(canvasElement, 'Species').getAttribute('d')).not.toBe(before), {
      timeout: 1200,
    });
  },
};

export const RailValuesAssignByDragWithoutConnectionHandles: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const source = await railValue(canvas, 'Output', 'Analysis', 'Mass Spectrometry');

    expect(within(source as HTMLElement).queryByTestId('provenance-connection-handle-Output-PropertyValue')).not.toBeInTheDocument();

    const target = canvas.getByText('Output D').closest('article')!;
    await dragByPointer(source, target);

    await waitFor(() => {
      expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('ProcessAssignmentAdded');
    });
  },
};

export const CreatesPropertyValueFromRail: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const source = await addRailValue(canvas, 'Output', 'Analysis', 'Imaging');
    await groupByProperty(canvasElement, 'Output', 'Analysis');
    const outputD = canvas.getByText('Output D').closest('article')!;

    await dragByPointer(source, outputD);

    await waitFor(() => {
      expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('ProcessAssignmentAdded');
      expect(getGroupCard(canvasElement, 'Output', 'Analysis: Imaging')).toBeInTheDocument();
    });
  },
};

export const DraftCreationSelectsAnnotationOwnerKind: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await addRailProperty(canvas, 'Output', 'Node Draft', 'node value', 'node');
    await waitFor(() =>
      expect(canvas.getByTestId('provenance-property-Output-Node Draft')).toHaveAttribute(
        'data-provenance-property-kind',
        'node',
      ),
    );

    await addRailProperty(canvas, 'Output', 'Process Draft', 'process value', 'process');
    await waitFor(() =>
      expect(canvas.getByTestId('provenance-property-Output-Process Draft')).toHaveAttribute(
        'data-provenance-property-kind',
        'process',
      ),
    );
  },
};

export const DraftPromotesOnFirstNodeAssignment: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const source = await addRailProperty(canvas, 'Output', 'Node Draft', 'first node', 'node');
    expect(source).toHaveAttribute('data-provenance-unassigned', 'true');

    await dragByPointer(source, canvas.getByText('Output D').closest('article')!);

    await waitFor(async () => {
      expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('NodeAssignmentAdded:Text:none');
      expect(await railValue(canvas, 'Output', 'Node Draft', 'first node')).not.toHaveAttribute(
        'data-provenance-unassigned',
      );
    });
  },
};

export const DraftPromotesOnFirstProcessAssignment: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const source = await addRailProperty(canvas, 'Output', 'Process Draft', 'first process', 'process');
    expect(source).toHaveAttribute('data-provenance-unassigned', 'true');

    await dragByPointer(source, canvas.getByText('Output D').closest('article')!);

    await waitFor(async () => {
      expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('ProcessAssignmentAdded:Text:none');
      expect(await railValue(canvas, 'Output', 'Process Draft', 'first process')).not.toHaveAttribute(
        'data-provenance-unassigned',
      );
    });
  },
};

export const SingleMemberValueDropTargetsExactlyOneNode: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const source = await addRailProperty(canvas, 'Output', 'Member Marker', 'one member', 'node');
    await groupByProperty(canvasElement, 'Output', 'Species');

    const group = await waitFor(() => getGroupCard(canvasElement, 'Output', 'Species: Arabidopsis'));
    await userEvent.click(within(group).getByRole('button', { name: 'Show members' }));
    const member = within(group).getByTestId('provenance-group-member-Output-node-output-a');
    await dragByPointer(source, member);

    await waitFor(() => {
      const additions = (canvas.getByTestId('provenance-mutation-preview').textContent ?? '')
        .split('\n')
        .filter((line) => line.startsWith('NodeAssignmentAdded:'));
      expect(additions).toHaveLength(1);
    });
  },
};

export const ProcessValueDropOnSingleEdgeAssignsThatLink: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const source = await addRailProperty(canvas, 'Output', 'Single Edge Process', 'single edge', 'process');
    const edge = await waitFor(() => {
      const candidates = canvas.getAllByTestId('provenance-connection');
      expect(candidates.length).toBeGreaterThan(0);
      return candidates[0];
    });

    await dragByPointer(source, edge);

    await waitFor(() =>
      expect(processAssignmentLinkCount(canvas.getByTestId('provenance-mutation-preview'))).toBe(1),
    );
  },
};

export const ProcessValueDropOnPooledEdgeAssignsAllLinks: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const source = await addRailProperty(canvas, 'Output', 'Pooled Edge Process', 'pooled edge', 'process');
    await groupByProperty(canvasElement, 'Input', 'Species');

    let expectedLinkCount = 0;
    const pooledKey = await waitFor(() => {
      const badges = canvas.getAllByTestId('provenance-connection-count');
      expect(badges.length).toBeGreaterThan(0);
      expectedLinkCount = Number((badges[0].textContent ?? '').match(/\d+/)?.[0] ?? 0);
      expect(expectedLinkCount).toBeGreaterThan(1);
      return badges[0].getAttribute('data-provenance-connection-key');
    });
    const edge = await waitFor(() => {
      const candidate = canvas
        .getAllByTestId('provenance-connection')
        .find((path) => path.getAttribute('data-provenance-connection-key') === pooledKey);
      expect(candidate).toBeTruthy();
      return candidate!;
    });

    await dragByPointer(source, edge);

    await waitFor(() =>
      expect(processAssignmentLinkCount(canvas.getByTestId('provenance-mutation-preview'))).toBe(
        expectedLinkCount,
      ),
    );
  },
};

export const NodeValueDropOnEdgeShowsInvalidFeedback: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const source = await railValue(canvas, 'Input', 'Species', 'Arabidopsis');
    const edge = await waitFor(() => canvas.getAllByTestId('provenance-connection')[0]);

    await dragByPointer(source, edge);

    await waitFor(() => expect(canvasElement).toHaveTextContent(/node annotation.*cannot be assigned to a connection/i));
    expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('No mutations recorded.');
  },
};

export const ProcessValueDropOnGroupCardAssignsConnectedProcesses: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const source = await addRailProperty(canvas, 'Output', 'Bulk Process', 'bulk process', 'process');
    await groupByProperty(canvasElement, 'Output', 'Species');
    const group = await waitFor(() => getGroupCard(canvasElement, 'Output', 'Species: Arabidopsis'));
    const expectedLinkCount = Number(group.getAttribute('data-provenance-group-link-count') ?? 0);
    expect(expectedLinkCount).toBeGreaterThan(1);

    await dragByPointer(source, group);

    await waitFor(() =>
      expect(processAssignmentLinkCount(canvas.getByTestId('provenance-mutation-preview'))).toBe(
        expectedLinkCount,
      ),
    );
  },
};

export const NodeValueDropOnGroupCardAssignsEveryMember: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const source = await addRailProperty(canvas, 'Output', 'Bulk Node', 'bulk node', 'node');
    await groupByProperty(canvasElement, 'Output', 'Species');
    const group = await waitFor(() => getGroupCard(canvasElement, 'Output', 'Species: Arabidopsis'));

    await dragByPointer(source, group);

    await waitFor(() => {
      const lines = (canvas.getByTestId('provenance-mutation-preview').textContent ?? '')
        .split('\n')
        .filter((line) => line.startsWith('NodeAssignmentAdded:'));
      expect(lines).toHaveLength(3);
    });
  },
};

export const ValueAssignmentTargetsEitherSideButRejectsMixedSelection: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await dragByPointer(
      await railValue(canvas, 'Output', 'Analysis', 'Mass Spectrometry'),
      canvas.getByText('Input D').closest('article')!,
    );
    await waitFor(() =>
      expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('ProcessAssignmentAdded'),
    );

    await dragByPointer(
      await railValue(canvas, 'Input', 'Species', 'Arabidopsis'),
      canvas.getByText('Output D').closest('article')!,
    );
    await waitFor(() =>
      expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('NodeAssignmentAdded'),
    );

    await selectGroup(canvas.getByText('Input C').closest('article')!);
    await selectGroup(canvas.getByText('Output E').closest('article')!);
    const source = await railValue(canvas, 'Output', 'Analysis', 'Mass Spectrometry');
    const preview = canvas.getByTestId('provenance-mutation-preview');
    const before = preview.textContent;
    await userEvent.click(within(source as HTMLElement).getByRole('button', { name: /apply to 2 selected groups/i }));

    await waitFor(() => expect(canvasElement).toHaveTextContent(/one side at a time/i));
    expect(preview.textContent).toBe(before);
  },
};

export const PaletteValuesLookTentativeUntilAssigned: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const source = await addRailValue(canvas, 'Output', 'Analysis', 'Sequencing');

    // A value created in the rail is only a palette entry until it is dropped.
    expect(source).toHaveAttribute('data-provenance-unassigned', 'true');
    expect(source).toHaveClass('swt:border-dashed');

    const outputD = canvas.getByText('Output D').closest('article')!;
    await dragByPointer(source, outputD);

    await waitFor(async () => {
      const assigned = await railValue(canvas, 'Output', 'Analysis', 'Sequencing');
      expect(assigned).not.toHaveAttribute('data-provenance-unassigned');
    });
  },
};

export const OverwritingAPaletteCreatedValueEmitsAnUpdatePatch: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const outputD = canvas.getByText('Output D').closest('article')!;

    const first = await addRailValue(canvas, 'Output', 'Analysis', 'Imaging');
    await dragByPointer(first, outputD);

    await waitFor(() => {
      expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('ProcessAssignmentAdded');
    });

    const second = await addRailValue(canvas, 'Output', 'Analysis', 'Sequencing');
    await dragByPointer(second, outputD);

    await waitFor(() => expect(canvas.getByTestId('provenance-overwrite-warning')).toBeInTheDocument());
    await userEvent.click(canvas.getByTestId('provenance-confirm-overwrite'));

    // The value being overwritten is Virtual (palette-created), not Real. Before
    // the PG-3 fix, editing a Virtual value emitted no patch, so the writeback
    // log would still say "add Imaging" while the model actually held
    // "Sequencing" - silent data loss for editor-created values.
    await waitFor(() => {
      expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('PropertyValueDefinitionUpdated:Text:none');
    });
  },
};

export const ConnectionDetailsShowEntityPairsWithoutPropertyCreation: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    const connector = await waitFor(() => canvas.getAllByTestId('provenance-connection')[0]);
    connector.focus();
    await userEvent.keyboard('{Enter}');

    const details = await waitFor(() => canvas.getByTestId('provenance-connection-details'));
    // Underlying connections are listed as readable entity name pairs.
    expect(within(details).getByTestId('provenance-connection-pairs')).toHaveTextContent('→');
    expect(details).toHaveTextContent(/connection/i);
    expect(within(details).queryByText(/Add value/i)).not.toBeInTheDocument();
    expect(within(details).queryByText(/Add annotation/i)).not.toBeInTheDocument();
    expect(within(details).getByRole('button', { name: /remove connection/i })).toBeInTheDocument();
  },
};

export const RemovesConnectionFromDetailsPanel: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const initialCount = (await waitFor(() => {
      const connectors = canvas.getAllByTestId('provenance-connection');
      expect(connectors.length).toBeGreaterThan(0);
      return connectors;
    })).length;

    await userEvent.click(canvas.getAllByTestId('provenance-connection')[0]);
    const details = await waitFor(() => canvas.getByTestId('provenance-connection-details'));
    await userEvent.click(within(details).getByTestId('provenance-connection-remove'));

    await waitFor(() => {
      expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('ProcessLinkRemoved');
      expect(canvas.queryAllByTestId('provenance-connection').length).toBeLessThan(initialCount);
    });
    expect(canvas.queryByTestId('provenance-connection-details')).not.toBeInTheDocument();
  },
};

export const SelectsConnectionWithKeyboard: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const connector = await waitFor(() => canvas.getAllByTestId('provenance-connection')[0]);

    expect(connector).toHaveAttribute('role', 'button');
    expect(connector).toHaveAttribute('aria-label');
    expect(connector).toHaveAttribute('tabindex', '0');
    connector.focus();
    await userEvent.keyboard('{Enter}');

    await waitFor(() => expect(canvas.getByTestId('provenance-connection-details')).toBeInTheDocument());

    const secondConnector = canvas.getAllByTestId('provenance-connection')[1];
    const secondLabel = secondConnector.getAttribute('aria-label')!.replace('Select connection ', '');
    secondConnector.focus();
    await userEvent.keyboard(' ');

    await waitFor(() =>
      expect(canvas.getByTestId('provenance-connection-details')).toHaveAttribute('data-connection-id', secondLabel),
    );
  },
};

export const RemovesConnectionFromContextMenu: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const initialCount = (await waitFor(() => {
      const connectors = canvas.getAllByTestId('provenance-connection');
      expect(connectors.length).toBeGreaterThan(0);
      return connectors;
    })).length;

    const connector = canvas.getAllByTestId('provenance-connection')[0];
    fireEvent.contextMenu(connector, { clientX: 320, clientY: 240, bubbles: true });
    const menu = await screen.findByTestId('context_menu');
    await userEvent.click(within(menu).getByRole('button', { name: /delete/i }));

    await waitFor(() => {
      expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('ProcessLinkRemoved');
      expect(canvas.queryAllByTestId('provenance-connection').length).toBeLessThan(initialCount);
    });
    expect(canvas.queryByTestId('provenance-connection-details')).not.toBeInTheDocument();
  },
};

export const RemovesExpandedMemberConnectionFromContextMenu: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await groupByProperty(canvasElement, 'Output', 'Species');

    const grouped = await waitFor(() => getGroupCard(canvasElement, 'Output', 'Species: Arabidopsis'));
    await userEvent.click(within(grouped).getByRole('button', { name: 'Show members' }));

    const connector = await waitFor(() => {
      const memberConnector = canvas.getAllByTestId('provenance-member-connection')[0];
      expect(memberConnector).toBeTruthy();
      expect(memberConnector).toHaveAttribute('role', 'button');
      return memberConnector!;
    });

    const removedKey = connector.getAttribute('data-provenance-connection-key');
    await userEvent.click(connector);

    await waitFor(() => {
      expect(canvas.getByTestId('provenance-connection-details')).toBeInTheDocument();
      expect(within(grouped).getAllByTestId('provenance-connection-handle-Output-GroupMember').length).toBeGreaterThan(0);
    });

    const selectedConnector = canvas
      .getAllByTestId('provenance-member-connection')
      .find((path) => path.getAttribute('data-provenance-connection-key') === removedKey);
    expect(selectedConnector).toBeTruthy();
    fireEvent.contextMenu(selectedConnector!, { clientX: 360, clientY: 280, bubbles: true });
    const menu = await screen.findByTestId('context_menu');
    await userEvent.click(within(menu).getByRole('button', { name: /delete/i }));

    await waitFor(() => {
      expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('ProcessLinkRemoved');
      expect(connectionKeys(canvas.queryAllByTestId('provenance-member-connection'))).not.toContain(removedKey);
      expect(within(grouped).getAllByTestId('provenance-connection-handle-Output-GroupMember').length).toBeGreaterThan(0);
    });
  },
};

export const RemovesConnectionWithDeleteKey: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const connector = await waitFor(() => canvas.getAllByTestId('provenance-connection')[0]);
    const initialCount = canvas.getAllByTestId('provenance-connection').length;

    connector.focus();
    await userEvent.keyboard('{Delete}');

    await waitFor(() => {
      expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('ProcessLinkRemoved');
      expect(canvas.queryAllByTestId('provenance-connection').length).toBeLessThan(initialCount);
    });
  },
};

export const WarnsBeforeOverwritingSingleValueFromRail: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    // Species is owned by the input nodes and only propagated to outputs. Use
    // the owning side so this exercises an exact assignment overwrite rather
    // than correctly adding a new local annotation to a receiver node.
    const source = await railValue(canvas, 'Input', 'Species', 'Arabidopsis');
    await groupByProperty(canvasElement, 'Input', 'Species');
    const target = getGroupCard(canvasElement, 'Input', 'Species: Chlamydomonas');

    await dragByPointer(source, target);

    await waitFor(() => expect(canvas.getByTestId('provenance-overwrite-warning')).toBeInTheDocument());
    expect(canvas.getByTestId('provenance-overwrite-warning')).toHaveTextContent('Overwrite Species value?');
    // userEvent.click emits the full pointerdown/pointerup/click sequence, so a
    // stray onPointerUp bound next to onClick would double-fire the confirm
    // here - the exact-one-line assertion below is what catches that.
    await userEvent.click(canvas.getByTestId('provenance-confirm-overwrite'));

    await waitFor(() => {
      const preview = canvas.getByTestId('provenance-mutation-preview').textContent ?? '';
      expect(preview).toContain('NodeAssignmentValueChanged:Text:none');
      const updateLines = preview.split('\n').filter((line) => line.startsWith('NodeAssignmentValueChanged:'));
      expect(updateLines).toHaveLength(1);
    });
  },
};

export const RejectsOverwriteWhenTargetHasMultipleValues: Story = {
  render: () => <Harness fixture="ambiguousProcessAssignment" />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const source = await railValue(canvas, 'Output', 'Replicate', '3');
    await groupByProperty(canvasElement, 'Output', 'Replicate');
    const target = getGroupCard(canvasElement, 'Output', 'Replicate: 1, Replicate: 2');

    await dragByPointer(source, target);

    await waitFor(() => expect(canvas.getByText(/Cannot overwrite: multiple distinct values/i)).toBeInTheDocument());
    expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('No mutations recorded.');
  },
};

export const RefreshesRailTermValueAfterControlledMetadataReplacement: Story = {
  render: () => <Harness fixture="typedSample" allowTermReplacement />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await userEvent.click(canvas.getByRole('button', { name: /Replace term metadata/i }));
    const source = await railValue(canvas, 'Output', 'Instrument', 'mass spectrometer');
    expect(source).toBeInTheDocument();
  },
};

export const CreatesNumericPropertyValue: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const source = await addRailValue(canvas, 'Output', 'Analysis', '1.5', 'Float');
    const outputD = canvas.getByText('Output D').closest('article')!;

    await dragByPointer(source, outputD);

    await waitFor(() =>
      expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('ProcessAssignmentAdded:Float:none'),
    );
  },
};

export const RejectsNonFiniteNumericPropertyValue: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const panel = await expandProperty(canvas, 'Output', 'Analysis');

    await userEvent.click(panel.getByText('Add value'));
    await userEvent.selectOptions(screen.getByRole('combobox', { name: /Value type/i }), 'Float');
    await userEvent.type(screen.getByRole('textbox', { name: /Analysis value/i }), 'Infinity');

    const submit = screen
      .getAllByRole('button', { name: /^Add value$/i })
      .find((button) => button.getAttribute('type') === 'submit')!;
    expect(submit).toBeDisabled();
    expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('No mutations recorded.');
  },
};

export const CreatesDataEndpointFromAvailableKindList: Story = {
  render: () => <Harness fixture="dataOutputOnly" />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await userEvent.click(canvas.getByTestId('popover_trigger_provenance-add-input'));
    // The offered kinds are the session's own, so a new endpoint is always
    // one the adapter that loaded the session can also write back.
    await userEvent.selectOptions(
      screen.getByRole('combobox', { name: /Endpoint kind/i }),
      'fixture:endpoint:data',
    );
    await userEvent.type(screen.getByRole('textbox', { name: /Endpoint name/i }), 'New Input');
    await userEvent.click(screen.getByRole('button', { name: /Create endpoint/i }));

    await waitFor(() =>
      expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('LayerEndpointAdded:fixture:endpoint:data:Data'),
    );
  },
};

export const TracksEndpointKindListWithSessionReplacement: Story = {
  render: () => <Harness fixture="dataOutputOnly" allowEndpointReplacement />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // The Data-only fixture offers Data...
    await userEvent.click(canvas.getByTestId('popover_trigger_provenance-add-input'));
    expect(screen.getByRole('combobox', { name: /Endpoint kind/i })).toHaveValue('fixture:endpoint:data');
    await userEvent.keyboard('{Escape}');

    // ...and swapping in a Sample-only session swaps the offer with it. The
    // list has to follow the loaded session: a fixed catalog would offer
    // kinds the session's own adapter cannot materialize on writeback.
    await userEvent.click(canvas.getByRole('button', { name: /Replace endpoint context/i }));
    await userEvent.click(canvas.getByTestId('popover_trigger_provenance-add-input'));

    await waitFor(() =>
      expect(screen.getByRole('combobox', { name: /Endpoint kind/i })).toHaveValue('fixture:endpoint:sample'),
    );
  },
};

export const CreatesEndpointFromSelectedAvailableKind: Story = {
  render: () => <Harness inputOnly />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await userEvent.click(canvas.getByTestId('popover_trigger_provenance-add-output'));

    // A sample-only session offers exactly its own kind: an adapter is never
    // handed an endpoint kind it cannot materialize on writeback.
    const kindSelect = screen.getByRole('combobox', { name: /Endpoint kind/i });
    expect([...kindSelect.querySelectorAll('option')].map((option) => option.getAttribute('value'))).toEqual([
      'fixture:endpoint:sample',
    ]);

    await userEvent.selectOptions(kindSelect, 'fixture:endpoint:sample');
    await userEvent.type(screen.getByRole('textbox', { name: /Endpoint name/i }), 'Custom Output');
    await userEvent.click(screen.getByRole('button', { name: /Create endpoint/i }));

    await waitFor(() =>
      expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent(
        'LayerEndpointAdded:fixture:endpoint:sample:Sample',
      ),
    );
  },
};

export const OffersHostDeclaredEndpointKindsBeyondSessionSets: Story = {
  render: () => <Harness inputOnly endpointKinds={sampleAndDataEndpointKinds()} />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await userEvent.click(canvas.getByTestId('popover_trigger_provenance-add-output'));

    // The host declares which kinds its adapter can write back, so a
    // sample-only session still offers Data - the session's own sets no
    // longer cap the list.
    const kindSelect = screen.getByRole('combobox', { name: /Endpoint kind/i });
    expect([...kindSelect.querySelectorAll('option')].map((option) => option.getAttribute('value'))).toEqual([
      'fixture:endpoint:sample',
      'fixture:endpoint:data',
    ]);

    await userEvent.selectOptions(kindSelect, 'fixture:endpoint:data');
    await userEvent.type(screen.getByRole('textbox', { name: /Endpoint name/i }), 'Late Data Output');
    await userEvent.click(screen.getByRole('button', { name: /Create endpoint/i }));

    await waitFor(() =>
      expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent(
        'LayerEndpointAdded:fixture:endpoint:data:Data',
      ),
    );
  },
};

export const CreatesNextLayerAndKeepsBoundaryEditsSynchronized: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    const outputA = canvas.getByText('Output A').closest('article')!;
    await selectGroup(outputA);
    await createLayer(canvas, 'Layer 2');

    await waitFor(() => {
      expect(canvas.getByTestId('provenance-layer-layer-2')).toHaveClass('swt:btn-primary');
      expect(canvasElement).toHaveTextContent('Output A');
    });

    // The seed is the same canonical node in both layers. Give that node a new
    // owned annotation; process values on the old layer are merely available
    // here and there is no new-layer process link to overwrite yet.
    const source = await addRailProperty(canvas, 'Input', 'Boundary Marker', 'Edited boundary', 'node');
    const carried = canvas.getByText('Output A').closest('article')!;
    await dragByPointer(source, carried);
    await waitFor(() =>
      expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('NodeAssignmentAdded'),
    );

    // The controlled-session publish may replace the tab node just as the drag
    // helper returns. Reacquire it on each attempt so this still requires a
    // real successful activation without dispatching to a detached element.
    await waitFor(() => {
      fireEvent.click(canvas.getByTestId('provenance-layer-layer-1'));
      expect(canvas.getByTestId('provenance-layer-layer-1')).toHaveClass('swt:btn-primary');
    });
    await railValue(canvas, 'Output', 'Boundary Marker', 'Edited boundary');
    expect(canvas.getByTestId('provenance-mutation-preview')).not.toHaveTextContent('No mutations recorded.');
  },
};

export const RapidEditThenLayerSwitchKeepsEdit: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    const outputA = canvas.getByText('Output A').closest('article')!;
    await selectGroup(outputA);
    await createLayer(canvas, 'Layer 2');

    await waitFor(() => {
      expect(canvas.getByTestId('provenance-layer-layer-2')).toHaveClass('swt:btn-primary');
      expect(canvasElement).toHaveTextContent('Output A');
    });

    const source = await addRailProperty(canvas, 'Input', 'Boundary Marker', 'Rapid boundary edit', 'node');
    const carried = canvas.getByText('Output A').closest('article')!;
    await dragByPointer(source, carried);

    // Count of real patch lines the edit committed - the exact number depends on
    // how many members the group has, so the duplication guard below compares
    // against this baseline instead of hard-coding it.
    const patchCount = () =>
      (canvas.getByTestId('provenance-mutation-preview').textContent ?? '')
        .split('\n')
        .filter((line) => line.trim().length > 0 && line !== 'No mutations recorded.').length;

    let committedPatches = 0;
    await waitFor(() => {
      committedPatches = patchCount();
      expect(committedPatches).toBeGreaterThan(0);
    });

    // fireEvent (not userEvent, which adds its own settle delay): switch away
    // from and immediately back to layer 2 right after the publish above,
    // without awaiting the UI in between. A handler that closed over a
    // render-scope session instead of reading a `latest*` ref could fire
    // against a session from before this edit, dropping or duplicating it.
    fireEvent.click(canvas.getByTestId('provenance-layer-layer-1'));
    fireEvent.click(canvas.getByTestId('provenance-layer-layer-2'));

    await waitFor(() => {
      expect(canvasElement).toHaveTextContent('Rapid boundary edit');
      // Neither dropped (Imaging gone / fewer patches) nor duplicated (more patches).
      expect(patchCount()).toBe(committedPatches);
    });
  },
};

export const CompletesAnInputOnlyLayer: Story = {
  render: () => <Harness inputOnly />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    expect(canvasElement).toHaveTextContent('Input Only A');
    expect(canvasElement).toHaveTextContent('No entries in this layer');

    await userEvent.click(canvas.getByTestId('popover_trigger_provenance-add-output'));
    await userEvent.type(screen.getByRole('textbox', { name: /Endpoint name/i }), 'New Output');
    await userEvent.click(screen.getByRole('button', { name: /Create endpoint/i }));

    await waitFor(() => expect(canvasElement).toHaveTextContent('New Output'));
    expect(canvas.getByTestId('provenance-mutation-preview')).not.toHaveTextContent('No mutations recorded.');
  },
};

export const AddsExistingPropertyToCreatedEmptySide: Story = {
  render: () => <Harness inputOnly />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await userEvent.click(canvas.getByTestId('provenance-add-output-trigger'));
    const endpoint = await waitFor(() => screen.getByRole('textbox', { name: /Endpoint name/i }));
    await userEvent.type(endpoint, 'New Output');
    await userEvent.click(screen.getByRole('button', { name: /Create endpoint/i }));

    const output = await waitFor(() => canvas.getByText('New Output').closest('article')!);
    const source = await railValue(canvas, 'Input', 'Species', 'Arabidopsis');
    await dragByPointer(source, output);

    await waitFor(() =>
      expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('NodeAssignmentAdded'),
    );
    expect(canvas.getByText('New Output').closest('article')!).not.toHaveTextContent('Species: Arabidopsis');
  },
};

export const AddsNewPropertyFromRail: Story = {
  render: () => <Harness inputOnly />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    const target = canvas.getByText('Input Only A').closest('article')!;
    const source = await addRailProperty(canvas, 'Input', 'Treatment', 'Drought');
    await dragByPointer(source, target);

    await waitFor(() =>
      expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('NodeAssignmentAdded'),
    );
    expect(target).not.toHaveTextContent('Treatment: Drought');
  },
};

export const HidingASideCentersTheRemainingSide: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    expect(canvas.getByTestId('provenance-property-rail-Output')).toBeInTheDocument();
    expect(canvas.getByText('Output A')).toBeInTheDocument();

    await userEvent.click(canvas.getByTestId('provenance-side-visibility-Output'));

    await waitFor(() => {
      expect(canvas.queryByTestId('provenance-property-rail-Output')).not.toBeInTheDocument();
      expect(canvas.queryByText('Output A')).not.toBeInTheDocument();
    });
    // The kept side and its rail stay on screen; only the hidden side leaves.
    expect(canvas.getByTestId('provenance-property-rail-Input')).toBeInTheDocument();
    expect(canvas.getByText('Input A')).toBeInTheDocument();

    // The visible side sits as a centered cluster: the rail is not flush to the
    // left edge, the card column keeps a generous width (no compact-container
    // downshift), and the empty space is balanced on both sides.
    {
      const surface = canvas.getByTestId('provenance-surface');
      const groupColumn = Array.from(surface.children).find((element) =>
        element.querySelector('[data-provenance-group-node^="provenance-node::Input::"]'),
      ) as HTMLElement;
      const sr = surface.getBoundingClientRect();
      const railLeft = canvas.getByTestId('provenance-property-rail-Input').getBoundingClientRect().left;
      const gc = groupColumn.getBoundingClientRect();
      const leftGap = railLeft - sr.left;
      const rightGap = sr.right - gc.right;
      expect(gc.width).toBeGreaterThan(360);
      expect(leftGap).toBeGreaterThan(24);
      expect(rightGap).toBeGreaterThan(24);
      // Equal spacers keep the cluster genuinely centered, not merely off the edge.
      expect(Math.abs(leftGap - rightGap)).toBeLessThan(32);
    }

    await userEvent.click(canvas.getByTestId('provenance-side-visibility-Output'));

    await waitFor(() => {
      expect(canvas.getByTestId('provenance-property-rail-Output')).toBeInTheDocument();
      expect(canvas.getByText('Output A')).toBeInTheDocument();
    });
  },
};

export const SwitchableAnnotationFollowsVisibleSideWhenItsSideIsHidden: Story = {
  render: () => <Harness fixture="switchableProperty" />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // Drag Batch onto the output rail. Batch can switch sides because it exists
    // on both input and output sets.
    await ensurePropertyInRail(canvas, 'Output', 'Batch');
    expect(canvas.queryByTestId('provenance-property-Input-Batch')).not.toBeInTheDocument();

    // With outputs hidden, the switchable annotation moves onto the input rail.
    await toggleSideVisibility(canvas, 'Output', () =>
      expect(canvas.queryByTestId('provenance-property-rail-Output')).not.toBeInTheDocument(),
    );
    expect(canvas.getByTestId('provenance-property-Input-Batch')).toBeInTheDocument();

    // The move is permanent: revealing the output side leaves Batch on the input
    // rail rather than sending it back.
    await toggleSideVisibility(canvas, 'Output', () =>
      expect(canvas.getByTestId('provenance-property-rail-Output')).toBeInTheDocument(),
    );
    expect(canvas.getByTestId('provenance-property-Input-Batch')).toBeInTheDocument();
    expect(canvas.queryByTestId('provenance-property-Output-Batch')).not.toBeInTheDocument();
  },
};

export const GroupBothOnVisibleSideAppliesToHiddenSideWhenRevealed: Story = {
  render: () => <Harness fixture="switchableProperty" />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await toggleSideVisibility(canvas, 'Output', () =>
      expect(canvas.queryByTestId('provenance-property-rail-Output')).not.toBeInTheDocument(),
    );

    // Batch now sits on the visible input rail; grouping "both" from here must
    // still drive the hidden output side.
    await showPropertyControls(canvas, 'Input', 'Batch');
    for (
      let attempt = 0;
      attempt < 3 && !queryGroupCard(canvasElement, 'Input', 'Batch: A');
      attempt += 1
    ) {
      fireEvent.click(canvas.getByTestId('provenance-property-both-Input-Batch'));
      await waitFor(
        () => expect(getGroupCard(canvasElement, 'Input', 'Batch: A')).toBeInTheDocument(),
        { timeout: 1000 },
      ).catch(() => undefined);
    }
    await waitFor(() =>
      expect(getGroupCard(canvasElement, 'Input', 'Batch: A')).toBeInTheDocument(),
    );

    // Showing the output side reveals the grouping the same action produced there.
    await toggleSideVisibility(canvas, 'Output', () =>
      expect(canvas.getByTestId('provenance-property-rail-Output')).toBeInTheDocument(),
    );

    await waitFor(() => {
      expect(getGroupCard(canvasElement, 'Output', 'Batch: A')).toBeInTheDocument();
      expect(getGroupCard(canvasElement, 'Output', 'Batch: B')).toBeInTheDocument();
    });
  },
};

let nextPointerId = 100;

function allocatePointerId() {
  nextPointerId += 1;
  return nextPointerId;
}

function nextFrame() {
  return new Promise((resolve) => requestAnimationFrame(resolve));
}

function activeDragElement() {
  return document.body.querySelector(
    [
      '[data-testid="foldered-draggable-drag-overlay"]',
      '[data-testid="provenance-drag-overlay-property"]',
      '[data-testid="provenance-drag-overlay-value"]',
      '[data-testid="provenance-live-connection"]',
    ].join(','),
  );
}

async function waitForDragActivation() {
  await waitFor(() => expect(activeDragElement()).not.toBeNull(), { timeout: 2000 });
  await nextFrame();
}

function pointerCenter(element: Element) {
  const geometry = element as SVGGeometryElement;

  if (typeof geometry.getTotalLength === 'function' && typeof geometry.getPointAtLength === 'function') {
    const matrix = geometry.getScreenCTM();
    if (matrix) {
      const point = geometry.getPointAtLength(geometry.getTotalLength() / 2);
      return {
        x: matrix.a * point.x + matrix.c * point.y + matrix.e,
        y: matrix.b * point.x + matrix.d * point.y + matrix.f,
      };
    }
  }

  const rect = element.getBoundingClientRect();
  return { x: rect.left + rect.width / 2, y: rect.top + rect.height / 2 };
}

async function dragByPointer(source: Element, target: Element) {
  const pointerId = allocatePointerId();
  const from = pointerCenter(source);
  const to = pointerCenter(target);
  const fromX = from.x;
  const fromY = from.y;
  let toX = to.x;
  let toY = to.y;
  const deltaX = toX - fromX;
  const deltaY = toY - fromY;
  const distance = Math.hypot(deltaX, deltaY) || 1;
  const activationX = fromX + (deltaX / distance) * 8;
  const activationY = fromY + (deltaY / distance) * 8;
  fireEvent.pointerDown(source, {
    clientX: fromX,
    clientY: fromY,
    button: 0,
    buttons: 1,
    isPrimary: true,
    pointerId,
  });
  await nextFrame();
  fireEvent.pointerMove(target, {
    clientX: activationX,
    clientY: activationY,
    button: 0,
    buttons: 1,
    isPrimary: true,
    pointerId,
  });
  await waitForDragActivation();
  // Connection targets visually react once the drag becomes active. Resolve
  // the final pointer against the settled target, as a user aiming at the
  // highlighted handle would, rather than against its pre-drag rectangle.
  const activeTarget = pointerCenter(target);
  toX = activeTarget.x;
  toY = activeTarget.y;
  fireEvent.pointerMove(document, {
    clientX: toX,
    clientY: toY,
    button: 0,
    buttons: 1,
    isPrimary: true,
    pointerId,
  });
  await nextFrame();
  fireEvent.pointerUp(target, {
    clientX: toX,
    clientY: toY,
    button: 0,
    buttons: 0,
    isPrimary: true,
    pointerId,
  });
  await nextFrame();
}

/// Clicks one action button on an open context menu and waits for the menu to
/// close, which every action does. The first real-pointer activation on a
/// freshly opened menu can be swallowed while it settles - the same
/// first-activation flake the rail's hover-revealed controls retry around
/// (see openRailValueRemoval) - so one retry against a fresh lookup.
async function clickMenuAction(name: RegExp) {
  const menu = screen.getByTestId('context_menu');
  const button = within(menu).getByRole('button', { name });
  expect(button).toBeEnabled();
  await userEvent.click(button);
  try {
    await waitFor(() => expect(screen.queryByTestId('context_menu')).not.toBeInTheDocument(), {
      timeout: 500,
    });
  } catch {
    const reopened = screen.getByTestId('context_menu');
    await userEvent.click(within(reopened).getByRole('button', { name }));
    await waitFor(() => expect(screen.queryByTestId('context_menu')).not.toBeInTheDocument());
  }
}

async function startDragByPointer(source: Element) {
  const pointerId = allocatePointerId();
  const from = source.getBoundingClientRect();
  const fromX = from.left + from.width / 2;
  const fromY = from.top + from.height / 2;
  const activationX = fromX + 8;
  const activationY = fromY;
  fireEvent.pointerDown(source, {
    clientX: fromX,
    clientY: fromY,
    button: 0,
    buttons: 1,
    isPrimary: true,
    pointerId,
  });
  await nextFrame();
  fireEvent.pointerMove(document, {
    clientX: activationX,
    clientY: activationY,
    button: 0,
    buttons: 1,
    isPrimary: true,
    pointerId,
  });
  await waitForDragActivation();

  return { x: activationX, y: activationY, pointerId };
}

async function moveDragPointerTo(target: Element, pointerId: number) {
  const to = target.getBoundingClientRect();
  const toX = to.left + to.width / 2;
  const toY = to.top + to.height / 2;
  fireEvent.pointerMove(document, {
    clientX: toX,
    clientY: toY,
    button: 0,
    buttons: 1,
    isPrimary: true,
    pointerId,
  });
  await nextFrame();

  return { x: toX, y: toY };
}

function escapeRegExp(value: string) {
  return value.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}

type ConnectionNodeMeasurementCounter = {
  count: () => number;
  restore: () => void;
};

let activeMeasurementCounter: ConnectionNodeMeasurementCounter | null = null;

function waitForMilliseconds(milliseconds: number) {
  return new Promise<void>((resolve) => {
    window.setTimeout(resolve, milliseconds);
  });
}

function installConnectionNodeMeasurementCounter(): ConnectionNodeMeasurementCounter {
  const originalGetBoundingClientRect = Element.prototype.getBoundingClientRect;
  let measurementCount = 0;

  Element.prototype.getBoundingClientRect = function getBoundingClientRectWithCounter(this: Element) {
    if (this instanceof HTMLElement && this.hasAttribute('data-provenance-connection-node')) {
      measurementCount += 1;
    }

    return originalGetBoundingClientRect.call(this);
  };

  return {
    count: () => measurementCount,
    restore: () => {
      Element.prototype.getBoundingClientRect = originalGetBoundingClientRect;
      activeMeasurementCounter = null;
    },
  };
}

async function waitForStableConnectionMeasurements() {
  await waitFor(async () => {
    const before = activeMeasurementCounter!.count();
    await waitForMilliseconds(80);
    expect(activeMeasurementCounter!.count()).toBe(before);
  }, { timeout: 1600 });
}

function firstMeasuredConnectorPath(canvasElement: HTMLElement): SVGPathElement {
  const path = canvasElement.querySelector<SVGPathElement>('[data-testid="provenance-connection"]');
  if (!(path instanceof SVGPathElement)) {
    throw new Error('Expected a measured provenance connector path.');
  }
  return path;
}

function firstPropertyConnectorPath(canvasElement: HTMLElement, propertyName: string): SVGPathElement {
  const path = Array.from(canvasElement.querySelectorAll<SVGPathElement>('[data-testid="provenance-property-connection"]'))
    .find((candidate) => candidate.getAttribute('data-provenance-connection-key')?.includes(propertyName));

  if (!path) {
    throw new Error(`Expected a measured property connector path for "${propertyName}".`);
  }

  return path;
}

function shelfFolders(canvas: ReturnType<typeof within>) {
  return Array.from(
    canvas
      .getByTestId('foldered-draggable-folder-row')
      .querySelectorAll<HTMLElement>('[data-testid^="foldered-draggable-folder-"]'),
  );
}

async function openShelfFolder(canvas: ReturnType<typeof within>, folder: HTMLElement) {
  // Folders render as index-card tabs; clicking the tab activates its card.
  const folderTestId = folder.getAttribute('data-testid')!;
  const currentFolder = () => canvas.getByTestId(folderTestId);

  if (currentFolder().getAttribute('aria-selected') !== 'true') {
    await userEvent.click(currentFolder());
  }

  await waitFor(() => expect(currentFolder()).toHaveAttribute('aria-selected', 'true'));
  return within(canvas.getByTestId('foldered-draggable-item-row'));
}

// Clicking the visibility toggle right after a drag is flaky (dnd-kit's pointer
// sensor can swallow the first click), so retry until the layout settles.
async function toggleSideVisibility(
  canvas: ReturnType<typeof within>,
  side: 'Input' | 'Output',
  settled: () => void | Promise<void>,
) {
  for (let attempt = 0; attempt < 3; attempt += 1) {
    fireEvent.click(canvas.getByTestId(`provenance-side-visibility-${side}`));
    try {
      await waitFor(async () => await settled(), { timeout: 1500 });
      return;
    } catch {
      // Retry the click on the next iteration.
    }
  }

  await waitFor(async () => await settled());
}

async function createLayer(canvas: ReturnType<typeof within>, name: string) {
  await userEvent.click(canvas.getByTestId('provenance-add-layer'));
  const dialog = within(document.body);
  const input = await waitFor(() => dialog.getByRole('textbox', { name: 'Layer name' }));
  await userEvent.clear(input);
  await userEvent.type(input, name);
  await userEvent.click(dialog.getByRole('button', { name: 'Create layer' }));
}

function layerPageIds(canvas: ReturnType<typeof within>) {
  return Array.from(
    canvas
      .getByTestId('provenance-layer-pages')
      .querySelectorAll<HTMLElement>('[data-provenance-layer-page]'),
  ).map((page) => page.getAttribute('data-provenance-layer-page'));
}

async function shelfProperty(canvas: ReturnType<typeof within>, propertyName: string) {
  const name = new RegExp(`^Drag ${escapeRegExp(propertyName)}$`);

  for (const folder of shelfFolders(canvas)) {
    const row = await openShelfFolder(canvas, folder);
    const item = row.queryAllByRole('button', { name })[0];

    if (item) {
      return item;
    }
  }

  throw new Error(`Could not find shelf property "${propertyName}".`);
}

async function ensurePropertyInRail(
  canvas: ReturnType<typeof within>,
  side: 'Input' | 'Output',
  propertyName: string,
) {
  const propertyId = `provenance-property-${side}-${propertyName}`;
  const existing = canvas.queryByTestId(propertyId);

  if (existing) {
    return existing;
  }

  const source = await shelfProperty(canvas, propertyName);
  await dragByPointer(source, canvas.getByTestId(`provenance-property-rail-${side}`));

  await waitFor(() => expect(canvas.queryByTestId('foldered-draggable-drag-overlay')).not.toBeInTheDocument(), {timeout: 10_000});
  return waitFor(() => canvas.getByTestId(propertyId), {timeout: 10_000});
}

function propertyColorSwatch(property: HTMLElement) {
  const swatch = property.querySelector<HTMLElement>('span[style*="background-color"]');

  if (!swatch) {
    throw new Error(`Property "${property.getAttribute('aria-label') ?? property.textContent}" has no color swatch.`);
  }

  return swatch;
}

async function setFolderPreviewColor(canvas: ReturnType<typeof within>, folder: HTMLElement, color: string) {
  // The color control sits in the active card's header, so the folder's tab
  // must be active first.
  await openShelfFolder(canvas, folder);
  const card = canvas.getByTestId('foldered-draggable-card');
  const trigger = within(card).getByRole('button', { name: /^Set color for folder / });
  const triggerLabel = trigger.getAttribute('aria-label') ?? '';
  const inputLabel = triggerLabel.replace(/^Set /, 'Choose ');

  await userEvent.click(trigger);
  const body = within(document.body);
  const colorInput = await waitFor(() => body.getByLabelText(inputLabel));
  fireEvent.change(colorInput, { target: { value: color } });
  await userEvent.click(body.getByRole('button', { name: 'Select' }));

  await waitFor(() => expect(trigger).toHaveStyle({ backgroundColor: color }));
}

async function showPropertyControls(
  canvas: ReturnType<typeof within>,
  side: 'Input' | 'Output',
  propertyName: string,
) {
  const property = await ensurePropertyInRail(canvas, side, propertyName);
  property.focus();
  await userEvent.hover(property);

  const controls = canvas.getByTestId(`provenance-property-both-${side}-${propertyName}`).parentElement!;
  await waitFor(() => expect(controls).not.toHaveClass('swt:hidden'));

  return property;
}

async function shelfPropertyOrder(canvas: ReturnType<typeof within>) {
  const folder = canvas.getByTestId('foldered-draggable-folder-source-fixture-assay-table');
  await openShelfFolder(canvas, folder);

  const assignmentLabels = Array.from(
    canvas
      .getByTestId('foldered-draggable-item-row')
      .querySelectorAll<HTMLElement>('[data-testid^="foldered-draggable-item-"]'),
  ).map((element) => (element.getAttribute('aria-label') ?? '').replace(/^Drag\s+/, ''));

  // The canonical shelf is assignment-granular, so several exact assignments
  // can share a displayed property name. This helper measures header ordering.
  return Array.from(new Set(assignmentLabels));
}

async function expandProperty(canvas: ReturnType<typeof within>, side: 'Input' | 'Output', propertyName: string) {
  const panelId = `provenance-property-values-${side}-${propertyName}`;
  const triggerId = `provenance-property-expand-${side}-${propertyName}`;
  await ensurePropertyInRail(canvas, side, propertyName);
  // The row controls only enter the layout while the row is hovered or focused.
  await userEvent.hover(canvas.getByTestId(`provenance-property-${side}-${propertyName}`));
  for (let attempt = 0; attempt < 3 && !canvas.queryByTestId(panelId); attempt += 1) {
    await userEvent.click(canvas.getByTestId(triggerId));
    await waitFor(() => expect(canvas.getByTestId(panelId)).toBeInTheDocument(), { timeout: 1000 }).catch(() => {
      if (!canvas.queryByTestId(panelId)) {
        fireEvent.click(canvas.getByTestId(triggerId));
      }
    });
  }
  await waitFor(() => expect(canvas.getByTestId(panelId)).toBeInTheDocument(), { timeout: 3000 });
  return within(canvas.getByTestId(panelId));
}

// -- Finding a group card ---------------------------------------------------
// A group id is generated and display-only ("group IDs never enter writeback",
// design §8.4; §1.1 forbids asserting internal shape), so no story selects a
// card by id. A card is addressed by the title the user reads on it: the
// "Header: Value" organizer tabs joined by ", " while it is grouped, and its
// endpoint name while it is not. That exact title is also the accessible name of
// the card's own select checkbox, so one lookup serves both shapes and a
// composite grouping key needs no special case.

function groupCards(container: HTMLElement, side: Side): HTMLElement[] {
  return Array.from(
    container.querySelectorAll<HTMLElement>(`article[data-provenance-group-node^="provenance-node::${side}::"]`),
  );
}

function groupCardTitle(card: HTMLElement): string {
  const label = card.querySelector('input[type="checkbox"]')?.getAttribute('aria-label') ?? '';
  return label.replace(/^(?:Des|S)elect group /, '');
}

function queryGroupCard(container: HTMLElement, side: Side, title: string): HTMLElement | null {
  return groupCards(container, side).find((card) => groupCardTitle(card) === title) ?? null;
}

/** Throws listing the titles actually on that side, so a mismatch names itself. */
function getGroupCard(container: HTMLElement, side: Side, title: string): HTMLElement {
  const card = queryGroupCard(container, side, title);
  if (card) return card;
  const present = groupCards(container, side).map((each) => `"${groupCardTitle(each)}"`);
  throw new Error(`No ${side} card titled "${title}". Present: ${present.join(', ') || '(none)'}`);
}

function groupCardTitles(container: HTMLElement, side: Side): string[] {
  return groupCards(container, side).map(groupCardTitle);
}

/** The organizer tab for one grouping value of a card. */
function groupCardTab(card: HTMLElement, label: string): HTMLElement {
  return within(card).getByRole('button', { name: label });
}

/** The collapsed member-type preview inside a card. */
function groupCardSymbols(card: HTMLElement): HTMLElement | null {
  return card.querySelector<HTMLElement>('[data-testid^="provenance-group-symbols-"]');
}

/**
 * The generated id of a card already found by title. Connector keys are suffixed
 * with what they target - `…:{groupId}` for the card, `…:{groupId}:{memberId}`
 * for a member - so asserting *what a connector points at* needs the id. This is
 * the only use of it: no story looks a card up by id.
 */
function groupCardId(card: HTMLElement): string {
  const raw = (card.getAttribute('data-provenance-group-node') ?? '').replace(
    /^provenance-node::(?:Input|Output)::/,
    '',
  );
  return decodeURIComponent(raw);
}

async function groupByProperty(canvasElement: HTMLElement, side: Side, propertyName: string) {
  const canvas = within(canvasElement);
  // Grouping by a header rewrites the cards into one per distinct value of it,
  // so the header appearing on a card tab is the signal that it took effect.
  const grouped = () =>
    groupCards(canvasElement, side).filter((card) => groupCardTitle(card).includes(`${propertyName}: `));

  await ensurePropertyInRail(canvas, side, propertyName);

  for (let attempt = 0; attempt < 3 && grouped().length === 0; attempt += 1) {
    fireEvent.click(canvas.getByTestId(`provenance-property-${side}-${propertyName}`));
    await waitFor(() => expect(grouped().length).toBeGreaterThan(0), {
      timeout: 1000,
    }).catch(() => undefined);
  }

  await waitFor(() => expect(grouped().length).toBeGreaterThan(0), { timeout: 3000 });
}

async function selectGroup(groupCard: HTMLElement) {
  for (let attempt = 0; attempt < 3 && !groupCard.classList.contains('swt:border-primary'); attempt += 1) {
    const checkbox = groupCard.querySelector<HTMLElement>('input[type="checkbox"]')!;
    await userEvent.click(checkbox);
    await waitFor(() => expect(groupCard).toHaveClass('swt:border-primary'), { timeout: 1000 }).catch(() => undefined);
  }

  await waitFor(() => expect(groupCard).toHaveClass('swt:border-primary'), { timeout: 3000 });
}

async function railValue(
  canvas: ReturnType<typeof within>,
  side: 'Input' | 'Output',
  propertyName: string,
  valueText: string,
) {
  const panel = await expandProperty(canvas, side, propertyName);
  const value = await waitFor(() => panel.getAllByText(valueText)[0]?.closest('button, [role="button"]'));
  expect(value).toBeTruthy();
  return value!;
}

async function addRailValue(
  canvas: ReturnType<typeof within>,
  side: 'Input' | 'Output',
  propertyName: string,
  valueText: string,
  valueType = 'Text',
) {
  const panel = await expandProperty(canvas, side, propertyName);
  const valueLabel = new RegExp(`${escapeRegExp(propertyName)} value`, 'i');
  // Late in the loaded full-suite run the AddValuePopover trigger occasionally
  // needs a second activation before its portal form mounts (the first popover
  // still animating out from a prior add). Retry opening it until the value input
  // exists instead of assuming a single click landed.
  for (let attempt = 0; attempt < 3 && !screen.queryByRole('textbox', { name: valueLabel }); attempt += 1) {
    await userEvent.click(panel.getByText('Add value'));
    await waitFor(() => expect(screen.getByRole('textbox', { name: valueLabel })).toBeInTheDocument(), {
      timeout: 1000,
    }).catch(() => undefined);
  }
  if (valueType !== 'Text') {
    await userEvent.selectOptions(screen.getByRole('combobox', { name: /Value type/i }), valueType);
  }
  await userEvent.type(screen.getByRole('textbox', { name: valueLabel }), valueText);
  const submit = screen
    .getAllByRole('button', { name: /^Add value$/i })
    .find((button) => button.getAttribute('type') === 'submit')!;
  await userEvent.click(submit);
  await userEvent.keyboard('{Escape}');
  return railValue(canvas, side, propertyName, valueText);
}

async function addRailProperty(
  canvas: ReturnType<typeof within>,
  side: 'Input' | 'Output',
  propertyName: string,
  valueText: string,
  scope?: 'node' | 'process',
) {
  const rail = within(canvas.getByTestId(`provenance-property-rail-${side}`));
  const addPropertyTrigger = within(rail.getByTestId('popover_trigger_provenance-add-value-Annotation'))
    .getByText('Add annotation')
    .closest('button')!;
  fireEvent.click(addPropertyTrigger);
  const category = await waitFor(() => screen.getAllByTestId('term-search-input')[0]).catch(async () => {
    fireEvent.click(addPropertyTrigger);
    return waitFor(() => screen.getAllByTestId('term-search-input')[0]);
  });
  if (scope) {
    await userEvent.click(screen.getByTestId(`provenance-draft-scope-${scope}`));
  }
  // One controlled change avoids userEvent continuing to type into the old
  // TermSearch input after the category-dependent popover content rerenders.
  fireEvent.change(category, { target: { value: propertyName } });
  const currentCategory = await waitFor(() => {
    const input = screen.getAllByTestId('term-search-input')[0];
    expect(input).toHaveValue(propertyName);
    return input;
  });
  // Close the nested term-search list only after it reports itself open. A
  // document-level Escape sent before that point can dismiss the outer form.
  await waitFor(() => expect(currentCategory).toHaveAttribute('aria-expanded', 'true'));
  fireEvent.keyDown(currentCategory, { key: 'Escape', code: 'Escape' });
  await waitFor(() =>
    expect(screen.getAllByTestId('term-search-input')[0]).toHaveAttribute('aria-expanded', 'false'),
  );
  const valueInput = await waitFor(() =>
    screen.getByRole('textbox', { name: new RegExp(`${propertyName} value`, 'i') }),
  );
  await userEvent.type(valueInput, valueText);
  const submit = screen
    .getAllByRole('button', { name: /^Add annotation$/i })
    .find((button) => button.getAttribute('type') === 'submit')!;
  await userEvent.click(submit);
  await userEvent.keyboard('{Escape}');
  return railValue(canvas, side, propertyName, valueText);
}

export const AppliesRailValueToSelectedGroupsByClick: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // Without a selection the chips offer no click-apply action.
    const before = await railValue(canvas, 'Output', 'Analysis', 'Mass Spectrometry');
    expect(within(before as HTMLElement).queryByRole('button', { name: /apply to/i })).not.toBeInTheDocument();

    await selectGroup(canvas.getByText('Output D').closest('article')!);
    await selectGroup(canvas.getByText('Output E').closest('article')!);

    const source = await railValue(canvas, 'Output', 'Analysis', 'Mass Spectrometry');
    await userEvent.click(
      within(source as HTMLElement).getByRole('button', { name: /apply to 2 selected groups/i }),
    );

    // Applying to more than one group goes through the fan-out confirmation.
    await waitFor(() => expect(canvas.getByTestId('provenance-apply-batch-prompt')).toBeInTheDocument());
    await userEvent.click(canvas.getByTestId('provenance-confirm-apply'));

    await waitFor(() =>
      expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('ProcessAssignmentAdded'),
    );
  },
};

export const CopiesValueOntoAGroup: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const source = await railValue(canvas, 'Output', 'Analysis', 'Mass Spectrometry');
    const target = canvas.getByText('Output D').closest('article')!;

    await dragByPointer(source, target);

    await waitFor(() => {
      expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('ProcessAssignmentAdded');
    });
  },
};

export const ResizesThreePanelLayout: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const surface = canvas.getByTestId('provenance-surface');
    const leftSplitter = canvas.getByTestId('provenance-left-splitter');
    const before = surface.getAttribute('style');
    const surfaceRect = surface.getBoundingClientRect();
    const splitterRect = leftSplitter.getBoundingClientRect();

    fireEvent.pointerDown(leftSplitter, {
      clientX: splitterRect.left + 2,
      clientY: splitterRect.top + 8,
      button: 0,
      buttons: 1,
      isPrimary: true,
      pointerId: 11,
    });
    fireEvent.pointerMove(document, {
      clientX: surfaceRect.left + surfaceRect.width * 0.32,
      clientY: splitterRect.top + 8,
      button: 0,
      buttons: 1,
      isPrimary: true,
      pointerId: 11,
    });
    fireEvent.pointerUp(document, {
      button: 0,
      buttons: 0,
      isPrimary: true,
      pointerId: 11,
    });

    await waitFor(() => expect(surface.getAttribute('style')).not.toEqual(before));
    expect(surface.getAttribute('style')).toContain('grid-template-columns');
  },
};

export const ConnectsGroups: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const input = canvas.getByText('Input C').closest('article')!;
    const output = canvas.getByText('Output E').closest('article')!;

    await dragByPointer(
      within(input).getByTestId('provenance-connection-handle-Input-GroupCard'),
      within(output).getByTestId('provenance-connection-handle-Output-GroupCard'),
    );

    await waitFor(() => {
      expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('ProcessLinkAdded');
    }, {timeout: 10_000});
    expect(canvas.queryByTestId('provenance-live-connection')).not.toBeInTheDocument();
  },
};

export const UndoRevertsLastChange: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    expect(canvas.getByTestId('provenance-undo')).toBeDisabled();

    const input = canvas.getByText('Input C').closest('article')!;
    const output = canvas.getByText('Output E').closest('article')!;
    const before = (await waitFor(() => {
      const connectors = canvas.getAllByTestId('provenance-connection');
      expect(connectors.length).toBeGreaterThan(0);
      return connectors;
    })).length;

    await dragByPointer(
      within(input).getByTestId('provenance-connection-handle-Input-GroupCard'),
      within(output).getByTestId('provenance-connection-handle-Output-GroupCard'),
    );
    await waitFor(() => expect(canvas.getAllByTestId('provenance-connection').length).toBeGreaterThan(before));

    expect(canvas.getByTestId('provenance-undo')).not.toBeDisabled();

    // fireEvent with a retry: toolbar reflow can move the button mid-click.
    for (
      let attempt = 0;
      attempt < 3 && !canvas.getByTestId('provenance-undo').hasAttribute('disabled');
      attempt += 1
    ) {
      fireEvent.click(canvas.getByTestId('provenance-undo'));
      await waitFor(() => expect(canvas.getByTestId('provenance-undo')).toBeDisabled(), {
        timeout: 1000,
      }).catch(() => undefined);
    }

    await waitFor(() => {
      expect(canvas.getByTestId('provenance-undo')).toBeDisabled();
      expect(canvas.queryAllByTestId('provenance-connection')).toHaveLength(before);
    });
  },
};

export const UndoRetractsPatchPreview: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('No mutations recorded.');

    const input = canvas.getByText('Input C').closest('article')!;
    const output = canvas.getByText('Output E').closest('article')!;

    await dragByPointer(
      within(input).getByTestId('provenance-connection-handle-Input-GroupCard'),
      within(output).getByTestId('provenance-connection-handle-Output-GroupCard'),
    );

    await waitFor(() => {
      expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('ProcessLinkAdded');
    });

    for (
      let attempt = 0;
      attempt < 3 && !canvas.getByTestId('provenance-undo').hasAttribute('disabled');
      attempt += 1
    ) {
      fireEvent.click(canvas.getByTestId('provenance-undo'));
      await waitFor(() => expect(canvas.getByTestId('provenance-undo')).toBeDisabled(), {
        timeout: 1000,
      }).catch(() => undefined);
    }

    // The patch preview reads the session's own PatchLog, so undoing the
    // connect (which restores the pre-edit session snapshot) must retract the
    // patch from the preview too, not just from the model.
    await waitFor(() => {
      expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('No mutations recorded.');
    });
  },
};

export const ExternalSessionReplacementDisablesUndo: Story = {
  render: () => <Harness fixture="typedSample" allowTermReplacement />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    expect(canvas.getByTestId('provenance-undo')).toBeDisabled();

    const connector = await waitFor(() => canvas.getAllByTestId('provenance-connection')[0]);
    connector.focus();
    await userEvent.keyboard('{Delete}');

    await waitFor(() => expect(canvas.getByTestId('provenance-undo')).not.toBeDisabled());
    await waitFor(() =>
      expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('ProcessLinkRemoved'),
    );

    // The host replaces the session prop directly (not through onChange) -
    // the undo snapshot refers to a session the host has already discarded,
    // so it must be invalidated rather than left able to resurrect it.
    await userEvent.click(canvas.getByRole('button', { name: /Replace term metadata/i }));

    await waitFor(() => expect(canvas.getByTestId('provenance-undo')).toBeDisabled());

    // Design §3.4: the controlled session owns the journal, the editor has no
    // private copy - the preview reflects the replacement session's own
    // (empty) journal, not a lingering copy of the discarded delete.
    expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('No mutations recorded.');
  },
};

export const IgnoresConnectionHandleDroppedOnCardBody: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const input = canvas.getByText('Input C').closest('article')!;
    const output = canvas.getByText('Output E').closest('article')!;
    const initialLineCount = await waitFor(() => {
      const lines = canvas.queryAllByTestId('provenance-connection');
      expect(lines.length).toBeGreaterThan(0);
      return lines.length;
    });

    await dragByPointer(
      within(input).getByTestId('provenance-connection-handle-Input-GroupCard'),
      output,
    );

    await waitFor(() => expect(canvas.queryByTestId('provenance-live-connection')).not.toBeInTheDocument());
    expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('No mutations recorded.');
    expect(canvas.queryAllByTestId('provenance-connection')).toHaveLength(initialLineCount);
  },
};

export const InvalidSameSideConnectionDropIsIgnored: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const inputA = canvas.getByText('Input A').closest('article')!;
    const inputB = canvas.getByText('Input B').closest('article')!;
    const initialLines = canvas.queryAllByTestId('provenance-connection').length;

    await dragByPointer(
      within(inputA).getByTestId('provenance-connection-handle-Input-GroupCard'),
      inputB,
    );

    await waitFor(() => expect(canvas.queryAllByTestId('provenance-connection')).toHaveLength(initialLines));
    expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('No mutations recorded.');
  },
};

export const MismatchedGroupConnectionPromptsForResolution: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await groupByProperty(canvasElement, 'Output', 'Species');

    const inputGroup = canvas.getByText('Input D').closest('article')!;
    const outputGroup = await waitFor(() => getGroupCard(canvasElement, 'Output', 'Species: Arabidopsis'));
    // This retained story proves mismatch resolution and the absence of a
    // canonical mutation, not pointer collision. The supported click-to-connect
    // path keeps that intent deterministic under the complete browser suite.
    await userEvent.click(within(inputGroup).getByTestId('provenance-connection-handle-Input-GroupCard'));
    await userEvent.click(within(outputGroup).getByTestId('provenance-connection-handle-Output-GroupCard'));

    await waitFor(() => expect(canvas.getByTestId('provenance-member-resolution-prompt')).toBeInTheDocument());
    expect(canvas.getByTestId('provenance-member-resolution-prompt')).toHaveTextContent('1 input member');
    expect(canvas.getByTestId('provenance-member-resolution-prompt')).toHaveTextContent('3 output members');
  },
};

export const EqualCountGroupConnectionOffersPairByOrder: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // Grouping Species on both sides yields two 3-member Arabidopsis groups.
    for (
      let attempt = 0;
      attempt < 3 && !queryGroupCard(canvasElement, 'Input', 'Species: Arabidopsis');
      attempt += 1
    ) {
      await showPropertyControls(canvas, 'Output', 'Species');
      fireEvent.click(canvas.getByTestId('provenance-property-both-Output-Species'));
      await waitFor(() => expect(queryGroupCard(canvasElement, 'Input', 'Species: Arabidopsis')).toBeInTheDocument(), {
        timeout: 1000,
      }).catch(() => undefined);
    }

    const inputGroup = await waitFor(() => getGroupCard(canvasElement, 'Input', 'Species: Arabidopsis'));
    const outputGroup = await waitFor(() => getGroupCard(canvasElement, 'Output', 'Species: Arabidopsis'));

    await dragByPointer(
      within(inputGroup).getByTestId('provenance-connection-handle-Input-GroupCard'),
      within(outputGroup).getByTestId('provenance-connection-handle-Output-GroupCard'),
    );

    // Equal counts are not connected silently; the prompt offers order pairing.
    const prompt = await waitFor(() => canvas.getByTestId('provenance-member-resolution-prompt'));
    expect(prompt).toHaveTextContent('3 input members');
    expect(prompt).toHaveTextContent('3 output members');

    // fireEvent with a retry: the floating prompt animates in, so a positioned
    // click can miss on slow runs.
    for (
      let attempt = 0;
      attempt < 3 && canvas.queryByTestId('provenance-member-resolution-prompt');
      attempt += 1
    ) {
      fireEvent.click(canvas.getByTestId('provenance-member-resolution-pair-by-order'));
      await waitFor(() => expect(canvas.queryByTestId('provenance-member-resolution-prompt')).not.toBeInTheDocument(), {
        timeout: 1000,
      }).catch(() => undefined);
    }

    // The three ordered pairs (input-a↔output-a, input-b↔output-b,
    // input-c↔output-c) are all already connected in the fixture, so pair-by-order
    // hits the connectSets duplicate guard: it resolves the prompt without
    // emitting a duplicate connection patch. Emitting ProcessLinkAdded here (as
    // this once asserted) would mean re-connecting an already-connected pair,
    // which the shared Edit layer deliberately makes a no-op.
    expect(canvas.queryByTestId('provenance-member-resolution-prompt')).not.toBeInTheDocument();
    expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('No mutations recorded.');
  },
};

export const PairingUsesLayerOrderPosition: Story = {
  render: () => <Harness fixture="layerOrder" />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await ensurePropertyInRail(canvas, 'Input', 'Species');
    await showPropertyControls(canvas, 'Input', 'Species');

    const bothSidesGrouped = () =>
      queryGroupCard(canvasElement, 'Input', 'Species: Shared') !== null
      && queryGroupCard(canvasElement, 'Output', 'Species: Shared') !== null;

    for (let attempt = 0; attempt < 3 && !bothSidesGrouped(); attempt += 1) {
      fireEvent.click(canvas.getByTestId('provenance-property-both-Input-Species'));
      await waitFor(() => expect(bothSidesGrouped()).toBe(true), { timeout: 1000 }).catch(() => undefined);
    }

    const inputGroup = await waitFor(() => getGroupCard(canvasElement, 'Input', 'Species: Shared'));
    const outputGroup = await waitFor(() => getGroupCard(canvasElement, 'Output', 'Species: Shared'));
    // This story is about deterministic member pairing, not pointer hit
    // testing. Use the supported click-to-connect path so the assertion is
    // isolated to LayerOrderPosition semantics.
    await userEvent.click(within(inputGroup).getByTestId('provenance-connection-handle-Input-GroupCard'));
    await userEvent.click(within(outputGroup).getByTestId('provenance-connection-handle-Output-GroupCard'));

    await userEvent.click(await waitFor(() => canvas.getByTestId('provenance-member-resolution-pair-by-order')));
    await waitFor(() => {
      const links = (canvas.getByTestId('provenance-mutation-preview').textContent ?? '')
        .split('\n')
        .filter((line) => line.startsWith('ProcessLinkAdded:'));
      expect(links).toEqual([
        'ProcessLinkAdded:node-input-z->node-output-z',
        'ProcessLinkAdded:node-input-a->node-output-a',
      ]);
    });
  },
};

export const ManualMismatchResolutionExpandsMembersWithoutPatches: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await groupByProperty(canvasElement, 'Output', 'Species');

    const inputGroup = canvas.getByText('Input D').closest('article')!;
    const outputGroup = await waitFor(() => getGroupCard(canvasElement, 'Output', 'Species: Arabidopsis'));
    // Manual resolution is the behavior under test. Use the same supported
    // click-to-connect path as the pairing story so pointer hit-testing cannot
    // prevent the mismatch prompt under full-suite load.
    await userEvent.click(within(inputGroup).getByTestId('provenance-connection-handle-Input-GroupCard'));
    await userEvent.click(within(outputGroup).getByTestId('provenance-connection-handle-Output-GroupCard'));
    await waitFor(() => expect(canvas.getByTestId('provenance-member-resolution-prompt')).toBeInTheDocument());
    await userEvent.click(canvas.getByTestId('provenance-member-resolution-manual'));

    // Exactly the two cards that were about to be connected open with their
    // member handles; other groups connected to them stay collapsed.
    await waitFor(() => {
      const currentInputGroup = getGroupCard(canvasElement, 'Input', 'Input D');
      const currentOutputGroup = getGroupCard(canvasElement, 'Output', 'Species: Arabidopsis');
      expect(within(currentInputGroup).getAllByTestId('provenance-connection-handle-Input-GroupMember').length).toBeGreaterThan(0);
      expect(within(currentOutputGroup).getAllByTestId('provenance-connection-handle-Output-GroupMember').length).toBeGreaterThan(0);
    });

    const otherOutputGroup = getGroupCard(canvasElement, 'Output', 'Species: Chlamydomonas');
    expect(within(otherOutputGroup).queryByTestId('provenance-connection-handle-Output-GroupMember')).not.toBeInTheDocument();
    expect(canvas.queryByTestId('provenance-member-resolution-prompt')).not.toBeInTheDocument();
    expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('No mutations recorded.');

    // A follow-up hint explains how to connect members individually.
    const hint = canvas.getByTestId('provenance-hint');
    expect(hint).toHaveTextContent(/connection handle/i);
    await userEvent.click(canvas.getByTestId('provenance-hint-dismiss'));
    await waitFor(() => expect(canvas.queryByTestId('provenance-hint')).not.toBeInTheDocument());
  },
};

export const ExpandedGroupedCardsDoNotExpandConnectedSingleCards: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await groupByProperty(canvasElement, 'Output', 'Species');

    const inputA = canvas.getByText('Input A').closest('article')!;
    const outputGroup = await waitFor(() => getGroupCard(canvasElement, 'Output', 'Species: Arabidopsis'));

    await userEvent.click(within(outputGroup).getByRole('button', { name: 'Show members' }));

    await waitFor(() => {
      expect(within(outputGroup).getAllByTestId('provenance-connection-handle-Output-GroupMember').length).toBeGreaterThan(0);
      expect(within(inputA).queryByTestId('provenance-group-member-Input-node-input-a')).not.toBeInTheDocument();
      expect(within(inputA).queryByTestId('provenance-connection-handle-Input-GroupMember')).not.toBeInTheDocument();
    });
  },
};

export const LayerTabsUseSourceColorsAndSideRails: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    const layer1 = canvas.getByTestId('provenance-layer-layer-1');
    const layerColor = layer1.getAttribute('data-provenance-layer-color') ?? '';

    expect(layer1).toHaveClass('swt:btn-primary');
    expect(layerColor).toMatch(/^#/);
    expect(layerColor).not.toContain('|');

    expect(canvas.getByTestId('provenance-property-rail-Input')).toHaveAttribute(
      'data-provenance-side-id',
      'layer-1-input',
    );
    expect(canvas.getByTestId('provenance-property-rail-Output')).toHaveAttribute(
      'data-provenance-side-id',
      'layer-1-output',
    );
  },
};

export const LayerPaginationUsesNeighborWindowAndArrowSwitches: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await createLayer(canvas, 'Layer 2');
    await createLayer(canvas, 'Layer 3');

    const pagination = within(canvas.getByTestId('provenance-layer-pagination'));
    expect(canvas.queryByTestId('provenance-layer-select')).not.toBeInTheDocument();
    expect(pagination.getByTestId('provenance-add-layer')).toBeInTheDocument();

    await waitFor(() => {
      expect(layerPageIds(canvas)).toEqual(['layer-1', 'layer-2', 'layer-3']);
      expect(canvas.getByTestId('provenance-layer-layer-3')).toHaveClass('swt:btn-primary');
    });
    // The jump trigger doubles as the layer position indicator.
    expect(pagination.getByTestId('provenance-layer-jump')).toHaveTextContent('3 / 3');
    expect(canvas.getByTestId('provenance-layer-layer-2')).toHaveClass('swt:opacity-50');
    expect(canvas.queryByTestId('provenance-layer-next')).not.toBeInTheDocument();
    expect(pagination.getByTestId('provenance-layer-prev').querySelector('[class*="fluent--chevron-left"]'))
      .toBeInTheDocument();

    await userEvent.click(pagination.getByTestId('provenance-layer-prev'));

    await waitFor(() => {
      expect(layerPageIds(canvas)).toEqual(['layer-1', 'layer-2', 'layer-3']);
      expect(canvas.getByTestId('provenance-layer-layer-2')).toHaveClass('swt:btn-primary');
    });
    expect(canvas.getByTestId('provenance-layer-layer-1')).toHaveClass('swt:opacity-50');
    expect(canvas.getByTestId('provenance-layer-layer-3')).toHaveClass('swt:opacity-50');
    expect(pagination.getByTestId('provenance-layer-next').querySelector('[class*="fluent--chevron-right"]'))
      .toBeInTheDocument();

    await userEvent.click(pagination.getByTestId('provenance-layer-prev'));

    await waitFor(() => {
      expect(layerPageIds(canvas)).toEqual(['layer-1', 'layer-2', 'layer-3']);
      expect(canvas.getByTestId('provenance-layer-layer-1')).toHaveClass('swt:btn-primary');
    });
    expect(pagination.getByTestId('provenance-layer-jump')).toHaveTextContent('1 / 3');
    expect(canvas.queryByTestId('provenance-layer-prev')).not.toBeInTheDocument();
    expect(pagination.getByTestId('provenance-layer-next').querySelector('[class*="fluent--chevron-right"]'))
      .toBeInTheDocument();
  },
};

export const AddsLayerFromMixedSelection: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const inputA = canvas.getByText('Input A').closest('article')!;
    const outputB = canvas.getByText('Output B').closest('article')!;

    await selectGroup(inputA);
    await selectGroup(outputB);
    await createLayer(canvas, 'Layer 2');

    await waitFor(() => {
      expect(canvas.getByTestId('provenance-layer-layer-2')).toHaveClass('swt:btn-primary');
      expect(canvasElement).toHaveTextContent('Input A');
      expect(canvasElement).toHaveTextContent('Output B');
    });
  },
};

export const AddLayerPopoverAnnouncesSeedEntities: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // Without a selection the new layer continues from all outputs by default.
    await userEvent.click(canvas.getByTestId('provenance-add-layer'));
    const dialog = within(document.body);
    await waitFor(() =>
      expect(dialog.getByTestId('provenance-layer-seed-summary')).toHaveTextContent(
        /Starts from all \d+ outputs of this layer \(default\)/,
      ),
    );
    await userEvent.keyboard('{Escape}');

    // With a selection the popover names the selected groups and entity count.
    const outputA = canvas.getByText('Output A').closest('article')!;
    await selectGroup(outputA);
    await userEvent.click(canvas.getByTestId('provenance-add-layer'));
    await waitFor(() =>
      expect(dialog.getByTestId('provenance-layer-seed-summary')).toHaveTextContent('Starts from 1 selected group (1 entity).'),
    );
    await userEvent.keyboard('{Escape}');
  },
};

export const CreatesNamedLayer: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await createLayer(canvas, 'Extraction');

    await waitFor(() => {
      const layer = canvas.getByTestId('provenance-layer-layer-2');
      expect(layer).toHaveClass('swt:btn-primary');
      expect(layer).toHaveAccessibleName('View provenance layer Extraction');
      expect(layer).toHaveTextContent('Extraction');
    });
  },
};

export const DoesNotReuseSelectionForEqualGroupIdsInDifferentLayers: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const outputA = canvas.getByText('Output A').closest('article')!;

    await selectGroup(outputA);
    await createLayer(canvas, 'Layer 2');

    await userEvent.click(canvas.getByTestId('popover_trigger_provenance-add-output'));
    await userEvent.type(screen.getByRole('textbox', { name: /Endpoint name/i }), 'Layer 2 Output');
    await userEvent.click(screen.getByRole('button', { name: /Create endpoint/i }));
    const layer2Output = await waitFor(() => canvas.getByText('Layer 2 Output').closest('article')!);

    await selectGroup(layer2Output);
    await createLayer(canvas, 'Layer 3');

    await userEvent.click(canvas.getByTestId('popover_trigger_provenance-add-output'));
    await userEvent.type(screen.getByRole('textbox', { name: /Endpoint name/i }), 'Layer 3 Output');
    await userEvent.click(screen.getByRole('button', { name: /Create endpoint/i }));
    const layer3Output = await waitFor(() => canvas.getByText('Layer 3 Output').closest('article')!);

    expect(layer3Output).not.toHaveClass('swt:border-primary');
  },
};

export const StrictModeSmoke: Story = {
  // React.StrictMode double-invokes renders (and, in the relevant React
  // versions, effects) in development - the closest browser-testable proxy
  // for a render being committed twice or discarded. Render-phase writes to
  // "latest" refs would show up here as duplicated patch lines from a single
  // user action.
  render: () => (
    <React.StrictMode>
      <Harness />
    </React.StrictMode>
  ),
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const source = await addRailValue(canvas, 'Output', 'Analysis', 'Imaging');
    await groupByProperty(canvasElement, 'Output', 'Analysis');
    const outputD = canvas.getByText('Output D').closest('article')!;

    await dragByPointer(source, outputD);

    await waitFor(() => {
      const preview = canvas.getByTestId('provenance-mutation-preview').textContent ?? '';
      const addLines = preview.split('\n').filter((line) => line.startsWith('ProcessAssignmentAdded:'));
      expect(addLines).toHaveLength(1);
    });
    expect(getGroupCard(canvasElement, 'Output', 'Analysis: Imaging')).toBeInTheDocument();

    await waitFor(() => expect(canvas.getByTestId('provenance-undo')).not.toBeDisabled());

    // fireEvent with a retry (as elsewhere in this file): late in the loaded
    // suite the first undo click can land during a toolbar reflow and miss.
    // Undo is single-step, so once it takes the button disables and extra
    // clicks are safe no-ops.
    for (
      let attempt = 0;
      attempt < 3 && !canvas.getByTestId('provenance-undo').hasAttribute('disabled');
      attempt += 1
    ) {
      fireEvent.click(canvas.getByTestId('provenance-undo'));
      await waitFor(() => expect(canvas.getByTestId('provenance-undo')).toBeDisabled(), {
        timeout: 1000,
      }).catch(() => undefined);
    }

    await waitFor(() => {
      expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('No mutations recorded.');
    });
  },
};

export const OpensInteractiveTutorialOnSampleData: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(await canvas.findByTestId('provenance-tutorial-trigger'));

    const modal = within(canvas.getByTestId('provenance-tutorial-modal'));
    expect(modal.getByText('Provenance editor tour')).toBeInTheDocument();
    expect(within(modal.getByTestId('tutorial-step-card')).getByText('Welcome')).toBeInTheDocument();

    // The sandboxed editor must not offer a tutorial of its own (no nesting).
    expect(modal.queryByTestId('provenance-tutorial-trigger')).not.toBeInTheDocument();

    // The feature list jumps straight to any step's explanation; the sandbox
    // remounts at that step's checkpoint, so the state its task needs (here:
    // inputs already grouped by Species) exists without doing earlier steps.
    await userEvent.click(modal.getByTestId('tutorial-sidebar-step-members'));
    expect(within(modal.getByTestId('tutorial-step-card')).getByText('Inspect group members')).toBeInTheDocument();
    await waitFor(() =>
      expect(getGroupCard(canvas.getByTestId('provenance-tutorial-modal'), 'Input', 'Species: Arabidopsis')).toBeInTheDocument(),
    );

    // Closing returns to the host editor without any writeback patches.
    await userEvent.click(modal.getByTestId('tutorial-close'));
    expect(canvas.queryByTestId('provenance-tutorial-modal')).not.toBeInTheDocument();
    expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('No mutations recorded.');
  },
};

export const TutorialTaskStepCompletesInsideSandbox: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(await canvas.findByTestId('provenance-tutorial-trigger'));
    const modal = within(canvas.getByTestId('provenance-tutorial-modal'));

    // Jump to the shelf-to-rail step and fulfil it by dragging Species into
    // the sandbox's input rail; the polled condition marks the step completed
    // and Skip becomes Next. The modal's feature list narrows the editor into
    // the medium tier, so the rail sits behind its fold toggle first.
    await userEvent.click(modal.getByTestId('tutorial-sidebar-step-shelf-to-rail'));
    expect(modal.getByTestId('tutorial-next')).toHaveTextContent('Skip');
    if (!modal.queryByTestId('provenance-property-rail-Input')) {
      await userEvent.click(modal.getByTestId('provenance-rail-toggle-Input'));
    }
    const source = await shelfProperty(modal, 'Species');
    await dragByPointer(source, modal.getByTestId('provenance-property-rail-Input'));
    await waitFor(() => expect(modal.getByTestId('tutorial-next')).toHaveTextContent('Next'), { timeout: 5000 });
    expect(within(modal.getByTestId('tutorial-task')).getByText('Completed:')).toBeInTheDocument();
    await userEvent.click(modal.getByTestId('tutorial-next'));
    expect(within(modal.getByTestId('tutorial-step-card')).getByText('Group by an annotation')).toBeInTheDocument();

    // The click task completes in place as well; the user moves on themselves.
    await userEvent.click(modal.getByTestId('provenance-property-Input-Species'));
    await waitFor(() => expect(modal.getByTestId('tutorial-next')).toHaveTextContent('Next'), { timeout: 5000 });
    expect(modal.getByText('2 of 14 features explored')).toBeInTheDocument();
    await userEvent.click(modal.getByTestId('tutorial-next'));
    expect(within(modal.getByTestId('tutorial-step-card')).getByText('Inspect group members')).toBeInTheDocument();
  },
};

export const ChainedTablesLoadAsLinkedLayers: Story = {
  render: () => <Harness fixture="chained" />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // Both loaded tables render as layer tabs, first layer active, labeled
    // by each model's source name.
    await waitFor(() => expect(canvas.getByTestId('provenance-layer-layer-1')).toHaveClass('swt:btn-primary'));
    expect(canvas.getByTestId('provenance-layer-layer-1')).toHaveTextContent('growth-table');
    expect(canvas.getByTestId('provenance-layer-layer-2')).toHaveTextContent('measurement-table');

    // The active layer shows the growth table's own groups.
    expect(canvas.getByText('Seed Stock')).toBeInTheDocument();
    expect(canvas.getByText('Culture Batch')).toBeInTheDocument();
  },
};

export const ChainedLayerSwitchShowsEachTableAndStaysLossless: Story = {
  render: () => <Harness fixture="chained" />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await userEvent.click(canvas.getByTestId('provenance-layer-layer-2'));
    await waitFor(() => expect(canvas.getByTestId('provenance-layer-layer-2')).toHaveClass('swt:btn-primary'));

    // The measurement table renders its own sets; the shared boundary sample
    // appears on its input side.
    expect(canvas.getByText('Culture Batch')).toBeInTheDocument();
    expect(canvas.getByText('Extract Batch')).toBeInTheDocument();

    await userEvent.click(canvas.getByTestId('provenance-layer-layer-1'));
    await waitFor(() => expect(canvas.getByTestId('provenance-layer-layer-1')).toHaveClass('swt:btn-primary'));
    expect(canvas.getByText('Seed Stock')).toBeInTheDocument();
    expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('No mutations recorded.');
  },
};

export const ChainedSecondLayerEditSurvivesLayerSwitches: Story = {
  render: () => <Harness fixture="chainedAlternateAnalysis" />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await userEvent.click(canvas.getByTestId('provenance-layer-layer-2'));
    await waitFor(() => expect(canvas.getByTestId('provenance-layer-layer-2')).toHaveClass('swt:btn-primary'));

    // Reuse an upstream loaded Parameter so the target edit carries the same
    // concrete kind. A newly authored Generic process value is intentionally a
    // distinct kind-bearing entry and would be added rather than overwrite.
    const extractBatch = canvas.getByText('Extract Batch').closest('article')!;
    const source = await railValue(canvas, 'Input', 'Analysis', 'Imaging');
    await dragByPointer(source, extractBatch);

    await waitFor(() => expect(canvas.getByTestId('provenance-overwrite-warning')).toBeInTheDocument());
    await userEvent.click(canvas.getByTestId('provenance-confirm-overwrite'));

    await waitFor(() => {
      expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('ProcessAssignmentValueChanged');
      expect(canvas.queryByTestId('provenance-overwrite-warning')).not.toBeInTheDocument();
    });

    // The canonical journal survives switching back to the first loaded layer.
    // A controlled publish can replace the tab node under full-suite load, so
    // reacquire and retry until the real active-layer state changes.
    await waitFor(() => {
      fireEvent.click(canvas.getByTestId('provenance-layer-layer-1'));
      expect(canvas.getByTestId('provenance-layer-layer-1')).toHaveClass('swt:btn-primary');
    });
    await waitFor(() => expect(canvas.getByText('Seed Stock')).toBeInTheDocument());
    expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('ProcessAssignmentValueChanged');
  },
};

// -- K.1: editing, removal, ambiguity ---------------------------------------

export const OwnerSpecificEditDetachesOnlyThatAssignment: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // Input A, B and C all reference the same shared "Arabidopsis" value.
    // Overwriting only Input A with Input D's "Chlamydomonas" must detach
    // Input A's assignment, leaving B and C referencing the original.
    const source = await railValue(canvas, 'Input', 'Species', 'Chlamydomonas');
    const inputA = canvas.getByText('Input A').closest('article')!;
    await dragByPointer(source, inputA);

    await waitFor(() => expect(canvas.getByTestId('provenance-overwrite-warning')).toBeInTheDocument());
    await userEvent.click(canvas.getByTestId('provenance-confirm-overwrite'));

    await waitFor(() => {
      const preview = canvas.getByTestId('provenance-mutation-preview').textContent ?? '';
      const changed = preview.split('\n').filter((line) => line.startsWith('NodeAssignmentValueChanged:'));
      expect(changed).toHaveLength(1);
    });

    await groupByProperty(canvasElement, 'Input', 'Species');
    const arabidopsis = await waitFor(() => getGroupCard(canvasElement, 'Input', 'Species: Arabidopsis'));
    await userEvent.click(within(arabidopsis).getByRole('button', { name: 'Show members' }));
    expect(within(arabidopsis).getByTestId('provenance-group-member-Input-node-input-b')).toBeInTheDocument();
    expect(within(arabidopsis).getByTestId('provenance-group-member-Input-node-input-c')).toBeInTheDocument();
    expect(within(arabidopsis).queryByTestId('provenance-group-member-Input-node-input-a')).not.toBeInTheDocument();

    const chlamydomonas = await waitFor(() => getGroupCard(canvasElement, 'Input', 'Species: Chlamydomonas'));
    await userEvent.click(within(chlamydomonas).getByRole('button', { name: 'Show members' }));
    expect(within(chlamydomonas).getByTestId('provenance-group-member-Input-node-input-a')).toBeInTheDocument();
    expect(within(chlamydomonas).getByTestId('provenance-group-member-Input-node-input-d')).toBeInTheDocument();
  },
};

export const FreshDraftValueOnOwnedHeaderPromptsOverwrite: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // Regression: a freshly authored draft under the loaded "Species" entry
    // carries that entry's established concrete kind (intent §1), so dropping
    // it on Input A - which owns Species: Arabidopsis - is a same-header
    // conflict prompting the overwrite flow (intent §3), not a silently added
    // second assignment for the same header on the same owner.
    const source = await addRailValue(canvas, 'Input', 'Species', 'Nicotiana');
    const inputA = canvas.getByText('Input A').closest('article')!;
    await dragByPointer(source, inputA);

    await waitFor(() => expect(canvas.getByTestId('provenance-overwrite-warning')).toBeInTheDocument());
    await userEvent.click(canvas.getByTestId('provenance-confirm-overwrite'));

    await waitFor(() => {
      const preview = canvas.getByTestId('provenance-mutation-preview').textContent ?? '';
      expect(preview.split('\n').filter((line) => line.startsWith('NodeAssignmentValueChanged:'))).toHaveLength(1);
      expect(preview).not.toContain('NodeAssignmentAdded');
    });

    // The fresh value detached only Input A's assignment to a new value; the
    // sibling assignments on B and C keep referencing the original (intent §4).
    await groupByProperty(canvasElement, 'Input', 'Species');
    const arabidopsis = await waitFor(() => getGroupCard(canvasElement, 'Input', 'Species: Arabidopsis'));
    await userEvent.click(within(arabidopsis).getByRole('button', { name: 'Show members' }));
    expect(within(arabidopsis).getByTestId('provenance-group-member-Input-node-input-b')).toBeInTheDocument();
    expect(within(arabidopsis).getByTestId('provenance-group-member-Input-node-input-c')).toBeInTheDocument();
    expect(within(arabidopsis).queryByTestId('provenance-group-member-Input-node-input-a')).not.toBeInTheDocument();

    const nicotiana = await waitFor(() => getGroupCard(canvasElement, 'Input', 'Species: Nicotiana'));
    await userEvent.click(within(nicotiana).getByRole('button', { name: 'Show members' }));
    expect(within(nicotiana).getByTestId('provenance-group-member-Input-node-input-a')).toBeInTheDocument();
  },
};

export const ReverseLocalAnnotationIsReadOnlyAtTheReceivingInput: Story = {
  render: () => <Harness fixture="reverseLocal" />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await groupByProperty(canvasElement, 'Input', 'Outcome');

    // Output A owns "Outcome: Success"; Input A is its direct upstream
    // neighbour on a Between link, so it sees the value reflected backward
    // for grouping only (design §4). Input B has no connected output
    // annotation, so it never groups by it.
    const reflected = await waitFor(() => getGroupCard(canvasElement, 'Input', 'Outcome: Success'));
    expect(canvas.getByText('Input B').closest('article')).toBeInTheDocument();

    // The value groups the card, but this receiver can neither remove nor edit
    // it, so the menu offers neither: an entry that never responds reads as
    // broken. With nothing else actionable on this card, no menu opens at all.
    fireEvent.contextMenu(reflected, { clientX: 200, clientY: 200, bubbles: true });
    await waitFor(() => expect(screen.queryByTestId('context_menu')).not.toBeInTheDocument());
    expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('No mutations recorded.');
  },
};

export const ForwardPropagatedAnnotationIsReadOnlyAtTheReceivingOutput: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await groupByProperty(canvasElement, 'Output', 'Species');

    // Species is owned by the input nodes and only forward-propagated to the
    // outputs they connect to; the receiving output cannot remove it locally
    // and must be directed to the owning input instead (design §5).
    const propagated = await waitFor(() => getGroupCard(canvasElement, 'Output', 'Species: Arabidopsis'));
    fireEvent.contextMenu(propagated, { clientX: 200, clientY: 200, bubbles: true });
    const menu = await screen.findByTestId('context_menu');

    // The value is one entry carrying both actions. Removal is greyed out —
    // the receiver does not own it, and the hint says where it can be removed.
    // Editing stays live, because a forward-propagated reference resolves to
    // its origin (design §4), which distinguishes this from the reverse-local
    // case where nothing is actionable and no entry appears at all.
    const removeButton = within(menu).getByRole('button', {
      name: /Remove annotation: Species: Arabidopsis/i,
    });
    expect(removeButton).toBeDisabled();
    expect(removeButton.closest('span')).toHaveAttribute('title', expect.stringMatching(/another layer/i));
    expect(
      within(menu).getByRole('button', { name: /Edit annotation: Species: Arabidopsis/i }),
    ).toBeEnabled();

    await userEvent.keyboard('{Escape}');
    await waitFor(() => expect(screen.queryByTestId('context_menu')).not.toBeInTheDocument());
    expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('No mutations recorded.');
  },
};

export const UnambiguousPropagatedNodeAnnotationEditsItsOwnerDownstream: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // Output A sees Species only through Input A's forward-propagated link
    // (link-a), so it resolves to exactly one originating node assignment
    // (design §4): editing there updates Input A's assignment and does not
    // create ownership on Output A.
    const outputA = canvas.getByText('Output A').closest('article')!;
    fireEvent.contextMenu(outputA, { clientX: 200, clientY: 200, bubbles: true });
    await screen.findByTestId('context_menu');
    await clickMenuAction(/Edit annotation: Species: Arabidopsis/i);

    const valueInput = await waitFor(() => canvas.getByTestId('provenance-annotation-edit-value'));
    await userEvent.clear(valueInput);
    await userEvent.type(valueInput, 'Nicotiana');
    await userEvent.click(canvas.getByTestId('provenance-confirm-annotation-edit'));

    await waitFor(() => {
      const preview = canvas.getByTestId('provenance-mutation-preview').textContent ?? '';
      expect(preview.split('\n').filter((line) => line.startsWith('NodeAssignmentValueChanged'))).toHaveLength(1);
      expect(preview).not.toContain('NodeAssignmentAdded');
    });

    // Input A now owns the new value; Input B/C, unaffected by the edit,
    // still offer removal of the original.
    const inputA = canvas.getByText('Input A').closest('article')!;
    fireEvent.contextMenu(inputA, { clientX: 200, clientY: 200, bubbles: true });
    const inputAMenu = await screen.findByTestId('context_menu');
    expect(
      within(inputAMenu).getByRole('button', { name: /Remove annotation: Species: Nicotiana/i }),
    ).toBeInTheDocument();
    await userEvent.keyboard('{Escape}');
    await waitFor(() => expect(screen.queryByTestId('context_menu')).not.toBeInTheDocument());

    const inputB = canvas.getByText('Input B').closest('article')!;
    fireEvent.contextMenu(inputB, { clientX: 200, clientY: 200, bubbles: true });
    const inputBMenu = await screen.findByTestId('context_menu');
    expect(
      within(inputBMenu).getByRole('button', { name: /Remove annotation: Species: Arabidopsis/i }),
    ).toBeInTheDocument();
    await userEvent.keyboard('{Escape}');
    await waitFor(() => expect(screen.queryByTestId('context_menu')).not.toBeInTheDocument());

    // Output A reflects the edit through propagation but still owns nothing of
    // its own, so its removal is greyed out — only the edit stays live, which
    // resolves to the owner and is exactly what this story just used.
    fireEvent.contextMenu(outputA, { clientX: 200, clientY: 200, bubbles: true });
    const outputAMenu = await screen.findByTestId('context_menu');
    expect(
      within(outputAMenu).getByRole('button', { name: /Remove annotation: Species: Nicotiana/i }),
    ).toBeDisabled();
    expect(
      within(outputAMenu).getByRole('button', { name: /Edit annotation: Species: Nicotiana/i }),
    ).toBeEnabled();
  },
};

export const MultiOriginPropagatedNodeAnnotationBulkEditsEveryOrigin: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // Output B sees Species through two forward-propagated links (link-b from
    // Input A, link-c from Input B). Each origin resolves uniquely, so the
    // entity surface bulk-edits both owning assignments as one atomic command
    // (intent §4) - several uniquely resolvable origins are not ambiguity.
    const outputB = canvas.getByText('Output B').closest('article')!;
    fireEvent.contextMenu(outputB, { clientX: 200, clientY: 200, bubbles: true });
    const menu2 = await screen.findByTestId('context_menu');

    // Values from other layers read as foreign: they sit behind a divider,
    // and their origin is hover info on the entry rather than label text.
    expect(menu2.getElementsByClassName('swt:divider').length).toBeGreaterThan(0);
    const rowLabel = within(menu2).getByText('Species: Arabidopsis');
    expect(rowLabel).toHaveAttribute(
      'title',
      expect.stringMatching(/^From (Input A, Input B|Input B, Input A)$/),
    );
    // The anchored accessible name pins that the origin stays out of the label.
    await clickMenuAction(/^Edit annotation: Species: Arabidopsis$/);

    const valueInput = await waitFor(() => canvas.getByTestId('provenance-annotation-edit-value'));
    await userEvent.clear(valueInput);
    await userEvent.type(valueInput, 'Nicotiana');
    await userEvent.click(canvas.getByTestId('provenance-confirm-annotation-edit'));

    await waitFor(() => {
      const preview = canvas.getByTestId('provenance-mutation-preview').textContent ?? '';
      expect(preview.split('\n').filter((line) => line.startsWith('NodeAssignmentValueChanged'))).toHaveLength(2);
      expect(preview).not.toContain('NodeAssignmentAdded');
    });

    // Both origins now own the new value; Input C, whose link does not reach
    // Output B, keeps the original.
    for (const owner of ['Input A', 'Input B']) {
      const card = canvas.getByText(owner).closest('article')!;
      fireEvent.contextMenu(card, { clientX: 200, clientY: 200, bubbles: true });
      const ownerMenu = await screen.findByTestId('context_menu');
      expect(
        within(ownerMenu).getByRole('button', { name: /Remove annotation: Species: Nicotiana/i }),
      ).toBeInTheDocument();
      await userEvent.keyboard('{Escape}');
      await waitFor(() => expect(screen.queryByTestId('context_menu')).not.toBeInTheDocument());
    }

    const inputC = canvas.getByText('Input C').closest('article')!;
    fireEvent.contextMenu(inputC, { clientX: 200, clientY: 200, bubbles: true });
    const inputCMenu = await screen.findByTestId('context_menu');
    expect(
      within(inputCMenu).getByRole('button', { name: /Remove annotation: Species: Arabidopsis/i }),
    ).toBeInTheDocument();
    await userEvent.keyboard('{Escape}');
    await waitFor(() => expect(screen.queryByTestId('context_menu')).not.toBeInTheDocument());
  },
};

export const RemovesNodeAnnotationFromGroupCardContextMenu: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const inputD = canvas.getByText('Input D').closest('article')!;

    fireEvent.contextMenu(inputD, { clientX: 200, clientY: 200, bubbles: true });
    await screen.findByTestId('context_menu');
    await clickMenuAction(/Remove annotation: Species: Chlamydomonas/i);

    await waitFor(() => {
      const preview = canvas.getByTestId('provenance-mutation-preview').textContent ?? '';
      expect(preview.split('\n').filter((line) => line === 'NodeAssignmentRemoved')).toHaveLength(1);
    });

    // An unrelated owner of an equal-header value still owns its assignment
    // (intent §5): Input A's menu still offers its own Species removal.
    const inputA = canvas.getByText('Input A').closest('article')!;
    fireEvent.contextMenu(inputA, { clientX: 200, clientY: 200, bubbles: true });
    const menuA = await screen.findByTestId('context_menu');
    expect(
      within(menuA).getByRole('button', { name: /Remove annotation: Species: Arabidopsis/i }),
    ).toBeEnabled();
    await userEvent.keyboard('{Escape}');
    await waitFor(() => expect(screen.queryByTestId('context_menu')).not.toBeInTheDocument());

    // Input D's owned assignment is gone: with the projection already rebuilt
    // (asserted above), its card offers no Species removal any more - either
    // no menu opens for the empty card or the item is absent.
    fireEvent.contextMenu(inputD, { clientX: 200, clientY: 200, bubbles: true });
    const reopened = screen.queryByTestId('context_menu');
    if (reopened) {
      expect(
        within(reopened).queryByRole('button', { name: /Remove annotation: Species: Chlamydomonas/i }),
      ).not.toBeInTheDocument();
    } else {
      expect(reopened).toBeNull();
    }
  },
};

export const RemovesProcessAnnotationFromSingleEdgeContextMenu: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const source = await addRailProperty(canvas, 'Output', 'Removable Edge Process', 'removable edge', 'process');
    const edge = await waitFor(() => canvas.getAllByTestId('provenance-connection')[0]);
    await dragByPointer(source, edge);
    await waitFor(() => expect(processAssignmentLinkCount(canvas.getByTestId('provenance-mutation-preview'))).toBe(1));

    const connectorCountBefore = canvas.getAllByTestId('provenance-connection').length;
    fireEvent.contextMenu(edge, { clientX: 320, clientY: 240, bubbles: true });
    await screen.findByTestId('context_menu');
    await clickMenuAction(/Remove annotation: Removable Edge Process: removable edge/i);

    await waitFor(() => {
      expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('ProcessAssignmentRemoved');
      // The annotation is gone but the structural link/connector remains.
      expect(canvas.getAllByTestId('provenance-connection').length).toBe(connectorCountBefore);
    });
  },
};

export const RemovesPooledProcessAnnotationFromEveryRepresentedLink: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const source = await addRailProperty(canvas, 'Output', 'Pooled Removable Process', 'pooled removable', 'process');
    await groupByProperty(canvasElement, 'Input', 'Species');

    let expectedLinkCount = 0;
    const pooledKey = await waitFor(() => {
      const badges = canvas.getAllByTestId('provenance-connection-count');
      expect(badges.length).toBeGreaterThan(0);
      expectedLinkCount = Number((badges[0].textContent ?? '').match(/\d+/)?.[0] ?? 0);
      expect(expectedLinkCount).toBeGreaterThan(1);
      return badges[0].getAttribute('data-provenance-connection-key');
    });
    const pooledEdge = () => {
      const candidate = canvas
        .getAllByTestId('provenance-connection')
        .find((path) => path.getAttribute('data-provenance-connection-key') === pooledKey);
      expect(candidate).toBeTruthy();
      return candidate!;
    };

    await dragByPointer(source, pooledEdge());
    await waitFor(() =>
      expect(processAssignmentLinkCount(canvas.getByTestId('provenance-mutation-preview'))).toBe(expectedLinkCount),
    );

    const connectorCountBefore = canvas.getAllByTestId('provenance-connection').length;
    fireEvent.contextMenu(pooledEdge(), { clientX: 320, clientY: 240, bubbles: true });
    await screen.findByTestId('context_menu');
    await clickMenuAction(/Remove annotation: Pooled Removable Process: pooled removable/i);

    await waitFor(() => {
      const preview = canvas.getByTestId('provenance-mutation-preview').textContent ?? '';
      // Every represented link was covered by the one new assignment, so
      // removing it deletes that assignment outright rather than shrinking
      // its coverage - the bulk meaning intent §5 requires for a pooled
      // connector - while the structural links (and the connector) remain.
      expect(preview.split('\n').filter((line) => line === 'ProcessAssignmentRemoved')).toHaveLength(1);
      // One atomic deletion, not a link-by-link coverage shrink.
      expect(preview).not.toContain('ProcessAssignmentCoverageChanged');
      expect(canvas.getAllByTestId('provenance-connection').length).toBe(connectorCountBefore);
    });
  },
};

export const EditsAnUnambiguousProcessAnnotationFromASingleEdgeContextMenu: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const source = await addRailProperty(canvas, 'Output', 'Editable Edge Process', 'editable edge', 'process');
    const edge = await waitFor(() => canvas.getAllByTestId('provenance-connection')[0]);
    await dragByPointer(source, edge);
    await waitFor(() => expect(processAssignmentLinkCount(canvas.getByTestId('provenance-mutation-preview'))).toBe(1));

    // The edge's single assignment covers exactly the one link it displays,
    // so `editAvailableReferences` resolves to exactly one originating
    // process-link reference and edits it in place (design §4).
    fireEvent.contextMenu(edge, { clientX: 320, clientY: 240, bubbles: true });
    await screen.findByTestId('context_menu');
    await clickMenuAction(/Edit annotation: Editable Edge Process: editable edge/i);
    await waitFor(() => expect(screen.queryByTestId('context_menu')).not.toBeInTheDocument());

    const valueInput = await waitFor(() => canvas.getByTestId('provenance-annotation-edit-value'));
    await userEvent.clear(valueInput);
    await userEvent.type(valueInput, 'edited value');
    await userEvent.click(canvas.getByTestId('provenance-confirm-annotation-edit'));

    await waitFor(() => {
      const preview = canvas.getByTestId('provenance-mutation-preview').textContent ?? '';
      expect(
        preview.split('\n').filter((line) => line.startsWith('PropertyValueDefinitionUpdated')),
      ).toHaveLength(1);
    });
  },
};

export const EditingAPooledProcessAnnotationEditsEveryPooledLink: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const source = await addRailProperty(canvas, 'Output', 'Pooled Editable Process', 'pooled editable', 'process');
    await groupByProperty(canvasElement, 'Input', 'Species');

    let expectedLinkCount = 0;
    const pooledKey = await waitFor(() => {
      const badges = canvas.getAllByTestId('provenance-connection-count');
      expect(badges.length).toBeGreaterThan(0);
      expectedLinkCount = Number((badges[0].textContent ?? '').match(/\d+/)?.[0] ?? 0);
      expect(expectedLinkCount).toBeGreaterThan(1);
      return badges[0].getAttribute('data-provenance-connection-key');
    });
    const pooledEdge = () => {
      const candidate = canvas
        .getAllByTestId('provenance-connection')
        .find((path) => path.getAttribute('data-provenance-connection-key') === pooledKey);
      expect(candidate).toBeTruthy();
      return candidate!;
    };

    await dragByPointer(source, pooledEdge());
    await waitFor(() =>
      expect(processAssignmentLinkCount(canvas.getByTestId('provenance-mutation-preview'))).toBe(expectedLinkCount),
    );

    // A pooled connector is a bulk-edit surface for the process annotations
    // its pooled links own in this layer (intent §4): every entry resolves
    // uniquely to the one assignment covering the pooled links, so the edit
    // applies over all of them - no split, no refusal, no guessing.
    fireEvent.contextMenu(pooledEdge(), { clientX: 320, clientY: 240, bubbles: true });
    await screen.findByTestId('context_menu');
    await clickMenuAction(/Edit annotation: Pooled Editable Process: pooled editable/i);
    await waitFor(() => expect(screen.queryByTestId('context_menu')).not.toBeInTheDocument());

    const valueInput = await waitFor(() => canvas.getByTestId('provenance-annotation-edit-value'));
    await userEvent.clear(valueInput);
    await userEvent.type(valueInput, 'pooled edited');
    await userEvent.click(canvas.getByTestId('provenance-confirm-annotation-edit'));

    await waitFor(() => {
      const preview = canvas.getByTestId('provenance-mutation-preview').textContent ?? '';
      // Full coverage stays one assignment: the value updates in place, and
      // the covered-link partition is untouched.
      expect(
        preview.split('\n').filter((line) => line.startsWith('PropertyValueDefinitionUpdated')),
      ).toHaveLength(1);
      expect(preview).not.toContain('ProcessAssignmentSplit');
    });
    expect(canvasElement).not.toHaveTextContent(/Multiple links cover this annotation/i);
  },
};

export const EndpointCardsStayIntactAfterConnectorRemoval: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const inputTitlesBefore = groupCardTitles(canvasElement, 'Input');
    const outputTitlesBefore = groupCardTitles(canvasElement, 'Output');
    const connectorCountBefore = canvas.getAllByTestId('provenance-connection').length;

    const connector = await waitFor(() => canvas.getAllByTestId('provenance-connection')[0]);
    fireEvent.contextMenu(connector, { clientX: 320, clientY: 240, bubbles: true });
    const menu = await screen.findByTestId('context_menu');
    await userEvent.click(within(menu).getByRole('button', { name: /delete/i }));

    await waitFor(() => {
      expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('ProcessLinkRemoved');
      expect(canvas.getAllByTestId('provenance-connection').length).toBe(connectorCountBefore - 1);
    });

    // Removing a link disconnects two cards; it removes neither of them.
    expect(groupCardTitles(canvasElement, 'Input')).toEqual(inputTitlesBefore);
    expect(groupCardTitles(canvasElement, 'Output')).toEqual(outputTitlesBefore);
  },
};

export const ProcessOnlyEntryDisappearsAfterItsLastAssignmentIsRemoved: Story = {
  render: () => <Harness fixture="allLinkShapes" />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const entry = await waitFor(() =>
      canvas.getByTestId('provenance-process-only-process-endpointless-link-endpointless'),
    );
    // Two loaded assignments back this grouping value, and the entry still
    // shows exactly one badge for it - getByText throws on duplicates.
    expect(within(entry).getByText('Endpointless marker: loaded')).toBeInTheDocument();

    // The container-bound (Recipe Component) value is read-only there: it
    // shows its badge but contributes no removal action, not an inert one.
    // The entry's writable Recipe reference keeps its own removal action, so
    // the filtering is per grouping value, not per entry.
    const boundEntry = canvas.getByTestId(
      'provenance-process-only-process-endpointless-bound-link-endpointless-bound',
    );
    expect(within(boundEntry).getByText('Bound marker: bound')).toBeInTheDocument();
    expect(
      within(boundEntry).queryByRole('button', { name: /Remove annotation: Bound marker: bound/i }),
    ).not.toBeInTheDocument();
    expect(
      within(boundEntry).getByRole('button', { name: /Remove annotation: Recipe marker/i }),
    ).toBeInTheDocument();

    await userEvent.click(
      within(entry).getByRole('button', { name: /Remove annotation: Endpointless marker: loaded/i }),
    );

    await waitFor(() => {
      const preview = canvas.getByTestId('provenance-mutation-preview').textContent ?? '';
      // One click removes every assignment behind the one displayed badge.
      const removed = preview.split('\n').filter((line) => line.startsWith('ProcessAssignmentRemoved'));
      expect(removed).toHaveLength(2);
      // Only the UI entry vanishes: the journal records no structural change,
      // so the loaded endpointless process survives in the canonical model.
      expect(preview).not.toContain('ProcessLinkRemoved');
      expect(preview).not.toContain('StructuralProcess');
      expect(
        canvas.queryByTestId('provenance-process-only-process-endpointless-link-endpointless'),
      ).not.toBeInTheDocument();
    });

    // The read-only bound entry is untouched by the removal. Re-query it: the
    // session change re-rendered the entries container.
    const boundAfter = canvas.getByTestId(
      'provenance-process-only-process-endpointless-bound-link-endpointless-bound',
    );
    expect(within(boundAfter).getByText('Bound marker: bound')).toBeInTheDocument();

    // The entry's projection is gated on having at least one annotation
    // (Projection.fs's projectProcessOnlyEntries drops an empty one), so a
    // fresh endpointless link never renders a card to drop onto - the only
    // observable "appearance" is undo restoring the removed assignments.
    fireEvent.click(canvas.getByTestId('provenance-undo'));

    await waitFor(() => {
      const restored = canvas.getByTestId('provenance-process-only-process-endpointless-link-endpointless');
      expect(within(restored).getByText('Endpointless marker: loaded')).toBeInTheDocument();
    });

    expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('No mutations recorded.');
  },
};

export const ProcessOnlyEntryAdvertisesOnlyProcessValueDrags: Story = {
  render: () => <Harness fixture="allLinkShapes" />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const entry = await waitFor(() =>
      canvas.getByTestId('provenance-process-only-process-endpointless-link-endpointless'),
    );

    // An endpointless link accepts process values only (intent §3), so a
    // node-value drag must not light the entry up as a drop target.
    const nodeValue = await addRailProperty(canvas, 'Input', 'Ring Node Value', 'ring', 'node');
    const nodeDrag = await startDragByPointer(nodeValue);
    await waitFor(() => expect(canvas.getByTestId('provenance-drag-overlay-value')).toBeInTheDocument());
    expect(entry.className).not.toContain('swt:ring-1');

    fireEvent.pointerUp(document, {
      clientX: nodeDrag.x,
      clientY: nodeDrag.y,
      button: 0,
      buttons: 0,
      isPrimary: true,
      pointerId: nodeDrag.pointerId,
    });
    await waitFor(() => expect(canvas.queryByTestId('provenance-drag-overlay-value')).not.toBeInTheDocument());

    // A process value in flight is exactly what the entry can accept.
    const processValue = await addRailProperty(canvas, 'Output', 'Ring Process Value', 'ring', 'process');
    const processDrag = await startDragByPointer(processValue);
    await waitFor(() => expect(entry.className).toContain('swt:ring-1'));

    fireEvent.pointerUp(document, {
      clientX: processDrag.x,
      clientY: processDrag.y,
      button: 0,
      buttons: 0,
      isPrimary: true,
      pointerId: processDrag.pointerId,
    });
  },
};

export const ProcessValueDropOnProcessOnlyEntryAssignsItsLink: Story = {
  render: () => <Harness fixture="allLinkShapes" />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const source = await addRailProperty(canvas, 'Output', 'Process Only Value', 'assigned', 'process');
    const entry = await waitFor(() =>
      canvas.getByTestId('provenance-process-only-process-endpointless-link-endpointless'),
    );

    await dragByPointer(source, entry);

    // One assignment covering exactly the entry's backing link, by identity.
    await waitFor(() => {
      const preview = canvas.getByTestId('provenance-mutation-preview').textContent ?? '';
      const added = preview.split('\n').filter((line) => line.startsWith('ProcessAssignmentAdded:'));
      expect(added).toHaveLength(1);
      expect(added[0]).toMatch(/links=link-endpointless$/);
    });

    // Intent §3: nowhere in the UI offers creating a new endpointless process
    // or entry - the only way one exists is by being loaded.
    expect(screen.queryByRole('button', { name: /create.*(process|endpointless)/i })).not.toBeInTheDocument();
  },
};

export const NodeValueDropOnProcessOnlyEntryIsRejected: Story = {
  render: () => <Harness fixture="allLinkShapes" />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const source = await addRailProperty(canvas, 'Input', 'Process Only Node Value', 'rejected', 'node');
    const entry = await waitFor(() =>
      canvas.getByTestId('provenance-process-only-process-endpointless-link-endpointless'),
    );

    await dragByPointer(source, entry);

    await waitFor(() =>
      expect(canvasElement).toHaveTextContent(/Only process annotations can be assigned to an endpointless process\./i),
    );
    expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('No mutations recorded.');
  },
};

export const OneProcessValueDropAcrossTwoProcessesEditsAsOneEntry: Story = {
  render: () => <Harness fixture="fanOut" />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // The entity's two outgoing links belong to two structural processes -
    // the shape ProcessCore produces, where every Process is one directed
    // edge - so one drop creates one assignment per process (intent §3).
    const source = await addRailProperty(canvas, 'Input', 'Fan Amount', '5', 'process');
    const entity = canvas.getByText('Fan Input').closest('article')!;
    await dragByPointer(source, entity);

    await waitFor(() => {
      const preview = canvas.getByTestId('provenance-mutation-preview').textContent ?? '';
      const added = preview.split('\n').filter((line) => line.startsWith('ProcessAssignmentAdded'));
      expect(added).toHaveLength(2);
    });

    // The card deduplicates the two assignments into one displayed entry, so
    // its menu offers exactly one edit action for the value - getByRole
    // throws on duplicates. Editing it covers every assignment behind it, as
    // one revision-advancing command (intent §4).
    fireEvent.contextMenu(entity, { clientX: 200, clientY: 200, bubbles: true });
    await screen.findByTestId('context_menu');
    await clickMenuAction(/Edit annotation: Fan Amount: 5/i);

    const valueInput = await waitFor(() => canvas.getByTestId('provenance-annotation-edit-value'));
    await userEvent.clear(valueInput);
    await userEvent.type(valueInput, '7');
    await userEvent.click(canvas.getByTestId('provenance-confirm-annotation-edit'));

    await waitFor(() => {
      const preview = canvas.getByTestId('provenance-mutation-preview').textContent ?? '';
      const changed = preview.split('\n').filter((line) => line.startsWith('ProcessAssignmentValueChanged'));
      expect(changed).toHaveLength(2);
    });

    // Still one displayed entry, now carrying the edited value.
    fireEvent.contextMenu(entity, { clientX: 200, clientY: 200, bubbles: true });
    const menuAfter = await screen.findByTestId('context_menu');
    expect(within(menuAfter).getByRole('button', { name: /Edit annotation: Fan Amount: 7/i })).toBeInTheDocument();
    expect(within(menuAfter).queryByText(/Fan Amount: 5/)).not.toBeInTheDocument();
    await userEvent.keyboard('{Escape}');
    await waitFor(() => expect(screen.queryByTestId('context_menu')).not.toBeInTheDocument());
  },
};

export const GroupCardNodeValueBulkEditCoversEveryOwningAssignment: Story = {
  render: () => <Harness fixture="fanOut" />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // Two owning assignments back the one displayed Species value, and the
    // card menu offers exactly one edit action for it - getByRole throws on
    // duplicates. The entity surface bulk-edits both as one atomic command
    // (intent §4).
    const entity = canvas.getByText('Fan Input').closest('article')!;
    fireEvent.contextMenu(entity, { clientX: 200, clientY: 200, bubbles: true });
    await screen.findByTestId('context_menu');
    await clickMenuAction(/Edit annotation: Species: Arabidopsis/i);

    const valueInput = await waitFor(() => canvas.getByTestId('provenance-annotation-edit-value'));
    await userEvent.clear(valueInput);
    await userEvent.type(valueInput, 'Nicotiana');
    await userEvent.click(canvas.getByTestId('provenance-confirm-annotation-edit'));

    await waitFor(() => {
      const preview = canvas.getByTestId('provenance-mutation-preview').textContent ?? '';
      expect(preview.split('\n').filter((line) => line.startsWith('NodeAssignmentValueChanged'))).toHaveLength(2);
      expect(preview).not.toContain('NodeAssignmentAdded');
    });

    // Both owning assignments now stand behind the one edited entry.
    fireEvent.contextMenu(entity, { clientX: 200, clientY: 200, bubbles: true });
    const menuAfter = await screen.findByTestId('context_menu');
    expect(
      within(menuAfter).getByRole('button', { name: /Remove annotation: Species: Nicotiana/i }),
    ).toBeInTheDocument();
    expect(within(menuAfter).queryByText(/Species: Arabidopsis/)).not.toBeInTheDocument();
    await userEvent.keyboard('{Escape}');
    await waitFor(() => expect(screen.queryByTestId('context_menu')).not.toBeInTheDocument());
  },
};

export const MixedParameterAndComponentEntryRefusesBulkEditWhole: Story = {
  render: () => <Harness fixture="fanOut" />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // The displayed Device setting value merges an editable Parameter with a
    // read-only container-bound (Recipe Component) backing. An edit claims to
    // cover the whole displayed value, and one entry it cannot cover blocks
    // the operation whole (intent §4) - never a silent partial edit that
    // would change only the Parameter and split the display.
    const entity = canvas.getByText('Fan Input').closest('article')!;
    fireEvent.contextMenu(entity, { clientX: 200, clientY: 200, bubbles: true });
    await screen.findByTestId('context_menu');
    await clickMenuAction(/Edit annotation: Device setting: 37/i);

    const valueInput = await waitFor(() => canvas.getByTestId('provenance-annotation-edit-value'));
    await userEvent.clear(valueInput);
    await userEvent.type(valueInput, '42');
    await userEvent.click(canvas.getByTestId('provenance-confirm-annotation-edit'));

    await waitFor(() => {
      expect(canvasElement).toHaveTextContent(/managed externally and cannot be modified here/i);
      expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('No mutations recorded.');
    });

    // The displayed value did not split: the menu still offers exactly one
    // entry for it, unchanged.
    fireEvent.contextMenu(entity, { clientX: 200, clientY: 200, bubbles: true });
    const menuAfter = await screen.findByTestId('context_menu');
    expect(
      within(menuAfter).getByRole('button', { name: /Edit annotation: Device setting: 37/i }),
    ).toBeInTheDocument();
    expect(within(menuAfter).queryByText(/Device setting: 42/)).not.toBeInTheDocument();
    await userEvent.keyboard('{Escape}');
    await waitFor(() => expect(screen.queryByTestId('context_menu')).not.toBeInTheDocument());
  },
};

export const EditsAPropertyValueGloballyFromTheSidebar: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByTestId('provenance-global-values-trigger'));
    // The popover content portals outside canvasElement, so it (and
    // everything inside it) is queried through the global `screen`, not
    // `canvas` - unlike the inline trigger button itself.
    const panel = await waitFor(() => screen.getByTestId('provenance-global-values-panel'));

    // Arabidopsis is shared by Input A, B and C. Editing it through the
    // sidebar is explicitly global (design §4/§7.2): it updates the shared
    // value definition in place rather than detaching just one assignment,
    // so every referencing owner sees the new value.
    await userEvent.click(within(panel).getByTestId('provenance-global-edit-value-value-species-arabidopsis'));
    const valueInput = await waitFor(() => screen.getByTestId('provenance-global-edit-value-input'));
    await userEvent.clear(valueInput);
    await userEvent.type(valueInput, 'Solanum');
    await userEvent.click(screen.getByTestId('provenance-confirm-global-value-edit'));

    await waitFor(() => {
      const preview = canvas.getByTestId('provenance-mutation-preview').textContent ?? '';
      expect(
        preview.split('\n').filter((line) => line.startsWith('PropertyValueDefinitionUpdated')),
      ).toHaveLength(1);
    });

    for (const label of ['Input A', 'Input B', 'Input C']) {
      const article = canvas.getByText(label).closest('article')!;
      fireEvent.contextMenu(article, { clientX: 200, clientY: 200, bubbles: true });
      const menu = await screen.findByTestId('context_menu');
      expect(
        within(menu).getByRole('button', { name: /Remove annotation: Species: Solanum/i }),
      ).toBeInTheDocument();
      await userEvent.keyboard('{Escape}');
      await waitFor(() => expect(screen.queryByTestId('context_menu')).not.toBeInTheDocument());
    }
  },
};

export const RemovesAPropertyValueGloballyFromTheSidebarWithConfirmation: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByTestId('provenance-global-values-trigger'));
    const panel = await waitFor(() => screen.getByTestId('provenance-global-values-panel'));

    // Chlamydomonas is owned only by Input D. Removing it through the
    // sidebar is a destructive global operation (design §5) gated on an
    // explicit confirmation step.
    await userEvent.click(
      within(panel).getByTestId('provenance-global-remove-value-value-species-chlamydomonas'),
    );
    await waitFor(() => expect(screen.getByTestId('provenance-global-removal-prompt')).toBeInTheDocument());
    await userEvent.click(screen.getByTestId('provenance-confirm-global-removal'));

    await waitFor(() => {
      const preview = canvas.getByTestId('provenance-mutation-preview').textContent ?? '';
      expect(preview.split('\n').filter((line) => line === 'NodeAssignmentRemoved')).toHaveLength(1);
      expect(
        preview.split('\n').filter((line) => line.startsWith('PropertyValueDefinitionDeleted')),
      ).toHaveLength(1);
    });

    // Input D's only annotation is gone, so its card offers no context menu
    // at all (`GroupCard.fs` only renders one while `group.Annotations` is
    // non-empty) - a stronger signal than a disabled/absent menu item.
    const inputD = canvas.getByText('Input D').closest('article')!;
    expect(within(inputD).queryByText(/Chlamydomonas/i)).not.toBeInTheDocument();
    fireEvent.contextMenu(inputD, { clientX: 200, clientY: 200, bubbles: true });
    await waitFor(() => expect(screen.queryByTestId('context_menu')).not.toBeInTheDocument());
  },
};

export const RemovesAPropertyGloballyFromTheSidebarWithConfirmation: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByTestId('provenance-global-values-trigger'));
    const panel = await waitFor(() => screen.getByTestId('provenance-global-values-panel'));

    // Replicate covers link-b and link-c through two separate process
    // assignments. Removing the whole property is a destructive global
    // operation (design §5) that removes both, gated on confirmation.
    await userEvent.click(within(panel).getByTestId('provenance-global-remove-property-property-replicate'));
    await waitFor(() => expect(screen.getByTestId('provenance-global-removal-prompt')).toBeInTheDocument());
    await userEvent.click(screen.getByTestId('provenance-confirm-global-removal'));

    await waitFor(() => {
      const preview = canvas.getByTestId('provenance-mutation-preview').textContent ?? '';
      expect(preview.split('\n').filter((line) => line === 'ProcessAssignmentRemoved')).toHaveLength(2);
      expect(preview.split('\n').filter((line) => line.startsWith('PropertyDefinitionDeleted'))).toHaveLength(1);
      expect(within(panel).queryByText('Replicate')).not.toBeInTheDocument();
    });
  },
};

export const SidebarRejectsEditingAReadOnlyRecipeComponent: Story = {
  render: () => <Harness fixture="referenceCatalog" />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByTestId('provenance-global-values-trigger'));
    const panel = await waitFor(() => screen.getByTestId('provenance-global-values-panel'));

    // GlobalValuesPanel.fs deliberately offers Edit/Delete on every value
    // unconditionally, including a Recipe Component - the command layer is
    // the enforcement point, refusing the mutation with
    // ReadOnlyAdapterResourceMutation rather than this panel special-casing
    // container-bound values the way the rail chip and context menus do.
    await userEvent.click(within(panel).getByTestId('provenance-global-edit-value-value-component-one'));
    const valueInput = await waitFor(() => screen.getByTestId('provenance-global-edit-value-input'));
    await userEvent.clear(valueInput);
    await userEvent.type(valueInput, 'Solvent');
    await userEvent.click(screen.getByTestId('provenance-confirm-global-value-edit'));

    await waitFor(() => {
      expect(
        canvas.getByText('This resource is managed externally and cannot be modified here.'),
      ).toBeInTheDocument();
    });

    expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('No mutations recorded.');
    expect(within(panel).getByText('Buffer')).toBeInTheDocument();
  },
};

// -- Rail removal and one-entry-per-grouping-value display ------------------

/** The "x" inside a rail chip. Chips are addressed by their value text, so a
 * merged chip (several backing assignments, one displayed value) is reached the
 * same way as a single-assignment one. */
function railValueRemoveButton(chip: HTMLElement, propertyName: string) {
  return within(chip).getByRole('button', { name: new RegExp(`^Remove ${escapeRegExp(propertyName)} value$`, 'i') });
}

/**
 * Clicks a rail chip's remove button and returns the destructive confirm. Both
 * this and its property sibling retry the click, because the affordances live in
 * hover-revealed row controls whose first activation can be swallowed while the
 * rail is still settling - the same reason `expandProperty` and
 * `groupByProperty` retry above.
 */
async function openRailValueRemoval(
  canvas: ReturnType<typeof within>,
  side: Side,
  propertyName: string,
  valueText: string,
) {
  for (let attempt = 0; attempt < 3 && !canvas.queryByTestId('provenance-rail-removal-prompt'); attempt += 1) {
    const chip = await railValue(canvas, side, propertyName, valueText);
    await userEvent.click(railValueRemoveButton(chip, propertyName));
    await waitFor(() => expect(canvas.getByTestId('provenance-rail-removal-prompt')).toBeInTheDocument(), {
      timeout: 1000,
    }).catch(() => undefined);
  }

  return waitFor(() => canvas.getByTestId('provenance-rail-removal-prompt'), { timeout: 3000 });
}

async function openRailPropertyRemoval(canvas: ReturnType<typeof within>, side: Side, propertyName: string) {
  await ensurePropertyInRail(canvas, side, propertyName);

  for (let attempt = 0; attempt < 3 && !canvas.queryByTestId('provenance-rail-removal-prompt'); attempt += 1) {
    // The row controls only enter the layout while the row is hovered.
    await userEvent.hover(canvas.getByTestId(`provenance-property-${side}-${propertyName}`));
    const remove = await waitFor(() => canvas.getByTestId(`provenance-property-remove-${side}-${propertyName}`));
    await userEvent.click(remove);
    await waitFor(() => expect(canvas.getByTestId('provenance-rail-removal-prompt')).toBeInTheDocument(), {
      timeout: 1000,
    }).catch(() => undefined);
  }

  return waitFor(() => canvas.getByTestId('provenance-rail-removal-prompt'), { timeout: 3000 });
}

export const RemovesADraftRailValueWithoutConfirmation: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // An unassigned draft is UI state only (intent §3: "An unassigned sidebar
    // value is a UI draft"), so its removal needs no destructive confirm and
    // records no mutation - there is no session value definition to delete.
    const draft = await addRailValue(canvas, 'Input', 'Species', 'Nicotiana');
    await userEvent.click(railValueRemoveButton(draft, 'Species'));

    expect(canvas.queryByTestId('provenance-rail-removal-prompt')).not.toBeInTheDocument();

    const panel = await expandProperty(canvas, 'Input', 'Species');
    await waitFor(() => expect(panel.queryByText('Nicotiana')).not.toBeInTheDocument());
    expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('No mutations recorded.');
  },
};

export const RemovesAnAssignedRailValueAfterConfirmation: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // Chlamydomonas is owned only by Input D. Removing an assigned value from
    // the rail is the explicit global operation of intent §5, so it is gated on
    // a destructive confirm naming how many assignments it reaches.
    const prompt = await openRailValueRemoval(canvas, 'Input', 'Species', 'Chlamydomonas');
    expect(prompt).toHaveTextContent('Deletes the value definition itself');
    expect(prompt).toHaveTextContent('removes it from 1 assignment(s) across the session.');

    // Cancelling leaves the session untouched: the confirm is a real gate, not
    // a notification shown after the fact.
    await userEvent.click(canvas.getByTestId('provenance-rail-cancel-removal'));
    await waitFor(() => expect(canvas.queryByTestId('provenance-rail-removal-prompt')).not.toBeInTheDocument());
    expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('No mutations recorded.');

    await openRailValueRemoval(canvas, 'Input', 'Species', 'Chlamydomonas');
    await userEvent.click(canvas.getByTestId('provenance-rail-confirm-removal'));

    await waitFor(() => {
      const preview = canvas.getByTestId('provenance-mutation-preview').textContent ?? '';
      expect(preview.split('\n').filter((line) => line === 'NodeAssignmentRemoved')).toHaveLength(1);
      expect(preview.split('\n').filter((line) => line.startsWith('PropertyValueDefinitionDeleted'))).toHaveLength(1);
    });

    // Gone from the rail and from its owning node's card alike: the removal is
    // against the canonical owner, not a display entry.
    const panel = await expandProperty(canvas, 'Input', 'Species');
    await waitFor(() => expect(panel.queryByText('Chlamydomonas')).not.toBeInTheDocument());
    const inputD = canvas.getByText('Input D').closest('article')!;
    expect(within(inputD).queryByText(/Chlamydomonas/i)).not.toBeInTheDocument();
  },
};

export const OneRailChipRepresentsEveryAssignmentOfAGroupingValue: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // Input A, B and C each own their own Species assignment referencing the
    // one shared Arabidopsis definition. Display identity follows the grouping
    // value key (intent §6/§7), so the three assignments are one chip, with the
    // assignment/owner identity retained in its backing rather than multiplied
    // into three visually identical entries.
    const panel = await expandProperty(canvas, 'Input', 'Species');
    expect(panel.getAllByText('Arabidopsis')).toHaveLength(1);

    // ...and the one chip's removal reaches every assignment it represents.
    const prompt = await openRailValueRemoval(canvas, 'Input', 'Species', 'Arabidopsis');
    expect(prompt).toHaveTextContent('every entry that displays it disappears');
    expect(prompt).toHaveTextContent('removes it from 3 assignment(s) across the session.');
    await userEvent.click(canvas.getByTestId('provenance-rail-confirm-removal'));

    await waitFor(() => {
      const preview = canvas.getByTestId('provenance-mutation-preview').textContent ?? '';
      expect(preview.split('\n').filter((line) => line === 'NodeAssignmentRemoved')).toHaveLength(3);
      expect(preview.split('\n').filter((line) => line.startsWith('PropertyValueDefinitionDeleted'))).toHaveLength(1);
    });

    for (const label of ['Input A', 'Input B', 'Input C']) {
      const article = canvas.getByText(label).closest('article')!;
      expect(within(article).queryByText(/Arabidopsis/i)).not.toBeInTheDocument();
    }
  },
};

export const OneShelfRowRepresentsEachPropertyPerFolder: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // Species has four owning assignments across Input A-D, all in the one
    // assay-table source folder. The shelf drag payload carries only the
    // property and side, so those assignments are writeback bookkeeping that
    // must not multiply the row.
    const folder = canvas.getByTestId('foldered-draggable-folder-source-fixture-assay-table');
    const row = await openShelfFolder(canvas, folder);
    expect(row.getAllByRole('button', { name: /^Drag Species$/ })).toHaveLength(1);
    expect(row.getAllByRole('button', { name: /^Drag Temperature$/ })).toHaveLength(1);
  },
};

export const MemberValuePopoverShowsOneEntryPerGroupingValue: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // Output A already sees Species: Arabidopsis forward-propagated from Input
    // A through link-a. Dropping the same value onto it adds an *owned*
    // assignment there too (intent §3), so the member now holds two annotations
    // sharing one grouping value key - one owned, one propagated. The
    // availability relation is evidence, never display identity, so the member
    // popover shows the value once (intent §6/§7).
    const source = await railValue(canvas, 'Output', 'Species', 'Arabidopsis');
    await dragByPointer(source, canvas.getByText('Output A').closest('article')!);
    await waitFor(() =>
      expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('NodeAssignmentAdded'),
    );

    await groupByProperty(canvasElement, 'Output', 'Species');
    const grouped = await waitFor(() => getGroupCard(canvasElement, 'Output', 'Species: Arabidopsis'));
    await userEvent.click(within(grouped).getByRole('button', { name: 'Show members' }));
    const member = within(grouped).getByTestId('provenance-group-member-Output-node-output-a');
    await userEvent.hover(member);

    const details = await waitFor(() =>
      within(grouped).getByTestId('provenance-member-values-Output-node-output-a'),
    );
    expect(within(details).getAllByText('Species: Arabidopsis')).toHaveLength(1);

    await userEvent.unhover(member);
  },
};

export const RemovesARailPropertyAfterConfirmation: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // Property definitions are category-keyed with no owner kind, so deleting
    // Species from the rail removes the category for node and process
    // annotations alike - the confirm text says so, and names the four
    // assignments (Arabidopsis x3, Chlamydomonas x1) it reaches.
    const prompt = await openRailPropertyRemoval(canvas, 'Input', 'Species');
    expect(prompt).toHaveTextContent('Deletes this category for node and process annotations alike');
    expect(prompt).toHaveTextContent('every entry that displays one of its values disappears');
    expect(prompt).toHaveTextContent('4 assignment(s) across the session.');
    await userEvent.click(canvas.getByTestId('provenance-rail-confirm-removal'));

    await waitFor(() => {
      const preview = canvas.getByTestId('provenance-mutation-preview').textContent ?? '';
      expect(preview.split('\n').filter((line) => line === 'NodeAssignmentRemoved')).toHaveLength(4);
      expect(preview.split('\n').filter((line) => line.startsWith('PropertyDefinitionDeleted'))).toHaveLength(1);
    });

    await waitFor(() => expect(canvas.queryByTestId('provenance-property-Input-Species')).not.toBeInTheDocument());
  },
};

export const ReadOnlyRailValueOffersNoRemoval: Story = {
  render: () => <Harness fixture="referenceCatalog" />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // A container-bound Component projection is a read-only dependent (intent
    // §5: "Read-only dependent values are excluded from global value and
    // property removal"), and a Reference-valued Recipe assignment is an
    // adapter resource the grouping layer never deletes. The existing
    // `canMutate` guard already excludes both, so neither the chip's remove
    // button nor the row's property delete button is rendered.
    const componentPanel = await expandProperty(canvas, 'Output', 'Component');
    const component = componentPanel.getAllByRole('button', { name: /^Read-only Component value$/i })[0];
    expect(
      within(component).queryByRole('button', { name: /^Remove Component value$/i }),
    ).not.toBeInTheDocument();
    expect(canvas.queryByTestId('provenance-property-remove-Output-Component')).not.toBeInTheDocument();

    const recipePanel = await expandProperty(canvas, 'Output', 'Recipe');
    for (const recipe of recipePanel.getAllByRole('button', { name: /Recipe value$/i })) {
      expect(within(recipe).queryByRole('button', { name: /^Remove Recipe value$/i })).not.toBeInTheDocument();
    }
    expect(canvas.queryByTestId('provenance-property-remove-Output-Recipe')).not.toBeInTheDocument();
  },
};

// -- Telling node and edge annotations apart, and edge drop feedback --------

export const AnnotationsSayWhetherTheyAreNodeOrEdgeValues: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // Species is owned by the input entities; Analysis belongs to the processes
    // and reaches those entities through the edges incident to them. A header
    // carries exactly one assignment kind, so the rail states it once on the
    // property header rather than repeating it on each of its values.
    await ensurePropertyInRail(canvas, 'Input', 'Species');
    const speciesHeader = canvas.getByTestId('provenance-property-Input-Species');
    expect(speciesHeader).toHaveAttribute('data-provenance-property-kind', 'node');
    expect(within(speciesHeader).getByText('Node annotation')).toBeInTheDocument();
    expect(speciesHeader.getAttribute('title')).toContain('Node annotation: owned by this entity.');

    await ensurePropertyInRail(canvas, 'Output', 'Analysis');
    const analysisHeader = canvas.getByTestId('provenance-property-Output-Analysis');
    expect(analysisHeader).toHaveAttribute('data-provenance-property-kind', 'process');
    expect(within(analysisHeader).getByText('Edge annotation')).toBeInTheDocument();
    expect(analysisHeader.getAttribute('title')).toContain(
      'Edge annotation: carried by the connections at this entity.',
    );

    // The values under a header say nothing about kind - the header already did.
    const species = await railValue(canvas, 'Input', 'Species', 'Arabidopsis');
    expect(species).not.toHaveAttribute('data-provenance-annotation-kind');

    // The shelf is where a property is picked up, so it carries the same marker
    // the rail shows once the property is placed.
    const folder = canvas.getByTestId('foldered-draggable-folder-source-fixture-assay-table');
    const row = await openShelfFolder(canvas, folder);
    const replicate = row.getAllByRole('button', { name: /^Drag Replicate$/ })[0];
    expect(within(replicate).getByText('Edge annotation')).toBeInTheDocument();

    const previousTreatment = row.getAllByRole('button', { name: /^Drag Previous Treatment$/ })[0];
    expect(within(previousTreatment).getByText('Node annotation')).toBeInTheDocument();

    // The same distinction rides the group-card tab that a value formed...
    await groupByProperty(canvasElement, 'Input', 'Species');
    const speciesCard = await waitFor(() => getGroupCard(canvasElement, 'Input', 'Species: Arabidopsis'));
    const speciesTab = groupCardTab(speciesCard, 'Species: Arabidopsis');
    expect(speciesTab).toHaveAttribute('data-provenance-annotation-kind', 'node');
    expect(speciesTab.getAttribute('title')).toContain('Node annotation: owned by this entity.');

    await groupByProperty(canvasElement, 'Output', 'Analysis');
    const analysisCard = await waitFor(() =>
      getGroupCard(canvasElement, 'Output', 'Analysis: Mass Spectrometry'),
    );
    const analysisTab = groupCardTab(analysisCard, 'Analysis: Mass Spectrometry');
    expect(analysisTab).toHaveAttribute('data-provenance-annotation-kind', 'process');
    expect(analysisTab.getAttribute('title')).toContain(
      'Edge annotation: carried by the connections at this entity.',
    );

    // ...and the member hover list, where both kinds sit side by side.
    await userEvent.click(within(analysisCard).getByRole('button', { name: 'Show members' }));
    const member = within(analysisCard).getByTestId('provenance-group-member-Output-node-output-a');
    await userEvent.hover(member);
    const details = await waitFor(() =>
      within(analysisCard).getByTestId('provenance-member-values-Output-node-output-a'),
    );

    const kindOf = (text: RegExp) =>
      within(details)
        .getByText(text)
        .closest('[data-provenance-annotation-kind]')!
        .getAttribute('data-provenance-annotation-kind');

    expect(kindOf(/^Species: Arabidopsis$/)).toBe('node');
    expect(kindOf(/^Analysis: Mass Spectrometry$/)).toBe('process');

    await userEvent.unhover(member);
  },
};

export const DraggingAProcessValueMarksEveryEdgeAsADropTarget: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const dropCandidates = () => canvasElement.querySelectorAll('[data-provenance-drop-candidate="true"]');

    expect(dropCandidates()).toHaveLength(0);

    // A process value can land on an edge, so while one is in flight every
    // existing edge announces itself as a target - the move the group cards
    // already make with their faint ring.
    const processValue = await railValue(canvas, 'Output', 'Analysis', 'Mass Spectrometry');
    const processDrag = await startDragByPointer(processValue);
    await waitFor(() => expect(dropCandidates().length).toBeGreaterThan(0));

    fireEvent.pointerUp(document, {
      clientX: processDrag.x,
      clientY: processDrag.y,
      button: 0,
      buttons: 0,
      isPrimary: true,
      pointerId: processDrag.pointerId,
    });
    await waitFor(() => expect(dropCandidates()).toHaveLength(0));

    // A node value never lands on an edge, so the edges stay inert rather than
    // promising a drop that would be refused.
    const nodeValue = await railValue(canvas, 'Input', 'Species', 'Arabidopsis');
    const nodeDrag = await startDragByPointer(nodeValue);
    await waitFor(() => expect(canvas.getByTestId('provenance-drag-overlay-value')).toBeInTheDocument());
    expect(dropCandidates()).toHaveLength(0);

    fireEvent.pointerUp(document, {
      clientX: nodeDrag.x,
      clientY: nodeDrag.y,
      button: 0,
      buttons: 0,
      isPrimary: true,
      pointerId: nodeDrag.pointerId,
    });
  },
};

export const AnnotationEditFormMatchesTheAddAnnotationSurface: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // Creating and editing an annotation are the same kind of act, so the edit
    // form uses the side rail's "Add annotation" popover surface rather than
    // arriving as a differently coloured alert.
    const addTrigger = within(canvas.getByTestId('provenance-property-rail-Input'))
      .getAllByText('Add annotation')[0]
      .closest('button')!;
    fireEvent.click(addTrigger);
    const addSurface = await waitFor(() => screen.getByTestId('popover_content_provenance-add-value-Annotation'));
    const addClasses = addSurface.className;
    await userEvent.keyboard('{Escape}');

    const inputA = canvas.getByText('Input A').closest('article')!;
    fireEvent.contextMenu(inputA, { clientX: 200, clientY: 200, bubbles: true });
    await screen.findByTestId('context_menu');
    await clickMenuAction(/Edit annotation: Species: Arabidopsis/i);

    const editSurface = await waitFor(() => canvas.getByTestId('provenance-annotation-edit-prompt'));

    // The alert palette is gone, and the panel carries the popover's own
    // surface classes instead.
    expect(editSurface.className).not.toMatch(/swt:alert/);
    for (const surfaceClass of ['swt:rounded-md', 'swt:border-base-content', 'swt:bg-base-100', 'swt:shadow-md']) {
      expect(addClasses).toContain(surfaceClass);
      expect(editSurface.className).toContain(surfaceClass);
    }
  },
};

export const EdgeAnnotationEditsResolveThroughTheEntityThatCarriesThem: Story = {
  render: () => <Harness fixture="chained" />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    fireEvent.click(canvas.getByTestId('provenance-layer-layer-2'));
    await waitFor(() => expect(canvas.getByTestId('provenance-layer-layer-2')).toHaveClass('swt:btn-primary'));

    // Extract Batch carries Temperature from the growth process upstream. Its
    // originating link is not one of this card's own links, and narrowing the
    // edit to the card's links used to leave nothing to act on - reported, in a
    // further error of its own, as "multiple links cover this annotation".
    // Editing resolves it to its originating link instead.
    const extract = await waitFor(() => canvas.getByText('Extract Batch').closest('article')!);
    fireEvent.contextMenu(extract, { clientX: 200, clientY: 200, bubbles: true });
    await screen.findByTestId('context_menu');
    await clickMenuAction(/Edit annotation: Temperature: 21 °C/i);
    await waitFor(() => expect(screen.queryByTestId('context_menu')).not.toBeInTheDocument());

    const valueInput = await waitFor(() => canvas.getByTestId('provenance-annotation-edit-value'));
    await userEvent.clear(valueInput);
    await userEvent.type(valueInput, '31 °C');
    await userEvent.click(canvas.getByTestId('provenance-confirm-annotation-edit'));

    await waitFor(() => {
      const preview = canvas.getByTestId('provenance-mutation-preview').textContent ?? '';
      expect(preview).not.toContain('No mutations recorded.');
      expect(preview).toMatch(/ProcessAssignment(ValueChanged|Split)|PropertyValueDefinitionUpdated/);
    });

    expect(canvasElement).not.toHaveTextContent(/Multiple links cover this annotation/i);
  },
};

export const UnavailableAnnotationActionsAreDisabledPerEntry: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // Output A carries two kinds of annotation at once: Species, forward-
    // propagated from Input A and therefore not removable here, and Analysis,
    // an annotation of the process on its own incident link, which is. Each
    // value is exactly one entry carrying both actions; the action that does
    // not apply is greyed out with a hint instead of vanishing or splitting
    // the value into per-action rows.
    const outputA = canvas.getByText('Output A').closest('article')!;
    fireEvent.contextMenu(outputA, { clientX: 200, clientY: 200, bubbles: true });
    const cardMenu = await screen.findByTestId('context_menu');

    // One entry per value: the label appears once, never once per action.
    expect(within(cardMenu).getAllByText('Species: Arabidopsis')).toHaveLength(1);
    expect(within(cardMenu).getAllByText('Analysis: Mass Spectrometry')).toHaveLength(1);

    const speciesRemove = within(cardMenu).getByRole('button', {
      name: /Remove annotation: Species: Arabidopsis/i,
    });
    expect(speciesRemove).toBeDisabled();
    expect(speciesRemove.closest('span')).toHaveAttribute(
      'title',
      expect.stringMatching(/another layer/i),
    );
    expect(
      within(cardMenu).getByRole('button', { name: /Edit annotation: Species: Arabidopsis/i }),
    ).toBeEnabled();
    expect(
      within(cardMenu).getByRole('button', { name: /Remove annotation: Analysis: Mass Spectrometry/i }),
    ).toBeEnabled();
    expect(
      within(cardMenu).getByRole('button', { name: /Edit annotation: Analysis: Mass Spectrometry/i }),
    ).toBeEnabled();
    await userEvent.keyboard('{Escape}');
    await waitFor(() => expect(screen.queryByTestId('context_menu')).not.toBeInTheDocument());

    // The edge menu follows the same rule. Every annotation it carries is one
    // of its own process's, so both actions stay live - none greyed.
    const edge = canvasElement.querySelector<HTMLElement>('[data-provenance-connector-edge-id]')!;
    fireEvent.contextMenu(edge, { clientX: 200, clientY: 200, bubbles: true });
    const edgeMenu = await screen.findByTestId('context_menu');

    expect(within(edgeMenu).getByRole('button', { name: /Delete connection/i })).toBeInTheDocument();
    expect(within(edgeMenu).getAllByText('Analysis: Mass Spectrometry')).toHaveLength(1);
    expect(
      within(edgeMenu).getByRole('button', { name: /Remove annotation: Analysis: Mass Spectrometry/i }),
    ).toBeEnabled();
    expect(
      within(edgeMenu).getByRole('button', { name: /Edit annotation: Analysis: Mass Spectrometry/i }),
    ).toBeEnabled();
  },
};

export const AnnotationContextMenuScrollsInsideTheViewport: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // The menu sizes to the window: floating-ui's size middleware caps it to
    // the space available at its spawn point (re-applied on window resize),
    // and overflow scrolls inside rather than running off screen.
    const outputA = canvas.getByText('Output A').closest('article')!;
    fireEvent.contextMenu(outputA, { clientX: 200, clientY: 200, bubbles: true });
    const menu = await screen.findByTestId('context_menu');

    await waitFor(() => expect(menu.style.maxHeight).toMatch(/px$/));
    expect(parseFloat(menu.style.maxHeight)).toBeGreaterThan(0);
    expect(parseFloat(menu.style.maxHeight)).toBeLessThanOrEqual(window.innerHeight);
    // Width is capped statically (class, not middleware), so it holds from
    // first paint and the action buttons never shift under a pointer.
    expect(parseFloat(getComputedStyle(menu).maxWidth)).toBeLessThanOrEqual(
      Math.max(480, window.innerWidth),
    );
    expect(getComputedStyle(menu).overflowY).toBe('auto');
  },
};

export const AnnotationMenuEntriesAreSortedAlphabetically: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const preview = () => canvas.getByTestId('provenance-mutation-preview').textContent ?? '';
    const addedLines = () =>
      preview()
        .split('\n')
        .filter((line) => line.startsWith('NodeAssignmentAdded')).length;

    // Drop two values in reverse-alphabetical insertion order, so the order
    // pinned below can only come from sorting, not from insertion.
    const inputA = canvas.getByText('Input A').closest('article')!;
    const zulu = await addRailProperty(canvas, 'Input', 'Zulu Marker', 'last', 'node');
    await dragByPointer(zulu as HTMLElement, inputA);
    await waitFor(() => expect(addedLines()).toBeGreaterThan(0));
    const afterZulu = addedLines();
    const alpha = await addRailProperty(canvas, 'Input', 'Alpha Marker', 'first', 'node');
    await dragByPointer(alpha as HTMLElement, inputA);
    await waitFor(() => expect(addedLines()).toBeGreaterThan(afterZulu));

    fireEvent.contextMenu(inputA, { clientX: 200, clientY: 200, bubbles: true });
    const menu = await screen.findByTestId('context_menu');
    const labels = Array.from(menu.querySelectorAll<HTMLElement>('.swt\\:col-start-2')).map(
      (element) => element.textContent ?? '',
    );
    expect(labels).toContain('Alpha Marker: first');
    expect(labels).toContain('Zulu Marker: last');
    expect(labels.indexOf('Alpha Marker: first')).toBeLessThan(labels.indexOf('Zulu Marker: last'));
    const lowercased = labels.map((label) => label.toLowerCase());
    expect(lowercased).toEqual([...lowercased].sort());
  },
};

// -- K.2: shelf, rail, and Recipe surfaces ----------------------------------

export const NodeAnnotationAppearsInEveryContainingLayersShelf: Story = {
  render: () => <Harness fixture="chained" />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // Culture Batch owns Batch Origin and appears in both the growth layer
    // (as its output) and the measurement layer (as its input). A node does
    // not belong to a layer, it appears in layers, so viewed from either
    // layer's shelf the assignment must show under every containing layer's
    // own source folder, not only the active one.
    const growthFolder = canvas.getByTestId('foldered-draggable-folder-source-fixture-growth-table');
    const growthShelf = await openShelfFolder(canvas, growthFolder);
    expect(growthShelf.getAllByRole('button', { name: /^Drag Batch Origin$/ })[0]).toBeInTheDocument();

    const measurementFolder = canvas.getByTestId('foldered-draggable-folder-source-fixture-measurement-table');
    const measurementShelf = await openShelfFolder(canvas, measurementFolder);
    expect(measurementShelf.getAllByRole('button', { name: /^Drag Batch Origin$/ })[0]).toBeInTheDocument();
  },
};

export const ShelfDragToRailClearsEveryFolderIndependentlyPerLayer: Story = {
  render: () => <Harness fixture="chained" />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // Bounce-back: dropping the shelf item somewhere that is not a rail
    // changes nothing, and it is still findable in its folder afterward.
    const source = await shelfProperty(canvas, 'Batch Origin');
    const nonRailTarget = canvas.getByText('Seed Stock').closest('article')!;
    await dragByPointer(source, nonRailTarget);
    await waitFor(() => expect(canvas.queryByTestId('foldered-draggable-drag-overlay')).not.toBeInTheDocument());
    expect(await shelfProperty(canvas, 'Batch Origin')).toBeInTheDocument();

    await ensurePropertyInRail(canvas, 'Input', 'Batch Origin');

    // The one property placement clears Batch Origin from every folder of
    // *this* layer's shelf at once, including the measurement-table folder
    // that only shows here because Culture Batch also appears there.
    const growthFolder = canvas.getByTestId('foldered-draggable-folder-source-fixture-growth-table');
    const growthShelf = await openShelfFolder(canvas, growthFolder);
    expect(growthShelf.queryAllByRole('button', { name: /^Drag Batch Origin$/ })).toHaveLength(0);

    const measurementFolderFromGrowthLayer = canvas.getByTestId(
      'foldered-draggable-folder-source-fixture-measurement-table',
    );
    const measurementShelfFromGrowthLayer = await openShelfFolder(canvas, measurementFolderFromGrowthLayer);
    expect(measurementShelfFromGrowthLayer.queryAllByRole('button', { name: /^Drag Batch Origin$/ })).toHaveLength(0);

    // Switching to the measurement layer is a different layer's shelf: rail
    // placement has no cross-layer memory, so it still shows there.
    await userEvent.click(canvas.getByTestId('provenance-layer-layer-2'));
    await waitFor(() => expect(canvas.getByTestId('provenance-layer-layer-2')).toHaveClass('swt:btn-primary'));

    expect(await shelfProperty(canvas, 'Batch Origin')).toBeInTheDocument();
  },
};

export const RecipeCatalogValuePlacedInRailAssignsAndReplacesAsAProcessValue: Story = {
  render: () => <Harness fixture="referenceCatalog" />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await ensurePropertyInRail(canvas, 'Output', 'Recipe');

    // Both stored Recipes are named "Extraction"; a per-item label cannot
    // tell them apart, so the rail disambiguates them from their ArcEditor
    // resource keys instead of dropping either as a duplicate.
    const panel = await expandProperty(canvas, 'Output', 'Recipe');
    expect(panel.getByText('Extraction (one)')).toBeInTheDocument();
    expect(panel.getByText('Extraction (two)')).toBeInTheDocument();

    // The link already carries the first Recipe and its dependent Component.
    // The Component is read-only, so it is observed where it is actually shown -
    // its rail chip - rather than through a context-menu entry, which the menu
    // now omits precisely because the action is unavailable.
    const componentPanel = await expandProperty(canvas, 'Output', 'Component');
    expect(componentPanel.getByText('Buffer')).toBeInTheDocument();

    // The property is also usable for grouping, same as any other header.
    await groupByProperty(canvasElement, 'Output', 'Recipe');
    const output = getGroupCard(canvasElement, 'Output', 'Recipe: Extraction');

    await selectGroup(output);
    const secondEntry = await railValue(canvas, 'Output', 'Recipe', 'Extraction (two)');
    await userEvent.click(
      within(secondEntry as HTMLElement).getByRole('button', { name: /apply to 1 selected group/i }),
    );

    await waitFor(() => {
      expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('AdapterResourceReferenceReplaced');
    });

    // Assigning through the catalog reuses the exact stored resource: no
    // Recipe/property/value editing mutation is recorded, only the reference
    // replacement.
    const preview = canvas.getByTestId('provenance-mutation-preview').textContent ?? '';
    expect(preview).not.toContain('PropertyValueDefinitionUpdated');
    expect(preview).not.toContain('PropertyDefinitionUpdated');

    // The second Recipe has no Components, so the old dependent projection for
    // the replaced link is gone: with no assignment left behind it, the whole
    // Component header leaves the rail.
    await waitFor(() =>
      expect(canvas.queryByTestId('provenance-property-Output-Component')).not.toBeInTheDocument(),
    );
  },
};

export const RecipeComponentsAreReadOnlyDependents: Story = {
  render: () => <Harness fixture="referenceCatalog" />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const output = canvas.getByText('Output').closest('article')!;

    // A Recipe Component is a container-bound dependent projection. This
    // fixture's "Recipe" label is just test data - the canonical model has no
    // idea it represents a ProcessCore Recipe (only ProcessCoreWritebackPlan.fs
    // knows that); what it does know, generically, is that a container-bound
    // (or Reference-valued) assignment is not directly editable. With neither
    // action available, the value contributes no menu entry at all
    // (GroupCard's existing container-bound check).
    fireEvent.contextMenu(output, { clientX: 200, clientY: 200, bubbles: true });
    const menu = await screen.findByTestId('context_menu');
    expect(within(menu).queryByText(/Component: Buffer/i)).not.toBeInTheDocument();
    await userEvent.keyboard('{Escape}');
    await waitFor(() => expect(screen.queryByTestId('context_menu')).not.toBeInTheDocument());

    // It still groups and displays like any other annotation.
    await groupByProperty(canvasElement, 'Output', 'Component');
    expect(getGroupCard(canvasElement, 'Output', 'Component: Buffer')).toBeInTheDocument();

    // Drag assignment, overwrite, and copy are rejected before they can even
    // be attempted: the rail chip is marked read-only rather than draggable,
    // and there is no "Add value" trigger through which a competing draft
    // could be authored to overwrite it (the same command-layer guard also
    // refuses a direct drag assignment or copy elsewhere - pinned in
    // Commands.Tests.fs: "a container-bound projection cannot be assigned
    // directly").
    const panel = await expandProperty(canvas, 'Output', 'Component');
    const chip = panel.getByRole('button', { name: 'Read-only Component value' });
    expect(chip).toHaveAttribute(
      'title',
      'Component values are read-only because they are stored inside the resource their process references, which the provenance editor does not edit.',
    );
    expect(panel.queryByText('Add value')).not.toBeInTheDocument();
  },
};

export const UndoAfterRecipeAssignmentRestoresReferenceSlotAndContainerBindings: Story = {
  render: () => <Harness fixture="referenceCatalog" />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    expect(canvas.getByTestId('provenance-undo')).toBeDisabled();

    await ensurePropertyInRail(canvas, 'Output', 'Recipe');
    await groupByProperty(canvasElement, 'Output', 'Recipe');
    const output = getGroupCard(canvasElement, 'Output', 'Recipe: Extraction');
    await selectGroup(output);

    const secondEntry = await railValue(canvas, 'Output', 'Recipe', 'Extraction (two)');
    await userEvent.click(
      within(secondEntry as HTMLElement).getByRole('button', { name: /apply to 1 selected group/i }),
    );

    await waitFor(() => {
      expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('AdapterResourceReferenceReplaced');
    });
    expect(canvas.getByTestId('provenance-undo')).not.toBeDisabled();

    for (
      let attempt = 0;
      attempt < 3 && !canvas.getByTestId('provenance-undo').hasAttribute('disabled');
      attempt += 1
    ) {
      fireEvent.click(canvas.getByTestId('provenance-undo'));
      await waitFor(() => expect(canvas.getByTestId('provenance-undo')).toBeDisabled(), {
        timeout: 1000,
      }).catch(() => undefined);
    }

    // Undo restores a whole prior session snapshot complete with its own
    // (shorter) journal, so the replacement's mutation is retracted for free
    // rather than needing its own inverse recorded.
    await waitFor(() => {
      expect(canvas.getByTestId('provenance-undo')).toBeDisabled();
      expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('No mutations recorded.');
    });

    // The restored snapshot's reference slot and container binding are intact:
    // the dependent Component projection the replacement would have carried
    // away is back, visible again as its own read-only rail chip.
    expect(getGroupCard(canvasElement, 'Output', 'Recipe: Extraction')).toBeInTheDocument();
    const componentPanel = await expandProperty(canvas, 'Output', 'Component');
    expect(componentPanel.getByText('Buffer')).toBeInTheDocument();
  },
};

// Step L.1's repaint-half measurement. The .NET half (Performance.Tests.fs)
// measures the canonical commit and availability resolution in isolation for
// all three scenarios; this measures the value-only edit end to end - through
// a real click, the canonical commit, and React's repaint of the active
// layer - over a stated repetition count, reporting p50/p95. "First correct
// repaint" is asserted on the layer itself: the active layer is grouped by
// the edited header first, so the new value must appear in a group card
// title, not merely in the journal preview. `waitFor` polls rather than
// hooking React's commit phase directly, so the samples carry that polling
// interval as measurement noise; the .NET half carries the precise
// distributions for the two cold scenarios, whose browser gestures (drop,
// connect) cannot be repeated cheaply here.
export const MeasuresEditToRepaintLatencyOnALargeSession: Story = {
  render: () => <Harness fixture="performance" />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const repetitions = 7;

    await groupByProperty(canvasElement, 'Input', 'Batch');
    getGroupCard(canvasElement, 'Input', 'Batch: Batch-0-0');

    await userEvent.click(canvas.getByTestId('provenance-global-values-trigger'));
    const panel = await waitFor(() => screen.getByTestId('provenance-global-values-panel'));

    const samples: number[] = [];

    for (let n = 1; n <= repetitions; n += 1) {
      await userEvent.click(
        within(panel).getByTestId('provenance-global-edit-value-perf-value-node-0-0'),
      );
      const valueInput = await waitFor(() => screen.getByTestId('provenance-global-edit-value-input'));
      await userEvent.clear(valueInput);
      await userEvent.type(valueInput, `Repainted-${n}`);

      const start = performance.now();
      await userEvent.click(screen.getByTestId('provenance-confirm-global-value-edit'));
      await waitFor(
        () => {
          // The edited value has actually painted on the active layer...
          getGroupCard(canvasElement, 'Input', `Batch: Repainted-${n}`);
          // ...and exactly this edit's journal entry is in the preview.
          const preview = canvas.getByTestId('provenance-mutation-preview').textContent ?? '';
          expect(
            preview.split('\n').filter((line) => line.startsWith('PropertyValueDefinitionUpdated')),
          ).toHaveLength(n);
        },
        { timeout: 30000 },
      );
      samples.push(performance.now() - start);
    }

    const sorted = [...samples].sort((a, b) => a - b);
    const percentile = (p: number) =>
      sorted[Math.min(sorted.length - 1, Math.max(0, Math.ceil(p * sorted.length) - 1))];

    console.info(
      `[provenance-benchmark] edit-to-repaint on ${perfLayers}x${perfNodesPerSide} nodes/side ` +
        `(density=${perfEdgeDensity}, repetitions=${repetitions}): ` +
        `p50 ${percentile(0.5).toFixed(1)}ms, p95 ${percentile(0.95).toFixed(1)}ms`,
    );
    expect(samples).toHaveLength(repetitions);
  },
};

// Regression: the downstream edit form keeps its draft kind independent of the
// typed text. Deriving the kind back from a half-typed value flipped Integer/
// Float edits to Text on the first keystroke and made switching to Term
// impossible (the Term option produced no value, so the select snapped back).
export const AnnotationEditFormPreservesTheDraftKind: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // Make Input D's Species a numeric value first, through the global
    // sidebar (its value is referenced by D alone, so the edit is in place).
    const trigger = canvas.getByTestId('provenance-global-values-trigger');
    await userEvent.click(trigger);
    const panel = await waitFor(() => screen.getByTestId('provenance-global-values-panel'));
    await userEvent.click(within(panel).getByTestId('provenance-global-edit-value-value-species-chlamydomonas'));
    const globalInput = await waitFor(() => screen.getByTestId('provenance-global-edit-value-input'));
    await userEvent.selectOptions(within(panel).getByRole('combobox'), 'Integer');
    await userEvent.clear(globalInput);
    await userEvent.type(globalInput, '37');
    await userEvent.click(screen.getByTestId('provenance-confirm-global-value-edit'));
    await waitFor(() => {
      expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('PropertyValueDefinitionUpdated');
    });
    await userEvent.click(trigger);
    await waitFor(() => expect(screen.queryByTestId('provenance-global-values-panel')).not.toBeInTheDocument());

    // Editing the now-Integer annotation downstream: the form opens as
    // Integer and typing must not degrade it to Text.
    const inputD = canvas.getByText('Input D').closest('article')!;
    fireEvent.contextMenu(inputD, { clientX: 200, clientY: 200, bubbles: true });
    await screen.findByTestId('context_menu');
    await clickMenuAction(/Edit annotation: Species: 37/i);

    const kindSelect = await waitFor(() => canvas.getByRole('combobox', { name: 'Value type' }));
    expect(kindSelect).toHaveValue('Integer');

    const valueInput = canvas.getByTestId('provenance-annotation-edit-value');
    await userEvent.clear(valueInput);
    await userEvent.type(valueInput, '42');
    expect(kindSelect).toHaveValue('Integer');

    await userEvent.click(canvas.getByTestId('provenance-confirm-annotation-edit'));
    await waitFor(() => {
      fireEvent.contextMenu(inputD, { clientX: 200, clientY: 200, bubbles: true });
      const reopened = screen.getByTestId('context_menu');
      expect(
        within(reopened).getByRole('button', { name: /Remove annotation: Species: 42/i }),
      ).toBeInTheDocument();
    });
    await userEvent.keyboard('{Escape}');
    await waitFor(() => expect(screen.queryByTestId('context_menu')).not.toBeInTheDocument());

    // Switching the kind to Term now genuinely switches the form: the text
    // input yields to the term search and the save stays disabled until a
    // term is chosen.
    fireEvent.contextMenu(inputD, { clientX: 200, clientY: 200, bubbles: true });
    await screen.findByTestId('context_menu');
    await clickMenuAction(/Edit annotation: Species: 42/i);

    const kindSelectAgain = await waitFor(() => canvas.getByRole('combobox', { name: 'Value type' }));
    await userEvent.selectOptions(kindSelectAgain, 'Term');
    expect(kindSelectAgain).toHaveValue('Term');
    expect(canvas.queryByTestId('provenance-annotation-edit-value')).not.toBeInTheDocument();
    expect(canvas.getByTestId('provenance-confirm-annotation-edit')).toBeDisabled();
    await userEvent.click(canvas.getByTestId('provenance-cancel-annotation-edit'));
  },
};

// Regression: an owned annotation on a multi-member card is edited at its
// owning member. The receiver used to be whichever member sorted first, which
// `editAvailableReferences`'s owner/receiver consistency check rejected with
// an internal-inconsistency error when the owner sorted later.
export const OwnedAnnotationOnAMultiMemberCardEditsAtItsOwner: Story = {
  render: () => <Harness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // Give Input B a Species of its own: overwrite its shared Arabidopsis
    // with the existing Chlamydomonas value, so exactly one member of the
    // upcoming card owns it - and that member sorts after Input A.
    const chlamydomonas = await railValue(canvas, 'Input', 'Species', 'Chlamydomonas');
    const inputB = canvas.getByText('Input B').closest('article')!;
    await dragByPointer(chlamydomonas as HTMLElement, inputB);
    await waitFor(() => expect(canvas.getByTestId('provenance-overwrite-warning')).toBeInTheDocument());
    await userEvent.click(canvas.getByTestId('provenance-confirm-overwrite'));
    await waitFor(() => {
      const preview = canvas.getByTestId('provenance-mutation-preview').textContent ?? '';
      expect(preview.split('\n').filter((line) => line.startsWith('NodeAssignmentValueChanged'))).toHaveLength(1);
    });

    // Temperature: 12 C is incident to Inputs A and B, so grouping by it
    // makes one card whose sorted-first member (A) is not the owner (B).
    await groupByProperty(canvasElement, 'Input', 'Temperature');
    const card = await waitFor(() => getGroupCard(canvasElement, 'Input', 'Temperature: 12 C'));

    fireEvent.contextMenu(card, { clientX: 200, clientY: 200, bubbles: true });
    await screen.findByTestId('context_menu');
    await clickMenuAction(/Edit annotation: Species: Chlamydomonas/i);

    const valueInput = await waitFor(() => canvas.getByTestId('provenance-annotation-edit-value'));
    await userEvent.clear(valueInput);
    await userEvent.type(valueInput, 'Nicotiana');
    await userEvent.click(canvas.getByTestId('provenance-confirm-annotation-edit'));

    // The edit resolves to Input B's assignment: a second value change lands
    // (detached from Input D's Chlamydomonas, which stays untouched), with no
    // "does not belong to receiver" refusal.
    await waitFor(() => {
      const preview = canvas.getByTestId('provenance-mutation-preview').textContent ?? '';
      expect(preview.split('\n').filter((line) => line.startsWith('NodeAssignmentValueChanged'))).toHaveLength(2);
    });
    expect(canvas.queryByText(/does not belong to receiver/i)).not.toBeInTheDocument();

    fireEvent.contextMenu(card, { clientX: 200, clientY: 200, bubbles: true });
    const after = await screen.findByTestId('context_menu');
    expect(
      within(after).getByRole('button', { name: /Remove annotation: Species: Nicotiana/i }),
    ).toBeInTheDocument();
  },
};

// The catalog Recipe chip's actual drag gesture: dropping a second stored
// Recipe on a card whose link already carries one is a slot replacement
// (intent §3), the same command the "Apply to selection" button story proves -
// this pins the drag path itself.
export const DraggingACatalogRecipeReplacesTheOccupiedSlot: Story = {
  render: () => <Harness fixture="referenceCatalog" />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await ensurePropertyInRail(canvas, 'Output', 'Recipe');
    const secondEntry = await railValue(canvas, 'Output', 'Recipe', 'Extraction (two)');
    const output = canvas.getByText('Output').closest('article')!;
    await dragByPointer(secondEntry as HTMLElement, output);

    await waitFor(() => {
      expect(canvas.getByTestId('provenance-mutation-preview')).toHaveTextContent('AdapterResourceReferenceReplaced');
    });

    // The stored resource is reused exactly - no definition edit is recorded -
    // and the replaced Recipe's dependent Component projection is gone.
    const preview = canvas.getByTestId('provenance-mutation-preview').textContent ?? '';
    expect(preview).not.toContain('PropertyValueDefinitionUpdated');
    expect(preview).not.toContain('PropertyDefinitionUpdated');

    fireEvent.contextMenu(output, { clientX: 200, clientY: 200, bubbles: true });
    const menu = await screen.findByTestId('context_menu');
    expect(within(menu).queryByText(/Component: Buffer/i)).not.toBeInTheDocument();
  },
};

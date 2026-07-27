import type { Meta, StoryObj } from '@storybook/react-vite';
import { useState } from 'react';
import ErrorModalProvider from '../../Primitive/ErrorModal/Provider.fs.js';
import MemberList from './MemberList.fs.js';
import ObjectBrowser from './ObjectBrowser.fs.js';
import { Items as memberCatalogItems } from './MemberCatalog.fs.js';
import { createProcessCoreArcFixture } from './ObjectBrowser.fixture.js';

const ObjectBrowserExample = () => {
  const [arc, setArc] = useState(createProcessCoreArcFixture);
  const [selectedKind, setSelectedKind] = useState(memberCatalogItems[0].data);

  return (
    <ErrorModalProvider>
      <div className="swt:grid swt:h-[42rem] swt:grid-cols-[20rem_minmax(0,1fr)] swt:gap-4">
        <aside className="swt:min-h-0 swt:overflow-y-auto swt:border-r swt:border-base-300 swt:pr-4">
          <MemberList
            arcStateCtx={{
              state: arc,
              setStateUpdater: update => setArc(current => update(current) ?? current),
            }}
            onSelect={setSelectedKind}
            onSelectEntity={entity => setSelectedKind(entity.memberKind)}
            selectedKind={selectedKind}
          />
        </aside>
        <main className="swt:min-h-0">
          <ObjectBrowser
            arcStateCtx={{
              state: arc,
              setStateUpdater: update => setArc(current => update(current) ?? current),
            }}
            kind={selectedKind}
          />
        </main>
      </div>
    </ErrorModalProvider>
  );
};

const meta = {
  title: 'Page Components/ObjectBrowser',
  component: ObjectBrowserExample,
  tags: ['autodocs'],
} satisfies Meta<typeof ObjectBrowserExample>;

export default meta;

type Story = StoryObj<typeof meta>;

export const Default: Story = {};

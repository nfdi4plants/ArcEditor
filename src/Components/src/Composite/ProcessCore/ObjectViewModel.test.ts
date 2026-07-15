import { describe, expect, it } from 'vitest';
import {
  MemberKind_Agent,
  MemberKind_Annotation,
  MemberKind_Data,
  MemberKind_DataContext,
  MemberKind_Dataset,
  MemberKind_Organization,
  MemberKind_Process,
  MemberKind_Recipe,
  MemberKind_Sample,
  MemberKind_ScholarlyArticle,
} from './MemberCatalog.fs.js';
import { getNames } from './ObjectViewModel.fs.js';
import {
  createFallbackArcFixture,
  createProcessCoreArcFixture,
} from './ObjectBrowser.fixture.js';

const namesFor = (arc: ReturnType<typeof createProcessCoreArcFixture>, kind: Parameters<typeof getNames>[1]) =>
  getNames(arc, kind);

describe('Process Core object view model', () => {
  it('collects all ten kinds across the complete ARC graph in discovery order', () => {
    const arc = createProcessCoreArcFixture();

    expect(namesFor(arc, MemberKind_Dataset())).toEqual([
      'Child dataset',
      'grandchild-dataset',
    ]);
    expect(namesFor(arc, MemberKind_Process())).toEqual([
      'Extraction process',
      'Analysis process',
    ]);
    expect(namesFor(arc, MemberKind_Sample())).toEqual([
      'Source sample',
      'Result sample',
    ]);
    expect(namesFor(arc, MemberKind_Data())).toEqual(['dataset/results.csv']);
    expect(namesFor(arc, MemberKind_Recipe())).toEqual(['Extraction recipe']);
    expect(namesFor(arc, MemberKind_Annotation())).toEqual(['Temperature']);
    expect(namesFor(arc, MemberKind_DataContext())).toEqual(['Measured values']);
    expect(namesFor(arc, MemberKind_Agent())).toEqual(['Ada Lovelace', 'Grace Hopper']);
    expect(namesFor(arc, MemberKind_Organization())).toEqual([
      'Research organization',
    ]);
    expect(namesFor(arc, MemberKind_ScholarlyArticle())).toEqual([
      'Research article',
    ]);
  });

  it('uses type-specific names when Process Core values are blank', () => {
    const arc = createFallbackArcFixture();

    expect(namesFor(arc, MemberKind_Dataset())).toEqual(['Unnamed dataset']);
    expect(namesFor(arc, MemberKind_Process())).toEqual(['Unnamed process']);
    expect(namesFor(arc, MemberKind_Sample())).toEqual(['Unnamed sample']);
    expect(namesFor(arc, MemberKind_Data())).toEqual(['Unnamed data']);
    expect(namesFor(arc, MemberKind_Recipe())).toEqual(['Unnamed recipe']);
    expect(namesFor(arc, MemberKind_Annotation())).toEqual(['Unnamed annotation']);
    expect(namesFor(arc, MemberKind_DataContext())).toEqual([
      'Unnamed data context',
    ]);
    expect(namesFor(arc, MemberKind_Agent())).toEqual(['Unnamed agent']);
    expect(namesFor(arc, MemberKind_Organization())).toEqual([
      'Unnamed organization',
    ]);
    expect(namesFor(arc, MemberKind_ScholarlyArticle())).toEqual([
      'Unnamed scholarly article',
    ]);
  });
});

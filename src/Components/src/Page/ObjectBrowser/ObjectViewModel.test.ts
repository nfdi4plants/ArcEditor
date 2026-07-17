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
} from './Types.fs.js';
import {
  getEntities,
  getNames,
  removeEntities,
  removeEntity,
} from './ObjectViewModel.fs.js';
import { Annotation } from '../../fable_modules/ProcessCore.Javascript.0.0.8/Annotation.fs.js';
import {
  Data,
  DataContext,
} from '../../fable_modules/ProcessCore.Javascript.0.0.8/Graph.fs.js';
import {
  createFallbackArcFixture,
  createProcessCoreArcFixture,
} from './ObjectBrowser.fixture.js';

type ArcFixture = ReturnType<typeof createProcessCoreArcFixture>;
type Kind = Parameters<typeof getNames>[1];

const namesFor = (arc: ArcFixture, kind: Kind) =>
  getNames(arc, kind);

const entityNamed = (arc: ArcFixture, kind: Kind, displayName: string) => {
  const entity = getEntities(arc, kind).find(
    candidate => candidate.displayName === displayName,
  );

  expect(entity, `${displayName} should be enumerated`).toBeDefined();
  return entity!;
};

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

    for (const kind of [
      MemberKind_Dataset(),
      MemberKind_Process(),
      MemberKind_Sample(),
      MemberKind_Data(),
      MemberKind_Recipe(),
      MemberKind_Annotation(),
      MemberKind_DataContext(),
      MemberKind_Agent(),
      MemberKind_Organization(),
      MemberKind_ScholarlyArticle(),
    ]) {
      for (const entity of getEntities(arc, kind)) {
        expect(entity.memberKind.tag).toBe(kind.tag);
        expect(entity.key).not.toBe('');
        expect(entity.displayName).not.toBe('');
        expect(entity.value.fields[0]).toBeDefined();
      }
    }
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

  it('removes a nested dataset subtree from its parent', () => {
    const arc = createProcessCoreArcFixture();

    removeEntity(
      arc,
      entityNamed(arc, MemberKind_Dataset(), 'Child dataset'),
    );

    expect(namesFor(arc, MemberKind_Dataset())).toEqual([]);
    expect(namesFor(arc, MemberKind_Process())).toEqual([]);
  });

  it('detaches process inputs and outputs before removing the process', () => {
    const arc = createProcessCoreArcFixture();

    removeEntity(
      arc,
      entityNamed(arc, MemberKind_Process(), 'Extraction process'),
    );

    expect(namesFor(arc, MemberKind_Process())).toEqual(['Analysis process']);
    expect(namesFor(arc, MemberKind_Sample())).toEqual(['Result sample']);
  });

  it('removes a sample from every process lane', () => {
    const arc = createProcessCoreArcFixture();
    const source = arc.AllSamples().find(sample => sample.Name === 'Source sample')!;
    arc.AllProcesses().find(process => process.Name === 'Analysis process')!.AddOutputSample(source);

    removeEntity(
      arc,
      entityNamed(arc, MemberKind_Sample(), 'Source sample'),
    );

    expect(namesFor(arc, MemberKind_Sample())).toEqual(['Result sample']);
    expect(arc.AllProcesses().find(process => process.Name === 'Extraction process')?.Inputs).toHaveLength(0);
    expect(
      arc.AllProcesses().flatMap(process => [...process.Inputs, ...process.Outputs])
        .some(node => node.fields[0].Name === 'Source sample'),
    ).toBe(false);
  });

  it('removes data from process lanes, dataset files, parents, and required data contexts', () => {
    const arc = createProcessCoreArcFixture();
    const data = arc.AllData().find(candidate => candidate.Name === 'dataset/results.csv')!;
    const parentData = new Data('dataset/container');
    parentData.AddPart(data);
    arc.HasPart[0].AddDataFile(parentData);

    removeEntity(
      arc,
      entityNamed(arc, MemberKind_Data(), 'dataset/results.csv'),
    );

    expect(namesFor(arc, MemberKind_Data())).toEqual(['dataset/container']);
    expect(namesFor(arc, MemberKind_DataContext())).toEqual([]);
    expect(arc.AllProcesses().flatMap(process => [...process.Inputs, ...process.Outputs])).toHaveLength(2);
    expect(arc.HasPart[0].DataFiles).toEqual([parentData]);
    expect(parentData.HasPart).toHaveLength(0);
  });

  it('clears matching recipes without deleting their supporting processes', () => {
    const arc = createProcessCoreArcFixture();

    removeEntity(
      arc,
      entityNamed(arc, MemberKind_Recipe(), 'Extraction recipe'),
    );

    expect(namesFor(arc, MemberKind_Recipe())).toEqual([]);
    expect(namesFor(arc, MemberKind_Process())).toEqual([
      'Extraction process',
      'Analysis process',
    ]);
    expect(arc.AllProcesses().every(process => process.ExecutesProtocol == null)).toBe(true);
  });

  it('removes every matching annotation occurrence', () => {
    const arc = createProcessCoreArcFixture();
    const matchingAnnotation = () => new Annotation('Temperature', '20');
    const extraction = arc.AllProcesses().find(process => process.Name === 'Extraction process')!;
    const source = arc.AllSamples().find(sample => sample.Name === 'Source sample')!;
    const data = arc.AllData().find(candidate => candidate.Name === 'dataset/results.csv')!;
    const child = arc.HasPart[0];
    const agent = child.Agents[0];
    const article = child.Citations[0];

    arc.AddAdditionalProperty(matchingAnnotation());
    extraction.ExecutesProtocol!.AddComponent(matchingAnnotation());
    extraction.ExecutesProtocol!.AddAdditionalProperty(matchingAnnotation());
    source.AddAdditionalProperty(matchingAnnotation());
    data.AddAdditionalProperty(matchingAnnotation());
    agent.AddAdditionalProperty(matchingAnnotation());
    article.Authors[0].AddAdditionalProperty(matchingAnnotation());
    article.AddAdditionalProperty(matchingAnnotation());

    removeEntity(
      arc,
      entityNamed(arc, MemberKind_Annotation(), 'Temperature'),
    );

    expect(namesFor(arc, MemberKind_Annotation())).toEqual([]);
    expect(arc.AllProcesses().flatMap(process => [...process.ParameterValue])).toHaveLength(0);
    expect(arc.AdditionalProperty).toHaveLength(0);
    expect(extraction.ExecutesProtocol!.Components).toHaveLength(0);
    expect(extraction.ExecutesProtocol!.AdditionalProperty).toHaveLength(0);
    expect(source.AdditionalProperty).toHaveLength(0);
    expect(data.AdditionalProperty).toHaveLength(0);
    expect(agent.AdditionalProperty).toHaveLength(0);
    expect(article.Authors[0].AdditionalProperty).toHaveLength(0);
    expect(article.AdditionalProperty).toHaveLength(0);
  });

  it('removes a data context from its owning dataset', () => {
    const arc = createProcessCoreArcFixture();
    const child = arc.HasPart[0];
    const duplicateContext = new DataContext(
      child.DataFiles[0],
      undefined,
      undefined,
      undefined,
      'Measured values',
    );
    child.HasPart[0].AddDataContext(duplicateContext);

    removeEntity(
      arc,
      entityNamed(arc, MemberKind_DataContext(), 'Measured values'),
    );

    expect(namesFor(arc, MemberKind_DataContext())).toEqual([]);
    expect(namesFor(arc, MemberKind_Data())).toEqual(['dataset/results.csv']);
    expect(child.DataContexts).toHaveLength(0);
    expect(child.HasPart[0].DataContexts).toHaveLength(0);
  });

  it('removes an agent from datasets while preserving unrelated article authors', () => {
    const arc = createProcessCoreArcFixture();
    const child = arc.HasPart[0];
    child.Citations[0].AddAuthor(child.Agents[0]);

    removeEntity(
      arc,
      entityNamed(arc, MemberKind_Agent(), 'Ada Lovelace'),
    );

    expect(namesFor(arc, MemberKind_Agent())).toEqual(['Grace Hopper']);
    expect(child.Agents).toHaveLength(0);
    expect(child.Citations[0].Authors).toHaveLength(1);
    expect(child.Citations[0].Authors[0].GivenName).toBe('Grace');
  });

  it('clears organization affiliations without deleting their agents', () => {
    const arc = createProcessCoreArcFixture();

    removeEntity(
      arc,
      entityNamed(arc, MemberKind_Organization(), 'Research organization'),
    );

    expect(namesFor(arc, MemberKind_Organization())).toEqual([]);
    expect(namesFor(arc, MemberKind_Agent())).toEqual(['Ada Lovelace', 'Grace Hopper']);
    expect(arc.HasPart[0].Agents[0].Affiliation).toBeUndefined();
    expect(arc.HasPart[0].Citations[0].Authors[0].Affiliation).toBeUndefined();
  });

  it('removes a scholarly article from every owning dataset', () => {
    const arc = createProcessCoreArcFixture();
    const article = arc.HasPart[0].Citations[0];
    arc.HasPart[0].HasPart[0].AddCitation(article);

    removeEntity(
      arc,
      entityNamed(arc, MemberKind_ScholarlyArticle(), 'Research article'),
    );

    expect(namesFor(arc, MemberKind_ScholarlyArticle())).toEqual([]);
    expect(namesFor(arc, MemberKind_Agent())).toEqual(['Ada Lovelace']);
    expect(arc.HasPart[0].Citations).toHaveLength(0);
    expect(arc.HasPart[0].HasPart[0].Citations).toHaveLength(0);
  });

  it('bulk-removes every selected entity', () => {
    const arc = createProcessCoreArcFixture();
    const samples = getEntities(arc, MemberKind_Sample());

    removeEntities(arc, samples);

    expect(namesFor(arc, MemberKind_Sample())).toEqual([]);
    expect(namesFor(arc, MemberKind_Process())).toHaveLength(2);
  });
});

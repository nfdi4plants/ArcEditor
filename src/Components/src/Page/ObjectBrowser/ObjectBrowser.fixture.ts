import { ARC } from '../../fable_modules/ProcessCore.Javascript.0.0.8/ARC.fs.js';
import {
  Data,
  DataContext,
  Dataset,
  IONode,
  Process,
  Recipe,
  Sample,
} from '../../fable_modules/ProcessCore.Javascript.0.0.8/Graph.fs.js';
import {
  Agent,
  Organization,
  ScholarlyArticle,
} from '../../fable_modules/ProcessCore.Javascript.0.0.8/Administrative.fs.js';
import { Annotation } from '../../fable_modules/ProcessCore.Javascript.0.0.8/Annotation.fs.js';

const sampleNode = (sample: Sample) => new IONode(0, [sample]);
const dataNode = (data: Data) => new IONode(1, [data]);

export const createProcessCoreArcFixture = () => {
  const arc = new ARC('root-arc', 'Root ARC');
  const child = new Dataset('child-dataset', 'Child dataset');
  const grandchild = new Dataset('grandchild-dataset');

  const organization = new Organization('Research organization', 'organization-1');
  const datasetAgent = new Agent(
    'Ada',
    'agent-1',
    'Lovelace',
    'ada@example.org',
    organization,
  );
  const citationAuthor = new Agent(
    'Grace',
    'agent-2',
    'Hopper',
    'grace@example.org',
    organization,
  );
  const article = new ScholarlyArticle(
    'Research article',
    'article-1',
    undefined,
    undefined,
    [citationAuthor],
  );

  const recipe = new Recipe('Extraction recipe', undefined, '1');
  const annotation = new Annotation('Temperature', '20', '°C');
  const source = new Sample('Source sample');
  const result = new Sample('Result sample');
  const data = new Data('dataset/results.csv');

  const extraction = new Process(
    'Extraction process',
    recipe,
    undefined,
    [sampleNode(source)],
    [dataNode(data)],
    [annotation],
  );
  const analysis = new Process(
    'Analysis process',
    recipe,
    undefined,
    [dataNode(data)],
    [sampleNode(result)],
  );

  child.AddProcess(extraction);
  child.AddDataFile(data);
  child.AddAgent(datasetAgent);
  child.AddCitation(article);
  child.AddDataContext(
    new DataContext(data, undefined, undefined, undefined, 'Measured values'),
  );
  grandchild.AddProcess(analysis);
  child.AddPart(grandchild);
  arc.AddPart(child);

  return arc;
};

export const createFallbackArcFixture = () => {
  const arc = new ARC('fallback-arc');
  const child = new Dataset('', '   ');
  const recipe = new Recipe();
  const annotation = new Annotation('');
  const sample = new Sample('');
  const data = new Data('');
  const organization = new Organization('');
  const agent = new Agent('', undefined, undefined, undefined, organization);
  const article = new ScholarlyArticle('');
  const process = new Process(
    '',
    recipe,
    undefined,
    [sampleNode(sample)],
    [dataNode(data)],
    [annotation],
  );

  child.AddProcess(process);
  child.AddAgent(agent);
  child.AddCitation(article);
  child.AddDataContext(new DataContext(data));
  arc.AddPart(child);

  return arc;
};

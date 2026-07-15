import { describe, expect, it } from 'vitest';
import { MemberCatalog_Items as items } from './MemberCatalog.fs.js';

const expectedLabels = [
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

const expectedIcons = [
  'swt:iconify-color swt:fluent-color--database-20',
  'swt:iconify-color swt:fluent-color--arrow-clockwise-dashes-settings-20',
  'swt:iconify-color swt:fluent-color--molecule-20',
  'swt:iconify-color swt:fluent-color--data-line-20',
  'swt:iconify-color swt:fluent-color--clipboard-text-edit-20',
  'swt:iconify-color swt:fluent-color--comment-multiple-20',
  'swt:iconify-color swt:fluent-color--content-view-20',
  'swt:iconify-color swt:fluent-color--agents-20',
  'swt:iconify-color swt:fluent-color--org-20',
  'swt:iconify-color swt:fluent-color--document-text-20',
];

describe('Process Core member catalog', () => {
  it('contains the ten representatives in Process Core order', () => {
    expect(items.map(item => item.label)).toEqual(expectedLabels);
    expect(items.map(item => item.icon)).toEqual(expectedIcons);
  });

  it('uses a unique kind, label, and icon for each representative', () => {
    expect(new Set(items.map(item => item.data.tag)).size).toBe(10);
    expect(new Set(items.map(item => item.label)).size).toBe(10);
    expect(new Set(items.map(item => item.icon)).size).toBe(10);
  });
});

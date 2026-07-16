import { describe, expect, it } from 'vitest';
import { displayName } from './ArcSidebarHelper.fs.js';

describe('ARC sidebar helpers', () => {
  it('uses a non-empty title before the identifier', () => {
    expect(displayName('  Human-readable ARC  ', 'arc-id')).toBe('Human-readable ARC');
  });

  it('falls back to identifier and then a defensive label', () => {
    expect(displayName('   ', '  arc-id  ')).toBe('arc-id');
    expect(displayName(undefined, '   ')).toBe('Untitled ARC');
  });
});

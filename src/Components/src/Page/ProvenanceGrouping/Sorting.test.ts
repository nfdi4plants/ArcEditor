import { describe, expect, it } from 'vitest';
import { Display_sortGroups } from './PropertyModel.fs.ts';
import { DisplayGroup } from './Types/ProjectionTypes.fs.ts';
import { ProvenanceSide_Output } from './Types/Identifiers.fs.ts';
import { GroupSort_NameAsc, GroupSort_MemberCountDesc, GroupSort_ConnectionCountDesc, type GroupSort_$union } from './Types.fs.ts';
import { ofArray, empty as emptyList } from '../../fable_modules/fable-library-ts.5.0.0-alpha.21/List.ts';
import { ofArray as setOfArray, empty as emptySet } from '../../fable_modules/fable-library-ts.5.0.0-alpha.21/Set.ts';
import { empty as emptyMap } from '../../fable_modules/fable-library-ts.5.0.0-alpha.21/Map.ts';
import { compare } from '../../fable_modules/fable-library-ts.5.0.0-alpha.21/Util.ts';

const comparer = { Compare: compare };
const group = (id: string, members = 1) => new DisplayGroup(
  id, ProvenanceSide_Output(), emptyList(),
  setOfArray(Array.from({ length: members }, (_, i) => `${id}-member-${i}`), comparer),
  emptySet(comparer), emptySet(comparer), emptyList(), emptyMap(comparer),
);

function sorted(sort: GroupSort_$union, groups: DisplayGroup[], names: Record<string, string>, counts: Record<string, number> = {}) {
  return Array.from(Display_sortGroups(sort, g => names[g.Id], g => counts[g.Id] ?? 0, ofArray(groups))).map(g => g.Id);
}

describe('provenance card sorting', () => {
  it('uses displayed names instead of internal IDs, ignoring case and sorting numbers naturally', () => {
    const groups = [group('group:1'), group('group:10'), group('group:2'), group('group:3')];
    const names = { 'group:1': 'Sample 10', 'group:10': 'sample 2', 'group:2': ' Zebra', 'group:3': 'apple' };
    expect(sorted(GroupSort_NameAsc(), groups, names)).toEqual(['group:3', 'group:10', 'group:1', 'group:2']);
  });

  it('sorts grouped labels and arbitrarily long numeric runs by their displayed value', () => {
    const groups = [group('a'), group('b'), group('c')];
    const names = { a: 'Replicate: 100000000000000000000', b: 'Replicate: 2', c: 'Replicate: 99999999999999999999' };
    expect(sorted(GroupSort_NameAsc(), groups, names)).toEqual(['b', 'c', 'a']);
  });

  it('keeps singleton cards alphabetical when switching to member count', () => {
    const groups = [group('a'), group('b'), group('c')];
    const names = { a: 'Zebra', b: 'apple', c: 'Sample 2' };
    expect(sorted(GroupSort_MemberCountDesc(), groups, names)).toEqual(sorted(GroupSort_NameAsc(), groups, names));
  });

  it('orders most members first, breaking ties by visible name', () => {
    const groups = [group('a', 2), group('b', 2), group('c', 1), group('d', 3)];
    const names = { a: 'Zebra', b: 'apple', c: 'Aardvark', d: 'Zulu' };
    expect(sorted(GroupSort_MemberCountDesc(), groups, names)).toEqual(['d', 'b', 'a', 'c']);
  });

  it('uses the supplied badge counts and alphabetical ties for most connections', () => {
    const groups = [group('a'), group('b'), group('c'), group('d')];
    const names = { a: 'Zebra', b: 'apple', c: 'Aardvark', d: 'Zulu' };
    expect(sorted(GroupSort_ConnectionCountDesc(), groups, names, { a: 2, b: 2, c: 0, d: 3 })).toEqual(['d', 'b', 'a', 'c']);
  });

  it('uses stable identity when displayed names compare equally', () => {
    const groups = [group('b'), group('a')];
    const names = { a: 'sample 02', b: 'Sample 2' };
    for (const sort of [GroupSort_NameAsc(), GroupSort_MemberCountDesc(), GroupSort_ConnectionCountDesc()]) {
      expect(sorted(sort, groups, names)).toEqual(['a', 'b']);
      expect(sorted(sort, [...groups].reverse(), names)).toEqual(['a', 'b']);
    }
  });
});

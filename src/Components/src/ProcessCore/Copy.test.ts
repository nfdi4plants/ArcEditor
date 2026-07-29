import { describe, expect, it } from 'vitest';
import { ARC } from '../fable_modules/ProcessCore.Javascript.0.1.2/ARC.fs.js';
import {
  Process,
  Recipe,
  Sample,
} from '../fable_modules/ProcessCore.Javascript.0.1.2/Graph.fs.js';
import { copyArc } from './Copy.fs.js';
import { findEmptyPrimaryFields } from './Hotfixes.fs.js';
import { recipes } from './ObjectGraph.fs.js';

const setDurableId = (recipe: Recipe, id: string) => {
  recipe.SetProperty('@id', id);
  return recipe;
};

const durableId = (recipe: Recipe) =>
  recipe.TryGetPropertyValue('@id') as string | undefined;

describe('ProcessCore copying and traversal', () => {
  it('ARC.Copy retains samples and recipes', () => {
    const arc = new ARC('copy-source');
    const sample = new Sample('unassigned sample');
    const recipe = setDurableId(
      new Recipe('unassigned recipe'),
      'recipe:unassigned',
    );
    arc.AddSample(sample);
    arc.AddRecipe(recipe);

    const copy = copyArc(arc);

    expect(copy.Samples.map(candidate => candidate.Name)).toEqual([
      'unassigned sample',
    ]);
    expect(copy.Recipes.map(durableId)).toEqual(['recipe:unassigned']);
    expect(copy.Samples[0]).not.toBe(sample);
    expect(copy.Recipes[0]).toBe(recipe);
  });

  it('a mandatory-field repair round-trip preserves an unassigned stored recipe', () => {
    const arc = new ARC('');
    const storedRecipe = setDurableId(
      new Recipe('catalog recipe'),
      'recipe:catalog',
    );
    arc.AddRecipe(storedRecipe);

    const issues = [...findEmptyPrimaryFields(arc)];
    expect(issues).toHaveLength(1);
    expect(issues[0].ObjectType).toBe('Dataset');
    expect(issues[0].FieldLabel).toBe('Identifier');

    // MandatoryFieldRepair applies this setter before publishing ARC.Copy().
    issues[0].SetValue('repaired identifier');
    const repaired = copyArc(arc);

    expect(repaired.Identifier).toBe('repaired identifier');
    expect(repaired.Recipes).toEqual([storedRecipe]);
  });

  it('the recipe traversal returns referenced, dynamic-fallback, and stored recipes without duplicate references', () => {
    const arc = new ARC('recipe-traversal');
    const referenced = setDurableId(
      new Recipe('referenced recipe'),
      'recipe:referenced',
    );
    const dynamicFallback = new Recipe(
      'dynamic fallback recipe',
      undefined,
      '1.0',
      'https://example.org/dynamic-fallback',
    );
    const stored = setDurableId(
      new Recipe('stored recipe'),
      'recipe:stored',
    );

    arc.AddProcess(new Process('process', referenced));
    arc.SetProperty('Protocols', [dynamicFallback, referenced]);
    arc.AddRecipe(stored);
    arc.AddRecipe(referenced);

    const traversed = recipes(arc);
    expect(traversed).toHaveLength(3);
    expect(traversed[0]).toBe(referenced);
    expect(traversed[1]).toBe(dynamicFallback);
    expect(traversed[2]).toBe(stored);
  });
});

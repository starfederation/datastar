import assert from 'node:assert/strict'
import fs from 'node:fs'
import path from 'node:path'
import test from 'node:test'
import languageData from '../src/language-data.json'

const attributes = new Map(languageData.attributes.map(attribute => [attribute.name, attribute]));

test('generated language data contains explicit attribute semantics', () => {
    assert.deepEqual(attributes.get('show')!.requirement, { key: 'denied', value: 'must' });
});

test('generated language data overlays explicit signal semantics', () => {
    assert.equal(attributes.get('bind')!.signals, 'key-or-value');
    assert.equal(attributes.get('computed')!.signals, 'key-or-object');
    assert.equal(attributes.get('match-media')!.signals, 'key');
    assert.equal(attributes.get('nonce')!.element, 'html');
    assert.equal(attributes.get('nonce')!.highlight, false);
});

test('generated language data includes modifier metadata from the docs', () => {
    assert.ok(attributes.get('on')!.modifiers.some(modifier => modifier.name === 'debounce'));
    assert.ok(attributes.get('on')!.modifiers.some(modifier => modifier.name === 'document'));
    assert.ok(attributes.get('on')!.modifiers.some(modifier => modifier.name === 'prevent'));
    assert.ok(attributes.get('bind')!.modifiers.some(modifier => modifier.name === 'case'));
    assert.ok(attributes.get('persist')!.modifiers.some(modifier => modifier.name === 'session'));
});

test('generated language data includes native DOM events', () => {
    assert.ok(languageData.nativeEvents.includes('click'));
    assert.ok(languageData.nativeEvents.includes('input'));
    assert.ok(languageData.nativeEvents.includes('pointerdown'));
});

test('TextMate grammar attributes match generated highlighted attributes', () => {
    const grammarPath = path.join(__dirname, '..', 'src', 'datastar.injection.tmLanguage.json');
    const grammar = JSON.parse(fs.readFileSync(grammarPath, 'utf8'));
    const begin = grammar.repository['datastar-attribute'].begin;
    const grammarNames = begin.slice(begin.indexOf(')(') + 2, begin.indexOf(')(?=')).split('|');
    const generatedNames = languageData.attributes
        .filter(attribute => attribute.highlight)
        .map(attribute => attribute.name);

    assert.deepEqual(grammarNames, generatedNames);
});

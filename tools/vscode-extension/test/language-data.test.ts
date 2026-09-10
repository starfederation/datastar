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
    assert.equal(attributes.get('show')!.pro, false);
    assert.equal(attributes.get('animate')!.pro, true);
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

test('generated language data includes action metadata from the docs', () => {
    const actions = new Map(languageData.actions.map(action => [action.name, action]));

    assert.equal(actions.get('peek')!.signature, '@peek(callable: () => any)');
    assert.equal(actions.get('post')!.signature, undefined);
    assert.deepEqual(
        actions.get('setAll')!.parameters!.map(parameter => parameter.label),
        ['value: any', 'filter?: {include: RegExp, exclude?: RegExp}'],
    );
    assert.equal(actions.get('get')!.pro, false);
    assert.deepEqual(languageData.backendActionNames, ['get', 'post', 'put', 'patch', 'delete']);
    assert.equal(actions.get('post')!.parameters, undefined);
    assert.deepEqual(
        languageData.backendActionParameters.map(parameter => parameter.label),
        ['uri: string', 'options={ }'],
    );
    assert.ok(languageData.backendActionOptions.some(option => option.name === 'contentType'));
    assert.ok(languageData.backendActionOptions.some(option => option.name === 'requestCancellation'));
    assert.equal(actions.get('clipboard')!.pro, true);
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

test('TextMate grammar highlights dotted signal keys', () => {
    const grammarPath = path.join(__dirname, '..', 'src', 'datastar.injection.tmLanguage.json');
    const grammar = JSON.parse(fs.readFileSync(grammarPath, 'utf8'));
    const keyPattern = grammar.repository['datastar-attribute'].patterns[1].match;
    const match = ':foo.bar='.match(new RegExp(keyPattern));

    assert.equal(match?.[2], 'foo.bar');
});

test('TextMate grammar highlights underscore-prefixed signal keys', () => {
    const grammarPath = path.join(__dirname, '..', 'src', 'datastar.injection.tmLanguage.json');
    const grammar = JSON.parse(fs.readFileSync(grammarPath, 'utf8'));
    const keyPattern = grammar.repository['datastar-attribute'].patterns[1].match;
    const match = ':_foo.bar='.match(new RegExp(keyPattern));

    assert.equal(match?.[2], '_foo.bar');
});

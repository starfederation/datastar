import assert from 'node:assert/strict'
import test from 'node:test'
import {
    collectSignalDeclarations,
    getCompletions,
    getDiagnostics,
    getHover,
    parseDocument,
} from '../src/language-service'

test('parses Datastar attributes in opening tags only', () => {
    const source = '<div data-show="$visible"><!-- <p data-text="bad"> --></div>';
    const attributes = parseDocument(source);

    assert.equal(attributes.length, 1);
    assert.equal(attributes[0].pluginName, 'show');
    assert.equal(attributes[0].value, '$visible');
    assert.equal(attributes[0].tagName, 'div');
});

test('ignores tag-like source inside script elements', () => {
    const source = '<script>const example = `<div data-show>`</script><div data-text="value">';
    const attributes = parseDocument(source);

    assert.deepEqual(attributes.map(attribute => attribute.pluginName), ['text']);
});

test('reports structural attribute errors', () => {
    const source = '<div data-on="doThing()" data-show data-effect:foo="bar"></div>';
    const diagnostics = getDiagnostics(source);

    assert.deepEqual(
        diagnostics.map(diagnostic => diagnostic.code),
        ['key-required', 'value-required', 'key-not-allowed'],
    );
});

test('enforces exclusive key and value forms', () => {
    assert.equal(getDiagnostics('<input data-bind:name>').length, 0);
    assert.equal(getDiagnostics('<input data-bind="name">').length, 0);
    assert.equal(getDiagnostics('<input data-bind:name="name">')[0].code, 'exclusive-key-value');
    assert.equal(getDiagnostics('<input data-bind>')[0].code, 'exclusive-key-value');
});

test('restricts data-nonce to the html element', () => {
    assert.equal(getDiagnostics('<html data-nonce="abc">').length, 0);
    assert.equal(getDiagnostics('<main data-nonce="abc">')[0].code, 'invalid-element');
});

test('does not treat arbitrary data attributes as Datastar errors', () => {
    assert.equal(getDiagnostics('<div data-testid="example" data-my-plugin="value">').length, 0);
});

test('provides snippets in tag attribute positions', () => {
    const source = '<div data-sh';
    const completions = getCompletions(source, source.length);
    const show = completions.find(completion => completion.label === 'data-show');

    assert.ok(show);
    assert.equal(show.insertText, 'data-show="${1:expression}"');
    assert.equal(source.slice(show.start, show.end), 'data-sh');
    assert.equal(completions.some(completion => completion.label === 'data-nonce'), false);
});

test('provides data-nonce completion on html', () => {
    const source = '<html data-n>';
    const completions = getCompletions(source, source.indexOf('>'));

    assert.equal(completions.some(completion => completion.label === 'data-nonce'), true);
});

test('provides native event completions after data-on:', () => {
    const source = '<button data-on:';
    const completions = getCompletions(source, source.length);
    const click = completions.find(completion => completion.label === 'data-on:click');

    assert.ok(click);
    assert.equal(click.insertText, 'data-on:click="${1:expression}"');
    assert.equal(source.slice(click.start, click.end), 'data-on:');
    assert.ok(completions.some(completion => completion.label === 'data-on:input'));
    assert.ok(completions.some(completion => completion.label === 'data-on:pointerdown'));
});

test('provides modifier and modifier tag completions', () => {
    const modifierSource = '<button data-on:click__';
    const modifiers = getCompletions(modifierSource, modifierSource.length);
    const debounce = modifiers.find(completion => completion.label === 'debounce');

    assert.ok(debounce);
    assert.equal(debounce.insertText, 'debounce');
    assert.equal(debounce.start, modifierSource.length);
    assert.ok(modifiers.some(completion => completion.label === 'document'));
    assert.equal(modifiers.some(completion => completion.label === 'debounce.500ms'), false);

    const tagSource = '<button data-on:click__debounce.';
    const tags = getCompletions(tagSource, tagSource.length);
    assert.ok(tags.some(completion => completion.label === '500ms'));
    assert.ok(tags.some(completion => completion.label === 'leading'));

    const repeatedSource = '<button data-on:click__once__';
    const remaining = getCompletions(repeatedSource, repeatedSource.length);
    assert.equal(remaining.some(completion => completion.label === 'once'), false);
});

test('does not provide attribute completions inside values', () => {
    const source = '<div data-show="data-sh">';
    const offset = source.indexOf('data-sh', source.indexOf('=')) + 'data-sh'.length;

    assert.deepEqual(getCompletions(source, offset), []);
});

test('collects document-local signals from key, value, and object declarations', () => {
    const source = [
        '<div data-signals="{count: 0, user: {name: \'Ada\'}}">',
        '<div data-computed:full-name__case.snake="$user.name">',
        '<input data-bind="query">',
    ].join('');
    const declarations = collectSignalDeclarations(source);

    assert.deepEqual(
        declarations.map(declaration => declaration.name),
        ['count', 'user', 'user.name', 'full_name', 'query'],
    );
});

test('completes root signals inside Datastar expressions', () => {
    const source = '<div data-signals="{user: {name: \'Ada\'}, count: 0}" data-text="$us">';
    const offset = source.indexOf('$us') + '$us'.length;
    const completions = getCompletions(source, offset);

    assert.ok(completions.some(completion => completion.label === '$user'));
    assert.ok(completions.some(completion => completion.label === '$count'));
    assert.equal(source.slice(completions[0].start, completions[0].end), '$us');
});

test('completes signals while the attribute and tag are incomplete', () => {
    const source = '<div data-signals:count="0" data-text="$co';
    const completions = getCompletions(source, source.length);

    assert.deepEqual(completions.map(completion => completion.label), ['$count']);
});

test('completes nested signal properties', () => {
    const source = '<div data-signals="{user: {name: \'Ada\', address: {city: \'Vienna\'}}}" data-text="$user.n">';
    const offset = source.indexOf('$user.n') + '$user.n'.length;
    const completions = getCompletions(source, offset);

    assert.deepEqual(completions.map(completion => completion.label), ['name', 'address']);
    assert.equal(source.slice(completions[0].start, completions[0].end), 'n');
});

test('does not offer signals in attributes that declare a plain signal name', () => {
    const source = '<input data-signals:query="\'\'" data-bind="$q">';
    const offset = source.indexOf('$q') + '$q'.length;

    assert.deepEqual(getCompletions(source, offset), []);
});

test('returns hover documentation and requirements', () => {
    const source = '<div data-show="$visible">';
    const hover = getHover(source, source.indexOf('data-show') + 2);

    assert.ok(hover);
    assert.match(hover.description, /Shows or hides/);
    assert.deepEqual(hover.requirements, ['Key: not allowed', 'Value: required']);
});

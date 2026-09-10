import assert from 'node:assert/strict'
import test from 'node:test'
import {
    collectSignalDeclarations,
    getCompletions,
    getDiagnostics,
    getDefinition,
    getHover,
    getReferences,
    getRenameEdits,
    getRenameTarget,
    getSignatureHelp,
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

test('identifies Pro attributes in completion details', () => {
    const source = '<div data-an';
    const animate = getCompletions(source, source.length)
        .find(completion => completion.label === 'data-animate:*');

    assert.equal(animate?.detail, 'Datastar Pro attribute');
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

test('finds definitions for root signals and nested properties', () => {
    const source = '<div data-signals="{user: {name: \'Ada\'}}" data-text="$user.name">';
    const reference = source.indexOf('$user.name');
    const root = getDefinition(source, reference + 2);
    const property = getDefinition(source, reference + '$user.'.length + 1);

    assert.equal(source.slice(root!.start, root!.end), 'user');
    assert.equal(source.slice(property!.start, property!.end), 'name');
});

test('finds key-form signal definitions after case conversion', () => {
    const source = '<div data-signals:full-name__case.snake="\'Ada\'" data-text="$full_name">';
    const definition = getDefinition(source, source.indexOf('$full_name') + 2);

    assert.equal(source.slice(definition!.start, definition!.end), 'full-name');
});

test('falls back to the nearest declared signal parent', () => {
    const source = '<div data-signals="{user: {name: \'Ada\'}}" data-text="$user.missing">';
    const definition = getDefinition(source, source.indexOf('missing') + 2);

    assert.equal(source.slice(definition!.start, definition!.end), 'user');
});

test('does not resolve an unknown property to a declared sibling', () => {
    const source = '<div data-signals:user.name="\'Ada\'" data-text="$user.missing">';
    const definition = getDefinition(source, source.indexOf('missing') + 2);

    assert.equal(definition, undefined);
});

test('does not return definitions for undeclared signals or plain signal-name values', () => {
    const undeclared = '<div data-text="$missing">';
    assert.equal(getDefinition(undeclared, undeclared.indexOf('$missing') + 2), undefined);

    const plainName = '<input data-bind="$query">';
    assert.equal(getDefinition(plainName, plainName.indexOf('$query') + 2), undefined);
});

test('finds signal references with or without declarations', () => {
    const source = '<div data-signals="{user: {name: \'Ada\'}}" data-text="$user.name" data-show="$user.name != \'\'">';
    const offset = source.indexOf('$user.name') + '$user.'.length + 1;

    assert.deepEqual(
        getReferences(source, offset).map(reference => source.slice(reference.start, reference.end)),
        ['name', 'name', 'name'],
    );
    assert.equal(getReferences(source, offset, false).length, 2);
});

test('finds root signal references used in nested paths', () => {
    const source = '<div data-signals="{user: {name: \'Ada\'}}" data-text="$user.name">';
    const offset = source.indexOf('$user') + 2;

    assert.deepEqual(
        getReferences(source, offset).map(reference => source.slice(reference.start, reference.end)),
        ['user', 'user'],
    );
});

test('prepares and applies a nested signal rename', () => {
    const source = '<div data-signals="{user: {name: \'Ada\'}}" data-text="$user.name" data-show="$user.name">';
    const offset = source.indexOf('$user.name') + '$user.'.length + 1;
    const target = getRenameTarget(source, offset);
    const edits = getRenameEdits(source, offset, 'fullName');
    let renamed = source;
    for (const edit of [...edits].sort((left, right) => right.start - left.start)) {
        renamed = renamed.slice(0, edit.start) + edit.newText + renamed.slice(edit.end);
    }

    assert.equal(target?.placeholder, 'name');
    assert.equal(edits.length, 3);
    assert.match(renamed, /user: \{fullName:/);
    assert.equal(renamed.match(/\$user\.fullName/g)?.length, 2);
});

test('renames root segments in key-form signal declarations', () => {
    const source = '<div data-signals:user.name="\'Ada\'" data-text="$user.name">';
    const offset = source.indexOf('$user') + 2;
    const edits = getRenameEdits(source, offset, 'account');

    assert.deepEqual(
        edits.map(edit => source.slice(edit.start, edit.end)),
        ['user', 'user'],
    );
});

test('renames nested segments in value-form signal declarations', () => {
    const source = '<input data-bind="user.name"><div data-text="$user.name">';
    const offset = source.indexOf('$user.name') + '$user.'.length + 1;

    assert.deepEqual(
        getRenameEdits(source, offset, 'fullName').map(edit => source.slice(edit.start, edit.end)),
        ['name', 'name'],
    );
});

test('renames quoted object declaration keys without replacing their quotes', () => {
    const source = '<div data-signals="{\'full-name\': \'Ada\'}" data-text="$full-name">';
    const offset = source.indexOf('$full-name') + 2;
    const edits = getRenameEdits(source, offset, 'display-name');

    assert.equal(source.slice(edits[0].start - 1, edits[0].end + 1), "'full-name'");
    assert.equal(source.slice(edits[0].start, edits[0].end), 'full-name');
});

test('rejects invalid signal rename names', () => {
    const source = '<div data-signals:count="0" data-text="$count">';
    assert.deepEqual(getRenameEdits(source, source.indexOf('$count') + 2, 'not valid'), []);
});

test('provides action completions inside Datastar expressions', () => {
    const source = '<button data-on:click="@po">';
    const offset = source.indexOf('@po') + '@po'.length;
    const completions = getCompletions(source, offset);
    const post = completions.find(completion => completion.label === '@post');

    assert.ok(post);
    assert.equal(post.insertText, '@post(');
    assert.equal(post.kind, 'action');
    assert.equal(post.snippet, undefined);
    assert.equal(post.detail, 'Datastar action');
    assert.equal(source.slice(post.start, post.end), '@po');
});

test('includes Pro actions and identifies them in completion details', () => {
    const source = '<div data-text="@fi">';
    const offset = source.indexOf('@fi') + '@fi'.length;
    const fit = getCompletions(source, offset).find(completion => completion.label === '@fit');

    assert.equal(fit?.detail, 'Datastar Pro action');
    assert.equal(getSignatureHelp('<div data-text="@fit(">', '<div data-text="@fit('.length)?.pro, true);
});

test('provides docs-derived option completions for every backend action', () => {
    for (const action of ['get', 'post', 'put', 'patch', 'delete']) {
        const source = `<button data-on:click="@${action}('/endpoint', {re}">`;
        const offset = source.indexOf('{re') + '{re'.length;
        const completions = getCompletions(source, offset);
        const retry = completions.find(completion => completion.label === 'retry');

        assert.ok(retry, `Expected retry completion for @${action}`);
        assert.equal(retry.insertText, 'retry: ');
        assert.equal(retry.kind, 'property');
        assert.equal(retry.detail, 'Datastar backend action option');
        assert.equal(source.slice(retry.start, retry.end), 're');
    }
});

test('does not repeat backend options or offer them inside nested objects', () => {
    const source = '<button data-on:click="@get(\'/endpoint\', {payload: {foo: 1}, re}">';
    const offset = source.indexOf(', re') + ', re'.length;
    const completions = getCompletions(source, offset);

    assert.equal(completions.some(completion => completion.label === 'payload'), false);
    assert.equal(completions.some(completion => completion.label === 'retry'), true);

    const nested = '<button data-on:click="@get(\'/endpoint\', {headers: {re}">';
    const nestedOffset = nested.indexOf('{re') + '{re'.length;
    assert.deepEqual(getCompletions(nested, nestedOffset), []);
});

test('does not provide backend options for other action object arguments', () => {
    const source = '<button data-on:click="@setAll(true, {re}">';
    const offset = source.indexOf('{re') + '{re'.length;

    assert.deepEqual(getCompletions(source, offset), []);
});

test('does not provide action completions in plain signal-name values', () => {
    const source = '<input data-bind="@">';
    const offset = source.indexOf('@') + 1;

    assert.deepEqual(getCompletions(source, offset), []);
});

test('provides action signature help and tracks the active argument', () => {
    const firstArgument = '<button data-on:click="@post(\'/endpoint\'">';
    const first = getSignatureHelp(firstArgument, firstArgument.indexOf("'/endpoint'") + "'/endpoint'".length);
    assert.equal(first?.label, '@post(uri: string, options={ })');
    assert.equal(first?.activeParameter, 0);

    const secondArgument = '<button data-on:click="@post(\'/endpoint\', {payload: [$foo, $bar]}">';
    const second = getSignatureHelp(secondArgument, secondArgument.indexOf('$bar') + '$bar'.length);
    assert.equal(second?.activeParameter, 1);
});

test('provides signature help for a nested action call', () => {
    const source = '<div data-text="@setAll(@peek(() => $count), {include: /count/})">';
    const offset = source.indexOf('$count') + '$count'.length;
    const signature = getSignatureHelp(source, offset);

    assert.equal(signature?.label, '@peek(callable: () => any)');
    assert.equal(signature?.activeParameter, 0);
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

test('identifies Pro attributes in hover documentation', () => {
    const source = '<div data-animate:opacity="$opacity">';
    const hover = getHover(source, source.indexOf('data-animate') + 2);

    assert.ok(hover?.requirements.includes('Requires Datastar Pro.'));
});

test('returns hover documentation for actions', () => {
    const source = '<button data-on:click="@post(\'/endpoint\')">';
    const hover = getHover(source, source.indexOf('@post') + 2);

    assert.equal(hover?.name, '@post(uri: string, options={ })');
    assert.match(hover!.description, /POST/);
    assert.deepEqual(hover?.requirements, []);
    assert.equal(hover?.references[0].url, 'https://data-star.dev/reference/actions#post');
});

test('returns docs-derived hover documentation for backend action options', () => {
    const source = '<button data-on:click="@get(\'/endpoint\', {retry: \'always\'})">';
    const hover = getHover(source, source.indexOf('retry') + 2);

    assert.equal(hover?.name, 'retry');
    assert.match(hover!.description, /Determines when to retry requests/);
    assert.equal(source.slice(hover!.start, hover!.end), 'retry');
    assert.equal(hover?.references[0].url, 'https://data-star.dev/reference/actions#options');
});

test('does not return backend option hover inside nested objects or other actions', () => {
    const nested = '<button data-on:click="@get(\'/endpoint\', {headers: {retry: true}})">';
    assert.equal(getHover(nested, nested.indexOf('retry') + 2), undefined);

    const otherAction = '<button data-on:click="@setAll(true, {retry: true})">';
    assert.equal(getHover(otherAction, otherAction.indexOf('retry') + 2), undefined);
});

test('identifies Pro actions in hover documentation', () => {
    const source = '<div data-text="@fit($value, 0, 1, 0, 100)">';
    const hover = getHover(source, source.indexOf('@fit') + 2);

    assert.match(hover!.name, /^@fit\(/);
    assert.deepEqual(hover?.requirements, ['Requires Datastar Pro.']);
});

test('returns hover information for signals and nested properties', () => {
    const source = '<div data-signals="{user: {name: \'Ada\'}}" data-text="$user.name">';
    const root = getHover(source, source.indexOf('$user') + 2);
    const property = getHover(source, source.indexOf('$user.name') + '$user.'.length + 1);

    assert.equal(root?.name, '$user');
    assert.equal(root?.description, 'Signal declared in this document.');
    assert.equal(property?.name, '$user.name');
    assert.equal(property?.description, 'Signal property declared in this document.');
});

test('identifies computed and undeclared signals in hover information', () => {
    const computedSource = '<div data-computed:total="$price * 2" data-text="$total">';
    const computed = getHover(computedSource, computedSource.lastIndexOf('$total') + 2);
    assert.equal(computed?.description, 'Computed signal declared in this document.');

    const undeclaredSource = '<div data-text="$missing">';
    const undeclared = getHover(undeclaredSource, undeclaredSource.indexOf('$missing') + 2);
    assert.equal(undeclared?.description, 'Signal is not explicitly declared in this document.');
});

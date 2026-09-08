import assert from 'node:assert/strict'
import { fork } from 'node:child_process'
import path from 'node:path'
import test from 'node:test'
import {
    createMessageConnection,
    IPCMessageReader,
    IPCMessageWriter,
} from 'vscode-jsonrpc/node'
import { PublishDiagnosticsNotification } from 'vscode-languageserver-protocol'
import type {
    CompletionItem,
    Diagnostic,
    Hover,
    InitializeResult,
    Location,
    PublishDiagnosticsParams,
    SignatureHelp,
} from 'vscode-languageserver-protocol'

test('language server completes, hovers, and publishes diagnostics over LSP', async () => {
    const child = fork(path.join(__dirname, '..', 'dist', 'server.js'), ['--node-ipc'], {
        stdio: ['pipe', 'pipe', 'pipe', 'ipc'],
    });
    const connection = createMessageConnection(
        new IPCMessageReader(child),
        new IPCMessageWriter(child),
    );
    connection.listen();

    try {
        const initialize = await connection.sendRequest<InitializeResult>('initialize', {
            processId: null,
            rootUri: null,
            capabilities: {},
            initializationOptions: {
                customAttributes: [],
                enabledLanguages: ['html'],
            },
        });
        assert.equal(initialize.capabilities.hoverProvider, true);
        assert.equal(initialize.capabilities.definitionProvider, true);
        assert.deepEqual(initialize.capabilities.signatureHelpProvider, {
            triggerCharacters: ['(', ','],
            retriggerCharacters: [','],
        });
        connection.sendNotification('initialized', {});

        const uri = 'file:///datastar-lsp-test.html';
        const initialText = '<div data-sh';
        connection.sendNotification('textDocument/didOpen', {
            textDocument: {
                uri,
                languageId: 'html',
                version: 1,
                text: initialText,
            },
        });

        const completions = await connection.sendRequest<CompletionItem[]>('textDocument/completion', {
            textDocument: { uri },
            position: { line: 0, character: initialText.length },
        });
        assert.ok(completions.some(completion => completion.label === 'data-show'));

        const proAttributeText = '<div data-an';
        connection.sendNotification('textDocument/didChange', {
            textDocument: { uri, version: 2 },
            contentChanges: [{ text: proAttributeText }],
        });
        const proAttributeCompletions = await connection.sendRequest<CompletionItem[]>('textDocument/completion', {
            textDocument: { uri },
            position: { line: 0, character: proAttributeText.length },
        });
        assert.equal(
            proAttributeCompletions.find(completion => completion.label === 'data-animate:*')?.detail,
            'Datastar Pro attribute',
        );

        const eventText = '<button data-on:';
        connection.sendNotification('textDocument/didChange', {
            textDocument: { uri, version: 3 },
            contentChanges: [{ text: eventText }],
        });
        const eventCompletions = await connection.sendRequest<CompletionItem[]>('textDocument/completion', {
            textDocument: { uri },
            position: { line: 0, character: eventText.length },
        });
        const clickCompletion = eventCompletions.find(completion => completion.label === 'data-on:click');
        assert.ok(clickCompletion);
        assert.equal(clickCompletion.textEdit && 'newText' in clickCompletion.textEdit
            ? clickCompletion.textEdit.newText
            : undefined, 'data-on:click="${1:expression}"');

        const modifierText = '<button data-on:click__';
        connection.sendNotification('textDocument/didChange', {
            textDocument: { uri, version: 4 },
            contentChanges: [{ text: modifierText }],
        });
        const modifierCompletions = await connection.sendRequest<CompletionItem[]>('textDocument/completion', {
            textDocument: { uri },
            position: { line: 0, character: modifierText.length },
        });
        const debounceCompletion = modifierCompletions.find(completion => completion.label === 'debounce');
        assert.ok(debounceCompletion);
        assert.equal(debounceCompletion.kind, 14);
        assert.equal(debounceCompletion.textEdit && 'newText' in debounceCompletion.textEdit
            ? debounceCompletion.textEdit.newText
            : undefined, 'debounce');

        const signalText = '<div data-signals="{user: {name: \'Ada\'}}" data-text="$us" data-show="$user.name">';
        connection.sendNotification('textDocument/didChange', {
            textDocument: { uri, version: 5 },
            contentChanges: [{ text: signalText }],
        });
        const signalCompletions = await connection.sendRequest<CompletionItem[]>('textDocument/completion', {
            textDocument: { uri },
            position: { line: 0, character: signalText.indexOf('$us') + '$us'.length },
        });
        const userCompletion = signalCompletions.find(completion => completion.label === '$user');
        assert.ok(userCompletion);
        assert.equal(userCompletion.kind, 6);

        const definition = await connection.sendRequest<Location>('textDocument/definition', {
            textDocument: { uri },
            position: { line: 0, character: signalText.indexOf('$user') + '$user.'.length + 1 },
        });
        assert.equal(definition.uri, uri);
        assert.deepEqual(definition.range, {
            start: { line: 0, character: signalText.indexOf('name:') },
            end: { line: 0, character: signalText.indexOf('name:') + 'name'.length },
        });

        const actionText = '<button data-on:click="@po">';
        connection.sendNotification('textDocument/didChange', {
            textDocument: { uri, version: 6 },
            contentChanges: [{ text: actionText }],
        });
        const actionCompletions = await connection.sendRequest<CompletionItem[]>('textDocument/completion', {
            textDocument: { uri },
            position: { line: 0, character: actionText.indexOf('@po') + '@po'.length },
        });
        const postCompletion = actionCompletions.find(completion => completion.label === '@post');
        assert.ok(postCompletion);
        assert.equal(postCompletion.kind, 3);
        assert.equal(postCompletion.insertTextFormat, 1);

        const fitCompletion = actionCompletions.find(completion => completion.label === '@fit');
        assert.equal(fitCompletion?.detail, 'Datastar Pro action');

        const signatureText = '<button data-on:click="@post(\'/endpoint\', ">';
        connection.sendNotification('textDocument/didChange', {
            textDocument: { uri, version: 7 },
            contentChanges: [{ text: signatureText }],
        });
        const signature = await connection.sendRequest<SignatureHelp>('textDocument/signatureHelp', {
            textDocument: { uri },
            position: { line: 0, character: signatureText.indexOf(',') + 2 },
        });
        assert.equal(signature.signatures[0].label, '@post(uri: string, options={ })');
        assert.equal(signature.activeParameter, 1);

        const diagnosticsPromise = new Promise<Diagnostic[]>(resolve => {
            const disposable = connection.onNotification(PublishDiagnosticsNotification.type, (params: PublishDiagnosticsParams) => {
                if (params.uri === uri && params.diagnostics.length > 0) {
                    disposable.dispose();
                    resolve(params.diagnostics);
                }
            });
        });
        const changedText = '<main data-nonce="abc">';
        connection.sendNotification('textDocument/didChange', {
            textDocument: { uri, version: 8 },
            contentChanges: [{ text: changedText }],
        });

        const diagnostics = await diagnosticsPromise;
        assert.equal(diagnostics[0].code, 'invalid-element');

        const hover = await connection.sendRequest<Hover>('textDocument/hover', {
            textDocument: { uri },
            position: { line: 0, character: changedText.indexOf('data-nonce') + 2 },
        });
        assert.match((hover.contents as { value: string }).value, /<html>/);

        const actionHoverText = '<button data-on:click="@post(\'/endpoint\')">';
        connection.sendNotification('textDocument/didChange', {
            textDocument: { uri, version: 9 },
            contentChanges: [{ text: actionHoverText }],
        });
        const actionHover = await connection.sendRequest<Hover>('textDocument/hover', {
            textDocument: { uri },
            position: { line: 0, character: actionHoverText.indexOf('@post') + 2 },
        });
        assert.match((actionHover.contents as { value: string }).value, /@post\(uri: string, options=\{ \}\)/);
        assert.match((actionHover.contents as { value: string }).value, /POST/);

        const proAttributeHoverText = '<div data-animate:opacity="$opacity">';
        connection.sendNotification('textDocument/didChange', {
            textDocument: { uri, version: 10 },
            contentChanges: [{ text: proAttributeHoverText }],
        });
        const proAttributeHover = await connection.sendRequest<Hover>('textDocument/hover', {
            textDocument: { uri },
            position: { line: 0, character: proAttributeHoverText.indexOf('data-animate') + 2 },
        });
        assert.match((proAttributeHover.contents as { value: string }).value, /Requires Datastar Pro/);

        await connection.sendRequest('shutdown');
        connection.sendNotification('exit');
    } finally {
        connection.dispose();
        if (!child.killed) child.kill();
    }
});

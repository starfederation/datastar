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
    PublishDiagnosticsParams,
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

        const eventText = '<button data-on:';
        connection.sendNotification('textDocument/didChange', {
            textDocument: { uri, version: 2 },
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

        const signalText = '<div data-signals="{user: {name: \'Ada\'}}" data-text="$us">';
        connection.sendNotification('textDocument/didChange', {
            textDocument: { uri, version: 3 },
            contentChanges: [{ text: signalText }],
        });
        const signalCompletions = await connection.sendRequest<CompletionItem[]>('textDocument/completion', {
            textDocument: { uri },
            position: { line: 0, character: signalText.indexOf('$us') + '$us'.length },
        });
        const userCompletion = signalCompletions.find(completion => completion.label === '$user');
        assert.ok(userCompletion);
        assert.equal(userCompletion.kind, 6);

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
            textDocument: { uri, version: 4 },
            contentChanges: [{ text: changedText }],
        });

        const diagnostics = await diagnosticsPromise;
        assert.equal(diagnostics[0].code, 'invalid-element');

        const hover = await connection.sendRequest<Hover>('textDocument/hover', {
            textDocument: { uri },
            position: { line: 0, character: changedText.indexOf('data-nonce') + 2 },
        });
        assert.match((hover.contents as { value: string }).value, /<html>/);

        await connection.sendRequest('shutdown');
        connection.sendNotification('exit');
    } finally {
        connection.dispose();
        if (!child.killed) child.kill();
    }
});

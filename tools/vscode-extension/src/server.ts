import {
    CompletionItemKind,
    createConnection,
    DiagnosticSeverity,
    InsertTextFormat,
    ProposedFeatures,
    TextDocuments,
    TextDocumentSyncKind,
} from 'vscode-languageserver/node'
import { TextDocument } from 'vscode-languageserver-textdocument'
import {
    getCompletions,
    getDiagnostics,
    getHover,
    getSignatureHelp,
} from './language-service'

type Settings = {
    customAttributes: string[]
    enabledLanguages: string[]
}

const connection = createConnection(ProposedFeatures.all);
const documents = new TextDocuments(TextDocument);

let settings: Settings = {
    customAttributes: [],
    enabledLanguages: ['html'],
};

connection.onInitialize(params => {
    settings = {
        ...settings,
        ...(params.initializationOptions as Partial<Settings> | undefined),
    };

    return {
        capabilities: {
            textDocumentSync: TextDocumentSyncKind.Incremental,
            completionProvider: {
                triggerCharacters: ['-', ':', '_', '$', '.'],
            },
            hoverProvider: true,
            signatureHelpProvider: {
                triggerCharacters: ['(', ','],
                retriggerCharacters: [','],
            },
        },
    };
});

connection.onDidChangeConfiguration(change => {
    const changedSettings = change.settings as { datastar?: Partial<Settings> } | undefined
    settings = { ...settings, ...(changedSettings?.datastar || {}) };
    for (const document of documents.all()) validate(document);
});

function isEnabled(document: TextDocument): boolean {
    return settings.enabledLanguages.some(item => (
        item.startsWith('.')
            ? document.uri.toLowerCase().endsWith(item.toLowerCase())
            : document.languageId === item
    ));
}

function validate(document: TextDocument): void {
    if (!isEnabled(document)) {
        connection.sendDiagnostics({ uri: document.uri, diagnostics: [] });
        return;
    }

    const diagnostics = getDiagnostics(document.getText(), settings.customAttributes).map(diagnostic => ({
        severity: DiagnosticSeverity.Error,
        range: {
            start: document.positionAt(diagnostic.start),
            end: document.positionAt(diagnostic.end),
        },
        message: diagnostic.message,
        code: diagnostic.code,
        source: 'Datastar',
    }));

    connection.sendDiagnostics({ uri: document.uri, diagnostics });
}

documents.onDidOpen(event => validate(event.document));
documents.onDidChangeContent(event => validate(event.document));
documents.onDidClose(event => connection.sendDiagnostics({ uri: event.document.uri, diagnostics: [] }));

connection.onCompletion(params => {
    const document = documents.get(params.textDocument.uri);
    if (!document || !isEnabled(document)) return [];

    return getCompletions(
        document.getText(),
        document.offsetAt(params.position),
        settings.customAttributes,
    ).map(completion => {
        const references = completion.references
            .map(reference => `[${reference.name}](${reference.url})`)
            .join(' | ');
        const documentation = references
            ? `${completion.description}\n\n${references}`
            : completion.description;

        return {
            label: completion.label,
            kind: completion.kind === 'signal'
                ? CompletionItemKind.Variable
                : completion.kind === 'property'
                    ? CompletionItemKind.Property
                    : completion.kind === 'modifier'
                        ? CompletionItemKind.Keyword
                    : completion.kind === 'action'
                        ? CompletionItemKind.Function
                    : CompletionItemKind.Snippet,
            detail: completion.detail || 'Datastar',
            documentation: { kind: 'markdown', value: documentation },
            insertTextFormat: completion.snippet || !completion.kind
                ? InsertTextFormat.Snippet
                : InsertTextFormat.PlainText,
            textEdit: {
                range: {
                    start: document.positionAt(completion.start),
                    end: document.positionAt(completion.end),
                },
                newText: completion.insertText,
            },
        };
    });
});

connection.onSignatureHelp(params => {
    const document = documents.get(params.textDocument.uri);
    if (!document || !isEnabled(document)) return undefined;

    const signature = getSignatureHelp(document.getText(), document.offsetAt(params.position));
    if (!signature) return undefined;

    return {
        signatures: [{
            label: signature.label,
            documentation: signature.pro
                ? `${signature.description}\n\nRequires Datastar Pro.`
                : signature.description,
            parameters: signature.parameters.map(parameter => ({
                label: parameter.label,
            })),
        }],
        activeSignature: 0,
        activeParameter: signature.activeParameter,
    };
});

connection.onHover(params => {
    const document = documents.get(params.textDocument.uri);
    if (!document || !isEnabled(document)) return undefined;

    const hover = getHover(document.getText(), document.offsetAt(params.position));
    if (!hover) return undefined;

    const sections = [`**${hover.name}**`, hover.description];
    if (hover.requirements.length > 0) sections.push(hover.requirements.join('  \n'));
    if (hover.references.length > 0) {
        sections.push(hover.references.map(reference => `[${reference.name}](${reference.url})`).join(' | '));
    }

    return {
        contents: { kind: 'markdown', value: sections.join('\n\n') },
        range: {
            start: document.positionAt(hover.start),
            end: document.positionAt(hover.end),
        },
    };
});

documents.listen(connection);
connection.listen();

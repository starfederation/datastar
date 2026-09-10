import * as vscode from 'vscode'
import fs from 'node:fs'
import path from 'node:path'
import { LanguageClient, TransportKind } from 'vscode-languageclient/node'
import languageData from './language-data.json'

const BUILTIN_ATTRIBUTES = languageData.attributes.map((attribute) => attribute.name)

let grammarPath = ''
let client: LanguageClient | undefined

export function activate(context: vscode.ExtensionContext): void {
    // Initialize grammar path and generate grammar
    grammarPath = path.join(context.extensionPath, 'src', 'datastar.injection.tmLanguage.json');
    generateGrammar();

    const config = vscode.workspace.getConfiguration('datastar');
    const serverModule = context.asAbsolutePath(path.join('dist', 'server.js'));
    client = new LanguageClient(
        'datastar',
        'Datastar Language Server',
        {
            run: { module: serverModule, transport: TransportKind.ipc },
            debug: { module: serverModule, transport: TransportKind.ipc },
        },
        {
            documentSelector: [
                { scheme: 'file' },
                { scheme: 'untitled' },
            ],
            initializationOptions: {
                customAttributes: config.get('customAttributes', []),
                enabledLanguages: config.get('enabledLanguages', ['html']),
            },
            synchronize: {
                configurationSection: 'datastar',
            },
        },
    );
    context.subscriptions.push(client.start());

    // Watch for configuration changes
    const configWatcher = vscode.workspace.onDidChangeConfiguration(event => {
        if (event.affectsConfiguration('datastar.enabledLanguages')) {
            // Configuration changed, the provider will automatically use new settings
            vscode.window.showInformationMessage('Datastar language settings updated!');
        }

        // Regenerate grammar when custom attributes change
        if (event.affectsConfiguration('datastar.customAttributes')) {
            generateGrammar();

            // Notify user to reload window (required for grammar changes)
            vscode.window.showInformationMessage(
                'Datastar custom attributes updated. Reload window to apply changes.',
                'Reload'
            ).then(selection => {
                if (selection === 'Reload') {
                    vscode.commands.executeCommand('workbench.action.reloadWindow');
                }
            });
        }
    });

    context.subscriptions.push(configWatcher);
}

function generateGrammar() {
    try {
        // Read custom attributes from configuration
        const config = vscode.workspace.getConfiguration('datastar');
    const customAttributes = config.get<string[]>('customAttributes', []);

        // Validate custom attributes
        const validCustomAttributes = customAttributes.filter((attr) => {
            if (!/^[a-z][a-z0-9-]*$/.test(attr)) {
                console.warn(`Invalid custom attribute name: ${attr}. Must be lowercase with hyphens.`);
                return false;
            }
            return true;
        });

        // Merge built-in and custom attributes
        const allAttrs = [...BUILTIN_ATTRIBUTES, ...validCustomAttributes];
        const attrList = allAttrs.join('|');

        // Read the grammar file
        const grammarContent = fs.readFileSync(grammarPath, 'utf8');
        const grammar = JSON.parse(grammarContent) as {
            repository: Record<string, { begin: string; patterns: Array<{ match: string }> }>
        };

        // Update the attr regex in datastar-attribute begin pattern
        const beginPattern = `\\b(data-)(${attrList})(?=__|:|[\\s>=])`;
        grammar.repository['datastar-attribute'].begin = beginPattern;

        // Update the attr regex in nested attr-like keys pattern
        const nestedPattern = `(:)(data-(?:${attrList}))(?=__|:|[\\s>=])`;
        grammar.repository['datastar-attribute'].patterns[0].match = nestedPattern;

        // Write updated grammar back
        fs.writeFileSync(grammarPath, JSON.stringify(grammar, null, 2), 'utf8');

        console.log(`Datastar grammar updated with ${allAttrs.length} attrs (${BUILTIN_ATTRIBUTES.length} built-in + ${validCustomAttributes.length} custom)`);
    } catch (error: unknown) {
        console.error('Failed to generate Datastar grammar:', error);
        const message = error instanceof Error ? error.message : String(error)
        vscode.window.showErrorMessage(`Failed to update Datastar grammar: ${message}`);
    }
}

export function deactivate(): Thenable<void> | undefined {
    if (client) {
        return client.stop();
    }
}

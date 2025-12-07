const vscode = require('vscode');
const fs = require('fs');
const path = require('path');

const BUILTIN_ATTRIBUTES = [
    // CORE
    'attr', 'bind', 'class', 'computed', 'effect', 'ignore', 'ignore-morph',
    'indicator', 'init', 'json-signals', 'on', 'on-intersect', 'on-interval',
    'on-signal-patch', 'on-signal-patch-filter', 'preserve-attr', 'ref', 'show',
    'signals', 'style', 'text',
    // PRO
    'animate', 'custom-validity', 'on-raf', 'on-resize', 'persist',
    'query-string', 'replace-url', 'rocket', 'scroll-into-view', 'view-transition'
];

let grammarPath;
let snippetData = null;

function activate(context) {
    // Initialize grammar path and generate grammar
    grammarPath = path.join(context.extensionPath, 'src', 'datastar.injection.tmLanguage.json');
    generateGrammar();

    // Load snippet data
    const snippetPath = path.join(__dirname, 'data-attributes.json');
    try {
        snippetData = JSON.parse(fs.readFileSync(snippetPath, 'utf8'));
    } catch (error) {
        console.error('Failed to load Datastar snippets:', error);
        return;
    }

    // Load package.json to get native snippet languages
    const packagePath = path.join(__dirname, '..', 'package.json');
    let nativeSnippetLanguages = [];
    try {
        const packageJson = JSON.parse(fs.readFileSync(packagePath, 'utf8'));
        nativeSnippetLanguages = packageJson.contributes.snippets.map(s => s.language);
    } catch (error) {
        console.error('Failed to load package.json for native snippet languages:', error);
        // Fallback to empty array - custom provider will handle all languages
    }

    // Register completion provider for custom file extensions and languages
    const provider = vscode.languages.registerCompletionItemProvider(
        { scheme: 'file' },
        {
            provideCompletionItems(document, position, token, context) {
                const config = vscode.workspace.getConfiguration('datastar');
                const enabledLanguages = config.get('enabledLanguages', ['html']);

                if (enabledLanguages.length === 0) {
                    return undefined;
                }

                const fileName = document.fileName;
                const languageId = document.languageId;

                // Only provide custom completions for file extensions or languages not covered by native snippets
                const shouldProvideSnippets = enabledLanguages.some(item => {
                    // If item starts with dot, treat as file extension (always provide for these)
                    if (item.startsWith('.')) {
                        return fileName.endsWith(item);
                    }
                    // For language IDs, only provide if not already covered by native snippets
                    return languageId === item && !nativeSnippetLanguages.includes(item);
                });

                if (!shouldProvideSnippets) {
                    return undefined;
                }

                return createCompletionItems();
            }
        }
    );

    context.subscriptions.push(provider);

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

function createCompletionItems() {
    if (!snippetData) {
        return [];
    }

    const completionItems = [];

    for (const [key, snippet] of Object.entries(snippetData)) {
        const item = new vscode.CompletionItem(snippet.prefix, vscode.CompletionItemKind.Snippet);
        item.insertText = new vscode.SnippetString(snippet.body);
        item.documentation = new vscode.MarkdownString(snippet.description);

        // Add references if available
        if (snippet.references && snippet.references.length > 0) {
            const referencesText = snippet.references
                .map(ref => `[${ref.name}](${ref.url})`)
                .join(' • ');
            item.documentation.appendMarkdown(`\n\n**References:** ${referencesText}`);
        }

        item.detail = 'Datastar';
        item.sortText = snippet.prefix;

        completionItems.push(item);
    }

    return completionItems;
}

function generateGrammar() {
    try {
        // Read custom attributes from configuration
        const config = vscode.workspace.getConfiguration('datastar');
        const customAttributes = config.get('customAttributes', []);

        // Validate custom attributes
        const validCustomAttributes = customAttributes.filter(attr => {
            if (typeof attr !== 'string' || !/^[a-z][a-z0-9-]*$/.test(attr)) {
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
        const grammar = JSON.parse(grammarContent);

        // Update the attr regex in datastar-attribute begin pattern
        const beginPattern = `\\b(data-)(${attrList})(?=__|:|[\\s>=])`;
        grammar.repository['datastar-attribute'].begin = beginPattern;

        // Update the attr regex in nested attr-like keys pattern
        const nestedPattern = `(:)(data-(?:${attrList}))(?=__|:|[\\s>=])`;
        grammar.repository['datastar-attribute'].patterns[0].match = nestedPattern;

        // Write updated grammar back
        fs.writeFileSync(grammarPath, JSON.stringify(grammar, null, 2), 'utf8');

        console.log(`Datastar grammar updated with ${allAttrs.length} attrs (${BUILTIN_ATTRIBUTES.length} built-in + ${validCustomAttributes.length} custom)`);
    } catch (error) {
        console.error('Failed to generate Datastar grammar:', error);
        vscode.window.showErrorMessage(`Failed to update Datastar grammar: ${error.message}`);
    }
}

function deactivate() { }

module.exports = {
    activate,
    deactivate
}; 
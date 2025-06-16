const vscode = require('vscode');
const fs = require('fs');
const path = require('path');

let snippetData = null;

function activate(context) {
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

function deactivate() {}

module.exports = {
    activate,
    deactivate
}; 
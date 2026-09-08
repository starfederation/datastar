# Datastar Extension for Visual Studio Code

Adds context-aware autocomplete, diagnostics, hover documentation, and syntax highlighting for [Datastar](https://data-star.dev/) to Visual Studio Code.

The extension provides:

- Datastar attribute, modifier, and native event completion
- Datastar action completion and signature help
- Attribute key and value validation
- Hover documentation and links to the Datastar reference
- Document-local signal and nested property completion
- Syntax highlighting in HTML and common template languages

Signals declared with attributes such as `data-signals`, `data-computed`, `data-bind`, `data-ref`, and `data-indicator` are suggested after typing `$` in a Datastar expression. Nested properties are suggested after typing a signal path followed by `.`.

![VSCode extension](https://data-star.dev/static/images/vscode-extension-120.png)

## Installation

Install the extension from the Visual Studio Code Marketplace or search for “Datastar” in the Extensions panel.

Visual Studio Code 1.63.0 or later is required.

## Configuration

Datastar language support is enabled by default for HTML and common template languages. Use `datastar.enabledLanguages` to customize the supported language IDs and file extensions:

```json
{
  "datastar.enabledLanguages": [
    "html",
    "php",
    "twig",
    ".edge",
    ".custom"
  ]
}
```

### Custom Attributes

Use `datastar.customAttributes` to add completion and syntax highlighting for custom Datastar plugins. Specify plugin names without the `data-` prefix:

```json
{
  "datastar.customAttributes": [
    "my-plugin"
  ]
}
```

After adding custom attributes, reload VS Code to apply the changes.

## License

This extension is licensed under the MIT License.

# Datastar Extension for Visual Studio Code

Adds autocomplete and syntax highlighting for [Datastar](https://data-star.dev/) to Visual Studio Code.

![VSCode extension](https://data-star.dev/static/images/vscode-extension-120.png)

## Configuration

By default, Datastar snippets work in HTML and most common template languages. You can customize which languages and file extensions have Datastar support by configuring the `datastar.enabledLanguages` setting.

**To configure:**
1. Open VS Code Settings (Cmd/Ctrl + ,)
2. Search for "datastar enabled languages"
3. Add language IDs (e.g., `html`, `php`, `twig`) or file extensions (e.g., `.edge`, `.njk`, `.custom`)

**Example:**
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

You can add syntax highlighting support for custom Datastar plugins using the `datastar.customAttributes` setting.

**To configure:**
1. Open VS Code Settings (Cmd/Ctrl + ,)
2. Search for "datastar custom attributes"
3. Add your custom plugin names (without the `data-` prefix)

**Example:**
```json
{
  "datastar.customAttributes": [
    "my-plugin"
  ]
}
```

After adding custom attributes, reload VS Code to apply the changes.

## License

This plugin is licensed for free under the MIT License.

## Requirements

This plugin requires Visual Studio Code version 1.63.0 or later.

## Installation

Install the extension from the Visual Studio Code Marketplace or from the extensions panel by searching for “Datastar”.

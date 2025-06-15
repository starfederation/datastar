# Datastar Extension for Visual Studio Code

Adds autocomplete for [Datastar](https://data-star.dev/) to Visual Studio Code.

![screenshot-Zi09CjE7@2x](https://github.com/user-attachments/assets/5082d177-46d2-4683-b7ed-f68749c12c7b)

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

## License

This plugin is licensed for free under the MIT License.

## Requirements

This plugin requires Visual Studio Code version 1.63.0 or later.

## Installation

Install the extension from the Visual Studio Code Marketplace or from the extensions panel by searching for “Datastar”.
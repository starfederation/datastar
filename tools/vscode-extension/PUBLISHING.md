# Building and Publishing the VS Code Extension

Run all commands from `tools/vscode-extension`.

## Prerequisites

- Node.js and npm
- A Visual Studio Marketplace publisher token in `VSCE_PAT`
- An Open VSX access token in `OVSX_PAT`

Keep access tokens out of source control and shell history.

## Install Dependencies

```sh
npm ci
```

## Update Generated Language Data

Run this after changing editor-specific attribute metadata, or when refreshing attribute and action metadata from the Datastar docs:

```sh
npm run generate
```

This command downloads metadata from `https://data-star.dev/docs.md` and updates the checked-in generated language data.

## Validate

```sh
npm test
```

This checks generated files, typechecks the TypeScript sources, builds the extension and language server, and runs the test suite.

## Build the VSIX

```sh
npm run build
```

The VS Code prepublish hook checks generated files, typechecks, and compiles before creating `datastar-vscode-<version>.vsix`.

To inspect the files that will be packaged:

```sh
npm exec vsce ls
```

To install the package locally for final testing:

```sh
code --install-extension datastar-vscode-<version>.vsix --force
```

Reload VS Code after installing it.

## Prepare a Release

Update `package.json` and `package-lock.json` without creating a Git tag:

```sh
npm version <major.minor.patch> --no-git-tag-version
```

Review and commit the version change before publishing.

## Publish

Set the marketplace tokens in the current shell, then run:

```sh
npm run publish
```

This builds one VSIX and publishes that same artifact to the Visual Studio Marketplace and Open VSX.

If one marketplace succeeds and the other fails, retry only the failed target:

```sh
npm run publish:marketplace
npm run publish:openvsx
```

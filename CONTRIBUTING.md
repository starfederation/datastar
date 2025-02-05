# Contributing to Datastar

Thanks for thinking about contributing to Datastar 🚀

## Before You Contribute

Before you contribute, please consider that Datastar is a lightweight JavaScript framework that aims to simplify reactive web apps by using a minimalist, attribute-based approach for state management and event handling directly in HTML.

Anything outside of this scope may not be accepted. If you have a need for a feature that is not within the scope of Datastar, consider using a Web Component or writing your own Datastar plugin.

## Bug Reports & Feature Requests

Before submitting bug reports and feature requests, please search the [open issues](https://github.com/starfederation/datastar/issues) and the _#help_ channel in the [Discord server](https://discord.gg/bnRNgZjgPh) to see if it has already been addressed. When submitting a [new issue](https://github.com/starfederation/datastar/issues/new), please use a descriptive title and include a clear description and as much relevant information as possible.

## Pull Requests

Always open an issue to discuss your idea _before_ submitting a pull request. Pull requests must have a descriptive title and clearly describe the problem and solution. If the pull request adds or changes behaviour, documentation and examples should be updated accordingly.

## Documentation

Datastar’s documentation is written in markdown files that live in [this folder](https://github.com/starfederation/datastar/tree/develop/site/static/md). Improvements to them can be submitted via pull requests.

## SDKs

SDKs must follow the [Architecture Decision Record](https://github.com/starfederation/datastar/blob/develop/sdk/README.md) document. Pull requests for new SDKs should also include the following:

- A README file that explains how to use the SDK.
- An entry in the [SDKs reference guide](https://github.com/starfederation/datastar/blob/develop/site/static/md/reference/sdks.md).
- A value in `SDKLanguages` in [`consts.go`](https://github.com/starfederation/datastar/blob/develop/build/consts.go).
- A `consts_[language].qtpl` file for generating [constants and defaults](https://github.com/starfederation/datastar/blob/develop/build).
- Sample [code snippets](https://github.com/starfederation/datastar/tree/develop/site/static/code_snippets) in the language of your SDK.
- An implementation of each of the [examples](https://github.com/starfederation/datastar/tree/develop/examples).
- Template values for consts and examples in [`run.go`](https://github.com/starfederation/datastar/blob/develop/build/run.go).

## Building

Datastar comes with instructions for building, either manually or via Docker. Read the [Building Guidelines »](BUILDING.md)

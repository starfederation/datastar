# Contributing to Datastar

Thanks for thinking about contributing to Datastar 🚀

## Before You Contribute

Before you contribute, please consider that Datastar is developed and maintained by a a small team of core contributors in a private repo. The source code for each version is made available in the public repo.

## Bug Reports

Before submitting bug reports, please search the [open issues](https://github.com/starfederation/datastar/issues) and the _#help_ channel in the [Discord server](https://discord.gg/bnRNgZjgPh) to see if it has already been addressed. When submitting a [new issue](https://github.com/starfederation/datastar/issues/new), please use a descriptive title and include a clear description and as much relevant information as possible.

## Feature Requests

Before requesting a new feature, please consider that Datastar is a lightweight JavaScript framework that aims to simplify reactive web apps by using a minimalist, attribute-based approach for state management and event handling directly in HTML. If you have a need for a feature that is outside of this scope, consider using a Web Component or writing your own Datastar plugin.

## Pull Requests

Always open an issue to discuss your idea _before_ submitting a pull request. Pull requests must be made to the `develop` branch, have a descriptive title and clearly describe the problem and solution. If the pull request adds or changes behavior, documentation and examples should be updated accordingly.

## SDKs

We are currently only accepting SDKs from people willing to contribute _and_ maintain them.

SDKs must follow the [Architecture Decision Record](https://github.com/starfederation/datastar/blob/develop/sdk/README.md). Pull requests for new SDKs must also include the following (use alphabetical order, where appropriate):

- A README file that explains how to use the SDK.
- An entry in the [SDKs reference guide](https://github.com/starfederation/datastar/blob/develop/site/static/md/reference/sdks.md).
- A value in `SDKLanguages` in [`consts.go`](https://github.com/starfederation/datastar/blob/develop/build/consts.go).
- A `consts_[language].qtpl` file for generating [constants and defaults](https://github.com/starfederation/datastar/blob/develop/build).
- Sample [code snippets](https://github.com/starfederation/datastar/tree/develop/site/static/code_snippets) in the language of your SDK.
- An implementation of each of the [examples](https://github.com/starfederation/datastar/tree/develop/examples).
- Template values for consts and examples in [`run.go`](https://github.com/starfederation/datastar/blob/develop/build/run.go).
# WIP Release Notes for Datastar

## v1.0.0-beta.4

In this release we tuned the engine, added the ability to react to specific signal changes, and added some new modifiers. Idiomorph got an upgrade, and a few bugs were squashed.

The most exciting part of this beta release is that it marks the final round of planned changes before we move to a stable v1 release 🚀

### Added

- Added the ability to add a signal path to the `data-on-signals-change-*` attribute ([#587](https://github.com/starfederation/datastar/issues/587)).
- Added a `__viewtransition` modifier to the `data-on-*` attribute that wraps the expression in `document.startViewTransition()` when the View Transition API is available ([#627](https://github.com/starfederation/datastar/issues/627)).
- Added a `__case` modifier to the `data-signals-*`, `data-computed-*`, `data-ref-*`, `data-indicator-*`, `data-persist-*`, `data-bind-*`,  `data-class-*`, and `data-on-*` attributes, allowing you to modify the casing of the key by adding `.kebab`, `.snake` or `.pascal`.
- Added a `retrying` event type that is dispatched when the SSE plugin is trying to reconnect ([#583](https://github.com/starfederation/datastar/issues/583)).
- Website now has embedded fuzzy search for the API documentation ([#631](

### Changed

- Idiomorph was updated to version 0.5.0 (pre-release) and is now imported as a module, making it easier to import future versions ([#608](https://github.com/starfederation/datastar/issues/608), [#633](https://github.com/starfederation/datastar/issues/633)).
- Class names are no longer converted to kebab case when used in the `data-class-*` attribute ([#610](https://github.com/starfederation/datastar/issues/610)).
- Event names are no longer converted to kebab case when used in the `data-on-*` attribute.

### Fixed

- Fixed a bug in which signals were being cleared when merge fragments were sending down `data-signals` attributes ([#588](https://github.com/starfederation/datastar/issues/588)).
- Fixed a bug in which using the `__ifmissing` modifier with the `data-signals-*` attribute was not having any effect ([#605](https://github.com/starfederation/datastar/issues/605)).
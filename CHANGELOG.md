# WIP Release Notes for Datastar

## v1.0.0-beta.4

### Added

- Added the ability to add a signal path to the `data-on-signals-change-*` attribute ([#587](https://github.com/starfederation/datastar/issues/587)).
- Added a `__case` modifier to the `data-signals-*`, `data-computed-*`, `data-ref-*`, `data-indicator-*`, `data-persist-*`, `data-bind-*`,  `data-class-*`, and `data-on-*` attributes, allowing you to modify the casing of the key by adding `.kebab`, `.snake` or `.pascal`.
- Added a `retrying` event type that is dispatched when the SSE plugin is trying to reconnect ([#583](https://github.com/starfederation/datastar/issues/583)).

### Changed

- Idiomorph was updated to version [0.4.0](https://github.com/bigskysoftware/idiomorph/releases/tag/v0.4.0) , fixing some bugs, and is now imported as a module, making it easier to import future versions ([#608](https://github.com/starfederation/datastar/issues/608)).
- Class names are no longer converted to kebab case when used in the `data-class-*` attribute ([#610](https://github.com/starfederation/datastar/issues/610)).
- Event names are no longer converted to kebab case when used in the `data-on-*` attribute.

### Fixed

- Fixed a bug in which signals were being cleared when merge fragments were sending down `data-signals` attributes ([#588](https://github.com/starfederation/datastar/issues/588)).
- Fixed a bug in which using the `__ifmissing` modifier with the `data-signals-*` attribute was not having any effect ([#605](https://github.com/starfederation/datastar/issues/605)).
# WIP Release Notes for Datastar

## v0.21.4

### Added

- Added a `contentType` option to the `sse()` action that, when set to `form`, submits the closest form element, or one specified using the `selector` option ([#400](https://github.com/starfederation/datastar/issues/400)).
- Added a `retryInterval` option to the `sse()` action, defaulting to 1 second ([#393](https://github.com/starfederation/datastar/issues/393)).
- Added the version number in a comment at the top of bundled files ([#401](https://github.com/starfederation/datastar/issues/401)).

### Changed

- The `data-on-submit` event listener now prevents the default submission behavior of forms.

### Fixed

- Fixed a bug in which local signals were being unintentionally sent with requests ([#387](https://github.com/starfederation/datastar/issues/387)).
- Fixed a bug in which the bundler was not exporting Datastar ([#403](https://github.com/starfederation/datastar/issues/403)).
- Fixed a bug in which the persist plugin was not maintain values if the values were signals ([#403](https://github.com/starfederation/datastar/issues/413)).
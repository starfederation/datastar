# WIP Release Notes for Datastar

## v1.0.0-beta.4

### Added

- Added a `retrying` event type that is dispatched when the SSE plugin is trying to reconnect ([#583](https://github.com/starfederation/datastar/issues/583)).

### Fixed

- Fixed a bug in which class names were being converted to kebab case when used in `data-class-*` attributes ([#610](https://github.com/starfederation/datastar/issues/610)).
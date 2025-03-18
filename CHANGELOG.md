# Release Notes for Datastar

Each tagged version of Datastar is accompanied by a release note. Read the [release notes »](https://github.com/starfederation/datastar/releases)

# WIP Release Notes

## v1.0.0-beta.10

### Added 

- Added the `data-json` attribute that sets the text content of an element to a reactive JSON stringified version of all signals ([#775](https://github.com/starfederation/datastar/issues/775)).

### Changed

- Replaced the Preact Signals library with our own “rocket” signals, which is between 70% and 6113% faster ([#775](https://github.com/starfederation/datastar/issues/775)).
- Updated Idiomorph to version [0.7.3](https://github.com/bigskysoftware/idiomorph/releases/tag/v0.7.3).
- Classes used in `data-class-*` attributes now default to kebab-case ([#761](https://github.com/starfederation/datastar/issues/761)).
- Events used in `data-on-*` attributes now default to kebab-case ([#761](https://github.com/starfederation/datastar/issues/761)).
- The `datastar-sse` event is now dispatched on the element itself ([#761](https://github.com/starfederation/datastar/issues/761)).
- The NPM package now also exports all official plugins and bundles ([#742](https://github.com/starfederation/datastar/issues/742)).
- Data attributes with plugin names in their prefix are no longer processed ([#771](https://github.com/starfederation/datastar/issues/771)).
- The alias used in the aliased bundle has been renamed to `star` (`data-star-*`) ([#772](https://github.com/starfederation/datastar/issues/772)).

### Fixed

- Fixed the applying of plugins to give custom plugins a chance to load ([#740](https://github.com/starfederation/datastar/issues/740)).
- Fixed a bug in which the indicator signal was not being reset when the element it was on was removed from the DOM ([#749](https://github.com/starfederation/datastar/issues/749)).

### Removed

- Removed settling from SSE events, which has become redundant ([#764](https://github.com/starfederation/datastar/issues/764)).

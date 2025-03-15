# Release Notes for Datastar

Each tagged version of Datastar is accompanied by a release note. Read the [release notes »](https://github.com/starfederation/datastar/releases)

# WIP Release Notes

## v1.0.0-beta.10

### Added

- Added the `data-on-resize` attribute that attaches a [ResizeObserver](https://developer.mozilla.org/en-US/docs/Web/API/ResizeObserver) to the element, and executes the expression each time the element’s dimensions change ([#759](https://github.com/starfederation/datastar/issues/759)).

### Changed

- Updated Idiomorph to version [0.7.3](https://github.com/bigskysoftware/idiomorph/releases/tag/v0.7.3).
- Events used in `data-on-*` attributes now default to kebab-case ([#761](https://github.com/starfederation/datastar/issues/761)).
- The `datastar-sse` event is now dispatched on the element itself ([#761](https://github.com/starfederation/datastar/issues/761)).
- The NPM package now also exports all official plugins and bundles ([#742](https://github.com/starfederation/datastar/issues/742)).

### Fixed

- Fixed the applying of plugins to give custom plugins a chance to load ([#740](https://github.com/starfederation/datastar/issues/740)).
- Fixed a bug in which the indicator signal was not being reset when the element it was on was removed from the DOM ([#749](https://github.com/starfederation/datastar/issues/749)).
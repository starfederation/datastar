# Release Notes for Datastar

Each tagged version of Datastar is accompanied by a release note. Read the [release notes »](https://github.com/starfederation/datastar/releases)

# WIP Release Notes

## v1.0.0-beta.8

### Added

- Added the ability for checkbox input elements to set bound signals to an array of values by predefining the signal as an array ([#664](https://github.com/starfederation/datastar/issues/674)).

### Changed

- Updated Idiomorph to version [0.7.2](https://github.com/bigskysoftware/idiomorph/blob/main/CHANGELOG.md#072---2025-02-20).
- When using `data-bind` on an element, the signal value now defaults to the element’s `value` attribute, provided the signal has not already been defined ([#685](https://github.com/starfederation/datastar/issues/685)).
- Whitespace is now maintained in merged fragments ([#658](https://github.com/starfederation/datastar/issues/658)).
- Data-* plugins are now applied to the HTML element instead of the body element
- All plugins now define an hash of their contents.  This avoid double applies and a host of other side effects. ([#682](https://github.com/starfederation/datastar/issues/682))

### Fixed

- Fixed a bug in which `datastar-remove-fragments` events were not having any effect ([#664](https://github.com/starfederation/datastar/issues/664)).
- Fixed a bug in which `datastarNaN` could be used as an auto-generated element ID ([#679](https://github.com/starfederation/datastar/issues/679)).
- Fixed a bug in which plugins were being applied to the DOM twice on page load.
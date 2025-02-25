# Release Notes for Datastar

Each tagged version of Datastar is accompanied by a release note. Read the [release notes »](https://github.com/starfederation/datastar/releases)

# WIP Release Notes

## v1.0.0-beta.8

### Added

- Added the ability for checkbox input elements to set bound signals to an array of values by predefining the signal as an array ([#664](https://github.com/starfederation/datastar/issues/674)).

### Changed

- Updated Idiomorph to version [0.7.2](https://github.com/bigskysoftware/idiomorph/blob/main/CHANGELOG.md#072---2025-02-20).
- When using `data-bind` on an element, the signal value now defaults to the element’s `value` attribute, provided the signal has not already been defined ([#685](https://github.com/starfederation/datastar/issues/685)).
- The expression passed into `data-on-signals-change` is no longer executed on page load ([#682](https://github.com/starfederation/datastar/issues/682)).
- Whitespace is now maintained in merged fragments ([#658](https://github.com/starfederation/datastar/issues/658)).
- Attribute plugins now define a hash of their contents, preventing duplicate applies ([#691](https://github.com/starfederation/datastar/issues/691)).
- Attribute plugins are now applied to the `html` element instead of the `body` element ([#691](https://github.com/starfederation/datastar/issues/691)).

### Fixed

- Fixed a bug in which `datastar-remove-fragments` events were not having any effect ([#664](https://github.com/starfederation/datastar/issues/664)).
- Fixed a bug in which `datastarNaN` could be used as an auto-generated element ID ([#679](https://github.com/starfederation/datastar/issues/679)).
- Fixed a bug in which `data-attr` was not removing the element attribute when using object syntax and the value was `false` ([#693](https://github.com/starfederation/datastar/issues/693)).

### Removed

- Removed the ability to import the Datastar class. The `apply`, `load`, and `setAlias` functions are exported instead.
# Release Notes for Datastar

Each tagged version of Datastar is accompanied by a release note. Read the [release notes »](https://github.com/starfederation/datastar/releases)

# WIP Release Notes

## v1.0.0-beta.8

### Added

- Added the ability for checkbox input elements to set bound signals to a boolean, string or array, depending on the predefined signal type ([#664](https://github.com/starfederation/datastar/issues/674)).

### Changed
- Datastar singleton removed, call load or setPrefix directly #695
- Updated Idiomorph to version [0.7.1](https://github.com/bigskysoftware/idiomorph/blob/main/CHANGELOG.md#071---2025-02-13).
- Whitespace is now maintained in merged fragments ([#658](https://github.com/starfederation/datastar/issues/658)).

### Fixed

- Fixed a bug in which `datastar-remove-fragments` events were not having any effect ([#664](https://github.com/starfederation/datastar/issues/664)).
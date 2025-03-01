# Release Notes for Datastar

Each tagged version of Datastar is accompanied by a release note. Read the [release notes »](https://github.com/starfederation/datastar/releases)

# WIP Release Notes

## v1.0.0-beta.9

### Added

- Added the ability for input elements to set bound signals to an array of values by predefining the signal as an array.

### Fixed

- Fixed a bug in which `data-signals` was being reapplied each time any attribute changed on an element ([#709](https://github.com/starfederation/datastar/issues/709)).
- Fixed a bug in which focus was not being restored to input elements after merging fragments ([#710](https://github.com/starfederation/datastar/issues/710)).
- Fixed a bug in which the `__delay` modifier was being ignored ([#720](https://github.com/starfederation/datastar/issues/720)).
- Fixed a bug in which signals bound to text input elements with a `value` attribute were being reset to the value when the entered value was empty.
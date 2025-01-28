# WIP Release Notes for Datastar

## v1.0.0-beta.3

### Changed

- Changed the `data-on-interval` attribute to accept a `__duration` modifier instead of a `__delay` modifier ([#513](https://github.com/starfederation/datastar/issues/513)).
- Changed the parsing of Datastar expressions so that only semicolons can be used to explicitly indicate a statement delimiter ([#525](https://github.com/starfederation/datastar/issues/525)).
- The `data-custom-validity` attribute can now be used on select and textarea elements, in addition to input elements ([534](https://github.com/starfederation/datastar/issues/534)).

### Fixed

- Fixed how Datastar expressions are evaluated to allow regular expressions and strings that contain semicolons and new lines ([#508](https://github.com/starfederation/datastar/issues/508)).
- Fixed a bug when using the `__delay` modifier on the `data-on-load` attribute ([#511](https://github.com/starfederation/datastar/issues/511)).
- Fixed a bug with the `inner` merge mode that was causing only the inner HTML of the fragment to be merged ([#524](https://github.com/starfederation/datastar/issues/524)).
- Fixed a bug when generating IDs for elements that was resulting in duplicate IDs ([#533](https://github.com/starfederation/datastar/issues/533)).
- Fixed a bug with signal value out of sync when input populated by chrome's back/forward cache ([#530](https://github.com/starfederation/datastar/pull/530))

### Removed

- Removed the version number variable from the JavaScript API ([#545](https://github.com/starfederation/datastar/issues/545)).

# WIP Release Notes for Datastar

## v1.0.0-beta.3

### Changed

- Changed the `data-on-interval` attribute to accept a `__duration` modifier instead of a `__delay` modifier ([#513](https://github.com/starfederation/datastar/issues/513)).

### Fixed

- Fixed how Datastar expressions are evaluated to allow regular expressions and strings that contain semicolons and new lines ([#508](https://github.com/starfederation/datastar/issues/508)).
- Fixed a bug when using the `__delay` modifier on the `data-on-load` attribute ([#511](https://github.com/starfederation/datastar/issues/511)).
- Fixed a bug with the `inner` merge mode that was causing only the inner HTML of the fragment to be merged ([#524](https://github.com/starfederation/datastar/issues/524)).
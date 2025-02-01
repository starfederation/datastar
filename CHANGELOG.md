# WIP Release Notes for Datastar

## v1.0.0-beta.3

### Added

- Added the ability to use an alias prefix when using the [bundler](https://data-star.dev/bundler). This makes it possible to use `data-customPrefix-*` attributes instead of `data-*`.

### Changed

- Changed the `data-on-interval` attribute to accept a `__duration` modifier instead of a `__delay` modifier ([#513](https://github.com/starfederation/datastar/issues/513)).
- The `data-custom-validity` attribute can now be used on `select` and `textarea` elements, in addition to `input` elements ([534](https://github.com/starfederation/datastar/issues/534)).
- Changed how plugins are applied and cleaned up to use `MutationObserver` ([#528](https://github.com/starfederation/datastar/issues/528)).
- Changed the parsing of Datastar expressions so that only semicolons can be used to explicitly indicate a statement delimiter ([#525](https://github.com/starfederation/datastar/issues/525)).

### Fixed

- Fixed a bug when using the `__delay` modifier on the `data-on-load` attribute ([#511](https://github.com/starfederation/datastar/issues/511)).
- Fixed how Datastar expressions are evaluated to allow regular expressions and strings that contain semicolons and new lines ([#508](https://github.com/starfederation/datastar/issues/508)).
- Fixed a bug with the `inner` merge mode that was causing only the inner HTML of a fragment to be merged ([#524](https://github.com/starfederation/datastar/issues/524)).
- Fixed a bug in which signal values could get out of sync when populated by an input element by Chrome’s back/forward cache ([#530](https://github.com/starfederation/datastar/pull/530)).
- Fixed a bug when generating IDs for elements that could result in duplicate IDs ([#533](https://github.com/starfederation/datastar/issues/533)).

### Removed

- Removed the `apply` function from the JavaScript API ([#528](https://github.com/starfederation/datastar/issues/528)).
- Removed the `version` property from the JavaScript API ([#545](https://github.com/starfederation/datastar/issues/545)).

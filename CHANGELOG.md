# WIP Release Notes for Datastar

## v1.0.0-beta.2

### Added

- Added the entire context to error messages output in the browser console.
- Added the ability to use an empty value when using the `data-signals-*` syntax, which sets the value to an empty string.

### Changed

- Changed the order in which plugins are applied to elements to be depth-first per element, then per `data` attribute ([#495](https://github.com/starfederation/datastar/issues/495)).
- Improved the handling of invalid expressions and signals, and made error handling generally more granular ([#452](https://github.com/starfederation/datastar/issues/452)).

### Fixed

- Fixed dashes not being trimmed from keys when double dashes were used ([#450](https://github.com/starfederation/datastar/issues/450)).

### Removed

- Removed the now redundant `method` option from backend plugin actions ([#443](https://github.com/starfederation/datastar/issues/443)).
- Removed the concept of macro plugins.
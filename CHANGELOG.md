# WIP Release Notes for Datastar

## v1.0.0-beta.1

Happy new 2025! We’ve tied up loose ends, made all final breaking changes to the API, and after a grueling couple of months we’re delighted to release Datastar v1.0.0-beta.1 🚀

After stabilising how nested signals work and living with the signal suffix `.value` for a while, we realised that it wasn’t as ergonomic as we wanted. So, honing our regex fu, we managed to switch (back) to a `$` prefix for signals and a `@` prefix for actions.

The symbols add Datastar specific namespacing – `$` for a $ignal, `@` for an @ction – so it’s immediately obvious what you’re working with, and you end up with much fewer characters to read and write!!

### Added

- Added the `data-custom-validity` attribute ([#410](https://github.com/starfederation/datastar/issues/410)).

### Changed

- Signals now have a `$` prefix (again) instead of a `.value` suffix (the regex search and replace from `(\w+(\.\w+)*)\.value` to `\$$1` may be helpful when updating your code).
- Actions now have a `@` prefix (again) instead of no suffix.
- Changed the `data-attributes` attribute to `data-attr` ([#422](https://github.com/starfederation/datastar/issues/422)).
- Changed TypeScript import paths back to relative paths, so that no config is required in the build step.

### Fixed

- Fixed the `__outside` modifier so that elements contained within it are ignored ([#425](https://github.com/starfederation/datastar/issues/425)).
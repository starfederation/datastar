# Release Notes for Datastar

Each tagged version of Datastar is accompanied by a release note. Read the [release notes »](https://github.com/starfederation/datastar/releases)

# WIP Release Notes

## v1.0.0-beta.11

### Added

- Added the `__debounce`, `__throttle` and `__viewtransition` modifiers to `data-on-intersect`.
- Added the `__debounce`, `__throttle` and `__viewtransition` modifiers to `data-on-signal-change`.

### Changed

- The `datastar-sse` event is now dispatched on the `document` element, and using `data-on-datastar-sse` automatically listens for the event on the `document`.